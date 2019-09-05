#include "compile.h"
#include "syntax_tree.h"
#include "utils.h"
#include "dlist.h"
#include "ast.h"
#include "objstore.h"
#include "errors.h"
#include "term_color.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>

struct string complie_job_names[] = {
#define COMPILE_JOB(name, data) STR(#name),
#include "compile_job_defs.h"
#undef COMPILE_JOB
};

#define COMPILE_JOB(name, data) typedef data job_##name##_t;
#include "compile_job_defs.h"
#undef COMPILE_JOB

enum job_status_code {
	JOB_STATUS_OK = 0,
	JOB_STATUS_ERROR = -1,
	JOB_STATUS_YIELD = 1,
	JOB_STATUS_YIELD_FOR_PHASE = 2,
	JOB_STATUS_IDLE = 3,
};

struct complie_job {
	size_t id;
	size_t last_dispatch_time;
	enum complie_job_type type;
	enum job_status_code status;

	struct complie_job *next_job;
	struct complie_job *first_dependant_node;

	struct stg_module *mod;

#define COMPILE_JOB(name, data) job_##name##_t name;
	union {
#include "compile_job_defs.h"
	};
#undef COMPILE_JOB
};

enum compile_phase_name {
	COMPILE_PHASE_DISCOVER = 0,
	COMPILE_PHASE_RESOLVE,

	COMPILE_NUM_PHASES
};

struct job_status {
	enum job_status_code status;
	struct complie_job *yield_for;
	enum compile_phase_name yield_for_phase;
};

#define JOB_OK  ((struct job_status){.status=JOB_STATUS_OK})
#define JOB_ERROR ((struct job_status){.status=JOB_STATUS_ERROR})
#define JOB_YIELD ((struct job_status){.status=JOB_STATUS_YIELD, .yield_for=NULL})
#define JOB_YIELD_FOR(job) ((struct job_status){.status=JOB_STATUS_YIELD, .yield_for=(job)})
#define JOB_YIELD_FOR_PHASE(phase) ((struct job_status){.status=JOB_STATUS_YIELD_FOR_PHASE, .yield_for_phase=(phase)})

struct compile_phase {
	struct complie_job *job_head;
	struct complie_job *job_end;
};

#define COMPILE_JOB_MAX_CONSECUTIVE_YIELDS (3)

struct compile_ctx {
	struct vm *vm;
	struct arena *mem;

	struct ast_context *ast_ctx;

	size_t next_job_id;
	// A measure of how many times jobs have yielded.
	size_t time;
	size_t last_completed_job_time;

	enum compile_phase_name current_phase;
	struct compile_phase phases[COMPILE_NUM_PHASES];

	// struct string *file_names;
	// size_t num_files;

	// size_t num_errors;
	size_t num_jobs_failed;

	struct stg_error_context err;
};

static void
append_job(struct compile_ctx *ctx, enum compile_phase_name ph, struct complie_job *job)
{
	assert(ph < COMPILE_NUM_PHASES);
	assert(ph >= ctx->current_phase);

	struct compile_phase *phase = &ctx->phases[ph];

	if (phase->job_end) {
		assert(phase->job_end->next_job == NULL);
		assert(job->next_job == NULL);
		assert(phase->job_end != job);

		phase->job_end->next_job = job;
		phase->job_end = job;

		if (!phase->job_head) {
			phase->job_head = job;
		}
	} else {
		assert(!phase->job_head);
		phase->job_end = job;
		phase->job_head = job;
	}
}

static struct complie_job *
dispatch_job(struct compile_ctx *ctx, enum compile_phase_name phase, struct complie_job job)
{
	struct complie_job *new_job = calloc(1, sizeof(struct complie_job));
	*new_job = job;
	new_job->status = JOB_STATUS_IDLE;
	new_job->next_job = NULL;
	new_job->id = ctx->next_job_id;
	ctx->next_job_id += 1;

	append_job(ctx, phase, new_job);

	return new_job;
}

#define DISPATCH_JOB(ctx, _mod, name, phase, ...)				\
	dispatch_job((ctx), (phase),								\
				 (struct complie_job){								\
					 .type=COMPILE_JOB_##name,						\
					 .mod = _mod,								\
					 .name = (job_##name##_t){__VA_ARGS__}		\
				 })

/*
static struct scope *
instantiate_scope_by_access_pattern(struct compile_ctx *ctx,
									struct stg_module *mod,
									struct scope *parent,
									struct st_node *node)
{
	switch (node->type) {
	case ST_NODE_ACCESS: {
		struct scope *scope;

		scope = instantiate_scope_by_access_pattern(ctx, mod,parent, node->ACCESS.lhs);
		if (!scope) {
			return NULL;
		}

		return instantiate_scope_by_access_pattern(ctx, mod,scope, node->ACCESS.rhs);
	} break;

	case ST_NODE_IDENT: {
		struct scope *scope;
		struct scope_entry entry;

		if (scope_local_lookup(parent, node->IDENT, &entry) == 0) {
			if (entry.anchor != SCOPE_ANCHOR_NONE || !entry.scope) {
				stg_error(&ctx->err, node->loc,
					"'%.*s' already exists, and is not a namespace.",
					ALIT(node->IDENT)
				);
				return NULL;
			}
		}

		scope = scope_push(parent);
		scope_insert(parent, node->IDENT, SCOPE_ANCHOR_NONE,
					 OBJ_NONE, scope);
		return scope;
	} break;

	default:
		panic("Invalid node in access pattern.");
		break;
	}

	return NULL;
}
*/

static void visit_stmt(struct compile_ctx *ctx, struct stg_module *mod,
						  struct ast_namespace *ns, struct st_node *stmt)
{
	assert(stmt->type == ST_NODE_STMT);

	// TODO: Handle attributes

	struct st_node *node = stmt->STMT.stmt;

	if (!node) {
		return;
	}

	switch (node->type) {

	case ST_NODE_USE_STMT:
		break;

	case ST_NODE_ASSIGN_STMT: {
		struct atom *name;

		name = node->ASSIGN_STMT.ident->IDENT;

		struct st_node *body_node;
		body_node = node->ASSIGN_STMT.body;

		struct ast_node *expr;
		expr = st_node_visit_expr(ctx->ast_ctx, mod, &mod->mod.env, body_node);

		if (node->ASSIGN_STMT.type) {
			panic("TODO: assign stmt type.");
		}

		ast_namespace_add_decl(
				ctx->ast_ctx, &mod->mod, ns, name, expr);
	} break;

	default:
		panic("Invalid node '%.*s' as statement.",
				LIT(st_node_names[node->type]));
		break;
	}
}

int parse_config_file(struct string filename,
					  struct atom_table *table,
					  struct arena *memory,
					  unsigned int file_id,
					  struct stg_error_context *err,
					  struct st_node **out_node);

static struct job_status
job_parse_file(struct compile_ctx *ctx, struct stg_module *mod,
			   job_parse_file_t *data)
{
	int err;
	struct st_node *node;

	file_id_t file_id;
	file_id = stg_err_add_file(&ctx->err, data->file_name);

	err = parse_config_file(data->file_name, mod->atom_table,
							&ctx->vm->memory, file_id, &ctx->err, &node);
	if (err) {
		return JOB_ERROR;
	}

	assert(node);
	assert(node->type == ST_NODE_MODULE);

	struct st_node *stmt = node->MODULE.body;
	while (stmt) {
		assert(stmt->type == ST_NODE_STMT);

		visit_stmt(ctx, mod, data->ns, stmt);
		stmt = stmt->next_sibling;
	}

	return JOB_OK;
}

static bool
has_extension(struct string str, struct string ext)
{
	if (str.length < ext.length + 1) {
		return false;
	}

	struct string file_ext;
	file_ext.length = ext.length;
	file_ext.text = (str.text + str.length) - ext.length;

	return string_equal(file_ext, ext);
}

static void
discover_module_files(struct compile_ctx *ctx, struct stg_module *mod,
					  struct string src_dir)
{
	/* // TODO: Ensure zero-terminated */
	char *paths[] = {src_dir.text, NULL};

	FTS *ftsp;
	ftsp = fts_open(paths, FTS_PHYSICAL, NULL);

	struct ast_namespace *dir_ns = &mod->mod.root;

	FTSENT *f;
	while ((f = fts_read(ftsp)) != NULL) {
		switch (f->fts_info) {
		case FTS_F: {
			struct string path;
			path.text = f->fts_path;
			path.length = f->fts_pathlen;

			if (has_extension(path, STR(".stg")) &&
				f->fts_name[0] != '.') {
				struct string name;
				name.text = f->fts_name;
				// Remove the ".stg" suffix
				name.length = f->fts_namelen - 4;

				struct string file_name = {0};
				string_duplicate(&ctx->vm->memory, &file_name, path);

				struct atom *atom = vm_atom(ctx->vm, name);
				struct ast_namespace *file_ns;

				if (atom == vm_atoms(ctx->vm, "mod")) {
					file_ns = dir_ns;
				} else {
					file_ns = ast_namespace_add_ns(dir_ns, atom);
				}

				DISPATCH_JOB(ctx, mod, parse_file, COMPILE_PHASE_DISCOVER,
							 .ns = file_ns,
							 .file_name = file_name);
			}
		} break;

		case FTS_D:
			if (f->fts_namelen) {
				struct string name;
				name.text = f->fts_name;
				name.length = f->fts_namelen;

				struct atom *atom = vm_atom(ctx->vm, name);

				dir_ns = ast_namespace_add_ns(dir_ns, atom);
			}
			break;

		case FTS_DP:
			if (f->fts_namelen) {
				dir_ns = dir_ns->parent;
			}
			break;

		default:
			break;
		}
	}

	if (errno != 0) {
		perror("fts");
	}

	fts_close(ftsp);
}

static struct job_status
job_discover_module(struct compile_ctx *ctx, struct stg_module *mod,
			   job_discover_module_t *data)
{
	discover_module_files(ctx, mod, data->module_src_dir);

	return JOB_OK;
}

#define COMPILE_DEBUG_JOBS 0

static void compile_exec_job(struct compile_ctx *ctx, struct complie_job *job)
{
	assert(job->next_job != job);

	if (job->status == JOB_STATUS_ERROR) {
		return;
	}

	assert(job->status == JOB_STATUS_IDLE || job->status == JOB_STATUS_YIELD);

	if (job->last_dispatch_time == ctx->time) {
		ctx->time += 1;
	}
	job->last_dispatch_time = ctx->time;

#if COMPILE_DEBUG_JOBS
	printf("[%zu] Dispatching 0x%03zx %.*s... ",
		   job->last_dispatch_time,
		   job->id,
		   LIT(complie_job_names[job->type]));
#endif

	struct job_status res;

	switch (job->type) {
#define COMPILE_JOB(name, data) case COMPILE_JOB_##name: res = job_##name(ctx, job->mod, &job->name); break;
#include "compile_job_defs.h"
#undef COMPILE_JOB

	case COMPILE_JOBS_LEN:
		assert(false);
		break;
	}

	job->status = res.status;

	switch (res.status) {
	case JOB_STATUS_OK: {
		struct complie_job *dep = job->first_dependant_node;
		while (dep) {
			struct complie_job *next = dep->next_job;

			dep->next_job = NULL;
			append_job(ctx, ctx->current_phase, dep);

			dep = next;
		}

		job->first_dependant_node = NULL;
#if COMPILE_DEBUG_JOBS
		printf(TERM_COLOR_GREEN("ok") "\n");
#endif
	} break;

	case JOB_STATUS_ERROR: {
		ctx->num_jobs_failed += 1;

		size_t num_canceled = 0;

		for (struct complie_job *dep = job->first_dependant_node;
			 dep != NULL;
			 dep = dep->next_job) {
			dep->status = JOB_STATUS_ERROR;
			num_canceled += 1;
		}

#if COMPILE_DEBUG_JOBS
		if (num_canceled > 0) {
			printf(TERM_COLOR_RED("=== error (%zu dependenc%s canceled) ===") "\n", num_canceled,
				   (num_canceled == 1 ? "y" : "ies"));
		} else {
			printf(TERM_COLOR_RED("=== error ===") "\n");
		}
#endif
	} break;

	case JOB_STATUS_YIELD:
		if (res.yield_for) {
#if COMPILE_DEBUG_JOBS
			printf(TERM_COLOR_YELLOW("=== yield for 0x%03zx %.*s ===") "\n",
				   res.yield_for->id, LIT(complie_job_names[res.yield_for->type]));
#endif

			if (res.yield_for->status == JOB_STATUS_ERROR) {
				job->status = JOB_STATUS_ERROR;
				break;

			} else if (res.yield_for->status == JOB_STATUS_OK) {
				job->next_job = NULL;
				append_job(ctx, ctx->current_phase, job);

			} else {
				// TODO: Thread-safe
				job->next_job = res.yield_for->first_dependant_node;
				res.yield_for->first_dependant_node = job;
			}
		} else {
#if COMPILE_DEBUG_JOBS
			printf(TERM_COLOR_YELLOW("=== yield ===") "\n");
#endif

			job->next_job = NULL;
			append_job(ctx, ctx->current_phase, job);
		}
		break;

	case JOB_STATUS_YIELD_FOR_PHASE:
		job->status = JOB_STATUS_YIELD;
		job->next_job = NULL;
		append_job(ctx, res.yield_for_phase, job);
#if COMPILE_DEBUG_JOBS
		printf(TERM_COLOR_YELLOW("=== yield for phase %i ===") "\n",
			   res.yield_for_phase);
#endif
		break;

	case JOB_STATUS_IDLE:
		panic("Job returned idle as result.");
		break;
	}

	if (job->status == JOB_STATUS_YIELD) {
		if (ctx->time - ctx->last_completed_job_time > COMPILE_JOB_MAX_CONSECUTIVE_YIELDS) {
#if COMPILE_DEBUG_JOBS
			printf(TERM_COLOR_RED("=== erroring due to no progress ===") "\n");
#endif
			job->status = JOB_STATUS_ERROR;
			ctx->num_jobs_failed += 1;
		}
	}

	if (job->status  != JOB_STATUS_YIELD) {
		ctx->last_completed_job_time = ctx->time;
	}
}

int
stg_compile(struct vm *vm, struct ast_context *ast_ctx, struct string initial_module_src_dir)
{
	struct compile_ctx ctx = {0};

	ctx.vm = vm;
	ctx.mem = &vm->memory;
	ctx.err.string_arena = &vm->memory;
	ctx.ast_ctx = ast_ctx;

	assert(!vm->err);
	vm->err = &ctx.err;

	struct stg_module_info modinfo;
	memset(&modinfo, 0, sizeof(struct stg_module_info));

	// TODO: Have an actual name.
	modinfo.name = STR("native");
	modinfo.version.major = 1;
	modinfo.version.minor = 0;

	struct stg_module *mod;
	// TODO: We should somehow initialize through the init method
	// instead of outside of register.
	mod = vm_register_module(vm, &modinfo);

	scope_use(&mod->root_scope, &vm->modules[0]->root_scope);

	DISPATCH_JOB(&ctx, mod, discover_module, COMPILE_PHASE_DISCOVER,
			.module_src_dir = initial_module_src_dir);

	for (; ctx.current_phase < COMPILE_NUM_PHASES; ctx.current_phase += 1) {
		struct compile_phase *phase = &ctx.phases[ctx.current_phase];
#if COMPILE_DEBUG_JOBS
		printf("\n" TERM_COLOR_BLUE("=== Phase %i ===") "\n",
			   ctx.current_phase);
#endif
		while (phase->job_head) {
			struct complie_job *job = phase->job_head;
			phase->job_head = job->next_job;
			if (!phase->job_head) {
				phase->job_end = NULL;
			}

			compile_exec_job(&ctx, job);
		}
	}

	print_errors(&ctx.err);

	if (ctx.num_jobs_failed > 0 || ctx.err.num_errors > 0) {
#if COMPILE_DEBUG_JOBS
		printf("\n");
#endif
		printf(TERM_COLOR_RED("Compilation failed! "
					"(%zu jobs failed, %zu errors)") "\n",
			   ctx.num_jobs_failed, ctx.err.num_errors);
	}

	// scope_print(vm, &vm->root_scope);
	printf("\n");
	ast_print_module(ctx.ast_ctx, &mod->mod);

	printf("\n");
	ast_env_print(vm, &mod->mod.env);

	vm->err = NULL;
	return 0;
}
