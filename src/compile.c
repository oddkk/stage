#include "compile.h"
#include "syntax_tree.h"
#include "utils.h"
#include "dlist.h"
#include "ast.h"
#include "native.h"
#include "objstore.h"
#include "errors.h"
#include "term_color.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <linux/limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>

struct string complie_job_names[] = {
#define COMPILE_JOB(name, data) STR(#name),
#include "compile_job_defs.h"
#undef COMPILE_JOB
};

enum job_load_module_state {
	JOB_LOAD_MODULE_DISCOVER = 0,
	JOB_LOAD_MODULE_PARSE,
	JOB_LOAD_MODULE_WAIT_FOR_DEPENDENCIES,
	JOB_LOAD_MODULE_RESOLVE_LOOKUPS,
	JOB_LOAD_MODULE_WAIT_FOR_TYPECHECK,
	JOB_LOAD_MODULE_DONE,
};

enum job_compile_expr_state {
	JOB_COMPILE_EXPR_WAIT_FOR_DEPENDENCIES = 0,
	JOB_COMPILE_EXPR_TYPECHECK,
	JOB_COMPILE_EXPR_GENERATE_OBJECT,
	JOB_COMPILE_EXPR_DONE,
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

	struct objstore store;

	struct ast_context *ast_ctx;

	size_t next_job_id;
	// A measure of how many times jobs have yielded.
	size_t time;
	size_t last_completed_job_time;

	enum compile_phase_name current_phase;
	struct compile_phase phases[COMPILE_NUM_PHASES];

	size_t num_jobs_failed;

	struct stg_error_context err;

	struct stg_compile_options opts;
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

#define DISPATCH_JOB(ctx, name, phase, ...)				\
	dispatch_job((ctx), (phase),								\
				 (struct complie_job){								\
					 .type=COMPILE_JOB_##name,						\
					 .name = (job_##name##_t){__VA_ARGS__}		\
				 })

static void visit_stmt(struct compile_ctx *ctx, struct ast_module *mod,
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

	case ST_NODE_ASSIGN_STMT:
		{
			struct atom *name;

			name = node->ASSIGN_STMT.ident->IDENT;

			struct st_node *body_node;
			body_node = node->ASSIGN_STMT.body;

			struct ast_node *expr;
			expr = st_node_visit_expr(ctx->ast_ctx,
					&mod->env, NULL, body_node);

			if (node->ASSIGN_STMT.type) {
				panic("TODO: assign stmt type.");
			}

			ast_namespace_add_decl(
					ctx->ast_ctx, mod, ns, name, expr);
		}
		break;

	case ST_NODE_LAMBDA:
	case ST_NODE_FUNC_CALL:
	case ST_NODE_BIN_OP:
	case ST_NODE_BIND:
		{
			struct ast_node *expr;
			expr = st_node_visit_expr(ctx->ast_ctx,
					&mod->env, NULL, node);

			ast_namespace_add_free_expr(
					ctx->ast_ctx, mod, ns, expr);
		}
		break;

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
job_parse_file(struct compile_ctx *ctx, job_parse_file_t *data)
{
	int err;
	struct st_node *node;

	file_id_t file_id;
	file_id = stg_err_add_file(&ctx->err, data->file_name);

	err = parse_config_file(data->file_name, &ctx->vm->atom_table,
							&ctx->vm->memory, file_id, &ctx->err, &node);
	if (err) {
		printf("Failed to parse source file.\n");
		return JOB_ERROR;
	}

	assert(node);
	assert(node->type == ST_NODE_MODULE);

	struct st_node *stmt = node->MODULE.body;
	while (stmt) {
		assert(stmt->type == ST_NODE_STMT);

		visit_stmt(ctx, data->mod, data->ns, stmt);
		stmt = stmt->next_sibling;
	}

	if (data->num_unparsed_files) {
		*data->num_unparsed_files -= 1;
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
discover_module_files(struct compile_ctx *ctx, struct ast_module *mod,
					  struct string src_dir, int *num_unparsed_files)
{
	char zero_terminated_src_dir[src_dir.length + 1];
	memcpy(zero_terminated_src_dir, src_dir.text, src_dir.length);
	zero_terminated_src_dir[src_dir.length] = 0;

	char *paths[] = {zero_terminated_src_dir, NULL};

	FTS *ftsp;
	ftsp = fts_open(paths, FTS_PHYSICAL, NULL);

	if (!ftsp) {
		perror("fts_open");
		return;
	}

	struct ast_namespace *dir_ns = &mod->root;

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
					file_ns = ast_namespace_add_ns(
							ctx->ast_ctx, &mod->env, dir_ns, atom);
				}

				if (num_unparsed_files) {
					*num_unparsed_files += 1;
				}

				DISPATCH_JOB(ctx, parse_file, COMPILE_PHASE_DISCOVER,
						.mod = mod,
						.ns = file_ns,
						.file_name = file_name,
						.num_unparsed_files = num_unparsed_files);
			}
		} break;

		case FTS_D:
			if (f->fts_namelen) {
				struct string name;
				name.text = f->fts_name;
				name.length = f->fts_namelen;

				struct atom *atom = vm_atom(ctx->vm, name);

				dir_ns = ast_namespace_add_ns(
						ctx->ast_ctx, &mod->env, dir_ns, atom);
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

static void
dispatch_compile_expr(struct compile_ctx *ctx,
		job_load_module_t *data, struct ast_namespace *ns)
{
	for (size_t i = 0; i < ns->num_names; i++) {
		switch (ns->names[i].kind) {
			case AST_MODULE_NAME_DECL:
				DISPATCH_JOB(ctx, compile_expr, COMPILE_PHASE_DISCOVER,
						.mod = data->mod,
						.expr = ns->names[i].decl.expr,
						.out_value = &ns->names[i].decl.value,
						.num_uncompiled_exprs = &data->num_uncompiled_exprs);
				data->num_uncompiled_exprs += 1;
				break;

			case AST_MODULE_NAME_NAMESPACE:
				dispatch_compile_expr(ctx, data, ns->names[i].ns);
				break;

			case AST_MODULE_NAME_IMPORT:
				break;
		}
	}

	for (size_t i = 0; i < ns->num_free_exprs; i++) {
		DISPATCH_JOB(ctx, compile_expr, COMPILE_PHASE_DISCOVER,
				.mod = data->mod,
				.expr = ns->free_exprs[i].expr,
				.out_value = &ns->free_exprs[i].value,
				.num_uncompiled_exprs = &data->num_uncompiled_exprs);
		data->num_uncompiled_exprs += 1;
	}
}

static struct job_status
job_load_module(struct compile_ctx *ctx, job_load_module_t *data)
{
	switch (data->state) {
		case JOB_LOAD_MODULE_DISCOVER:
			assert(data->module_name != NULL || data->module_src_dir.length != 0);

			if (data->module_name) {
				struct stg_module *lookup_mod;

				lookup_mod = vm_get_module(ctx->vm, data->module_name->name);

				if (lookup_mod) {
					if (data->out_module) {
						*data->out_module = &lookup_mod->mod;
					}

					return JOB_OK;
				}

				for (size_t i = 0; i < ctx->vm->num_precompiled_native_modules; i++) {
					struct stg_native_module *native_mod;
					native_mod = ctx->vm->precompiled_native_modules[i];
					if (native_mod->name == data->module_name) {
						data->native_mod = native_mod;
						break;
					}
				}
			}

			{
				bool should_discover_files = true;

				if (data->module_src_dir.length == 0) {
					bool module_found = false;

					char path_buffer[PATH_MAX + 1];
					for (size_t i = 0; i < ctx->opts.num_module_locations; i++) {
						struct string base_dir = ctx->opts.module_locations[i];

						size_t predicted_path_length =
							base_dir.length +
							data->module_name->name.length + 2;

						if (predicted_path_length > PATH_MAX) {
							print_error("lookup module",
									"Module location path '%.*s' is too long. (%zu of max %zu)",
									base_dir.length + data->module_name->name.length, PATH_MAX);
							continue;
						}
						size_t path_length =
							snprintf(path_buffer, PATH_MAX + 1,
								"%.*s/%.*s/", LIT(base_dir), ALIT(data->module_name));

						for (size_t i = 0; i < path_length; i++) {
							if (path_buffer[i] == '\0') {
								print_error("lookup module",
										"Path contained invalid character '0'.");
								return JOB_ERROR;
							}
						}

						char real_path_buffer[PATH_MAX + 2];
						realpath(path_buffer, real_path_buffer);

						struct stat file_stat = {0};

						int err;
						err = stat(real_path_buffer, &file_stat);
						if (err) {
							perror("stat");
							continue;
						}

						if (!S_ISDIR(file_stat.st_mode)) {
							print_info("'%s' is not a module as it is not a directory.\n",
									real_path_buffer);
							continue;
						}

						struct string real_path_string;
						real_path_string.text = real_path_buffer;
						real_path_string.length = strlen(real_path_buffer);

						if (real_path_string.text[real_path_string.length - 1] != '/') {
							real_path_string.length += 1;
							real_path_string.text[real_path_string.length - 1] = '/';
							real_path_string.text[real_path_string.length] = '\0';
						}

						data->module_src_dir =
							string_duplicate_cstr(real_path_buffer);
						module_found = true;
					}

					if (!module_found && !data->native_mod) {
						stg_error(&ctx->err, STG_NO_LOC, "Could not find module '%.*s'",
								ALIT(data->module_name));
						return JOB_ERROR;
					}

					should_discover_files = module_found;
				}

				data->mod = &data->_tmp_module;
				data->mod->env.store = &ctx->store;
				data->mod->root.instance = ast_bind_slot_cons(ctx->ast_ctx, &data->mod->env,
						AST_BIND_NEW, NULL, NULL);

				if (data->module_name != vm_atoms(ctx->vm, "base")) {
					ast_slot_id dep_obj;
					dep_obj = ast_module_add_dependency(ctx->ast_ctx, data->mod,
							vm_atoms(ctx->vm, "base"));
					ast_namespace_use(ctx->ast_ctx, data->mod,
							&data->mod->root, dep_obj);
				}

				if (should_discover_files) {
					discover_module_files(ctx, data->mod,
							data->module_src_dir,
							&data->num_unparsed_files);
				} else {
					data->state = JOB_LOAD_MODULE_WAIT_FOR_DEPENDENCIES;
					return JOB_YIELD;
				}
			}

			data->state = JOB_LOAD_MODULE_PARSE;
			// fallthrough

		case JOB_LOAD_MODULE_PARSE:
			if (data->num_unparsed_files > 0) {
				return JOB_YIELD;
			}

			assert(data->num_unparsed_files == 0);

			for (size_t i = 0; i < data->mod->num_dependencies; i++) {
				DISPATCH_JOB(ctx, load_module, COMPILE_PHASE_DISCOVER,
						.module_name = data->mod->dependencies[i].name,
						.out_module  = &data->mod->dependencies[i].mod);
			}

			data->state = JOB_LOAD_MODULE_WAIT_FOR_DEPENDENCIES;
			// fallthrough

		case JOB_LOAD_MODULE_WAIT_FOR_DEPENDENCIES:
			for (size_t i = 0; i < data->mod->num_dependencies; i++) {
				if (data->mod->dependencies[i].mod == NULL) {
					return JOB_YIELD;
				}
			}

			ast_module_resolve_dependencies(
					ctx->ast_ctx, data->mod);

			{
				struct stg_module_info modinfo = {
					.name = data->module_name->name,
					.version = {0, 1},
				};

				if (data->native_mod) {
					modinfo.init = data->native_mod->hook_init;
					modinfo.free = data->native_mod->hook_free;
				}

				// We register the module here because at this point all
				// dependencies are resolved, so the module's init function can
				// reference them. After this point, _tmp_module should not be
				// used.
				data->stg_mod = vm_register_module(ctx->vm,
						ctx->ast_ctx, data->mod, &modinfo);
				data->mod = &data->stg_mod->mod;

				memset(&data->_tmp_module, 0, sizeof(struct ast_module));
			}

			data->state = JOB_LOAD_MODULE_RESOLVE_LOOKUPS;
			// fallthrough

		case JOB_LOAD_MODULE_RESOLVE_LOOKUPS:
			ast_module_resolve_names(ctx->ast_ctx, data->mod,
					data->native_mod);

			dispatch_compile_expr(ctx, data, &data->mod->root);

			data->state = JOB_LOAD_MODULE_WAIT_FOR_TYPECHECK;
			// fallthrough

		case JOB_LOAD_MODULE_WAIT_FOR_TYPECHECK:
			if (data->num_uncompiled_exprs > 0) {
				return JOB_YIELD;
			}

			assert(data->num_uncompiled_exprs == 0);

			data->state = JOB_LOAD_MODULE_DONE;
			// fallthrough

		case JOB_LOAD_MODULE_DONE:
			{
				int err;
				err = ast_module_finalize(ctx->ast_ctx, data->mod);
				if (err) {
					print_error("compile", "Failed to finalize module '%.*s'.",
							ALIT(data->module_name));
					return JOB_ERROR;
				}
			}

#if 0
			printf("%.*s (%.*s)\n", ALIT(data->module_name), LIT(data->module_src_dir));
			ast_print_module(ctx->ast_ctx, data->mod);

			printf("\n");
			ast_env_print(ctx->vm, &data->mod->env);
#endif

			if (data->out_module) {
				*data->out_module = data->mod;
			}

			return JOB_OK;
	}

	panic("Load module reached an invalid state.");
	return JOB_ERROR;
}

static struct job_status
job_compile_expr(struct compile_ctx *ctx, job_compile_expr_t *data)
{
	switch (data->state) {
		case JOB_COMPILE_EXPR_WAIT_FOR_DEPENDENCIES:
			{
				enum ast_node_dependencies_state dep_state;
				dep_state = ast_node_dependencies_fulfilled(
						ctx->ast_ctx, &data->mod->env, data->expr);
				if (dep_state == AST_NODE_DEPS_NOT_READY) {
					return JOB_YIELD;
				} else if (dep_state == AST_NODE_DEPS_NOT_OK) {
					return JOB_ERROR;
				}
				assert(dep_state == AST_NODE_DEPS_OK);
			}

			data->state = JOB_COMPILE_EXPR_TYPECHECK;
			// fallthrough

		case JOB_COMPILE_EXPR_TYPECHECK:
			ast_node_resolve_slots(ctx->ast_ctx, data->mod,
					&data->mod->env, data->expr);
			if (!ast_node_is_typed(ctx->ast_ctx, &data->mod->env, data->expr)) {
				printf("Failed type expression.\n");
#if 0
				ast_print(ctx->ast_ctx, &data->mod->env, data->expr);

				printf("\n");
				ast_env_print(ctx->vm, &data->mod->env);
#endif
				return JOB_ERROR;
			}

			data->state = JOB_COMPILE_EXPR_GENERATE_OBJECT;
			// fallthrough

		case JOB_COMPILE_EXPR_GENERATE_OBJECT:
			{
				int err;
				struct object obj = {0};
				err = ast_node_eval(ctx->ast_ctx, data->mod,
						&data->mod->env, data->expr, &obj);
				if (err) {
					printf("Failed to generate object.\n");
					return JOB_ERROR;
				}

				*data->out_value =
					ast_bind_slot_const(ctx->ast_ctx, &data->mod->env,
							*data->out_value, NULL, obj);
			}

			*data->num_uncompiled_exprs -= 1;
			data->state = JOB_COMPILE_EXPR_DONE;
			// fallthrough

		case JOB_COMPILE_EXPR_DONE:
			return JOB_OK;
	}

	panic("Compile expr reached an invalid state.");
	return JOB_ERROR;
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
#define COMPILE_JOB(name, data) case COMPILE_JOB_##name: res = job_##name(ctx, &job->name); break;
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
stg_compile(struct vm *vm, struct ast_context *ast_ctx,
		struct stg_compile_options opts, struct string initial_module_src_dir)
{
	struct compile_ctx ctx = {0};

	ctx.vm = vm;
	ctx.mem = &vm->memory;
	ctx.err.string_arena = &vm->memory;
	ctx.ast_ctx = ast_ctx;
	ctx.opts = opts;

	assert(!ctx.ast_ctx->err);
	ctx.ast_ctx->err = &ctx.err;

	DISPATCH_JOB(&ctx, load_module, COMPILE_PHASE_DISCOVER,
			.module_name = vm_atoms(vm, "main"),
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

	ctx.ast_ctx->err = NULL;
	return 0;
}
