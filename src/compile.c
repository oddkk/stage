#include "compile.h"
#include "compile_datatype.h"
#include "syntax_tree.h"
#include "utils.h"
#include "dlist.h"
#include "ast.h"
#include "native.h"
#include "objstore.h"
#include "errors.h"
#include "term_color.h"
#include "base/mod.h"

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

	struct paged_list jobs;

	// A measure of how many times jobs have yielded.
	size_t time;
	size_t last_completed_job_time;

	enum compile_phase_name current_phase;
	struct compile_phase phases[COMPILE_NUM_PHASES];

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
	size_t job_id = paged_list_push(&ctx->jobs);
	struct complie_job *new_job = paged_list_get(&ctx->jobs, job_id);
	*new_job = job;
	new_job->status = JOB_STATUS_IDLE;
	new_job->next_job = NULL;
	new_job->id = job_id;

	append_job(ctx, phase, new_job);

	return new_job;
}

#define DISPATCH_JOB(ctx, name, phase, ...)				\
	dispatch_job((ctx), (phase),								\
				 (struct complie_job){								\
					 .type=COMPILE_JOB_##name,						\
					 .name = (job_##name##_t){__VA_ARGS__}		\
				 })

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
	struct st_node *stmt;
	stmt = node->MODULE.body;

	size_t num_errors = ctx->ast_ctx->err->num_errors;

	while (stmt) {
		st_node_visit_stmt(ctx->ast_ctx, data->mod, data->scope, stmt);
		stmt = stmt->next_sibling;
	}

	if (num_errors < ctx->ast_ctx->err->num_errors) {
		return JOB_ERROR;
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
discover_module_files(struct compile_ctx *ctx, struct stg_module *mod,
		struct ast_node *mod_root,
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

	// TODO: Allow modules with arbitrary depth.
#define DIR_NS_STACK_CAP 128
	struct ast_node *dir_ns_stack[DIR_NS_STACK_CAP];
	size_t dir_ns_head = 0;

	struct ast_node *mod_root_dt;
	mod_root_dt = ast_module_node_get_data_type(
			mod->vm, mod_root);

	dir_ns_stack[dir_ns_head] = mod_root_dt;

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
				struct ast_node *file_ns;

				if (atom == vm_atoms(ctx->vm, "mod") || f->fts_level == 0) {
					file_ns = dir_ns_stack[dir_ns_head];
				} else {
					file_ns = ast_namespace_add_ns(
							ctx->ast_ctx, mod->ast_mod,
							dir_ns_stack[dir_ns_head], atom);
				}

				if (num_unparsed_files) {
					*num_unparsed_files += 1;
				}

				DISPATCH_JOB(ctx, parse_file, COMPILE_PHASE_DISCOVER,
						.mod = mod,
						.scope = file_ns,
						.file_name = file_name,
						.num_unparsed_files = num_unparsed_files);
			} else if (has_extension(path, STR("module.so")))  {
				if (mod->has_native_module_ext) {
					stg_error(&ctx->err, STG_NO_LOC,
							"Found multiple native module extensions for module.");
					break;
				}

				mod->has_native_module_ext = true;
				string_duplicate(ctx->mem, &mod->native_module_ext, path);
			}
		} break;

		case FTS_D:
			// The root of the project should be the root name space.
			if (f->fts_level > 0) {
				assert(f->fts_namelen > 0);

				struct string name;
				name.text = f->fts_name;
				name.length = f->fts_namelen;

				// Discard directories named src as those probably contains c-code.
				if (string_equal(name, STR("src"))) {
					fts_set(ftsp, f, FTS_SKIP);
					break;
				}

				struct atom *atom = vm_atom(ctx->vm, name);

				assert(dir_ns_head < DIR_NS_STACK_CAP - 1);
				dir_ns_head += 1;
				dir_ns_stack[dir_ns_head] =
					ast_namespace_add_ns(
							ctx->ast_ctx, mod->ast_mod,
							dir_ns_stack[dir_ns_head-1], atom);
			}
			break;

		case FTS_DP:
			if (f->fts_level > 0) {
				assert(f->fts_namelen > 0);

				struct string name;
				name.text = f->fts_name;
				name.length = f->fts_namelen;

				// Discard directories named src as those probably contains c-code.
				if (string_equal(name, STR("src"))) {
					break;
				}

				assert(dir_ns_head > 0);
				dir_ns_head -= 1;
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
job_load_module(struct compile_ctx *ctx, job_load_module_t *data)
{
	struct stg_module *mod;
	mod = data->stg_mod;
	assert(mod->state == STG_MOD_LIFE_COMPILING);

	switch (data->state) {
		case JOB_LOAD_MODULE_DISCOVER:
			{
				assert(mod->name != NULL || mod->src_dir.length != 0);

				if (mod->name) {
					for (size_t i = 0; i < ctx->vm->num_precompiled_native_modules; i++) {
						struct stg_native_module *native_mod;
						native_mod = ctx->vm->precompiled_native_modules[i];
						if (native_mod->name == mod->name) {
							mod->native_mod = native_mod;
							break;
						}
					}
				}

				bool should_discover_files = true;

				if (mod->src_dir.length == 0) {
					bool module_found = false;

					char path_buffer[PATH_MAX + 1];
					for (size_t i = 0; i < ctx->vm->compile_options.num_module_locations; i++) {
						struct string base_dir = ctx->vm->compile_options.module_locations[i];

						size_t predicted_path_length =
							base_dir.length +
							mod->name->name.length + 2;

						if (predicted_path_length > PATH_MAX) {
							print_error("lookup module",
									"Module location path '%.*s' is too long. (%zu of max %zu)",
									base_dir.length + mod->name->name.length, PATH_MAX);
							continue;
						}
						size_t path_length =
							snprintf(path_buffer, PATH_MAX + 1,
								"%.*s/%.*s/", LIT(base_dir), ALIT(mod->name));

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

						mod->src_dir =
							string_duplicate_cstr(real_path_buffer);
						module_found = true;
					}

					if (!module_found && !mod->native_mod) {
						stg_error(&ctx->err, STG_NO_LOC, "Could not find module '%.*s'",
								ALIT(mod->name));
						return JOB_ERROR;
					}

					should_discover_files = module_found;
				}

				mod->ast_mod = arena_alloc(
						&mod->mem, sizeof(struct ast_module));
				ast_module_init(ctx->vm, mod->id, mod->ast_mod);

				ast_data_type_id root_data_type_id;
				root_data_type_id = ast_module_add_composite(
						ctx->ast_ctx, mod->ast_mod, STG_NO_LOC,
						AST_COMPOSITE_MODULE);

				data->mod_root = ast_init_node_data_type(
						ctx->ast_ctx, AST_NODE_NEW, STG_NO_LOC,
						mod->id, root_data_type_id);

				struct atom *base_mod_name = vm_atoms(ctx->vm, "base");

				if (mod->name != base_mod_name) {
					struct ast_node *root_data_type;
					root_data_type = ast_module_get_data_type(
							mod->ast_mod, root_data_type_id);
					root_data_type->composite.is_init_monad = true;

					vm_request_module(ctx->vm, mod->id,
							base_mod_name, VM_REQUEST_MOD_NO_LOC);

					struct ast_node *use_base_mod_node;
					use_base_mod_node = ast_init_node_mod(
							ctx->ast_ctx, AST_NODE_NEW, STG_NO_LOC,
							base_mod_name);

					struct ast_node *use_base_target_node;
					use_base_target_node = ast_init_node_access(
							ctx->ast_ctx, AST_NODE_NEW, STG_NO_LOC,
							use_base_mod_node,
							vm_atoms(ctx->vm, "prelude"));

					struct ast_node *mod_root_dt;
					mod_root_dt = ast_module_node_get_data_type(
							mod->vm, data->mod_root);

					ast_node_composite_add_use(
							ctx->ast_ctx, STG_NO_LOC,
							mod_root_dt,
							use_base_target_node,
							NULL);
				}


				if (should_discover_files) {
					discover_module_files(ctx, mod, data->mod_root,
							mod->src_dir,
							&data->num_unparsed_files);
				}

				if (!mod->native_mod && mod->has_native_module_ext) {
					mod->native_mod = stg_native_load_module_ext(
							ctx->vm, mod->native_module_ext);
				}

				if (!should_discover_files) {
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

			data->state = JOB_LOAD_MODULE_WAIT_FOR_DEPENDENCIES;
			// fallthrough

		case JOB_LOAD_MODULE_WAIT_FOR_DEPENDENCIES:
			for (size_t i = 0; i < mod->num_dependencies; i++) {
				struct stg_module *dep;
				dep = vm_get_module_by_id(ctx->vm, mod->dependencies[i]);
				if (!stg_mod_state_ok(dep->state)) {
					return JOB_YIELD;
				}
			}

			stg_mod_invoke_register(mod);
			stg_mod_invoke_pre_compile(ctx->ast_ctx, mod, data->mod_root);

			data->state = JOB_LOAD_MODULE_DONE;
			// fallthrough

		case JOB_LOAD_MODULE_DONE:
			{
				int err;
				err = ast_module_finalize(ctx->ast_ctx, mod, data->mod_root);

				if (err) {
					print_error("compile", "Failed to initialize module '%.*s'",
							ALIT(mod->name));
					return JOB_ERROR;
				}
			}

			assert(mod->state == STG_MOD_LIFE_COMPILING);
			mod->state = STG_MOD_LIFE_IDLE;

			return JOB_OK;
	}

	panic("Load module reached an invalid state.");
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

static void
stg_dispatch_load_modules(struct compile_ctx *ctx)
{
	for (size_t i = 0; i < ctx->vm->num_modules; i++) {
		struct stg_module *mod;
		mod = ctx->vm->modules[i];
		if (mod->state == STG_MOD_LIFE_PRE_COMPILE) {
			mod->state = STG_MOD_LIFE_COMPILING;

			DISPATCH_JOB(ctx, load_module, COMPILE_PHASE_DISCOVER,
					.stg_mod=mod);
		}
	}
}

int
stg_compile(struct vm *vm, struct ast_context *ast_ctx)
{
	struct compile_ctx ctx = {0};

	ctx.vm = vm;
	ctx.mem = &vm->memory;
	ctx.err.string_arena = &vm->memory;
	ctx.err.transient_arena = &vm->transient;
	ctx.ast_ctx = ast_ctx;

	paged_list_init(&ctx.jobs, &vm->mem,
			sizeof(struct complie_job));

	assert(!ctx.ast_ctx->err);
	ctx.ast_ctx->err = &ctx.err;

	stg_dispatch_load_modules(&ctx);

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
			stg_dispatch_load_modules(&ctx);
		}
	}

	paged_list_destroy(&ctx.jobs);

	if (ctx.num_jobs_failed > 0 || ctx.err.num_errors > 0) {
#if COMPILE_DEBUG_JOBS
		printf("\n");
		printf(TERM_COLOR_RED("Compilation failed! "
					"(%zu jobs failed, %zu errors)") "\n",
			   ctx.num_jobs_failed, ctx.err.num_errors);
#endif

		assert(ctx.err.num_errors > 0);

		print_errors(&ctx.err);
		ctx.ast_ctx->err = NULL;
		return -1;
	}

	print_errors(&ctx.err);

	if (ctx.err.num_errors > 0) {
		ctx.ast_ctx->err = NULL;
		return -1;
	}

	ctx.ast_ctx->err = NULL;
	return 0;
}

struct ast_dt_dependency {
	ast_member_id from, to;
	bool visited;
};

struct string ast_dt_job_names[] = {
	STR("FREE"),

#define JOB(name, ...) STR(#name),
	AST_DT_JOBS
#undef JOB
};

enum ast_dt_job_kind {
	AST_DT_JOB_FREE = 0,

#define JOB(name, ...) AST_DT_JOB_##name,
	AST_DT_JOBS
#undef JOB

	AST_DT_JOB_LENGTH
};

#if AST_DT_DEBUG_JOBS
static int
ast_dt_job_name_max_length()
{
	static int result = -1;
	if (result < 0) {
		int max = 0;
		for (size_t i = 0; i < AST_DT_JOB_LENGTH; i++) {
			max = ast_dt_job_names[i].length > max
				? ast_dt_job_names[i].length : max;
		}
		result = max;
	}
	return result;
}
#endif

struct ast_dt_job_dep {
	bool visited;
	ast_dt_job_id to;
};

struct ast_dt_job {
	enum ast_dt_job_kind kind;

	// Use to keep track of internal vertex id in cycle detection.
	int aux_id;
	bool failed;
	bool suspended;

	size_t num_incoming_deps;
	size_t num_outgoing_deps;
	struct ast_dt_job_dep *outgoing_deps;

	// Used for the linked list terminal_jobs in ast_dt_context.
	ast_dt_job_id terminal_jobs;

	union {
#define JOB(name, type) type name;
		AST_DT_JOBS
#undef JOB

		// Used when the job is not allocated.
		ast_dt_job_id free_list;
	} data;
};

struct ast_dt_job *
get_job(struct ast_dt_context *ctx, ast_dt_job_id id)
{
#if AST_DEBUG_UNINITIALIZED_JOB_ID
	assert(id > 0);
#endif
	return paged_list_get(&ctx->jobs, id);
}

static inline void
ast_dt_free_job(struct ast_dt_context *ctx, ast_dt_job_id id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, id);
	assert(job->kind != AST_DT_JOB_FREE);

	free(job->outgoing_deps);

	memset(job, 0, sizeof(struct ast_dt_job));
	job->kind = AST_DT_JOB_FREE;

	job->data.free_list = ctx->free_list;
	ctx->free_list = id;
}

static ast_dt_job_id
ast_dt_alloc_job(struct ast_dt_context *ctx)
{
	ast_dt_job_id res = -1;
	bool is_new = false;
	if (ctx->free_list != -1) {
		res = ctx->free_list;
	} else {
		res = paged_list_push(&ctx->jobs);
		is_new = true;
	}

	assert(res >= 0);

	struct ast_dt_job *job;
	job = get_job(ctx, res);

	if (!is_new) {
		ctx->free_list = job->data.free_list;
	}

	memset(job, 0, sizeof(struct ast_dt_job));

	job->data.free_list = -1;

	job->terminal_jobs = ctx->terminal_jobs;
	ctx->terminal_jobs = res;

	return res;
}

#define JOB(name, type)								\
	ast_dt_job_id									\
	ast_dt_job_##name(struct ast_dt_context *ctx,	\
			type value)								\
{													\
	ast_dt_job_id job_id;							\
	job_id = ast_dt_alloc_job(ctx);					\
													\
	struct ast_dt_job *job;							\
	job = get_job(ctx, job_id);						\
	job->kind = AST_DT_JOB_##name;					\
	job->data.name = value;							\
													\
	return job_id;									\
}
AST_DT_JOBS
#undef JOB

#if AST_DT_DEBUG_JOBS
ast_dt_job_id
ast_dt_job_nopf(struct ast_dt_context *ctx, ast_dt_job_id *id_ref, char *fmt, ...) {
	struct ast_dt_job_nop_data data = {0};
	data.id_ref = id_ref;

	va_list ap;

	va_start(ap, fmt);
	data.name = arena_vsprintf(ctx->tmp_mem, fmt, ap);
	va_end(ap);

	return ast_dt_job_nop(ctx, data);
}
#endif

enum ast_dtc_vertex_color {
	AST_DTC_WHITE = 0,
	AST_DTC_GRAY,
	AST_DTC_BLACK,
};

struct ast_dtc_vertex {
	ast_dt_job_id job_id;

	bool visited, assigned;
	struct ast_dtc_vertex *next_l, *next_in_component;

	struct ast_dtc_edge *outgoing_edges;
	size_t num_outgoing_edges;

	struct ast_dtc_edge *incoming_edges;
	size_t num_incoming_edges;
	size_t num_incoming_edges_filled;
};

struct ast_dtc_edge {
	struct ast_dtc_vertex *from, *to;
};

struct ast_dtc_component {
	struct ast_dtc_vertex *head;
};


struct ast_dtc_components {
	struct ast_dtc_component *components;
	size_t num_components;
};

struct ast_dtc_kosaraju {
	struct ast_dtc_vertex *head_l, *head_components;
};

static void
ast_dtc_kosaraju_visit(struct ast_dtc_kosaraju *ctx, struct ast_dtc_vertex *vert)
{
	if (vert->visited) {
		return;
	}

	vert->visited = true;

	for (size_t i = 0; i < vert->num_outgoing_edges; i++) {
		ast_dtc_kosaraju_visit(ctx, vert->outgoing_edges[i].to);
	}

	vert->next_l = ctx->head_l;
	ctx->head_l = vert;
}

static void
ast_dtc_kosaraju_assign(
		struct ast_dtc_kosaraju *ctx,
		struct ast_dtc_vertex *vert,
		struct ast_dtc_component *component)
{
	if (vert->assigned) {
		return;
	}

	vert->assigned = true;
	vert->next_in_component = component->head;
	component->head = vert;

	for (size_t i = 0; i < vert->num_incoming_edges; i++) {
		ast_dtc_kosaraju_assign(ctx,
				vert->incoming_edges[i].from, component);
	}
}

static struct ast_dtc_components
ast_dt_find_components(struct ast_dt_context *ctx, struct arena *mem)
{
	// Recreate the job dependency graph.

	size_t num_unfinished_jobs = 0;
	size_t num_unvisited_edges = 0;

	for (ast_dt_job_id job_i = 0; job_i < ctx->jobs.length; job_i++) {
		struct ast_dt_job *job;
		job = get_job(ctx, job_i);

		if (job->kind == AST_DT_JOB_FREE) {
			continue;
		}

		job->aux_id = num_unfinished_jobs;
		num_unfinished_jobs += 1;
		num_unvisited_edges += job->num_incoming_deps;
	}

	size_t num_vertices = num_unfinished_jobs;
	struct ast_dtc_vertex *vertices = arena_allocn(mem,
			num_vertices, sizeof(struct ast_dtc_vertex));

	struct ast_dtc_edge *incoming_edges = arena_allocn(mem,
			num_unvisited_edges, sizeof(struct ast_dtc_edge));
	struct ast_dtc_edge *outgoing_edges = arena_allocn(mem,
			num_unvisited_edges, sizeof(struct ast_dtc_edge));

	size_t unvisited_incoming_edge_i = 0;
	for (ast_dt_job_id job_i = 0; job_i < ctx->jobs.length; job_i++) {
		struct ast_dt_job *job;
		job = get_job(ctx, job_i);

		if (job->kind == AST_DT_JOB_FREE) {
			continue;
		}

		struct ast_dtc_vertex *vert;
		vert = &vertices[job->aux_id];

		vert->job_id = job_i;
		vert->num_outgoing_edges = job->num_outgoing_deps;
		vert->num_incoming_edges = job->num_incoming_deps;
		vert->incoming_edges = &incoming_edges[unvisited_incoming_edge_i];
		unvisited_incoming_edge_i += job->num_incoming_deps;
	}

	size_t num_unvisited_outgoing_edges = 0;
	for (size_t vert_i = 0; vert_i < num_unfinished_jobs; vert_i++) {
		struct ast_dtc_vertex *vert = &vertices[vert_i];
		struct ast_dt_job *job = get_job(ctx, vert->job_id);

		vert->outgoing_edges = &outgoing_edges[num_unvisited_outgoing_edges];

		for (size_t edge_i = 0; edge_i < job->num_outgoing_deps; edge_i++) {
			if (!job->outgoing_deps[edge_i].visited) {
				struct ast_dt_job *to_job;
				to_job = get_job(ctx, job->outgoing_deps[edge_i].to);

				struct ast_dtc_vertex *to;
				to = &vertices[to_job->aux_id];
				assert(to->num_incoming_edges_filled < to->num_incoming_edges);

				struct ast_dtc_edge *forward_edge, *back_edge;
				forward_edge = &outgoing_edges[num_unvisited_outgoing_edges];
				back_edge = &to->incoming_edges[to->num_incoming_edges_filled];

				forward_edge->from = vert;
				forward_edge->to = to;

				*back_edge = *forward_edge;

				num_unvisited_outgoing_edges += 1;
				to->num_incoming_edges_filled += 1;
			}
		}
	}

	assert(num_unvisited_edges == num_unvisited_outgoing_edges);

	// Perform Kosaraju's algorithm to find the strongly connected components
	// of the graph.
	struct ast_dtc_kosaraju comp_ctx = {0};
	for (size_t vert_i = 0; vert_i < num_vertices; vert_i++) {
		struct ast_dtc_vertex *vert = &vertices[vert_i];
		ast_dtc_kosaraju_visit(&comp_ctx, vert);
	}

	struct ast_dtc_component *comps;
	comps = arena_allocn(mem, num_vertices, sizeof(struct ast_dtc_component));
	size_t comps_i = 0;

	for (struct ast_dtc_vertex *vert = comp_ctx.head_l; vert; vert = vert->next_l) {
		struct ast_dtc_component comp = {0};
		ast_dtc_kosaraju_assign(&comp_ctx, vert, &comp);

		// We only keep components that contains at least two vertices (a head
		// and its next) because single node components can not contain cycles.
		if (comp.head && comp.head->next_in_component) {
			comps[comps_i] = comp;
			comps_i += 1;
		}
	}

	struct ast_dtc_components res = {0};
	res.num_components = comps_i;
	res.components = comps;

	return res;
}

static struct ast_dt_job_info
ast_dt_job_get_info(struct ast_dt_context *ctx, ast_dt_job_id job_id);

static struct stg_location
ast_dt_job_location(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job_info info;
	info = ast_dt_job_get_info(ctx, job_id);
	return info.loc;
}

#if AST_DT_DEBUG_JOBS
static struct string
ast_dt_job_kind_name(enum ast_dt_job_kind kind) {
	assert(kind < AST_DT_JOB_LENGTH);
	return ast_dt_job_names[kind];
}
#endif

#if AST_DT_DEBUG_JOBS
static void
ast_dt_print_job_desc(struct ast_dt_context *ctx,
		ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	printf(" 0x%03x %s %-*.*s|", job_id,
			job->suspended ? TC(TC_BRIGHT_MAGENTA, "s") : " ",
			ast_dt_job_name_max_length(),
			LIT(ast_dt_job_kind_name(job->kind)));

	struct ast_dt_job_info info;
	info = ast_dt_job_get_info(ctx, job_id);

	if (info.description.length > 0 && info.description.text) {
		printf("%.*s", LIT(info.description));
	}
}
#endif


void
ast_dt_report_cyclic_dependency_chain(struct ast_dt_context *ctx,
		struct ast_dtc_vertex *component)
{
	struct ast_dtc_vertex *it = component;

	stg_error(ctx->ast_ctx->err,
			ast_dt_job_location(ctx, it->job_id),
			"Found member dependency cycle. %03x", it->job_id);
	it = it->next_in_component;

	while (it) {
		stg_appendage(ctx->ast_ctx->err,
				ast_dt_job_location(ctx, it->job_id),
				"Through. %03x", it->job_id);
		it = it->next_in_component;
	}
}

static void
ast_dt_report_cyclic_dependencies(struct ast_dt_context *ctx)
{
	struct arena *mem = ctx->tmp_mem;
	arena_mark cp = arena_checkpoint(mem);

	struct ast_dtc_components comps;
	comps = ast_dt_find_components(ctx, mem);

	for (size_t i = 0; i < comps.num_components; i++) {
		ast_dt_report_cyclic_dependency_chain(
				ctx, comps.components[i].head);
	}

	arena_reset(mem, cp);
}

#if AST_DT_DEBUG_JOBS
static bool
ast_dtc_component_contains(
		struct ast_dtc_component *comp,
		struct ast_dtc_vertex *vert)
{
	struct ast_dtc_vertex *it = comp->head;
	while (it) {
		if (it == vert) {
			return true;
		}

		it = it->next_in_component;
	}

	return false;
}

static void
ast_dt_debug_print_cyclic_dependencies(struct ast_dt_context *ctx)
{
	struct arena *mem = ctx->tmp_mem;
	arena_mark cp = arena_checkpoint(mem);

	struct ast_dtc_components comps;
	comps = ast_dt_find_components(ctx, mem);

	for (size_t i = 0; i < comps.num_components; i++) {
		struct ast_dtc_component *comp = &comps.components[i];
		struct ast_dtc_vertex *it = comp->head;

		printf("\nCycle %zu/%zu:\n", i+1, comps.num_components);
		while (it) {
			printf(TC(TC_BRIGHT_YELLOW, "->") " ");
			ast_dt_print_job_desc(ctx, it->job_id);
			printf("\n");

			struct ast_dt_job *job;
			job = get_job(ctx, it->job_id);

			bool first_print;

			printf(TC_BRIGHT_YELLOW "       (");
			first_print = true;
			for (size_t edge_i = 0; edge_i < it->num_incoming_edges; edge_i++) {
				struct ast_dtc_vertex *in = it->incoming_edges[edge_i].from;
				if (ast_dtc_component_contains(comp, in)) {
					printf("%s0x%03x", first_print ? "" : ", ", in->job_id);
					first_print = false;
				}
			}

			printf(") -> 0x%03x -> (", it->job_id);
			first_print = true;
			for (size_t edge_i = 0; edge_i < it->num_outgoing_edges; edge_i++) {
				struct ast_dtc_vertex *out = it->outgoing_edges[edge_i].to;
				if (ast_dtc_component_contains(comp, out)) {
					printf("%s0x%03x", first_print ? "" : ", ", out->job_id);
					first_print = false;
				}
			}
			printf(")" TC_CLEAR "\n");

			it = it->next_in_component;
		}
	}

	fflush(stdout);
	arena_reset(mem, cp);
}
#endif

static inline int
ast_dt_dispatch_job(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);

#if AST_DT_DEBUG_JOBS
	printf("%03zx "TC(TC_BRIGHT_YELLOW, "==>") " ", ctx->run_i);
	ast_dt_print_job_desc(ctx, job_id);
	printf("\n");
#endif

	int err = -1;

	switch (job->kind) {
		case AST_DT_JOB_FREE:
			panic("Attempted to dispatch a freed job.");
			break;

		case AST_DT_JOB_LENGTH:
			panic("Invalid job kind");
			break;

#define JOB(name, type) 													\
		case AST_DT_JOB_##name:												\
			err = ast_dt_job_dispatch_##name(ctx, job_id, job->data.name);	\
			break;
	AST_DT_JOBS
#undef JOB
	}

	return err;
}

static struct ast_dt_job_info
ast_dt_job_get_info(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);

	struct ast_dt_job_info res = {0};

	switch (job->kind) {
		case AST_DT_JOB_FREE:
			panic("Attempted to fetch info for a freed job.");
			break;

		case AST_DT_JOB_LENGTH:
			panic("Invalid job kind");
			break;

#define JOB(name, type) 													\
		case AST_DT_JOB_##name:												\
			res = ast_dt_job_get_info_##name(ctx, job_id, job->data.name);	\
			break;
	AST_DT_JOBS
#undef JOB
	}

	return res;
}

#if 0
#define JOB(name, type)						\
	void									\
	ast_dt_job_remove_from_target_##name(	\
			struct ast_dt_context *, ast_dt_job_id, type);
AST_DT_JOBS
#undef JOB
*/

static void
ast_dt_remove_job_from_target(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);

	switch (job->kind) {
		case AST_DT_JOB_FREE:
			panic("Tried to remove freed job.");
			break;

		case AST_DT_JOB_LENGTH:
			panic("Invalid job kind");
			break;

#define JOB(name, type) 																\
		case AST_DT_JOB_##name:															\
			ast_dt_job_remove_from_target_##name(ctx, job_id, job->data.name);	\
			break;
	AST_DT_JOBS
#undef JOB
	}
}
#endif

static void
ast_dt_remove_job_from_target(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job_info info;
	info = ast_dt_job_get_info(ctx, job_id);

	if (info.num_targets > 1) {
		for (size_t i = 0; i < info.num_targets; i++) {
			if (info.targets[i]) {
				assert(*info.targets[i] == job_id);
				*info.targets[i] = -1;
			}
		}
	} else if (info.target) {
		assert(*info.target == job_id);
		*info.target = -1;
	}
}

static void
ast_dt_job_remove_from_terminal_jobs(struct ast_dt_context *ctx,
		ast_dt_job_id target_id)
{
	struct ast_dt_job *target;
	target = get_job(ctx, target_id);

	for (ast_dt_job_id *job_id = &ctx->terminal_jobs;
			(*job_id) >= 0;
			job_id = &get_job(ctx, *job_id)->terminal_jobs) {
		if ((*job_id) == target_id) {
			(*job_id) = target->terminal_jobs;
			target->terminal_jobs = -1;
			return;
		}
	}
}


void
ast_dt_job_suspend(struct ast_dt_context *ctx, ast_dt_job_id target_id)
{
	assert(target_id > -1);

	struct ast_dt_job *target;
	target = get_job(ctx, target_id);

	if (target->suspended) {
		return;
	}

#if AST_DT_DEBUG_JOBS
		printf("%03zx " TC(TC_BRIGHT_MAGENTA, "sus") " ", ctx->run_i);
		ast_dt_print_job_desc(ctx, target_id);
		printf("\n");
#endif

	target->suspended = true;

	if (target->num_incoming_deps == 0) {
		ast_dt_job_remove_from_terminal_jobs(ctx, target_id);
	}
}

void
ast_dt_job_resume(struct ast_dt_context *ctx, ast_dt_job_id target_id)
{
	assert(target_id > -1);

	struct ast_dt_job *target;
	target = get_job(ctx, target_id);

	if (!target->suspended) {
		return;
	}

#if AST_DT_DEBUG_JOBS
		printf("%03zx " TC(TC_BRIGHT_GREEN, "res") " ", ctx->run_i);
		ast_dt_print_job_desc(ctx, target_id);
		printf("\n");
#endif

	target->suspended = false;

	if (target->num_incoming_deps == 0) {
		assert(target->terminal_jobs == -1);

		target->terminal_jobs = ctx->terminal_jobs;
		ctx->terminal_jobs = target_id;
	}
}

// Requests that from must be evaluated before to.
void
ast_dt_job_dependency(struct ast_dt_context *ctx,
		ast_dt_job_id from_id, ast_dt_job_id to_id)
{
	if (from_id < 0) {
		// The dependecy was already completed.
#if AST_DT_DEBUG_JOBS
		printf("%03zx " TC(TC_BRIGHT_BLUE, "dep") " "
				TC(TC_BRIGHT_GREEN, "(completed)"), ctx->run_i);
		// ast_dt_print_job_desc(ctx, from_id);
		// Move the cursor to column 80 to align the dependent jobs.
		printf("\033[80G " TC(TC_BRIGHT_BLUE, "->") " ");
		ast_dt_print_job_desc(ctx, to_id);
		printf("\n");
#endif
		return;
	}

	assert(from_id != to_id);

	struct ast_dt_job *from, *to;
	from = get_job(ctx, from_id);
	to = get_job(ctx, to_id);

	assert(from->kind != AST_DT_JOB_FREE);

	for (size_t i = 0; i < from->num_outgoing_deps; i++) {
		if (from->outgoing_deps[i].to == to_id) {
			return;
		}
	}

#if AST_DT_DEBUG_JOBS
	printf("%03zx " TC(TC_BRIGHT_BLUE, "dep") " ", ctx->run_i);
	ast_dt_print_job_desc(ctx, from_id);
	// Move the cursor to column 80 to align the dependent jobs.
	printf("\033[80G " TC(TC_BRIGHT_BLUE, "->") " ");
	ast_dt_print_job_desc(ctx, to_id);
	printf("\n");
#endif

	struct ast_dt_job_dep dep = {0};
	dep.visited = false;
	dep.to = to_id;

	dlist_append(
			from->outgoing_deps,
			from->num_outgoing_deps,
			&dep);

	if (to->num_incoming_deps == 0) {
		ast_dt_job_remove_from_terminal_jobs(ctx, to_id);
	}

	assert(to->terminal_jobs < 0);

	to->num_incoming_deps += 1;
	ctx->unvisited_job_deps += 1;
}

static int
ast_dt_run_jobs(struct ast_dt_context *ctx)
{
	int failed_jobs = 0;

	// Dispatch the jobs in topological order. (Kahn's algorithm.)
	while (ctx->terminal_jobs >= 0) {
		ast_dt_job_id job_id;
		struct ast_dt_job *job;

		job_id = ctx->terminal_jobs;
		job = get_job(ctx, job_id);
		ctx->terminal_jobs = job->terminal_jobs;
		job->terminal_jobs = -1;

		if (job->failed) {
			ast_dt_remove_job_from_target(ctx, job_id);
			ast_dt_free_job(ctx, job_id);
			continue;
		}

		int err;
		err = ast_dt_dispatch_job(ctx, job_id);
		if (err < 0) {
#if AST_DT_DEBUG_JOBS
			printf(TC(TC_BRIGHT_RED, "job failed!") "\n");
#endif

			failed_jobs += 1;

			if (job->num_incoming_deps > 0) {
				// The job prepared itself to yield, but failed instead.
				// Because other jobs now have outgoing dependencies on this
				// node we have to allow it to pass through again and
				// immediatly fail on the next dispatch.

				job->failed = true;
				continue;
			}

			ast_dt_remove_job_from_target(ctx, job_id);
			ast_dt_free_job(ctx, job_id);
			continue;
		}

		if (job->num_incoming_deps > 0) {
#if AST_DT_DEBUG_JOBS
			printf(TC(TC_BRIGHT_YELLOW, "yield") "\n");
#endif
			// If the node gave itself new dependencies we don't mark it as
			// visited to allow it to pass through again.
			continue;
		}

		assert(!err);

		for (size_t i = 0; i < job->num_outgoing_deps; i++) {
			struct ast_dt_job_dep *dep;
			dep = &job->outgoing_deps[i];

			if (!dep->visited) {
				dep->visited = true;

				struct ast_dt_job *to;
				to = get_job(ctx, dep->to);

				assert(to->kind != AST_DT_JOB_FREE);
				assert(to->num_incoming_deps > 0);
				to->num_incoming_deps -= 1;

				assert(ctx->unvisited_job_deps > 0);
				ctx->unvisited_job_deps -= 1;

				if (to->num_incoming_deps == 0 && !to->suspended) {
					to->terminal_jobs = ctx->terminal_jobs;
					ctx->terminal_jobs = dep->to;
				}
			}
		}

		ast_dt_remove_job_from_target(ctx, job_id);
		ast_dt_free_job(ctx, job_id);
	}

	if (failed_jobs) {
		return -1;
	}

	if (ctx->unvisited_job_deps > 0) {
		ast_dt_report_cyclic_dependencies(ctx);

		printf("Failed to evalutate datatype because we found one or more cycles.\n");
#if AST_DT_DEBUG_JOBS
		printf("Problematic jobs: \n");
		for (ast_dt_job_id job_i = 0; job_i < ctx->jobs.length; job_i++) {
			struct ast_dt_job *job;
			job = get_job(ctx, job_i);
			if (job->kind != AST_DT_JOB_FREE) {
				printf(" - 0x%03x (%zu):", job_i, job->num_incoming_deps);

				// Find all refs to this job.
				for (ast_dt_job_id job_j = 0; job_j < ctx->jobs.length; job_j++) {
					struct ast_dt_job *other;
					other = get_job(ctx, job_j);
					if (other->kind == AST_DT_JOB_FREE) {
						continue;
					}
					for (size_t i = 0; i < other->num_outgoing_deps; i++) {
						if (other->outgoing_deps[i].to == job_i) {
							printf(" 0x%03x", job_j);
						}
					}
				}

				printf("\n");
			}
		}

		printf("\n");
		ast_dt_debug_print_cyclic_dependencies(ctx);
#endif
		return -1;
	}

	return 0;
}

int
ast_dt_process(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_dt_context dt_ctx = {0};
	dt_ctx.ast_ctx = ctx;
	dt_ctx.mod = mod;

	dt_ctx.tmp_mem = &ctx->vm->transient;
	arena_mark transient_cp = arena_checkpoint(dt_ctx.tmp_mem);

	// The jobs table is currently defined here because it is not available in
	// ast_datatype.c where cpl_dt_init_context is defined.
	paged_list_init(
			&dt_ctx.jobs, &ctx->vm->mem,
			sizeof(struct ast_dt_job));

#if AST_DEBUG_UNINITIALIZED_JOB_ID
	size_t empty_job_id = paged_list_push(&dt_ctx.jobs);
	assert(empty_job_id == 0);
#endif

	cpl_dt_init_context(&dt_ctx);

	dt_ctx.impl_targets_resolved =
		ast_dt_job_nopf(&dt_ctx, &dt_ctx.impl_targets_resolved,
				"impl targets resolved");

#if AST_DT_DEBUG_JOBS
	static size_t next_run_i = 0;
	dt_ctx.run_i  = next_run_i++;

	printf("\nbegin composite %zu\n", dt_ctx.run_i);
#endif

	for (ast_data_type_id i = 0; i < mod->ast_mod->data_types.length; i++) {
		struct ast_node *node;
		node = ast_module_get_data_type(mod->ast_mod, i);

		ast_dt_register_composite(
				&dt_ctx, node, true, NULL, 0);
	}

	int err;
	err = ast_dt_run_jobs(&dt_ctx);
#if AST_DT_DEBUG_JOBS
	if (err) {
		printf("One or more jobs failed when resolving datastructure.\n");
	}
#endif

	cpl_dt_destroy_context(&dt_ctx);

	for (size_t job_i = 0; job_i < dt_ctx.jobs.length; job_i++) {
#if AST_DEBUG_UNINITIALIZED_JOB_ID
		if (job_i == 0) {
			continue;
		}
#endif
		struct ast_dt_job *job;
		job = get_job(&dt_ctx, job_i);
		free(job->outgoing_deps);
	}

	paged_list_destroy(&dt_ctx.jobs);

	arena_reset(dt_ctx.tmp_mem, transient_cp);

#if AST_DT_DEBUG_JOBS
	printf("end composite\n\n");
	// print_type_repr(ctx->vm, vm_get_type(ctx->vm, result));
	// printf("\n\n");
#endif

	return err;
}
