#include "vm.h"
#include "ast.h"
#include "dlist.h"
#include "module.h"
#include "base/mod.h"
#include <stdlib.h>
#include <string.h>

// For sysconf
#include <unistd.h>

// For mmap
#include <sys/mman.h>

#define AST_DT_DEBUG_JOBS 0

typedef int ast_dt_job_id;

struct ast_dt_expr_jobs {
	ast_dt_job_id resolve_names;
	ast_dt_job_id resolve_types;
	ast_dt_job_id codegen;
};

struct ast_dt_bind {
	struct ast_node *target_node;

	union {
		ast_member_id single_target;
		ast_member_id *multiple_targets;
	};
	int *target_descendent_id;

	// A function for eatch target that, given this bind's value, returns the
	// appropriate value for that target.
	func_id *target_unpack_func;
	func_id _single_target_unpack_func;

	size_t num_targets;

	// The targets that are found by l-expr during BIND_TARGET_RESOLVE_NAMES.
	// These members will be expanded to only contain terminal nodes that are
	// stored in single_target or multiple_targets.
	ast_member_id *explicit_targets;
	size_t num_explicit_targets;

	bool is_type_giving;

	// TODO: For now there is no support for multiple explicit targets.
	// Therefore we just use this field as pointer target for explicit_targets,
	// and set num_explicit_targets to 1 in BIND_TARGET_RESOLVE_NAMES.
	ast_member_id _single_explicit_target;

	bool overridable;

	enum ast_object_def_bind_kind kind;
	union {
		struct {
			struct ast_node *node;
			func_id func;
		} value;
		struct ast_object_def *pack;
		struct object const_value;
	};

	struct stg_location loc;

	ast_member_id *member_deps;
	size_t num_member_deps;

	struct {
		ast_dt_job_id resolve_names;
		ast_dt_job_id resolve;
	} target_jobs;
	struct ast_dt_expr_jobs value_jobs;
	struct ast_dt_bind *next_alloced;
};

struct ast_dt_dependency;

enum ast_dt_member_flags {
	AST_DT_MEMBER_IS_LOCAL = 0x1,
	AST_DT_MEMBER_IS_CONST = 0x2,
};

struct ast_dt_member {
	struct atom *name;
	type_id type;
	struct ast_node *type_node;

	struct stg_location decl_loc;
	struct ast_dt_bind *bound;
	struct ast_dt_bind *overridden_bind;

	enum ast_dt_member_flags flags;
	union {
		// If flags IS_LOCAL is true.
		ast_member_id first_child;

		// If flags IS_LOCAL is false.
		ast_member_id anscestor_local_member;
	};

	ast_dt_job_id type_resolved;
	ast_dt_job_id const_resolved;
	struct ast_dt_expr_jobs type_jobs;

	struct object const_value;

	ast_member_id persistant_id;

	// Used as part of the terminal_nodes linked list until after the member
	// has been sorted. Then next becomes the id of the next member in the
	// topological order. The index of another member.
	ast_member_id next;
};

struct ast_dt_dependency {
	ast_member_id from, to;
	bool visited;
};

enum ast_dt_job_kind {
	AST_DT_JOB_FREE = 0,
	AST_DT_JOB_NOP,

	AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES,
	AST_DT_JOB_MBR_TYPE_RESOLVE_TYPES,
	AST_DT_JOB_MBR_TYPE_EVAL,
	AST_DT_JOB_MBR_CONST_EVAL,

	AST_DT_JOB_BIND_RESOLVE_NAMES,
	AST_DT_JOB_BIND_RESOLVE_TYPES,
	AST_DT_JOB_BIND_CODEGEN,

	AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES,
	AST_DT_JOB_BIND_TARGET_RESOLVE,
};

struct ast_dt_job_dep {
	bool visited;
	ast_dt_job_id to;
};

struct ast_dt_job {
	enum ast_dt_job_kind kind;

	size_t num_incoming_deps;
	size_t num_outgoing_deps;
	struct ast_dt_job_dep *outgoing_deps;

	// Used for the linked list terminal_jobs in ast_dt_context.
	ast_dt_job_id terminal_jobs;

	union {
		// For nop
		ast_dt_job_id *id_loc;

		// For AST_DT_JOB_EXPR_TYPE
		ast_member_id member;

		// For AST_DT_JOB_EXPR_BIND and AST_DT_JOB_EXPR_TARGET
		struct ast_dt_bind *bind;

		// Used when the job is not allocated.
		ast_dt_job_id free_list;
	};
};

struct ast_dt_context {
	struct ast_dt_member *members;
	size_t num_members;

	struct ast_node *root_node;
	// An array of the ids of all local members, in the same order as
	// root_node->composite.members. The lenght is root_node->composite.num_members.
	ast_member_id *local_member_ids;

	struct ast_dt_bind *alloced_binds;

	// A linked list of all nodes with no incoming edges. The index of another
	// member.
	ast_member_id terminal_nodes;
	size_t unvisited_edges;

	struct ast_dt_job **job_pages;
	size_t num_job_pages;
	size_t page_size;
	ast_dt_job_id free_list;
	// A linked list of nodes that have no incoming edges.
	ast_dt_job_id terminal_jobs;
	size_t unvisited_job_deps;

	ast_dt_job_id target_names_resolved;

	struct ast_context *ast_ctx;
	struct ast_env     *ast_env;
	struct ast_module  *ast_mod;

	struct ast_typecheck_closure *closures;
	size_t num_closures;

	size_t num_errors;

#if AST_DT_DEBUG_JOBS
	// A run # that allows us to see more clearly what struct each job belongs
	// to when debugging jobs.
	size_t run_i;
#endif
};

static inline struct ast_dt_job *
get_job(struct ast_dt_context *ctx, ast_dt_job_id id)
{
	assert(ctx->page_size > 0);
	size_t jobs_per_page = ctx->page_size / sizeof(struct ast_dt_job);
	assert(id >= 0 && id < jobs_per_page * ctx->num_job_pages);

	return &ctx->job_pages[id / jobs_per_page][id % jobs_per_page];
}

static inline struct ast_dt_member *
get_member(struct ast_dt_context *ctx, ast_member_id id)
{
	assert(id < ctx->num_members);
	return &ctx->members[id];
}

static inline ast_member_id *
ast_dt_get_bind_targets(struct ast_dt_bind *bind)
{
	if (bind->num_targets == 0) {
		return NULL;
	} else if (bind->num_targets == 1) {
		return &bind->single_target;
	} else {
		return bind->multiple_targets;
	}
}

static inline void
ast_dt_free_job(struct ast_dt_context *ctx, ast_dt_job_id id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, id);
	assert(job->kind != AST_DT_JOB_FREE);
	memset(job, 0, sizeof(struct ast_dt_job));
	job->kind = AST_DT_JOB_FREE;

	job->free_list = ctx->free_list;
	ctx->free_list = id;
}

static ast_dt_job_id
ast_dt_alloc_job(struct ast_dt_context *ctx)
{
	if (ctx->page_size == 0) {
		ctx->page_size = sysconf(_SC_PAGESIZE);
	}

	size_t jobs_per_page = ctx->page_size / sizeof(struct ast_dt_job);

	if (ctx->job_pages == NULL || ctx->free_list == -1) {
		size_t new_size = (ctx->num_job_pages + 1) * sizeof(struct ast_dt_job *);
		struct ast_dt_job **new_pages = realloc(ctx->job_pages, new_size);

		if (!new_pages) {
			perror("realloc");
			return -1;
		}

		ctx->job_pages = new_pages;

		struct ast_dt_job *new_jobs;
		new_jobs = mmap(
				NULL, ctx->page_size,
				PROT_READ|PROT_WRITE,
				MAP_PRIVATE|MAP_ANONYMOUS,
				-1, 0);

		new_pages[ctx->num_job_pages] = new_jobs;

		if (new_pages[ctx->num_job_pages] == MAP_FAILED) {
			perror("mmap");
			return -1;
		}

		ctx->num_job_pages += 1;

		size_t first_in_page = jobs_per_page * (ctx->num_job_pages-1);

		for (size_t i = 0; i < jobs_per_page-1; i++) {
			new_jobs[i].free_list = first_in_page+i+1;
		}

		new_jobs[jobs_per_page-1].free_list = -1;
		ctx->free_list = jobs_per_page * (ctx->num_job_pages-1);
	}

	assert(ctx->free_list >= 0);

	ast_dt_job_id res;
	res = ctx->free_list;

	struct ast_dt_job *job;
	job = get_job(ctx, res);

	ctx->free_list = job->free_list;

	memset(job, 0, sizeof(struct ast_dt_job));

	job->free_list = -1;

	job->terminal_jobs = ctx->terminal_jobs;
	ctx->terminal_jobs = res;

	return res;
}

static ast_dt_job_id
ast_dt_job_nop(struct ast_dt_context *ctx,
		ast_dt_job_id *loc)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = AST_DT_JOB_NOP;
	job->id_loc = loc;

	return job_id;
}

static ast_dt_job_id
ast_dt_job_type(struct ast_dt_context *ctx,
		ast_member_id mbr_id, enum ast_dt_job_kind kind)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = kind;

	job->member = mbr_id;

	return job_id;
}

static ast_dt_job_id
ast_dt_job_bind(struct ast_dt_context *ctx,
		struct ast_dt_bind *bind, enum ast_dt_job_kind kind)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = kind;
	job->bind = bind;

	return job_id;
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

#if AST_DT_DEBUG_JOBS
static const char *
ast_dt_job_kind_name(enum ast_dt_job_kind kind) {
	switch (kind) {
		case AST_DT_JOB_NOP:
			return "NOP";
		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
			return "MBR TPE NAMES";
		case AST_DT_JOB_MBR_TYPE_RESOLVE_TYPES:
			return "MBR TPE TYPES";
		case AST_DT_JOB_MBR_TYPE_EVAL:
			return "MBR TPE EVAL";
		case AST_DT_JOB_MBR_CONST_EVAL:
			return "MBR CONST EVAL";
		case AST_DT_JOB_BIND_RESOLVE_NAMES:
			return "BND VAL NAMES";
		case AST_DT_JOB_BIND_RESOLVE_TYPES:
			return "BND VAL TYPES";
		case AST_DT_JOB_BIND_CODEGEN:
			return "BND VAL CODEGEN";
		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			return "BND TAR NAMES";
		case AST_DT_JOB_BIND_TARGET_RESOLVE:
			return "BND TAR RESOLVE";

		case AST_DT_JOB_FREE:
			return "FREE";
	}
	return "(unknown)";
}
#endif

#if AST_DT_DEBUG_JOBS
static void
ast_dt_print_job_desc(struct ast_dt_context *ctx,
		ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	printf("0x%03x (%-15s: ", job_id,
			ast_dt_job_kind_name(job->kind));

	if (job->kind == AST_DT_JOB_FREE) {
		return;
	}

	switch (job->kind) {
		case AST_DT_JOB_FREE:
			panic("Printing description of freed job.");
			break;

		case AST_DT_JOB_NOP:
			break;

		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
		case AST_DT_JOB_MBR_TYPE_RESOLVE_TYPES:
		case AST_DT_JOB_MBR_TYPE_EVAL:
		case AST_DT_JOB_MBR_CONST_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				printf("mbr 0x%03x[%-10.*s]",
						job->member, ALIT(mbr->name));
			}
			break;

		case AST_DT_JOB_BIND_RESOLVE_NAMES:
		case AST_DT_JOB_BIND_RESOLVE_TYPES:
		case AST_DT_JOB_BIND_CODEGEN:
		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
		case AST_DT_JOB_BIND_TARGET_RESOLVE:
				printf("bind ");
				ast_print_node(ctx->ast_ctx, ctx->ast_env,
						job->bind->target_node);
			break;
	}
	printf(")");
}
#endif

// Requests that from must be evaluated before to.
static void
ast_dt_job_dependency(struct ast_dt_context *ctx,
		ast_dt_job_id from_id, ast_dt_job_id to_id)
{
	if (from_id < 0) {
		// The dependecy was already completed.
		return;
	}

	assert(from_id != to_id);

	struct ast_dt_job *from, *to;
	from = get_job(ctx, from_id);
	to = get_job(ctx, to_id);

	for (size_t i = 0; i < from->num_outgoing_deps; i++) {
		if (from->outgoing_deps[i].to == to_id) {
			return;
		}
	}

#if AST_DT_DEBUG_JOBS
	printf("%03zx job dep ", ctx->run_i);
	ast_dt_print_job_desc(ctx, from_id);
	// Move the cursor to column 50 to align the dependent jobs.
	printf("\033[60G -> ");
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

static void
ast_dt_bind_to_member(struct ast_dt_context *ctx,
		struct ast_dt_bind *bind, ast_member_id mbr_id)
{
	struct ast_dt_member *member;
	member = get_member(ctx, mbr_id);
	if (member->bound) {
		if (!member->bound->overridable && !bind->overridable) {
			stg_error(ctx->ast_ctx->err, STG_NO_LOC,
					"'%.*s' is bound multiple times.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return;
		}

		if (member->bound->overridable && bind->overridable) {
			stg_error(ctx->ast_ctx->err, STG_NO_LOC,
					"'%.*s' has multiple default binds.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return;
		}

		if (bind->overridable) {
			member->overridden_bind = bind;
		} else {
			member->overridden_bind = member->bound;
			member->bound = bind;
		}
	} else {
		member->bound = bind;
	}
}

static struct ast_dt_bind *
ast_dt_register_bind(struct ast_dt_context *ctx,
		struct stg_location loc, struct ast_node *target,
		struct ast_node *value, bool overridable)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));

	new_bind->target_node = target;
	new_bind->kind = AST_OBJECT_DEF_BIND_VALUE;
	new_bind->value.node = value;
	new_bind->overridable = overridable;
	new_bind->loc = loc;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	new_bind->value_jobs.resolve_names =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_RESOLVE_NAMES);

	new_bind->value_jobs.resolve_types =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_RESOLVE_TYPES);

	new_bind->value_jobs.codegen =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_CODEGEN);

	ast_dt_job_dependency(ctx,
			new_bind->value_jobs.resolve_names,
			new_bind->value_jobs.resolve_types);

	ast_dt_job_dependency(ctx,
			new_bind->value_jobs.resolve_types,
			new_bind->value_jobs.codegen);

	new_bind->target_jobs.resolve_names =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES);

	new_bind->target_jobs.resolve =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_TARGET_RESOLVE);

	ast_dt_job_dependency(ctx,
			new_bind->target_jobs.resolve_names,
			new_bind->target_jobs.resolve);

	ast_dt_job_dependency(ctx,
			new_bind->value_jobs.resolve_types,
			new_bind->target_jobs.resolve);

	ast_dt_job_dependency(ctx,
			new_bind->target_jobs.resolve_names,
			ctx->target_names_resolved);

	return new_bind;
}

static struct ast_dt_bind *
ast_dt_register_typegiving_bind(struct ast_dt_context *ctx,
		struct stg_location loc, struct ast_node *target,
		ast_member_id target_member, struct ast_node *value,
		bool overridable)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));

	new_bind->_single_explicit_target = target_member;
	new_bind->explicit_targets = &new_bind->_single_explicit_target;
	new_bind->num_explicit_targets = 1;
	new_bind->is_type_giving = true;

	new_bind->target_node = target;
	new_bind->kind = AST_OBJECT_DEF_BIND_VALUE;
	new_bind->value.node = value;
	new_bind->overridable = overridable;
	new_bind->loc = loc;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;


	new_bind->value_jobs.resolve_names =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_RESOLVE_NAMES);

	new_bind->value_jobs.resolve_types =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_RESOLVE_TYPES);

	new_bind->value_jobs.codegen =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_CODEGEN);

	ast_dt_job_dependency(ctx,
			new_bind->value_jobs.resolve_names,
			new_bind->value_jobs.resolve_types);

	ast_dt_job_dependency(ctx,
			new_bind->value_jobs.resolve_types,
			new_bind->value_jobs.codegen);

	new_bind->target_jobs.resolve_names =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES);
	new_bind->target_jobs.resolve =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_BIND_TARGET_RESOLVE);

	ast_dt_job_dependency(ctx,
			new_bind->value_jobs.resolve_types,
			new_bind->target_jobs.resolve);

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, target_member);

	assert(mbr->type_resolved < 0);
	mbr->type_resolved = new_bind->target_jobs.resolve;

	ast_dt_job_dependency(ctx,
			mbr->type_resolved,
			mbr->const_resolved);

	ast_dt_job_dependency(ctx,
			new_bind->value_jobs.codegen,
			mbr->const_resolved);

	return new_bind;
}

static struct ast_dt_bind *
ast_dt_register_bind_func(struct ast_dt_context *ctx,
		ast_member_id target, func_id unpack_func, func_id func,
		ast_member_id *value_params, size_t num_value_params,
		bool overridable)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));


	new_bind->num_targets = 1;
	new_bind->single_target = target;
	new_bind->_single_target_unpack_func = unpack_func;
	new_bind->target_unpack_func = &new_bind->_single_target_unpack_func;
	new_bind->kind = AST_OBJECT_DEF_BIND_VALUE;
	new_bind->value.func = func;
	new_bind->overridable = overridable;
	new_bind->member_deps = value_params;
	new_bind->num_member_deps = num_value_params;

	for (size_t i = 0; i < num_value_params; i++) {
		assert(value_params[i] >= 0);
	}

	new_bind->value_jobs.resolve_names = -1;
	new_bind->value_jobs.resolve_types = -1;
	new_bind->value_jobs.codegen = -1;
	new_bind->target_jobs.resolve_names = -1;
	new_bind->target_jobs.resolve = -1;

	new_bind->loc = STG_NO_LOC;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	ast_dt_bind_to_member(
			ctx, new_bind, target);

	return new_bind;
}

static struct ast_dt_bind *
ast_dt_register_bind_const(struct ast_dt_context *ctx,
		ast_member_id target, func_id unpack_func,
		struct object value, bool overridable)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));

	new_bind->num_targets = 1;
	new_bind->single_target = target;
	new_bind->_single_target_unpack_func = unpack_func;
	new_bind->target_unpack_func = &new_bind->_single_target_unpack_func;
	new_bind->kind = AST_OBJECT_DEF_BIND_CONST;
	new_bind->const_value = value;
	new_bind->overridable = overridable;
	new_bind->member_deps = NULL;
	new_bind->num_member_deps = 0;

	new_bind->value_jobs.resolve_names = -1;
	new_bind->value_jobs.resolve_types = -1;
	new_bind->value_jobs.codegen = -1;
	new_bind->target_jobs.resolve_names = -1;
	new_bind->target_jobs.resolve = -1;

	new_bind->loc = STG_NO_LOC;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	ast_dt_bind_to_member(
			ctx, new_bind, target);

	struct ast_dt_member *member;
	member = get_member(ctx, target);

	assert_type_equals(ctx->ast_ctx->vm,
			member->type, new_bind->const_value.type);

	return new_bind;
}

static struct ast_dt_bind *
ast_dt_register_bind_pack(struct ast_dt_context *ctx,
		ast_member_id target, func_id unpack_func, struct ast_object_def *pack,
		ast_member_id *value_params, size_t num_value_params)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));

	new_bind->num_targets = 1;
	new_bind->single_target = target;
	new_bind->_single_target_unpack_func = unpack_func;
	new_bind->target_unpack_func = &new_bind->_single_target_unpack_func;
	new_bind->kind = AST_OBJECT_DEF_BIND_PACK;
	new_bind->pack = pack;
	new_bind->overridable = false;
	new_bind->member_deps = value_params;
	new_bind->num_member_deps = num_value_params;

	new_bind->value_jobs.resolve_names = -1;
	new_bind->value_jobs.resolve_types = -1;
	new_bind->value_jobs.codegen = -1;
	new_bind->target_jobs.resolve_names = -1;
	new_bind->target_jobs.resolve = -1;

	new_bind->loc = STG_NO_LOC;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	struct ast_dt_member *member;
	member = get_member(ctx, target);

	for (size_t i = 0; i < num_value_params; i++) {
		assert(value_params[i] >= 0);
		struct ast_dt_member *dep;
		dep = get_member(ctx, value_params[i]);
		ast_dt_job_dependency(ctx,
				dep->type_resolved,
				new_bind->value_jobs.resolve_types);
		ast_dt_job_dependency(ctx,
				dep->const_resolved,
				member->const_resolved);
	}


	type_id type;
	int err;
	err = ast_slot_pack_type(
			ctx->ast_ctx, ctx->ast_mod, &pack->env,
			pack->ret_type, &type);
	if (err) {
		printf("Failed to pack return type for pack bind.\n");
		return new_bind;
	}

	assert_type_equals(ctx->ast_ctx->vm,
			member->type, type);

	if (member->bound) {
		panic("Object with obj_def was already bound.");
	} else {
		member->bound = new_bind;
	}

	return new_bind;
}

static ast_member_id
ast_dt_register_local_member(struct ast_dt_context *ctx,
		struct atom *name, struct stg_location decl_loc,
		struct ast_node *type_expr)
{
	struct ast_dt_member new_mbr = {0};

	new_mbr.name = name;
	new_mbr.decl_loc = decl_loc;
	new_mbr.next = ctx->terminal_nodes;
	new_mbr.flags |= AST_DT_MEMBER_IS_LOCAL;
	new_mbr.first_child = -1;
	new_mbr.persistant_id = -1;
	new_mbr.type_node = type_expr;

	new_mbr.type_resolved = -1;
	new_mbr.const_resolved = -1;

	new_mbr.type_jobs.resolve_names = -1;
	new_mbr.type_jobs.resolve_types = -1;
	new_mbr.type_jobs.codegen = -1;

	ast_member_id mbr_id;
	mbr_id = dlist_append(
			ctx->members,
			ctx->num_members,
			&new_mbr);

	ctx->terminal_nodes = mbr_id;

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	if (type_expr) {
		mbr->type_jobs.resolve_names =
			ast_dt_job_type(ctx, mbr_id,
					AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES);

		mbr->type_jobs.resolve_types =
			ast_dt_job_type(ctx, mbr_id,
					AST_DT_JOB_MBR_TYPE_RESOLVE_TYPES);

		mbr->type_jobs.codegen =
			ast_dt_job_type(ctx, mbr_id,
					AST_DT_JOB_MBR_TYPE_EVAL);

		ast_dt_job_dependency(ctx,
				mbr->type_jobs.resolve_names,
				mbr->type_jobs.resolve_types);

		ast_dt_job_dependency(ctx,
				mbr->type_jobs.resolve_types,
				mbr->type_jobs.codegen);

		mbr->type_resolved = mbr->type_jobs.codegen;
	}

	ast_dt_job_dependency(ctx,
			mbr->type_jobs.resolve_types,
			mbr->type_jobs.codegen);

	mbr->const_resolved =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_MBR_CONST_EVAL);

	ast_dt_job_dependency(ctx,
			ctx->target_names_resolved,
			mbr->const_resolved);

	ast_dt_job_dependency(ctx,
			mbr->type_jobs.codegen,
			mbr->const_resolved);

	return mbr_id;
}

static ast_member_id
ast_dt_register_descendent_member(struct ast_dt_context *ctx,
		struct atom *name, struct stg_location decl_loc,
		type_id type, ast_member_id anscestor_id)
{
	struct ast_dt_member new_mbr = {0};

	new_mbr.name = name;
	new_mbr.decl_loc = decl_loc;
	new_mbr.type = type;
	new_mbr.next = ctx->terminal_nodes;

	// Not local
	new_mbr.flags &= ~AST_DT_MEMBER_IS_LOCAL;
	new_mbr.anscestor_local_member = anscestor_id;
	new_mbr.persistant_id = -1;

	new_mbr.type_jobs.resolve_names = -1;
	new_mbr.type_jobs.resolve_types = -1;
	new_mbr.type_jobs.codegen = -1;

	new_mbr.type_resolved = -1;

	ast_member_id mbr_id;
	mbr_id = dlist_append(
			ctx->members,
			ctx->num_members,
			&new_mbr);

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	mbr->const_resolved =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_MBR_CONST_EVAL);

	ast_dt_job_dependency(ctx,
			ctx->target_names_resolved,
			mbr->const_resolved);

	struct ast_dt_member *anscestor;
	anscestor = get_member(ctx, anscestor_id);

	assert((anscestor->flags & AST_DT_MEMBER_IS_LOCAL) != 0);
	if (anscestor->first_child < 0) {
		anscestor->first_child = mbr_id;
	}

	ctx->terminal_nodes = mbr_id;

	return mbr_id;
}

static ast_member_id
ast_dt_resolve_l_expr(struct ast_dt_context *ctx, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_LOOKUP:
			if (node->lookup.ref.kind != AST_NAME_REF_MEMBER) {
				// TODO: We can delay this message until after typecheck to
				// have the error indicate if the member was a closure or
				// not found at all.
				stg_error(ctx->ast_ctx->err, node->loc,
						"This member does not exist.");
				return -1;
			}
			return node->lookup.ref.member;

		case AST_NODE_ACCESS:
			{
				ast_member_id target;
				target = ast_dt_resolve_l_expr(ctx, node->access.target);
				if (target < 0) {
					return -1;
				}

				struct ast_dt_member *mbr;
				mbr = get_member(ctx, target);
				assert(mbr->type != TYPE_UNSET);

				struct type *mbr_type;
				mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

				if (!mbr_type->obj_def) {
					stg_error(ctx->ast_ctx->err, node->access.target->loc,
							"This object does not have any members.");
					return -1;
				}

				bool found = false;
				size_t param_id = 0;
				for (size_t i = 0; i < mbr_type->obj_def->num_params; i++) {
					if (mbr_type->obj_def->params[i].name == node->access.name) {
						found = true;
						break;
					}

					param_id += 1;
					struct type *param_type;
					assert(TYPE_VALID(mbr_type->obj_def->params[i].type));
					param_type = vm_get_type(ctx->ast_ctx->vm,
							mbr_type->obj_def->params[i].type);
					if (param_type->obj_def) {
						param_id += ast_object_def_num_descendant_members(
								ctx->ast_ctx, ctx->ast_mod, param_type->obj_def);
					}
				}

				if (!found) {
					stg_error(ctx->ast_ctx->err, node->access.target->loc,
							"This object does not have a member '%.*s'.",
							ALIT(node->access.name));
					return -1;
				}

				ast_member_id first_child;
				if ((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
					first_child = mbr->first_child;
				} else {
					first_child = target + 1;
				}

				return first_child + param_id;
			}

		default:
			stg_error(ctx->ast_ctx->err, node->loc,
					"Bind targets may only contain names and accesses.");
			return -1;
	}
}

int
ast_dt_l_expr_members(struct ast_dt_context *ctx, struct ast_node *node,
		ast_member_id **out_members, size_t *out_num_members)
{
	switch (node->kind) {
		case AST_NODE_LOOKUP:
			if (node->lookup.ref.kind != AST_NAME_REF_MEMBER) {
				// TODO: We can delay this message until after typecheck to
				// have the error indicate if the member was a closure or
				// not found at all.
				stg_error(ctx->ast_ctx->err, node->loc,
						"This member does not exist.");
				return -1;
			}
			dlist_append(
					*out_members,
					*out_num_members,
					&node->lookup.ref.member);
			return 0;

		case AST_NODE_ACCESS:
			return ast_dt_l_expr_members(
					ctx, node->access.target,
					out_members, out_num_members);

		default:
			stg_error(ctx->ast_ctx->err, node->loc,
					"Bind targets may only contain names and accesses.");
			return -1;
	}
}

static void
ast_dt_composite_populate(struct ast_dt_context *ctx, struct ast_node *node)
{
	assert(node->kind == AST_NODE_COMPOSITE);

	ast_member_id *members;
	members = calloc(node->composite.num_members, sizeof(ast_member_id));
	ctx->local_member_ids = members;

	ast_member_id type_giving_for[node->composite.num_binds];
	for (size_t i = 0; i < node->composite.num_binds; i++) {
		type_giving_for[i] = -1;
	}

	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_datatype_member *mbr;
		mbr = &node->composite.members[i];

		// TODO: Better location.
		members[i] = ast_dt_register_local_member(
				ctx, mbr->name, mbr->loc, mbr->type);

		if (mbr->type_giving_bind >= 0) {
			type_giving_for[mbr->type_giving_bind] = members[i];
		}
	}

	for (size_t i = 0; i < node->composite.num_binds; i++) {
		if (type_giving_for[i] >= 0) {
			ast_dt_register_typegiving_bind(ctx,
					node->composite.binds[i].loc,
					node->composite.binds[i].target,
					type_giving_for[i],
					node->composite.binds[i].value,
					node->composite.binds[i].overridable);
		} else {
			ast_dt_register_bind(ctx,
					node->composite.binds[i].loc,
					node->composite.binds[i].target,
					node->composite.binds[i].value,
					node->composite.binds[i].overridable);
		}
	}

	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_dt_member *mbr;
		mbr = get_member(ctx, members[i]);

		assert(mbr->type_resolved >= 0);
	}
}

static void
ast_dt_populate_descendants(struct ast_dt_context *ctx, ast_member_id local_anscestor, ast_member_id parent_id)
{
	struct ast_dt_member *parent;
	parent = get_member(ctx, parent_id);

	assert(parent->type != TYPE_UNSET);

	struct ast_dt_member *anscestor;
	anscestor = get_member(ctx, local_anscestor);

	struct type *parent_type;
	parent_type = vm_get_type(ctx->ast_ctx->vm, parent->type);

	if (parent_type->obj_def) {
		struct ast_object_def *def;
		def = parent_type->obj_def;

		for (size_t i = 0; i < def->num_params; i++) {
			// TODO: Better location.
			ast_slot_id param_mbr_id;

			assert(TYPE_VALID(def->params[i].type));
			param_mbr_id = ast_dt_register_descendent_member(
					ctx, def->params[i].name, STG_NO_LOC,
					def->params[i].type, local_anscestor);

			ast_dt_populate_descendants(ctx, local_anscestor, param_mbr_id);
		}
	}
}

static void
ast_dt_populate_descendant_binds(struct ast_dt_context *ctx, ast_member_id parent_id)
{
	struct ast_dt_member *parent;
	parent = get_member(ctx, parent_id);

	assert(parent->type != TYPE_UNSET);
	assert((parent->flags & AST_DT_MEMBER_IS_LOCAL) != 0);

	struct type *parent_type;
	parent_type = vm_get_type(ctx->ast_ctx->vm, parent->type);

	if (parent_type->obj_def) {
		struct ast_object_def *def;
		def = parent_type->obj_def;

		for (size_t i = 0; i < def->num_binds; i++) {
			ast_member_id *deps;
			deps = calloc(def->binds[i].num_value_params, sizeof(ast_member_id));
			for (size_t j = 0; j < def->binds[i].num_value_params; j++) {
				deps[j] = parent->first_child + def->binds[i].value_params[j];
			}

			ast_member_id mbr_id;
			mbr_id = parent->first_child + def->binds[i].target;

			switch (def->binds[i].kind) {
				case AST_OBJECT_DEF_BIND_VALUE:
					ast_dt_register_bind_func(ctx,
							mbr_id, def->binds[i].unpack_func,
							def->binds[i].value.func,
							deps, def->binds[i].num_value_params,
							def->binds[i].value.overridable);
					break;

				case AST_OBJECT_DEF_BIND_CONST:
					ast_dt_register_bind_const(ctx,
							mbr_id, def->binds[i].unpack_func,
							def->binds[i].const_value, false);
					break;

				case AST_OBJECT_DEF_BIND_PACK:
					ast_dt_register_bind_pack(ctx,
							mbr_id, def->binds[i].unpack_func,
							def->binds[i].pack,
							deps, def->binds[i].num_value_params);
					break;
			}
		}
	}
}

static void
ast_dt_add_dependency_on_member(struct ast_dt_context *ctx,
		ast_dt_job_id target_job, enum ast_name_dep_requirement req,
		ast_member_id mbr_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	switch (req) {
		case AST_NAME_DEP_REQUIRE_TYPE:
			ast_dt_job_dependency(ctx,
					mbr->const_resolved,
					target_job);
			break;

		case AST_NAME_DEP_REQUIRE_VALUE:
			ast_dt_job_dependency(ctx,
					mbr->const_resolved,
					target_job);
			break;
	}
}

static void
ast_dt_find_named_dependencies(struct ast_dt_context *ctx,
		ast_dt_job_id target_job, enum ast_name_dep_requirement req,
		struct ast_node *node)
{
	struct ast_name_dep *deps = NULL;
	size_t num_deps = 0;

	ast_node_find_named_dependencies(
			node, req, &deps, &num_deps);

	for (size_t i = 0; i < num_deps; i++) {
		if (deps[i].ref.kind == AST_NAME_REF_MEMBER) {
			ast_dt_add_dependency_on_member(
					ctx, target_job, deps[i].req, deps[i].ref.member);
		}
	}

	free(deps);
}

static inline size_t
ast_dt_member_num_descendents(
		struct ast_dt_context *ctx, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	assert(mbr->type != TYPE_UNSET);

	struct type *type;
	type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

	if (type->obj_def) {
		return ast_object_def_num_descendant_members(
				ctx->ast_ctx, ctx->ast_mod, type->obj_def) + 1;
	} else {
		return 1;
	}
}

// Returns the number of descendent members this memeber has (including
// itself).
int
ast_dt_find_terminal_members(
		struct ast_dt_context *ctx, ast_member_id mbr_id,
		ast_member_id **out_members, int **out_descendent_members, size_t *out_num_members,
		size_t descendent_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);
	assert(mbr->type != TYPE_UNSET);

	struct type *mbr_type;
	mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

	if (mbr_type->obj_def) {
		ast_member_id first_child;

		if ((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
			first_child = mbr->first_child;
		} else {
			first_child = mbr_id + 1;
		}

		int num_descendents = 0;
		for (size_t i = 0; i < mbr_type->obj_def->num_params; i++) {
			num_descendents += ast_dt_find_terminal_members(
					ctx, first_child + num_descendents,
					out_members, out_descendent_members, out_num_members,
					1 + descendent_id + num_descendents);
		}

		return num_descendents + 1;
	} else {
		dlist_append(
				*out_members,
				*out_num_members,
				&mbr_id);
		if (out_descendent_members) {
			*out_descendent_members = realloc(
					*out_descendent_members,
					sizeof(int) * (*out_num_members));
			(*out_descendent_members)[*out_num_members-1] = descendent_id;
		}
		return 1;
	}
}

static void
ast_dt_bind_typecheck_dep(struct ast_dt_context *ctx,
		struct ast_typecheck_dep *dep)
{
	if (!dep->determined) {
		return;
	}

	if (dep->lookup_failed) {
		dep->value = ast_bind_slot_wildcard(
				ctx->ast_ctx, ctx->ast_env, AST_BIND_NEW,
				NULL, ast_bind_slot_wildcard(
					ctx->ast_ctx, ctx->ast_env, AST_BIND_NEW,
					NULL, AST_SLOT_TYPE));
		return;
	}

	dep->value = AST_BIND_FAILED;

	switch (dep->req) {
		case AST_NAME_DEP_REQUIRE_VALUE:
			dep->value =
				ast_bind_slot_const(
						ctx->ast_ctx, ctx->ast_env, AST_BIND_NEW,
						NULL, dep->val);
			break;

		case AST_NAME_DEP_REQUIRE_TYPE:
			dep->value =
				ast_bind_slot_closure(
						ctx->ast_ctx, ctx->ast_env,
						AST_BIND_NEW, NULL,
						ast_bind_slot_const_type(
							ctx->ast_ctx, ctx->ast_env,
							AST_BIND_NEW, NULL,
							dep->type));
			break;
	}
}

static int
ast_try_set_local_member_type(struct ast_dt_context *ctx,
		ast_member_id mbr_id, type_id type)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	if (mbr->type != TYPE_UNSET) {
		assert_type_equals(ctx->ast_ctx->vm, type, mbr->type);
		return 0;
	}

	mbr->type = type;
	assert(mbr->type != TYPE_UNSET);

	ast_dt_populate_descendants(ctx, mbr_id, mbr_id);
	ast_dt_populate_descendant_binds(ctx, mbr_id);

	// We re-fetch mbr as it might have been
	// invalidated by ast_dt_populate_descendant_binds.
	mbr = get_member(ctx, mbr_id);

	struct type *mbr_type;
	mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

	if (mbr_type->obj_def) {
		size_t num_children = 0;
		ast_member_id *children = NULL;

		num_children = mbr_type->obj_def->num_params;
		if (num_children > 0) {
			children = calloc(num_children, sizeof(ast_member_id));

			assert((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0 &&
					mbr->first_child >= 0);

			size_t offset = 0;
			for (size_t i = 0; i < num_children; i++) {
				children[i] = mbr->first_child + offset;
				offset += ast_dt_member_num_descendents(ctx, children[i]);
			}
		}

		struct ast_dt_bind *bind;
		bind = ast_dt_register_bind_pack(ctx,
				mbr_id, FUNC_UNSET, mbr_type->obj_def,
				children, num_children);
	}

	return 0;
}

static func_id
ast_dt_expr_codegen(struct ast_dt_context *ctx, struct ast_node *node,
		enum ast_name_dep_requirement dep_req, ast_member_id **out_dep_members,
		size_t *out_num_dep_members)
{
	struct ast_name_dep *names = NULL;
	size_t num_names = 0;

	ast_node_find_named_dependencies(
			node, dep_req, &names, &num_names);

	size_t num_dep_members = 0;
	for (size_t i = 0; i < num_names; i++) {
		if (names[i].ref.kind == AST_NAME_REF_MEMBER) {
			num_dep_members += 1;
		}
	}

	ast_member_id *dep_members = calloc(num_dep_members, sizeof(ast_member_id));
	type_id dep_member_types[num_dep_members];
	struct object dep_member_const[num_dep_members];

	size_t dep_mbr_i = 0;
	for (size_t name_i = 0; name_i < num_names; name_i++) {
		if (names[name_i].ref.kind == AST_NAME_REF_MEMBER) {
			dep_members[dep_mbr_i] = names[name_i].ref.member;
			struct ast_dt_member *mbr = get_member(ctx, dep_members[dep_mbr_i]);
			assert(mbr->type != TYPE_UNSET);
			dep_member_types[dep_mbr_i] = mbr->type;

			if ((mbr->flags & AST_DT_MEMBER_IS_CONST) != 0) {
				dep_member_const[dep_mbr_i] = mbr->const_value;
			} else {
				dep_member_const[dep_mbr_i].type = TYPE_UNSET;
				dep_member_const[dep_mbr_i].data = NULL;
			}

			dep_mbr_i += 1;
		}
	}

	num_names = 0;
	free(names);
	names = NULL;

	struct bc_env *bc_env;
	bc_env = ast_composite_bind_gen_bytecode(
				ctx->ast_ctx, ctx->ast_mod, ctx->ast_env,
				dep_members, dep_member_types,
				dep_member_const, num_dep_members,
				ctx->closures, ctx->num_closures, node);

	struct func func = {0};
	func.type = stg_register_func_type(ctx->ast_mod->stg_mod,
			node->type, dep_member_types, num_dep_members);

	func.kind = FUNC_BYTECODE;
	func.bytecode = bc_env;

	func_id fid;
	fid = stg_register_func(ctx->ast_mod->stg_mod, func);

	*out_num_dep_members = num_dep_members;
	*out_dep_members = dep_members;

	return fid;
}

static int
ast_dt_expr_typecheck(struct ast_dt_context *ctx, struct ast_node *node,
		enum ast_name_dep_requirement dep_req)
{
	struct ast_name_dep *deps = NULL;
	size_t num_deps = 0;

	ast_node_find_named_dependencies(
			node, dep_req, &deps, &num_deps);

	struct ast_typecheck_dep body_deps[num_deps];

	for (size_t i = 0; i < num_deps; i++) {
		switch (deps[i].ref.kind) {

			case AST_NAME_REF_MEMBER:
				{
					struct ast_dt_member *dep_mbr;
					dep_mbr = get_member(ctx, deps[i].ref.member);

					body_deps[i].ref = deps[i].ref;
					body_deps[i].req = deps[i].req;
					body_deps[i].lookup_failed = false;

					if (deps[i].req == AST_NAME_DEP_REQUIRE_VALUE ||
							(dep_mbr->flags & AST_DT_MEMBER_IS_CONST) != 0) {
						assert((dep_mbr->flags & AST_DT_MEMBER_IS_CONST) != 0);

						body_deps[i].determined = true;
						body_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;
						body_deps[i].val = dep_mbr->const_value;
					} else {
						assert(dep_mbr->type != TYPE_UNSET);

						body_deps[i].determined = true;
						body_deps[i].type = dep_mbr->type;
					}

					ast_dt_bind_typecheck_dep(ctx, &body_deps[i]);
				}
				break;

			case AST_NAME_REF_CLOSURE:
				if (ctx->closures) {
					assert(deps[i].ref.closure >= 0 &&
							deps[i].ref.closure < ctx->num_closures);
					struct ast_typecheck_closure *cls;
					cls = &ctx->closures[deps[i].ref.closure];
					body_deps[i].ref = deps[i].ref;
					body_deps[i].req = cls->req;

					body_deps[i].determined = true;
					body_deps[i].lookup_failed = cls->lookup_failed;

					switch (cls->req) {
						case AST_NAME_DEP_REQUIRE_VALUE:
							// The closure provided a value. It
							// does not matter if we asked for a
							// type instead.
							body_deps[i].val = cls->value;
							break;

						case AST_NAME_DEP_REQUIRE_TYPE:
							// The closure provided only a type.
							// Make sure that is all we asked for.
							assert(deps[i].req == AST_NAME_DEP_REQUIRE_TYPE);
							body_deps[i].type = cls->type;
							break;
					}

					ast_dt_bind_typecheck_dep(ctx, &body_deps[i]);
				} else {
					body_deps[i].ref = deps[i].ref;
					body_deps[i].req = deps[i].req;
					body_deps[i].value = AST_BIND_FAILED;
					body_deps[i].lookup_failed = true;
					body_deps[i].determined = false;
				}
				break;

			default:
			case AST_NAME_REF_NOT_FOUND:
				panic("Invalid name reference.");
				break;

			case AST_NAME_REF_PARAM:
				panic("Got unexpected reference to a param in composite.");
				break;
		}
	}

	int err;
	err = ast_node_typecheck(
			ctx->ast_ctx, ctx->ast_mod, ctx->ast_env, node,
			body_deps, num_deps);
	if (err) {
		return -1;
	}

	return 0;
}

static int
ast_dt_unpack_descendent(struct ast_dt_context *ctx,
		struct object obj, size_t descendent,
		struct object *out)
{
	if (descendent == 0) {
		*out = register_object(
				ctx->ast_ctx->vm,
				ctx->ast_env->store, obj);
		return 0;
	}

	struct type *type;
	type = vm_get_type(ctx->ast_ctx->vm, obj.type);

	struct ast_object_def *def;
	def = type->obj_def;
	// If descendent is not 0 it is implied that it must be a child of this
	// member. If this member does not have a obj_def it can not have children.
	assert(def);

	size_t offset = 1;
	for (size_t i = 0; i < def->num_params; i++) {
		struct type *mbr_type;
		mbr_type = vm_get_type(ctx->ast_ctx->vm,
				def->params[i].type);

		size_t num_desc;
		if (mbr_type->obj_def) {
			num_desc = ast_object_def_num_descendant_members(
					ctx->ast_ctx, ctx->ast_mod, mbr_type->obj_def);
		} else {
			num_desc = 1;
		}

		assert(descendent >= offset);
		if (descendent >= offset + num_desc) {
			offset += num_desc;
			continue;
		}

		uint8_t buffer[mbr_type->size];
		assert(def->unpack_func);
		def->unpack_func(ctx->ast_ctx->vm,
				def->data, buffer, obj.data,
				def->params[i].param_id);

		struct object mbr = {0};
		mbr.data = buffer;
		mbr.type = def->params[i].type;

		return ast_dt_unpack_descendent(ctx,
				mbr, descendent - offset, out);
	}

	return -1;
}

static inline int
ast_dt_dispatch_job(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);

#if AST_DT_DEBUG_JOBS
	printf("%03zx    ===> ", ctx->run_i);
	ast_dt_print_job_desc(ctx, job_id);
	printf("\n");
#endif

	switch (job->kind) {
		case AST_DT_JOB_NOP:
			return 0;

		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->ast_env,
						ctx->ast_mod->stg_mod->native_mod,
						NULL, true, ctx->root_node, mbr->type_node,
						ctx->local_member_ids);
				if (err) {
					printf("Failed to resolve names.\n");
					break;
				}

				ast_dt_find_named_dependencies(
						ctx, mbr->type_jobs.resolve_types,
						AST_NAME_DEP_REQUIRE_VALUE, mbr->type_node);
			}
			return 0;

		case AST_DT_JOB_BIND_RESOLVE_NAMES:
			{
				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->ast_env,
						ctx->ast_mod->stg_mod->native_mod,
						NULL, false, ctx->root_node,
						job->bind->value.node,
						ctx->local_member_ids);
				if (err) {
					printf("Failed to resolve names.\n");
					break;
				}

				ast_dt_find_named_dependencies(
						ctx, job->bind->value_jobs.resolve_types,
						AST_NAME_DEP_REQUIRE_TYPE, job->bind->value.node);
			}
			return 0;

		case AST_DT_JOB_MBR_TYPE_RESOLVE_TYPES:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);

				int err;
				err = ast_dt_expr_typecheck(
						ctx, mbr->type_node,
						AST_NAME_DEP_REQUIRE_VALUE);
				if (err) {
					return -1;
				}
			}
			return 0;

		case AST_DT_JOB_BIND_RESOLVE_TYPES:
			{
				int err;
				err = ast_dt_expr_typecheck(
						ctx, job->bind->value.node,
						AST_NAME_DEP_REQUIRE_TYPE);
				if (err) {
					return -1;
				}
			}
			return 0;

		case AST_DT_JOB_MBR_TYPE_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);

				ast_member_id *dep_members = NULL;
				size_t num_dep_members = 0;

				assert(mbr->type_node);

				assert_type_equals(ctx->ast_ctx->vm,
						ctx->ast_ctx->types.type, mbr->type_node->type);

				func_id fid;
				fid = ast_dt_expr_codegen(
						ctx, mbr->type_node,
						AST_NAME_DEP_REQUIRE_VALUE,
						&dep_members, &num_dep_members);

				struct object const_member_values[num_dep_members];
				for (size_t i = 0; i < num_dep_members; i++) {
					struct ast_dt_member *dep_mbr;
					dep_mbr = get_member(ctx, dep_members[i]);

					assert((dep_mbr->flags & AST_DT_MEMBER_IS_CONST) != 0);

					const_member_values[i] = dep_mbr->const_value;
				}

				type_id out_type = TYPE_UNSET;
				struct object out = {0};
				out.type = ctx->ast_ctx->types.type;
				out.data = &out_type;

				int err;
				err = vm_call_func(ctx->ast_ctx->vm, fid,
						const_member_values, num_dep_members,
						&out);
				if (err) {
					printf("Failed to evaluate member type.\n");
					return -1;
				}

				ast_try_set_local_member_type(
						ctx, job->member, out_type);
			}
			return 0;

		case AST_DT_JOB_MBR_CONST_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				assert(mbr->type != TYPE_UNSET);

				bool is_const = true;

				struct ast_dt_bind *bind;
				bind = mbr->bound;

				if (!bind) {
					return 0;
				}

				struct object dep_member_obj[bind->num_member_deps];

				for (size_t i = 0; i < bind->num_member_deps; i++) {
					assert(bind->member_deps[i] >= 0);

					struct ast_dt_member *dep_mbr;
					dep_mbr = get_member(ctx, bind->member_deps[i]);

					if ((dep_mbr->flags & AST_DT_MEMBER_IS_CONST) == 0) {
						is_const = false;
					}

					dep_member_obj[i] = dep_mbr->const_value;
				}

				if (is_const && !bind->overridable) {
					struct object const_value;
					switch (mbr->bound->kind) {
						case AST_OBJECT_DEF_BIND_VALUE:
							{
								struct type *ret_type;
								ret_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

								func_id fid;
								fid = mbr->bound->value.func;
								assert(fid != FUNC_UNSET);

								uint8_t buffer[ret_type->size];
								struct object obj = {0};
								obj.type = bind->value.node->type;
								obj.data = buffer;

								int err;
								err = vm_call_func(ctx->ast_ctx->vm, fid, dep_member_obj,
										bind->num_member_deps, &obj);
								if (err) {
									printf("Failed to evaluate constant member.\n");
									return -1;
								}
								const_value =
									register_object(ctx->ast_ctx->vm, ctx->ast_env->store, obj);
							}
							break;

						case AST_OBJECT_DEF_BIND_PACK:
							{
								struct ast_object_def *def;
								def = bind->pack;
								assert(def);

								struct type *ret_type;
								ret_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

								assert(bind->num_member_deps == def->num_params);

								void *dep_member_val[bind->num_member_deps];
								for (size_t i = 0; i < bind->num_member_deps; i++) {
									assert_type_equals(ctx->ast_ctx->vm,
											dep_member_obj[i].type, def->params[i].type);
									dep_member_val[i] = dep_member_obj[i].data;
								}

								uint8_t value_buffer[ret_type->size];

								def->pack_func(
										ctx->ast_ctx->vm,
										def->data, value_buffer,
										dep_member_val, def->num_params);

								struct object val = {0};
								val.type = mbr->type;
								val.data = value_buffer;

								const_value = register_object(
										ctx->ast_ctx->vm, ctx->ast_env->store, val);

								// ast_dt_eval_pack_bind_value(
								// 		ctx, mbr->bound);
							}
							break;

						case AST_OBJECT_DEF_BIND_CONST:
							const_value = mbr->bound->const_value;
							break;
					}

					int descendent_id = -1;

					if (mbr->bound->target_descendent_id) {
						ast_member_id *targets;
						targets = ast_dt_get_bind_targets(mbr->bound);
						for (size_t i = 0; i < mbr->bound->num_targets; i++) {
							if (targets[i] == job->member) {
								descendent_id = mbr->bound->target_descendent_id[i];
							}
						}

						assert(descendent_id >= 0);
					} else {
						assert(mbr->bound->num_targets == 1);
						descendent_id = 0;
					}

					int err;
					err = ast_dt_unpack_descendent(
							ctx, const_value, descendent_id,
							&mbr->const_value);

					assert_type_equals(ctx->ast_ctx->vm,
							mbr->type, mbr->const_value.type);

					mbr->flags |= AST_DT_MEMBER_IS_CONST;
				}
			}
			return 0;

		case AST_DT_JOB_BIND_CODEGEN:
			{
				ast_member_id *dep_members = NULL;
				size_t num_dep_members = 0;

				func_id fid;
				fid = ast_dt_expr_codegen(
						ctx, job->bind->value.node,
						AST_NAME_DEP_REQUIRE_TYPE,
						&dep_members, &num_dep_members);

				job->bind->num_member_deps = num_dep_members;
				job->bind->member_deps = dep_members;

				job->bind->value.func = fid;
			}
			return 0;

		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			{
				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->ast_env, NULL, NULL,
						false, ctx->root_node, job->bind->target_node,
						ctx->local_member_ids);
				if (err) {
					return -1;
				}


				job->bind->explicit_targets = NULL;
				job->bind->num_explicit_targets = 0;

				err = ast_dt_l_expr_members(
						ctx, job->bind->target_node,
						&job->bind->explicit_targets,
						&job->bind->num_explicit_targets);
				if (err) {
					printf("Failed to find l-expr members\n");
				}

				if (!job->bind->is_type_giving) {
					for (size_t i = 0; i < job->bind->num_explicit_targets; i++) {
						ast_member_id mbr_id;
						mbr_id = job->bind->explicit_targets[i];

						struct ast_dt_member *mbr;
						mbr = get_member(ctx, mbr_id);

						ast_dt_job_dependency(ctx,
								mbr->type_resolved,
								job->bind->target_jobs.resolve);
					}
				}

				for (size_t i = 0; i < job->bind->num_explicit_targets; i++) {
					ast_member_id mbr_id;
					mbr_id = job->bind->explicit_targets[i];

					struct ast_dt_member *mbr;
					mbr = get_member(ctx, mbr_id);

					// We place this dependency to ensure the member can get
					// all its dependencies from bind target resolve before its
					// const resolved is dispatched.
					ast_dt_job_dependency(ctx,
							job->bind->target_jobs.resolve,
							mbr->const_resolved);
				}
			}
			return 0;

		case AST_DT_JOB_BIND_TARGET_RESOLVE:
			{
				// TODO: Multiple targets
				assert(job->bind->num_explicit_targets == 1);
				assert(job->bind->kind == AST_OBJECT_DEF_BIND_VALUE);

				if (job->bind->is_type_giving) {
					assert(job->bind->num_explicit_targets == 1);
					assert(job->bind->value.node->type != TYPE_UNSET);

					ast_try_set_local_member_type(ctx,
							job->bind->explicit_targets[0],
							job->bind->value.node->type);
				}

				// We run resolve_l_expr here because at this point all types
				// of the targets should have been resolved.
				ast_member_id target;
				target = ast_dt_resolve_l_expr(
						ctx, job->bind->target_node);

				if (target < 0) {
					return -1;
				}

				ast_member_id *targets = NULL;
				int *descendent_ids = NULL;
				size_t num_targets = 0;

				ast_dt_find_terminal_members(
						ctx, target, &targets,
						&descendent_ids, &num_targets, 0);

				assert(num_targets > 0);

				if (num_targets == 1) {
					job->bind->single_target = targets[0];
					free(targets);
				} else {
					job->bind->multiple_targets = targets;
				}
				job->bind->num_targets = num_targets;
				job->bind->target_descendent_id = descendent_ids;

				job->bind->target_unpack_func =
					calloc(job->bind->num_targets, sizeof(func_id));

				targets = ast_dt_get_bind_targets(job->bind);

				type_id bind_value_type = TYPE_UNSET;
				switch (job->bind->kind) {
					case AST_OBJECT_DEF_BIND_VALUE:
						bind_value_type = job->bind->value.node->type;
						break;

					case AST_OBJECT_DEF_BIND_PACK:
						// TODO: Resolve the type from pack.
						assert(job->bind->num_targets == 1);
						bind_value_type = get_member(ctx, job->bind->single_target)->type;
						break;

					case AST_OBJECT_DEF_BIND_CONST:
						bind_value_type = job->bind->const_value.type;
						break;
				}
				assert(bind_value_type != TYPE_UNSET);

				for (size_t i = 0; i < job->bind->num_targets; i++) {
					struct bc_env *bc_env;
					bc_env = ast_gen_value_unpack_func(
							ctx->ast_ctx, ctx->ast_mod, ctx->ast_env,
							bind_value_type, descendent_ids[i]);

					struct func func = {0};
					func.type = stg_register_func_type(ctx->ast_mod->stg_mod,
							get_member(ctx, targets[i])->type,
							&bind_value_type, 1);

					func.kind = FUNC_BYTECODE;
					func.bytecode = bc_env;

					job->bind->target_unpack_func[i] =
						stg_register_func(ctx->ast_mod->stg_mod, func);
				}

				for (size_t i = 0; i < job->bind->num_targets; i++) {
					struct ast_dt_member *mbr;
					mbr = get_member(ctx, targets[i]);
					ast_dt_bind_to_member(
							ctx, job->bind, targets[i]);

					ast_dt_job_dependency(ctx,
							job->bind->value_jobs.codegen,
							mbr->const_resolved);
				}
			}
			return 0;

		case AST_DT_JOB_FREE:
			panic("Attempted to dispatch job that has been freed.");
			break;
	}

	return -1;
}

static void
ast_dt_remove_job_from_target(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);

	switch (job->kind) {
		case AST_DT_JOB_FREE:
			panic("Tried to remove freed job.");
			break;

		case AST_DT_JOB_NOP:
			if (job->id_loc) {
				assert(*job->id_loc == job_id);
				*job->id_loc = -1;
			}
			break;

		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				assert(mbr->type_jobs.resolve_names == job_id);
				mbr->type_jobs.resolve_names = -1;
			}
			break;
		case AST_DT_JOB_MBR_TYPE_RESOLVE_TYPES:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				assert(mbr->type_jobs.resolve_types == job_id);
				mbr->type_jobs.resolve_types = -1;
			}
			break;
		case AST_DT_JOB_MBR_TYPE_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				assert(mbr->type_jobs.codegen == job_id);
				mbr->type_jobs.codegen = -1;
				assert(mbr->type_resolved == job_id);
				mbr->type_resolved = -1;
			}
			break;

		case AST_DT_JOB_MBR_CONST_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				assert(mbr->const_resolved == job_id);
				mbr->const_resolved = -1;
			}
			break;

		case AST_DT_JOB_BIND_RESOLVE_NAMES:
			assert(job->bind->value_jobs.resolve_names == job_id);
			job->bind->value_jobs.resolve_names = -1;
			break;
		case AST_DT_JOB_BIND_RESOLVE_TYPES:
			assert(job->bind->value_jobs.resolve_types == job_id);
			job->bind->value_jobs.resolve_types = -1;
			break;
		case AST_DT_JOB_BIND_CODEGEN:
			assert(job->bind->value_jobs.codegen == job_id);
			job->bind->value_jobs.codegen = -1;
			break;

		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			assert(job->bind->target_jobs.resolve_names == job_id);
			job->bind->target_jobs.resolve_names = -1;
			break;
		case AST_DT_JOB_BIND_TARGET_RESOLVE:
			assert(job->bind->target_jobs.resolve == job_id);
			job->bind->target_jobs.resolve = -1;
			if (job->bind->is_type_giving) {
				struct ast_dt_member *mbr;
				assert(job->bind->num_explicit_targets == 1);
				mbr = get_member(ctx, job->bind->explicit_targets[0]);

				assert(mbr->type_resolved == job_id);
				mbr->type_resolved = -1;
			}
			break;

	}
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

		int err;
		err = ast_dt_dispatch_job(ctx, job_id);
		if (err) {
#if AST_DT_DEBUG_JOBS
			printf("job failed!\n");
#endif
			failed_jobs += 1;

			ast_dt_remove_job_from_target(ctx, job_id);
			ast_dt_free_job(ctx, job_id);
			continue;
		}

		if (job->num_incoming_deps > 0) {
			// If the node gave itself new dependencies we don't mark it as
			// visited to allow it to pass through again.
			continue;
		}

		for (size_t i = 0; i < job->num_outgoing_deps; i++) {
			struct ast_dt_job_dep *dep;
			dep = &job->outgoing_deps[i];

			if (!dep->visited) {
				dep->visited = true;

				struct ast_dt_job *to;
				to = get_job(ctx, dep->to);

				assert(to->num_incoming_deps > 0);
				to->num_incoming_deps -= 1;

				assert(ctx->unvisited_job_deps > 0);
				ctx->unvisited_job_deps -= 1;

				if (to->num_incoming_deps == 0) {
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
		printf("Failed to evalutate datatype because we found one or more cycles.\n");
#if AST_DT_DEBUG_JOBS
		printf("Problematic jobs: \n");
		size_t jobs_per_page = ctx->page_size / sizeof(struct ast_dt_job);
		size_t cap_jobs = ctx->num_job_pages * jobs_per_page;
		for (ast_dt_job_id job_i = 0; job_i < cap_jobs; job_i++) {
			struct ast_dt_job *job;
			job = get_job(ctx, job_i);
			if (job->kind != AST_DT_JOB_FREE) {
				printf(" - 0x%03x (%zu):", job_i, job->num_incoming_deps);

				// Find all refs to this job.
				for (ast_dt_job_id job_j = 0; job_j < cap_jobs; job_j++) {
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
#endif
		return -1;
	}

	return 0;
}

static ast_member_id
ast_dt_num_descendant_members(struct ast_dt_context *ctx,
		struct ast_module *mod, type_id type)
{
	struct type *member_type;
	member_type = vm_get_type(ctx->ast_ctx->vm, type);

	if (member_type->obj_def) {
		return ast_object_def_num_descendant_members(
				ctx->ast_ctx, mod, member_type->obj_def);
	} else {
		return 0;
	}
}

static ast_member_id
ast_dt_calculate_persistant_id(struct ast_dt_context *ctx, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr = get_member(ctx, mbr_id);

	if ((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
		assert(mbr->persistant_id >= 0 && mbr->persistant_id < ctx->num_members);
		return mbr->persistant_id;
	} else {
		struct ast_dt_member *local_anscestor;
		local_anscestor = get_member(ctx, mbr->anscestor_local_member);
		assert((local_anscestor->flags & AST_DT_MEMBER_IS_LOCAL) != 0 &&
				local_anscestor->first_child >= 0 &&
				local_anscestor->first_child <= mbr_id);

		ast_member_id result;

		result = 1 + local_anscestor->persistant_id +
			(mbr_id - local_anscestor->first_child);

		assert(result >= 0 && result < ctx->num_members);

		return result;
	}
}

struct ast_dt_local_member {
	struct atom *name;
	type_id type;
	size_t location;
	size_t size;
};

struct ast_dt_composite_info {
	struct ast_dt_local_member *members;
	size_t num_members;
};

static struct string
ast_dt_composite_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct ast_dt_composite_info *info = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("Struct { "));

	for (size_t i = 0; i < info->num_members; i++) {
		struct type *member_type;
		member_type = vm_get_type(vm, info->members[i].type);
		arena_string_append_sprintf(mem, &res, "%.*s: ", ALIT(info->members[i].name));
		arena_string_append_type_repr(&res, vm, mem, member_type);
		arena_string_append(mem, &res, STR("; "));
	}

	arena_string_append(mem, &res, STR("}"));

	return res;
}

static struct string
ast_dt_composite_obj_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	struct type *type = vm_get_type(vm, obj->type);
	struct ast_dt_composite_info *info = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("{ "));

	for (size_t i = 0; i < info->num_members; i++) {
		struct object mbr = {0};
		mbr.type = info->members[i].type;
		mbr.data = (void *)((uint8_t *)obj->data + info->members[i].location);

		arena_string_append_sprintf(mem, &res, "%.*s=", ALIT(info->members[i].name));

		arena_string_append_obj_repr(&res, vm, mem, &mbr);
		arena_string_append(mem, &res, STR("; "));
	}

	arena_string_append(mem, &res, STR("}"));

	return res;
}

static void
ast_dt_pack_func(struct vm *vm, void *data, void *out,
		void **params, size_t num_params)
{
	struct ast_dt_composite_info *info = data;
	assert(info->num_members == num_params);

	uint8_t *out_ptr = out;

	for (size_t i = 0; i < num_params; i++) {
		memcpy(out_ptr+info->members[i].location,
				params[i], info->members[i].size);
	}
}

static void
ast_dt_unpack_func(
		struct vm *vm, void *data, void *out,
		void *obj, int param_id)
{
	struct ast_dt_composite_info *info = data;
	assert(param_id >= 0 && param_id < info->num_members);

	struct ast_dt_local_member *mbr = &info->members[param_id];
	memcpy(out, (uint8_t *)obj+mbr->location, mbr->size);
}

struct type_base ast_dt_composite_base = {
	.name     = STR("Struct"),
	.repr     = ast_dt_composite_repr,
	.obj_repr = ast_dt_composite_obj_repr,

	// TODO: Implement
	// .free = ...,
};

static type_id
ast_dt_composite_make_type(struct ast_dt_context *ctx, struct ast_module *mod)
{
	struct ast_node *comp = ctx->root_node;

	struct ast_object_def *def;
	def = ast_object_def_register(mod->env.store);
	def->pack_func   = ast_dt_pack_func;
	def->unpack_func = ast_dt_unpack_func;

	struct ast_dt_local_member *local_members;
	local_members = calloc(comp->composite.num_members,
			sizeof(struct ast_dt_local_member));

	struct ast_object_def_param *params;
	params = calloc(comp->composite.num_members,
			sizeof(struct ast_object_def_param));

	size_t num_bound_members = 0;
	for (ast_member_id mbr = 0; mbr < ctx->num_members; mbr++) {
		if (get_member(ctx, mbr)->bound) {
			num_bound_members += 1;
		}
	}
	struct ast_object_def_bind *binds;
	binds = calloc(num_bound_members,
			sizeof(struct ast_object_def_bind));

	size_t offset = 0;
	ast_member_id cumulative_persistant_id = 0;
	for (size_t i = 0; i < comp->composite.num_members; i++) {
		params[i].param_id = i;
		params[i].name = comp->composite.members[i].name;
		local_members[i].name = comp->composite.members[i].name;
		local_members[i].location = offset;

		ast_member_id mbr_id;
		mbr_id = ctx->local_member_ids[i];

		struct ast_dt_member *mbr;
		mbr = get_member(ctx, mbr_id);

		params[i].type = mbr->type;
		params[i].slot =
			ast_bind_slot_wildcard(ctx->ast_ctx, &def->env, AST_BIND_NEW, NULL,
					ast_bind_slot_const_type(ctx->ast_ctx, &def->env, AST_BIND_NEW,
						NULL, mbr->type));

		// NOTE: We keep the old member_id here so that we can replace all
		// slot member ids after the binds have been added.
		def->env.slots[params[i].slot].member_id = mbr_id;

		mbr->persistant_id = cumulative_persistant_id;
		cumulative_persistant_id += 1 +
			ast_dt_num_descendant_members(ctx, mod, mbr->type);

		struct type *member_type;
		member_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

		local_members[i].type = mbr->type;
		local_members[i].size = member_type->size;

		offset += member_type->size;
	}

	// NOTE: ast_dt_calculate_persistant_id can only be used after this
	// point because it is dependent on persistant_id being set for local
	// members.

	def->params = params;
	def->num_params = comp->composite.num_members;

	size_t bind_i = 0;
	for (size_t mbr_i = 0; mbr_i < ctx->num_members; mbr_i++) {
		struct ast_dt_member *mbr = get_member(ctx, mbr_i);

		if (!mbr->bound) {
			continue;
		}

		binds[bind_i].kind = mbr->bound->kind;
		switch (mbr->bound->kind) {
			case AST_OBJECT_DEF_BIND_VALUE:
				if ((mbr->flags & AST_DT_MEMBER_IS_CONST) == 0 &&
						!mbr->bound->overridable) {
					assert(mbr->bound->value.func  != FUNC_UNSET);
					binds[bind_i].value.func        = mbr->bound->value.func;
					binds[bind_i].value.overridable = mbr->bound->overridable;
				} else {
					binds[bind_i].kind = AST_OBJECT_DEF_BIND_CONST;
					binds[bind_i].const_value = mbr->const_value;
				}
				break;

			case AST_OBJECT_DEF_BIND_CONST:
				binds[bind_i].const_value = mbr->const_value;
				break;

			case AST_OBJECT_DEF_BIND_PACK:
				assert(mbr->bound->num_targets == 1);
				if ((mbr->flags & AST_DT_MEMBER_IS_CONST) == 0 &&
						!mbr->bound->overridable) {
					assert(mbr->bound->pack);
					binds[bind_i].pack = mbr->bound->pack;
				} else {
					binds[bind_i].kind = AST_OBJECT_DEF_BIND_CONST;
					binds[bind_i].const_value = mbr->const_value;
				}
				break;
		}

		for (size_t i = 0; i < mbr->bound->num_member_deps; i++) {
			mbr->bound->member_deps[i] =
				ast_dt_calculate_persistant_id(
						ctx, mbr->bound->member_deps[i]);
		}

		binds[bind_i].unpack_func = FUNC_UNSET;
		ast_member_id *targets;
		targets = ast_dt_get_bind_targets(mbr->bound);
		for (size_t i = 0; i < mbr->bound->num_targets; i++) {
			if (targets[i] == mbr_i) {
				binds[bind_i].unpack_func = mbr->bound->target_unpack_func[i];
				break;
			}
		}
		if (binds[bind_i].unpack_func == FUNC_UNSET) {
			assert(mbr->bound->num_targets == 1);

			struct bc_env *bc_env;
			bc_env = ast_gen_value_unpack_func(
					ctx->ast_ctx, ctx->ast_mod, ctx->ast_env,
					mbr->type, 0);

			struct func func = {0};
			func.type = stg_register_func_type(ctx->ast_mod->stg_mod,
					mbr->type, &mbr->type, 1);

			func.kind = FUNC_BYTECODE;
			func.bytecode = bc_env;

			binds[bind_i].unpack_func =
				stg_register_func(ctx->ast_mod->stg_mod, func);
		}

		binds[bind_i].value_params     = mbr->bound->member_deps;
		binds[bind_i].num_value_params = mbr->bound->num_member_deps;
		binds[bind_i].target =
			ast_dt_calculate_persistant_id(
					ctx, mbr_i);

		bind_i += 1;
	}

	assert(bind_i == num_bound_members);

	def->binds = binds;
	def->num_binds = num_bound_members;

	struct ast_dt_composite_info *info;
	info = calloc(1, sizeof(struct ast_dt_composite_info));

	info->num_members = comp->composite.num_members;
	info->members = local_members;

	def->data = info;

	struct type dt_type = {0};
	dt_type.base = &ast_dt_composite_base;
	dt_type.obj_def = def;
	dt_type.size = offset;
	dt_type.data = info;

	type_id result;
	result = stg_register_type(mod->stg_mod, dt_type);

	def->ret_type = ast_bind_slot_const_type(
			ctx->ast_ctx, &def->env, AST_BIND_NEW, NULL, result);

	return result;
}

type_id
ast_dt_finalize_composite(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *comp,
		struct ast_typecheck_closure *closures, size_t num_closures)
{
	if (comp->composite.type != TYPE_UNSET) {
		return comp->composite.type;
	}

	struct ast_dt_context dt_ctx = {0};
	dt_ctx.ast_ctx = ctx;
	dt_ctx.ast_env = env;
	dt_ctx.ast_mod = mod;
	dt_ctx.root_node = comp;
	dt_ctx.terminal_nodes = -1;
	dt_ctx.terminal_jobs  = -1;

	dt_ctx.closures = closures;
	dt_ctx.num_closures  = num_closures;

#if AST_DT_DEBUG_JOBS
	static size_t next_run_i = 0;
	dt_ctx.run_i  = next_run_i++;

	printf("\nbegin composite %zu\n", dt_ctx.run_i);
#endif

	int err;

	dt_ctx.target_names_resolved =
		ast_dt_job_nop(&dt_ctx, &dt_ctx.target_names_resolved);

	ast_dt_composite_populate(&dt_ctx, comp);

	err = ast_dt_run_jobs(&dt_ctx);
	if (err) {
#if AST_DT_DEBUG_JOBS
		printf("One or more jobs failed when resolving datastructure.\n");
#endif
		return TYPE_UNSET;
	}

	type_id result = TYPE_UNSET;

	if (dt_ctx.num_errors == 0) {
		result = ast_dt_composite_make_type(&dt_ctx, mod);
	}

	free(dt_ctx.members);
	free(dt_ctx.local_member_ids);

	for (struct ast_dt_bind *bind = dt_ctx.alloced_binds;
			bind != NULL;) {
		struct ast_dt_bind *this_bind = bind;
		bind = bind->next_alloced;
		free(this_bind);
	}

	for (size_t i = 0; i < dt_ctx.num_job_pages; i++) {
		int err;
		err = munmap(dt_ctx.job_pages[i], dt_ctx.page_size);
		if (err) {
			perror("munmap");
		}
	}
	free(dt_ctx.job_pages);

#if AST_DT_DEBUG_JOBS
	printf("end composite ");
	print_type_repr(ctx->vm, vm_get_type(ctx->vm, result));
	printf("\n\n");
#endif
	comp->composite.type = result;
	return result;
}
