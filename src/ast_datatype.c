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
	ast_member_id target;
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
	AST_DT_JOB_RESOLVE_NAMES,
	AST_DT_JOB_RESOLVE_TYPES,
	AST_DT_JOB_CODEGEN,
};

enum ast_dt_job_expr {
	AST_DT_JOB_EXPR_TARGET,
	AST_DT_JOB_EXPR_BIND,
	AST_DT_JOB_EXPR_TYPE,
};

struct ast_dt_job_dep {
	bool visited;
	ast_dt_job_id to;
};

struct ast_dt_job {
	enum ast_dt_job_kind kind;
	enum ast_dt_job_expr expr;

	size_t num_incoming_deps;
	size_t num_outgoing_deps;
	struct ast_dt_job_dep *outgoing_deps;

	// Used for the linked list terminal_jobs in ast_dt_context.
	ast_dt_job_id terminal_jobs;

	union {
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

	struct ast_context *ast_ctx;
	struct ast_env     *ast_env;
	struct ast_module  *ast_mod;

	struct ast_typecheck_closure *closures;
	size_t num_closures;

	size_t num_errors;
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

		for (size_t i = 0; i < jobs_per_page-1; i++) {
			new_jobs[i].free_list = i+1;
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
	job->free_list = -1;

	job->terminal_jobs = ctx->terminal_jobs;
	ctx->terminal_jobs = res;

	return res;
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

	job->expr = AST_DT_JOB_EXPR_TYPE;
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

	job->expr = AST_DT_JOB_EXPR_BIND;
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
		case AST_DT_JOB_RESOLVE_NAMES:
			return "RESOLVE_NAMES";

		case AST_DT_JOB_RESOLVE_TYPES:
			return "RESOLVE_TYPES";

		case AST_DT_JOB_CODEGEN:
			return "CODEGEN";

		case AST_DT_JOB_FREE:
			return "FREE";
	}
	return "(unknown)";
}
#endif

#if AST_DT_DEBUG_JOBS
static const char *
ast_dt_job_expr_name(enum ast_dt_job_expr expr) {
	switch (expr) {
		case AST_DT_JOB_EXPR_TARGET:
			return "TARGET";

		case AST_DT_JOB_EXPR_BIND:
			return "BIND";

		case AST_DT_JOB_EXPR_TYPE:
			return "TYPE";
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
	printf("0x%03x (%13s, %s: ", job_id,
			ast_dt_job_kind_name(job->kind),
			ast_dt_job_expr_name(job->expr));

	if (job->kind == AST_DT_JOB_FREE) {
		return;
	}

	ast_member_id mbr_id = -1;
	switch (job->expr) {
		case AST_DT_JOB_EXPR_TARGET:
		case AST_DT_JOB_EXPR_BIND:
			mbr_id = job->bind->target;
			break;

		case AST_DT_JOB_EXPR_TYPE:
			mbr_id = job->member;
			break;
	}

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);
	printf("mbr 0x%03x[%-10.*s])", mbr_id, ALIT(mbr->name));
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

#if AST_DT_DEBUG_JOBS
	printf("job dep ");
	ast_dt_print_job_desc(ctx, from_id);
	printf(" -> ");
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
ast_dt_bind_value_jobs_init(struct ast_dt_context *ctx, struct ast_dt_bind *bind)
{
	bind->value_jobs.resolve_names =
		ast_dt_job_bind(ctx, bind,
				AST_DT_JOB_RESOLVE_NAMES);

	bind->value_jobs.resolve_types =
		ast_dt_job_bind(ctx, bind,
				AST_DT_JOB_RESOLVE_TYPES);

	bind->value_jobs.codegen =
		ast_dt_job_bind(ctx, bind,
				AST_DT_JOB_CODEGEN);

	ast_dt_job_dependency(ctx,
			bind->value_jobs.resolve_names,
			bind->value_jobs.resolve_types);

	ast_dt_job_dependency(ctx,
			bind->value_jobs.resolve_types,
			bind->value_jobs.codegen);

	for (size_t i = 0; i < bind->num_member_deps; i++) {
		struct ast_dt_member *dep;
		dep = get_member(ctx, bind->member_deps[i]);

		ast_dt_job_dependency(ctx,
				dep->type_jobs.codegen,
				bind->value_jobs.codegen);
	}
}

static struct ast_dt_bind *
ast_dt_register_bind(struct ast_dt_context *ctx,
		struct stg_location loc, ast_member_id target,
		struct ast_node *value, bool overridable)
{
	struct ast_dt_member *member;
	member = get_member(ctx, target);

	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));
	new_bind->target = target;
	new_bind->kind = AST_OBJECT_DEF_BIND_VALUE;
	new_bind->value.node = value;
	new_bind->overridable = overridable;
	new_bind->loc = loc;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	if (member->bound) {
		if (!member->bound->overridable && !overridable) {
			stg_error(ctx->ast_ctx->err, loc,
					"'%.*s' is bound multiple times.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return new_bind;
		}

		if (member->bound->overridable && overridable) {
			stg_error(ctx->ast_ctx->err, loc,
					"'%.*s' has multiple default binds.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return new_bind;
		}

		if (overridable) {
			member->overridden_bind = new_bind;
		} else {
			member->overridden_bind = member->bound;
			member->bound = new_bind;
		}
	} else {
		member->bound = new_bind;
	}

	return new_bind;
}

static struct ast_dt_bind *
ast_dt_register_bind_func(struct ast_dt_context *ctx,
		ast_member_id target, func_id func,
		ast_member_id *value_params, size_t num_value_params,
		bool overridable)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));
	new_bind->target = target;
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

	new_bind->loc = STG_NO_LOC;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	struct ast_dt_member *member;
	member = get_member(ctx, target);

	if (member->bound) {
		if (!member->bound->overridable && !overridable) {
			stg_error(ctx->ast_ctx->err, STG_NO_LOC,
					"'%.*s' is bound multiple times.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return new_bind;
		}

		if (member->bound->overridable && overridable) {
			stg_error(ctx->ast_ctx->err, STG_NO_LOC,
					"'%.*s' has multiple default binds.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return new_bind;
		}

		if (overridable) {
			member->overridden_bind = new_bind;
		} else {
			member->overridden_bind = member->bound;
			member->bound = new_bind;
		}
	} else {
		member->bound = new_bind;
	}


	return new_bind;
}

static struct ast_dt_bind *
ast_dt_register_bind_const(struct ast_dt_context *ctx,
		ast_member_id target, struct object value, bool overridable)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));
	new_bind->target = target;
	new_bind->kind = AST_OBJECT_DEF_BIND_CONST;
	new_bind->const_value = value;
	new_bind->overridable = overridable;
	new_bind->member_deps = NULL;
	new_bind->num_member_deps = 0;

	new_bind->value_jobs.resolve_names = -1;
	new_bind->value_jobs.resolve_types = -1;
	new_bind->value_jobs.codegen = -1;

	new_bind->loc = STG_NO_LOC;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	struct ast_dt_member *member;
	member = get_member(ctx, target);

	if (member->bound) {
		if (!member->bound->overridable && !overridable) {
			stg_error(ctx->ast_ctx->err, STG_NO_LOC,
					"'%.*s' is bound multiple times.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return new_bind;
		}

		if (member->bound->overridable && overridable) {
			stg_error(ctx->ast_ctx->err, STG_NO_LOC,
					"'%.*s' has multiple default binds.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return new_bind;
		}

		if (overridable) {
			member->overridden_bind = new_bind;
		} else {
			member->overridden_bind = member->bound;
			member->bound = new_bind;
		}
	} else {
		member->bound = new_bind;
		member->const_value = new_bind->const_value;
		member->flags |= AST_DT_MEMBER_IS_CONST;
	}


	return new_bind;
}

static struct ast_dt_bind *
ast_dt_register_bind_pack(struct ast_dt_context *ctx,
		ast_member_id target, struct ast_object_def *pack,
		ast_member_id *value_params, size_t num_value_params)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));
	new_bind->target = target;
	new_bind->kind = AST_OBJECT_DEF_BIND_PACK;
	new_bind->pack = pack;
	new_bind->overridable = false;
	new_bind->member_deps = value_params;
	new_bind->num_member_deps = num_value_params;

	for (size_t i = 0; i < num_value_params; i++) {
		assert(value_params[i] >= 0);
	}

	new_bind->value_jobs.resolve_names = -1;
	new_bind->value_jobs.resolve_types = -1;
	new_bind->value_jobs.codegen = -1;

	new_bind->loc = STG_NO_LOC;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	struct ast_dt_member *member;
	member = get_member(ctx, target);

	if (member->bound) {
		panic("Object with obj_def was already bound.");
	} else {
		member->bound = new_bind;
	}

	return new_bind;
}

static ast_member_id
ast_dt_register_member(struct ast_dt_context *ctx,
		struct atom *name, struct stg_location decl_loc)
{
	struct ast_dt_member new_mbr = {0};

	new_mbr.name = name;
	new_mbr.decl_loc = decl_loc;
	new_mbr.next = ctx->terminal_nodes;
	new_mbr.first_child = -1;
	new_mbr.anscestor_local_member = -1;
	new_mbr.persistant_id = -1;

	ast_member_id mbr_id;
	mbr_id = dlist_append(
			ctx->members,
			ctx->num_members,
			&new_mbr);

	ctx->terminal_nodes = mbr_id;

	return mbr_id;
}

static void
ast_dt_member_type_jobs_init(struct ast_dt_context *ctx, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	mbr->type_jobs.resolve_names =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_RESOLVE_NAMES);

	mbr->type_jobs.resolve_types =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_RESOLVE_TYPES);

	mbr->type_jobs.codegen =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_CODEGEN);

	ast_dt_job_dependency(ctx,
			mbr->type_jobs.resolve_names,
			mbr->type_jobs.resolve_types);

	ast_dt_job_dependency(ctx,
			mbr->type_jobs.resolve_types,
			mbr->type_jobs.codegen);
}

static void
ast_dt_composite_populate(struct ast_dt_context *ctx, struct ast_node *node)
{
	assert(node->kind == AST_NODE_COMPOSITE);

	ast_member_id *members;
	members = calloc(node->composite.num_members, sizeof(ast_member_id));
	ctx->local_member_ids = members;

	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_datatype_member *mbr;
		mbr = &node->composite.members[i];

		// TODO: Better location.
		ast_member_id mbr_id;
		mbr_id = ast_dt_register_member(ctx, mbr->name,
				node->composite.members[i].loc);
		ast_dt_member_type_jobs_init(ctx, mbr_id);

		members[i] = mbr_id;

		struct ast_dt_member *new_mbr;
		new_mbr = get_member(ctx, mbr_id);

		new_mbr->type_node = node->composite.members[i].type;
		new_mbr->flags |= AST_DT_MEMBER_IS_LOCAL;
		new_mbr->first_child = -1;
	}

	for (size_t i = 0; i < node->composite.num_binds; i++) {
		ast_composite_node_resolve_names(
				ctx->ast_ctx, ctx->ast_env, NULL, NULL, false,
				node, node->composite.binds[i].target, members);
	}

	for (size_t i = 0; i < node->composite.num_binds; i++) {
		struct ast_node *target_node;
		target_node = node->composite.binds[i].target;

		// TODO: Support more complex bind targets.
		assert(target_node->kind == AST_NODE_LOOKUP);
		assert(target_node->lookup.ref.kind == AST_NAME_REF_MEMBER);

		struct ast_dt_bind *bind;
		bind = ast_dt_register_bind(ctx, node->composite.binds[i].loc,
				target_node->lookup.ref.member,
				node->composite.binds[i].value,
				node->composite.binds[i].overridable);
		ast_dt_bind_value_jobs_init(ctx, bind);
	}

	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_datatype_member *node_mbr;
		node_mbr = &node->composite.members[i];

		ast_member_id mbr_id;
		mbr_id = members[i];

		struct ast_dt_member *mbr;
		mbr = get_member(ctx, mbr_id);

		if (mbr->bound) {
			// TODO: We should evaluate the binds that are being overridden.

			if (!mbr->type_node) {
				ast_dt_job_dependency(ctx,
						mbr->bound->value_jobs.resolve_types,
						mbr->type_jobs.resolve_names);
			}
		} else {
			assert(mbr->type_node);
		}
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

		ast_slot_id first_child = -1;

		// Be careful with parent and anscestor as ast_dt_register_member can
		// invalidate those pointers.

		for (size_t i = 0; i < def->num_params; i++) {
			// TODO: Better location.
			ast_slot_id param_mbr_id;
			param_mbr_id = ast_dt_register_member(
					ctx, def->params[i].name, STG_NO_LOC);

			if (first_child < 0) {
				first_child = param_mbr_id;

				anscestor = get_member(ctx, local_anscestor);
				assert((anscestor->flags & AST_DT_MEMBER_IS_LOCAL) != 0);
				if (anscestor->first_child < 0) {
					anscestor->first_child = first_child;
				}
			}

			struct ast_dt_member *child;
			child = get_member(ctx, param_mbr_id);
			child->anscestor_local_member = local_anscestor;

			ast_slot_id param_type_slot;
			param_type_slot = ast_env_slot(ctx->ast_ctx, &def->env, def->params[i].slot).type;

			int err;
			err = ast_slot_pack_type(ctx->ast_ctx, ctx->ast_mod, &def->env,
					param_type_slot, &child->type);
			if (err) {
				printf("Failed to evaluate obj_def member type.");
				return;
			}
			assert(child->type != TYPE_UNSET);

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
							mbr_id, def->binds[i].value.func,
							deps, def->binds[i].num_value_params,
							def->binds[i].value.overridable);
					break;

				case AST_OBJECT_DEF_BIND_CONST:
					ast_dt_register_bind_const(ctx,
							mbr_id, def->binds[i].const_value,
							def->binds[i].value.overridable);
					break;

				case AST_OBJECT_DEF_BIND_PACK:
					ast_dt_register_bind_pack(ctx,
							mbr_id, def->binds[i].pack,
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
					mbr->type_jobs.codegen,
					target_job);
			break;

		case AST_NAME_DEP_REQUIRE_VALUE:
			if (!mbr->bound) {
				printf("Can not depend on the value of this "
						"member as it is not bound.\n");
				ctx->num_errors += 1;
				return;
			}
			if (mbr->bound->overridable) {
				printf("Can not depend on the value of this "
						"member as it is overridable.\n");
				ctx->num_errors += 1;
				return;
			}

			ast_dt_job_dependency(ctx,
					mbr->bound->value_jobs.codegen,
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

static void
ast_dt_bind_typecheck_dep(struct ast_dt_context *ctx,
		struct ast_typecheck_dep *dep)
{
	if (!dep->determined) {
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

static inline int
ast_dt_dispatch_job(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);

#if AST_DT_DEBUG_JOBS
	printf("Dispatch job ");
	ast_dt_print_job_desc(ctx, job_id);
	printf("\n");
#endif

	struct ast_node *node;
	enum ast_name_dep_requirement dep_req;
	dep_req = AST_NAME_DEP_REQUIRE_TYPE;

	switch (job->expr) {
		case AST_DT_JOB_EXPR_TARGET:
			return 0;

		case AST_DT_JOB_EXPR_BIND:
			node = job->bind->value.node;
			break;

		case AST_DT_JOB_EXPR_TYPE:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				node = mbr->type_node;

				if (!node) {
					assert(mbr->type != TYPE_UNSET);
					return 0;
				}

				dep_req = AST_NAME_DEP_REQUIRE_VALUE;
			}
			break;

		default:
			panic("Invalid job expr.");
			return -1;
	}

	switch (job->kind) {
		case AST_DT_JOB_RESOLVE_NAMES:
			{
				bool require_const = false;
				ast_dt_job_id next_job = -1;

				switch (job->expr) {
					case AST_DT_JOB_EXPR_TARGET:
						break;

					case AST_DT_JOB_EXPR_BIND:
						next_job = job->bind->value_jobs.resolve_types;
						break;

					case AST_DT_JOB_EXPR_TYPE:
						{
							struct ast_dt_member *mbr;
							mbr = get_member(ctx, job->member);

							next_job = mbr->type_jobs.resolve_types;

							require_const = true;
						}
						break;
				}

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->ast_env,
						ctx->ast_mod->stg_mod->native_mod,
						NULL, require_const, ctx->root_node, node,
						ctx->local_member_ids);
				if (err) {
					printf("Failed to resolve names.\n");
					break;
				}

				ast_dt_find_named_dependencies(
						ctx, next_job, dep_req, node);
			}
			return 0;

		case AST_DT_JOB_RESOLVE_TYPES:
			{
				if (job->expr == AST_DT_JOB_EXPR_BIND) {
					struct ast_dt_member *target;
					target = get_member(ctx, job->bind->target);
					if (job->bind->kind == AST_OBJECT_DEF_BIND_PACK) {
						struct ast_object_def *def;
						def = job->bind->pack;
						assert(def);

						type_id type;
						int err;
						err = ast_slot_pack_type(
								ctx->ast_ctx, ctx->ast_mod, &def->env,
								def->ret_type, &type);
						if (err) {
							printf("Failed to pack return type for pack bind.\n");
							return -1;
						}

						if (target->type == TYPE_UNSET) {
							target->type = type;
						} else {
							assert_type_equals(ctx->ast_ctx->vm,
									target->type, type);
						}

						return 0;
					} else if (job->bind->kind == AST_OBJECT_DEF_BIND_CONST) {
						if (target->type == TYPE_UNSET) {
							target->type = job->bind->const_value.type;
						} else {
							assert_type_equals(ctx->ast_ctx->vm,
									target->type, job->bind->const_value.type);
						}

						return 0;
					}
				}
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

								if (deps[i].req == AST_NAME_DEP_REQUIRE_VALUE) {
									assert((dep_mbr->flags & AST_DT_MEMBER_IS_CONST) != 0);

									body_deps[i].determined = true;
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
							{
								assert(deps[i].ref.closure >= 0 &&
										deps[i].ref.closure < ctx->num_closures);
								struct ast_typecheck_closure *cls;
								cls = &ctx->closures[deps[i].ref.closure];
								body_deps[i].ref = deps[i].ref;
								body_deps[i].req = cls->req;

								body_deps[i].determined = true;

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
					printf("Failed to typecheck.\n");
					return -1;
				}

				if (job->expr == AST_DT_JOB_EXPR_BIND) {
					ast_member_id mbr_id;
					mbr_id = job->bind->target;

					struct ast_dt_member *mbr;
					mbr = get_member(ctx, mbr_id);

					if (mbr->type != TYPE_UNSET) {
						assert_type_equals(ctx->ast_ctx->vm, node->type, mbr->type);
					} else {
						mbr->type = node->type;
						assert(mbr->type != TYPE_UNSET);

						struct type *mbr_type;
						mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

						if (mbr_type->obj_def) {
							size_t num_children;
							ast_member_id *children;

							num_children = mbr_type->obj_def->num_params;
							children = calloc(num_children, sizeof(ast_member_id));

							for (size_t i = 0; i < num_children; i++) {
								children[i] = mbr->first_child + i;
							}

							ast_dt_register_bind_pack(ctx,
									mbr_id, mbr_type->obj_def,
									children, num_children);
						}

						ast_dt_populate_descendants(ctx, mbr_id, mbr_id);
						ast_dt_populate_descendant_binds(ctx, mbr_id);
					}
				}
			}
			return 0;

		case AST_DT_JOB_CODEGEN:
			{
				if (job->expr == AST_DT_JOB_EXPR_BIND) {
					struct ast_dt_member *target;
					target = get_member(ctx, job->bind->target);
					if (job->bind->kind == AST_OBJECT_DEF_BIND_PACK) {
						struct ast_object_def *def;
						def = job->bind->pack;
						assert(def);

						printf("pack\n");

						ast_member_id first_child;
						if ((target->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
							first_child = target->first_child;
						} else {
							first_child = job->bind->target + 1;
						}

						assert(job->bind->num_member_deps == def->num_params);

						bool is_const = true;
						void *params[def->num_params];

						for (size_t i = 0; i < def->num_params; i++) {
							struct ast_dt_member *dep;
							dep = get_member(ctx, first_child+i);

							if ((dep->flags & AST_DT_MEMBER_IS_CONST) == 0) {
								printf("%10.*s is not const\n", ALIT(dep->name));
								is_const = false;
							} else {
								printf("%10.*s is     const\n", ALIT(dep->name));
							}

							params[i] = dep->const_value.data;
						}

						printf("is const %i\n", is_const);
						if (is_const) {
							struct type *type;
							type = vm_get_type(ctx->ast_ctx->vm, target->type);

							uint8_t value_buffer[type->size];

							def->pack_func(
									ctx->ast_ctx->vm,
									def->data, value_buffer,
									params, def->num_params);

							struct object val = {0};
							val.type = target->type;
							val.data = value_buffer;

							target->const_value = register_object(
									ctx->ast_ctx->vm, ctx->ast_env->store, val);

							printf("const value:\n");
							print_obj_repr(ctx->ast_ctx->vm, target->const_value);
							printf("\n");

							target->flags |= AST_DT_MEMBER_IS_CONST;
						}

						return 0;
					} else if (job->bind->kind == AST_OBJECT_DEF_BIND_CONST) {
						target->const_value = job->bind->const_value;
						target->flags |= AST_DT_MEMBER_IS_CONST;

						return 0;
					}
				}
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

				ast_member_id dep_members[num_dep_members];
				type_id dep_member_types[num_dep_members];

				for (size_t i = 0; i < num_names; i++) {
					if (names[i].ref.kind == AST_NAME_REF_MEMBER) {
						dep_members[i] = names[i].ref.member;
						struct ast_dt_member *mbr = get_member(ctx, dep_members[i]);
						assert(mbr->type != TYPE_UNSET);
						dep_member_types[i] = mbr->type;
					}
				}

				num_names = 0;
				free(names);
				names = NULL;

				struct bc_env *bc_env;
				bc_env =
					ast_composite_bind_gen_bytecode(
							ctx->ast_ctx, ctx->ast_mod, ctx->ast_env,
							dep_members, dep_member_types, num_dep_members,
							ctx->closures, ctx->num_closures, node);

				struct ast_dt_member *mbr;

				type_id expected_type;

				switch (job->expr) {
					case AST_DT_JOB_EXPR_TARGET:
						panic("Invalid target job.");
						return -1;

					case AST_DT_JOB_EXPR_BIND:
						mbr = get_member(ctx, job->bind->target);
						assert(mbr->type != TYPE_UNSET);
						expected_type = mbr->type;
						break;

					case AST_DT_JOB_EXPR_TYPE:
						mbr = get_member(ctx, job->member);
						expected_type = ctx->ast_ctx->types.type;
						break;
				}

				struct func func = {0};
				func.type = stg_register_func_type(ctx->ast_mod->stg_mod,
						expected_type, dep_member_types, num_dep_members);

				func.kind = FUNC_BYTECODE;
				func.bytecode = bc_env;

				func_id fid;
				fid = stg_register_func(ctx->ast_mod->stg_mod, func);

				switch (job->expr) {
					case AST_DT_JOB_EXPR_TARGET:
						panic("Tried to generate bytecode for target.");
						break;

					case AST_DT_JOB_EXPR_BIND:
						{
							job->bind->num_member_deps = num_dep_members;
							job->bind->member_deps = calloc(
									job->bind->num_member_deps, sizeof(ast_member_id));

							bool is_const = true;

							struct object dep_member_values[job->bind->num_member_deps];

							for (size_t i = 0; i < job->bind->num_member_deps; i++) {
								job->bind->member_deps[i] = dep_members[i];
								assert(dep_members[i] >= 0);

								struct ast_dt_member *dep_mbr;
								dep_mbr = get_member(ctx, dep_members[i]);

								if ((dep_mbr->flags & AST_DT_MEMBER_IS_CONST) == 0) {
									is_const = false;
								}

								dep_member_values[i] = dep_mbr->const_value;
							}

							job->bind->value.func = fid;

							if (is_const && !job->bind->overridable) {
								struct type *ret_type;
								ret_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

								uint8_t buffer[ret_type->size];
								struct object obj = {0};
								obj.type = mbr->type;
								obj.data = buffer;

								int err;
								err = vm_call_func(ctx->ast_ctx->vm, fid, dep_member_values,
										job->bind->num_member_deps, &obj);
								if (err) {
									printf("Failed to evaluate constant member.\n");
									return -1;
								}

								mbr->const_value =
									register_object(ctx->ast_ctx->vm, ctx->ast_env->store, obj);
								mbr->flags |= AST_DT_MEMBER_IS_CONST;
							}
						}
						break;

					case AST_DT_JOB_EXPR_TYPE:
						{
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

							if (mbr->type != TYPE_UNSET) {
								assert_type_equals(ctx->ast_ctx->vm, mbr->type, out_type);
							} else {
								mbr->type = out_type;

								struct type *mbr_type;
								mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

								ast_dt_populate_descendants(ctx, job->member, job->member);
								ast_dt_populate_descendant_binds(ctx, job->member);

								// We re-fetch mbr as it might have been
								// invalidated by ast_dt_populate_descendant_binds.
								mbr = get_member(ctx, job->member);

								if (mbr_type->obj_def) {
									size_t num_children;
									ast_member_id *children;

									num_children = mbr_type->obj_def->num_params;
									children = calloc(num_children, sizeof(ast_member_id));

									for (size_t i = 0; i < num_children; i++) {
										children[i] = mbr->first_child + i;
									}

									struct ast_dt_bind *bind;
									bind = ast_dt_register_bind_pack(ctx,
											job->member, mbr_type->obj_def,
											children, num_children);
									ast_dt_bind_value_jobs_init(ctx, bind);
								}

							}
						}
						break;
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

	assert(job->kind != AST_DT_JOB_FREE);

	struct ast_dt_expr_jobs *jobs = NULL;

	switch (job->expr) {
		case AST_DT_JOB_EXPR_TARGET:
			return;

		case AST_DT_JOB_EXPR_BIND:
			jobs = &job->bind->value_jobs;
			break;

		case AST_DT_JOB_EXPR_TYPE:
			jobs = &get_member(ctx, job->member)->type_jobs;
			break;
	}

	assert(jobs);

	switch (job->kind) {
		case AST_DT_JOB_FREE:
			return;

		case AST_DT_JOB_RESOLVE_NAMES:
			assert(jobs->resolve_names == job_id);
			jobs->resolve_names = -1;
			break;

		case AST_DT_JOB_RESOLVE_TYPES:
			assert(jobs->resolve_types == job_id);
			jobs->resolve_types = -1;
			break;

		case AST_DT_JOB_CODEGEN:
			assert(jobs->codegen == job_id);
			jobs->codegen = -1;
			break;
	}
}

static int
ast_dt_run_jobs(struct ast_dt_context *ctx)
{
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
			printf("job failed!\n");
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

static struct object
ast_dt_unpack_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, int param_id, struct object obj)
{
	struct ast_dt_composite_info *info = def->data;

	assert(param_id >= 0 && param_id < info->num_members);

	struct object result = {0};
	result.data = ((uint8_t *)obj.data) + info->members[param_id].location;
	result.type = info->members[param_id].type;

	return result;
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
	def->pack_func = ast_dt_pack_func;
	def->unpack    = ast_dt_unpack_func;

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

		params[i].slot =
			ast_bind_slot_member(ctx->ast_ctx, &def->env, AST_BIND_NEW, NULL,
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

		// int err;
		// err = ast_dt_make_bind_func(ctx, mod, mbr->bound);

		binds[bind_i].kind = mbr->bound->kind;
		switch (mbr->bound->kind) {
			case AST_OBJECT_DEF_BIND_VALUE:
				if ((mbr->flags & AST_DT_MEMBER_IS_CONST) == 0) {
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
				if ((mbr->flags & AST_DT_MEMBER_IS_CONST) == 0) {
					assert(mbr->bound->pack);
					binds[bind_i].pack = mbr->bound->pack;
				} else {
					binds[bind_i].kind = AST_OBJECT_DEF_BIND_CONST;
					binds[bind_i].const_value = mbr->const_value;
					printf("%.*s is const\n", ALIT(mbr->name));
				}
				break;
		}

		for (size_t i = 0; i < mbr->bound->num_member_deps; i++) {
			mbr->bound->member_deps[i] =
				ast_dt_calculate_persistant_id(
						ctx, mbr->bound->member_deps[i]);
		}

		binds[bind_i].value_params     = mbr->bound->member_deps;
		binds[bind_i].num_value_params = mbr->bound->num_member_deps;
		binds[bind_i].target =
			ast_dt_calculate_persistant_id(
					ctx, mbr->bound->target);

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

	int err;

	ast_dt_composite_populate(&dt_ctx, comp);

	err = ast_dt_run_jobs(&dt_ctx);
	if (err) {
		printf("One or more jobs failed when resolving datastructure.\n");
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

	comp->composite.type = result;
	return result;
}
