#include "vm.h"
#include "ast.h"
#include "dlist.h"
#include "module.h"
#include "native_bytecode.h"
#include "term_color.h"
#include "base/mod.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

#define AST_DT_DEBUG_JOBS 0

typedef int ast_dt_job_id;
typedef int ast_dt_bind_id;
typedef int ast_dt_expr_id;
typedef int ast_dt_composite_id;
typedef struct {
	ast_dt_composite_id parent;
	ast_use_id id;
} ast_dt_use_id;

static inline ast_dt_use_id
dt_use_id(ast_dt_composite_id parent, ast_use_id id)
{
	ast_dt_use_id res;
	res.parent = parent;
	res.id = id;
	return res;
}

struct ast_dt_mbr_id {
	ast_member_id local_member;
	int descendant;
};

struct ast_dt_expr_jobs {
	ast_dt_job_id resolve_names;
	ast_dt_job_id resolve_types;
	ast_dt_job_id codegen;
};

struct ast_dt_expr {
	bool constant;
	ast_dt_composite_id parent;
	struct object const_value;

	// Used when finalizing its parent's data type.
	int comp_local_id;

	struct {
		struct ast_node *node;
		func_id func;
	} value;

	type_id type;

	struct atom *trivial_name;

	struct stg_location loc;

	struct ast_gen_dt_param *deps;
	size_t num_deps;

	struct ast_dt_expr_jobs value_jobs;
};

struct ast_dt_bind {
	struct ast_node *target_node;
	ast_dt_expr_id expr;

	bool overridable;

	struct {
		ast_dt_job_id resolve_names;
	} target_jobs;

	struct stg_location loc;
};

struct ast_dt_dependency;

enum ast_dt_member_flags {
	AST_DT_MEMBER_IS_CONST = 0x2,
};

enum ast_dt_member_bind_flags {
	AST_DT_MBR_BIND_TYPEGIVING = 0x1,
	AST_DT_MBR_BIND_UNPACK_ID_RESOLVED = 0x2,
};

struct ast_dt_member_bind {
	enum ast_dt_member_bind_flags flags;
	struct ast_node *l_expr;
	ast_dt_bind_id bind;
	size_t unpack_id;
};

struct ast_dt_member {
	struct atom *name;
	type_id type;
	struct ast_node *type_node;
	ast_dt_composite_id parent;
	ast_dt_composite_id sub_composite;

	struct stg_location decl_loc;

	struct ast_dt_member_bind *binds;
	size_t num_binds;
	bool local_explicitly_bound;

	enum ast_dt_member_flags flags;

	ast_dt_job_id type_names_resolved;
	ast_dt_job_id type_resolved;
	ast_dt_job_id const_resolved;

	struct object const_value;

	ast_member_id persistant_id;
};

struct ast_dt_composite {
	struct ast_node *root_node;
	// An array of the ids of all local members, in the same order as
	// root_node->composite.members. The lenght is root_node->composite.num_members.
	ast_member_id *local_member_ids;

	ast_member_id self;

	struct ast_dt_use *uses;
	size_t num_uses;

	struct ast_typecheck_closure *closures;
	size_t num_closures;

	ast_dt_job_id resolve_names;
	ast_dt_job_id closures_evaled;
	ast_dt_job_id finalize_job;

	ast_dt_job_id target_names_resolved;
	ast_dt_job_id use_resolved;

	type_id type;
};

struct ast_dt_use {
	ast_dt_expr_id expr;
	ast_dt_composite_id parent;
	struct atom *as_name;

	type_id type;

	bool is_const;
	struct object const_value;

	ast_dt_job_id const_resolved;
};

struct ast_dt_init_expr {
	ast_init_expr_id id;
	ast_dt_composite_id parent;
	ast_dt_expr_id expr;
};

struct ast_dt_dependency {
	ast_member_id from, to;
	bool visited;
};

enum ast_dt_job_kind {
	AST_DT_JOB_FREE = 0,
	AST_DT_JOB_NOP,

	AST_DT_JOB_COMPOSITE_RESOLVE_NAMES,
	AST_DT_JOB_COMPOSITE_EVAL_CLOSURE,
	AST_DT_JOB_COMPOSITE_PACK,
	AST_DT_JOB_COMPOSITE_CONST_EVAL,

	AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES,
	AST_DT_JOB_MBR_TYPE_EVAL,
	AST_DT_JOB_MBR_CONST_EVAL,

	AST_DT_JOB_EXPR_RESOLVE_NAMES,
	AST_DT_JOB_EXPR_RESOLVE_TYPES,
	AST_DT_JOB_EXPR_CODEGEN,

	AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES,

	AST_DT_JOB_USE_CONST_EVAL,
};

struct ast_dt_job_dep {
	bool visited;
	ast_dt_job_id to;
};

struct ast_dt_job {
	enum ast_dt_job_kind kind;

	// Use to keep track of internal vertex id in cycle detection.
	int aux_id;

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
		ast_dt_expr_id expr;

		ast_dt_bind_id bind;

		ast_dt_use_id use;

		ast_dt_composite_id composite;

		// Used when the job is not allocated.
		ast_dt_job_id free_list;
	};
};

struct ast_dt_context {
	struct arena *tmp_mem;

	struct paged_list jobs;
	ast_dt_job_id free_list;
	// A linked list of nodes that have no incoming edges.
	ast_dt_job_id terminal_jobs;
	size_t unvisited_job_deps;

	struct paged_list composites;
	struct paged_list exprs;
	struct paged_list binds;
	struct paged_list members;

	struct ast_dt_init_expr *init_exprs;
	size_t num_init_exprs;

	struct ast_context *ast_ctx;
	struct stg_module  *mod;

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
	return paged_list_get(&ctx->jobs, id);
}

static inline struct ast_dt_expr *
get_expr(struct ast_dt_context *ctx, ast_dt_expr_id id)
{
	return paged_list_get(&ctx->exprs, id);
}

static inline struct ast_dt_bind *
get_bind(struct ast_dt_context *ctx, ast_dt_bind_id id)
{
	return paged_list_get(&ctx->binds, id);
}

static inline struct ast_dt_member *
get_member(struct ast_dt_context *ctx, ast_member_id id)
{
	return paged_list_get(&ctx->members, id);
}

static inline struct ast_dt_composite *
get_composite(struct ast_dt_context *ctx, ast_dt_composite_id id)
{
	return paged_list_get(&ctx->composites, id);
}

static inline struct ast_dt_use *
get_use(struct ast_dt_context *ctx, ast_dt_use_id id)
{
	struct ast_dt_composite *parent;
	parent = get_composite(ctx, id.parent);
	assert(id.id < parent->num_uses);
	return &parent->uses[id.id];
}

static inline struct ast_dt_init_expr *
get_init_expr(struct ast_dt_context *ctx, ast_init_expr_id id)
{
	for (size_t i = 0; i < ctx->num_init_exprs; i++) {
		if (ctx->init_exprs[i].id == id) {
			return &ctx->init_exprs[i];
		}
	}
	panic("Invalid init expr id.\n");
	return NULL;
}

static ast_dt_expr_id
ast_dt_alloc_expr(struct ast_dt_context *ctx)
{
	return paged_list_push(&ctx->exprs);
}

static ast_dt_expr_id
ast_dt_alloc_bind(struct ast_dt_context *ctx)
{
	return paged_list_push(&ctx->binds);
}

static ast_dt_expr_id
ast_dt_alloc_member(struct ast_dt_context *ctx)
{
	return paged_list_push(&ctx->members);
}

static ast_dt_composite_id
ast_dt_alloc_composite(struct ast_dt_context *ctx)
{
	return paged_list_push(&ctx->composites);
}


static ast_dt_use_id
ast_dt_alloc_use(struct ast_dt_context *ctx, ast_dt_composite_id parent_id)
{
	struct ast_dt_use use = {0};

	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);

	ast_use_id id;
	id = dlist_append(
			parent->uses,
			parent->num_uses,
			&use);

	return dt_use_id(parent_id, id);
}

static int
ast_dt_alloc_init_expr(struct ast_dt_context *ctx)
{
	struct ast_dt_init_expr init_expr = {0};

	return dlist_append(
			ctx->init_exprs,
			ctx->num_init_exprs,
			&init_expr);
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

	job->free_list = ctx->free_list;
	ctx->free_list = id;
}

static ast_dt_job_id
ast_dt_alloc_job(struct ast_dt_context *ctx)
{
	ast_dt_job_id res = -1;
	if (ctx->free_list == -1) {
		res = ctx->free_list;
	} else {
		res = paged_list_push(&ctx->jobs);
	}

	assert(res >= 0);

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
ast_dt_job_composite(struct ast_dt_context *ctx,
		ast_dt_composite_id comp, enum ast_dt_job_kind kind)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);
	assert(comp < ctx->composites.length);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = kind;

	job->composite = comp;

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
ast_dt_job_expr(struct ast_dt_context *ctx,
		ast_dt_expr_id expr, enum ast_dt_job_kind kind)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = kind;
	job->expr = expr;

	return job_id;
}

static ast_dt_job_id
ast_dt_job_bind(struct ast_dt_context *ctx,
		ast_dt_bind_id bind, enum ast_dt_job_kind kind)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = kind;
	job->bind = bind;

	return job_id;
}

static ast_dt_job_id
ast_dt_job_use(struct ast_dt_context *ctx,
		ast_dt_use_id use,
		enum ast_dt_job_kind kind)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = kind;
	job->use  = use;

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
		case AST_DT_JOB_COMPOSITE_RESOLVE_NAMES:
			return "COMP NAMES";
		case AST_DT_JOB_COMPOSITE_EVAL_CLOSURE:
			return "COMP CLOSURE";
		case AST_DT_JOB_COMPOSITE_PACK:
			return "COMP PACK";
		case AST_DT_JOB_COMPOSITE_CONST_EVAL:
			return "COMP CONSTEVAL";
		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
			return "MBR TPE NAMES";
		case AST_DT_JOB_MBR_TYPE_EVAL:
			return "MBR TPE EVAL";
		case AST_DT_JOB_MBR_CONST_EVAL:
			return "MBR CONST EVAL";
		case AST_DT_JOB_EXPR_RESOLVE_NAMES:
			return "EXP VAL NAMES";
		case AST_DT_JOB_EXPR_RESOLVE_TYPES:
			return "EXP VAL TYPES";
		case AST_DT_JOB_EXPR_CODEGEN:
			return "EXP VAL CODEGEN";
		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			return "BND TAR NAMES";

		case AST_DT_JOB_USE_CONST_EVAL:
			return "USE CONST EVAL";

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

		case AST_DT_JOB_COMPOSITE_RESOLVE_NAMES:
		case AST_DT_JOB_COMPOSITE_EVAL_CLOSURE:
		case AST_DT_JOB_COMPOSITE_PACK:
		case AST_DT_JOB_COMPOSITE_CONST_EVAL:
			{
				// struct ast_dt_composite *comp;
				// comp = get_composite(ctx, job->comp);
				printf("comp %i", job->composite);
			}
			break;

		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
		case AST_DT_JOB_MBR_TYPE_EVAL:
		case AST_DT_JOB_MBR_CONST_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				printf("mbr 0x%03x[%-10.*s]",
						job->member, ALIT(mbr->name));
			}
			break;

		case AST_DT_JOB_EXPR_RESOLVE_NAMES:
		case AST_DT_JOB_EXPR_RESOLVE_TYPES:
		case AST_DT_JOB_EXPR_CODEGEN:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);

				printf("expr ");
				if (expr->constant) {
				} else if (expr->value.node) {
					ast_print_node(ctx->ast_ctx,
							expr->value.node, false);
				} else {
					printf("func %lu", expr->value.func);
				}
			}
			break;

		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			{
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, job->bind);
				printf("bind ");
				if (bind->target_node) {
					ast_print_node(ctx->ast_ctx,
							bind->target_node, false);
				}
			}
			break;

		case AST_DT_JOB_USE_CONST_EVAL:
			{
				struct ast_dt_use *use;
				use = get_use(ctx, job->use);

				struct ast_dt_expr *expr;
				expr = get_expr(ctx, use->expr);

				printf("use %i,%i ", job->use.parent, job->use.id);
				if (expr->constant) {
				} else if (expr->value.node) {
					ast_print_node(ctx->ast_ctx,
							expr->value.node, false);
				} else {
					printf("func %lu", expr->value.func);
				}
			}
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

	assert(from->kind != AST_DT_JOB_FREE);

	for (size_t i = 0; i < from->num_outgoing_deps; i++) {
		if (from->outgoing_deps[i].to == to_id) {
			return;
		}
	}

#if AST_DT_DEBUG_JOBS
	printf("%03zx " TC(TC_BRIGHT_BLUE, "job dep") " ", ctx->run_i);
	ast_dt_print_job_desc(ctx, from_id);
	// Move the cursor to column 50 to align the dependent jobs.
	printf("\033[60G " TC(TC_BRIGHT_BLUE, "->") " ");
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

// When a lookup is determined to not refer to a local name it can either refer
// to a field exposed by a 'use' statement or to a closure. Expressions
// containing such lookups are considered ambigous.
static inline bool
ast_dt_expr_has_ambiguous_refs(
		struct ast_dt_context *ctx, ast_dt_composite_id parent_id,
		struct ast_node *node, struct atom *self_name)
{
	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);
	int res;
	res = ast_composite_node_has_ambiguous_refs(
			ctx->ast_ctx, NULL, parent->root_node,
			node, parent->local_member_ids, self_name);
	assert(res >= 0);

	return res != 0;
}

static void
ast_dt_bind_to_member_explicit(struct ast_dt_context *ctx,
		ast_member_id mbr_id, ast_dt_bind_id bind_id,
		int unpack_id, bool typegiving)
{
	struct ast_dt_member *member;
	member = get_member(ctx, mbr_id);

	struct ast_dt_bind *new_bind;
	new_bind = get_bind(ctx, bind_id);

	struct ast_dt_member_bind mbr_bind = {0};
	mbr_bind.flags |= typegiving ? AST_DT_MBR_BIND_TYPEGIVING : 0;
	mbr_bind.flags |= AST_DT_MBR_BIND_UNPACK_ID_RESOLVED;

	assert(bind_id >= 0);
	mbr_bind.bind = bind_id;
	mbr_bind.unpack_id = unpack_id;

	dlist_append(
			member->binds,
			member->num_binds,
			&mbr_bind);
}

static void
ast_dt_bind_to_member(struct ast_dt_context *ctx,
		ast_member_id mbr_id, ast_dt_bind_id bind_id,
		struct ast_node *target, bool typegiving)
{
	struct ast_dt_member *member;
	member = get_member(ctx, mbr_id);

	struct ast_dt_bind *new_bind;
	new_bind = get_bind(ctx, bind_id);

	struct ast_dt_member_bind mbr_bind = {0};
	mbr_bind.flags |= typegiving ? AST_DT_MBR_BIND_TYPEGIVING : 0;

	assert(bind_id >= 0);
	mbr_bind.l_expr = target;
	mbr_bind.bind = bind_id;

	dlist_append(
			member->binds,
			member->num_binds,
			&mbr_bind);
}

static void
ast_dt_composite_populate(struct ast_dt_context *ctx,
		ast_dt_composite_id parent_id, struct ast_node *node);

static ast_dt_composite_id
ast_dt_register_composite(
		struct ast_dt_context *ctx, struct ast_node *root_node,
		bool closure_available, struct ast_typecheck_closure *closures, size_t num_closures)
{
	ast_dt_composite_id comp_id;
	comp_id = ast_dt_alloc_composite(ctx);

	struct ast_dt_composite *comp;
	comp = get_composite(ctx, comp_id);

	comp->root_node = root_node;
	comp->closures = closures;
	comp->num_closures = num_closures;
	comp->self = -1;

	comp->finalize_job =
		ast_dt_job_composite(ctx, comp_id,
				AST_DT_JOB_COMPOSITE_PACK);

	comp->target_names_resolved =
		ast_dt_job_nop(ctx, &comp->target_names_resolved);
	comp->use_resolved =
		ast_dt_job_nop(ctx, &comp->use_resolved);

	comp->resolve_names = -1;
	comp->closures_evaled = -1;

	if (!closure_available) {
		assert(comp->num_closures == 0);

		comp->resolve_names =
			ast_dt_job_composite(ctx, comp_id,
					AST_DT_JOB_COMPOSITE_RESOLVE_NAMES);
		comp->closures_evaled =
			ast_dt_job_composite(ctx, comp_id,
					AST_DT_JOB_COMPOSITE_EVAL_CLOSURE);

		ast_dt_job_dependency(ctx,
				comp->resolve_names,
				comp->closures_evaled);
		ast_dt_job_dependency(ctx,
				comp->closures_evaled,
				comp->finalize_job);
	}

	ast_dt_composite_populate(
			ctx, comp_id, root_node);

	return comp_id;
}

static ast_dt_expr_id
ast_dt_register_expr(struct ast_dt_context *ctx,
		ast_dt_composite_id parent_id,
		struct ast_node *value, struct atom *trivial_name)
{
	ast_dt_expr_id expr_i;
	expr_i = ast_dt_alloc_expr(ctx);

	struct ast_dt_expr *new_expr;
	new_expr = get_expr(ctx, expr_i);

	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);

	new_expr->constant = false;
	new_expr->parent = parent_id;
	new_expr->value.node = value;
	new_expr->value.func = FUNC_UNSET;
	new_expr->type = TYPE_UNSET;
	new_expr->loc = value->loc;
	new_expr->trivial_name = trivial_name;
	new_expr->comp_local_id = -1;

	new_expr->value_jobs.resolve_names =
		ast_dt_job_expr(ctx, expr_i,
				AST_DT_JOB_EXPR_RESOLVE_NAMES);

	new_expr->value_jobs.resolve_types =
		ast_dt_job_expr(ctx, expr_i,
				AST_DT_JOB_EXPR_RESOLVE_TYPES);

	new_expr->value_jobs.codegen =
		ast_dt_job_expr(ctx, expr_i,
				AST_DT_JOB_EXPR_CODEGEN);

	ast_dt_job_dependency(ctx,
			parent->closures_evaled,
			new_expr->value_jobs.resolve_types);

	ast_dt_job_dependency(ctx,
			new_expr->value_jobs.resolve_names,
			new_expr->value_jobs.resolve_types);

	ast_dt_job_dependency(ctx,
			new_expr->value_jobs.resolve_types,
			new_expr->value_jobs.codegen);

	ast_dt_job_dependency(ctx,
			new_expr->value_jobs.codegen,
			parent->finalize_job);

	if (ast_dt_expr_has_ambiguous_refs(
				ctx, parent_id, new_expr->value.node,
				new_expr->trivial_name)) {

		ast_dt_job_dependency(ctx,
				parent->use_resolved,
				new_expr->value_jobs.resolve_names);
	}


	return expr_i;
}

static ast_member_id
ast_dt_register_local_member(struct ast_dt_context *ctx,
		ast_dt_composite_id parent_id, struct atom *name, struct stg_location decl_loc,
		struct ast_node *type_expr)
{
	ast_member_id mbr_id;
	mbr_id = ast_dt_alloc_member(ctx);

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);

	mbr->name = name;
	mbr->decl_loc = decl_loc;
	mbr->type_node = type_expr;
	mbr->parent = parent_id;
	mbr->sub_composite = -1;

	mbr->type_resolved =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_MBR_TYPE_EVAL);

	mbr->const_resolved =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_MBR_CONST_EVAL);

	ast_dt_job_dependency(ctx,
			mbr->type_resolved,
			mbr->const_resolved);

	ast_dt_job_dependency(ctx,
			mbr->const_resolved,
			parent->finalize_job);


	mbr->type_names_resolved = -1;

	if (type_expr) {
		mbr->type_names_resolved =
			ast_dt_job_type(ctx, mbr_id,
					AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES);

		ast_dt_job_dependency(ctx,
				parent->closures_evaled,
				mbr->type_resolved);

		ast_dt_job_dependency(ctx,
				mbr->type_names_resolved,
				mbr->type_resolved);

		if (ast_dt_expr_has_ambiguous_refs(
					ctx, parent_id,
					mbr->type_node, NULL)) {
			ast_dt_job_dependency(ctx,
					parent->use_resolved,
					mbr->type_names_resolved);
		}

		ast_dt_job_dependency(ctx,
				parent->target_names_resolved,
				mbr->type_resolved);
	} else {
		ast_dt_job_dependency(ctx,
				parent->target_names_resolved,
				mbr->const_resolved);
	}

	return mbr_id;
}

static ast_member_id
ast_dt_register_local_sub_composite_member(
		struct ast_dt_context *ctx, ast_dt_composite_id parent_id, struct atom *name,
		struct stg_location decl_loc, ast_dt_composite_id sub_composite_id)
{
	ast_member_id mbr_id;
	mbr_id = ast_dt_alloc_member(ctx);

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	mbr->name = name;
	mbr->decl_loc = decl_loc;
	mbr->type_node = NULL;
	mbr->parent = parent_id;
	mbr->sub_composite = sub_composite_id;

	mbr->type_names_resolved = -1;
	mbr->type_resolved = -1;
	mbr->const_resolved = -1;

	struct ast_dt_composite *sub_composite;
	sub_composite = get_composite(ctx, sub_composite_id);

	assert(sub_composite->self < 0);
	sub_composite->self = mbr_id;

	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);

	mbr->type_resolved = sub_composite->finalize_job;

	mbr->const_resolved =
		ast_dt_job_composite(ctx, sub_composite_id,
				AST_DT_JOB_COMPOSITE_CONST_EVAL);

	ast_dt_job_dependency(ctx,
			parent->target_names_resolved,
			mbr->type_resolved);

	ast_dt_job_dependency(ctx,
			mbr->type_resolved,
			mbr->const_resolved);

	ast_dt_job_dependency(ctx,
			mbr->const_resolved,
			parent->finalize_job);

	// TODO: Does sub-composits receive a trivial name?
	if (ast_dt_expr_has_ambiguous_refs(ctx, parent_id,
				sub_composite->root_node, NULL)) {
		ast_dt_job_dependency(ctx,
				parent->use_resolved,
				sub_composite->resolve_names);
	}
	return mbr_id;
}

static ast_dt_bind_id
ast_dt_register_bind(struct ast_dt_context *ctx,
		ast_dt_composite_id parent_id,
		struct ast_node *target_node, ast_dt_expr_id expr_id)
{
	ast_dt_bind_id bind_id;
	bind_id = ast_dt_alloc_bind(ctx);

	struct ast_dt_bind *bind;
	bind = get_bind(ctx, bind_id);

	struct ast_dt_expr *expr;
	expr = get_expr(ctx, expr_id);

	assert(expr->parent == parent_id);

	bind->target_node = target_node;
	bind->expr = expr_id;

	bind->target_jobs.resolve_names = -1;

	if (bind->target_node) {
		bind->target_jobs.resolve_names =
			ast_dt_job_bind(ctx, bind_id,
					AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES);

		ast_dt_job_dependency(ctx,
				bind->target_jobs.resolve_names,
				expr->value_jobs.resolve_types);
	}

	struct ast_dt_composite *parent;
	parent = get_composite(ctx, expr->parent);

	ast_dt_job_dependency(ctx,
			bind->target_jobs.resolve_names,
			parent->target_names_resolved);

	// if (ast_dt_expr_has_ambiguous_refs(ctx, bind->target_node, NULL)) {
	// 	ast_dt_job_dependency(ctx,
	// 			ctx->use_resolved,
	// 			bind->target_jobs.resolve_names);
	// }

	return bind_id;
}

static ast_dt_bind_id
ast_dt_register_explicit_bind(struct ast_dt_context *ctx,
		ast_dt_composite_id parent_id,
		ast_member_id mbr_id, ast_dt_expr_id expr_id,
		int unpack_id, bool typegiving)
{
	ast_dt_bind_id bind_id;
	bind_id = ast_dt_register_bind(
			ctx, parent_id, NULL, expr_id);

	struct ast_dt_bind *bind;
	bind = get_bind(ctx, bind_id);

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);
	assert(parent_id == mbr->parent);

	struct ast_dt_expr *expr;
	expr = get_expr(ctx, expr_id);

	if (typegiving) {
		ast_dt_job_dependency(ctx,
				expr->value_jobs.resolve_types,
				mbr->type_resolved);
	}

	ast_dt_job_dependency(ctx,
			expr->value_jobs.codegen,
			mbr->const_resolved);

	ast_dt_bind_to_member_explicit(
			ctx, mbr_id, bind_id,
			unpack_id, typegiving);

	return bind_id;
}

static ast_dt_use_id
ast_dt_register_use(struct ast_dt_context *ctx,
		ast_dt_composite_id parent_id,
		ast_dt_expr_id expr_id, struct atom *as_name)
{
	ast_dt_use_id use_id;
	use_id = ast_dt_alloc_use(ctx, parent_id);

	struct ast_dt_use *use;
	use = get_use(ctx, use_id);

	struct ast_dt_expr *expr;
	expr = get_expr(ctx, expr_id);

	use->parent = parent_id;
	use->expr = expr_id;
	use->as_name = as_name;

	assert(expr->parent == use->parent);

	use->const_resolved = ast_dt_job_use(ctx, use_id,
			AST_DT_JOB_USE_CONST_EVAL);

	ast_dt_job_dependency(ctx,
			expr->value_jobs.codegen,
			use->const_resolved);

	// TODO: Support use of non-constant targets.
	// ast_dt_job_dependency(ctx,
	// 		expr->value_jobs.resolve_types,
	// 		ctx->use_resolved);

	struct ast_dt_composite *parent;
	parent = get_composite(ctx, use->parent);

	ast_dt_job_dependency(ctx,
			use->const_resolved,
			parent->use_resolved);

	return use_id;
}

static void
ast_dt_register_init_expr(struct ast_dt_context *ctx,
		ast_dt_composite_id parent_id,
		ast_init_expr_id id, ast_dt_expr_id expr_id)
{
	int index = ast_dt_alloc_init_expr(ctx);
	struct ast_dt_init_expr *init_expr;
	init_expr = &ctx->init_exprs[index];

	init_expr->parent = parent_id;
	init_expr->id = id;
	init_expr->expr = expr_id;
}

static int
ast_dt_try_unpack_member_const(struct ast_dt_context *ctx,
		struct arena *mem, ast_member_id mbr_id, int unpack_id, struct object *out)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	if ((mbr->flags & AST_DT_MEMBER_IS_CONST) == 0) {
		memset(out, 0, sizeof(struct object));
		return 1;
	}

	int err;
	err = object_cons_descendant_type(
			ctx->ast_ctx->vm, mbr->const_value.type,
			unpack_id, &out->type);
	if (err) {
		memset(out, 0, sizeof(struct object));
		return -1;
	}

	struct type *out_type;
	out_type = vm_get_type(ctx->ast_ctx->vm, out->type);

	out->data = arena_alloc(mem, out_type->size);

	err = object_unpack(
			ctx->ast_ctx->vm, mbr->const_value,
			unpack_id, out);
	if (err) {
		memset(out, 0, sizeof(struct object));
		return -1;
	}

	return 0;
}

static int
ast_dt_try_eval_expr_const(struct ast_dt_context *ctx, ast_dt_expr_id expr_id,
		struct object *out)
{
	struct ast_dt_expr *expr;
	expr = get_expr(ctx, expr_id);

	if (expr->constant) {
		*out = expr->const_value;
		return 0;
	}

	struct object dep_member_obj[expr->num_deps];
	bool is_const = true;

	for (size_t i = 0; i < expr->num_deps; i++) {
		struct ast_gen_dt_param *dep;
		dep = &expr->deps[i];

		switch (dep->ref.kind) {
			case AST_GEN_DT_PARAM_MEMBER:
				{
					assert(dep->ref.member.id >= 0);

					int err;
					// TODO: Proper memory context
					err = ast_dt_try_unpack_member_const(
							ctx, ctx->ast_ctx->mem, dep->ref.member.id,
							dep->ref.member.unpack_id,
							&dep_member_obj[i]);

					if (err > 0) {
						is_const = false;
					} else if (err < 0) {
						return -1;
					}
				}
				break;

			default:
				is_const = false;
				break;
		}
	}

	if (!is_const) {
		return 1;
	}

	if (expr->value.func == FUNC_UNSET) {
		return -2;
	}

	assert(expr->type != TYPE_UNSET);

	struct type *ret_type;
	ret_type = vm_get_type(ctx->ast_ctx->vm, expr->type);

	func_id fid;
	fid = expr->value.func;

	uint8_t buffer[ret_type->size];
	struct object obj = {0};
	obj.type = expr->value.node->type;
	obj.data = buffer;

	struct stg_exec exec_ctx = {0};
	exec_ctx.heap = &ctx->ast_ctx->vm->transient;
	arena_mark cp = arena_checkpoint(exec_ctx.heap);

	int err;
	err = vm_call_func(
			ctx->ast_ctx->vm, &exec_ctx, fid, dep_member_obj,
			expr->num_deps, &obj);
	if (err) {
		printf("Failed to evaluate constant member.\n");
		return -3;
	}

	expr->const_value =
		register_object(ctx->ast_ctx->vm,
				&ctx->mod->store, obj);

	arena_reset(exec_ctx.heap, cp);

	expr->constant = true;
	*out = expr->const_value;

	return 0;
}

int
ast_dt_l_expr_top_member(struct ast_dt_context *ctx, struct ast_node *node,
		ast_member_id *out_member)
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
			*out_member = node->lookup.ref.member.id;
			return 0;

		case AST_NODE_ACCESS:
			return ast_dt_l_expr_top_member(
					ctx, node->access.target,
					out_member);

		default:
			stg_error(ctx->ast_ctx->err, node->loc,
					"Bind targets may only contain names and accesses.");
			return -1;
	}
}

struct ast_dt_l_expr_target {
	ast_member_id member;
	int unpack_id;

	int err;
};

struct ast_dt_l_expr_target
ast_dt_l_expr_real_member(struct ast_dt_context *ctx, type_id mbr_type, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_LOOKUP:
			{
				if (node->lookup.ref.kind != AST_NAME_REF_MEMBER) {
					// TODO: We can delay this message until after typecheck to
					// have the error indicate if the member was a closure or
					// not found at all.
					stg_error(ctx->ast_ctx->err, node->loc,
							"This member does not exist.");
					break;
				}

				struct ast_dt_l_expr_target out = {0};
				out.member = node->lookup.ref.member.id;
				out.unpack_id = node->lookup.ref.member.unpack_id;

				return out;
			}

		case AST_NODE_ACCESS:
			{
				struct ast_dt_l_expr_target lhs;
				lhs = ast_dt_l_expr_real_member(
						ctx, mbr_type, node->access.target);
				if (lhs.err) {
					return lhs;
				}

				type_id tid;
				int err;
				err = object_cons_descendant_type(
						ctx->ast_ctx->vm, mbr_type,
						lhs.unpack_id, &tid);
				if (err) {
					break;
				}

				struct type *type;
				type = vm_get_type(ctx->ast_ctx->vm, tid);

				if (!type->obj_inst) {
					struct string type_str;
					type_str = type_repr_to_alloced_string(
							ctx->ast_ctx->vm, vm_get_type(ctx->ast_ctx->vm, tid));
					stg_error(ctx->ast_ctx->err, node->loc,
							"This object of type '%.*s' does not have any members.",
							LIT(type_str));
					free(type_str.text);
					break;
				}

				struct ast_dt_l_expr_target out = {0};
				out.member = lhs.member;
				out.unpack_id = object_cons_find_param_unpack_id(
						ctx->ast_ctx->vm, type->obj_inst->cons,
						node->access.name);

				return out;
			}

		default:
			stg_error(ctx->ast_ctx->err, node->loc,
					"Bind targets may only contain names and accesses.");
			break;
	}
	return (struct ast_dt_l_expr_target){.err = -1};
}

static void
ast_dt_composite_populate(struct ast_dt_context *ctx,
		ast_dt_composite_id parent_id, struct ast_node *node)
{
	assert(node->kind == AST_NODE_COMPOSITE);

	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);

	ast_member_id *members;
	members = calloc(node->composite.num_members, sizeof(ast_member_id));
	assert(!parent->local_member_ids);
	parent->local_member_ids = members;

	ast_member_id type_giving_for[node->composite.num_binds];
	for (size_t i = 0; i < node->composite.num_binds; i++) {
		type_giving_for[i] = -1;
	}

	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_datatype_member *mbr;
		mbr = &node->composite.members[i];

		if (mbr->type && mbr->type->kind == AST_NODE_COMPOSITE &&
				mbr->type->composite.kind == AST_COMPOSITE_NAMESPACE) {
			ast_dt_composite_id comp_id;
			comp_id = ast_dt_register_composite(
					ctx, mbr->type, false, NULL, 0);

			members[i] = ast_dt_register_local_sub_composite_member(
					ctx, parent_id, mbr->name, mbr->loc, comp_id);
		} else {
			// TODO: Better location.
			members[i] = ast_dt_register_local_member(
					ctx, parent_id, mbr->name, mbr->loc, mbr->type);

			if (mbr->type_giving_bind >= 0) {
				type_giving_for[mbr->type_giving_bind] = members[i];
			} else {
				assert(mbr->type);
			}
		}
	}

	for (size_t i = 0; i < node->composite.num_binds; i++) {
		struct ast_datatype_bind *bind;
		bind = &node->composite.binds[i];
		if (bind->erroneous) {
			// TODO: Maybe we should register the bind, but tag it as erroneous,
			// to allow typechecking and reporting of potential errors.
			continue;
		}

		struct atom *trivial_name = NULL;

		if (bind->target->kind == AST_NODE_LOOKUP) {
			trivial_name = bind->target->lookup.name;
		}

		ast_dt_expr_id expr_id;
		expr_id = ast_dt_register_expr(ctx,
					parent_id, bind->value, trivial_name);

		if (type_giving_for[i] >= 0) {
			ast_dt_register_explicit_bind(
					ctx, parent_id, type_giving_for[i], expr_id, 0, true);
		} else {
			ast_dt_register_bind(
					ctx, parent_id, bind->target, expr_id);
		}
	}

	// Make sure all the members we just created have some job that will
	// resolve their types.
	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_dt_member *mbr;
		mbr = get_member(ctx, members[i]);
		assert(mbr->type_resolved >= 0);
	}

	for (size_t i = 0; i < node->composite.num_uses; i++) {
		ast_dt_expr_id expr_id;
		expr_id = ast_dt_register_expr(ctx, parent_id,
				node->composite.uses[i].target, NULL);

		ast_dt_register_use(ctx, parent_id, expr_id,
				node->composite.uses[i].as_name);
	}

	for (size_t i = 0; i < node->composite.num_init_exprs; i++) {
		ast_dt_expr_id expr_id;
		expr_id = ast_dt_register_expr(ctx, parent_id,
				node->composite.init_exprs[i], NULL);
		ast_dt_register_init_expr(ctx, parent_id, i, expr_id);
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
		switch (deps[i].ref.kind) {
			case AST_NAME_REF_MEMBER:
				ast_dt_add_dependency_on_member(
						ctx, target_job, deps[i].req, deps[i].ref.member.id);
				break;
				
			case AST_NAME_REF_INIT_EXPR:
				{
					struct ast_dt_init_expr *init_expr;
					init_expr = get_init_expr(
							ctx, deps[i].ref.init_expr);

					struct ast_dt_expr *expr;
					expr = get_expr(ctx, init_expr->expr);

					ast_dt_job_dependency(ctx,
							expr->value_jobs.resolve_types,
							target_job);
				}
				break;

			default:
				break;
		}
	}

	free(deps);
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

	for (size_t i = 0; i < mbr->num_binds; i++) {
		struct ast_dt_member_bind *mbr_bind;
		mbr_bind = &mbr->binds[i];

		if (mbr_bind->l_expr && mbr_bind->unpack_id < 0) {
			struct ast_dt_l_expr_target target;
			target = ast_dt_l_expr_real_member(
					ctx, mbr->type, mbr_bind->l_expr);
			if (target.err) {
				return -1;
			}
			assert(target.member == mbr_id);

			mbr_bind->unpack_id = target.unpack_id;
		}

		assert(mbr_bind->unpack_id >= 0);
	}

	return 0;
}

static func_id
ast_dt_expr_codegen(struct ast_dt_context *ctx, ast_dt_composite_id parent_id,
		struct ast_node *node, enum ast_name_dep_requirement dep_req,
		struct ast_gen_dt_param **out_deps, size_t *out_num_deps)
{
	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);

	struct ast_name_dep *names = NULL;
	size_t num_names = 0;

	ast_node_find_named_dependencies(
			node, dep_req, &names, &num_names);

	size_t num_dep_members = 0;
	size_t num_dep_init_exprs = 0;
	for (size_t i = 0; i < num_names; i++) {
		switch (names[i].ref.kind) { 
			case AST_NAME_REF_MEMBER:
				num_dep_members += 1;
				break;

			case AST_NAME_REF_INIT_EXPR:
				num_dep_init_exprs += 1;
				break;

			default:
				break;
		}
	}

	size_t num_dt_params = num_dep_members + num_dep_init_exprs;
	struct ast_gen_dt_param *dt_params;
	dt_params = calloc(num_dt_params, sizeof(struct ast_gen_dt_param));

	size_t dt_param_i = 0;
	for (size_t name_i = 0; name_i < num_names; name_i++) {
		switch (names[name_i].ref.kind) {
			case AST_NAME_REF_MEMBER:
				{
					ast_member_id member_id;
					member_id = names[name_i].ref.member.id;

					struct ast_dt_member *mbr;
					mbr = get_member(ctx, member_id);
					assert(mbr->type != TYPE_UNSET);

					dt_params[dt_param_i].ref.kind = AST_GEN_DT_PARAM_MEMBER;
					dt_params[dt_param_i].ref.member.id = member_id;
					dt_params[dt_param_i].ref.member.unpack_id =
						names[name_i].ref.member.unpack_id;

					int err;

					err = object_cons_descendant_type(
							ctx->ast_ctx->vm, mbr->type,
							names[name_i].ref.member.unpack_id,
							&dt_params[dt_param_i].type);

					// TODO: Proper memory context.
					err = ast_dt_try_unpack_member_const(
							ctx, ctx->ast_ctx->mem,
							names[name_i].ref.member.id,
							names[name_i].ref.member.unpack_id,
							&dt_params[dt_param_i].const_val);
					if (err < 0) {
						return FUNC_UNSET;
					} else if (err == 0) {
						dt_params[dt_param_i].is_const = true;
					} else {
						dt_params[dt_param_i].const_val.type = TYPE_UNSET;
						dt_params[dt_param_i].const_val.data = NULL;
					}

					dt_param_i += 1;
				}
				break;

			case AST_NAME_REF_INIT_EXPR:
				{
					ast_init_expr_id init_expr_id;
					init_expr_id = names[name_i].ref.init_expr;

					struct ast_dt_init_expr *init_expr;
					init_expr = get_init_expr(ctx, init_expr_id);

					struct ast_dt_expr *expr;
					expr = get_expr(ctx, init_expr->expr);

					type_id type;
					type = stg_init_get_return_type(
							ctx->ast_ctx->vm, expr->type);

					dt_params[dt_param_i].ref.kind = AST_GEN_DT_PARAM_INIT_EXPR;
					dt_params[dt_param_i].ref.init_expr = init_expr_id;
					dt_params[dt_param_i].type = type;

					dt_param_i += 1;
				}
				break;

			default:
				break;
		}
	}

	num_names = 0;
	free(names);
	names = NULL;

	struct object const_use_values[parent->num_uses];

	for (ast_use_id use_i = 0; use_i < parent->num_uses; use_i++) {
		struct ast_dt_use *use;
		use = get_use(ctx, dt_use_id(parent_id, use_i));

		if (use->is_const) {
			const_use_values[use_i] = use->const_value;
		} else {
			const_use_values[use_i] = OBJ_UNSET;
		}
	}

	struct bc_env *bc_env;
	bc_env = ast_composite_bind_gen_bytecode(
				ctx->ast_ctx, ctx->mod,
				dt_params, num_dt_params,
				const_use_values, parent->num_uses,
				parent->closures, parent->num_closures, node);
	if (!bc_env) {
		return FUNC_UNSET;
	}

	type_id dt_param_types[num_dt_params];
	for (size_t i = 0; i < num_dt_params; i++) {
		dt_param_types[i] = dt_params[i].type;
	}

	struct func func = {0};
	func.type = stg_register_func_type(ctx->mod,
			node->type, dt_param_types, num_dt_params);

	func.kind = FUNC_BYTECODE;
	func.bytecode = bc_env;

	func_id fid;
	fid = stg_register_func(ctx->mod, func);

	*out_num_deps = num_dt_params;
	*out_deps = dt_params;

	return fid;
}

static int
ast_dt_body_deps(struct ast_dt_context *ctx, ast_dt_composite_id parent_id,
		struct ast_name_dep *deps, size_t num_deps,
		struct ast_typecheck_dep *body_deps)
{
	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);

	memset(body_deps, 0, sizeof(struct ast_typecheck_dep) * num_deps);

	for (size_t i = 0; i < num_deps; i++) {
		struct ast_typecheck_dep *dep;
		dep = &body_deps[i];
		switch (deps[i].ref.kind) {

			case AST_NAME_REF_MEMBER:
				{
					struct ast_dt_member *dep_mbr;
					dep_mbr = get_member(ctx, deps[i].ref.member.id);

					dep->ref = deps[i].ref;
					dep->req = deps[i].req;
					dep->lookup_failed = false;
					dep->determined = false;

					struct object obj = {0};
					int err;
					// TODO: Proper memory context
					err = ast_dt_try_unpack_member_const(
							ctx, ctx->ast_ctx->mem, dep->ref.member.id,
							dep->ref.member.unpack_id,
							&obj);
					if (err < 0) {
						return -1;
					}

					if (err == 0) {
						dep->determined = true;
						dep->req = AST_NAME_DEP_REQUIRE_VALUE;
						dep->val = obj;
					} else if (err > 0) {
						assert(dep_mbr->type != TYPE_UNSET);

						dep->determined = true;
						dep->req = AST_NAME_DEP_REQUIRE_TYPE;
						err = object_cons_descendant_type(
								ctx->ast_ctx->vm, dep_mbr->type,
								dep->ref.member.unpack_id, &dep->type);
						if (err) {
							return -1;
						}

						assert(dep->type != TYPE_UNSET);
					}
				}
				break;

			case AST_NAME_REF_CLOSURE:
				if (parent->closures) {
					assert(deps[i].ref.closure >= 0 &&
							deps[i].ref.closure < parent->num_closures);
					struct ast_typecheck_closure *cls;
					cls = &parent->closures[deps[i].ref.closure];
					dep->ref = deps[i].ref;
					dep->req = cls->req;

					dep->determined = true;
					dep->lookup_failed = cls->lookup_failed;

					switch (cls->req) {
						case AST_NAME_DEP_REQUIRE_VALUE:
							// The closure provided a value. It
							// does not matter if we asked for a
							// type instead.
							dep->val = cls->value;
							break;

						case AST_NAME_DEP_REQUIRE_TYPE:
							// The closure provided only a type.
							// Make sure that is all we asked for.
							assert(deps[i].req == AST_NAME_DEP_REQUIRE_TYPE);
							dep->type = cls->type;
							break;
					}
				} else {
					dep->ref = deps[i].ref;
					dep->req = deps[i].req;
					dep->value = -1;
					dep->lookup_failed = true;
					dep->determined = false;
				}
				break;

			case AST_NAME_REF_USE:
				{
					struct ast_dt_use *use;
					use = get_use(ctx, dt_use_id(parent_id, deps[i].ref.use.id));

					// TODO: Support use of non-constant values.
					assert(use->is_const);

					type_id target_type_id;
					int err;
					err = object_cons_descendant_type(
							ctx->ast_ctx->vm, use->type,
							deps[i].ref.use.param, &target_type_id);
					if (err) {
						printf("Failed to resolve the use target's type.\n");
						return -1;
					}

					struct type *target_type;
					target_type = vm_get_type(ctx->ast_ctx->vm, target_type_id);

					uint8_t buffer[target_type->size];
					memset(buffer, 0, target_type->size);
					struct object obj = {0};
					obj.type = target_type_id;
					obj.data = buffer;

					err = object_unpack(
							ctx->ast_ctx->vm,
							use->const_value,
							deps[i].ref.use.param, &obj);
					if (err) {
						printf("Failed to unpack use target.\n");
						return -1;
					}

					// TODO: Is this necessary?
					obj = register_object(ctx->ast_ctx->vm, &ctx->mod->store, obj);

					dep->ref = deps[i].ref;
					dep->req = AST_NAME_DEP_REQUIRE_VALUE;
					dep->val = obj;
					dep->value = -1;
					dep->lookup_failed = false;
					dep->determined = true;
				}
				break;

			case AST_NAME_REF_INIT_EXPR:
				{
					struct ast_dt_init_expr *init_expr;
					init_expr = get_init_expr(ctx, deps[i].ref.init_expr);

					struct ast_dt_expr *expr;
					expr = get_expr(ctx, init_expr->expr);

					assert(deps[i].req == AST_NAME_DEP_REQUIRE_TYPE);

					dep->ref = deps[i].ref;
					dep->req = deps[i].req;
					dep->lookup_failed = false;
					dep->determined = true;

					type_id type;
					type = stg_init_get_return_type(
							ctx->ast_ctx->vm, expr->type);

					dep->type = type;
				}
				break;

			case AST_NAME_REF_SELF:
				dep->ref = deps[i].ref;
				dep->req = deps[i].req;
				dep->lookup_failed = false;
				dep->determined = false;
				break;

			case AST_NAME_REF_TEMPL:
				panic("TODO: Pass template information to datatype.");
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

	return 0;
}

static int
ast_dt_expr_typecheck(struct ast_dt_context *ctx, ast_dt_composite_id parent_id,
		struct ast_node *node, enum ast_name_dep_requirement dep_req,
		type_id expected_type, struct object *out)
{
	struct ast_name_dep *deps = NULL;
	size_t num_deps = 0;

	struct ast_dt_composite *parent;
	parent = get_composite(ctx, parent_id);

	int err;
	err = ast_node_find_named_dependencies(
			node, dep_req, &deps, &num_deps);
	if (err) {
		printf("Failed to find the named dependencies.\n");
		return -1;
	}

	struct ast_typecheck_dep body_deps[num_deps];


	err = ast_dt_body_deps(ctx, parent_id,
			deps, num_deps, body_deps);
	if (err) {
		return -1;
	}

	err = ast_node_typecheck(
			ctx->ast_ctx, ctx->mod, node,
			body_deps, num_deps,
			expected_type, out);
	if (err) {
		return -1;
	}

	return 0;
}

static bool
ast_dt_try_pack_member(
		struct ast_dt_context *ctx, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	assert(mbr->type != TYPE_UNSET);

	struct type *mbr_type;
	mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

	// 0: The top member is not bound.
	// 1: The top member is bound.
	// >1: The top member is bound multiple times.
	int top_bind_state = 0;
	int top_bind_overr_state = 0;
	int non_top_state = 0;

	ast_dt_bind_id top_bind = -1;

	for (size_t i = 0; i < mbr->num_binds; i++) {
		assert((mbr->binds[i].flags & AST_DT_MBR_BIND_UNPACK_ID_RESOLVED) != 0);

		if (mbr->binds[i].unpack_id == 0) {
			struct ast_dt_bind *bind;
			bind = get_bind(ctx, mbr->binds[i].bind);

			if (bind->overridable) {
				top_bind_overr_state += 1;
			} else {
				top_bind_state += 1;
				top_bind = mbr->binds[i].bind;
			}
		} else {
			non_top_state += 1;
		}
	}

	if (top_bind_overr_state > 1) {
		stg_error(ctx->ast_ctx->err, mbr->decl_loc,
				"This member has multiple overridable binds.");

		for (size_t i = 0; i < mbr->num_binds; i++) {
			if (mbr->binds[i].unpack_id == 0) {
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, mbr->binds[i].bind);

				if (bind->overridable) {
					stg_appendage(
							ctx->ast_ctx->err, bind->loc,
							"Here.");
				}
			}
		}
	}

	if (top_bind_state > 1) {
		stg_error(ctx->ast_ctx->err, mbr->decl_loc,
				"This member is bound multiple times.");

		for (size_t i = 0; i < mbr->num_binds; i++) {
			if (mbr->binds[i].unpack_id == 0) {
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, mbr->binds[i].bind);

				if (!bind->overridable) {
					stg_appendage(
							ctx->ast_ctx->err, bind->loc,
							"Here.");
				}
			}
		}
	}

	if (top_bind_state == 1 && non_top_state > 0) {
		stg_error(ctx->ast_ctx->err, mbr->decl_loc,
				"This member has conflicting binds.");

		for (size_t i = 0; i < mbr->num_binds; i++) {
			struct ast_dt_bind *bind;
			bind = get_bind(ctx, mbr->binds[i].bind);

			if (!bind->overridable) {
				stg_appendage(
						ctx->ast_ctx->err, bind->loc,
						"Here.");
			}
		}
	}

	struct object result = {0};

	if (top_bind_state == 0) {
		struct object_inst *inst;
		inst = mbr_type->obj_inst;
		assert(inst);

		struct arena *mem;
		mem = &ctx->ast_ctx->vm->transient;
		arena_mark cp = arena_checkpoint(mem);

		size_t num_binds = mbr->num_binds;
		struct object_inst_extra_expr *extra_exprs;
		extra_exprs = arena_allocn(mem, num_binds,
				sizeof(struct object_inst_extra_expr));

		struct object_inst_bind *extra_binds;
		extra_binds = arena_allocn(mem, num_binds,
				sizeof(struct object_inst_bind));

		size_t num_expr_deps = 0;
		for (size_t i = 0; i < mbr->num_binds; i++) {
			struct ast_dt_member_bind *mbr_bind;
			mbr_bind = &mbr->binds[i];

			struct ast_dt_bind *bind;
			bind = get_bind(ctx, mbr_bind->bind);

			struct ast_dt_expr *expr;
			expr = get_expr(ctx, bind->expr);

			num_expr_deps += expr->num_deps;
		}

		struct object_inst_dep *expr_deps;
		expr_deps = arena_allocn(mem, num_expr_deps,
				sizeof(struct object_inst_dep));

		size_t expr_dep_head = 0;
		for (size_t i = 0; i < mbr->num_binds; i++) {
			struct ast_dt_member_bind *mbr_bind;
			mbr_bind = &mbr->binds[i];

			struct ast_dt_bind *bind;
			bind = get_bind(ctx, mbr_bind->bind);

			struct ast_dt_expr *expr;
			expr = get_expr(ctx, bind->expr);

			extra_exprs[i].type = expr->type;
			extra_exprs[i].deps = &expr_deps[expr_dep_head];
			extra_exprs[i].num_deps = expr->num_deps;

			size_t num_deps = 0;
			for (size_t dep_i = 0; dep_i < expr->num_deps; dep_i++) {
				struct ast_gen_dt_param *dep;
				dep = &expr->deps[dep_i];

				if (dep->ref.kind == AST_GEN_DT_PARAM_MEMBER) {
					if (dep->ref.member.id == mbr_id) {
						extra_exprs[i].deps[num_deps].kind = OBJECT_INST_DEP_MEMBER;
						extra_exprs[i].deps[num_deps].member =
							dep->ref.member.unpack_id;
						num_deps += 1;
					} else {
						if (!dep->is_const) {
							return false;
						}
					}
				} else if (dep->ref.kind == AST_GEN_DT_PARAM_INIT_EXPR) {
					// Init-expressions must be handled run-time.
					return false;
				}
			}
			expr_dep_head += num_deps;

		}
		assert(expr_dep_head <= num_expr_deps);
		num_expr_deps = expr_dep_head;

		struct object_inst_action *actions = NULL;
		size_t num_actions = 0;

		// TODO: Figure out if the resulting value is constant.
		int err;
		err = object_inst_order(
				ctx->ast_ctx->vm, ctx->ast_ctx->err, inst,
				extra_exprs, num_binds,
				extra_binds, num_binds,
				&actions, &num_actions,
				mbr->decl_loc);

		if (err) {
			free(actions);
			arena_reset(mem, cp);
			return false;
		}

		struct object *exprs;
		exprs = arena_allocn(mem,
				inst->num_exprs + num_binds,
				sizeof(struct object));

		size_t num_members;
		num_members = object_cons_num_descendants(
				ctx->ast_ctx->vm, inst->cons) + 1;

		struct object *mbrs;
		mbrs = arena_allocn(mem,
				num_members,
				sizeof(struct object));

		int res = 0;
		for (size_t act_i = 0; act_i < num_actions; act_i++) {
			struct object_inst_action *act;
			act = &actions[act_i];
			switch (act->op) {
				case OBJ_INST_EXPR:
					if (act->expr.id < inst->num_exprs) {
						struct object_inst_expr *expr;
						expr = &inst->exprs[act->expr.id];
						if (expr->constant) {
							exprs[act->expr.id] = expr->const_value;
						} else {
							struct object params[expr->num_deps];
							for (size_t i = 0; i < expr->num_deps; i++) {
								if (expr->deps[i].kind != OBJECT_INST_DEP_MEMBER) {
									res = 1;
									break;
								}

								struct object *dep_obj;
								dep_obj = &exprs[i];
								assert(dep_obj->type != TYPE_UNSET);
								params[i] = *dep_obj;
							}

							if (res) {
								break;
							}

							struct stg_exec exec_ctx = {0};
							exec_ctx.heap = ctx->ast_ctx->mem;

							struct object *out_obj;
							out_obj = &exprs[act->expr.id];
							out_obj->type = func_inst_return_type(
									ctx->ast_ctx->vm, expr->func);

							struct type *out_type;
							out_type = vm_get_type(ctx->ast_ctx->vm, out_obj->type);
							out_obj->data = arena_alloc(
									ctx->ast_ctx->mem, out_type->size);

							int err;
							err = vm_call_func(
									ctx->ast_ctx->vm, &exec_ctx, expr->func,
									params, expr->num_deps, out_obj);
							if (err) {
								res = -1;
							}
						}
					} else {
						int expr_i = act->expr.id - inst->num_exprs;
						assert(expr_i < num_binds);

						struct ast_dt_member_bind *mbr_bind;
						mbr_bind = &mbr->binds[expr_i];

						struct ast_dt_bind *bind;
						bind = get_bind(ctx, mbr_bind->bind);

						struct ast_dt_expr *expr;
						expr = get_expr(ctx, bind->expr);

						if (expr->constant) {
							assert(expr->const_value.type != TYPE_UNSET);
							exprs[act->expr.id] = expr->const_value;
						} else {
							assert(expr->value.func != FUNC_UNSET);

							struct object params[expr->num_deps];
							for (size_t i = 0; i < expr->num_deps; i++) {
								if (!expr->deps[i].is_const) {
									res = 1;
									break;
								}
								params[i] = expr->deps[i].const_val;
							}
							if (res) {
								break;
							}

							struct stg_exec exec_ctx = {0};
							exec_ctx.heap = ctx->ast_ctx->mem;

							struct object *out_obj;
							out_obj = &exprs[act->expr.id];
							out_obj->type = expr->type;

							struct type *out_type;
							out_type = vm_get_type(ctx->ast_ctx->vm, out_obj->type);
							out_obj->data = arena_alloc(
									ctx->ast_ctx->mem, out_type->size);

							int err;
							err = vm_call_func(
									ctx->ast_ctx->vm, &exec_ctx, expr->value.func,
									params, expr->num_deps, out_obj);
							if (err) {
								res = -1;
							}
						}
					}
					break;

				case OBJ_INST_INIT_EXPR:
					res = 1;
					break;

				case OBJ_INST_BIND:
					{
						int err;
						struct object *out_obj;
						assert(act->bind.member_id < num_members);
						out_obj = &mbrs[act->bind.member_id];
						assert(out_obj->type == TYPE_UNSET);

						struct object in_obj;
						assert(act->bind.expr_id < inst->num_exprs+num_binds);
						in_obj = exprs[act->bind.expr_id];
						assert(in_obj.type != TYPE_UNSET);

						err = object_cons_descendant_type(
								ctx->ast_ctx->vm, in_obj.type,
								act->bind.unpack_id, &out_obj->type);

						struct type *out_type;
						out_type = vm_get_type(ctx->ast_ctx->vm, out_obj->type);

						out_obj->data = arena_alloc(mem, out_type->size);

						err = object_unpack(ctx->ast_ctx->vm,
								in_obj, act->bind.unpack_id,
								out_obj);
					}
					break;

				case OBJ_INST_PACK:
					{
						type_id target_type_id;
						int err;
						err = object_cons_descendant_type(
								ctx->ast_ctx->vm, mbr->type,
								act->pack.member_id, &target_type_id);
						if (err) {
							res = -1;
							break;
						}

						struct type *target_type;
						target_type = vm_get_type(ctx->ast_ctx->vm,
								target_type_id);

						struct object_inst *inst;
						inst = target_type->obj_inst;

						// A member we are asked to pack must have an object
						// inst.
						assert(inst);

						size_t num_descs;
						num_descs = object_cons_num_descendants(
								ctx->ast_ctx->vm, inst->cons);

						void *params[num_descs];
						for (size_t i = 0; i < num_descs; i++) {
							size_t mbr_i = act->pack.member_id + 1 + i;
							assert(mbrs[mbr_i].type != TYPE_UNSET);
							params[i] = mbrs[mbr_i].data;
						}

						struct type *out_type;
						out_type = vm_get_type(ctx->ast_ctx->vm, inst->type);

						struct object *out;
						out = &mbrs[act->pack.member_id];
						out->type = inst->type;
						out->data = arena_alloc(
								ctx->ast_ctx->mem, out_type->size);

						assert(inst->cons->pack);

						inst->cons->pack(
								ctx->ast_ctx->vm,
								inst->cons->data,
								out->data, params, num_descs);
					}
					break;
			}

			if (res) {
				break;
			}
		}

		free(actions);

		if (res < 0) {
			arena_reset(mem, cp);
			return -1;
		} else if (res > 0) {
			arena_reset(mem, cp);
			return 0;
		}

		result = mbrs[0];

		arena_reset(mem, cp);
	} else {
		assert(top_bind_state > 0);
		assert(top_bind >= 0);

		mbr->local_explicitly_bound = true;

		struct ast_dt_bind *bind;
		bind = get_bind(ctx, top_bind);

		struct ast_dt_expr *expr;
		expr = get_expr(ctx, bind->expr);

		if (!expr->constant) {
			return false;
		}

		result = expr->const_value;
	}

	result = register_object(
			ctx->ast_ctx->vm, &ctx->mod->store, result);

	mbr->const_value = result;
	mbr->flags |= AST_DT_MEMBER_IS_CONST;

	return true;
}

static type_id
ast_dt_composite_make_type(struct ast_dt_context *ctx,
		ast_dt_composite_id comp_id);

static inline int
ast_dt_dispatch_job(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);

#if AST_DT_DEBUG_JOBS
	printf("%03zx    "TC(TC_BRIGHT_YELLOW, "===>") " ", ctx->run_i);
	ast_dt_print_job_desc(ctx, job_id);
	printf("\n");
#endif

	switch (job->kind) {
		case AST_DT_JOB_NOP:
			return 0;

		case AST_DT_JOB_COMPOSITE_RESOLVE_NAMES:
			{
				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);

				// We should only try to resolve names of sub-composites.
				assert(comp->self >= 0);

				struct ast_dt_member *self;
				self = get_member(ctx, comp->self);

				struct ast_dt_composite *parent;
				parent = get_composite(ctx, self->parent);

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->mod->native_mod,
						NULL, true, parent->root_node, comp->root_node,
						parent->local_member_ids, NULL);
				if (err) {
					printf("Failed to resolve names.\n");
					break;
				}

				ast_dt_find_named_dependencies(
						ctx, comp->closures_evaled,
						AST_NAME_DEP_REQUIRE_VALUE, comp->root_node);
			}
			return 0;

		case AST_DT_JOB_COMPOSITE_EVAL_CLOSURE:
			{
				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);

				// We should only try to resolve names of sub-composites.
				assert(comp->self >= 0);

				struct ast_dt_member *self;
				self = get_member(ctx, comp->self);

				struct ast_name_dep *deps = NULL;
				size_t num_deps = 0;

				int err;
				err = ast_node_find_named_dependencies(
						comp->root_node, AST_NAME_DEP_REQUIRE_VALUE,
						&deps, &num_deps);
				if (err) {
					printf("Failed to find the named dependencies.\n");
					return -1;
				}

				struct ast_typecheck_dep body_deps[num_deps];

				err = ast_dt_body_deps(ctx, self->parent,
						deps, num_deps, body_deps);
				if (err) {
					return -1;
				}

				struct ast_closure_target *closure;
				closure = &comp->root_node->composite.closure;

				struct ast_typecheck_closure *closure_values;
				closure_values = arena_allocn(ctx->tmp_mem,
						closure->num_members,
						sizeof(struct ast_typecheck_closure));
				memset(closure_values, 0,
						sizeof(struct ast_typecheck_closure) * closure->num_members);

				ast_fill_closure(closure, closure_values,
						body_deps, num_deps);

				comp->closures = closure_values;
				comp->num_closures = closure->num_members;
			}
			return 0;

		case AST_DT_JOB_COMPOSITE_PACK:
			{
				type_id result;
				result = ast_dt_composite_make_type(
						ctx, job->composite);
				if (result == TYPE_UNSET) {
					return -1;
				}

				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);

				if (comp->self >= 0) {
					ast_try_set_local_member_type(
							ctx, comp->self, result);
				}
			}
			return 0;

		case AST_DT_JOB_COMPOSITE_CONST_EVAL:
			{
				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);
				assert(comp->self >= 0);

				ast_dt_try_pack_member(ctx, comp->self);
			}
			return 0;

		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);

				struct ast_dt_composite *parent;
				parent = get_composite(ctx, mbr->parent);

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->mod->native_mod,
						NULL, true, parent->root_node, mbr->type_node,
						parent->local_member_ids, NULL);
				if (err) {
					printf("Failed to resolve names.\n");
					break;
				}

				ast_dt_find_named_dependencies(
						ctx, mbr->type_resolved,
						AST_NAME_DEP_REQUIRE_VALUE, mbr->type_node);
			}
			return 0;

		case AST_DT_JOB_EXPR_RESOLVE_NAMES:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);

				struct ast_dt_composite *parent;
				parent = get_composite(ctx, expr->parent);

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->mod->native_mod,
						NULL, false, parent->root_node,
						expr->value.node,
						parent->local_member_ids,
						expr->trivial_name);
				if (err) {
					printf("Failed to resolve names.\n");
					break;
				}

				ast_dt_find_named_dependencies(
						ctx, expr->value_jobs.resolve_types,
						AST_NAME_DEP_REQUIRE_TYPE, expr->value.node);
			}
			return 0;

		case AST_DT_JOB_EXPR_RESOLVE_TYPES:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);

				int err;
				err = ast_dt_expr_typecheck(
						ctx, expr->parent, expr->value.node,
						AST_NAME_DEP_REQUIRE_TYPE,
						expr->type, NULL);
				if (err) {
					return -1;
				}

				assert(expr->type == TYPE_UNSET);
				expr->type = expr->value.node->type;
			}
			return 0;

		case AST_DT_JOB_MBR_TYPE_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				type_id out_type = TYPE_UNSET;

				if (mbr->type_node) {
					struct object out_type_obj = {0};

					int err;
					err = ast_dt_expr_typecheck(
							ctx, mbr->parent, mbr->type_node,
							AST_NAME_DEP_REQUIRE_VALUE,
							ctx->ast_ctx->vm->default_types.type,
							&out_type_obj);
					if (err) {
						return -1;
					}

					assert_type_equals(ctx->ast_ctx->vm,
							out_type_obj.type,
							ctx->ast_ctx->vm->default_types.type);

					out_type = *(type_id *)out_type_obj.data;
				} else {
					for (size_t i = 0; i < mbr->num_binds; i++) {
						struct ast_dt_member_bind *mbr_bind;
						mbr_bind = &mbr->binds[i];

						if ((mbr_bind[i].flags & AST_DT_MBR_BIND_TYPEGIVING) != 0) {
							assert(mbr_bind->unpack_id == 0);
							struct ast_dt_bind *bind;
							bind = get_bind(ctx, mbr_bind->bind);

							struct ast_dt_expr *expr;
							expr = get_expr(ctx, bind->expr);

							out_type = expr->type;
						}
					}
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

				ast_dt_try_pack_member(ctx, job->member);
			}
			return 0;

		case AST_DT_JOB_EXPR_CODEGEN:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);

				struct ast_gen_dt_param *deps = NULL;
				size_t num_deps = 0;

				func_id fid;
				fid = ast_dt_expr_codegen(
						ctx, expr->parent, expr->value.node,
						AST_NAME_DEP_REQUIRE_TYPE,
						&deps, &num_deps);
				if (fid == FUNC_UNSET) {
					return -1;
				}

				expr->num_deps = num_deps;
				expr->deps = deps;

				expr->value.func = fid;

				int err;
				err = ast_dt_try_eval_expr_const(
						ctx, job->expr, &expr->const_value);
				if (err) {
					return err < 0;
				}
			}
			return 0;

		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			{
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, job->bind);

				struct ast_dt_expr *expr;
				expr = get_expr(ctx, bind->expr);

				struct ast_dt_composite *parent;
				parent = get_composite(ctx, expr->parent);

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, NULL, NULL,
						false, parent->root_node, bind->target_node,
						parent->local_member_ids, NULL);
				if (err) {
					return -1;
				}

				ast_member_id target;
				err = ast_dt_l_expr_top_member(
						ctx, bind->target_node, &target);
				if (err) {
					return -1;
				}

				struct ast_dt_member *mbr;
				mbr = get_member(ctx, target);

				ast_dt_job_dependency(ctx,
						expr->value_jobs.codegen,
						mbr->const_resolved);

				ast_dt_bind_to_member(
						ctx, target, job->bind,
						bind->target_node, false);
			}
			return 0;

		case AST_DT_JOB_USE_CONST_EVAL:
			{
				struct ast_dt_use *use;
				use = get_use(ctx, job->use);

				struct ast_dt_expr *expr;
				expr = get_expr(ctx, use->expr);

				assert(expr->type != TYPE_UNSET);
				use->type = expr->type;

				int err;
				err = ast_dt_try_eval_expr_const(
						ctx, use->expr, &use->const_value);
				if (err) {
					return err < 0;
				}

				use->is_const = true;

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

		case AST_DT_JOB_COMPOSITE_RESOLVE_NAMES:
			{
				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);

				assert(comp->resolve_names == job_id);
				comp->resolve_names = -1;
			}
			break;
		case AST_DT_JOB_COMPOSITE_EVAL_CLOSURE:
			{
				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);

				assert(comp->closures_evaled == job_id);
				comp->closures_evaled = -1;
			}
			break;
		case AST_DT_JOB_COMPOSITE_PACK:
			{
				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);

				assert(comp->finalize_job == job_id);
				comp->finalize_job = -1;

				if (comp->self >= 0) {
					struct ast_dt_member *self;
					self = get_member(ctx, comp->self);
					assert(self->type_resolved == job_id);
					self->type_resolved = job_id;
				}
			}
			break;
		case AST_DT_JOB_COMPOSITE_CONST_EVAL:
			{
				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);

				assert(comp->self >= 0);

				struct ast_dt_member *self;
				self = get_member(ctx, comp->self);
				assert(self->const_resolved == job_id);
				self->const_resolved = job_id;
			}
			break;

		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				assert(mbr->type_names_resolved == job_id);
				mbr->type_names_resolved = -1;
			}
			break;
		case AST_DT_JOB_MBR_TYPE_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
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

		case AST_DT_JOB_EXPR_RESOLVE_NAMES:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);
				assert(expr->value_jobs.resolve_names == job_id);
				expr->value_jobs.resolve_names = -1;
			}
			break;
		case AST_DT_JOB_EXPR_RESOLVE_TYPES:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);
				assert(expr->value_jobs.resolve_types == job_id);
				expr->value_jobs.resolve_types = -1;
			}
			break;
		case AST_DT_JOB_EXPR_CODEGEN:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);
				assert(expr->value_jobs.codegen == job_id);
				expr->value_jobs.codegen = -1;
			}
			break;

		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			{
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, job->expr);
				assert(bind->target_jobs.resolve_names == job_id);
				bind->target_jobs.resolve_names = -1;
			}
			break;

		case AST_DT_JOB_USE_CONST_EVAL:
			{
				struct ast_dt_use *use;
				use = get_use(ctx, job->use);
				assert(use->const_resolved == job_id);
				use->const_resolved = -1;
			}
			break;
	}
}

enum ast_dtc_vertex_color {
	AST_DTC_WHITE = 0,
	AST_DTC_GRAY,
	AST_DTC_BLACK,
};

struct ast_dtc_vertex {
	ast_dt_job_id job_id;

	enum ast_dtc_vertex_color color;
	int discover, finish;
	struct ast_dtc_vertex *pred;

	struct ast_dtc_edge *outgoing_edges;
	size_t num_outgoing_edges;
};

struct ast_dtc_edge {
	struct ast_dtc_vertex *from, *to;
};

struct ast_dtc_graph {
	struct ast_dtc_vertex *vertices;
	size_t num_vertices;

	struct ast_dtc_edge *edges;
	size_t num_edges;
};

struct ast_dtc_components {
	struct ast_dtc_vertex **comps;
	size_t num_comps;
};

static void
ast_dtc_dfs_visit(
		struct ast_dtc_graph *graph,
		struct ast_dtc_vertex *vert,
		int *time, struct ast_dtc_components *out_comps)
{
	*time += 1;
	vert->discover = *time;
	vert->color = AST_DTC_GRAY;

	bool is_terminal = true;

	for (size_t i = 0; i < graph->num_edges; i++) {
		struct ast_dtc_edge *edge;
		edge = &graph->edges[i];
		if (edge->from == vert &&
				edge->to->color == AST_DTC_WHITE) {
			edge->to->pred = vert;
			ast_dtc_dfs_visit(graph, edge->to, time, out_comps);
			is_terminal = false;
		}
	}

	if (is_terminal && out_comps) {
		dlist_append(
				out_comps->comps,
				out_comps->num_comps,
				&vert);
	}

	// for (size_t i = 0; i < vert->num_outgoing_edges; i++) {
	// 	struct ast_dtc_edge *edge;
	// 	edge = &vert->outgoing_edges[i];
	// 	if (edge->to->color == AST_DTC_WHITE) {
	// 		ast_dtc_dfs_visit(graph, edge->to, time);
	// 	}
	// }

	vert->color = AST_DTC_BLACK;
	*time += 1;
	vert->finish = *time;
}

struct ast_dtc_vert_sort {
	int finish;
	struct ast_dtc_vertex *vert;
};

static int
ast_dtc_dfs_sorted_comp(const void *lhs_ptr, const void *rhs_ptr)
{
	const struct ast_dtc_vert_sort *lhs, *rhs;
	lhs = lhs_ptr;
	rhs = rhs_ptr;

	return rhs->finish - lhs->finish;
}

static void
ast_dtc_dfs(struct ast_dtc_graph *graph,
		struct ast_dtc_components *out_comps,
		bool sort)
{
	for (size_t i = 0; i < graph->num_vertices; i++) {
		graph->vertices[i].color = AST_DTC_WHITE;
		graph->vertices[i].pred = NULL;
	}

	int time = 0;

	struct ast_dtc_vert_sort order[graph->num_vertices];
	for (size_t i = 0; i < graph->num_vertices; i++) {
		order[i].finish = graph->vertices[i].finish;
		order[i].vert = &graph->vertices[i];
	}

	if (sort) {
		qsort(order, graph->num_vertices,
				sizeof(struct ast_dtc_vert_sort),
				ast_dtc_dfs_sorted_comp);
	}

	for (size_t i = 0; i < graph->num_vertices; i++) {
		struct ast_dtc_vertex *vert;
		vert = order[i].vert;
		if (vert->color == AST_DTC_WHITE) {
			ast_dtc_dfs_visit(graph, vert, &time, out_comps);
		}
	}
}

static struct stg_location
ast_dt_job_location(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job = get_job(ctx, job_id);

	switch (job->kind) {
		case AST_DT_JOB_FREE:
			return STG_NO_LOC;
		case AST_DT_JOB_NOP:
			return STG_NO_LOC;

		case AST_DT_JOB_COMPOSITE_RESOLVE_NAMES:
		case AST_DT_JOB_COMPOSITE_EVAL_CLOSURE:
		case AST_DT_JOB_COMPOSITE_PACK:
		case AST_DT_JOB_COMPOSITE_CONST_EVAL:
			{
				struct ast_dt_composite *comp;
				comp = get_composite(ctx, job->composite);
				return comp->root_node->loc;
			}

		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
		case AST_DT_JOB_MBR_TYPE_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				return mbr->type_node->loc;
			}

		case AST_DT_JOB_MBR_CONST_EVAL:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				// TODO: Should this job point to the bind?
				return mbr->decl_loc;
			}

		case AST_DT_JOB_EXPR_RESOLVE_NAMES:
		case AST_DT_JOB_EXPR_RESOLVE_TYPES:
		case AST_DT_JOB_EXPR_CODEGEN:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);
				return expr->loc;
			}

		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			{
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, job->bind);
				return bind->loc;
			}

		case AST_DT_JOB_USE_CONST_EVAL:
			{
				struct ast_dt_use *use;
				use = get_use(ctx, job->use);

				struct ast_dt_expr *expr;
				expr = get_expr(ctx, use->expr);

				return expr->loc;
			}
	}
}

void
ast_dt_report_cyclic_dependency_chain(struct ast_dt_context *ctx,
		struct ast_dtc_vertex *component)
{
	struct ast_dtc_vertex *it = component;

	stg_error(ctx->ast_ctx->err,
			ast_dt_job_location(ctx, it->job_id),
			"Found member dependency cycle. %03x", it->job_id);
	it = it->pred;

	while (it) {
		stg_appendage(ctx->ast_ctx->err,
				ast_dt_job_location(ctx, it->job_id),
				"Through. %03x", it->job_id);
		it = it->pred;
	}
}

static void
ast_dt_report_cyclic_dependencies(struct ast_dt_context *ctx)
{
	struct ast_dtc_graph graph = {0};

	size_t num_unfinished_jobs = 0;
	size_t num_unvisited_edges = 0;

	for (ast_dt_job_id job_i = 0; job_i < ctx->jobs.length; job_i++) {
		struct ast_dt_job *job;
		job = get_job(ctx, job_i);

		if (job->kind == AST_DT_JOB_FREE) {
			continue;
		}

		num_unfinished_jobs += 1;
		num_unvisited_edges += job->num_outgoing_deps;
	}

	struct ast_dtc_vertex _verts[num_unfinished_jobs];
	memset(_verts, 0, sizeof(struct ast_dtc_vertex) * num_unfinished_jobs);
	struct ast_dtc_edge _edges[num_unvisited_edges];
	memset(_edges, 0, sizeof(struct ast_dtc_edge) * num_unvisited_edges);

	graph.vertices = _verts;
	graph.num_vertices = num_unfinished_jobs;
	graph.edges = _edges;
	graph.num_edges = num_unvisited_edges;

	size_t vert_i = 0;

	for (ast_dt_job_id job_i = 0; job_i < ctx->jobs.length; job_i++) {
		struct ast_dt_job *job;
		job = get_job(ctx, job_i);

		if (job->kind == AST_DT_JOB_FREE) {
			continue;
		}

		assert(vert_i < graph.num_vertices);
		struct ast_dtc_vertex *vert;
		vert = &graph.vertices[vert_i];
		vert->job_id = job_i;
		job->aux_id = vert_i;

		vert_i += 1;
	}

	size_t edge_i = 0;

	for (ast_dt_job_id job_i = 0; job_i < ctx->jobs.length; job_i++) {
		struct ast_dt_job *job;
		job = get_job(ctx, job_i);

		if (job->kind == AST_DT_JOB_FREE) {
			continue;
		}

		struct ast_dtc_vertex *vert;
		vert = &graph.vertices[job->aux_id];

		assert(edge_i < graph.num_edges);
		vert->outgoing_edges = &graph.edges[edge_i];

		for (size_t i = 0; i < job->num_outgoing_deps; i++) {
			struct ast_dt_job *dep;
			dep = get_job(ctx, job->outgoing_deps[i].to);

			assert(job->kind != AST_DT_JOB_FREE);
			vert->outgoing_edges[i].from = vert;
			vert->outgoing_edges[i].to = &graph.vertices[dep->aux_id];
		}

		edge_i += job->num_outgoing_deps;
	}

	// Use Kosaraju's algorithm to find the cycles.

	ast_dtc_dfs(&graph, NULL, false);

	// Transpose the graph.
	for (size_t i = 0; i < graph.num_edges; i++) {
		struct ast_dtc_vertex *tmp = graph.edges[i].from;
		graph.edges[i].from = graph.edges[i].to;
		graph.edges[i].to = tmp;
	}

	struct ast_dtc_components comps = {0};
	ast_dtc_dfs(&graph, &comps, true);

	for (size_t i = 0; i < comps.num_comps; i++) {
		ast_dt_report_cyclic_dependency_chain(
				ctx, comps.comps[i]);
	}

	free(comps.comps);
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
			printf(TC(TC_BRIGHT_RED, "job failed!") "\n");
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
#endif
		return -1;
	}

	return 0;
}

static ast_member_id
ast_dt_num_descendant_members(struct ast_dt_context *ctx,
		struct stg_module *mod, type_id type)
{
	struct type *member_type;
	member_type = vm_get_type(ctx->ast_ctx->vm, type);

	if (member_type->obj_inst) {
		return object_cons_num_descendants(
				ctx->ast_ctx->vm, member_type->obj_inst->cons);
	} else {
		return 0;
	}
}

// Note that the top-level object itself is concidered to have persistant ID 0,
// and hence the first member receives ID 1.
static ast_member_id
ast_dt_calculate_persistant_id(struct ast_dt_context *ctx,
		ast_member_id mbr_id, size_t unpack_id)
{
	struct ast_dt_member *mbr = get_member(ctx, mbr_id);
	return mbr->persistant_id + unpack_id;
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
	type_id type;
};

static struct string
ast_dt_composite_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct ast_dt_composite_info *info = type->data;
	struct string res = {0};

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
	struct string res = {0};

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

static type_id
ast_dt_pack_type_func(struct vm *vm, void *data,
		void **params, size_t num_params)
{
	struct ast_dt_composite_info *info = data;
	return info->type;
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
ast_dt_composite_make_type(struct ast_dt_context *ctx,
		ast_dt_composite_id comp_id)
{
	struct ast_dt_composite *comp;
	comp = get_composite(ctx, comp_id);

	struct ast_node *node = comp->root_node;

	struct arena *mem = &ctx->mod->mem;

	struct object_cons *def;
	def = arena_alloc(&ctx->mod->mem, sizeof(struct object_cons));
	def->pack      = ast_dt_pack_func;
	def->pack_type = ast_dt_pack_type_func;
	def->unpack    = ast_dt_unpack_func;

	size_t num_members = node->composite.num_members;

	struct ast_dt_local_member *local_members;
	local_members = arena_allocn(mem, num_members,
			sizeof(struct ast_dt_local_member));

	struct object_cons_param *params;
	params = arena_allocn(mem, num_members,
			sizeof(struct object_cons_param));

	size_t num_mbr_binds = 0;
	size_t num_mbr_exprs = 0;

	size_t offset = 0;
	ast_member_id cumulative_persistant_id = 1;
	for (size_t i = 0; i < num_members; i++) {
		local_members[i].name = node->composite.members[i].name;
		local_members[i].location = offset;

		ast_member_id mbr_id;
		mbr_id = comp->local_member_ids[i];

		struct ast_dt_member *mbr;
		mbr = get_member(ctx, mbr_id);

		assert(mbr->parent == comp_id);

		params[i].name = node->composite.members[i].name;
		params[i].type = mbr->type;
		params[i].def_loc = mbr->decl_loc;

		mbr->persistant_id = cumulative_persistant_id;
		cumulative_persistant_id += 1 +
			ast_dt_num_descendant_members(ctx, ctx->mod, mbr->type);

		struct type *member_type;
		member_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

		local_members[i].type = mbr->type;
		local_members[i].size = member_type->size;

		if (!mbr->local_explicitly_bound) {
			struct object_inst *inst;
			inst = member_type->obj_inst;
			if (inst) {
				num_mbr_binds += inst->num_binds;
				num_mbr_exprs += inst->num_exprs;
			}
		}

		offset += member_type->size;
	}

	// NOTE: ast_dt_calculate_persistant_id can only be used after this
	// point because it is dependent on persistant_id being set for local
	// members.

	def->params = params;
	def->num_params = node->composite.num_members;

	size_t num_exprs = num_mbr_exprs;
	for (ast_dt_expr_id expr_i = 0;
			expr_i < ctx->exprs.length; expr_i++) {
		struct ast_dt_expr *expr;
		expr = get_expr(ctx, expr_i);
		
		if (expr->parent == comp_id) {
			num_exprs += 1;
		}
	}

	struct object_inst_expr *exprs;
	exprs = arena_allocn(mem, num_exprs,
			sizeof(struct object_inst_expr));

	size_t num_binds = num_mbr_binds;
	for (size_t mbr_i = 0; mbr_i < ctx->members.length; mbr_i++) {
		struct ast_dt_member *mbr = get_member(ctx, mbr_i);

		if (mbr->parent != comp_id) {
			continue;
		}

		num_binds += mbr->num_binds;
	}

	struct object_inst_bind *binds;
	binds = arena_allocn(mem, num_binds,
			sizeof(struct object_inst_bind));

	size_t bind_i = 0;
	size_t expr_i = 0;
	size_t init_i = 0;

	for (size_t i = 0; i < num_members; i++) {
		ast_member_id mbr_id;
		mbr_id = comp->local_member_ids[i];

		struct ast_dt_member *mbr;
		mbr = get_member(ctx, mbr_id);

		assert(mbr->parent == comp_id);

		struct type *member_type;
		member_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

		struct object_inst *inst;
		inst = member_type->obj_inst;

		if (mbr->local_explicitly_bound || !inst) {
			continue;
		}

		ssize_t max_init_i = -1;
		for (size_t i = 0; i < inst->num_exprs; i++) {
			struct object_inst_expr *out;
			out = &exprs[expr_i+i];

			struct object_inst_expr *expr;
			expr = &inst->exprs[i];

			*out = *expr;

			out->deps = arena_allocn(mem, out->num_deps,
					sizeof(struct object_inst_dep));
			for (size_t dep_i = 0; dep_i < out->num_deps; dep_i++) {
				struct object_inst_dep *out_dep;
				out_dep = &out->deps[dep_i];

				struct object_inst_dep *dep;
				dep = &expr->deps[dep_i];
				out_dep->kind = dep->kind;
				switch (out_dep->kind) {
					case OBJECT_INST_DEP_MEMBER:
						out_dep->member = mbr->persistant_id + dep->member;
						break;

					case OBJECT_INST_DEP_INIT_EXPR:
						out_dep->init_expr = init_i + dep->init_expr;
						break;
				}
			}

			if (out->is_init_expr) {
				if ((ssize_t)out->init_id > max_init_i) {
					max_init_i = out->init_id;
				}
				out->init_id += init_i;
			}
		}

		for (size_t i = 0; i < inst->num_binds; i++) {
			struct object_inst_bind *out;
			out = &binds[bind_i+i];

			struct object_inst_bind *bnd;
			bnd = &inst->binds[i];

			out->target_id   = mbr->persistant_id + bnd->target_id;
			out->unpack_id   = bnd->unpack_id;
			out->expr_id     = expr_i + bnd->expr_id;
			out->overridable = bnd->overridable;
			out->loc         = bnd->loc;
		}

		bind_i += inst->num_binds;
		expr_i += inst->num_exprs;
		init_i += max_init_i+1;
	}

	for (ast_dt_expr_id i = 0;
			i < ctx->exprs.length; i++) {
		struct ast_dt_expr *expr;
		expr = get_expr(ctx, i);

		if (expr->parent != comp_id) {
			continue;
		}

		struct object_inst_expr *out_expr;
		out_expr = &exprs[expr_i];
		assert(expr->comp_local_id < 0);
		expr->comp_local_id = expr_i;
		expr_i += 1;

		if (expr->constant) {
			out_expr->constant = true;
			out_expr->const_value = expr->const_value;
		} else {
			assert(expr->value.func != FUNC_UNSET);
			out_expr->constant = false;
			out_expr->func = expr->value.func;
		}

		out_expr->num_deps = expr->num_deps;
		out_expr->deps = arena_allocn(mem,
				out_expr->num_deps, sizeof(struct object_inst_dep));

		for (size_t i = 0; i < expr->num_deps; i++) {
			struct ast_gen_dt_param *dep;
			dep = &expr->deps[i];
			switch (dep->ref.kind) {
				case AST_GEN_DT_PARAM_MEMBER:
					out_expr->deps[i].kind = OBJECT_INST_DEP_MEMBER;
					out_expr->deps[i].member =
						ast_dt_calculate_persistant_id(
								ctx, dep->ref.member.id,
								dep->ref.member.unpack_id);
					break;

				case AST_GEN_DT_PARAM_INIT_EXPR:
					out_expr->deps[i].kind = OBJECT_INST_DEP_INIT_EXPR;
					out_expr->deps[i].init_expr = init_i + dep->ref.init_expr;
					break;
			}
		}

		out_expr->loc = expr->loc;
	}
	assert(expr_i == num_exprs);

	ssize_t local_max_init_i = -1;
	for (size_t i = 0; i < ctx->num_init_exprs; i++) {
		struct ast_dt_init_expr *init_expr;
		init_expr = &ctx->init_exprs[i];

		if (init_expr->parent != comp_id) {
			continue;
		}

		struct ast_dt_expr *dt_expr;
		dt_expr = get_expr(ctx, init_expr->expr);

		struct object_inst_expr *expr;
		expr = &exprs[dt_expr->comp_local_id];
		expr->is_init_expr = true;
		expr->init_id = init_i + init_expr->id;
		if (init_expr->id > local_max_init_i) {
			local_max_init_i = init_expr->id;
		}
	}
	init_i += local_max_init_i+1;

	for (size_t mbr_i = 0; mbr_i < ctx->members.length; mbr_i++) {
		struct ast_dt_member *mbr = get_member(ctx, mbr_i);

		if (mbr->parent != comp_id) {
			continue;
		}

		for (size_t i = 0; i < mbr->num_binds; i++) {
			struct ast_dt_member_bind *mbr_bind;
			mbr_bind = &mbr->binds[i];

			struct ast_dt_bind *bind;
			bind = get_bind(ctx, mbr_bind->bind);

			struct ast_dt_expr *expr;
			expr = get_expr(ctx, bind->expr);
			assert(expr->parent == comp_id);

			binds[bind_i].target_id =
				ast_dt_calculate_persistant_id(
						ctx, mbr_i, mbr_bind->unpack_id);
			binds[bind_i].loc = bind->loc;
			binds[bind_i].expr_id = expr->comp_local_id;
			binds[bind_i].unpack_id = mbr_bind->unpack_id;
			binds[bind_i].overridable = bind->overridable;

			bind_i += 1;
		}
	}

	assert(bind_i == num_binds);

	struct object_inst *inst;
	inst = arena_alloc(mem, sizeof(struct object_inst));

	inst->exprs = exprs;
	inst->num_exprs = num_exprs;
	inst->init_monad = node->composite.is_init_monad;

	inst->binds = binds;
	inst->num_binds = num_binds;
	inst->cons = def;

	struct ast_dt_composite_info *info;
	info = arena_alloc(mem, sizeof(struct ast_dt_composite_info));

	info->num_members = node->composite.num_members;
	info->members = local_members;

	def->data = info;

	struct type dt_type = {0};
	dt_type.base = &ast_dt_composite_base;
	dt_type.obj_inst = inst;
	dt_type.size = offset;
	dt_type.data = info;

	type_id result;
	result = stg_register_type(ctx->mod, dt_type);

	info->type = result;
	inst->type = result;

	comp->type = result;

	return result;
}

type_id
ast_dt_finalize_composite(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *comp, struct ast_typecheck_closure *closures, size_t num_closures)
{
	if (comp->composite.type != TYPE_UNSET) {
		return comp->composite.type;
	}

	struct ast_dt_context dt_ctx = {0};
	dt_ctx.ast_ctx = ctx;
	dt_ctx.mod = mod;
	dt_ctx.terminal_jobs  = -1;

	dt_ctx.tmp_mem = &ctx->vm->transient;
	arena_mark transient_cp = arena_checkpoint(dt_ctx.tmp_mem);


	paged_list_init(
			&dt_ctx.jobs,
			&ctx->vm->mem,
			sizeof(struct ast_dt_expr));

	paged_list_init(
			&dt_ctx.composites,
			&ctx->vm->mem,
			sizeof(struct ast_dt_composite));

	paged_list_init(
			&dt_ctx.exprs,
			&ctx->vm->mem,
			sizeof(struct ast_dt_expr));

	paged_list_init(
			&dt_ctx.binds,
			&ctx->vm->mem,
			sizeof(struct ast_dt_bind));

	paged_list_init(
			&dt_ctx.members,
			&ctx->vm->mem,
			sizeof(struct ast_dt_member));

#if AST_DT_DEBUG_JOBS
	static size_t next_run_i = 0;
	dt_ctx.run_i  = next_run_i++;

	printf("\nbegin composite %zu\n", dt_ctx.run_i);
#endif

	int err;

	ast_dt_composite_id root_comp_id;
	root_comp_id = ast_dt_register_composite(
			&dt_ctx, comp, true, closures, num_closures);

	err = ast_dt_run_jobs(&dt_ctx);
#if AST_DT_DEBUG_JOBS
	if (err) {
		printf("One or more jobs failed when resolving datastructure.\n");
	}
#endif

	type_id result = TYPE_UNSET;

	if (!err && dt_ctx.num_errors == 0) {
		struct ast_dt_composite *comp;
		comp = get_composite(&dt_ctx, root_comp_id);
		result = comp->type;
	}

	for (size_t comp_i = 0; comp_i < dt_ctx.composites.length; comp_i++) {
		struct ast_dt_composite *comp;
		comp = get_composite(&dt_ctx, comp_i);
		free(comp->local_member_ids);
	}

	for (size_t expr_i = 0; expr_i < dt_ctx.exprs.length; expr_i++) {
		struct ast_dt_expr *expr;
		expr = get_expr(&dt_ctx, expr_i);
		free(expr->deps);
	}

	for (size_t job_i = 0; job_i < dt_ctx.jobs.length; job_i++) {
		struct ast_dt_job *job;
		job = get_job(&dt_ctx, job_i);
		free(job->outgoing_deps);
	}

	paged_list_destroy(&dt_ctx.jobs);
	paged_list_destroy(&dt_ctx.exprs);
	paged_list_destroy(&dt_ctx.binds);
	paged_list_destroy(&dt_ctx.members);
	paged_list_destroy(&dt_ctx.composites);

	arena_reset(dt_ctx.tmp_mem, transient_cp);

#if AST_DT_DEBUG_JOBS
	printf("end composite ");
	print_type_repr(ctx->vm, vm_get_type(ctx->vm, result));
	printf("\n\n");
#endif
	comp->composite.type = result;
	return result;
}

struct stg_type_variant_option {
	struct atom *name;
	type_id data_type;
	size_t size;
	obj_copy copy;
	void *type_data;
	struct stg_location loc;
};

struct stg_type_variant_info {
	struct stg_type_variant_option *options;
	size_t num_options;
	uint8_t tag_size;
	size_t total_size;
	struct stg_location loc;
	type_id type;
};

struct ast_dt_variant {
	uint64_t tag;
	struct object data;
};

static struct ast_dt_variant
ast_dt_decode_variant(struct stg_type_variant_info *info, void *obj_data)
{
	uint64_t tag;
	void *data;

	switch (info->tag_size) {
		case 1: tag = *(uint8_t  *)obj_data; break;
		case 2: tag = *(uint16_t *)obj_data; break;
		case 4: tag = *(uint32_t *)obj_data; break;
		case 8: tag = *(uint64_t *)obj_data; break;
		default:
			panic("Invalid variant tag size");
			return (struct ast_dt_variant){0};
	}

	assert(tag < info->num_options);

	data = (void *)((uint8_t *)obj_data + info->tag_size);

	struct ast_dt_variant var = {0};
	var.tag = tag;
	var.data.type = info->options[tag].data_type;
	var.data.data = data;

	return var;
}

static void
ast_dt_encode_variant(struct stg_type_variant_info *info,
		uint64_t tag, void *in_data, void *out_data)
{
	memset(out_data, 0, info->total_size);
	switch (info->tag_size) {
		case 1: *((uint8_t  *)out_data) = (uint8_t )tag; break;
		case 2: *((uint16_t *)out_data) = (uint16_t)tag; break;
		case 4: *((uint32_t *)out_data) = (uint32_t)tag; break;
		case 8: *((uint64_t *)out_data) = (uint64_t)tag; break;
		default:
			panic("Invalid variant tag size");
			return;
	}

	void *out_data_ptr = ((uint8_t *)out_data) + info->tag_size;
	if (info->options[tag].size) {
		memcpy(out_data_ptr, in_data, info->options[tag].size);
	}
}

static struct string
ast_dt_variant_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct stg_type_variant_info *info = type->data;
	struct string res = {0};

	arena_string_append(mem, &res, STR("Variant { "));

	for (size_t i = 0; i < info->num_options; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		arena_string_append_sprintf(mem, &res, "%.*s%s", ALIT(info->options[i].name),
				info->options[i].data_type != TYPE_UNSET ? " " : "");

		if (info->options[i].data_type != TYPE_UNSET) {
			struct type *option_type;
			option_type = vm_get_type(vm, info->options[i].data_type);
			arena_string_append_type_repr(&res, vm, mem, option_type);
		}
	}

	arena_string_append(mem, &res, STR(" }"));

	return res;
}

static struct string
ast_dt_variant_obj_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	struct type *type = vm_get_type(vm, obj->type);
	struct stg_type_variant_info *info = type->data;
	struct string res = {0};

	struct ast_dt_variant var;
	var = ast_dt_decode_variant(info, obj->data);

	struct stg_type_variant_option *opt;
	opt = &info->options[var.tag];

	if (opt->data_type != TYPE_UNSET) {
		arena_string_append_sprintf(mem, &res, "%.*s(", ALIT(opt->name));
		arena_string_append_obj_repr(&res, vm, mem, &var.data);
		arena_string_append(mem, &res, STR(")"));
	} else {
		arena_string_append(mem, &res, opt->name->name);
	}

	return res;
}

static void
ast_dt_variant_obj_copy(struct stg_exec *heap, void *type_data, void *obj_data)
{
	struct stg_type_variant_info *info = type_data;
	struct ast_dt_variant var;
	var = ast_dt_decode_variant(info, obj_data);

	struct stg_type_variant_option *opt;
	opt = &info->options[var.tag];

	if (opt->copy) {
		opt->copy(heap,
				opt->type_data,
				var.data.data);
	}
}

struct type_base variant_type_base = {
	.name     = STR("Variant"),
	.repr     = ast_dt_variant_repr,
	.obj_repr = ast_dt_variant_obj_repr,
	.obj_copy = ast_dt_variant_obj_copy,

	// TODO: Implement
	// .free = ...,
};

struct ast_dt_variant_cons_closure {
	struct stg_type_variant_info *info;
	uint64_t tag;
};

static void
ast_dt_variant_pack(struct vm *vm, void *data, void *out,
		void **args, size_t num_args)
{
	struct ast_dt_variant_cons_closure *closure = data;
	void *arg = args[0];

	ast_dt_encode_variant(
			closure->info, closure->tag,
			arg, out);
}

static void
ast_dt_variant_unpack(struct vm *vm, void *data, void *out,
		void *obj, int param_id)
{
	assert(param_id == 0);

	struct ast_dt_variant_cons_closure *closure = data;

	struct ast_dt_variant val;
	val = ast_dt_decode_variant(
			closure->info, obj);
	assert(val.tag == closure->tag);

	memcpy(out, val.data.data,
			closure->info->options[val.tag].size);
}

static bool
ast_dt_variant_can_unpack(struct vm *vm, void *data, void *obj)
{
	struct ast_dt_variant_cons_closure *closure = data;

	struct ast_dt_variant val;
	val = ast_dt_decode_variant(
			closure->info, obj);
	return val.tag == closure->tag;
}

static struct object
ast_dt_create_variant_type_scope(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_type_variant_info *info)
{
	struct arena *mem = &mod->mem;

	struct ast_node *scope;

	scope = ast_init_node_composite(
			ctx, AST_NODE_NEW, info->loc,
			AST_COMPOSITE_STATIC_OBJ);

	struct type *variant_type;
	variant_type = vm_get_type(mod->vm, info->type);

	for (size_t tag = 0; tag < info->num_options; tag++) {
		struct object obj = {0};
		type_id option_type = info->options[tag].data_type;
		if (option_type != TYPE_UNSET) {
			struct func func = {0};
			func.type = stg_register_func_type(
					mod, info->type, &option_type, 1);

			struct ast_dt_variant_cons_closure *closure;
			closure = arena_alloc(mem, sizeof(struct ast_dt_variant_cons_closure));
			closure->tag = tag;
			closure->info = info;

			func.kind = FUNC_CONS;
			func.cons = arena_alloc(mem, sizeof(struct object_cons));
			func.cons->num_params = 1;
			func.cons->params = arena_alloc(mem, sizeof(struct object_cons_param));
			func.cons->params[0].name = NULL;
			func.cons->params[0].type = option_type;
			func.cons->data = closure;
			func.cons->pack = ast_dt_variant_pack;
			func.cons->unpack = ast_dt_variant_unpack;
			func.cons->can_unpack = ast_dt_variant_can_unpack;

			func_id fid;
			fid = stg_register_func(mod, func);

			obj = stg_register_func_object(
					mod->vm, &mod->store, fid, NULL);
		} else {
			uint8_t buffer[variant_type->size];
			assert(info->options[tag].size == 0);
			ast_dt_encode_variant(
					info, tag, NULL, buffer);

			obj.type = info->type;
			obj.data = buffer;
			obj = register_object(mod->vm, &mod->store, obj);
		}

		struct ast_node *value_node;
		value_node = ast_init_node_lit(
				ctx, AST_NODE_NEW,
				info->options[tag].loc, obj);

		struct ast_node *target_node;
		target_node = ast_init_node_lookup(
				ctx, AST_NODE_NEW, info->options[tag].loc,
				info->options[tag].name);

		int bind_id;
		bind_id = ast_node_composite_bind(
				ctx, scope, target_node,
				value_node, false);

		ast_node_composite_add_member(
				ctx, scope, info->options[tag].name,
				NULL, bind_id);
	}

	type_id scope_type;
	scope_type = ast_dt_finalize_composite(
			ctx, mod, scope, NULL, 0);

	struct object res = {0};

	int err;
	err = stg_instantiate_static_object(
			ctx, mod, scope_type, &res);

	if (err) {
		printf("Failed to create static object for variant.\n");
		return OBJ_UNSET;
	}

	return res;
}

type_id
ast_dt_finalize_variant(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_datatype_variant *options, size_t num_options,
		struct ast_typecheck_dep *deps, size_t num_deps)
{
	bool ok = true;
	size_t max_data_size = 0;

	struct stg_type_variant_option *opts;
	opts = arena_allocn(&mod->mem,
			num_options, sizeof(struct stg_type_variant_option));

	for (size_t i = 0; i < num_options; i++) {
		opts[i].name = options[i].name;
		opts[i].data_type = TYPE_UNSET;
		opts[i].loc = options[i].loc;

		if (options[i].data_type) {
			int err;

			struct object type_out = {0};

			err = ast_node_typecheck(
					ctx, mod,
					options[i].data_type,
					deps, num_deps,
					ctx->vm->default_types.type,
					&type_out);
			if (err) {
				ok = false;
				continue;
			}

			assert_type_equals(ctx->vm,
					options[i].data_type->type, ctx->vm->default_types.type);

			if (!type_equals(ctx->vm,
						type_out.type, ctx->vm->default_types.type)) {
				stg_error(ctx->err, opts[i].loc,
						"Failed to evaluate the option's data type.");
				ok = false;
				continue;
			}

			opts[i].data_type = *(type_id *)type_out.data;

			struct type *type;
			type = vm_get_type(ctx->vm, opts[i].data_type);
			if (type->size > max_data_size) {
				max_data_size = type->size;
			}

			opts[i].size = type->size;
			opts[i].copy = type->base->obj_copy;
			opts[i].type_data = type->data;
		}
	}

	if (!ok) {
		free(opts);
		return TYPE_UNSET;
	}

	struct stg_type_variant_info *info;
	info = arena_alloc(&mod->mem,
			sizeof(struct stg_type_variant_info));

	info->num_options = num_options;
	info->options = opts;
	// TODO: Variant location
	// info->loc = node->loc;
	info->loc = STG_NO_LOC;

	size_t tag_size;
	ffi_type *tag_type;

	if (num_options <= UINT8_MAX) {
		tag_size = 1;
		tag_type = &ffi_type_uint8;
	} else if (num_options <= UINT16_MAX) {
		tag_size = 2;
		tag_type = &ffi_type_uint16;
	} else if (num_options <= UINT32_MAX) {
		tag_size = 4;
		tag_type = &ffi_type_uint32;
	} else {
		tag_size = 8;
		tag_type = &ffi_type_uint64;
	}

	info->tag_size = tag_size;
	info->total_size = tag_size + max_data_size;

	struct type new_type = {0};

	new_type.name = mod_atoms(mod, "Variant");
	new_type.base = &variant_type_base;
	new_type.size = info->total_size;
	new_type.data = info;

	if (max_data_size == 0) {
		new_type.ffi_type = tag_type;
	}

	type_id variant_type_id;
	variant_type_id = stg_register_type(mod, new_type);

	info->type = variant_type_id;

	struct type *variant_type;
	variant_type = vm_get_type(mod->vm, variant_type_id);

	variant_type->static_object =
		ast_dt_create_variant_type_scope(
				ctx, mod, info);

	return variant_type_id;
}
