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

// For sysconf
#include <unistd.h>

// For mmap
#include <sys/mman.h>

#define AST_DT_DEBUG_JOBS 0

typedef int ast_dt_job_id;
typedef int ast_dt_bind_id;
typedef int ast_dt_expr_id;

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
	struct object const_value;

	struct {
		struct ast_node *node;
		func_id func;
	} value;

	type_id type;

	struct stg_location loc;

	ast_member_id *member_deps;
	size_t num_member_deps;

	struct ast_dt_expr_jobs value_jobs;
};

struct ast_dt_bind {
	struct ast_node *target_node;
	ast_member_id target;
	ast_dt_expr_id expr;

	bool overridable;

	struct {
		ast_dt_job_id resolve_names;
		ast_dt_job_id resolve;
	} target_jobs;

	struct stg_location loc;
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

	ast_dt_bind_id bound;
	int bound_unpack_id;

	ast_dt_bind_id typegiving_bind;
	ast_dt_bind_id overridden_bind;

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
};

struct ast_dt_use {
	ast_dt_expr_id expr;
	struct atom *as_name;

	type_id type;

	bool is_const;
	struct object const_value;

	ast_dt_job_id const_resolved;
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

	AST_DT_JOB_EXPR_RESOLVE_NAMES,
	AST_DT_JOB_EXPR_RESOLVE_TYPES,
	AST_DT_JOB_EXPR_CODEGEN,

	AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES,
	AST_DT_JOB_BIND_TARGET_RESOLVE,

	AST_DT_JOB_USE_CONST_EVAL,
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
		ast_dt_expr_id expr;

		ast_dt_bind_id bind;

		ast_use_id use;

		// Used when the job is not allocated.
		ast_dt_job_id free_list;
	};
};

struct ast_dt_context {
	struct ast_node *root_node;
	// An array of the ids of all local members, in the same order as
	// root_node->composite.members. The lenght is root_node->composite.num_members.
	ast_member_id *local_member_ids;

	struct paged_list jobs;
	ast_dt_job_id free_list;
	// A linked list of nodes that have no incoming edges.
	ast_dt_job_id terminal_jobs;
	size_t unvisited_job_deps;

	struct paged_list exprs;
	struct paged_list binds;
	struct paged_list members;

	struct ast_dt_use *uses;
	size_t num_uses;

	ast_dt_job_id target_names_resolved;
	ast_dt_job_id use_resolved;

	struct ast_context *ast_ctx;
	struct stg_module  *mod;

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

static inline struct ast_dt_use *
get_use(struct ast_dt_context *ctx, ast_use_id id)
{
	assert(id < ctx->num_uses);
	return &ctx->uses[id];
}

static inline ast_member_id
ast_dt_member_get_descendant(struct ast_dt_context *ctx,
		ast_member_id mbr_id, size_t descendant_id)
{
	if (descendant_id == 0) {
		return mbr_id;
	}

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);
	if ((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
		return mbr->first_child + descendant_id-1;
	} else {
		return mbr_id + descendant_id;
	}
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

static ast_use_id
ast_dt_alloc_use(struct ast_dt_context *ctx)
{
	struct ast_dt_use use = {0};

	return dlist_append(
			ctx->uses,
			ctx->num_uses,
			&use);
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
		ast_use_id use, enum ast_dt_job_kind kind)
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
		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
			return "MBR TPE NAMES";
		case AST_DT_JOB_MBR_TYPE_RESOLVE_TYPES:
			return "MBR TPE TYPES";
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
		case AST_DT_JOB_BIND_TARGET_RESOLVE:
			return "BND TAR RESOLVE";

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
		case AST_DT_JOB_BIND_TARGET_RESOLVE:
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

				printf("use ");
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
ast_dt_expr_has_ambiguous_refs(struct ast_dt_context *ctx, struct ast_node *node)
{
	int res;
	res = ast_composite_node_has_ambiguous_refs(
			ctx->ast_ctx, NULL, ctx->root_node, node, ctx->local_member_ids);

	return res != 0;
}

static void
ast_dt_bind_to_member(struct ast_dt_context *ctx,
		ast_member_id mbr_id, ast_dt_bind_id bind_id,
		int unpack_id, bool typegiving)
{
	struct ast_dt_member *member;
	member = get_member(ctx, mbr_id);

	struct ast_dt_bind *new_bind;
	new_bind = get_bind(ctx, bind_id);

	if (typegiving) {
		if (member->typegiving_bind >= 0) {
			struct ast_dt_bind *prev_bind;
			prev_bind = get_bind(ctx, member->typegiving_bind);

			stg_error(ctx->ast_ctx->err, new_bind->loc,
					"'%.*s' has multiple typegiving binds.", ALIT(member->name));
			stg_error(ctx->ast_ctx->err, prev_bind->loc,
					"Also has a typegiving bind from here.");
		} else {
			member->typegiving_bind = bind_id;
		}
	}

	if (member->bound >= 0) {
		struct ast_dt_bind *prev_bind;
		prev_bind = get_bind(ctx, member->bound);

		if (!prev_bind->overridable && !new_bind->overridable) {
			stg_error(ctx->ast_ctx->err, new_bind->loc,
					"'%.*s' is bound multiple times.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err, prev_bind->loc,
					"Also bound here.");
			ctx->num_errors += 1;
			return;
		}

		if (prev_bind->overridable && new_bind->overridable) {
			stg_error(ctx->ast_ctx->err, new_bind->loc,
					"'%.*s' has multiple default binds.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err, prev_bind->loc,
					"Also bound here.");
			ctx->num_errors += 1;
			return;
		}

		if (new_bind->overridable) {
			member->overridden_bind = bind_id;
		} else {
			member->overridden_bind = member->bound;
			member->bound = bind_id;
			member->bound_unpack_id = unpack_id;
		}
	} else {
		member->bound = bind_id;
		member->bound_unpack_id = unpack_id;
	}
}

static ast_dt_expr_id
ast_dt_register_expr(struct ast_dt_context *ctx,
		struct ast_node *value)
{
	ast_dt_expr_id expr_i;
	expr_i = ast_dt_alloc_expr(ctx);

	struct ast_dt_expr *new_expr;
	new_expr = get_expr(ctx, expr_i);

	new_expr->constant = false;
	new_expr->value.node = value;
	new_expr->value.func = FUNC_UNSET;
	new_expr->type = TYPE_UNSET;
	new_expr->loc = value->loc;

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
			new_expr->value_jobs.resolve_names,
			new_expr->value_jobs.resolve_types);

	ast_dt_job_dependency(ctx,
			new_expr->value_jobs.resolve_types,
			new_expr->value_jobs.codegen);

	if (ast_dt_expr_has_ambiguous_refs(ctx, new_expr->value.node)) {
		ast_dt_job_dependency(ctx,
				ctx->use_resolved,
				new_expr->value_jobs.resolve_names);
	}


	return expr_i;
}

static ast_dt_expr_id
ast_dt_register_expr_func(struct ast_dt_context *ctx,
		struct stg_location loc, func_id value_func)
{
	ast_dt_expr_id expr_i;
	expr_i = ast_dt_alloc_expr(ctx);

	struct ast_dt_expr *new_expr;
	new_expr = get_expr(ctx, expr_i);

	new_expr->constant = false;
	new_expr->value.node = NULL;
	new_expr->value.func = value_func;
	new_expr->loc = loc;

	struct func *func;
	func = vm_get_func(ctx->ast_ctx->vm, value_func);

	struct type *func_type;
	func_type = vm_get_type(ctx->ast_ctx->vm, func->type);

	struct stg_func_type *func_info;
	func_info = func_type->data;

	new_expr->type = func_info->return_type;

	new_expr->value_jobs.resolve_names = -1;
	new_expr->value_jobs.resolve_types = -1;
	new_expr->value_jobs.codegen = -1;

	return expr_i;
}

static ast_dt_expr_id
ast_dt_register_expr_const(struct ast_dt_context *ctx,
		struct stg_location loc, struct object obj)
{
	ast_dt_expr_id expr_i;
	expr_i = ast_dt_alloc_expr(ctx);

	struct ast_dt_expr *new_expr;
	new_expr = get_expr(ctx, expr_i);

	new_expr->constant = true;
	new_expr->const_value = obj;
	new_expr->value.node = NULL;
	new_expr->value.func = FUNC_UNSET;
	new_expr->loc = loc;
	new_expr->type = obj.type;

	new_expr->value_jobs.resolve_names = -1;
	new_expr->value_jobs.resolve_types = -1;
	new_expr->value_jobs.codegen = -1;

	return expr_i;
}

static ast_member_id
ast_dt_register_local_member(struct ast_dt_context *ctx,
		struct atom *name, struct stg_location decl_loc,
		struct ast_node *type_expr)
{
	ast_member_id mbr_id;
	mbr_id = ast_dt_alloc_member(ctx);

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	mbr->name = name;
	mbr->decl_loc = decl_loc;
	mbr->flags |= AST_DT_MEMBER_IS_LOCAL;
	mbr->first_child = -1;
	mbr->persistant_id = -1;
	mbr->type_node = type_expr;

	mbr->bound = -1;
	mbr->typegiving_bind = -1;
	mbr->overridden_bind = -1;

	mbr->type_resolved = -1;
	mbr->const_resolved = -1;

	mbr->type_jobs.resolve_names = -1;
	mbr->type_jobs.resolve_types = -1;
	mbr->type_jobs.codegen = -1;

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

		if (ast_dt_expr_has_ambiguous_refs(ctx, mbr->type_node)) {
			ast_dt_job_dependency(ctx,
					ctx->use_resolved,
					mbr->type_jobs.resolve_names);
		}

		mbr->type_resolved = mbr->type_jobs.codegen;
	}

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
ast_dt_register_descendant_member(struct ast_dt_context *ctx,
		struct atom *name, struct stg_location decl_loc,
		type_id type, ast_member_id anscestor_id)
{
	ast_member_id mbr_id;
	mbr_id = ast_dt_alloc_member(ctx);

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	mbr->name = name;
	mbr->decl_loc = decl_loc;
	mbr->type = type;

	// Not local
	mbr->flags &= ~AST_DT_MEMBER_IS_LOCAL;
	mbr->anscestor_local_member = anscestor_id;
	mbr->persistant_id = -1;

	mbr->bound = -1;
	mbr->type_jobs.resolve_names = -1;
	mbr->type_jobs.resolve_types = -1;
	mbr->type_jobs.codegen = -1;

	mbr->type_resolved = -1;

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

	return mbr_id;
}

static ast_dt_bind_id
ast_dt_register_bind(struct ast_dt_context *ctx,
		struct ast_node *target_node, ast_dt_expr_id expr_id)
{
	ast_dt_bind_id bind_id;
	bind_id = ast_dt_alloc_bind(ctx);

	struct ast_dt_bind *bind;
	bind = get_bind(ctx, bind_id);

	struct ast_dt_expr *expr;
	expr = get_expr(ctx, expr_id);

	bind->target_node = target_node;
	bind->target = -1;
	bind->expr = expr_id;

	bind->target_jobs.resolve_names =
		ast_dt_job_bind(ctx, bind_id,
				AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES);

	bind->target_jobs.resolve =
		ast_dt_job_bind(ctx, bind_id,
				AST_DT_JOB_BIND_TARGET_RESOLVE);

	ast_dt_job_dependency(ctx,
			bind->target_jobs.resolve_names,
			bind->target_jobs.resolve);

	ast_dt_job_dependency(ctx,
			bind->target_jobs.resolve,
			expr->value_jobs.resolve_types);

	ast_dt_job_dependency(ctx,
			bind->target_jobs.resolve_names,
			ctx->target_names_resolved);

	// if (ast_dt_expr_has_ambiguous_refs(ctx, bind->target_node)) {
	// 	ast_dt_job_dependency(ctx,
	// 			ctx->use_resolved,
	// 			bind->target_jobs.resolve_names);
	// }

	return bind_id;
}

static ast_dt_bind_id
ast_dt_register_explicit_bind(struct ast_dt_context *ctx,
		ast_member_id mbr_id, ast_dt_expr_id expr_id,
		int unpack_id, bool typegiving)
{
	ast_dt_bind_id bind_id;
	bind_id = ast_dt_alloc_bind(ctx);

	struct ast_dt_bind *bind;
	bind = get_bind(ctx, bind_id);

	bind->target_node = NULL;
	bind->expr = expr_id;
	bind->target = mbr_id;

	bind->target_jobs.resolve_names = -1;
	bind->target_jobs.resolve = -1;

	if (typegiving) {
		struct ast_dt_expr *expr;
		expr = get_expr(ctx, expr_id);

		bind->target_jobs.resolve =
			ast_dt_job_bind(ctx, bind_id,
					AST_DT_JOB_BIND_TARGET_RESOLVE);

		ast_dt_job_dependency(ctx,
				expr->value_jobs.resolve_types,
				bind->target_jobs.resolve);

		struct ast_dt_member *mbr;
		mbr = get_member(ctx, mbr_id);

		ast_dt_job_dependency(ctx,
				bind->target_jobs.resolve,
				mbr->const_resolved);

		ast_dt_job_dependency(ctx,
				expr->value_jobs.codegen,
				mbr->const_resolved);


		struct ast_dt_member *member;
		member = get_member(ctx, mbr_id);

		assert(member->type_resolved < 0);
		member->type_resolved = bind->target_jobs.resolve;
	}

	ast_dt_bind_to_member(
			ctx, mbr_id, bind_id, unpack_id, typegiving);

	return bind_id;
}

static ast_use_id
ast_dt_register_use(struct ast_dt_context *ctx,
		ast_dt_expr_id expr_id, struct atom *as_name)
{
	ast_use_id use_id;
	use_id = ast_dt_alloc_use(ctx);

	struct ast_dt_use *use;
	use = get_use(ctx, use_id);

	struct ast_dt_expr *expr;
	expr = get_expr(ctx, expr_id);

	use->expr = expr_id;
	use->as_name = as_name;

	use->const_resolved = ast_dt_job_use(ctx, use_id,
			AST_DT_JOB_USE_CONST_EVAL);

	ast_dt_job_dependency(ctx,
			expr->value_jobs.codegen,
			use->const_resolved);

	// TODO: Support use of non-constant targets.
	// ast_dt_job_dependency(ctx,
	// 		expr->value_jobs.resolve_types,
	// 		ctx->use_resolved);

	ast_dt_job_dependency(ctx,
			use->const_resolved,
			ctx->use_resolved);

	return use_id;
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

	struct object dep_member_obj[expr->num_member_deps];
	bool is_const = true;

	for (size_t i = 0; i < expr->num_member_deps; i++) {
		assert(expr->member_deps[i] >= 0);

		struct ast_dt_member *dep_mbr;
		dep_mbr = get_member(ctx, expr->member_deps[i]);

		if ((dep_mbr->flags & AST_DT_MEMBER_IS_CONST) == 0) {
			is_const = false;
		}

		dep_member_obj[i] = dep_mbr->const_value;
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
	arena_init(&exec_ctx.heap, &ctx->ast_ctx->vm->mem);

	int err;
	err = vm_call_func(
			ctx->ast_ctx->vm, &exec_ctx, fid, dep_member_obj,
			expr->num_member_deps, &obj);
	if (err) {
		printf("Failed to evaluate constant member.\n");
		return -3;
	}

	expr->const_value =
		register_object(ctx->ast_ctx->vm,
				&ctx->mod->store, obj);

	arena_destroy(&exec_ctx.heap);

	expr->constant = true;
	*out = expr->const_value;

	return 0;
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

				struct object_inst *mbr_inst;
				mbr_inst = mbr_type->obj_inst;


				if (!mbr_inst) {
					stg_error(ctx->ast_ctx->err, node->access.target->loc,
							"This object does not have any members.");
					return -1;
				}

				bool found = false;
				size_t param_id = 0;
				for (size_t i = 0; i < mbr_inst->cons->num_params; i++) {
					if (mbr_inst->cons->params[i].name == node->access.name) {
						found = true;
						break;
					}

					param_id += 1;
					struct type *param_type;
					assert(TYPE_VALID(mbr_inst->cons->params[i].type));
					param_type = vm_get_type(ctx->ast_ctx->vm,
							mbr_inst->cons->params[i].type);
					if (param_type->obj_inst) {
						param_id += object_cons_num_descendants(
								ctx->ast_ctx->vm, param_type->obj_inst->cons);
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

int
ast_dt_l_expr_member(struct ast_dt_context *ctx, struct ast_node *node,
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
			*out_member = node->lookup.ref.member;
			return 0;

		case AST_NODE_ACCESS:
			return ast_dt_l_expr_member(
					ctx, node->access.target,
					out_member);

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
		} else {
			assert(mbr->type);
		}
	}

	for (size_t i = 0; i < node->composite.num_binds; i++) {
		struct ast_datatype_bind *bind;
		bind = &node->composite.binds[i];
		if (bind->erroneous) {
			// TODO: Maby we should register the bind, but tag it as erroneous,
			// to allow typechecking and reporting of potential errors.
			continue;
		}

		ast_dt_expr_id expr_id;
		expr_id = ast_dt_register_expr(ctx,
					bind->value);

		if (type_giving_for[i] >= 0) {
			ast_dt_register_explicit_bind(
					ctx, type_giving_for[i], expr_id, 0, true);
		} else {
			ast_dt_register_bind(
					ctx, bind->target, expr_id);
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
		expr_id = ast_dt_register_expr(ctx,
					node->composite.uses[i].target);

		ast_dt_register_use(ctx, expr_id,
				node->composite.uses[i].as_name);
	}
}

static void
ast_dt_populate_descendants(
		struct ast_dt_context *ctx, ast_member_id local_anscestor,
		ast_member_id parent_id)
{
	struct ast_dt_member *parent;
	parent = get_member(ctx, parent_id);

	assert(parent->type != TYPE_UNSET);

	struct ast_dt_member *anscestor;
	anscestor = get_member(ctx, local_anscestor);

	struct type *parent_type;
	parent_type = vm_get_type(ctx->ast_ctx->vm, parent->type);

	if (parent_type->obj_inst) {
		struct object_cons *def;
		def = parent_type->obj_inst->cons;

		for (size_t i = 0; i < def->num_params; i++) {
			// TODO: Better location.
			ast_slot_id param_mbr_id;

			assert(TYPE_VALID(def->params[i].type));
			param_mbr_id = ast_dt_register_descendant_member(
					ctx, def->params[i].name, STG_NO_LOC,
					def->params[i].type, local_anscestor);

			struct ast_dt_member *param_mbr;
			param_mbr = get_member(ctx, param_mbr_id);

			ast_dt_job_dependency(ctx,
					param_mbr->const_resolved,
					parent->const_resolved);

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

	if (parent_type->obj_inst) {
		struct object_inst *def;
		def = parent_type->obj_inst;

		for (size_t expr_i = 0; expr_i < def->num_exprs; expr_i++) {
			struct object_inst_expr *expr;
			expr = &def->exprs[expr_i];

			size_t num_binds = 0;
			for (size_t bind_i = 0; bind_i < def->num_binds; bind_i++) {
				if (def->binds[bind_i].expr_id == expr_i) {
					num_binds += 1;
				}
			}

			if (num_binds == 0) {
				// TODO: Handle free expressions
				continue;
			}

			ast_member_id *deps;
			deps = calloc(expr->num_deps, sizeof(ast_member_id));
			for (size_t j = 0; j < expr->num_deps; j++) {
				deps[j] = parent->first_child + expr->deps[j];
			}

			ast_member_id targets[num_binds];
			size_t unpack_ids[num_binds];

			size_t target_i = 0;
			for (size_t bind_i = 0; bind_i < def->num_binds; bind_i++) {
				if (def->binds[bind_i].expr_id == expr_i) {
					if (def->binds[bind_i].target_id == 0) {
						targets[target_i] = parent_id;
					} else {
						// Subtract 1 to compansate for def's object being at 0
						// and the first member at 1.
						targets[target_i] = parent->first_child + def->binds[bind_i].target_id-1;
					}
					unpack_ids[target_i] = def->binds[bind_i].unpack_id;
					target_i += 1;
				}
			}
			assert(target_i == num_binds);

			ast_dt_expr_id expr_id;

			if (expr->constant) {
				expr_id = ast_dt_register_expr_const(ctx,
						expr->loc, expr->const_value);
			} else {
				expr_id = ast_dt_register_expr_func(ctx,
						expr->loc, expr->func);
			}

			for (size_t target_i = 0; target_i < num_binds; target_i++) {
				ast_dt_register_explicit_bind(ctx,
						targets[target_i], expr_id,
						unpack_ids[target_i], false);
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
ast_dt_member_num_descendants(
		struct ast_dt_context *ctx, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	assert(mbr->type != TYPE_UNSET);

	struct type *type;
	type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

	if (type->obj_inst) {
		return object_cons_num_descendants(
				ctx->ast_ctx->vm, type->obj_inst->cons) + 1;
	} else {
		return 1;
	}
}

// Returns the number of descendant members this memeber has (including
// itself).
int
ast_dt_find_terminal_members(
		struct ast_dt_context *ctx, ast_member_id mbr_id,
		ast_member_id **out_members, int **out_descendant_members, size_t *out_num_members,
		size_t descendant_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);
	assert(mbr->type != TYPE_UNSET);

	struct type *mbr_type;
	mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

	if (mbr_type->obj_inst) {
		ast_member_id first_child;

		if ((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
			first_child = mbr->first_child;
		} else {
			first_child = mbr_id + 1;
		}

		int num_descendants = 0;
		for (size_t i = 0; i < mbr_type->obj_inst->cons->num_params; i++) {
			num_descendants += ast_dt_find_terminal_members(
					ctx, first_child + num_descendants,
					out_members, out_descendant_members, out_num_members,
					1 + descendant_id + num_descendants);
		}

		return num_descendants + 1;
	} else {
		dlist_append(
				*out_members,
				*out_num_members,
				&mbr_id);
		if (out_descendant_members) {
			*out_descendant_members = realloc(
					*out_descendant_members,
					sizeof(int) * (*out_num_members));
			(*out_descendant_members)[*out_num_members-1] = descendant_id;
		}
		return 1;
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

	struct object const_use_values[ctx->num_uses];

	for (ast_use_id use_i = 0; use_i < ctx->num_uses; use_i++) {
		struct ast_dt_use *use;
		use = get_use(ctx, use_i);

		if (use->is_const) {
			const_use_values[use_i] = use->const_value;
		} else {
			const_use_values[use_i] = OBJ_UNSET;
		}
	}

	struct bc_env *bc_env;
	bc_env = ast_composite_bind_gen_bytecode(
				ctx->ast_ctx, ctx->mod,
				dep_members, dep_member_types,
				dep_member_const, num_dep_members,
				const_use_values, ctx->num_uses,
				ctx->closures, ctx->num_closures, node);
	if (!bc_env) {
		return FUNC_UNSET;
	}

	struct func func = {0};
	func.type = stg_register_func_type(ctx->mod,
			node->type, dep_member_types, num_dep_members);

	func.kind = FUNC_BYTECODE;
	func.bytecode = bc_env;

	func_id fid;
	fid = stg_register_func(ctx->mod, func);

	*out_num_dep_members = num_dep_members;
	*out_dep_members = dep_members;

	return fid;
}

static int
ast_dt_expr_typecheck(struct ast_dt_context *ctx, struct ast_node *node,
		enum ast_name_dep_requirement dep_req, type_id expected_type)
{
	struct ast_name_dep *deps = NULL;
	size_t num_deps = 0;

	int err;
	err = ast_node_find_named_dependencies(
			node, dep_req, &deps, &num_deps);
	if (err) {
		printf("Failed to find the named dependencies.\n");
		return -1;
	}

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
				} else {
					body_deps[i].ref = deps[i].ref;
					body_deps[i].req = deps[i].req;
					body_deps[i].value = -1;
					body_deps[i].lookup_failed = true;
					body_deps[i].determined = false;
				}
				break;

			case AST_NAME_REF_USE:
				{
					struct ast_dt_use *use;
					use = get_use(ctx, deps[i].ref.use.id);

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

					body_deps[i].ref = deps[i].ref;
					body_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;
					body_deps[i].val = obj;
					body_deps[i].value = -1;
					body_deps[i].lookup_failed = false;
					body_deps[i].determined = true;
				}
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

	err = ast_node_typecheck(
			ctx->ast_ctx, ctx->mod, node,
			body_deps, num_deps, expected_type);
	if (err) {
		return -1;
	}

	return 0;
}

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

		case AST_DT_JOB_MBR_TYPE_RESOLVE_NAMES:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->mod->native_mod,
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

		case AST_DT_JOB_EXPR_RESOLVE_NAMES:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, ctx->mod->native_mod,
						NULL, false, ctx->root_node,
						expr->value.node,
						ctx->local_member_ids);
				if (err) {
					printf("Failed to resolve names.\n");
					break;
				}

				ast_dt_find_named_dependencies(
						ctx, expr->value_jobs.resolve_types,
						AST_NAME_DEP_REQUIRE_TYPE, expr->value.node);
			}
			return 0;

		case AST_DT_JOB_MBR_TYPE_RESOLVE_TYPES:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);

				int err;
				err = ast_dt_expr_typecheck(
						ctx, mbr->type_node,
						AST_NAME_DEP_REQUIRE_VALUE,
						ctx->ast_ctx->types.type);
				if (err) {
					return -1;
				}
			}
			return 0;

		case AST_DT_JOB_EXPR_RESOLVE_TYPES:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);

				int err;
				err = ast_dt_expr_typecheck(
						ctx, expr->value.node,
						AST_NAME_DEP_REQUIRE_TYPE,
						expr->type);
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
				if (fid == FUNC_UNSET) {
					free(dep_members);
					return -1;
				}

				struct object const_member_values[num_dep_members];
				for (size_t i = 0; i < num_dep_members; i++) {
					struct ast_dt_member *dep_mbr;
					dep_mbr = get_member(ctx, dep_members[i]);

					assert((dep_mbr->flags & AST_DT_MEMBER_IS_CONST) != 0);

					const_member_values[i] = dep_mbr->const_value;
				}

				free(dep_members);

				type_id out_type = TYPE_UNSET;
				struct object out = {0};
				out.type = ctx->ast_ctx->types.type;
				out.data = &out_type;

				int err;
				err = vm_call_func(
						ctx->ast_ctx->vm, NULL, fid,
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

				struct type *type;
				type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

				if (type->obj_inst) {
					struct object_cons *cons;
					cons = type->obj_inst->cons;
					bool all_bound = true;

					int local_members_offset[cons->num_params];
					object_cons_local_descendent_ids(
							ctx->ast_ctx->vm, cons,
							local_members_offset);

					void *const_member_values[cons->num_params];

					ast_member_id first_child;
					if ((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
						first_child = mbr->first_child;
					} else {
						first_child = job->member + 1;
					}

					for (size_t i = 0; i < cons->num_params; i++) {
						ast_member_id desc_id;
						// Subtract one to compansate for the parent having ID
						// 0 in local_members_offset
						desc_id = first_child +
							local_members_offset[i] -1;

						struct ast_dt_member *desc;
						desc = get_member(ctx, desc_id);

						if ((desc->flags & AST_DT_MEMBER_IS_CONST) == 0) {
							all_bound = false;
							break;
						}

						const_member_values[i] = desc->const_value.data;
					}

					if (!all_bound) {
						return 0;
					}

					int err;
					type_id res_type;
					err = object_ct_pack_type(
							ctx->ast_ctx, ctx->mod,
							cons, const_member_values, cons->num_params,
							&res_type);
					if (err) {
						return -1;
					}
					assert_type_equals(ctx->ast_ctx->vm,
							res_type, mbr->type);

					struct object res = {0};
					uint8_t buffer[type->size];
					res.type = mbr->type;
					res.data = buffer;

					err = object_ct_pack(
							ctx->ast_ctx, ctx->mod,
							cons, const_member_values, cons->num_params,
							&res);
					if (err) {
						return -1;
					}

					mbr->const_value = register_object(
							ctx->ast_ctx->vm, &ctx->mod->store, res);
					mbr->flags |= AST_DT_MEMBER_IS_CONST;
				} else {
					if (mbr->bound < 0) {
						return 0;
					}

					struct ast_dt_bind *bind;
					bind = get_bind(ctx, mbr->bound);

					if (bind->overridable) {
						return 0;
					}

					struct ast_dt_expr *expr;
					expr = get_expr(ctx, bind->expr);

					struct object const_value;
					int err;
					err = ast_dt_try_eval_expr_const(
							ctx, bind->expr, &const_value);
					if (err) {
						return err < 0;
					}

					mbr->const_value.type = mbr->type;

					struct type *mbr_type;
					mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->const_value.type);

					uint8_t buffer[mbr_type->size];
					mbr->const_value.data = buffer;

					err = object_unpack(
							ctx->ast_ctx->vm, const_value,
							mbr->bound_unpack_id, &mbr->const_value);

					mbr->const_value =
						register_object(ctx->ast_ctx->vm,
								&ctx->mod->store, mbr->const_value);

					assert_type_equals(ctx->ast_ctx->vm,
							mbr->type, mbr->const_value.type);

					mbr->flags |= AST_DT_MEMBER_IS_CONST;
				}
			}
			return 0;

		case AST_DT_JOB_EXPR_CODEGEN:
			{
				struct ast_dt_expr *expr;
				expr = get_expr(ctx, job->expr);

				ast_member_id *dep_members = NULL;
				size_t num_dep_members = 0;

				func_id fid;
				fid = ast_dt_expr_codegen(
						ctx, expr->value.node,
						AST_NAME_DEP_REQUIRE_TYPE,
						&dep_members, &num_dep_members);
				if (fid == FUNC_UNSET) {
					return -1;
				}

				expr->num_member_deps = num_dep_members;
				expr->member_deps = dep_members;

				expr->value.func = fid;
			}
			return 0;

		case AST_DT_JOB_BIND_TARGET_RESOLVE_NAMES:
			{
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, job->bind);

				int err;
				err = ast_composite_node_resolve_names(
						ctx->ast_ctx, NULL, NULL,
						false, ctx->root_node, bind->target_node,
						ctx->local_member_ids);
				if (err) {
					return -1;
				}

				ast_member_id target;
				err = ast_dt_l_expr_member(
						ctx, bind->target_node, &target);
				if (err) {
					return -1;
				}

				struct ast_dt_member *mbr;
				mbr = get_member(ctx, target);

				ast_dt_job_dependency(ctx,
						mbr->type_resolved,
						bind->target_jobs.resolve);

				if (mbr->typegiving_bind == job->bind) {
					bind->target = target;
				}

				// We place this dependency to ensure the member can get
				// all its dependencies from bind target resolve before its
				// const resolved is dispatched.
				ast_dt_job_dependency(ctx,
						bind->target_jobs.resolve,
						mbr->const_resolved);
			}
			return 0;

		case AST_DT_JOB_BIND_TARGET_RESOLVE:
			{
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, job->bind);

				struct ast_dt_expr *expr;
				expr = get_expr(ctx, bind->expr);

				if (bind->target >= 0) {
					struct ast_dt_member *mbr;
					mbr = get_member(ctx, bind->target);

					assert(mbr->typegiving_bind == job->bind);
					assert(expr->type != TYPE_UNSET);

					ast_try_set_local_member_type(ctx,
							bind->target, expr->type);

				} else {
					// We run resolve_l_bind here because at this point all types
					// of the targets should have been resolved.
					ast_member_id target;
					target = ast_dt_resolve_l_expr(
							ctx, bind->target_node);

					if (target < 0) {
						return -1;
					}

					bind->target = target;

					size_t num_descendants;
					num_descendants = ast_dt_member_num_descendants(
							ctx, bind->target);

					for (size_t i = 0; i < num_descendants; i++) {
						ast_member_id desc_id;
						desc_id = ast_dt_member_get_descendant(
								ctx, bind->target, i);
						ast_dt_bind_to_member(
								ctx, desc_id, job->bind, i, false);

						struct ast_dt_member *desc;
						desc = get_member(ctx, desc_id);

						ast_dt_job_dependency(ctx,
								expr->value_jobs.codegen,
								desc->const_resolved);
					}
				}
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
					printf("not constant %i\n", err);
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
		case AST_DT_JOB_BIND_TARGET_RESOLVE:
			{
				struct ast_dt_bind *bind;
				bind = get_bind(ctx, job->expr);
				assert(bind->target_jobs.resolve == job_id);
				bind->target_jobs.resolve = -1;
				// TODO: Remove the job from any target having this bind as
				// typegiving.
				//
				// if (bind->is_type_giving) {
				// 	struct ast_dt_member *mbr;
				// 	assert(bind->num_explicit_targets == 1);
				// 	mbr = get_member(ctx, bind->explicit_targets[0]);

				// 	assert(mbr->type_resolved == job_id);
				// 	mbr->type_resolved = -1;
				// }
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
ast_dt_calculate_persistant_id(struct ast_dt_context *ctx, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr = get_member(ctx, mbr_id);

	if ((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
		// Members can not have persistant ID 0 as that ID is reserved for the
		// top-level object.
		assert(mbr->persistant_id > 0 &&
				mbr->persistant_id < ctx->members.length + 1);
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

		assert(result > 0 && result < ctx->members.length + 1);

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
ast_dt_composite_make_type(struct ast_dt_context *ctx, struct stg_module *mod)
{
	struct ast_node *comp = ctx->root_node;

	struct object_cons *def;
	def = calloc(1, sizeof(struct object_cons));
	def->pack      = ast_dt_pack_func;
	def->pack_type = ast_dt_pack_type_func;
	def->unpack    = ast_dt_unpack_func;

	struct ast_dt_local_member *local_members;
	local_members = calloc(comp->composite.num_members,
			sizeof(struct ast_dt_local_member));

	struct object_cons_param *params;
	params = calloc(comp->composite.num_members,
			sizeof(struct object_cons_param));

	size_t offset = 0;
	ast_member_id cumulative_persistant_id = 1;
	for (size_t i = 0; i < comp->composite.num_members; i++) {
		local_members[i].name = comp->composite.members[i].name;
		local_members[i].location = offset;

		ast_member_id mbr_id;
		mbr_id = ctx->local_member_ids[i];

		struct ast_dt_member *mbr;
		mbr = get_member(ctx, mbr_id);

		params[i].name = comp->composite.members[i].name;
		params[i].type = mbr->type;
		params[i].def_loc = mbr->decl_loc;

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

	size_t num_bound_members = 0;
	for (ast_member_id mbr = 0; mbr < ctx->members.length; mbr++) {
		if (get_member(ctx, mbr)->bound >= 0) {
			num_bound_members += 1;
		}
	}
	struct object_inst_bind *binds;
	binds = calloc(num_bound_members,
			sizeof(struct object_inst_bind));

	struct object_inst_expr *exprs;
	exprs = calloc(ctx->exprs.length,
			sizeof(struct object_inst_expr));

	struct object_inst *inst;
	inst = calloc(1, sizeof(struct object_inst));

	for (ast_dt_expr_id expr_i = 0;
			expr_i < ctx->exprs.length; expr_i++) {
		struct ast_dt_expr *expr;
		expr = get_expr(ctx, expr_i);

		if (expr->constant) {
			exprs[expr_i].constant = true;
			exprs[expr_i].const_value = expr->const_value;
		} else {
			assert(expr->value.func != FUNC_UNSET);
			exprs[expr_i].constant = false;
			exprs[expr_i].func = expr->value.func;
		}

		exprs[expr_i].num_deps = expr->num_member_deps;
		exprs[expr_i].deps = calloc(exprs[expr_i].num_deps,
				sizeof(size_t));

		for (size_t i = 0; i < expr->num_member_deps; i++) {
			exprs[expr_i].deps[i] =
				ast_dt_calculate_persistant_id(
						ctx, expr->member_deps[i]);
		}

		exprs[expr_i].loc = expr->loc;
	}

	inst->exprs = exprs;
	inst->num_exprs = ctx->exprs.length;

	size_t bind_i = 0;
	for (size_t mbr_i = 0; mbr_i < ctx->members.length; mbr_i++) {
		struct ast_dt_member *mbr = get_member(ctx, mbr_i);

		if (mbr->bound < 0) {
			continue;
		}

		struct ast_dt_bind *bind;
		bind = get_bind(ctx, mbr->bound);

		binds[bind_i].target_id =
			ast_dt_calculate_persistant_id(
					ctx, mbr_i);
		binds[bind_i].loc = bind->loc;
		binds[bind_i].expr_id = bind->expr;
		binds[bind_i].unpack_id = mbr->bound_unpack_id;
		binds[bind_i].overridable = bind->overridable;

		bind_i += 1;
	}

	assert(bind_i == num_bound_members);

	inst->binds = binds;
	inst->num_binds = num_bound_members;
	inst->cons = def;

	struct ast_dt_composite_info *info;
	info = calloc(1, sizeof(struct ast_dt_composite_info));

	info->num_members = comp->composite.num_members;
	info->members = local_members;

	def->data = info;

	struct type dt_type = {0};
	dt_type.base = &ast_dt_composite_base;
	dt_type.obj_inst = inst;
	dt_type.size = offset;
	dt_type.data = info;

	type_id result;
	result = stg_register_type(mod, dt_type);

	info->type = result;
	inst->type = result;

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
	dt_ctx.root_node = comp;
	dt_ctx.terminal_jobs  = -1;

	dt_ctx.closures = closures;
	dt_ctx.num_closures  = num_closures;

	paged_list_init(
			&dt_ctx.jobs,
			&ctx->vm->mem,
			sizeof(struct ast_dt_expr));

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

	dt_ctx.target_names_resolved =
		ast_dt_job_nop(&dt_ctx, &dt_ctx.target_names_resolved);
	dt_ctx.use_resolved =
		ast_dt_job_nop(&dt_ctx, &dt_ctx.use_resolved);

	ast_dt_composite_populate(&dt_ctx, comp);

	err = ast_dt_run_jobs(&dt_ctx);
#if AST_DT_DEBUG_JOBS
	if (err) {
		printf("One or more jobs failed when resolving datastructure.\n");
	}
#endif

	type_id result = TYPE_UNSET;

	if (!err && dt_ctx.num_errors == 0) {
		result = ast_dt_composite_make_type(&dt_ctx, mod);
	}

	free(dt_ctx.local_member_ids);

	for (size_t expr_i = 0; expr_i < dt_ctx.exprs.length; expr_i++) {
		struct ast_dt_expr *expr;
		expr = get_expr(&dt_ctx, expr_i);
		free(expr->member_deps);
	}

	paged_list_destroy(&dt_ctx.jobs);
	paged_list_destroy(&dt_ctx.exprs);
	paged_list_destroy(&dt_ctx.binds);
	paged_list_destroy(&dt_ctx.members);

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
	struct ast_node *scope;

	scope = ast_init_node_composite(
			ctx, AST_NODE_NEW, info->loc);

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
			closure = calloc(1, sizeof(struct ast_dt_variant_cons_closure));
			closure->tag = tag;
			closure->info = info;

			func.kind = FUNC_CONS;
			func.cons = calloc(1, sizeof(struct object_cons));
			func.cons->num_params = 1;
			func.cons->params = calloc(1, sizeof(struct object_cons_param));
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
		struct ast_typecheck_closure *closure_values, size_t num_closures)
{
	bool ok = true;
	size_t max_data_size = 0;

	struct stg_type_variant_option *opts;
	opts = calloc(num_options, sizeof(struct stg_type_variant_option));

	struct ast_typecheck_dep body_deps[num_closures];

	for (size_t i = 0; i < num_closures; i++) {
		assert(closure_values[i].req == AST_NAME_DEP_REQUIRE_VALUE);

		body_deps[i].ref.kind = AST_NAME_REF_CLOSURE;
		body_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;
		body_deps[i].ref.closure = i;
		body_deps[i].lookup_failed = closure_values[i].lookup_failed;
		body_deps[i].determined = true;
		body_deps[i].val = closure_values[i].value;
	}

	for (size_t i = 0; i < num_options; i++) {
		opts[i].name = options[i].name;
		opts[i].data_type = TYPE_UNSET;
		opts[i].loc = options[i].loc;

		if (options[i].data_type) {
			int err;
			err = ast_node_typecheck(
					ctx, mod,
					options[i].data_type,
					body_deps, num_closures,
					ctx->types.type);
			if (err) {
				ok = false;
				continue;
			}

			assert_type_equals(ctx->vm,
					options[i].data_type->type, ctx->types.type);

			struct bc_env *type_expr_bc;
			type_expr_bc = ast_type_expr_gen_bytecode(
					ctx, mod, options[i].data_type,
					closure_values, num_closures);

			if (!type_expr_bc) {
				printf("Failed to generate bytecode for variant data type expression.\n");
				return TYPE_UNSET;
			}

			type_id out_type = TYPE_UNSET;

			struct stg_exec exec_ctx = {0};
			mod_arena(mod, &exec_ctx.heap);
			nbc_exec(ctx->vm, &exec_ctx, type_expr_bc->nbc,
					NULL, 0, NULL, &out_type);
			arena_destroy(&exec_ctx.heap);

			assert(out_type != TYPE_UNSET);

			opts[i].data_type = out_type;

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
	info = calloc(1, sizeof(struct stg_type_variant_info));

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

	struct type new_type = {0};

	new_type.name = mod_atoms(mod, "Variant");
	new_type.base = &variant_type_base;
	new_type.size = tag_size + max_data_size;
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
