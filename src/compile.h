#ifndef STAGE_CONFIG_H
#define STAGE_CONFIG_H

#include "vm.h"
#include "ast.h"
#include "module.h"
#include "errors.h"

enum complie_job_type {
#define COMPILE_JOB(name, data) COMPILE_JOB_##name,
	#include "compile_job_defs.h"
#undef COMPILE_JOB
	COMPILE_JOBS_LEN
};

extern struct string complie_job_names[COMPILE_JOBS_LEN];

int stg_compile(struct vm *vm, struct ast_context *);

typedef int ast_dt_job_id;
typedef int ast_dt_bind_id;
typedef int ast_dt_expr_id;
typedef int ast_dt_composite_id;
typedef int ast_dt_tc_id;
typedef int ast_dt_tc_impl_id;
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
	struct paged_list type_classes;
	struct paged_list type_class_impls;

	struct ast_dt_init_expr *init_exprs;
	size_t num_init_exprs;

	ast_dt_job_id impl_targets_resolved;

	struct ast_context *ast_ctx;
	struct stg_module  *mod;

	size_t num_errors;

#if AST_DT_DEBUG_JOBS
	// A run # that allows us to see more clearly what struct each job belongs
	// to when debugging jobs.
	size_t run_i;
#endif
};

#define AST_DT_JOBS                                     \
	JOB(nop, ast_dt_job_id *)                           \
	                                                    \
	JOB(composite_resolve_names, ast_dt_composite_id)   \
	JOB(composite_eval_closure, ast_dt_composite_id)    \
	JOB(composite_pack, ast_dt_composite_id)            \
	JOB(composite_const_eval, ast_dt_composite_id)      \
	                                                    \
	JOB(mbr_type_resolve_names, ast_member_id)          \
	JOB(mbr_type_eval, ast_member_id)                   \
	JOB(mbr_const_eval, ast_member_id)                  \
	                                                    \
	JOB(expr_resolve_names, ast_dt_expr_id)             \
	JOB(expr_resolve_types, ast_dt_expr_id)             \
	JOB(expr_codegen, ast_dt_expr_id)                   \
	                                                    \
	JOB(bind_target_resolve_names, ast_dt_bind_id)      \
	                                                    \
	JOB(tc_impl_resolve_names, ast_dt_tc_impl_id)       \
	JOB(tc_impl_resolve_target, ast_dt_tc_impl_id)      \
	JOB(tc_impl_resolve, ast_dt_tc_impl_id)             \
	                                                    \
	JOB(use_const_eval, ast_dt_use_id)

#define JOB(name, type)								\
	ast_dt_job_id									\
	ast_dt_job_##name(struct ast_dt_context *ctx,	\
			type value);
AST_DT_JOBS
#undef JOB

// Requests that from must be evaluated before to.
void
ast_dt_job_dependency(struct ast_dt_context *ctx,
		ast_dt_job_id from_id, ast_dt_job_id to_id);

int
ast_dt_process(struct ast_context *ctx, struct stg_module *mod);

#endif
