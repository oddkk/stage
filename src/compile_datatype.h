#ifndef STAGE_COMPILE_DATATYPE_H
#define STAGE_COMPILE_DATATYPE_H

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

	ast_init_expr_id init_expr;

	struct {
		struct ast_node *node;
		func_id func;
	} value;

	type_id type;

	struct atom *trivial_name;

	struct stg_location loc;

	struct ast_dt_expr_dep *deps;
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
	// The unpack id refers to what descendent id of the target should be
	// bound.
	ssize_t unpack_id;
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

struct ast_dt_type_class_impl {
	ast_dt_composite_id parent;
	struct ast_datatype_impl *impl;

	ast_dt_tc_id tc;

	struct object *arg_values;

	ast_dt_job_id resolve_names;
	ast_dt_job_id resolve_target;
	ast_dt_job_id resolve;
};

struct ast_dt_composite_ambigous_name {
	struct atom *name;

	ast_dt_job_id type_ready;
	ast_dt_job_id value_ready;

	ast_dt_expr_id expr_id;
	ssize_t unpack_id;
};

struct ast_dt_composite {
	struct ast_node *root_node;
	// An array of the ids of all local members, in the same order as
	// root_node->composite.members. The lenght is root_node->composite.num_members.
	ast_member_id *local_member_ids;

	ast_member_id self;

	struct ast_dt_use *uses;
	size_t num_uses;

	// struct ast_typecheck_closure *closures;
	// size_t num_closures;

	struct ast_dt_composite_ambigous_name *closures;
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

struct ast_dt_type_class {
	struct stg_type_class *tc;

	// The type class instance is ready to be used.
	ast_dt_job_id type_class_ready;
};

struct ast_dt_context;

void
cpl_dt_init_context(struct ast_dt_context *);
void
cpl_dt_destroy_context(struct ast_dt_context *);

ast_dt_composite_id
ast_dt_register_composite(struct ast_dt_context *ctx, struct ast_node *root_node);

#endif
