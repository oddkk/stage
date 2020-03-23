#ifndef STG_TYPE_CLASS_H
#define STG_TYPE_CLASS_H

#include "module.h"
#include "objstore.h"

struct ast_node;
struct ast_context;
struct ast_typecheck_dep;

struct stg_type_class_impl {
	struct object *params;
	struct object inst;
};

struct stg_type_class_param {
	struct atom *name;
	type_id type;
	struct stg_location decl_loc;
};

struct stg_type_class_member {
	struct atom *name;
	// type_id type;
	struct ast_node *type;
};

struct stg_type_class {
	struct stg_type_class_param *params;
	size_t num_params;

	struct stg_type_class_member *members;
	size_t num_members;

	struct stg_type_class_impl *impls;
	size_t num_impls;

	struct ast_typecheck_dep *deps;
	size_t num_deps;

	struct object_cons *cons;

	// TODO: Constraints
};

struct stg_type_class *
stg_type_class_create(struct stg_module *,
		struct stg_type_class_param *params, size_t num_params,
		struct stg_type_class_member *members, size_t num_members);

struct stg_type_class *
stg_type_class_from_ast_node(struct ast_context *,
		struct stg_module *, struct ast_node *type_class_node,
		struct ast_typecheck_dep *deps, size_t num_deps);

int
stg_type_class_impl(struct stg_module *, struct stg_type_class *,
		struct object *params, size_t num_params,
		struct object value);

#endif
