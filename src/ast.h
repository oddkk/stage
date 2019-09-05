#ifndef STAGE_AST_H
#define STAGE_AST_H

#include "errors.h"
#include "objstore.h"

typedef int32_t ast_slot_id;
#define AST_SLOT_TYPE ((ast_slot_id)-1)
#define AST_SLOT_DC ((ast_slot_id)-2)
#define AST_SLOT_NOT_FOUND ((ast_slot_id)-3)

#define AST_BIND_NEW ((ast_slot_id)-4)
#define AST_BIND_FAILED ((ast_slot_id)-5)


struct ast_object_def;

struct ast_object_arg {
	struct atom *name;
	ast_slot_id slot;
};

struct ast_object {
	struct ast_object_def *def;
	struct ast_object_arg *args;
	size_t num_present_args;
};

struct ast_array {
	ast_slot_id member_type;
	ast_slot_id member_count;
	ast_slot_id *members;
	size_t num_present_members;
	size_t num_members;
};

ssize_t
ast_object_lookup_arg(struct ast_object *obj, struct atom *arg_name);

struct ast_scope_name {
	struct atom *name;
	ast_slot_id slot;
};

struct ast_env;
struct ast_namespace;

struct ast_scope {
	// Used for multiple scopes within a single expression.
	struct ast_scope *parent;
	// Used for resolving closures from parent functions.
	struct ast_scope *parent_func;
	// Used for looking up objects from outside the expression.
	struct ast_scope *parent_ns;

	ast_slot_id object;
	struct ast_scope_name *names;
	size_t num_names;
};

int
ast_scope_insert(struct ast_scope *,
		struct atom *name, ast_slot_id);

enum ast_slot_kind {
	AST_SLOT_ERROR,

	AST_SLOT_WILDCARD,
	AST_SLOT_CONST_TYPE,
	AST_SLOT_CONST,
	AST_SLOT_PARAM,
	AST_SLOT_TEMPL,
	AST_SLOT_FREE,
	AST_SLOT_CONS,
	AST_SLOT_CONS_ARRAY,

	AST_SLOT_SUBST,
};

const char *
ast_slot_name(enum ast_slot_kind kind);

struct ast_env_slot {
	struct atom *name;
	ast_slot_id type;

	enum ast_slot_kind kind;
	union {
		type_id const_type;
		struct object const_object;
		int64_t param_index;
		struct ast_object cons;
		struct ast_array cons_array;

		ast_slot_id subst;
	};
};

struct ast_env {
	struct ast_env_slot *slots;
	size_t num_slots;

	struct objstore *store;
};

struct vm;

struct ast_context {
	struct stg_error_context *err;

	struct {
		struct atom *type;

		struct atom *func_cons_arg_ret;
		struct atom *func_cons_arg_params;

		struct atom *array_cons_arg_type;
		struct atom *array_cons_arg_count;
	} atoms;

	struct {
		type_id type;
		type_id integer;
	} types;

	struct {
		struct ast_object_def *func;
		struct ast_object_def *array;
	} cons;

	// TODO: Get rid of dependency on vm?
	struct vm *vm;
};

struct ast_context
ast_init_context(struct stg_error_context *, struct atom_table *, struct vm *, type_id type, type_id integer);

ast_slot_id
ast_bind_slot_wildcard(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, ast_slot_id type);

ast_slot_id
ast_bind_slot_const(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, struct object);

ast_slot_id
ast_bind_slot_const_type(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, type_id);

ast_slot_id
ast_bind_slot_param(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, int64_t param_index, ast_slot_id type);

ast_slot_id
ast_bind_slot_templ(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type);

ast_slot_id
ast_bind_slot_free(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, ast_slot_id type);

ast_slot_id
ast_bind_slot_cons(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, struct ast_object_def *);

ast_slot_id
ast_bind_slot_cons_array(struct ast_context *,
		struct ast_env *, ast_slot_id target, struct atom *name,
		ast_slot_id *members, size_t num_members, ast_slot_id member_type);

ast_slot_id
ast_unpack_arg_named(struct ast_context *,
		struct ast_env *, ast_slot_id obj, struct atom *name);

ast_slot_id
ast_union_slot(struct ast_context *, struct ast_env *,
		ast_slot_id target, ast_slot_id src_slot);

ast_slot_id
ast_copy_slot(struct ast_context *,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot);

void
ast_substitute(struct ast_context *, struct ast_env *,
		ast_slot_id new_slot, ast_slot_id target);

ast_slot_id
ast_env_lookup(struct ast_env *, struct atom *name);

ast_slot_id
ast_env_lookup_or_alloc_free(struct ast_context *,
		struct ast_env *, struct atom *name, ast_slot_id type);

struct ast_env_slot
ast_env_slot(struct ast_context *, struct ast_env *, ast_slot_id);

void
ast_env_print(struct vm *vm, struct ast_env *);

struct ast_object_def_param {
	struct atom *name;
	ast_slot_id type;
};

struct ast_object_def {
	struct ast_object_def_param *params;
	size_t num_params;
	struct ast_env env;

	ast_slot_id  ret_type;
};

struct ast_object_def *
ast_object_def_register(struct objstore *);

void
ast_object_def_finalize(struct ast_object_def *,
		struct ast_object_def_param *params, size_t num_params,
		ast_slot_id ret_type);

enum ast_node_kind {
	AST_NODE_FUNC,
	AST_NODE_CALL,
	AST_NODE_SLOT,
	AST_NODE_LOOKUP,

	AST_NODE_FUNC_UNINIT,
};

struct ast_node;

struct ast_func_arg {
	struct atom *name;
	struct ast_node *value;
};

struct ast_func_param {
	struct atom *name;
	struct ast_node *type;
	ast_slot_id slot;
};

struct ast_node {
	enum ast_node_kind kind;
	struct stg_location loc;

	union {
		struct {
			struct ast_node *body;

			struct ast_func_param *params;
			size_t num_params;

			struct ast_node *return_type;
			ast_slot_id return_type_slot;

			ast_slot_id type;
		} func;

		struct {
			struct ast_node *func;

			struct ast_func_arg *args;
			size_t num_args;
		} call;

		ast_slot_id slot;

		struct {
			struct atom *name;
			ast_slot_id slot;
		} lookup;
	};
};

ast_slot_id
ast_node_resolve_slot(struct ast_env *env, ast_slot_id *slot);

ast_slot_id
ast_node_type(struct ast_context *, struct ast_env *, struct ast_node *);

ast_slot_id
ast_node_value(struct ast_context *, struct ast_env *, struct ast_node *);

#define AST_NODE_NEW ((struct ast_node *)1)

// func nodes have to be initialized in two steps, init and finalize, because
// the function's params, return type, and body depends on its env. Init
// initializes the env, and finalize binds the params, return type, and body to
// the function.
struct ast_node *
ast_init_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct atom **param_names, size_t num_params);

struct ast_node *
ast_finalize_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct ast_node **params, size_t num_params,
		struct ast_node *return_type, struct ast_node *body);

struct ast_node *
ast_init_node_call(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args);

struct ast_node *
ast_init_node_slot(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		ast_slot_id slot);

struct ast_node *
ast_init_node_lookup(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct atom *name, ast_slot_id slot);

void
ast_node_substitute_slot(struct ast_node *,
		ast_slot_id target, ast_slot_id new_slot);

void
ast_print(struct ast_context *, struct ast_env *, struct ast_node *);

enum ast_module_name_kind {
	AST_MODULE_NAME_DECL,
	AST_MODULE_NAME_NAMESPACE,
};

struct ast_namespace;

struct ast_module_name {
	enum ast_module_name_kind kind;
	struct atom *name;
	union {
		struct {
			struct ast_node *expr;
			ast_slot_id value;
		} decl;
		struct ast_namespace *ns;
	};
};

struct ast_namespace {
	struct atom *name;
	struct ast_namespace *parent;

	struct ast_module_name *names;
	size_t num_names;

	struct ast_object_def def;
};

struct ast_module {
	struct ast_env env;

	struct ast_namespace root;
};

int
ast_namespace_add_decl(struct ast_context *, struct ast_module *,
		struct ast_namespace *,
		struct atom *name, struct ast_node *expr);

struct ast_namespace *
ast_namespace_add_ns(struct ast_namespace *,
		struct atom *name);

ast_slot_id
ast_module_finalize(struct ast_context *, struct ast_module *);

void
ast_print_module(struct ast_context *, struct ast_module *);

#endif
