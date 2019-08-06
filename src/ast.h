#ifndef STAGE_AST_H
#define STAGE_AST_H

#include "errors.h"
#include "objstore.h"

typedef int32_t ast_slot_id;
#define AST_SLOT_NOT_FOUND ((ast_slot_id)-2)
#define AST_SLOT_TYPE ((ast_slot_id)-1)

struct ast_object_def;

struct ast_object_arg {
	struct atom *name;
	ast_slot_id slot;
};

struct ast_object {
	struct ast_object_def *def;
	ast_slot_id *args;
};

ssize_t
ast_object_lookup_arg(struct ast_object *obj, struct atom *arg_name);

struct ast_name {
	struct atom *name;
	ast_slot_id slot;
};

struct ast_env;

struct ast_scope {
	struct ast_env *ctx;
	struct ast_scope *parent;

	struct ast_name *names;
	size_t num_names;
};

int
ast_scope_insert(struct ast_scope *,
		struct atom *name, ast_slot_id);

enum ast_slot_kind {
	AST_SLOT_WILDCARD,
	AST_SLOT_CONST_TYPE,
	AST_SLOT_CONST,
	AST_SLOT_PARAM,
	AST_SLOT_TEMPL,
	AST_SLOT_FREE,
	AST_SLOT_CONS,
};

struct ast_env_slot {
	struct atom *name;
	ast_slot_id type;

	enum ast_slot_kind kind;
	union {
		type_id const_type;
		struct object const_object;
		int64_t param_index;
		struct ast_object cons;
	};
};

struct ast_env {
	struct ast_scope *parent_scope;
	struct ast_scope *scope;

	struct ast_env_slot *slots;
	size_t num_slots;
};

struct ast_context {
	struct stg_error_context *err;

	struct {
		struct atom *type;
	} atoms;

	struct {
		type_id type;
	} types;
};

#define AST_BIND_NEW ((ast_slot_id)-2)
#define AST_BIND_FAILED ((ast_slot_id)-2)

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
		struct atom *name, struct ast_object_def *,
		struct ast_object_arg *args, size_t num_args);

ast_slot_id
ast_cons_arg_slot(struct ast_env *, ast_slot_id,
		struct atom *arg_name);

ast_slot_id
ast_union_slot(struct ast_context *,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot);

void
ast_substitute(struct ast_context *, struct ast_env *,
		ast_slot_id target, ast_slot_id new_slot);

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


enum ast_node_kind {
	AST_NODE_FUNC,
	AST_NODE_CALL,
	AST_NODE_SLOT,
};

struct ast_node;

struct ast_func_arg {
	struct atom *name;
	struct ast_node *value;
};

struct ast_func_param {
	struct atom *name;
	struct ast_node *type;
};

struct ast_node {
	enum ast_node_kind kind;
	struct stg_location loc;

	ast_slot_id type;

	union {
		struct {
			struct ast_env env;

			struct ast_node *body;

			struct ast_func_param *params;
			size_t num_params;

			struct ast_node *return_type;
		} func;

		struct {
			struct ast_node *func;

			struct ast_func_arg *args;
			size_t num_args;
		} call;

		ast_slot_id slot;
	};
};

struct ast_node *
ast_init_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *, struct stg_location,
		struct ast_func_param *params, size_t num_params,
		struct ast_node *return_type, struct ast_node *body);

struct ast_node *
ast_init_node_call(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *, struct stg_location,
		struct ast_func_arg *args, size_t num_args);

struct ast_node *
ast_init_node_slot(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *, struct stg_location,
		ast_slot_id slot);

void
ast_node_substitute_slot(struct ast_node *,
		ast_slot_id target, ast_slot_id new_slot);

void
ast_print(struct ast_env *, struct ast_node *);

#endif
