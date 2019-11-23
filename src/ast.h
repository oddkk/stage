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

typedef int ast_member_id;
typedef int ast_param_id;
typedef int ast_closure_id;

enum ast_name_ref_kind {
	AST_NAME_REF_NOT_FOUND = 0,
	AST_NAME_REF_MEMBER,
	AST_NAME_REF_PARAM,
	AST_NAME_REF_CLOSURE,
};

struct ast_name_ref {
	enum ast_name_ref_kind kind;
	union {
		ast_member_id  member;
		ast_param_id   param;
		ast_closure_id closure;
	};
};

// Defined in ast_typecheck.c
bool
ast_name_ref_equals(struct ast_name_ref lhs, struct ast_name_ref rhs);

void
ast_print_name_ref(struct ast_name_ref);

struct ast_node;
struct ast_module;
struct ast_object_def;

struct ast_object_arg {
	struct atom *name;
	ast_slot_id slot;
};

struct ast_object_bind {
	ast_slot_id target;
	ast_slot_id value;
};

struct ast_object {
	struct ast_object_def *def;
	struct ast_object_arg *args;
	size_t num_present_args;

	// def_bind is a list of (target, value)-pairs. The list an unrolled
	// version of the value params of the binds of the def. That means each
	// target in def will appear num_value_params consecutive times, once for
	// each value_param, in the same order as in def.
	struct ast_object_bind *def_binds;
};

struct ast_array {
	ast_slot_id member_type;
	ast_slot_id member_count;
	ast_slot_id *members;
	size_t num_members;
};

ssize_t
ast_object_lookup_arg(struct ast_object *obj, struct atom *arg_name);

struct ast_scope_name {
	struct atom *name;
	struct ast_name_ref ref;
};

struct ast_env;

enum ast_scope_parent_kind {
	AST_SCOPE_PARENT_LOCAL = 0,
	AST_SCOPE_PARENT_CLOSURE,
};

struct ast_scope {
	struct ast_scope *parent;
	enum ast_scope_parent_kind parent_kind;

	struct ast_node *closure_target;

	struct ast_scope_name *names;
	size_t num_names;
};

void
ast_scope_push_composite(struct ast_scope *target, struct ast_scope *parent);

void
ast_scope_push_expr(struct ast_scope *target, struct ast_scope *parent);

void
ast_scope_push_func(struct ast_scope *target, struct ast_scope *parent);

int
ast_scope_insert(struct ast_scope *,
		struct atom *name, ast_slot_id);

enum ast_slot_kind {
	AST_SLOT_ERROR,

	AST_SLOT_WILDCARD,
	AST_SLOT_CONST_TYPE,
	AST_SLOT_CONST,
	AST_SLOT_PARAM,
	AST_SLOT_MEMBER,
	AST_SLOT_CLOSURE,
	AST_SLOT_TEMPL,
	AST_SLOT_CONS,
	AST_SLOT_CONS_ARRAY,

	AST_SLOT_SUBST,
};

const char *
ast_slot_name(enum ast_slot_kind kind);

struct ast_env_slot {
	struct atom *name;
	ast_slot_id type;
	ast_member_id member_id;

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
		type_id unit;
		type_id type;
		type_id cons;
		type_id integer;
	} types;

	struct {
		struct ast_object_def *func;
		struct ast_object_def *array;
	} cons;

	// TODO: Get rid of dependency on vm?
	struct vm *vm;
};

enum ast_bind_result_code {
	AST_BIND_OK = 0,
	AST_BIND_TYPE_MISMATCH,
	AST_BIND_VALUE_MISMATCH,

	// A value mismatch with two types.
	AST_BIND_TYPE_VALUE_MISMATCH,
	AST_BIND_ARRAY_LENGTH_MISMATCH,
	AST_BIND_OBJ_HAS_NO_MEMBERS,
	AST_BIND_TYPE_HAS_NO_MEMBERS,
	AST_BIND_OBJ_MISSING_MEMBER,
	AST_BIND_COMPILER_ERROR,
};

struct ast_bind_result {
	enum ast_bind_result_code code;

	union {
		struct {
			ast_slot_id result;
		} ok;

		struct {
			type_id old, new;
		} type_mismatch;

		struct {
			struct object old, new;
		} value_mismatch;

		struct {
			size_t old, new;
		} array_length_mismatch;

		struct {
			type_id obj_type;
		} obj_no_members;

		struct {
			struct atom *name;
		} obj_missing_member;

		struct {
			type_id obj_type;
		} type_no_members;
	};
};

struct ast_context
ast_init_context(struct stg_error_context *, struct atom_table *, struct vm *);

ast_slot_id
ast_bind_result_to_slot(struct ast_bind_result res);

ast_slot_id
ast_bind_require_ok(struct ast_bind_result res);

struct ast_bind_result
ast_try_bind_slot_error(struct ast_context *,
		struct ast_env *, ast_slot_id target);

struct ast_bind_result
ast_try_bind_slot_wildcard(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, ast_slot_id type);

struct ast_bind_result
ast_try_bind_slot_const(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, struct object);

struct ast_bind_result
ast_try_bind_slot_const_type(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, type_id);

struct ast_bind_result
ast_try_bind_slot_param(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, int64_t param_index, ast_slot_id type);

struct ast_bind_result
ast_try_bind_slot_templ(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type);

struct ast_bind_result
ast_try_bind_slot_member(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type);

struct ast_bind_result
ast_try_bind_slot_closure(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type);

struct ast_bind_result
ast_try_bind_slot_cons(struct ast_context *,
		struct ast_env *, ast_slot_id target,
		struct atom *name, struct ast_object_def *);

struct ast_bind_result
ast_try_bind_slot_cons_array(struct ast_context *,
		struct ast_env *, ast_slot_id target, struct atom *name,
		ast_slot_id *members, size_t num_members, ast_slot_id member_type);

struct ast_bind_result
ast_try_union_slot(struct ast_context *, struct ast_env *,
		ast_slot_id target, ast_slot_id src_slot);


ast_slot_id
ast_bind_slot_error(struct ast_context *,
		struct ast_env *, ast_slot_id target);

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
ast_bind_slot_member(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type);

ast_slot_id
ast_bind_slot_closure(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
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
ast_unpack_arg_named(struct ast_context *, struct ast_env *,
		ast_slot_id obj, ast_slot_id target, struct atom *name);

struct ast_bind_result
ast_try_unpack_arg_named(struct ast_context *, struct ast_env *,
		ast_slot_id obj, ast_slot_id target, struct atom *name);

ast_slot_id
ast_union_slot(struct ast_context *, struct ast_env *,
		ast_slot_id target, ast_slot_id src_slot);


ast_slot_id
ast_copy_slot(struct ast_context *,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot);

ast_slot_id
ast_copy_slot_with_member_id(struct ast_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot);

void
ast_substitute(struct ast_context *, struct ast_env *,
		ast_slot_id new_slot, ast_slot_id target);

struct ast_env_slot
ast_env_slot(struct ast_context *, struct ast_env *, ast_slot_id);

bool
ast_object_def_from_cons(struct ast_context *, struct ast_env *,
		struct ast_object_def *out, ast_slot_id);

void
ast_env_print(struct vm *vm, struct ast_env *);

void
ast_print_slot(struct ast_context *, struct ast_env *, ast_slot_id);

struct ast_object_def_param {
	int param_id;
	struct atom *name;
	type_id type;
	ast_slot_id slot;
};

enum ast_object_def_bind_kind {
	AST_OBJECT_DEF_BIND_VALUE,
	AST_OBJECT_DEF_BIND_PACK,
	AST_OBJECT_DEF_BIND_CONST,
};

struct ast_object_def_bind {
	enum ast_object_def_bind_kind kind;

	ast_member_id target;
	ast_member_id *value_params;
	size_t num_value_params;

	func_id unpack_func;

	union {
		struct {
			bool overridable;
			func_id func;
		} value;
		struct ast_object_def *pack;
		struct object const_value;
	};
};

typedef struct object (*ast_object_unpack)(
		struct ast_context *, struct ast_env *,
		struct ast_object_def *, int param_id, struct object);

typedef struct object (*ast_object_pack)(
		struct ast_context *, struct ast_module *, struct ast_env *,
		struct ast_object_def *, ast_slot_id);

struct ast_object_def {
	struct ast_object_def_param *params;
	size_t num_params;
	struct ast_env env;

	struct ast_object_def_bind *binds;
	size_t num_binds;

	ast_slot_id ret_type;

	ast_object_unpack unpack;
	ast_object_pack pack;

	object_pack_func pack_func;
	object_unpack_func unpack_func;

	void *data;
};

struct ast_object_def *
ast_object_def_register(struct objstore *);

void
ast_object_def_finalize(struct ast_object_def *,
		struct ast_object_def_param *params, size_t num_params,
		ast_slot_id ret_type);

size_t
ast_object_def_num_descendant_members(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_object_def *def);

// out_bind_order should be an array of length (def->num_binds+num_extra_binds).
// If the return value of this function is 0, the array out_bind_order will
// contain the index of each bind in topological order. def's binds will have
// index 0 to (excluding) def->num_binds, and extra_binds will have index
// def->num_binds to (excluding) def->num_binds+num_extra_binds.
int
ast_object_def_order_binds(
		struct ast_context *ctx,
		struct ast_module *mod,
		struct ast_object_def *def,
		struct ast_object_def_bind *extra_binds,
		size_t num_extra_binds,
		int *out_bind_order);

typedef struct object (*ast_array_unpack)(
		struct ast_context *, struct ast_env *,
		struct ast_array_def *, size_t member_id, struct object);

typedef struct object (*ast_array_pack)(
		struct ast_context *, struct ast_module *, struct ast_env *,
		struct ast_array_def *, ast_slot_id);

struct ast_array_def {
	ast_array_unpack unpack;
	ast_array_pack pack;

	void *data;
};

int
ast_slot_pack(struct ast_context *, struct ast_module *,
		struct ast_env *, ast_slot_id obj, struct object *out);

int
ast_slot_pack_type(struct ast_context *, struct ast_module *,
		struct ast_env *, ast_slot_id obj, type_id *out);

enum ast_node_kind {
	AST_NODE_FUNC,
	AST_NODE_FUNC_NATIVE,
	AST_NODE_CALL,
	AST_NODE_CONS,
	AST_NODE_ACCESS,
	AST_NODE_TEMPL,
	AST_NODE_SLOT,
	AST_NODE_LIT,
	AST_NODE_FUNC_TYPE,
	AST_NODE_LOOKUP,

	// Datatype declarations
	AST_NODE_COMPOSITE,
	AST_NODE_VARIANT,
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

struct ast_template_param {
	struct atom *name;
	ast_slot_id slot;
	struct stg_location loc;
};

struct ast_datatype_variant {
	struct atom *name;
	struct ast_node *type;
};

struct ast_datatype_member {
	struct atom *name;
	struct ast_node *type;
	struct stg_location loc;

	// If this member is not explicitly typed a bind must be specified to
	// determine the members type.
	int type_giving_bind;

	// TODO: Is this necessary? It was added to allow it to be printed.
	ast_slot_id slot;
};

struct ast_datatype_bind {
	struct ast_node *target;
	struct ast_node *value;
	struct stg_location loc;
	bool overridable;
};

struct ast_closure_member {
	struct atom *name;
	struct ast_name_ref ref;

	bool require_const;
};

struct ast_closure_target {
	struct ast_closure_member *members;
	size_t num_members;
};

// defined in ast_nodes.c
struct ast_templ_node_data;

struct ast_node {
	enum ast_node_kind kind;
	type_id type;
	struct stg_location loc;

	union {
		struct {
			union {
				struct ast_node *body;
				struct {
					struct string name;
				} native;
			};

			struct ast_func_param *params;
			size_t num_params;

			struct ast_node *return_type;
			ast_slot_id return_type_slot;

			ast_slot_id param_types_slot;

			ast_slot_id type;

			ast_slot_id slot;

			func_id instance;

			struct ast_closure_target closure;
		} func;

		// Used for both call and cons.
		struct {
			struct ast_node *func;

			struct ast_func_arg *args;
			size_t num_args;

			ast_slot_id ret_type;

			// Used only for cons.
			ast_slot_id cons;
		} call;

		struct {
			struct ast_node **param_types;
			size_t num_params;
			
			struct ast_node *ret_type;

			ast_slot_id slot;
		} func_type;

		struct {
			struct atom *name;
			struct ast_node *target;
			ast_slot_id slot;
		} access;

		struct {
			struct ast_node *body;

			struct ast_template_param *params;
			size_t num_params;

			ast_slot_id cons;

			ast_slot_id slot;

			struct ast_object_def *def;
		} templ;

		ast_slot_id slot;

		struct {
			struct object obj;
			ast_slot_id slot;
		} lit;

		struct {
			struct atom *name;
			struct ast_name_ref ref;

			ast_slot_id slot;
		} lookup;

		struct {
			struct ast_datatype_member *members;
			size_t num_members;

			struct ast_datatype_bind *binds;
			size_t num_binds;

			struct ast_node **free_exprs;
			size_t num_free_exprs;

			ast_slot_id cons;

			ast_slot_id ret_value;

			struct ast_closure_target closure;

			type_id type;
		} composite;

		struct {
			struct ast_datatype_variant *variants;
			size_t num_variants;

			ast_slot_id ret_value;
		} variant;
	};
};

char *
ast_node_name(enum ast_node_kind);

#define AST_NODE_VISIT(node, visit_composite_body, visit_func_body)				\
	do { assert((node)); switch ((node)->kind) {								\
		case AST_NODE_FUNC:														\
			if (visit_func_body) {												\
				VISIT_NODE((node)->func.body);									\
			}																	\
		case AST_NODE_FUNC_NATIVE:												\
			for (size_t i = 0; i < (node)->func.num_params; i++) { 				\
				if ((node)->func.params[i].type) { 								\
					VISIT_NODE((node)->func.params[i].type); 					\
				}																\
			}																	\
			if ((node)->func.return_type) {										\
				VISIT_NODE((node)->func.return_type);							\
			}																	\
			break;																\
		case AST_NODE_CALL:														\
		case AST_NODE_CONS:														\
			VISIT_NODE((node)->call.func);										\
			for (size_t i = 0; i < (node)->call.num_args; i++) {				\
				VISIT_NODE((node)->call.args[i].value);							\
			}																	\
			break;																\
		case AST_NODE_FUNC_TYPE:												\
			for (size_t i = 0; i < (node)->func_type.num_params; i++) {			\
				VISIT_NODE((node)->func_type.param_types[i]);					\
			}																	\
			VISIT_NODE((node)->func_type.ret_type);								\
			break;																\
		case AST_NODE_ACCESS:													\
			VISIT_NODE((node)->access.target);									\
			break;																\
		case AST_NODE_TEMPL:													\
			printf("TODO: Visit template.\n");									\
			break;																\
		case AST_NODE_SLOT:														\
			break;																\
		case AST_NODE_LIT:														\
			break;																\
		case AST_NODE_LOOKUP:													\
			break;																\
		case AST_NODE_COMPOSITE:												\
			if (visit_composite_body) {											\
				for (size_t i = 0; i < (node)->composite.num_members; i++) {	\
					if ((node)->composite.members[i].type) {					\
						VISIT_NODE((node)->composite.members[i].type);			\
					}															\
				}																\
				for (size_t i = 0; i < (node)->composite.num_binds; i++) {		\
					VISIT_NODE((node)->composite.binds[i].target);				\
					VISIT_NODE((node)->composite.binds[i].value);				\
				}																\
				for (size_t i = 0; i < (node)->composite.num_binds; i++) {		\
					VISIT_NODE((node)->composite.free_exprs[i]);				\
				}																\
			}																	\
			break;																\
		case AST_NODE_VARIANT:													\
			printf("TODO: Visit variant.\n");									\
			break;																\
	}} while (0);

ast_slot_id
ast_node_resolve_slot(struct ast_env *env, ast_slot_id *slot);

ast_slot_id
ast_node_type(struct ast_context *, struct ast_env *, struct ast_node *);

ast_slot_id
ast_node_value(struct ast_context *, struct ast_env *, struct ast_node *);

#define AST_NODE_NEW ((struct ast_node *)1)

struct ast_node *
ast_init_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct atom **param_names, struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct ast_node *body);

struct ast_node *
ast_init_node_func_native(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct atom **param_names, struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct string native_func_name);

struct ast_node *
ast_init_node_templ(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct ast_node *body);

struct ast_node *
ast_init_node_call(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args);

struct ast_node *
ast_init_node_cons(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args);

struct ast_node *
ast_init_node_func_type(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct ast_node **param_types, size_t num_params,
		struct ast_node *ret_type);

struct ast_node *
ast_init_node_slot(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		ast_slot_id slot);

struct ast_node *
ast_init_node_access(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct ast_node *lhs, struct atom *name);

struct ast_node *
ast_init_node_lit(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct object);

struct ast_node *
ast_init_node_lookup(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location,
		struct atom *name);

void
ast_node_templ_register_param(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *templ, struct atom *name,
		struct stg_location loc);

struct ast_node *
ast_init_node_composite(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location);

#define AST_NO_TYPE_GIVING_BIND ((int)-1)

int
ast_node_composite_add_member(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct atom *name,
		struct ast_node *type, int type_giving_bind);

ast_slot_id
ast_node_composite_get_member(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct atom *name);

int
ast_node_composite_bind(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *composite, struct ast_node *target,
		struct ast_node *value, bool overridable);

void
ast_node_composite_add_free_expr(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct ast_node *expr);

void
ast_node_substitute_slot(struct ast_node *,
		ast_slot_id target, ast_slot_id new_slot);

struct stg_native_module;

int
ast_node_resolve_names(struct ast_context *ctx, struct ast_env *env,
		struct stg_native_module *native_mod, struct ast_scope *scope,
		bool require_const, struct ast_node *node);

int
ast_composite_node_resolve_names(struct ast_context *ctx, struct ast_env *env,
		struct stg_native_module *native_mod, struct ast_scope *scope,
		bool require_const, struct ast_node *comp, struct ast_node *node,
		ast_member_id *local_members);

int
ast_node_discover_potential_closures(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *scope, bool require_const, struct ast_node *node);

enum ast_name_dep_requirement {
	AST_NAME_DEP_REQUIRE_TYPE,
	AST_NAME_DEP_REQUIRE_VALUE,
};

struct ast_name_dep {
	enum ast_name_dep_requirement req;
	struct ast_name_ref ref;
};

int
ast_node_find_named_dependencies(
		struct ast_node *node, enum ast_name_dep_requirement req,
		struct ast_name_dep **out_refs, size_t *out_num_refs);

struct ast_typecheck_dep {
	// Describes what type is available compile time.
	enum ast_name_dep_requirement req;
	struct ast_name_ref ref;

	ast_slot_id value;

	// If determined is true, type or val is set if req is require_type or
	// require_value respectively.
	bool determined;
	bool lookup_failed;
	union {
		type_id type;
		struct object val;
	};
};

int
ast_node_typecheck(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node,
		struct ast_typecheck_dep *deps, size_t num_deps);

// Defined in ast_slots.c
struct ast_node *
ast_node_deep_copy(struct ast_context *ctx, struct ast_env *dest_env,
		struct ast_env *src_env, struct ast_node *src);

struct bc_env;
struct bc_instr;
typedef int bc_var;

struct ast_gen_bc_result {
	struct bc_instr *first;
	struct bc_instr *last;
	bc_var out_var;
};

struct ast_gen_info {
	ast_member_id *members;
	type_id *member_types;
	struct object *const_member_values;
	size_t num_members;

	struct ast_typecheck_closure *closures;
	size_t num_closures;
};

struct ast_gen_bc_result
ast_node_gen_bytecode(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_gen_info *info,
		struct bc_env *bc_env, struct ast_node *node);

struct bc_env *
ast_func_gen_bytecode(
		struct ast_context *ctx, struct ast_module *mod, struct ast_env *env,
		struct ast_typecheck_closure *closures, size_t num_closures, struct ast_node *node);

struct bc_env *
ast_composite_bind_gen_bytecode(
		struct ast_context *ctx, struct ast_module *mod, struct ast_env *env,
		ast_member_id *members, type_id *member_types,
		struct object *const_member_values, size_t num_members,
		struct ast_typecheck_closure *closures, size_t num_closures, struct ast_node *expr);

struct bc_env *
ast_gen_value_unpack_func(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, type_id value_type, size_t descendent);
void
ast_print(struct ast_context *, struct ast_env *, struct ast_node *);

void
ast_print_node(struct ast_context *, struct ast_env *, struct ast_node *);

enum ast_module_name_kind {
	AST_MODULE_NAME_DECL,
	AST_MODULE_NAME_NAMESPACE,
	AST_MODULE_NAME_IMPORT,
};

struct ast_module_dependency {
	struct atom *name;
	struct ast_node *container;

	struct ast_module *mod;
};

struct stg_module;

struct ast_module {
	// TODO: This should probably not be here.
	struct stg_module *stg_mod;
	struct ast_env env;

	struct ast_node *root;

	struct ast_module_dependency *dependencies;
	size_t num_dependencies;

	bool has_native_module_ext;
	struct string native_module_ext;

	type_id type;
};

int
ast_namespace_add_decl(struct ast_context *, struct ast_module *,
		struct ast_node *, struct atom *name, struct ast_node *expr);

void
ast_namespace_add_free_expr(struct ast_context *, struct ast_module *,
		struct ast_node *, struct ast_node *expr);

struct ast_node *
ast_namespace_add_ns(struct ast_context *, struct ast_env *,
		struct ast_node *, struct atom *name);

void
ast_namespace_add_import(struct ast_context *, struct ast_module *,
		struct ast_node *, struct atom *name);

void
ast_module_add_dependency(struct ast_context *,
		struct ast_module *, struct ast_node *container,
		struct atom *name);

void
ast_namespace_use(struct ast_context *,
		struct ast_module *, struct ast_node *,
		ast_slot_id object);

int
ast_module_resolve_dependencies(struct ast_context *, struct ast_module *);

int
ast_module_finalize(struct ast_context *, struct ast_module *);

void
ast_print_module(struct ast_context *, struct ast_module *);

bool
ast_dt_is_valid(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *comp);

struct ast_typecheck_closure {
	enum ast_name_dep_requirement req;
	bool lookup_failed;

	union {
		type_id type;
		struct object value;
	};
};

type_id
ast_dt_finalize_composite(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *comp,
		struct ast_typecheck_closure *closures, size_t num_closures);

#endif
