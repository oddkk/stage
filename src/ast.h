#ifndef STAGE_AST_H
#define STAGE_AST_H

#include "errors.h"
#include "objstore.h"

#define AST_DEBUG_SLOT_SOLVE 0
#define AST_DEBUG_SLOT_SOLVE_APPLY 0
#define AST_DEBUG_SLOT_SOLVE_GRAPH 0
#define AST_DEBUG_UNINITIALIZED_SLOT_ID 0

typedef int32_t ast_slot_id;
typedef int ast_member_id;
typedef int ast_param_id;
typedef int ast_closure_id;
typedef int ast_use_id;
typedef int ast_init_expr_id;
typedef int ast_data_type_id;

struct ast_module;

enum ast_name_ref_kind {
	AST_NAME_REF_NOT_FOUND = 0,
	AST_NAME_REF_MEMBER,
	AST_NAME_REF_PARAM,
	AST_NAME_REF_CLOSURE,
	AST_NAME_REF_TEMPL,
	AST_NAME_REF_USE,
	AST_NAME_REF_INIT_EXPR,
	AST_NAME_REF_SELF,
};

struct ast_name_ref {
	enum ast_name_ref_kind kind;
	union {
		struct {
			ast_member_id id;
			int unpack_id;
		} member;
		ast_param_id   param;
		ast_closure_id closure;
		ast_param_id   templ;
		struct {
			ast_use_id   id;
			ast_param_id param;
		} use;
		ast_init_expr_id init_expr;
		int self_offset;
	};
};

// Defined in ast_typecheck.c
bool
ast_name_ref_equals(struct ast_name_ref lhs, struct ast_name_ref rhs);

void
ast_print_name_ref(struct ast_name_ref);

struct ast_node;

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

enum ast_traverse_role {
	AST_TRAV_ROOT = 0,

	AST_TRAV_FUNC_PARAM_TYPE,
	AST_TRAV_FUNC_RET_TYPE,
	AST_TRAV_FUNC_BODY,

	AST_TRAV_CALL_TARGET,
	AST_TRAV_CALL_ARG,

	AST_TRAV_INST_TARGET,
	AST_TRAV_INST_ARG,

	AST_TRAV_CONS_TARGET,
	AST_TRAV_CONS_ARG,

	AST_TRAV_FUNC_TYPE_PARAM_TYPE,
	AST_TRAV_FUNC_TYPE_RET_TYPE,

	AST_TRAV_TEMPL_PARAM_TYPE,
	AST_TRAV_TEMPL_BODY,

	AST_TRAV_ACCESS_TARGET,

	AST_TRAV_MATCH_VALUE,
	AST_TRAV_MATCH_PATTERN_TYPE,
	AST_TRAV_MATCH_PATTERN_EXPR,
	AST_TRAV_MATCH_PATTERN,

	AST_TRAV_COMPOSITE_MEMBER_TYPE,
	AST_TRAV_COMPOSITE_BIND_TARGET,
	AST_TRAV_COMPOSITE_BIND_VALUE,
	AST_TRAV_COMPOSITE_FREE_EXPR,
	AST_TRAV_COMPOSITE_INIT_EXPR,
	AST_TRAV_COMPOSITE_IMPL_TARGET,
	AST_TRAV_COMPOSITE_IMPL_ARG,
	AST_TRAV_COMPOSITE_IMPL_VALUE,

	AST_TRAV_TC_PATTERN_PARAM_TYPE,
	AST_TRAV_TC_MBR_PARAM_TYPE,
	AST_TRAV_TC_MBR_TYPE,
	AST_TRAV_VARIANT_OPTION_TYPE,
};

typedef int (*ast_traverse_scope_t)(
		struct ast_context *, struct ast_scope *,
		struct ast_node *, enum ast_traverse_role,
		void *user_data);

int
ast_node_traverse_scope(ast_traverse_scope_t visit,
		void *user_data, struct ast_context *ctx,
		struct ast_scope *scope, struct ast_node *node);

struct ast_scope_name *
ast_scope_lookup(struct ast_scope *scope, struct atom *name);

size_t
ast_node_num_children(struct ast_node *);

enum ast_constraint_kind {
	AST_SLOT_REQ_ERROR,

	AST_SLOT_REQ_IS_OBJ,
	AST_SLOT_REQ_IS_TYPE,
	AST_SLOT_REQ_IS_FUNC_TYPE,
	AST_SLOT_REQ_EQUALS,
	AST_SLOT_REQ_CONS_OR_VALUE_FROM,
	AST_SLOT_REQ_TYPE,
	AST_SLOT_REQ_MEMBER_NAMED,
	// AST_SLOT_REQ_MEMBER_INDEXED,
	AST_SLOT_REQ_PARAM_NAMED,
	AST_SLOT_REQ_PARAM_INDEXED,
	AST_SLOT_REQ_CONS,
	AST_SLOT_REQ_INST,
};

enum ast_constraint_source {
	AST_CONSTR_SRC_EXPECTED,
	AST_CONSTR_SRC_DT_DECL,
	AST_CONSTR_SRC_FUNC_DECL,
	AST_CONSTR_SRC_TEMPL_PARAM_DECL,
	AST_CONSTR_SRC_TEMPL_PARAM_VAL,
	AST_CONSTR_SRC_CLOSURE,
	AST_CONSTR_SRC_CALL_ARG,
	AST_CONSTR_SRC_CONS_ARG,
	AST_CONSTR_SRC_MATCH_VALUE,
	AST_CONSTR_SRC_MATCH_RESULT,
	AST_CONSTR_SRC_ACCESS,
	AST_CONSTR_SRC_MOD,
	AST_CONSTR_SRC_LIT,
	AST_CONSTR_SRC_LOOKUP,
	AST_CONSTR_SRC_DECAY,
	AST_CONSTR_SRC_SELF,
};

#if AST_DEBUG_SLOT_SOLVE
struct ast_constraint_loc {
	const char *filename;
	size_t line;
};
#endif

struct ast_slot_constraint {
	enum ast_constraint_kind kind;
	enum ast_constraint_source source;
	ast_slot_id target;
	bool disabled;

	union {
		union {
			struct object obj;
			type_id type;
		} is;

		ast_slot_id equals;

		ast_slot_id cons_or_value_from;

		ast_slot_id type;

		struct {
			ast_slot_id slot;
			union {
				struct atom *name;
				size_t index;
			};
		} member;

		struct {
			ast_slot_id slot;
			union {
				struct atom *name;
				size_t index;
			};
		} param;

		struct object_cons *is_cons;

		ast_slot_id cons;
		ast_slot_id inst;
	};

	struct {
		struct stg_location loc;
#if AST_DEBUG_SLOT_SOLVE
		struct ast_constraint_loc impose_loc;
#endif

	} reason;
};

struct ast_env {
	size_t num_alloced_slots;

	struct ast_slot_constraint **constraint_pages;
	size_t last_page_num_used;
	size_t num_pages;
	size_t page_size;
	size_t constraints_per_page;
	// If this environment was copied from another, num_borrowed_pages will
	// indicate how many of this env's pages was borrowed from the src.
	size_t num_borrowed_pages;

	struct objstore *store;

	size_t invoc_id;
};

void
ast_env_free(struct ast_env *env);

struct vm;

struct ast_context {
	struct stg_error_context *err;
	struct vm *vm;
	struct arena *mem;

	struct arena _mem;
};

void
ast_init_context(struct ast_context *ctx, struct stg_error_context *, struct vm *);
void
ast_destroy_context(struct ast_context *ctx);

ast_slot_id
ast_slot_alloc(struct ast_env *env);

#if AST_DEBUG_SLOT_SOLVE
#	define AST_SLOT_DEBUG_PARAM , struct ast_constraint_loc constr_loc
#else
#	define AST_SLOT_DEBUG_PARAM
#endif

void
ast_slot_value_error(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_is_obj(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, struct object val
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_is_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, type_id val
		AST_SLOT_DEBUG_PARAM);

// type should be the type type. It is passed explicitly to avoid having the
// function take ast_context. TODO: type should not be a parameter.
void
ast_slot_require_is_func_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		type_id type,
		ast_slot_id target, ast_slot_id ret_type,
		ast_slot_id *param_types, size_t num_params
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_equals(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id slot
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_cons_or_value_from(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id slot
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id type
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_member_named(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, struct atom *name, ast_slot_id member
		AST_SLOT_DEBUG_PARAM);

/*
void
ast_slot_require_member_index(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, size_t index, ast_slot_id member
		AST_SLOT_DEBUG_PARAM);
*/

void
ast_slot_require_param_named(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, struct atom *name, ast_slot_id param
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_param_index(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, size_t index, ast_slot_id param
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_cons(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id cons
		AST_SLOT_DEBUG_PARAM);

void
ast_slot_require_inst(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id inst
		AST_SLOT_DEBUG_PARAM);

#if AST_DEBUG_SLOT_SOLVE
#	define AST_SLOT_DEBUG_ARG (struct ast_constraint_loc){.filename=__FILE__, .line=__LINE__}

#	define ast_slot_value_error(...)          ast_slot_value_error (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_is_obj(...)       ast_slot_require_is_obj (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_is_type(...)      ast_slot_require_is_type (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_is_func_type(...) ast_slot_require_is_func_type (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_cons(...)         ast_slot_require_cons (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_inst(...)         ast_slot_require_inst (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_equals(...)       ast_slot_require_equals (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_cons_or_value_from(...) ast_slot_require_cons_or_value_from (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_type(...)         ast_slot_require_type (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_member_named(...) ast_slot_require_member_named (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_param_named(...) ast_slot_require_param_named (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#	define ast_slot_require_param_index(...) ast_slot_require_param_index (__VA_ARGS__, AST_SLOT_DEBUG_ARG)
#endif

enum ast_slot_result_state {
	AST_SLOT_RES_ERROR                  = 0x000,

	AST_SLOT_RES_VALUE_UNKNOWN          = 0x001,
	AST_SLOT_RES_TYPE_FOUND             = 0x002,
	AST_SLOT_RES_VALUE_FOUND_OBJ        = 0x003,
	AST_SLOT_RES_VALUE_FOUND_TYPE       = 0x004,

	AST_SLOT_RES_VALUE_MASK             = 0x007,

	AST_SLOT_RES_CONS_UNKNOWN           = 0x010,
	AST_SLOT_RES_CONS_FOUND             = 0x020,
	AST_SLOT_RES_CONS_FOUND_FUNC_TYPE   = 0x030,

	AST_SLOT_RES_CONS_MASK              = 0x030,

	AST_SLOT_RES_INST_UNKNOWN           = 0x040,
	AST_SLOT_RES_INST_FOUND             = 0x080,

	AST_SLOT_RES_INST_MASK              = 0x0c0,

	AST_SLOT_RES_DECAYED                = 0x100,
};

static inline enum ast_slot_result_state
ast_slot_value_result(enum ast_slot_result_state state)
{
	return state & AST_SLOT_RES_VALUE_MASK;
}

static inline enum ast_slot_result_state
ast_slot_cons_result(enum ast_slot_result_state state)
{
	return state & AST_SLOT_RES_CONS_MASK;
}

static inline enum ast_slot_result_state
ast_slot_inst_result(enum ast_slot_result_state state)
{
	return state & AST_SLOT_RES_INST_MASK;
}

struct ast_slot_result {
	enum ast_slot_result_state result;
	type_id type;
	union {
		type_id type;
		struct object obj;
	} value;

	struct object_cons *cons;
	struct object_inst *inst;
};

// out_result is expected to be an array of length env->num_alloced_slots.
int
ast_slot_try_solve(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_env *env, struct ast_slot_result *out_result);

enum ast_node_kind {
	AST_NODE_FUNC,
	AST_NODE_FUNC_NATIVE,
	AST_NODE_CALL,
	AST_NODE_CONS,
	AST_NODE_INST,
	AST_NODE_ACCESS,
	AST_NODE_TEMPL,
	AST_NODE_LIT,
	AST_NODE_LIT_NATIVE,
	AST_NODE_FUNC_TYPE,
	AST_NODE_LOOKUP,
	AST_NODE_MOD,
	AST_NODE_MATCH,
	AST_NODE_WILDCARD,
	AST_NODE_INIT_EXPR,
	AST_NODE_TYPE_CLASS,

	// Datatype declarations
	AST_NODE_COMPOSITE,
	AST_NODE_VARIANT,

	AST_NODE_DATA_TYPE,
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

struct ast_pattern_param {
	struct atom *name;
	struct ast_node *type;
	struct stg_location loc;
};

struct ast_pattern {
	struct ast_node *node;

	struct ast_pattern_param *params;
	size_t num_params;
};

struct ast_match_case {
	struct ast_pattern pattern;
	struct ast_node *expr;
};

struct ast_datatype_variant {
	struct atom *name;
	struct ast_node *data_type;
	struct stg_location loc;
};

struct ast_datatype_member {
	struct atom *name;
	struct ast_node *type;
	struct stg_location loc;

	bool is_namespace;

	// If this member is not explicitly typed a bind must be specified to
	// determine the members type.
	int type_giving_bind;
};

struct ast_datatype_bind {
	struct ast_node *target;
	struct ast_node *value;
	struct stg_location loc;
	bool overridable;
	bool erroneous;
};

struct ast_datatype_use {
	struct ast_node *target;
	struct stg_location loc;

	// If as_name is not NULL, the name as_name will be exposed with the value
	// of target. If as_name is NULL all the members of target will be exposed.
	struct atom *as_name;
};

struct ast_datatype_impl_arg {
	struct atom *name;
	struct ast_node *value;
};

struct ast_datatype_impl {
	struct ast_node *target;
	struct ast_datatype_impl_arg *args;
	size_t num_args;

	// TODO: Support parametric impls.

	struct ast_node *value;

	struct stg_location loc;
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

// struct ast_type_class_param {
// 	struct atom *name;
// 	struct ast_node *type;
// };

struct ast_type_class_member {
	struct atom *name;
	struct ast_pattern type;
};

// defined in ast_nodes.c
struct ast_templ_node_data;

enum ast_composite_kind {
	AST_COMPOSITE_STRUCT,
	AST_COMPOSITE_STATIC_OBJ,
	AST_COMPOSITE_MODULE,
	AST_COMPOSITE_NAMESPACE,
	AST_COMPOSITE_TYPE_CLASS,
	AST_COMPOSITE_TYPE_CLASS_IMPL,
};

struct ast_node {
	enum ast_node_kind kind;
	type_id type;
	ast_slot_id typecheck_slot;
	struct stg_location loc;

	union {
		struct {
			union {
				struct ast_node *body;
				struct {
					struct string name;
					struct stg_native_module *native_mod;
				} native;
			};

			struct ast_func_param *params;
			size_t num_params;

			struct ast_node *return_type;
			func_id instance;

			struct ast_closure_target closure;
		} func;

		// Used for both call and cons.
		struct {
			struct ast_node *func;

			struct ast_func_arg *args;
			size_t num_args;

			union {
				// Used only for call.
				struct stg_func_object func_val;
				// Used only for cons.
				struct object_cons *cons;

				// Used only for inst.
				struct object_inst *inst;
			};

			struct object cons_value;
		} call;

		struct {
			struct ast_node **param_types;
			size_t num_params;
			
			struct ast_node *ret_type;

			type_id func_type;
		} func_type;

		struct {
			struct atom *name;
			struct ast_node *target;
			type_id const_target_value_type;
		} access;

		struct {
			struct ast_pattern pattern;

			struct object_cons *cons;
			bool failed;

			struct ast_closure_target closure;
		} templ;

		struct {
			struct object obj;
		} lit;

		struct {
			struct atom *name;
		} lit_native;

		struct {
			struct atom *name;
			struct ast_name_ref ref;
		} lookup;

		struct {
			ast_init_expr_id id;
		} init_expr;

		struct {
			struct atom *name;
		} mod;

		struct {
			struct ast_node *value;

			struct ast_match_case *cases;
			size_t num_cases;
		} match;

		struct {
			struct ast_datatype_member *members;
			size_t num_members;

			struct ast_datatype_bind *binds;
			size_t num_binds;

			struct ast_node **free_exprs;
			size_t num_free_exprs;

			struct ast_node **init_exprs;
			size_t num_init_exprs;

			struct ast_datatype_use *uses;
			size_t num_uses;

			struct ast_datatype_impl *impls;
			size_t num_impls;

			struct ast_closure_target closure;

			enum ast_composite_kind kind;

			bool is_init_monad;

			type_id type;
			bool failed;
		} composite;

		struct {
			struct ast_pattern pattern;

			struct ast_type_class_member *members;
			size_t num_members;

			struct ast_closure_target closure;

			struct object_cons *cons;

			bool failed;
		} type_class;

		struct {
			struct ast_datatype_variant *options;
			size_t num_options;

			type_id type;
			bool failed;
		} variant;

		struct {
			stg_mod_id mod;
			ast_data_type_id id;
		} data_type;
	};
};

char *
ast_node_name(enum ast_node_kind);

#define AST_NODE_VISIT(node, 													\
		visit_composite_body, 													\
		visit_func_body, 														\
		visit_templ_body) 														\
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
		case AST_NODE_INST:														\
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
			for (size_t i = 0; i < (node)->templ.pattern.num_params; i++) {		\
				if ((node)->templ.pattern.params[i].type) {						\
					VISIT_NODE((node)->templ.pattern.params[i].type);			\
				}																\
			}																	\
			if (visit_templ_body) {												\
				VISIT_NODE((node)->templ.pattern.node);							\
			}																	\
			break;																\
		case AST_NODE_LIT:														\
			break;																\
		case AST_NODE_LIT_NATIVE:												\
			break;																\
		case AST_NODE_LOOKUP:													\
			break;																\
		case AST_NODE_MOD:														\
			break;																\
		case AST_NODE_MATCH:													\
			VISIT_NODE((node)->match.value);									\
			for (size_t i = 0; i < (node)->match.num_cases; i++) {				\
				VISIT_NODE((node)->match.cases[i].pattern.node);				\
				VISIT_NODE((node)->match.cases[i].expr);						\
			}																	\
			break;																\
		case AST_NODE_WILDCARD:													\
			break;																\
		case AST_NODE_INIT_EXPR:												\
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
				for (size_t i = 0; i < (node)->composite.num_free_exprs; i++) { \
					VISIT_NODE((node)->composite.free_exprs[i]);				\
				}																\
				for (size_t i = 0; i < (node)->composite.num_init_exprs; i++) { \
					VISIT_NODE((node)->composite.init_exprs[i]);				\
				}																\
				for (size_t i = 0; i < (node)->composite.num_impls; i++) {		\
					struct ast_datatype_impl *impl; 							\
					impl = &(node)->composite.impls[i];							\
					VISIT_NODE(impl->target);									\
					for (size_t j = 0; j < impl->num_args; j++) { 				\
						VISIT_NODE(impl->args[j].value);						\
					}															\
					VISIT_NODE(impl->value);									\
				}																\
			}																	\
			break;																\
		case AST_NODE_TYPE_CLASS:												\
			for (size_t i = 0; i < (node)->type_class.pattern.num_params; i++) {\
				if ((node)->type_class.pattern.params[i].type) {				\
					VISIT_NODE((node)->type_class.pattern.params[i].type);		\
				}																\
			}																	\
			if (visit_composite_body) {											\
				for (size_t i = 0; i < (node)->type_class.num_members; i++) {	\
					VISIT_NODE((node)->type_class.members[i].type.node);		\
				}																\
			}																	\
			break;																\
		case AST_NODE_VARIANT:													\
			for (size_t i = 0; i < (node)->variant.num_options; i++) {			\
				if ((node)->variant.options[i].data_type) {						\
					VISIT_NODE((node)->variant.options[i].data_type);			\
				}																\
			}																	\
			break;																\
		case AST_NODE_DATA_TYPE:												\
			break;																\
	}} while (0);

#define AST_NODE_NEW ((struct ast_node *)1)

struct ast_node *
ast_init_node_func(struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct atom **param_names, struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct ast_node *body);

struct ast_node *
ast_init_node_func_native(struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct atom **param_names, struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct string native_func_name);

struct ast_node *
ast_init_node_templ(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct ast_node *body);

struct ast_node *
ast_init_node_call(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args);

struct ast_node *
ast_init_node_cons(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args);

struct ast_node *
ast_init_node_inst(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args);

struct ast_node *
ast_init_node_func_type(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct ast_node **param_types, size_t num_params,
		struct ast_node *ret_type);

struct ast_node *
ast_init_node_access(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct ast_node *lhs, struct atom *name);

struct ast_node *
ast_init_node_lit(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct object);

struct ast_node *
ast_init_node_lit_native(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct atom *name);

struct ast_node *
ast_init_node_lookup(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct atom *name);

struct ast_node *
ast_init_node_mod(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct atom *name);

struct ast_node *
ast_init_node_match(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct ast_node *value,
		struct ast_match_case *cases, size_t num_cases);

struct ast_node *
ast_init_node_wildcard(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location);

struct ast_node *
ast_init_node_init_expr(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		ast_init_expr_id);

void
ast_pattern_register_param(
		struct ast_context *ctx,
		struct ast_pattern *pat, struct atom *name,
		struct ast_node *type, struct stg_location loc);

struct ast_node *
ast_init_node_composite(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		enum ast_composite_kind kind);

struct ast_node *
ast_init_node_data_type(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		stg_mod_id, ast_data_type_id);

struct ast_node *
ast_init_node_type_class(
		struct ast_context *ctx, struct ast_node *node,
		struct stg_location,
		struct ast_type_class_member *members, size_t num_members);

#define AST_NO_TYPE_GIVING_BIND ((int)-1)

int
ast_node_composite_add_member(
		struct ast_context *ctx,
		struct ast_node *target, struct atom *name,
		struct ast_node *type, int type_giving_bind);

int
ast_node_composite_add_namespace(
		struct ast_context *ctx, struct ast_module *,
		struct ast_node *target, struct atom *name,
		struct ast_node *type);

int
ast_node_composite_bind(
		struct ast_context *ctx,
		struct ast_node *composite, struct ast_node *target,
		struct ast_node *value, bool overridable);

void
ast_node_composite_tag_bind_erroneous(
		struct ast_context *ctx,
		struct ast_node *composite, int bind_id);


void
ast_node_composite_add_free_expr(
		struct ast_context *ctx,
		struct ast_node *target, struct ast_node *expr);

ast_init_expr_id
ast_node_composite_add_init_expr(
		struct ast_context *ctx,
		struct ast_node *target, struct ast_node *expr);

void
ast_node_composite_add_use(
		struct ast_context *ctx, struct stg_location,
		struct ast_node *target, struct ast_node *expr,
		struct atom *as_name);

void
ast_node_composite_add_impl(
		struct ast_context *ctx, struct stg_location,
		struct ast_node *composite, struct ast_node *target,
		struct ast_datatype_impl_arg *args, size_t num_args,
		struct ast_node *value);

struct ast_node *
ast_init_node_variant(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location);

void
ast_node_variant_add_option(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location,
		struct atom *name, struct ast_node *data_type);

struct ast_module {
	stg_mod_id id;
	struct paged_list data_types;
};

void
ast_module_init(struct vm *, stg_mod_id, struct ast_module *);

struct ast_node *
ast_module_get_data_type(struct ast_module *, ast_data_type_id);

struct ast_node *
ast_module_node_get_data_type(struct vm *, struct ast_node *);

ast_data_type_id
ast_module_add_composite(
		struct ast_context *, struct ast_module *,
		struct stg_location, enum ast_composite_kind);

ast_data_type_id
ast_module_add_variant(struct ast_context *, struct ast_module *,
		struct stg_location);

ast_data_type_id
ast_module_add_type_class(struct ast_context *, struct ast_module *,
		struct stg_location, struct ast_type_class_member *members,
		size_t num_members);

int
ast_node_has_ambiguous_refs(struct ast_context *ctx,
		struct ast_scope *scope, struct ast_node *node);

int
ast_composite_node_resolve_names(struct ast_context *ctx,
		struct stg_native_module *native_mod, struct ast_scope *scope,
		bool require_const, struct ast_node *comp, struct ast_node *node,
		ast_member_id *local_members, struct atom *self_name);

int
ast_composite_node_has_ambiguous_refs(
		struct ast_context *ctx, struct ast_scope *scope,
		struct ast_node *comp, struct ast_node *node,
		ast_member_id *local_members, struct atom *self_name);

int
ast_node_discover_potential_closures(struct ast_context *ctx,
		struct stg_module *mod, struct ast_scope *scope,
		bool require_const, struct ast_node *node);

enum ast_name_dep_requirement {
	AST_NAME_DEP_REQUIRE_TYPE,
	AST_NAME_DEP_REQUIRE_VALUE,
};

struct ast_name_dep {
	enum ast_name_dep_requirement req;
	struct ast_name_ref ref;
};

int
ast_node_find_named_dependencies(struct vm *,
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

enum ast_tc_expected_kind {
	AST_NODE_TC_EXP_NOTHING = 0,
	AST_NODE_TC_EXP_TYPE,
	AST_NODE_TC_EXP_CONS,
};

struct ast_tc_expected {
	enum ast_tc_expected_kind kind;

	union {
		type_id type;
		struct object_cons *cons;
	};
};

#define AST_TC_NO_EXP ((struct ast_tc_expected){.kind=AST_NODE_TC_EXP_NOTHING})

int
ast_node_typecheck(struct ast_context *ctx,
		struct stg_module *mod, struct ast_node *node,
		struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_tc_expected, struct object *out_value);

void
ast_node_resolve_datatypes(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_node *node);

void
ast_constr_fill_closure_deps(struct ast_context *ctx, struct ast_env *env,
		struct ast_typecheck_dep *out_deps, struct ast_closure_target *closure,
		struct ast_typecheck_dep *deps, size_t num_deps);

ast_slot_id
ast_node_constraints(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_env *env, struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_node *node);

void
ast_typecheck_deps_slots(struct ast_env *env,
		struct ast_typecheck_dep *body_deps, size_t num_deps);

struct ast_typecheck_dep *
ast_find_dep(struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_name_ref ref);

struct ast_node *
ast_node_deep_copy(struct arena *, struct ast_node *src);

struct object_cons *
ast_node_create_templ(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *templ_node,
		struct ast_typecheck_dep *outer_deps, size_t num_outer_deps,
		struct ast_typecheck_dep *inner_deps, size_t num_inner_deps);

struct bc_env;
struct bc_instr;
typedef int bc_var;
typedef unsigned int bc_closure;

#define AST_BC_CLOSURE_PRUNED ((bc_closure)UINT_MAX)

struct ast_gen_init_expr {
	ast_init_expr_id id;
	type_id type;
};

enum ast_gen_dt_param_kind {
	AST_GEN_DT_PARAM_MEMBER,
	AST_GEN_DT_PARAM_INIT_EXPR,
};

struct ast_gen_dt_ref {
	enum ast_gen_dt_param_kind kind;

	union {
		struct {
			ast_member_id id;
			int unpack_id;
		} member;
		ast_init_expr_id init_expr;
	};
};

struct ast_gen_dt_param {
	struct ast_gen_dt_ref ref;

	type_id type;
	bool is_const;
	struct object const_val;
};

struct ast_gen_info {
	struct ast_gen_dt_param *dt_params;
	size_t num_dt_params;

	struct ast_typecheck_closure *closures;
	bc_closure *closure_refs;
	size_t num_closures;

	struct object *const_use_values;
	size_t num_use;

	struct object *templ_values;
	size_t num_templ_values;

	bc_var *pattern_params;
	size_t num_pattern_params;
};

struct bc_result
ast_node_gen_bytecode(struct ast_context *ctx, struct stg_module *mod,
		struct ast_gen_info *info, struct bc_env *bc_env, struct ast_node *node);

struct bc_env *
ast_func_gen_bytecode(
		struct ast_context *ctx, struct bc_env *, struct stg_module *mod,
		struct ast_typecheck_closure *closures, bc_closure *closure_refs,
		size_t num_closures, struct ast_node *node);

struct bc_env *
ast_composite_bind_gen_bytecode(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_gen_dt_param *dt_params, size_t num_dt_params,
		struct object *const_use_values, size_t num_use,
		struct ast_typecheck_closure *closures, size_t num_closures, struct ast_node *expr);

struct bc_env *
ast_type_expr_gen_bytecode(
		struct ast_context *ctx, struct stg_module *mod, struct ast_node *expr,
		struct ast_typecheck_closure *closures, size_t num_closures);

struct bc_env *
ast_gen_value_unpack_func(
		struct ast_context *ctx, struct stg_module *mod,
		type_id value_type, size_t descendent);
void
ast_print(struct ast_context *, struct ast_node *);

void
ast_print_node(struct ast_context *, struct ast_node *,
		bool print_type_slot);

struct ast_node *
ast_namespace_add_ns(struct ast_context *,
		struct ast_module *, struct ast_node *, struct atom *name);

int
ast_module_finalize(struct ast_context *, struct stg_module *, struct ast_node *root);

struct ast_typecheck_closure {
	enum ast_name_dep_requirement req;
	bool lookup_failed;

	union {
		type_id type;
		struct object value;
	};
};

void
ast_fill_closure(struct ast_closure_target *closure,
		struct ast_typecheck_closure *closure_values,
		struct ast_typecheck_dep *deps, size_t num_deps);

type_id
ast_dt_finalize_composite(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *comp, struct ast_typecheck_closure *closures, size_t num_closures);

type_id
ast_dt_finalize_variant(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_datatype_variant *options, size_t num_options,
		struct ast_typecheck_dep *deps, size_t num_deps);

struct ast_dt_variant {
	uint64_t tag;
	struct object data;
};

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


struct ast_dt_variant
ast_dt_decode_variant(struct stg_type_variant_info *info, void *obj_data);

void
ast_dt_encode_variant(struct stg_type_variant_info *info,
		uint64_t tag, void *in_data, void *out_data);

bool
ast_dt_variant_type_is_inst(
		struct vm *vm, type_id);

ssize_t
ast_dt_variant_tag_by_name(
		struct vm *vm, type_id, struct atom *name);

type_id
ast_dt_variant_tag_data_type(
		struct vm *vm, type_id, size_t tag);

#endif
