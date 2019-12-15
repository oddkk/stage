#ifndef STAGE_SYNTAX_TREE_H
#define STAGE_SYNTAX_TREE_H

#include "atom.h"
#include "str.h"
#include "errors.h"

#define ST_BIN_OPS								\
	OP(ADD, "+")								\
	OP(SUB, "-")								\
	OP(MUL, "*")								\
	OP(DIV, "/")								\
	OP(RANGE, "..")								\
	OP(EQ, "==")								\
	OP(NEQ, "!=")								\
	OP(GTE, ">=")								\
	OP(LTE, "<=")								\
	OP(GT, ">")									\
	OP(LT, "<")									\
	OP(BAND, "&")								\
	OP(BOR, "|")								\
	OP(BXOR, "|")								\
	OP(LAND, "&&")								\
	OP(LOR, "||")								\
	OP(LSFT, "<<")								\
	OP(RSFT, ">>")								\
	OP(BIND, "->")								\
	OP(SUBSCRIPT, "[]")							\

#define ST_BIN_OPS_MAX_LEN (2)

enum st_bin_op {
#define OP(name, sym) ST_OP_##name,
	ST_BIN_OPS
#undef OP

	ST_OP_LEN
};

extern struct string st_bin_op_sym[ST_OP_LEN];

struct atom *binop_atom(struct atom_table *, enum st_bin_op);

enum st_node_type {
#define ST_NODE(name, data) ST_NODE_##name,
#include "syntax_tree_node_defs.h"
#undef ST_NODE
	ST_NODES_LEN
};

#define ST_NODE(name, data) typedef data name##_t;
#include "syntax_tree_node_defs.h"
#undef ST_NODE

struct st_node {
	enum st_node_type type;

	struct stg_location loc;

	struct st_node *next_sibling;

#define ST_NODE(name, data) name##_t name;
	union {
#include "syntax_tree_node_defs.h"
	};
#undef ST_NODE
};

extern struct string st_node_names[ST_NODES_LEN];

#define ST_NODE_VISIT(node)							\
	switch ((node)->type) {								\
	case ST_NODE_INTERNAL_LIST:						\
		TREE_VISIT_NODE((node), INTERNAL_LIST, head);	\
		TREE_VISIT_NODE((node), INTERNAL_LIST, tail);	\
		break;											\
														\
	case ST_NODE_MODULE:								\
		TREE_VISIT_NODE((node), MODULE, body);			\
		break;											\
														\
	case ST_NODE_STMT:									\
		TREE_VISIT_NODE((node), STMT, attrs);			\
		TREE_VISIT_NODE((node), STMT, stmt);			\
		break;											\
														\
	case ST_NODE_DECL_STMT:							\
		TREE_VISIT_NODE((node), DECL_STMT, name);		\
		TREE_VISIT_NODE((node), DECL_STMT, args);		\
		TREE_VISIT_NODE((node), DECL_STMT, decl);		\
		break;											\
														\
	case ST_NODE_OBJ_DECL:								\
		TREE_VISIT_NODE((node), OBJ_DECL, ident);		\
		TREE_VISIT_NODE((node), OBJ_DECL, body);		\
		break;											\
														\
	case ST_NODE_VARIANT_DECL:							\
		TREE_VISIT_NODE((node), VARIANT_DECL, items);	\
		TREE_VISIT_NODE((node), VARIANT_DECL, params);	\
		break;											\
														\
	case ST_NODE_VARIANT_ITEM:							\
		TREE_VISIT_ATOM((node), VARIANT_ITEM, name);	\
		TREE_VISIT_NODE((node), VARIANT_ITEM, data_type);\
		break;											\
														\
	case ST_NODE_MOD_STMT:								\
		break;											\
														\
	case ST_NODE_USE_STMT:								\
		TREE_VISIT_NODE((node), USE_STMT, ident);		\
		break;											\
														\
	case ST_NODE_USE_ALL:								\
		TREE_VISIT_NODE((node), USE_ALL, target);		\
		break;											\
														\
	case ST_NODE_ASSERT_STMT:							\
		TREE_VISIT_NODE((node), ASSERT_STMT, expr);		\
		break;											\
														\
	case ST_NODE_FUNC_PROTO:							\
		TREE_VISIT_NODE((node), FUNC_PROTO, params);	\
		TREE_VISIT_NODE((node), FUNC_PROTO, ret);		\
		break;											\
														\
	case ST_NODE_ASSIGN_STMT:							\
		TREE_VISIT_NODE((node), ASSIGN_STMT, ident);	\
		TREE_VISIT_NODE((node), ASSIGN_STMT, type);		\
		TREE_VISIT_NODE((node), ASSIGN_STMT, body);		\
		break;											\
														\
	case ST_NODE_BIND:									\
		TREE_VISIT_NODE((node), BIND, src);				\
		TREE_VISIT_NODE((node), BIND, drain);			\
		break;											\
														\
	case ST_NODE_NAMESPACE:							\
		TREE_VISIT_NODE((node), NAMESPACE, name);		\
		TREE_VISIT_NODE((node), NAMESPACE, body);		\
		break;											\
														\
	case ST_NODE_TEMPLATE_VAR:							\
		TREE_VISIT_ATOM((node), TEMPLATE_VAR, name);	\
		break;											\
														\
	case ST_NODE_ACCESS:								\
		TREE_VISIT_NODE((node), ACCESS, target);		\
		TREE_VISIT_ATOM((node), ACCESS, name);			\
		break;											\
														\
	case ST_NODE_BIN_OP:								\
		TREE_VISIT_NODE((node), BIN_OP, lhs);			\
		TREE_VISIT_NODE((node), BIN_OP, rhs);			\
		break;											\
														\
	case ST_NODE_LAMBDA:								\
		TREE_VISIT_NODE((node), LAMBDA, proto);			\
		TREE_VISIT_NODE((node), LAMBDA, body);			\
		break;											\
														\
	case ST_NODE_FUNC_CALL:							\
		TREE_VISIT_NODE((node), FUNC_CALL, ident);		\
		TREE_VISIT_NODE((node), FUNC_CALL, params);		\
		break;											\
														\
	case ST_NODE_TEMPL_INST:							\
		TREE_VISIT_NODE((node), FUNC_CALL, ident);		\
		TREE_VISIT_NODE((node), FUNC_CALL, params);		\
		break;											\
														\
	case ST_NODE_OBJECT_DECL:							\
		TREE_VISIT_NODE((node), OBJECT_DECL, params);	\
		TREE_VISIT_NODE((node), OBJECT_DECL, body); 	\
		break;											\
														\
	case ST_NODE_OBJECT_INST:							\
		TREE_VISIT_NODE((node), OBJECT_INST, name); 	\
		TREE_VISIT_NODE((node), OBJECT_INST, body); 	\
		break;											\
														\
	case ST_NODE_TUPLE_DECL:							\
		TREE_VISIT_NODE((node), TUPLE_DECL, items);		\
		break;											\
														\
	case ST_NODE_TUPLE_DECL_ITEM:						\
		TREE_VISIT_ATOM((node), TUPLE_DECL_ITEM, name);	\
		TREE_VISIT_NODE((node), TUPLE_DECL_ITEM, type);	\
		break;											\
														\
	case ST_NODE_TUPLE_LIT:							\
		TREE_VISIT_NODE((node), TUPLE_LIT, items);		\
		break;											\
														\
	case ST_NODE_TUPLE_LIT_ITEM:						\
		TREE_VISIT_ATOM((node), TUPLE_LIT_ITEM, name);	\
		TREE_VISIT_NODE((node), TUPLE_LIT_ITEM, value);	\
		break;											\
														\
	case ST_NODE_ARRAY_LIT:							\
		TREE_VISIT_NODE((node), ARRAY_LIT, items);		\
		break;											\
														\
	case ST_NODE_SPECIAL:								\
		TREE_VISIT_NODE((node), SPECIAL, args);		 	\
		break;											\
														\
	case ST_NODE_NUM_LIT:								\
		break;											\
														\
	case ST_NODE_STR_LIT:								\
		break;											\
														\
	case ST_NODE_IDENT:								\
		break;											\
														\
	case ST_NODES_LEN:									\
		assert(false);									\
		break;											\
														\
	}													\

void st_print(struct st_node *node);
void st_clean(struct st_node **tree);

struct ast_node;
struct ast_env;
struct ast_context;
struct ast_module;

bool
st_node_has_templ_params(struct st_node *node);

void
st_node_visit_stmt(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *struct_node, struct st_node *stmt);

struct ast_node *
st_node_visit_expr(struct ast_context *, struct ast_module *mod, struct ast_env *,
		struct ast_node *templ_node, struct st_node *);

#endif
