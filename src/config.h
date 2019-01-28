#ifndef STAGE_CONFIG_H
#define STAGE_CONFIG_H

#include "vm.h"
#include "module.h"

#define CFG_BIN_OPS								\
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
	OP(BIND, "->")								\
	OP(SUBSCRIPT, "[]")							\

#define CFG_BIN_OPS_MAX_LEN (2)

enum cfg_bin_op {
#define OP(name, sym) CFG_OP_##name,
	CFG_BIN_OPS
#undef OP

	CFG_OP_LEN
};

extern struct string cfg_bin_op_sym[CFG_OP_LEN];

struct atom *binop_atom(struct atom_table *, enum cfg_bin_op);

enum cfg_node_type {
#define CFG_NODE(name, data) CFG_NODE_##name,
#include "config_nodes.h"
#undef CFG_NODE
	CFG_NODES_LEN
};

struct cfg_location {
	size_t line;
	size_t column;
};

#define CFG_NODE(name, data) typedef data name##_t;
#include "config_nodes.h"
#undef CFG_NODE

struct cfg_node {
	enum cfg_node_type type;

	unsigned int file_id;
	struct cfg_location from;
	struct cfg_location to;

	struct cfg_node *next_sibling;

#define CFG_NODE(name, data) name##_t name;
	union {
#include "config_nodes.h"
	};
#undef CFG_NODE
};

extern struct string cfg_node_names[CFG_NODES_LEN];

enum cfg_job_type {
#define CFG_JOB(name, data) CFG_JOB_##name,
	#include "config_jobs.h"
#undef CFG_JOB
	CFG_JOBS_LEN
};

extern struct string cfg_job_names[CFG_JOBS_LEN];

void cfg_tree_print(struct cfg_node *node);

int cfg_compile(struct vm *vm, struct string cfg_dir);

#define CFG_NODE_VISIT(node)							\
	switch ((node)->type) {								\
	case CFG_NODE_INTERNAL_LIST:						\
		TREE_VISIT_NODE((node), INTERNAL_LIST, head);	\
		TREE_VISIT_NODE((node), INTERNAL_LIST, tail);	\
		break;											\
														\
	case CFG_NODE_MODULE:								\
		TREE_VISIT_NODE((node), MODULE, body);			\
		break;											\
														\
	case CFG_NODE_STMT:									\
		TREE_VISIT_NODE((node), STMT, attrs);			\
		TREE_VISIT_NODE((node), STMT, stmt);			\
		break;											\
														\
	case CFG_NODE_DECL_STMT:							\
		TREE_VISIT_NODE((node), DECL_STMT, name);		\
		TREE_VISIT_NODE((node), DECL_STMT, args);		\
		TREE_VISIT_NODE((node), DECL_STMT, decl);		\
		break;											\
														\
	case CFG_NODE_OBJ_DECL:								\
		TREE_VISIT_NODE((node), OBJ_DECL, ident);		\
		TREE_VISIT_NODE((node), OBJ_DECL, body);		\
		break;											\
														\
	case CFG_NODE_ENUM_DECL:							\
		TREE_VISIT_NODE((node), ENUM_DECL, items);		\
		break;											\
														\
	case CFG_NODE_ENUM_ITEM:							\
		TREE_VISIT_ATOM((node), ENUM_ITEM, name);		\
		TREE_VISIT_NODE((node), ENUM_ITEM, data);		\
		break;											\
														\
	case CFG_NODE_USE_STMT:								\
		TREE_VISIT_NODE((node), USE_STMT, ident);		\
		break;											\
														\
	case CFG_NODE_USE_ALL:								\
		break;											\
														\
	case CFG_NODE_ASSERT_STMT:							\
		TREE_VISIT_NODE((node), ASSERT_STMT, expr);		\
		break;											\
														\
	case CFG_NODE_FUNC_STMT:							\
		TREE_VISIT_NODE((node), FUNC_STMT, ident);		\
		TREE_VISIT_NODE((node), FUNC_STMT, proto);		\
		TREE_VISIT_NODE((node), FUNC_STMT, body);		\
		break;											\
														\
	case CFG_NODE_FUNC_PROTO:							\
		TREE_VISIT_NODE((node), FUNC_PROTO, params);	\
		TREE_VISIT_NODE((node), FUNC_PROTO, ret);		\
		break;											\
														\
	case CFG_NODE_ASSIGN_STMT:							\
		TREE_VISIT_NODE((node), ASSIGN_STMT, ident);	\
		TREE_VISIT_NODE((node), ASSIGN_STMT, type);		\
		TREE_VISIT_NODE((node), ASSIGN_STMT, body);		\
		break;											\
														\
	case CFG_NODE_BIND:									\
		TREE_VISIT_NODE((node), BIND, src);				\
		TREE_VISIT_NODE((node), BIND, drain);			\
		break;											\
														\
	case CFG_NODE_NAMESPACE:							\
		TREE_VISIT_NODE((node), NAMESPACE, name);		\
		TREE_VISIT_NODE((node), NAMESPACE, body);		\
		break;											\
														\
	case CFG_NODE_TEMPLATE_VAR:							\
		TREE_VISIT_ATOM((node), TEMPLATE_VAR, name);	\
		break;											\
														\
	case CFG_NODE_ACCESS:								\
		TREE_VISIT_NODE((node), ACCESS, lhs);			\
		TREE_VISIT_NODE((node), ACCESS, rhs);			\
		break;											\
														\
	case CFG_NODE_BIN_OP:								\
		TREE_VISIT_NODE((node), BIN_OP, lhs);			\
		TREE_VISIT_NODE((node), BIN_OP, rhs);			\
		break;											\
														\
	case CFG_NODE_LAMBDA:								\
		TREE_VISIT_NODE((node), LAMBDA, proto);			\
		TREE_VISIT_NODE((node), LAMBDA, body);			\
		break;											\
														\
	case CFG_NODE_FUNC_CALL:							\
		TREE_VISIT_NODE((node), FUNC_CALL, ident);		\
		TREE_VISIT_NODE((node), FUNC_CALL, params);		\
		break;											\
														\
	case CFG_NODE_TUPLE_DECL:							\
		TREE_VISIT_NODE((node), TUPLE_DECL, items);		\
		break;											\
														\
	case CFG_NODE_TUPLE_DECL_ITEM:						\
		TREE_VISIT_ATOM((node), TUPLE_DECL_ITEM, name);	\
		TREE_VISIT_NODE((node), TUPLE_DECL_ITEM, type);	\
		break;											\
														\
	case CFG_NODE_TUPLE_LIT:							\
		TREE_VISIT_NODE((node), TUPLE_LIT, items);		\
		break;											\
														\
	case CFG_NODE_TUPLE_LIT_ITEM:						\
		TREE_VISIT_ATOM((node), TUPLE_LIT_ITEM, name);	\
		TREE_VISIT_NODE((node), TUPLE_LIT_ITEM, value);	\
		break;											\
														\
	case CFG_NODE_ARRAY_LIT:							\
		TREE_VISIT_NODE((node), ARRAY_LIT, items);		\
		break;											\
														\
	case CFG_NODE_NUM_LIT:								\
		break;											\
														\
	case CFG_NODE_STR_LIT:								\
		break;											\
														\
	case CFG_NODE_IDENT:								\
		break;											\
														\
	case CFG_NODES_LEN:									\
		assert(false);									\
		break;											\
														\
	}													\

#endif
