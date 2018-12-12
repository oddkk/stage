#ifndef STAGE_CONFIG_H
#define STAGE_CONFIG_H

#include "vm.h"

enum cfg_bin_op {
	CFG_OP_ADD,
	CFG_OP_SUB,
	CFG_OP_MUL,
	CFG_OP_DIV,
	CFG_OP_RANGE,
	CFG_OP_EQ,
	CFG_OP_NEQ,
	CFG_OP_GTE,
	CFG_OP_LTE,
	CFG_OP_GT,
	CFG_OP_LT,
};

#define CFG_NODES \
	CFG_NODE(INTERNAL_LIST, struct {			\
			struct cfg_node *head;				\
			struct cfg_node *tail;				\
	})											\
	CFG_NODE(MODULE, struct {					\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(STMT, struct {						\
			struct cfg_node *attrs;				\
			struct cfg_node *stmt;				\
	})											\
	CFG_NODE(DECL_STMT, struct {				\
			struct cfg_node *name;				\
			struct cfg_node *args;				\
			struct cfg_node *decl;				\
	})											\
	CFG_NODE(OBJ_DECL, struct {					\
			struct cfg_node *ident;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(ENUM_DECL, struct {				\
			struct cfg_node *items;				\
	})											\
	CFG_NODE(ENUM_ITEM, struct {				\
			struct atom *name;					\
			struct cfg_node *data;				\
	})											\
	CFG_NODE(USE_STMT, struct {					\
			struct cfg_node *ident;				\
	})											\
	CFG_NODE(USE_ALL, struct {					\
			int _dc;							\
	})											\
	CFG_NODE(ASSERT_STMT, struct {				\
			struct cfg_node *expr;				\
	})											\
	CFG_NODE(FUNC_STMT, struct {				\
			struct cfg_node *ident;				\
			struct cfg_node *proto;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(FUNC_PROTO, struct {				\
			struct cfg_node *params;			\
			struct cfg_node *ret;				\
	})											\
	CFG_NODE(ASSIGN_STMT, struct {				\
			struct cfg_node *ident;				\
			struct cfg_node *type;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(BIND, struct {						\
			struct cfg_node *src;				\
			struct cfg_node *drain;				\
	})											\
	CFG_NODE(NAMESPACE, struct {				\
			struct cfg_node *name;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(TEMPLATE_VAR, struct {				\
			struct atom *name;					\
	})											\
	CFG_NODE(ACCESS, struct {					\
			struct cfg_node *lhs;				\
			struct cfg_node *rhs;				\
	})											\
	CFG_NODE(SUBSCRIPT, struct {				\
			struct cfg_node *lhs;				\
			struct cfg_node *index;				\
	})											\
	CFG_NODE(BIN_OP, struct {					\
			enum cfg_bin_op op;					\
			struct cfg_node *lhs;				\
			struct cfg_node *rhs;				\
	})											\
	CFG_NODE(LAMBDA, struct {					\
			struct cfg_node *proto;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(FUNC_CALL, struct {				\
			struct cfg_node *ident;				\
			struct cfg_node *params;			\
	})											\
	CFG_NODE(TUPLE_DECL, struct {				\
			struct cfg_node *items;				\
			bool named;							\
	})											\
	CFG_NODE(TUPLE_DECL_ITEM, struct {			\
			struct atom *name;					\
			struct cfg_node *type;				\
	})											\
	CFG_NODE(TUPLE_LIT, struct {				\
			struct cfg_node *items;				\
			bool named;							\
	})											\
	CFG_NODE(TUPLE_LIT_ITEM, struct {			\
			struct atom *name;					\
			struct cfg_node *value;				\
	})											\
	CFG_NODE(ARRAY_LIT, struct {				\
			struct cfg_node *items;				\
	})											\
	CFG_NODE(NUM_LIT, int64_t)					\
	CFG_NODE(STR_LIT, struct string)			\
	CFG_NODE(IDENT, struct atom *)				\

enum cfg_node_type {
#define CFG_NODE(name, data) CFG_NODE_##name,
	CFG_NODES
#undef CFG_NODE
	CFG_NODES_LEN
};

struct cfg_location {
	size_t line;
	size_t column;
};

#define CFG_NODE(name, data) typedef data name##_t;
CFG_NODES
#undef CFG_NODE

struct cfg_node {
	enum cfg_node_type type;

	struct cfg_location from;
	struct cfg_location to;

	struct cfg_node *next_sibling;

#define CFG_NODE(name, data) name##_t name;
	union {
		CFG_NODES
	};
#undef CFG_NODE
};

extern struct string cfg_node_names[CFG_NODES_LEN];

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
	case CFG_NODE_SUBSCRIPT:							\
		TREE_VISIT_NODE((node), SUBSCRIPT, lhs);		\
		TREE_VISIT_NODE((node), SUBSCRIPT, index);		\
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


#define CFG_JOBS \
	CFG_JOB(parse_file, struct {				\
			struct string file_name;			\
	})											\
	CFG_JOB(visit_module, struct {				\
			struct scope *root_scope;	\
			struct cfg_node *node;				\
	})											\

enum cfg_job_type {
#define CFG_JOB(name, data) CFG_JOB_##name,
	CFG_JOBS
#undef CFG_JOB
	CFG_JOBS_LEN
};

extern struct string cfg_job_names[CFG_JOBS_LEN];

void cfg_tree_print(struct cfg_node *node);

int cfg_compile(struct vm *vm, struct string cfg_dir);

#endif
