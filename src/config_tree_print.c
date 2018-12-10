#include "config.h"
#include "utils.h"
#include <stdio.h>

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static char *_cfg_bin_op_sym(enum cfg_bin_op op) {
	switch (op) {
	case CFG_OP_ADD:   return "+";
	case CFG_OP_SUB:   return "-";
	case CFG_OP_MUL:   return "*";
	case CFG_OP_DIV:   return "/";
	case CFG_OP_RANGE: return "..";
	case CFG_OP_EQ:    return "==";
	case CFG_OP_NEQ:   return "!=";
	case CFG_OP_GTE:   return ">=";
	case CFG_OP_LTE:   return "<=";
	case CFG_OP_GT:    return ">";
	case CFG_OP_LT:    return "<";
	}
}

#define TREE_PRINT_VISIT(node, type, member)		\
	_print_indent(depth + 1);						\
	printf(#member ":\n");							\
	_cfg_tree_print(node->type.member, depth + 2);

#define TREE_PRINT_ATOM(node, type, member)					\
	_print_indent(depth + 1);								\
	printf(#member ": %.*s\n", ALIT(node->type.member));

static void _cfg_tree_print(struct cfg_node *node, int depth);

static void _cfg_tree_print_list(struct cfg_node *node, int depth) {
	if (!node) {
		return;
	}
	assert(node->type == CFG_NODE_INTERNAL_LIST);
	_cfg_tree_print_list(node->INTERNAL_LIST.tail, depth);
	_cfg_tree_print(node->INTERNAL_LIST.head, depth);
}

static void _cfg_tree_print(struct cfg_node *node, int depth) {
	_print_indent(depth);

	if (!node) {
		printf("(none)\n");
		return;
	}

	printf("%.*s:\n", LIT(cfg_node_names[node->type]));

	switch (node->type) {
	case CFG_NODE_INTERNAL_LIST:
		_cfg_tree_print_list(node, depth + 1);
		break;

	/* case CFG_NODE_INTERNAL_LIST: { */

	/* 	struct cfg_node *n = node; */

	/* 	while (n) { */
	/* 		_cfg_tree_print(n->INTERNAL_LIST.head, depth + 1); */
	/* 		n = n->INTERNAL_LIST.tail; */
	/* 	} */
	/* } break; */

	case CFG_NODE_MODULE:
		TREE_PRINT_VISIT(node, MODULE, body);
		break;

	case CFG_NODE_STMT:
		TREE_PRINT_VISIT(node, STMT, attrs);
		TREE_PRINT_VISIT(node, STMT, stmt);
		break;

	case CFG_NODE_DECL_STMT:
		TREE_PRINT_VISIT(node, DECL_STMT, name)
		TREE_PRINT_VISIT(node, DECL_STMT, args);
		TREE_PRINT_VISIT(node, DECL_STMT, decl);
		break;

	case CFG_NODE_OBJ_DECL:
		TREE_PRINT_VISIT(node, OBJ_DECL, ident);
		TREE_PRINT_VISIT(node, OBJ_DECL, body);
		break;

	case CFG_NODE_ENUM_DECL:
		_cfg_tree_print(node->ENUM_DECL.items, depth + 1);
		break;

	case CFG_NODE_ENUM_ITEM:
		TREE_PRINT_ATOM(node, ENUM_ITEM, name)
		TREE_PRINT_VISIT(node, ENUM_ITEM, data);
		break;

	case CFG_NODE_USE_STMT:
		TREE_PRINT_VISIT(node, USE_STMT, ident);
		break;

	case CFG_NODE_USE_ALL:
		break;

	case CFG_NODE_FUNC_STMT:
		TREE_PRINT_VISIT(node, FUNC_STMT, ident);
		TREE_PRINT_VISIT(node, FUNC_STMT, proto);
		TREE_PRINT_VISIT(node, FUNC_STMT, body);
		break;

	case CFG_NODE_FUNC_PROTO:
		TREE_PRINT_VISIT(node, FUNC_PROTO, params);
		TREE_PRINT_VISIT(node, FUNC_PROTO, ret);
		break;

	case CFG_NODE_ASSIGN_STMT:
		TREE_PRINT_VISIT(node, ASSIGN_STMT, ident);
		TREE_PRINT_VISIT(node, ASSIGN_STMT, type);
		TREE_PRINT_VISIT(node, ASSIGN_STMT, body);
		break;

	case CFG_NODE_BIND:
		TREE_PRINT_VISIT(node, BIND, src);
		TREE_PRINT_VISIT(node, BIND, drain);
		break;

	case CFG_NODE_NAMESPACE:
		TREE_PRINT_VISIT(node, NAMESPACE, name);
		TREE_PRINT_VISIT(node, NAMESPACE, body);
		break;

	case CFG_NODE_TEMPLATE_VAR:
		TREE_PRINT_ATOM(node, TEMPLATE_VAR, name);
		break;

	case CFG_NODE_ACCESS:
		TREE_PRINT_VISIT(node, ACCESS, lhs);
		TREE_PRINT_VISIT(node, ACCESS, rhs);
		break;

	case CFG_NODE_SUBSCRIPT:
		TREE_PRINT_VISIT(node, SUBSCRIPT, lhs);
		TREE_PRINT_VISIT(node, SUBSCRIPT, index);
		break;

	case CFG_NODE_BIN_OP:
		_print_indent(depth + 1);
		printf("op: %s\n", _cfg_bin_op_sym(node->BIN_OP.op));
		TREE_PRINT_VISIT(node, BIN_OP, lhs);
		TREE_PRINT_VISIT(node, BIN_OP, rhs);
		break;

	case CFG_NODE_LAMBDA:
		TREE_PRINT_VISIT(node, LAMBDA, proto);
		TREE_PRINT_VISIT(node, LAMBDA, body);
		break;

	case CFG_NODE_FUNC_CALL:
		TREE_PRINT_VISIT(node, FUNC_CALL, ident);
		TREE_PRINT_VISIT(node, FUNC_CALL, params);
		break;

	case CFG_NODE_TUPLE_DECL:
		_print_indent(depth + 1);
		printf("named: %s\n", node->TUPLE_DECL.named ? "true" : "false");
		TREE_PRINT_VISIT(node, TUPLE_DECL, items);
		break;

	case CFG_NODE_TUPLE_DECL_ITEM:
		TREE_PRINT_ATOM(node, TUPLE_DECL_ITEM, name);
		TREE_PRINT_VISIT(node, TUPLE_DECL_ITEM, type);
		break;

	case CFG_NODE_TUPLE_LIT:
		TREE_PRINT_VISIT(node, TUPLE_LIT, items);
		break;

	case CFG_NODE_TUPLE_LIT_ITEM:
		TREE_PRINT_ATOM(node, TUPLE_LIT_ITEM, name);
		TREE_PRINT_VISIT(node, TUPLE_LIT_ITEM, value);
		break;

	case CFG_NODE_ARRAY_LIT:
		TREE_PRINT_VISIT(node, ARRAY_LIT, items);
		break;

	case CFG_NODE_NUM_LIT:
		_print_indent(depth + 1);
		printf("%i\n", node->NUM_LIT);
		break;

	case CFG_NODE_IDENT:
		_print_indent(depth + 1);
		printf("value: %.*s\n", ALIT(node->IDENT));
		break;

	case CFG_NODES_LEN:
		assert(false);
		break;

	}
}

void cfg_tree_print(struct cfg_node *node) {
	_cfg_tree_print(node, 0);
}
