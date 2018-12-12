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

static void _cfg_tree_print(struct cfg_node *node, int depth) {
	_print_indent(depth);

	if (!node) {
		printf("(none)\n");
		return;
	}

	printf("%.*s:\n", LIT(cfg_node_names[node->type]));

	switch (node->type) {
	case CFG_NODE_BIN_OP:
		_print_indent(depth + 1);
		printf("op: %s\n", _cfg_bin_op_sym(node->BIN_OP.op));
		break;

	case CFG_NODE_TUPLE_DECL:
		_print_indent(depth + 1);
		printf("named: %s\n", node->TUPLE_DECL.named ? "true" : "false");
		break;

	case CFG_NODE_NUM_LIT:
		_print_indent(depth + 1);
		printf("%li\n", node->NUM_LIT);
		break;

	case CFG_NODE_STR_LIT:
		_print_indent(depth + 1);
		printf("\"%.*s\"\n", LIT(node->STR_LIT));
		break;

	case CFG_NODE_IDENT:
		_print_indent(depth + 1);
		printf("value: %.*s\n", ALIT(node->IDENT));
		break;

	default:
		break;

	}

#define TREE_VISIT_NODE(node, type, member) TREE_PRINT_VISIT(node, type, member)
#define TREE_VISIT_ATOM(node, type, member) TREE_PRINT_ATOM(node, type, member)
	CFG_NODE_VISIT(node)
#undef TREE_VISIT_NODE
#undef TREE_VISIT_ATOM

	if (node->next_sibling) {
		_cfg_tree_print(node->next_sibling, depth);
	}
}

void cfg_tree_print(struct cfg_node *node) {
	_cfg_tree_print(node, 0);
}
