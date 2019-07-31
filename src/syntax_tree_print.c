#include "syntax_tree.h"
#include "utils.h"
#include <stdio.h>

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

#define TREE_PRINT_VISIT(node, type, member)		\
	_print_indent(depth + 1);						\
	printf(#member ":\n");							\
	_st_print(node->type.member, depth + 2);

#define TREE_PRINT_ATOM(node, type, member)					\
	_print_indent(depth + 1);								\
	printf(#member ": %.*s\n", ALIT(node->type.member));

static void _st_print(struct st_node *node, int depth) {
	_print_indent(depth);

	if (!node) {
		printf("(none)\n");
		return;
	}

	printf("%.*s:\n", LIT(st_node_names[node->type]));

	switch (node->type) {
	case ST_NODE_BIN_OP:
		_print_indent(depth + 1);
		printf("op: %.*s\n", LIT(st_bin_op_sym[node->BIN_OP.op]));
		break;

	case ST_NODE_TUPLE_DECL:
		_print_indent(depth + 1);
		printf("named: %s\n", node->TUPLE_DECL.named ? "true" : "false");
		break;

	case ST_NODE_NUM_LIT:
		_print_indent(depth + 1);
		printf("%li\n", node->NUM_LIT);
		break;

	case ST_NODE_STR_LIT:
		_print_indent(depth + 1);
		printf("\"%.*s\"\n", LIT(node->STR_LIT));
		break;

	case ST_NODE_IDENT:
		_print_indent(depth + 1);
		printf("value: %.*s\n", ALIT(node->IDENT));
		break;

	default:
		break;

	}

#define TREE_VISIT_NODE(node, type, member) TREE_PRINT_VISIT(node, type, member)
#define TREE_VISIT_ATOM(node, type, member) TREE_PRINT_ATOM(node, type, member)
	ST_NODE_VISIT(node)
#undef TREE_VISIT_NODE
#undef TREE_VISIT_ATOM

	if (node->next_sibling) {
		_st_print(node->next_sibling, depth);
	}
}

void st_print(struct st_node *node) {
	_st_print(node, 0);
}
