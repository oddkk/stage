#include "config.h"
#include "utils.h"

struct cfg_node_ll {
	struct cfg_node *first, *last;
};

struct cfg_node_ll _cfg_tree_clean_list(struct cfg_node *node) {
	assert(node->type == CFG_NODE_INTERNAL_LIST);

	if (!node->INTERNAL_LIST.tail) {
		struct cfg_node_ll res;

		res.first = node->INTERNAL_LIST.head;
		res.last = res.first;

		return res;
	} else {
		struct cfg_node_ll res;

		res = _cfg_tree_clean_list(node->INTERNAL_LIST.tail);

		res.last->next_sibling = node->INTERNAL_LIST.head;
		res.last = node->INTERNAL_LIST.head;

		return res;
	}
}

void cfg_tree_clean(struct cfg_node **node) {
	if (!(*node)) {
		return;
	}

	switch ((*node)->type) {
	case CFG_NODE_INTERNAL_LIST: {
		struct cfg_node_ll ll = _cfg_tree_clean_list(*node);
		*node = ll.first;
	} break;

	default:
		break;
	}

#define TREE_VISIT_NODE(node, type, member) cfg_tree_clean(&node->type.member)
#define TREE_VISIT_ATOM(node, type, member)
	CFG_NODE_VISIT(*node)
#undef TREE_VISIT_NODE
#undef TREE_VISIT_ATOM

	cfg_tree_clean(&(*node)->next_sibling);
}
