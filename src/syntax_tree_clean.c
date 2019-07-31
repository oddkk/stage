#include "syntax_tree.h"
#include "utils.h"

struct st_node_ll {
	struct st_node *first, *last;
};

struct st_node_ll _st_clean_list(struct st_node *node) {
	assert(node->type == ST_NODE_INTERNAL_LIST);

	struct st_node_ll res = {0};

	if (!node->INTERNAL_LIST.tail) {
		res.first = node->INTERNAL_LIST.head;
		res.last = res.first;
	} else {
		res = _st_clean_list(node->INTERNAL_LIST.tail);

		if (res.last) {
			res.last->next_sibling = node->INTERNAL_LIST.head;
		} else {
			assert(!res.first);
			res.first = node->INTERNAL_LIST.head;
		}

		if (node->INTERNAL_LIST.head) {
			res.last = node->INTERNAL_LIST.head;
		}

	}

	return res;
}

void st_clean(struct st_node **node) {
	if (!(*node)) {
		return;
	}

	switch ((*node)->type) {
	case ST_NODE_INTERNAL_LIST: {
		struct st_node_ll ll = _st_clean_list(*node);
		*node = ll.first;
	} break;

	default:
		break;
	}

	if (!(*node)) {
		return;
	}

#define TREE_VISIT_NODE(node, type, member) st_clean(&node->type.member)
#define TREE_VISIT_ATOM(node, type, member)
	ST_NODE_VISIT(*node)
#undef TREE_VISIT_NODE
#undef TREE_VISIT_ATOM

	st_clean(&(*node)->next_sibling);
}
