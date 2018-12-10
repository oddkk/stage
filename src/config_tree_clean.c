#include "config.h"
#include "utils.h"

void cfg_tree_clean(struct cfg_node *node) {
	if (!node) {
		return;
	}

	switch (node->type) {
	case CFG_NODE_INTERNAL_LIST:
		break;

	default:
		break;
	}

#define TREE_VISIT_NODE(node, type, member) cfg_tree_clean(node->type.member)
#define TREE_VISIT_ATOM(node, type, member)
	CFG_NODE_VISIT
#undef TREE_VISIT_NODE
#undef TREE_VISIT_ATOM
}

static void collapse_tree_nodes(struct config_node *tree) {
	if (!tree) {
		return;
	}

	switch (tree->type) {
	case CONFIG_NODE_MODULE:
		collapse_tree_nodes(tree->module.first_child);
		break;

	case CONFIG_NODE_NAMESPACE:
		collapse_tree_nodes(tree->namespace.first_child);
		break;

	case CONFIG_NODE_DEVICE_TYPE:
		collapse_tree_nodes(tree->device_type.first_child);
		collapse_tree_nodes(tree->device_type.params);
		break;

	case CONFIG_NODE_DEVICE:
		collapse_tree_nodes(tree->device.type);
		collapse_tree_nodes(tree->device.first_child);
		collapse_tree_nodes(tree->device.args);
		break;

	case CONFIG_NODE_TYPE_DECL:
		collapse_tree_nodes(tree->type_decl.type);
		break;

	case CONFIG_NODE_TYPE:
		collapse_tree_nodes(tree->type_def.first_child);
		break;

	case CONFIG_NODE_TUPLE:
		collapse_tree_nodes(tree->tuple.first_child);
		break;

	case CONFIG_NODE_TUPLE_ITEM:
		collapse_tree_nodes(tree->tuple_item.type);
		break;

	case CONFIG_NODE_TYPE_TEMPLATE_PARAM:
		collapse_tree_nodes(tree->type_template_param.expr);
		break;

	case CONFIG_NODE_ARRAY_TYPE:
		collapse_tree_nodes(tree->array_type.lhs);
		collapse_tree_nodes(tree->array_type.length);
		break;

	case CONFIG_NODE_ATTR:
		collapse_tree_nodes(tree->attr.type);
		collapse_tree_nodes(tree->attr.def_value);
		break;

	case CONFIG_NODE_INPUT:
		collapse_tree_nodes(tree->input.type);
		break;

	case CONFIG_NODE_OUTPUT:
		collapse_tree_nodes(tree->output.type);
		break;

	case CONFIG_NODE_BINARY_OP:
		collapse_tree_nodes(tree->binary_op.lhs);
		collapse_tree_nodes(tree->binary_op.rhs);
		break;

	case CONFIG_NODE_SUBSCRIPT_RANGE:
		collapse_tree_nodes(tree->subscript_range.lhs);
		collapse_tree_nodes(tree->subscript_range.low);
		collapse_tree_nodes(tree->subscript_range.high);
		break;

	case CONFIG_NODE_SUBRANGE:
		collapse_tree_nodes(tree->subscript_range.lhs);
		collapse_tree_nodes(tree->subscript_range.low);
		collapse_tree_nodes(tree->subscript_range.high);
		break;

	case CONFIG_NODE_TUPLE_LIT:
		collapse_tree_nodes(tree->tuple_lit.first_child);
		break;

	case CONFIG_NODE_TUPLE_LIT_ITEM:
		collapse_tree_nodes(tree->tuple_lit_item.expr);
		break;

	case CONFIG_NODE_ARRAY_LIT:
		collapse_tree_nodes(tree->array_lit.first_child);
		break;


	case CONFIG_NODE_INTERNAL_LIST: {
		struct config_node *head, *tail;

		head = tree->internal_list.head;
		tail = tree->internal_list.tail;

		collapse_tree_nodes(head);
		collapse_tree_nodes(tail);

		if (tail) {
			*tree = *tail;
			tail = tree;
			while (tail->next_sibling) {
				tail = tail->next_sibling;
			}
			tail->next_sibling = head;
		} else {
			*tree = *head;
		}
	} break;

	case CONFIG_NODE_IDENT:
	case CONFIG_NODE_NUMLIT:
		break;
	}

	collapse_tree_nodes(tree->next_sibling);
}

void config_tree_clean(struct config_node *tree) {
	collapse_tree_nodes(tree);
}
