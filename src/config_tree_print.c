#include "config.h"
#include <stdio.h>

static void _print_indent(int depth)
{
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static char *_binary_op_name(enum config_binary_op op)
{
	switch (op) {
	case CONFIG_OP_ASSIGN:
		return "=";
	case CONFIG_OP_BIND:
		return "<-";
	case CONFIG_OP_ADD:
		return "+";
	case CONFIG_OP_SUB:
		return "-";
	case CONFIG_OP_MUL:
		return "*";
	case CONFIG_OP_DIV:
		return "/";
	case CONFIG_OP_ACCESS:
		return ".";
	case CONFIG_OP_SUBSCRIPT:
		return "[]";
	}
}

static void _config_print_tree(struct config_node *node, int depth)
{
	if (!node) {
		return;
	}

	_print_indent(depth);

	switch (node->type) {
	case CONFIG_NODE_MODULE:
		printf("module\n");
		_print_indent(depth + 1);
		printf("version: %i.%i.%i\n",
		       node->module.version.major,
		       node->module.version.minor, node->module.version.patch);
		_config_print_tree(node->module.first_child, depth + 1);
		break;
	case CONFIG_NODE_DEVICE_TYPE:
		printf("device_type\n");
		_print_indent(depth + 1);
		printf("name: %.*s\n", LIT(node->device_type.name->name));
		_config_print_tree(node->device_type.first_child, depth + 1);
		break;
	case CONFIG_NODE_DEVICE:
		printf("device\n");

		_print_indent(depth + 1);
		if (node->device.name) {
			printf("name: %.*s\n", LIT(node->device.name->name));
		} else {
			printf("name: (unnamed)\n");
		}

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->device.type, depth + 2);

		_config_print_tree(node->device.first_child, depth + 1);
		break;

	case CONFIG_NODE_TYPE_DECL:
		printf("type_decl\n");

		_print_indent(depth + 1);
		printf("name: %.*s\n", LIT(node->type_decl.name->name));

		_config_print_tree(node->type_decl.type, depth + 1);
		break;

	case CONFIG_NODE_TYPE:
		printf("type:\n");
		_config_print_tree(node->type_def.first_child, depth + 1);
		break;

	case CONFIG_NODE_TUPLE:
		printf("tuple%s:\n", node->tuple.named ? " (named)" : "");
		_config_print_tree(node->tuple.first_child, depth + 1);
		break;

	case CONFIG_NODE_TUPLE_ITEM:
		printf("tuple_item\n");

		_print_indent(depth + 1);
		printf("name: %.*s\n", ALIT(node->tuple_item.name));

		_config_print_tree(node->tuple_item.type, depth + 1);
		break;

	case CONFIG_NODE_ATTR:
		printf("attr\n");

		_print_indent(depth + 1);
		printf("name: %.*s\n", LIT(node->attr.name->name));

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->attr.type, depth + 2);

		_print_indent(depth + 1);
		printf("def_value:\n");
		_config_print_tree(node->attr.def_value, depth + 2);
		break;

	case CONFIG_NODE_INPUT:
		printf("input\n");

		_print_indent(depth + 1);
		if (node->input.name) {
			printf("name: %.*s\n", LIT(node->input.name->name));
		} else {
			printf("name: _\n");
		}

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->input.type, depth + 2);
		break;

	case CONFIG_NODE_OUTPUT:
		printf("output\n");

		_print_indent(depth + 1);
		if (node->output.name) {
			printf("name: %.*s\n", LIT(node->output.name->name));
		} else {
			printf("name: _\n");
		}

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->output.type, depth + 2);
		break;

	case CONFIG_NODE_BINARY_OP:
		printf("binary %s\n", _binary_op_name(node->binary_op.op));

		_print_indent(depth + 1);
		printf("lhs:\n");
		_config_print_tree(node->binary_op.lhs, depth + 2);

		_print_indent(depth + 1);
		printf("rhs:\n");
		_config_print_tree(node->binary_op.rhs, depth + 2);

		break;

	case CONFIG_NODE_SUBSCRIPT_RANGE:
		printf("subscript range\n");

		_print_indent(depth + 1);
		printf("lhs:\n");
		_config_print_tree(node->subscript_range.lhs, depth + 2);

		_print_indent(depth + 1);
		printf("low:\n");
		_config_print_tree(node->subscript_range.low, depth + 2);

		_print_indent(depth + 1);
		printf("high:\n");
		_config_print_tree(node->subscript_range.high, depth + 2);
		break;

	case CONFIG_NODE_SUBRANGE:
		printf("subrange\n");

		_print_indent(depth + 1);
		printf("lhs:\n");
		_config_print_tree(node->subrange.lhs, depth + 2);

		_print_indent(depth + 1);
		printf("low:\n");
		_config_print_tree(node->subrange.low, depth + 2);

		_print_indent(depth + 1);
		printf("high:\n");
		_config_print_tree(node->subrange.high, depth + 2);
		break;

	case CONFIG_NODE_INTERNAL_LIST:
		printf("list_node:\n");

		_print_indent(depth + 1);
		printf("head:\n");
		_config_print_tree(node->internal_list.head, depth + 2);

		_print_indent(depth + 1);
		printf("tail:\n");
		_config_print_tree(node->internal_list.tail, depth + 2);
		break;

	case CONFIG_NODE_IDENT:
		if (node->ident) {
			printf("ident %.*s\n", LIT(node->ident->name));
		} else {
			printf("ident (self)\n");
		}
		break;

	case CONFIG_NODE_NUMLIT:
		printf("numlit %i\n", node->numlit);
		break;

	case CONFIG_NODE_NAMESPACE:
		printf("namespace %.*s\n", ALIT(node->namespace.name));
		_config_print_tree(node->namespace.first_child, depth + 1);
		break;
	}

	if (node->next_sibling) {
		_config_print_tree(node->next_sibling, depth);
	}
}

void config_tree_print(struct config_node *node)
{
	_config_print_tree(node, 0);
}
