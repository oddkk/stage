#include "config.h"
#include "stage.h"
#include "utils.h"
#include <stdio.h>

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static char *_binary_op_name(enum config_binary_op op) {
	switch (op) {
	case CONFIG_OP_ASSIGN:    return "=";
	case CONFIG_OP_BIND:      return "<-";
	case CONFIG_OP_ADD:       return "+";
	case CONFIG_OP_SUB:       return "-";
	case CONFIG_OP_MUL:       return "*";
	case CONFIG_OP_DIV:       return "/";
	case CONFIG_OP_ACCESS:    return ".";
	case CONFIG_OP_SUBSCRIPT: return "[]";
	}
}

static void _config_print_tree(struct config_node *node, int depth) {
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
			   node->module.version.minor,
			   node->module.version.patch);
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

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->type_decl.type, depth + 2);
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
	}

	if (node->next_sibling) {
		_config_print_tree(node->next_sibling, depth);
	}
}

void config_print_tree(struct config_node *node) {
	_config_print_tree(node, 0);
}

static int _apply_config_device_type(struct scoped_hash *parent_scope, struct config_node *node)
{
	struct scoped_hash *scope;

	scope = scoped_hash_push(parent_scope);
	if (!scope) {
		return -1;
	}

	assert(node->device_type.name);
	scoped_hash_insert(parent_scope, node->device_type.name, SCOPE_ENTRY_DEVICE_TYPE, -1, node, scope);

	return 0;
}

static int _apply_config_device(struct scoped_hash *parent_scope, struct config_node *node)
{
	struct scoped_hash *scope;

	scope = scoped_hash_push(parent_scope);
	if (!scope) {
		return -1;
	}

	if (node->device.name) {
		scoped_hash_insert(parent_scope, node->device.name, SCOPE_ENTRY_DEVICE, -1, node, scope);
	}

	return 0;
}

static int _apply_config_type_decl(struct scoped_hash *parent_scope, struct config_node *node)
{
	if (node->device.name) {
		scoped_hash_insert(parent_scope, node->type_decl.name, SCOPE_ENTRY_TYPE, -1, node, NULL);
	}

	return 0;
}

static int _apply_config_scope(struct scoped_hash *scope, struct config_node *first_child)
{
	switch (first_child->type) {
	case CONFIG_NODE_DEVICE_TYPE:
		return _apply_config_device_type(scope, first_child);

	case CONFIG_NODE_DEVICE:
		return _apply_config_device(scope, first_child);

	case CONFIG_NODE_TYPE_DECL:
		return _apply_config_type_decl(scope, first_child);

	default:
		print_error("apply config", "Got unexpected node %i", first_child->type);
		return -1;
	}

	return 0;
}

struct resolve_dep_context {
	int time;
};

static struct config_node *_resolve_expression_dependency(struct config_node *stmt, struct config_node *node, struct scoped_hash *scope, struct resolve_dep_context *ctx)
{
	ctx->time += 1;

	node->dfs.discovered = ctx->time;
	node->dfs.color = DFS_GRAY;

	switch (node->type) {
	/* case CONFIG_NODE_MODULE: */
	/* 	break; */
			
	/* case CONFIG_NODE_DEVICE_TYPE: */
	/* 	break; */
			
	/* case CONFIG_NODE_DEVICE: */
	/* 	break; */
			
	/* case CONFIG_NODE_TYPE_DECL: */
	/* 	break; */
			
	/* case CONFIG_NODE_ATTR: */
	/* 	break; */
			
	/* case CONFIG_NODE_INPUT: */
	/* 	break; */
			
	/* case CONFIG_NODE_OUTPUT: */
	/* 	break; */
			
	case CONFIG_NODE_BINARY_OP: {
		if (node->binary_op.op == CONFIG_OP_ACCESS) {
			
		}
	} break;
			
	case CONFIG_NODE_SUBSCRIPT_RANGE:
		break;
			
	case CONFIG_NODE_IDENT: {

	} break;
			
	case CONFIG_NODE_NUMLIT:
		break;

	default:
		print_error("apply config", "Invalid node in expression, %i.", node->type);
		break;
	}
	

	ctx->time += 1;

	node->dfs.color = DFS_BLACK;
	node->dfs.finished = ctx->time;

	return 0;
}

static void _resolve_dependency_order(struct config_node *node, struct resolve_dep_context *ctx)
{
	ctx->time += 1;
	node->dfs.discovered = ctx->time;
	node->dfs.color = DFS_GRAY;

	switch (node->type) {
	case CONFIG_NODE_MODULE: {
		struct config_node *current_child = node->module.first_child;

		while (current_child) {
			if (current_child->dfs.color == DFS_WHITE) {
				_resolve_dependency_order(current_child, ctx);
			}

			current_child = current_child->next_sibling;
		}

	} break;
			
	case CONFIG_NODE_DEVICE_TYPE:
		break;
			
	case CONFIG_NODE_DEVICE:
		//_resolve_expression_dependency(node, node->device.type);
		(void)_resolve_expression_dependency;
		break;
			
	case CONFIG_NODE_TYPE_DECL:
		break;
			
	case CONFIG_NODE_ATTR:
		break;
			
	case CONFIG_NODE_INPUT:
		break;
			
	case CONFIG_NODE_OUTPUT:
		break;
			
	case CONFIG_NODE_BINARY_OP:
		break;
			
	case CONFIG_NODE_SUBSCRIPT_RANGE:
		break;
			
	case CONFIG_NODE_IDENT:
		break;
			
	case CONFIG_NODE_NUMLIT:
		break;
			
	}

	ctx->time += 1;

	node->dfs.color = DFS_BLACK;
	node->dfs.finished = ctx->time;
}

int apply_config(struct stage *stage, struct config_node *root)
{
	int err;
	struct resolve_dep_context dependency_context;

	assert(root->type == CONFIG_NODE_MODULE);

	err = _apply_config_scope(&stage->root_scope, root->module.first_child);
	if (err < 0) {
		return err;
	}

	_resolve_dependency_order(root, &dependency_context);

	return 0;
}
