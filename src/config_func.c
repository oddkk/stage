#include "config_func.h"
#include <stdlib.h>
#include "utils.h"

struct cfg_func_node *
cfg_func_call(struct vm *vm, struct cfg_func_context *ctx,
			  struct atom *name)
{
	struct cfg_func_node *node;

	node = calloc(1, sizeof(struct cfg_func_node));

	node->type = CFG_FUNC_NODE_FUNC_CALL;
	node->func_call.ctx = ctx;
	node->func_call.name = name;
	node->func_call.args = NULL;

	return node;
}

void
cfg_func_call_add_arg(struct cfg_func_node *func,
						struct cfg_func_node *arg)
{
	assert(func->type == CFG_FUNC_NODE_FUNC_CALL);

	struct cfg_func_node **p;
	p = &func->func_call.args;

	while (*p) {
		p = &(*p)->next_arg;
	}

	(*p)->next_arg = arg;
}

struct cfg_func_node *
cfg_func_lit_int(struct vm *vm, int64_t value)
{
	struct cfg_func_node *node;

	node = calloc(1, sizeof(struct cfg_func_node));

	node->type = CFG_FUNC_NODE_LIT_INT;
	node->lit_int = value;

	return node;
}

struct cfg_func_node *
cfg_func_lit_str(struct vm *vm, struct string value)
{
	struct cfg_func_node *node;

	node = calloc(1, sizeof(struct cfg_func_node));

	node->type = CFG_FUNC_NODE_LIT_STR;
	node->lit_str = value;

	return node;
}

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static void
cfg_func_print_internal(struct cfg_func_node *node, int depth)
{
	if (!node) {
		return;
	}

	_print_indent(depth);

	switch (node->type) {
	case CFG_FUNC_NODE_FUNC_CALL:
		printf("%.*s", ALIT(node->func_call.name));
		break;

	case CFG_FUNC_NODE_GLOBAL:
		printf("global %u", node->global);
		break;

	case CFG_FUNC_NODE_LIT_INT:
		printf("%li", node->lit_int);
		break;

	case CFG_FUNC_NODE_LIT_STR:
		printf("\"%.*s\"", LIT(node->lit_str));
		break;
	}

	printf("\n");

	switch (node->type) {
	case CFG_FUNC_NODE_FUNC_CALL:
		cfg_func_print_internal(node->func_call.args, depth + 1);
		break;

	default:
		break;
	}

	cfg_func_print_internal(node->next_arg, depth);
}

void
cfg_func_print(struct cfg_func_node *node)
{
	cfg_func_print_internal(node, 0);
}
