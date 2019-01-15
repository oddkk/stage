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

	*p = arg;
}

struct cfg_func_node *
cfg_func_global(struct vm *vm, struct object value)
{
	struct cfg_func_node *node;

	node = calloc(1, sizeof(struct cfg_func_node));

	node->type = CFG_FUNC_NODE_GLOBAL;
	node->obj = value;

	return node;
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

int
cfg_func_eval(struct vm *vm, struct exec_stack *stack,
			  struct cfg_func_node *node, struct object *out)
{
	type_id out_type = TYPE_NONE;

	switch (node->type) {
	case CFG_FUNC_NODE_FUNC_CALL:
		printf("TODO: Func call\n");
		break;

	case CFG_FUNC_NODE_GLOBAL: {
		struct type *obj_type = get_type(&vm->store, node->obj.type);
		stack_push(stack, node->obj.data, obj_type->size);
		out_type = node->obj.type;
	} break;

	case CFG_FUNC_NODE_LIT_INT:
		stack_push(stack, &node->lit_int, sizeof(node->lit_int));
		out_type = vm->default_types.integer;
		break;

	case CFG_FUNC_NODE_LIT_STR:
		stack_push(stack, &node->lit_str, sizeof(node->lit_str));
		out_type = vm->default_types.string;
		break;
	}

	if (out) {
		struct type *type;
		struct object res = {0};

		type = get_type(&vm->store, out_type);
		res.data = stack->sp - type->size;
		res.type = out_type;
		*out = res;
	}

	return 0;
}

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static void
cfg_func_print_internal(struct vm *vm, struct cfg_func_node *node, int depth)
{
	if (!node) {
		return;
	}

	_print_indent(depth);

	switch (node->type) {
	case CFG_FUNC_NODE_FUNC_CALL:
		printf("%.*s", ALIT(node->func_call.name));
		break;

	case CFG_FUNC_NODE_GLOBAL: {
		printf("global ");
		/* struct type *type = get_type(&vm->store, node->global.type); */
		print_obj_repr(vm, node->obj);
	} break;

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
		cfg_func_print_internal(vm, node->func_call.args, depth + 1);
		break;

	default:
		break;
	}

	cfg_func_print_internal(vm, node->next_arg, depth);
}

void
cfg_func_print(struct vm *vm, struct cfg_func_node *node)
{
	cfg_func_print_internal(vm, node, 0);
}
