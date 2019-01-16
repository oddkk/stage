#include "config_func.h"
#include <stdlib.h>
#include "utils.h"

struct cfg_func_node *
cfg_func_call(struct vm *vm, struct cfg_func_node *func)
{
	struct cfg_func_node *node;

	node = calloc(1, sizeof(struct cfg_func_node));

	node->type = CFG_FUNC_NODE_FUNC_CALL;
	node->func_call.func = func;
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
cfg_func_lookup(struct vm *vm, struct atom *name,
				struct cfg_func_node *scope,
				enum cfg_func_lookup_mode lookup_mode)
{
	struct cfg_func_node *node;

	node = calloc(1, sizeof(struct cfg_func_node));

	if (lookup_mode == CFG_FUNC_LOOKUP_LOCAL) {
		node->type = CFG_FUNC_NODE_LOOKUP_LOCAL;
	} else {
		node->type = CFG_FUNC_NODE_LOOKUP_GLOBAL;
	}

	node->lookup.name = name;
	node->lookup.scope = scope;

	return node;
}

struct cfg_func_node *
cfg_func_scope(struct vm *vm, struct scope *value)
{
	struct cfg_func_node *node;

	node = calloc(1, sizeof(struct cfg_func_node));

	node->type = CFG_FUNC_NODE_SCOPE;
	node->scope = value;

	return node;
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

static int
cfg_func_eval_lookup(struct vm *vm, struct exec_stack *stack,
					 struct cfg_func_node *node, struct scope_entry *result)
{
	assert(node->type == CFG_FUNC_NODE_LOOKUP_LOCAL ||
		   node->type == CFG_FUNC_NODE_LOOKUP_GLOBAL);

	struct object out_scope;
	int err;

	err = cfg_func_eval(vm, stack, node->lookup.scope, &out_scope);

	if (err) {
		return err;
	}

	struct scope *scope = NULL;

	if (node->type == CFG_FUNC_NODE_LOOKUP_GLOBAL) {
		if (out_scope.type != TYPE_SCOPE) {
			printf("not a scope\n");
			return -1;
		}

		scope = *(struct scope **)out_scope.data;
		stack_pop_void(stack, sizeof(struct scope *));

	} else {
		if (out_scope.type == TYPE_SCOPE) {
			scope = *(struct scope **)out_scope.data;
			stack_pop_void(stack, sizeof(struct scope *));
		} else {
			struct type *target_type = get_type(&vm->store, out_scope.type);
			scope = target_type->object_scope;
		}
	}

	assert(scope);

	if (node->type == CFG_FUNC_NODE_LOOKUP_GLOBAL) {
		err = scope_lookup(scope, node->lookup.name, result);
	} else {
		err = scope_local_lookup(scope, node->lookup.name, result);
	}

	if (err) {
		printf("'%.*s' was not found.\n", ALIT(node->lookup.name));
	}

	return err;
}

int
cfg_func_eval(struct vm *vm, struct exec_stack *stack,
			  struct cfg_func_node *node, struct object *out)
{
	type_id out_type = TYPE_NONE;

	switch (node->type) {
	case CFG_FUNC_NODE_FUNC_CALL: {
		struct cfg_func_node *arg = node->func_call.args;
		int err;

		uint8_t *prev_bp = stack->bp;
		stack->bp = stack->sp;

		size_t num_args = 0;
		while (arg) {
			struct object arg_obj;

			err = cfg_func_eval(vm, stack, arg, &arg_obj);
			if (err) {
				return err;
			}

			num_args += 1;
			arg = arg->next_arg;
		}

		struct object func_obj;

		err = cfg_func_eval(vm, stack, node->func_call.func, &func_obj);
		if (err) {
			return err;
		}

		struct type *type = get_type(&vm->store, func_obj.type);

		if (type->base != &vm->default_types.func_base) {
			printf("Not a function.\n");
			return -1;
		}

		struct type_func *type_func = type->data;

		if (type_func->num_params != num_args) {
			printf("Wrong number of arguments. Expected %zu, got %zu.\n",
				   type_func->num_params, num_args);
			return -1;
		}

		struct obj_builtin_func_data func;
		assert(type->size == sizeof(struct obj_builtin_func_data));

		stack_pop(stack, &func, sizeof(struct obj_builtin_func_data));

		func.func(vm, stack, func.data);

		stack->bp = prev_bp;

		out_type = type_func->ret;
	} break;

	case CFG_FUNC_NODE_LOOKUP_LOCAL:
	case CFG_FUNC_NODE_LOOKUP_GLOBAL: {
		int err;
		struct scope_entry result;

		err = cfg_func_eval_lookup(vm, stack, node, &result);
		if (err) {
			return -1;
		}

		if (result.object.type == TYPE_NONE) {
			assert(result.scope);
			stack_push(stack, &result.scope, sizeof(struct scope *));
			out_type = TYPE_SCOPE;
		} else {
			struct type *res_type = get_type(&vm->store, result.object.type);
			if (result.anchor == SCOPE_ANCHOR_STACK) {
				stack_push(stack, stack->bp + (size_t)result.object.data, res_type->size);
			} else {
				stack_push(stack, result.object.data, res_type->size);
			}
			out_type = result.object.type;
		}
	} break;

	case CFG_FUNC_NODE_SCOPE: {
		stack_push(stack, &node->scope, sizeof(struct scope *));
		out_type = TYPE_SCOPE;
	} break;

	case CFG_FUNC_NODE_STACK: {
		struct type *obj_type = get_type(&vm->store, node->obj.type);
		stack_push(stack, stack->bp + (size_t)node->obj.data, obj_type->size);
		out_type = node->obj.type;
	} break;

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

int
cfg_func_eval_simple(struct vm *vm,
					 struct cfg_func_node *node,
					 struct object *out)
{
	struct exec_stack stack = {0};
	struct arena mem = arena_push(&vm->memory);

	arena_alloc_stack(&stack, &mem, 1024); //mem.capacity - mem.head - 1);

	int err;
	err = cfg_func_eval(vm, &stack, node, out);

	arena_pop(&vm->memory, mem);

	return err;
}

enum cfg_func_simplify_result {
	CFG_SIMPLIFY_ERROR = 0x0,
	CFG_SIMPLIFY_OK = 0x1,
	CFG_SIMPLIFY_CONST = 0x2,

	CFG_SIMPLIFY_ALL =
		CFG_SIMPLIFY_OK |
		CFG_SIMPLIFY_CONST,
};

static int
cfg_func_do_simplify(struct vm *vm, struct cfg_func_node *node)
{
	printf("\nsimplify:\n");
	cfg_func_print(vm, node);

	struct object result;
	int err;
	err = cfg_func_eval_simple(vm, node, &result);

	if (err) {
		return err;
	}

	obj_id new_obj = register_object(&vm->store, result);

	node->type = CFG_FUNC_NODE_GLOBAL;
	node->obj = get_object(&vm->store, new_obj);

	// @TODO: Free orphaned nodes.

	return 0;
}

static enum cfg_func_simplify_result
cfg_func_simplify_internal(struct vm *vm, struct cfg_func_node *node)
{
	assert(node);

	enum cfg_func_simplify_result result;
	result = CFG_SIMPLIFY_OK;

	switch (node->type) {

	case CFG_FUNC_NODE_FUNC_CALL: {
		result = CFG_SIMPLIFY_ALL;

		result &= cfg_func_simplify_internal(vm, node->func_call.func);

		struct cfg_func_node *arg;
		arg = node->func_call.args;

		while (arg) {
			result &= cfg_func_simplify_internal(vm, arg);
			arg = arg->next_arg;
		}

		if ((result & CFG_SIMPLIFY_CONST) == 0) {
			// @TODO: Checking constness again is unnessecary. It is
			// done now because we want to evaluate from as high in
			// the call tree as possible to reduce the number of
			// values we have to cache in the object store.

			enum cfg_func_simplify_result res;
			res = cfg_func_simplify_internal(vm, node->func_call.func);
			if ((res & CFG_SIMPLIFY_CONST) != 0) {
				cfg_func_do_simplify(vm, node->func_call.func);
			}

			while (arg) {
				res = cfg_func_simplify_internal(vm, arg);
				if ((res & CFG_SIMPLIFY_CONST) != 0) {
					cfg_func_do_simplify(vm, arg);
				}
				arg = arg->next_arg;
			}
		}
	} break;

	case CFG_FUNC_NODE_LOOKUP_LOCAL:
	case CFG_FUNC_NODE_LOOKUP_GLOBAL: {
		result = cfg_func_simplify_internal(vm, node->lookup.scope);

		if ((result & CFG_SIMPLIFY_OK) != 0) {
			break;
		}

		if ((result & CFG_SIMPLIFY_CONST) != 0) {
			struct scope_entry entry;
			int err;

			struct exec_stack stack = {0};
			struct arena mem = arena_push(&vm->memory);

			arena_alloc_stack(&stack, &mem, 1024); //mem.capacity - mem.head - 1);
			err = cfg_func_eval_lookup(vm, &stack, node, &entry);
			arena_pop(&vm->memory, mem);

			if (err) {
				return CFG_SIMPLIFY_ERROR;
			}


			if (entry.object.type != TYPE_NONE &&
				entry.anchor == SCOPE_ANCHOR_STACK) {
				result &= ~CFG_SIMPLIFY_CONST;

				node->type = CFG_FUNC_NODE_STACK;
				node->obj = entry.object;
				// @TODO: Free orphaned nodes.
			}

		} else {
			// @TODO: Is this allowed?
			panic("Non-constant l-expr");
		}
	} break;

	case CFG_FUNC_NODE_GLOBAL:
		result = CFG_SIMPLIFY_CONST;
		break;

	case CFG_FUNC_NODE_STACK:
		break;

	case CFG_FUNC_NODE_SCOPE:
		result = CFG_SIMPLIFY_CONST;
		break;

	case CFG_FUNC_NODE_LIT_INT:
		result = CFG_SIMPLIFY_CONST;
		break;

	case CFG_FUNC_NODE_LIT_STR:
		result = CFG_SIMPLIFY_CONST;
		break;
	}

	return result;
}

void
cfg_func_simplify(struct vm *vm, struct cfg_func_node *node)
{
	enum cfg_func_simplify_result result;
	result = cfg_func_simplify_internal(vm, node);

	if ((result & CFG_SIMPLIFY_CONST) != 0) {
		cfg_func_do_simplify(vm, node);
	}
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
	case CFG_FUNC_NODE_LOOKUP_GLOBAL:
		printf("%.*s (global)", ALIT(node->lookup.name));
		break;

	case CFG_FUNC_NODE_LOOKUP_LOCAL:
		printf("%.*s (local)", ALIT(node->lookup.name));
		break;

	case CFG_FUNC_NODE_SCOPE:
		printf("scope\n");
		break;

	case CFG_FUNC_NODE_FUNC_CALL:
		printf("call");
		break;

	case CFG_FUNC_NODE_GLOBAL: {
		printf("global ");
		print_obj_repr(vm, node->obj);
	} break;

	case CFG_FUNC_NODE_STACK: {
		printf("local bp + 0x%lx ", (size_t)node->obj.data);
		print_type_repr(vm, get_type(&vm->store, node->obj.type));
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
		_print_indent(depth + 1);
		printf("func\n");
		cfg_func_print_internal(vm, node->func_call.func, depth + 2);
		_print_indent(depth + 1);
		printf("args\n");
		cfg_func_print_internal(vm, node->func_call.args, depth + 2);
		break;

	case CFG_FUNC_NODE_LOOKUP_GLOBAL:
	case CFG_FUNC_NODE_LOOKUP_LOCAL:
		cfg_func_print_internal(vm, node->lookup.scope, depth + 1);

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
