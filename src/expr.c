#include "expr.h"
#include <stdlib.h>
#include "utils.h"

struct expr_node *
expr_call(struct vm *vm, struct expr_node *func)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_FUNC_CALL;
	node->func_call.func = func;
	node->func_call.args = NULL;

	return node;
}

void
expr_call_add_arg(struct expr_node *func,
						struct expr_node *arg)
{
	assert(func->type == EXPR_NODE_FUNC_CALL);

	struct expr_node **p;
	p = &func->func_call.args;

	while (*p) {
		p = &(*p)->next_arg;
	}

	*p = arg;
}

struct expr_node *
expr_lookup(struct vm *vm, struct atom *name,
				struct expr_node *scope,
				enum expr_lookup_mode lookup_mode)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	if (lookup_mode == EXPR_LOOKUP_LOCAL) {
		node->type = EXPR_NODE_LOOKUP_LOCAL;
	} else {
		node->type = EXPR_NODE_LOOKUP_GLOBAL;
	}

	node->lookup.name = name;
	node->lookup.scope = scope;

	return node;
}

struct expr_node *
expr_scope(struct vm *vm, struct scope *value)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_SCOPE;
	node->scope = value;

	return node;
}

struct expr_node *
expr_global(struct vm *vm, struct object value)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_GLOBAL;
	node->obj = value;

	return node;
}

struct expr_node *
expr_lit_int(struct vm *vm, int64_t value)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_LIT_INT;
	node->lit_int = value;

	return node;
}

struct expr_node *
expr_lit_str(struct vm *vm, struct string value)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_LIT_STR;
	node->lit_str = value;

	return node;
}

static int
expr_eval_lookup(struct vm *vm, struct exec_stack *stack,
					 struct expr_node *node, struct scope_entry *result)
{
	assert(node->type == EXPR_NODE_LOOKUP_LOCAL ||
		   node->type == EXPR_NODE_LOOKUP_GLOBAL);

	struct object out_scope;
	int err;

	err = expr_eval(vm, stack, node->lookup.scope, &out_scope);

	if (err) {
		return err;
	}

	struct scope *scope = NULL;

	if (node->type == EXPR_NODE_LOOKUP_GLOBAL) {
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

	if (node->type == EXPR_NODE_LOOKUP_GLOBAL) {
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
expr_eval(struct vm *vm, struct exec_stack *stack,
			  struct expr_node *node, struct object *out)
{
	type_id out_type = TYPE_NONE;

	switch (node->type) {
	case EXPR_NODE_FUNC_CALL: {
		struct expr_node *arg = node->func_call.args;
		int err;

		uint8_t *prev_bp = stack->bp;
		stack->bp = stack->sp;

		size_t num_args = 0;
		while (arg) {
			struct object arg_obj;

			err = expr_eval(vm, stack, arg, &arg_obj);
			if (err) {
				return err;
			}

			num_args += 1;
			arg = arg->next_arg;
		}

		struct object func_obj;

		err = expr_eval(vm, stack, node->func_call.func, &func_obj);
		if (err) {
			return err;
		}

		struct type *type = get_type(&vm->store, func_obj.type);
		assert(!type->base->abstract);

		/* if (type->base != &vm->default_types.func_base) { */
		/* 	printf("Not a function.\n"); */
		/* 	return -1; */
		/* } */

		struct type_func *type_func = type->data;

		if (type_func->num_params != num_args) {
			printf("Wrong number of arguments. Expected %zu, got %zu.\n",
				   type_func->num_params, num_args);
			return -1;
		}

		assert(type->base->eval != NULL);
		type->base->eval(vm, stack, NULL);

		/* struct obj_builtin_func_data func; */
		/* assert(type->size == sizeof(struct obj_builtin_func_data)); */

		/* stack_pop(stack, &func, sizeof(struct obj_builtin_func_data)); */

		/* func.func(vm, stack, func.data); */

		stack->bp = prev_bp;

		out_type = type_func->ret;
	} break;

	case EXPR_NODE_LOOKUP_LOCAL:
	case EXPR_NODE_LOOKUP_GLOBAL: {
		int err;
		struct scope_entry result;

		err = expr_eval_lookup(vm, stack, node, &result);
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

	case EXPR_NODE_SCOPE: {
		stack_push(stack, &node->scope, sizeof(struct scope *));
		out_type = TYPE_SCOPE;
	} break;

	case EXPR_NODE_STACK: {
		struct type *obj_type = get_type(&vm->store, node->obj.type);
		stack_push(stack, stack->bp + (size_t)node->obj.data, obj_type->size);
		out_type = node->obj.type;
	} break;

	case EXPR_NODE_GLOBAL: {
		struct type *obj_type = get_type(&vm->store, node->obj.type);
		stack_push(stack, node->obj.data, obj_type->size);
		out_type = node->obj.type;
	} break;

	case EXPR_NODE_LIT_INT:
		stack_push(stack, &node->lit_int, sizeof(node->lit_int));
		out_type = vm->default_types.integer;
		break;

	case EXPR_NODE_LIT_STR:
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
expr_eval_simple(struct vm *vm,
					 struct expr_node *node,
					 struct object *out)
{
	struct exec_stack stack = {0};
	struct arena mem = arena_push(&vm->memory);

	arena_alloc_stack(&stack, &mem, 1024); //mem.capacity - mem.head - 1);

	int err;
	err = expr_eval(vm, &stack, node, out);

	arena_pop(&vm->memory, mem);

	return err;
}

enum expr_simplify_result {
	CFG_SIMPLIFY_ERROR = 0x0,
	CFG_SIMPLIFY_OK = 0x1,
	CFG_SIMPLIFY_CONST = 0x2,

	CFG_SIMPLIFY_ALL =
		CFG_SIMPLIFY_OK |
		CFG_SIMPLIFY_CONST,
};

static int
expr_do_simplify_const(struct vm *vm, struct expr_node *node)
{
	printf("\nsimplify const:\n");
	expr_print(vm, node);

	struct object result;
	int err;

	err = expr_eval_simple(vm, node, &result);
	if (err) {
		return err;
	}

	obj_id new_obj = register_object(&vm->store, result);

	expr_destroy(node);

	node->type = EXPR_NODE_GLOBAL;
	node->obj = get_object(&vm->store, new_obj);

	return 0;
}

static enum expr_simplify_result
expr_simplify_internal(struct vm *vm, struct expr_node *node)
{
	assert(node);

	enum expr_simplify_result result;
	result = CFG_SIMPLIFY_OK;

	switch (node->type) {

	case EXPR_NODE_FUNC_CALL: {
		result = CFG_SIMPLIFY_ALL;

		result &= expr_simplify_internal(vm, node->func_call.func);

		struct expr_node *arg;
		arg = node->func_call.args;

		while (arg) {
			result &= expr_simplify_internal(vm, arg);
			arg = arg->next_arg;
		}

		if ((result & CFG_SIMPLIFY_CONST) == 0) {
			// @TODO: Checking constness again is unnessecary. It is
			// done now because we want to evaluate from as high in
			// the call tree as possible to reduce the number of
			// values we have to cache in the object store.

			enum expr_simplify_result res;
			res = expr_simplify_internal(vm, node->func_call.func);
			if ((res & CFG_SIMPLIFY_CONST) != 0) {
				expr_do_simplify_const(vm, node->func_call.func);
			}

			while (arg) {
				res = expr_simplify_internal(vm, arg);
				if ((res & CFG_SIMPLIFY_CONST) != 0) {
					expr_do_simplify_const(vm, arg);
				}
				arg = arg->next_arg;
			}
		}
	} break;

	case EXPR_NODE_LOOKUP_LOCAL:
	case EXPR_NODE_LOOKUP_GLOBAL: {
		result = expr_simplify_internal(vm, node->lookup.scope);

		if ((result & CFG_SIMPLIFY_OK) != 0) {
			break;
		}

		if ((result & CFG_SIMPLIFY_CONST) != 0) {
			struct scope_entry entry;
			int err;

			struct exec_stack stack = {0};
			struct arena mem = arena_push(&vm->memory);

			arena_alloc_stack(&stack, &mem, 1024); //mem.capacity - mem.head - 1);
			err = expr_eval_lookup(vm, &stack, node, &entry);
			arena_pop(&vm->memory, mem);

			if (err) {
				return CFG_SIMPLIFY_ERROR;
			}


			if (entry.object.type != TYPE_NONE &&
				entry.anchor == SCOPE_ANCHOR_STACK) {
				result &= ~CFG_SIMPLIFY_CONST;

				expr_destroy(node);

				node->type = EXPR_NODE_STACK;
				node->obj = entry.object;
			}

		} else {
			// @TODO: Is this allowed?
			panic("Non-constant l-expr");
		}
	} break;

	case EXPR_NODE_GLOBAL:
		result = CFG_SIMPLIFY_CONST;
		break;

	case EXPR_NODE_STACK:
		break;

	case EXPR_NODE_SCOPE:
		result = CFG_SIMPLIFY_CONST;
		break;

	case EXPR_NODE_LIT_INT:
		result = CFG_SIMPLIFY_CONST;
		break;

	case EXPR_NODE_LIT_STR:
		result = CFG_SIMPLIFY_CONST;
		break;
	}

	return result;
}

void
expr_simplify(struct vm *vm, struct expr_node *node)
{
	enum expr_simplify_result result;
	result = expr_simplify_internal(vm, node);

	if ((result & CFG_SIMPLIFY_CONST) != 0) {
		expr_do_simplify_const(vm, node);
	}
}

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static void
expr_print_internal(struct vm *vm, struct expr_node *node, int depth)
{
	if (!node) {
		return;
	}

	_print_indent(depth);

	switch (node->type) {
	case EXPR_NODE_LOOKUP_GLOBAL:
		printf("%.*s (global)", ALIT(node->lookup.name));
		break;

	case EXPR_NODE_LOOKUP_LOCAL:
		printf("%.*s (local)", ALIT(node->lookup.name));
		break;

	case EXPR_NODE_SCOPE:
		printf("scope\n");
		break;

	case EXPR_NODE_FUNC_CALL:
		printf("call");
		break;

	case EXPR_NODE_GLOBAL: {
		printf("global ");
		print_obj_repr(vm, node->obj);
	} break;

	case EXPR_NODE_STACK: {
		printf("local bp + 0x%lx ", (size_t)node->obj.data);
		print_type_repr(vm, get_type(&vm->store, node->obj.type));
	} break;

	case EXPR_NODE_LIT_INT:
		printf("%li", node->lit_int);
		break;

	case EXPR_NODE_LIT_STR:
		printf("\"%.*s\"", LIT(node->lit_str));
		break;
	}

	printf("\n");

	switch (node->type) {
	case EXPR_NODE_FUNC_CALL:
		_print_indent(depth + 1);
		printf("func\n");
		expr_print_internal(vm, node->func_call.func, depth + 2);
		_print_indent(depth + 1);
		printf("args\n");
		expr_print_internal(vm, node->func_call.args, depth + 2);
		break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL:
		expr_print_internal(vm, node->lookup.scope, depth + 1);

	default:
		break;
	}

	expr_print_internal(vm, node->next_arg, depth);
}

void
expr_print(struct vm *vm, struct expr_node *node)
{
	expr_print_internal(vm, node, 0);
}

void
expr_destroy(struct expr_node *node)
{
	switch (node->type) {
	case EXPR_NODE_FUNC_CALL: {
		expr_free(node->func_call.func);

		struct expr_node *next_arg;
		next_arg = node->func_call.args;
		while (next_arg) {
			struct expr_node *arg;
			arg = next_arg;
			next_arg = next_arg->next_arg;

			expr_free(arg);
		}
	} break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL:
		expr_free(node->lookup.scope);
		break;

	default:
		break;
	}
}

void
expr_free(struct expr_node *node)
{
	expr_destroy(node);
	free(node);
}
