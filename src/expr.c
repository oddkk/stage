#include "expr.h"
#include <stdlib.h>
#include "utils.h"

static func_type_id
alloc_type_slot(struct expr *expr)
{
	func_type_id res;

	// The expr should not have already been finalized.
	assert(expr->slots == NULL);

	res = expr->num_type_slots;
	expr->num_type_slots += 1;

	return res;
}

static struct expr_node *
expr_type_expr(struct vm *vm, struct expr *expr,
			   struct expr_node *type_expr, func_type_id slot)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_TYPE_EXPR;
	node->type_expr = type_expr;

	node->rule.out =
		alloc_type_slot(expr);

	node->rule.type = slot;

	return node;
}

struct expr_node *
expr_func_decl(struct vm *vm, struct expr *expr,
			   struct expr_func_decl_param *params,
			   size_t num_params,
			   struct expr_node *ret,
			   struct expr_node *body)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_FUNC_DECL;
	node->func_decl.params = params;
	node->func_decl.num_params = num_params;
	node->func_decl.ret_type = ret;
	node->func_decl.body = body;


	node->rule.abs.num_params =
		node->func_decl.num_params;

	node->rule.abs.params =
		calloc(node->rule.abs.num_params,
			   sizeof(func_type_id));

	for (size_t i = 0; i < node->rule.abs.num_params; i++) {
		node->func_decl.params[i].type =
			expr_type_expr(vm, expr,
						   node->func_decl.params[i].type,
						   alloc_type_slot(expr));

		node->rule.abs.params[i] =
			node->func_decl.params[i].type->rule.type;
	}

	node->func_decl.ret_type =
		expr_type_expr(vm, expr, node->func_decl.ret_type,
					   node->func_decl.body->rule.out);
	node->rule.abs.ret =
		node->func_decl.body->rule.out;

	node->rule.out =
		alloc_type_slot(expr);

	return node;
}

struct expr_node *
expr_call(struct vm *vm, struct expr *expr,
		  struct expr_node *func,
		  struct expr_node *first_arg)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_FUNC_CALL;
	node->func_call.func = func;
	node->func_call.args = first_arg;

	node->rule.app.func = func->rule.out;
	node->rule.app.num_args = 0;

	struct expr_node *arg;
	arg = node->func_call.args;
	while (arg) {
		node->rule.app.num_args += 1;

		arg = arg->next_arg;
	}

	node->rule.app.args =
		calloc(node->rule.app.num_args, sizeof(func_type_id));

	size_t i = 0;
	arg = node->func_call.args;
	while (arg) {
		node->rule.app.args[i] =
			arg->rule.out;

		i += 1;
		arg = arg->next_arg;
	}

	node->rule.out = alloc_type_slot(expr);

	return node;
}

struct expr_node *
expr_lookup(struct vm *vm, struct expr *expr,
			struct atom *name, struct expr_node *scope,
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
	node->rule.out = alloc_type_slot(expr);

	return node;
}

struct expr_node *
expr_scope(struct vm *vm, struct expr *expr,
		   struct scope *value)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_SCOPE;
	node->scope = value;
	node->rule.out = alloc_type_slot(expr);
	node->flags |= EXPR_CONST;

	return node;
}

struct expr_node *
expr_global(struct vm *vm, struct expr *expr,
			struct object value)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_GLOBAL;
	node->obj = value;
	node->rule.out = alloc_type_slot(expr);
	node->flags |= EXPR_CONST;

	return node;
}

struct expr_node *
expr_lit_int(struct vm *vm, struct expr *expr,
			 int64_t value)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_LIT_INT;
	node->lit_int = value;
	node->rule.out = alloc_type_slot(expr);
	node->flags |= EXPR_CONST;

	return node;
}

struct expr_node *
expr_lit_str(struct vm *vm, struct expr *expr,
			 struct string value)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_LIT_STR;
	node->lit_str = value;
	node->rule.out = alloc_type_slot(expr);
	node->flags |= EXPR_CONST;

	return node;
}

void
expr_finalize(struct vm *vm, struct expr *expr)
{
	assert(expr->slots == NULL);
	expr->slots = calloc(expr->num_type_slots, sizeof(type_id));
}

int
expr_bind_type(struct vm *vm, struct expr *expr,
			   func_type_id slot, type_id type)
{
	assert(slot < expr->num_type_slots);

	// The expr shuold have been finalized before we try to bind
	// anything.
	assert(expr->slots != NULL);

	type_id unified;

	if (!unify_types(vm, expr->slots[slot], type, &unified)) {
		printf("Conflicting types '");
		print_type_repr(vm, get_type(&vm->store, expr->slots[slot]));
		printf("' and '");
		print_type_repr(vm, get_type(&vm->store, type));
		printf("'. (slot %u)\n", slot);

		expr->num_type_errors += 1;
		return -1;
	}

	// @TODO: Should the type be updated if they can be generalized?
	expr->slots[slot] = unified;

	return 0;
}

static void
expr_bind_obvious_types(struct vm *vm, struct expr *expr,
						struct expr_node *node)
{
	switch (node->type) {
	case EXPR_NODE_FUNC_DECL:
		assert(node->rule.abs.num_params == node->func_decl.num_params);
		for (size_t i = 0; i < node->rule.abs.num_params; i++) {
			struct expr_node *param;
			param = node->func_decl.params[i].type;

			expr_bind_type(vm, expr, param->rule.out,
						   vm->default_types.type);
			expr_bind_obvious_types(vm, expr, param);
		}

		expr_bind_type(vm, expr, node->func_decl.ret_type->rule.out,
					   vm->default_types.type);
		expr_bind_obvious_types(vm, expr, node->func_decl.ret_type);

		expr_bind_obvious_types(vm, expr, node->func_decl.body);
		break;

	case EXPR_NODE_FUNC_CALL: {
		struct expr_node *arg;
		arg = node->func_call.args;
		while (arg) {
			expr_bind_obvious_types(vm, expr, arg);
			arg = arg->next_arg;
		}

		expr_bind_obvious_types(vm, expr, node->func_call.func);
	} break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL:
		expr_bind_obvious_types(vm, expr, node->lookup.scope);
		break;

	case EXPR_NODE_GLOBAL:
		node->flags |= EXPR_CONST;
	case EXPR_NODE_STACK:
		expr_bind_type(vm, expr, node->rule.out,
					   node->obj.type);
		node->flags |= EXPR_TYPED;
		break;

	case EXPR_NODE_SCOPE:
		expr_bind_type(vm, expr, node->rule.out,
					   TYPE_SCOPE);
		node->flags |= EXPR_TYPED | EXPR_CONST;
		break;

	case EXPR_NODE_LIT_INT:
		expr_bind_type(vm, expr, node->rule.out,
					   vm->default_types.integer);
		node->flags |= EXPR_TYPED | EXPR_CONST;
		break;

	case EXPR_NODE_LIT_STR:
		expr_bind_type(vm, expr, node->rule.out,
					   vm->default_types.string);
		node->flags |= EXPR_TYPED | EXPR_CONST;
		break;

	case EXPR_NODE_TYPE_EXPR:
		if (node->type_expr) {
			expr_bind_obvious_types(vm, expr, node->type_expr);
		} else {
			node->flags |= EXPR_TYPED | EXPR_CONST;
		}
		expr_bind_type(vm, expr, node->rule.out,
					   vm->default_types.type);
		break;
	}
}

static int
expr_try_infer_types(struct vm *vm, struct expr *expr,
					 struct expr_node *node)
{
	switch (node->type) {
	case EXPR_NODE_FUNC_DECL: {
		enum expr_node_flags flags;
		flags = EXPR_TYPED | EXPR_CONST;

		size_t num_params = node->func_decl.num_params;
		struct atom *param_names[num_params];
		type_id param_types[num_params];

		for (size_t i = 0; i < node->func_decl.num_params; i++) {
			struct expr_node *param;
			param = node->func_decl.params[i].type;
			expr_try_infer_types(vm, expr, param);

			param_names[i] = node->func_decl.params[i].name;
			param_types[i] = expr->slots[node->rule.abs.params[i]];

			flags &= param->flags;
		}

		struct expr_node *ret;
		ret = node->func_decl.ret_type;
		expr_try_infer_types(vm, expr, ret);
		flags &= ret->flags;

		expr_try_infer_types(vm, expr, node->func_decl.body);

		flags &= node->func_decl.body->flags;

		type_id ret_type;
		ret_type = expr->slots[node->rule.abs.ret];

		type_id func_type;
		func_type =
			type_register_function(vm, param_names, param_types,
								   num_params, ret_type,
								   TYPE_FUNCTION_NATIVE);

		expr_bind_type(vm, expr, node->rule.out, func_type);

		node->flags = flags;
	} break;

	case EXPR_NODE_FUNC_CALL: {
		size_t num_unresolved_args = 0;

		struct expr_node *arg;
		size_t arg_i = 0;
		arg = node->func_call.args;

		type_id *param_types;
		param_types = calloc(node->rule.app.num_args, sizeof(type_id));

		while (arg) {
			expr_try_infer_types(vm, expr, arg);

			if (expr->slots[node->rule.app.args[arg_i]] == TYPE_UNSET) {
				num_unresolved_args += 1;
			}

			param_types[arg_i] = expr->slots[node->rule.app.args[arg_i]];

			arg_i += 1;
			arg = arg->next_arg;
		}

		assert(arg_i == node->rule.app.num_args);

		type_id ret_type;
		ret_type = expr->slots[node->rule.out];

		type_id func_type;
		func_type =
			type_register_function(vm, NULL, param_types,
								   node->rule.app.num_args,
								   ret_type, TYPE_FUNCTION_GENERIC);

		free(param_types);

		expr_bind_type(vm, expr, node->rule.app.func, func_type);
		expr_try_infer_types(vm, expr, node->func_call.func);

		struct type *new_func_type;
		new_func_type =
			get_type(&vm->store,
					 expr->slots[node->rule.app.func]);

		struct type_func *new_func;
		new_func = new_func_type->data;

		enum expr_node_flags all_params_flags;
		all_params_flags = EXPR_TYPED | EXPR_CONST;
		arg_i = 0;
		arg = node->func_call.args;

		while (arg) {
			expr_bind_type(vm, expr, node->rule.app.args[arg_i],
						   new_func->param_types[arg_i]);
			expr_try_infer_types(vm, expr, arg);

			all_params_flags &= arg->flags;

			arg_i += 1;
			arg = arg->next_arg;
		}

		expr_bind_type(vm, expr, node->rule.out,
					   new_func->ret);

		node->flags = node->func_call.func->flags & all_params_flags;

	} break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL: {
		expr_try_infer_types(vm, expr, node->lookup.scope);

		int flags = EXPR_TYPED | EXPR_CONST;

		if ((node->lookup.scope->flags & flags) == flags) {
			int err;
			struct object obj;
			err = expr_eval_simple(vm, node, &obj);

			if (err) {
				return -1;
			}

			expr_bind_type(vm, expr, node->rule.out, obj.type);
			node->flags |= EXPR_TYPED | EXPR_CONST;
		}
	} break;

	case EXPR_NODE_GLOBAL:
	case EXPR_NODE_STACK:
		break;

	case EXPR_NODE_SCOPE:
		break;

	case EXPR_NODE_LIT_INT:
		break;

	case EXPR_NODE_LIT_STR:
		break;

	case EXPR_NODE_TYPE_EXPR:
		if (node->type_expr) {
			expr_try_infer_types(vm, expr, node->type_expr);

			if ((node->type_expr->flags & EXPR_CONST) != 0) {
				int err;
				struct object type_obj;

				err = expr_eval_simple(vm, node->type_expr, &type_obj);
				if (err) {
					return -1;
				}

				type_id type = type_obj_get(vm, type_obj);
				expr_bind_type(vm, expr, node->rule.type, type);
			}

			node->flags = node->type_expr->flags;
		}
		break;

	}

	return 0;
}

int
expr_typecheck(struct vm *vm, struct expr *expr)
{
	expr_bind_obvious_types(vm, expr, expr->body);
	printf("\nbefore typecheck:\n");
	expr_print(vm, expr);

	expr_try_infer_types(vm, expr, expr->body);
	printf("\ntypecheck iter 0:\n");
	expr_print(vm, expr);
	return -1;
}


static int
expr_eval_lookup(struct vm *vm, struct exec_stack *stack,
				 struct expr_node *node,
				 struct scope_entry *result)
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

	case EXPR_NODE_FUNC_DECL: {
		panic("TODO: func decl expr");
	} break;

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

	case EXPR_NODE_TYPE_EXPR:
		panic("TODO: type expr");
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
	/* expr_print_internal(vm, node); */

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

	case EXPR_NODE_FUNC_DECL: {
		panic("TODO: func decl expr");
	} break;

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

	case EXPR_NODE_TYPE_EXPR:
		panic("TODO: type expr");
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
expr_print_slot(struct vm *vm, struct expr *expr, func_type_id slot)
{
	if (expr->slots) {
		printf("%u (", slot);
		print_type_repr(vm, get_type(&vm->store, expr->slots[slot]));
		printf(")");
	} else {
		printf("%u", slot);
	}
}

static void
expr_print_internal(struct vm *vm, struct expr *expr, struct expr_node *node, int depth)
{
	if (!node) {
		return;
	}

	_print_indent(depth);

	switch (node->type) {

	case EXPR_NODE_FUNC_DECL: {
		printf("func decl");
	} break;

	case EXPR_NODE_LOOKUP_GLOBAL:
		printf("%.*s (global)", ALIT(node->lookup.name));
		break;

	case EXPR_NODE_LOOKUP_LOCAL:
		printf("%.*s (local)", ALIT(node->lookup.name));
		break;

	case EXPR_NODE_SCOPE:
		printf("scope");
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

	case EXPR_NODE_TYPE_EXPR:
		printf("type expr");
		break;
	}

	switch (node->type) {
	case EXPR_NODE_FUNC_DECL: // [ABS]
		printf(" ((");
		for (size_t i = 0; i < node->rule.abs.num_params; i++) {
			if (i != 0) {
				printf(", ");
			}
			expr_print_slot(vm, expr, node->rule.abs.params[i]);
		}
		printf(") -> ");
		expr_print_slot(vm, expr, node->rule.abs.ret);
		printf(") -> ");
		expr_print_slot(vm, expr, node->rule.out);
		printf(")");
		break;

	case EXPR_NODE_FUNC_CALL: // [APP]
		printf(" (");
		expr_print_slot(vm, expr, node->rule.app.func);
		printf(") (");
		for (size_t i = 0; i < node->rule.app.num_args; i++) {
			if (i != 0) {
				printf(", ");
			}
			expr_print_slot(vm, expr, node->rule.app.args[i]);
		}
		printf(") -> ");
		expr_print_slot(vm, expr, node->rule.out);

		break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL:
	case EXPR_NODE_GLOBAL:
	case EXPR_NODE_STACK:
	case EXPR_NODE_SCOPE:
	case EXPR_NODE_LIT_INT:
	case EXPR_NODE_LIT_STR: // [VAR]
		printf(" () -> ");
		expr_print_slot(vm, expr, node->rule.out);
		break;

	case EXPR_NODE_TYPE_EXPR:
		printf(" ");
		expr_print_slot(vm, expr, node->rule.out);
		printf(" <- ");
		expr_print_slot(vm, expr, node->rule.type);
	}

	if ((node->flags & EXPR_TYPED) != 0) {
		printf(" TYPED");
	}

	if ((node->flags & EXPR_CONST) != 0) {
		printf(" CONST");
	}

	printf("\n");

	switch (node->type) {
	case EXPR_NODE_FUNC_DECL:
		for (size_t i = 0; i < node->func_decl.num_params; i++) {
			_print_indent(depth + 1);
			printf("param %.*s type\n", ALIT(node->func_decl.params[i].name));
			expr_print_internal(vm, expr, node->func_decl.params[i].type, depth + 2);
		}

		_print_indent(depth + 1);
		printf("ret type\n");
		expr_print_internal(vm, expr, node->func_decl.ret_type, depth + 2);

		_print_indent(depth + 1);
		printf("body\n");
		expr_print_internal(vm, expr, node->func_decl.body, depth + 2);
		break;

	case EXPR_NODE_FUNC_CALL:
		_print_indent(depth + 1);
		printf("func\n");
		expr_print_internal(vm, expr, node->func_call.func, depth + 2);
		_print_indent(depth + 1);
		printf("args\n");
		expr_print_internal(vm, expr, node->func_call.args, depth + 2);
		break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL:
		expr_print_internal(vm, expr, node->lookup.scope, depth + 1);
		break;

	case EXPR_NODE_TYPE_EXPR:
		if (node->type_expr) {
			expr_print_internal(vm, expr, node->type_expr, depth + 1);
		}
		break;

	default:
		break;
	}

	expr_print_internal(vm, expr, node->next_arg, depth);
}

void
expr_print(struct vm *vm, struct expr *expr)
{
	expr_print_internal(vm, expr, expr->body, 0);
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
