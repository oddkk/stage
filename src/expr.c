#include "expr.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "modules/base/mod.h"

#define TYPECHECK_DEBUG 1

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
expr_type_expr(struct stg_module *mod, struct expr *expr,
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
expr_func_decl(struct stg_module *mod, struct expr *expr,
			   struct expr_func_decl_param *params,
			   size_t num_params,
			   struct expr_node *ret,
			   struct expr_node *body)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	expr_init_func_decl(mod, expr, node, params,
						num_params, ret, body);

	return node;
}

struct expr_node *
expr_init_func_decl(struct stg_module *mod, struct expr *expr,
					struct expr_node *node,
					struct expr_func_decl_param *params,
					size_t num_params,
					struct expr_node *ret,
					struct expr_node *body)
{
	struct expr *func_expr = &node->func_decl.expr;

	node->type = EXPR_NODE_FUNC_DECL;
	node->func_decl.params = params;
	node->func_decl.num_params = num_params;
	node->func_decl.ret_type = ret;
	node->func_decl.scope.outer_scope = expr->outer_scope;
	node->func_decl.scope.num_entries = node->func_decl.num_params;
	func_expr->body = body;
	func_expr->outer_scope = expr->outer_scope;
	func_expr->mod = mod;

	node->func_decl.scope.entry_names =
		calloc(node->func_decl.scope.num_entries,
			   sizeof(struct atom *));
	node->func_decl.scope.entry_types =
		calloc(node->func_decl.scope.num_entries,
			   sizeof(func_type_id));


	node->rule.abs.num_params =
		node->func_decl.num_params;

	node->rule.abs.params =
		calloc(node->rule.abs.num_params,
			   sizeof(func_type_id));

	for (size_t i = 0; i < node->rule.abs.num_params; i++) {
		node->func_decl.params[i].type =
			expr_type_expr(mod, func_expr,
						   node->func_decl.params[i].type,
						   alloc_type_slot(func_expr));

		node->rule.abs.params[i] =
			node->func_decl.params[i].type->rule.type;

		node->func_decl.scope.entry_names[i] =
			node->func_decl.params[i].name;
		node->func_decl.scope.entry_types[i] =
			node->rule.abs.params[i];
	}

	node->func_decl.ret_type =
		expr_type_expr(mod, func_expr, node->func_decl.ret_type,
					   node->func_decl.expr.body->rule.out);
	node->rule.abs.ret =
		node->func_decl.expr.body->rule.out;

	node->rule.out =
		alloc_type_slot(expr);

	return node;
}

struct expr_node *
expr_call(struct stg_module *mod, struct expr *expr,
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
expr_lookup_func_scope(struct stg_module *mod, struct expr *expr,
					   struct atom *name,
					   struct expr_func_scope *scope)
{
	struct expr_node *node;

	node = calloc(1, sizeof(struct expr_node));

	node->type = EXPR_NODE_LOOKUP_FUNC;

	node->lookup.name = name;
	node->lookup.func_scope = scope;
	node->rule.out = alloc_type_slot(expr);

	return node;
}

struct expr_node *
expr_lookup(struct stg_module *mod, struct expr *expr,
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
expr_scope(struct stg_module *mod, struct expr *expr,
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
expr_global(struct stg_module *mod, struct expr *expr,
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
expr_lit_int(struct stg_module *mod, struct expr *expr,
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
expr_lit_str(struct stg_module *mod, struct expr *expr,
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
expr_finalize(struct stg_module *mod, struct expr *expr)
{
	if (expr->slots == NULL) {
		expr->slots =
			calloc(expr->num_type_slots,
				   sizeof(struct expr_type_slot));
	}
}

int
expr_bind_type(struct stg_module *mod, struct expr *expr,
			   func_type_id slot, type_id type)
{
	assert(slot < expr->num_type_slots && slot >= 0);

	// The expr shuold have been finalized before we try to bind
	// anything.
	assert(expr->slots != NULL);

	type_id unified;

	func_type_id real_slot;
	real_slot = expr_get_actual_slot(expr, slot);

	type_id slot_type = expr_get_slot_type(expr, real_slot);

	if (!unify_types(mod->vm, &mod->store, slot_type, type, &unified)) {
		printf("Conflicting types '");
		print_type_repr(mod->vm, vm_get_type(mod->vm, expr_get_slot_type(expr, slot)));
		printf("' and '");
		print_type_repr(mod->vm, vm_get_type(mod->vm, type));
		printf("'. (slot %u)\n", slot);

		expr->num_type_errors += 1;
		return -1;
	}

	// printf("unify(");
	// print_type_repr(mod->vm, vm_get_type(mod->vm, expr_get_slot_type(expr, slot)));
	// printf(", ");
	// print_type_repr(mod->vm, vm_get_type(mod->vm, type));
	// printf(") = ");
	// print_type_repr(mod->vm, vm_get_type(mod->vm, unified));
	// printf(" (slot %i)\n", slot);


	// @TODO: Should the type be updated if they can be generalized?
	expr->slots[real_slot].type = unified;
	expr->slots[real_slot].state =
		SLOT_BOUND | (expr->slots[real_slot].state & SLOT_TEMPLATE);

	return 0;
}

void
expr_slot_mark_template(struct stg_module *mod, struct expr *expr,
						func_type_id slot)
{
	expr->slots[slot].state |= SLOT_TEMPLATE;
}

int
expr_bind_ref_slot(struct stg_module *mod, struct expr *expr,
				   func_type_id slot, func_type_id other)
{
	if (SLOT_STATE(expr->slots[slot].state) == SLOT_BOUND_REF) {

		expr_bind_ref_slot(
			mod, expr,
			expr_get_actual_slot(expr, slot),
			expr_get_actual_slot(expr, other)
		);
		return 0;
	} else {
		type_id old_type = TYPE_UNSET;
		if (SLOT_STATE(expr->slots[slot].state) == SLOT_BOUND) {
			old_type = expr->slots[slot].type;
		}

		expr->slots[slot].state = SLOT_BOUND_REF;
		expr->slots[slot].ref = other;

		return expr_bind_type(mod, expr, other, old_type);
	}
}

func_type_id
expr_get_actual_slot(struct expr *expr, func_type_id slot)
{
	while (SLOT_STATE(expr->slots[slot].state) == SLOT_BOUND_REF) {
		slot = expr->slots[slot].ref;
	}

	return slot;
}

// Returns the offset of 
static func_type_id
expr_append_expr_slots(struct stg_module *mod, struct expr *expr, struct expr *func_expr)
{
	func_type_id func_slot_begin = expr->num_type_slots;
	size_t num_new_slots = func_expr->num_type_slots;

	printf("Expand slots: %d + %zu\n", func_slot_begin, num_new_slots);

	expr->num_type_slots += num_new_slots;
	expr->slots = realloc(expr->slots,
			sizeof(struct expr_type_slot) * expr->num_type_slots);

	memcpy(&expr->slots[func_slot_begin], func_expr->slots,
			sizeof(struct expr_type_slot) * num_new_slots);
	for (size_t i = 0; i < num_new_slots; i++) {
		struct expr_type_slot *slot = &expr->slots[func_slot_begin + i];
		switch (SLOT_STATE(slot->state)) {
			case SLOT_BOUND_REF:
				slot->ref += func_slot_begin;
				break;

			default:
				break;
		}
	}

	return func_slot_begin;
}


type_id
expr_get_slot_type(struct expr *expr, func_type_id slot)
{
	slot = expr_get_actual_slot(expr, slot);
	assert(SLOT_STATE(expr->slots[slot].state) != SLOT_BOUND_REF);
	if (SLOT_STATE(expr->slots[slot].state) == SLOT_BOUND) {
		return expr->slots[slot].type;
	} else {
		return TYPE_UNSET;
	}
}

type_id
expr_slot_is_template(struct expr *expr, func_type_id slot)
{
	return SLOT_IS_TEMPLATE(expr->slots[slot].state);
}

static int
expr_resolve_scope_lookup(struct vm *vm, struct expr *expr,
						  struct scope *scope,
						  struct atom *name,
						  type_id expected_type,
						  bool global_lookup,
						  struct scope_entry *result)
{
	printf("looking for '%.*s': ", ALIT(name));
	print_type_repr(vm, vm_get_type(vm, expected_type));
	printf("\n");

	int iter_err = 0;
	struct scope_iter iter = {0};
	size_t num_matching_entries = 0;
	while (iter_err == 0) {
		if (global_lookup) {
			iter_err = scope_iterate_overloads(scope, name, &iter);
		} else {
			iter_err = scope_iterate_local_overloads(scope, name, &iter);
		}

		if (iter_err != 0) {
			break;
		}

		printf("  candidate: ");
		print_type_repr(vm, vm_get_type(vm, iter.entry->object.type));

		type_id out_type;
		if (unify_types(vm, &expr->mod->store,
						expected_type,
						iter.entry->object.type, &out_type)) {
			printf(" match: ");
			print_type_repr(vm, vm_get_type(vm, out_type));
			*result = *iter.entry;
			num_matching_entries += 1;
		}
		printf("\n");
	}

	if (num_matching_entries == 0) {
		printf("'%.*s' was not found.\n", ALIT(name));
		return -1;
	} else if (num_matching_entries > 1) {
		printf("%zu matching results for %.*s.\n", num_matching_entries, ALIT(name));
		return 1;
	}

	return 0;
}


static void
expr_bind_obvious_types(struct stg_module *mod, struct expr *expr,
						struct expr_node *node)
{
	switch (node->type) {
	case EXPR_NODE_FUNC_DECL:
		assert(node->rule.abs.num_params == node->func_decl.num_params);
		for (size_t i = 0; i < node->rule.abs.num_params; i++) {
			struct expr_node *param;
			param = node->func_decl.params[i].type;

			expr_bind_type(mod, &node->func_decl.expr, param->rule.out,
						   mod->vm->default_types.type);
			expr_bind_obvious_types(mod, &node->func_decl.expr, param);
		}

		expr_bind_type(mod, &node->func_decl.expr,
					   node->func_decl.ret_type->rule.out,
					   mod->vm->default_types.type);
		expr_bind_obvious_types(mod, &node->func_decl.expr,
								node->func_decl.ret_type);

		expr_bind_obvious_types(mod, &node->func_decl.expr,
								node->func_decl.expr.body);
		break;

	case EXPR_NODE_FUNC_CALL: {
		struct expr_node *arg;
		arg = node->func_call.args;
		while (arg) {
			expr_bind_obvious_types(mod, expr, arg);
			arg = arg->next_arg;
		}

		expr_bind_obvious_types(mod, expr, node->func_call.func);
	} break;

	case EXPR_NODE_LOOKUP_FUNC: {
		struct expr_func_scope *func_scope = node->lookup.func_scope;
		bool found = false;
		for (size_t i = 0; i < func_scope->num_entries; i++) {
			if (func_scope->entry_names[i] == node->lookup.name) {
				expr_bind_ref_slot(mod, expr, node->rule.out,
								   func_scope->entry_types[i]);
				found = true;
				break;
			}
		}
	} break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL:
		expr_bind_obvious_types(mod, expr, node->lookup.scope);
		break;

	case EXPR_NODE_GLOBAL:
		node->flags |= EXPR_CONST;
	case EXPR_NODE_STACK:
		expr_bind_type(mod, expr, node->rule.out,
					   node->obj.type);
		node->flags |= EXPR_TYPED;
		break;

	case EXPR_NODE_SCOPE:
		expr_bind_type(mod, expr, node->rule.out,
					   TYPE_SCOPE);
		node->flags |= EXPR_TYPED | EXPR_CONST;
		break;

	case EXPR_NODE_LIT_INT:
		expr_bind_type(mod, expr, node->rule.out,
					   mod->vm->default_types.integer);
		node->flags |= EXPR_TYPED | EXPR_CONST;
		break;

	case EXPR_NODE_LIT_STR:
		expr_bind_type(mod, expr, node->rule.out,
					   mod->vm->default_types.string);
		node->flags |= EXPR_TYPED | EXPR_CONST;
		break;

	case EXPR_NODE_TYPE_EXPR:
		if (node->type_expr) {
			expr_bind_obvious_types(mod, expr, node->type_expr);
		} else {
			node->flags |= EXPR_TYPED | EXPR_CONST;
			expr_bind_type(mod, expr, node->rule.type, TYPE_TEMPLATE_PARAM);
		}
		expr_bind_type(mod, expr, node->rule.out,
					   mod->vm->default_types.type);
		break;
	}
}

enum expr_infer_types_error {
	EXPR_INFER_TYPES_OK    = 0x3, // 0b11
	EXPR_INFER_TYPES_YIELD = 0x1, // 0b01
	EXPR_INFER_TYPES_ERROR = 0x0, // 0b00
};

static enum expr_infer_types_error
expr_try_infer_types(struct stg_module *mod, struct expr *expr,
					 struct expr_node *node, func_type_id slot_offset)
{
	enum expr_infer_types_error ret_err;
	ret_err = EXPR_INFER_TYPES_OK;

	switch (node->type) {
	case EXPR_NODE_FUNC_DECL: {
		enum expr_node_flags flags;
		flags = EXPR_TYPED | EXPR_CONST;

		struct expr *func_expr;
		func_type_id func_slot_offset;
		if (node->func_decl.use_external_expr) {
			func_expr = expr;
			func_slot_offset = slot_offset;
		} else {
			func_expr= &node->func_decl.expr;
			func_slot_offset = 0;
		}

		size_t num_params = node->func_decl.num_params;
		struct atom *param_names[num_params];
		type_id param_types[num_params];

		for (size_t i = 0; i < node->func_decl.num_params; i++) {
			struct expr_node *param;
			param = node->func_decl.params[i].type;

			ret_err &= expr_try_infer_types(mod, func_expr, param, func_slot_offset);

			param_names[i] = node->func_decl.params[i].name;
			param_types[i] = expr_get_slot_type(func_expr, node->rule.abs.params[i] + func_slot_offset);

			flags &= param->flags;
		}

		struct expr_node *ret;
		ret = node->func_decl.ret_type;
		ret_err &= expr_try_infer_types(mod, func_expr, ret, func_slot_offset);
		flags &= ret->flags;

		struct expr_node *body;
		body = node->func_decl.expr.body;
		ret_err &= expr_try_infer_types(mod, func_expr, body, func_slot_offset);
		flags &= body->flags;

		type_id ret_type;
		ret_type = expr_get_slot_type(func_expr, node->rule.abs.ret + func_slot_offset);

		type_id func_type;
		func_type =
			type_register_function(mod->vm, &mod->store, param_names, param_types,
								   num_params, ret_type,
								   TYPE_FUNCTION_NATIVE);

		expr_bind_type(mod, expr, node->rule.out + slot_offset, func_type);

		node->flags = flags;
	} return ret_err;

	case EXPR_NODE_FUNC_CALL: {
		struct expr_node *arg;
		size_t arg_i = 0;

		arg = node->func_call.args;

		type_id *param_types;
		param_types = calloc(node->rule.app.num_args, sizeof(type_id));

		while (arg) {
			ret_err &= expr_try_infer_types(mod, expr, arg, slot_offset);

			type_id slot_type;
			slot_type = expr_get_slot_type(expr, node->rule.app.args[arg_i] + slot_offset);

			param_types[arg_i] = slot_type;

			arg_i += 1;
			arg = arg->next_arg;
		}

		assert(arg_i == node->rule.app.num_args);

		type_id ret_type;
		ret_type = expr_get_slot_type(expr, node->rule.out + slot_offset);

		type_id func_type;
		func_type =
			type_register_function(mod->vm, &mod->store, NULL, param_types,
					node->rule.app.num_args,
					ret_type, TYPE_FUNCTION_GENERIC);

		free(param_types);

		expr_bind_type(mod, expr, node->rule.app.func + slot_offset, func_type);


		ret_err &= expr_try_infer_types(mod, expr, node->func_call.func, slot_offset);

		type_id new_func_type_id;
		new_func_type_id = expr_get_slot_type(expr, node->rule.app.func + slot_offset);

		if (new_func_type_id == TYPE_UNSET) {
			ret_err &= EXPR_INFER_TYPES_YIELD;
		} else {
			struct type *new_func_type;
			new_func_type =
				vm_get_type(mod->vm, new_func_type_id);

			struct type_func *new_func;
			new_func = new_func_type->data;

			enum expr_node_flags all_params_flags;
			all_params_flags = EXPR_TYPED | EXPR_CONST;

			arg_i = 0;
			arg = node->func_call.args;

			while (arg) {
				expr_bind_type(mod, expr, node->rule.app.args[arg_i] + slot_offset,
							   new_func->param_types[arg_i]);
				ret_err &= expr_try_infer_types(mod, expr, arg, slot_offset);
				all_params_flags &= arg->flags;

				arg_i += 1;
				arg = arg->next_arg;
			}

			expr_bind_type(mod, expr, node->rule.out + slot_offset,
						   new_func->ret);
			node->flags = node->func_call.func->flags & all_params_flags;
		}

	} return ret_err;

	case EXPR_NODE_LOOKUP_FUNC:
	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL: {
		struct scope *scope = NULL;
		bool global_lookup;

		printf("looking for %.*s... ", ALIT(node->lookup.name));

		if (node->type == EXPR_NODE_LOOKUP_FUNC) {
			printf("func first... ");
			struct expr_func_scope *func_scope = node->lookup.func_scope;
			bool found = false;
			for (size_t i = 0; i < func_scope->num_entries; i++) {
				if (func_scope->entry_names[i] == node->lookup.name) {
					found = true;
					break;
				}
			}

			if (found) {
				printf("found locally.\n");
				if (expr_get_slot_type(expr, node->rule.out + slot_offset) != TYPE_UNSET) {
					node->flags |= EXPR_TYPED;
				}
				return ret_err;
			}

			scope = func_scope->outer_scope;
			global_lookup = true;
		} else {
			ret_err &= expr_try_infer_types(mod, expr, node->lookup.scope, slot_offset);

			int flags = EXPR_TYPED | EXPR_CONST;
			if ((node->lookup.scope->flags & flags) != flags) {
				printf("scope is not constant and typed.\n");
				return EXPR_INFER_TYPES_YIELD;
			}

			struct object out_scope;
			int err;

			err = expr_eval_simple_offset(mod->vm, mod, expr, node->lookup.scope,
										  slot_offset, &out_scope);

			if (err) {
				printf("could not resolve scope\n");
				return err;
			}

			if (node->type == EXPR_NODE_LOOKUP_GLOBAL) {
				global_lookup = true;

				if (out_scope.type != TYPE_SCOPE) {
					printf("not a scope\n");
					return -1;
				}

				scope = *(struct scope **)out_scope.data;

			} else {
				global_lookup = false;

				if (out_scope.type == TYPE_SCOPE) {
					scope = *(struct scope **)out_scope.data;
				} else {
					struct type *target_type = vm_get_type(mod->vm, out_scope.type);
					scope = target_type->object_scope;
					if (!scope) {
						printf("%.*s has no scope.\n", ALIT(target_type->name));
						return -1;
					}
				}
			}

			assert(scope != NULL);
		}

		type_id expected_type;
		expected_type = expr_get_slot_type(expr, node->rule.out);

		bool is_template = true;

		if (vm_get_type(mod->vm, expected_type)->num_template_params > 0) {
			node->flags |= EXPR_TEMPLATE;
			is_template = true;
		}

		printf("checking (%s, %p):\n", (global_lookup ? "global" : "local"), (void *)scope);
		struct scope_entry result;
		int err = expr_resolve_scope_lookup(mod->vm, expr, scope, node->lookup.name,
											expected_type, global_lookup, &result);

		if (err < 0) {
			// The object was not found.
			printf("[lookup] Object %.*s not found.\n", ALIT(node->lookup.name));
			return EXPR_INFER_TYPES_ERROR;
		} else if (err > 0) {
			// Not enough type information to uniquely find a matching
			// object.
			printf("[lookup] Object %.*s not enough type info.\n", ALIT(node->lookup.name));
			return is_template
				? (ret_err & EXPR_INFER_TYPES_OK)
				: (ret_err & EXPR_INFER_TYPES_YIELD);
		}

		struct object obj = result.object;

		if (obj.type == TYPE_UNSET) {
			// The object has not yet been initialized.
			printf("[lookup] Object %.*s not yet initialized.\n", ALIT(node->lookup.name));
			return ret_err & EXPR_INFER_TYPES_YIELD;
		}

		expr_bind_type(mod, expr, node->rule.out + slot_offset, obj.type);

		struct type *result_type;
		result_type = vm_get_type(mod->vm, obj.type);

		if (result_type->num_template_params > 0) {
			node->flags |= EXPR_TEMPLATE;
			return EXPR_INFER_TYPES_OK;
		}

		// TODO: The expression should only be tagged as const if the function
		// itself is const.
		node->flags |= EXPR_TYPED | EXPR_CONST;
		return ret_err;
	} break;

	case EXPR_NODE_GLOBAL:
	case EXPR_NODE_STACK:
		return ret_err;

	case EXPR_NODE_SCOPE:
		return ret_err;

	case EXPR_NODE_LIT_INT:
		return ret_err;

	case EXPR_NODE_LIT_STR:
		return ret_err;

	case EXPR_NODE_TYPE_EXPR:
		if (node->type_expr) {
			ret_err &= expr_try_infer_types(mod, expr, node->type_expr, slot_offset);

			if (expr_get_slot_type(expr, node->type_expr->rule.out + slot_offset) != mod->vm->default_types.type) {
				printf("Expected 'type', got '");
				print_type_repr(mod->vm,
								vm_get_type(mod->vm, expr_get_slot_type(expr,
																   node->type_expr->rule.out + slot_offset)));
				printf("'. (slot %u)\n", node->type_expr->rule.out);
				return ret_err | EXPR_INFER_TYPES_ERROR;
			}

			if ((node->type_expr->flags & EXPR_CONST) != 0) {
				int err;
				struct object type_obj;

				err = expr_eval_simple_offset(mod->vm, mod, expr, node->type_expr,
											  slot_offset, &type_obj);
				if (err) {
					return EXPR_INFER_TYPES_ERROR;
				}

				type_id type = type_obj_get(mod->vm, type_obj);
				expr_bind_type(mod, expr, node->rule.type + slot_offset, type);
			}

			node->flags = node->type_expr->flags;
		}
		return ret_err;

	}

	panic("Unhandled expr node in infer types.");
	return EXPR_INFER_TYPES_ERROR;
}

int
expr_typecheck(struct stg_module *mod, struct expr *expr)
{
	switch (expr->state) {
	case EXPR_TYPECHECK_IDLE:
		expr_finalize(mod, expr);
		expr_bind_obvious_types(mod, expr, expr->body);
		expr->state = EXPR_TYPECHECK_INFER_TYPES;

		// fallthrough

	case EXPR_TYPECHECK_INFER_TYPES: {
		enum expr_infer_types_error err;

#if TYPECHECK_DEBUG
		if (expr->num_infer == 0) {
			printf("typecheck iter -1:\n");
			expr_print(mod->vm, expr);
			printf("\n");
		}
#endif

		err = expr_try_infer_types(mod, expr, expr->body, 0);

#if TYPECHECK_DEBUG
		printf("typecheck iter %zu (err %i):\n", expr->num_infer, err);
		expr_print(mod->vm, expr);
		printf("\n");
#endif

		expr->num_infer += 1;

		switch (err) {
		case EXPR_INFER_TYPES_OK:
			if (expr->num_type_errors == 0) {
				expr->state = EXPR_TYPECHECK_DONE;
				return 0;
			} else {
				expr->state = EXPR_TYPECHECK_ERROR;
				return -1;
			}

		case EXPR_INFER_TYPES_ERROR:
			expr->state = EXPR_TYPECHECK_ERROR;
			return -1;

		case EXPR_INFER_TYPES_YIELD:
			return 1;
		}
	}

	case EXPR_TYPECHECK_DONE:
		return 0;

	case EXPR_TYPECHECK_ERROR:
		return -1;
	}
}

static int
expr_eval_lookup(struct vm *vm, struct expr *expr,
				 struct exec_stack *stack,
				 struct expr_node *node,
				 type_id expected_type,
				 struct scope_entry *result)
{
	assert(node->type == EXPR_NODE_LOOKUP_LOCAL ||
		   node->type == EXPR_NODE_LOOKUP_GLOBAL);


	struct object out_scope;
	int err;

	err = expr_eval(vm, expr, stack, node->lookup.scope, &out_scope);

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
			struct type *target_type = vm_get_type(vm, out_scope.type);
			scope = target_type->object_scope;
			if (!scope) {
				printf("%.*s has no scope.\n", ALIT(target_type->name));
				return -1;
			}
		}
	}

	assert(scope);
	bool global_lookup =
		(node->type == EXPR_NODE_LOOKUP_GLOBAL);

	return expr_resolve_scope_lookup(vm, expr, scope,
									 node->lookup.name,
									 expected_type,
									 global_lookup,
									 result);
}

static int
expr_eval_push_args(struct vm *vm, struct expr *expr,
					struct exec_stack *stack, struct expr_node *arg)
{
	if (!arg) {
		return 0;
	}

	int err = 0;
	struct object arg_obj;

	err |= expr_eval_push_args(vm, expr, stack, arg->next_arg);

	err |= expr_eval(vm, expr, stack, arg, &arg_obj);

	if (err) {
		return err;
	}

	return 0;
}

static int
expr_eval_internal(struct vm *vm, struct expr *expr,
		struct exec_stack *stack, struct expr_node *node,
		func_type_id slot_offset, struct object *out)
{
	type_id out_type = TYPE_NONE;

	if ((node->flags & EXPR_TYPED) == 0) {
		panic("Trying to evaluate an untyped expression.");
		return EXPR_EVAL_ERROR;
	}

	switch (node->type) {

	case EXPR_NODE_FUNC_DECL: {
		struct object func_obj;
		struct obj_native_func_data *data;

		func_obj.type = expr_get_slot_type(expr, node->rule.out + slot_offset);
		assert(vm_get_type(vm, func_obj.type)->size ==
			   sizeof(struct obj_native_func_data));
		func_obj.data =
			stack_push_void(stack, sizeof(struct obj_native_func_data));

		data = func_obj.data;

		data->storage = NATIVE_FUNC_STORAGE_NODES;
		data->node.decl_node = node;
		// data->node.expr = &node->func_decl.expr;
		// data->node.node = node->func_decl.expr.body;

		out_type = func_obj.type;
	} break;

	case EXPR_NODE_FUNC_CALL: {
		int err;

		uint8_t *prev_bp = stack->bp;
		uint8_t *prev_sp = stack->sp;

		/* size_t num_args = 0; */
		/* while (arg) { */
		/* 	struct object arg_obj; */

		/* 	err = expr_eval_internal(vm, expr, stack, arg, slot_offset, &arg_obj); */
		/* 	if (err) { */
		/* 		return err; */
		/* 	} */

		/* 	num_args += 1; */
		/* 	arg = arg->next_arg; */
		/* } */

		err = expr_eval_push_args(vm, expr, stack,
								  node->func_call.args);
		if (err) {
			return EXPR_EVAL_ERROR;
		}

		stack->bp = stack->sp;

		struct object ret_obj;
		struct type *ret_type;

		struct object func_obj;

		err = expr_eval_internal(vm, expr, stack, node->func_call.func, slot_offset, &func_obj);
		if (err) {
			return err;
		}

		struct type *type = vm_get_type(vm, func_obj.type);
		assert(!type->base->abstract);

		struct type_func *type_func = type->data;

		// TODO: Is this check necessary? We already know the types
		// are ok after the type inference/check.

		/* if (type_func->num_params != num_args) { */
		/* 	printf("Wrong number of arguments. Expected %zu, got %zu.\n", */
		/* 		   type_func->num_params, num_args); */
		/* 	return -1; */
		/* } */

		assert(type->base->eval != NULL);
		type->base->eval(vm, stack, NULL);

		assert(type_func->ret == expr_get_slot_type(expr, node->rule.out + slot_offset));
		ret_type = vm_get_type(vm, type_func->ret);

		ret_obj.type = type_func->ret;
		ret_obj.data = stack->sp - ret_type->size;

		stack->sp = prev_sp;
		stack->bp = prev_bp;

		stack_push(stack, ret_obj.data, ret_type->size);

		out_type = ret_obj.type;
	} break;

	case EXPR_NODE_LOOKUP_FUNC: {
		struct expr_func_scope *func_scope = node->lookup.func_scope;

		size_t offset = 0;
		bool found = false;

		for (ssize_t i = func_scope->num_entries - 1; i >= 0; i--) {
			type_id tid = expr_get_slot_type(expr, func_scope->entry_types[i] + slot_offset);
			struct type *type = vm_get_type(vm, tid);

			offset += type->size;

			if (func_scope->entry_names[i] == node->lookup.name) {
				stack_push(stack, stack->bp - offset, type->size);
				out_type = tid;
				found = true;
				break;
			}
		}

		if (!found) {
			int err;
			struct scope_entry result;

			err = expr_resolve_scope_lookup(vm, expr,
											func_scope->outer_scope,
											node->lookup.name,
											expr_get_slot_type(expr, node->rule.out + slot_offset),
											true, &result);

			if (err) {
				return err;
			}

			if (result.object.type == TYPE_NONE) {
				assert(result.scope);
				stack_push(stack, &result.scope, sizeof(struct scope *));
				out_type = TYPE_SCOPE;
			} else {
				struct type *res_type = vm_get_type(vm, result.object.type);
				stack_push(stack, result.object.data, res_type->size);
				out_type = result.object.type;
			}
		}

	} break;

	case EXPR_NODE_LOOKUP_LOCAL:
	case EXPR_NODE_LOOKUP_GLOBAL: {
		int err;
		struct scope_entry result;

		err = expr_eval_lookup(vm, expr, stack, node,
							   expr_get_slot_type(expr, node->rule.out + slot_offset),
							   &result);
		if (err < 0) {
			return err;
		} else if (err > 0) {
			return 1;
		} else {
			if (result.object.type == TYPE_NONE) {
				assert(result.scope);
				stack_push(stack, &result.scope, sizeof(struct scope *));
				out_type = TYPE_SCOPE;
			} else {
				struct type *res_type = vm_get_type(vm, result.object.type);
				stack_push(stack, result.object.data, res_type->size);
				out_type = result.object.type;
			}
		}
	} break;

	case EXPR_NODE_SCOPE: {
		stack_push(stack, &node->scope, sizeof(struct scope *));
		out_type = TYPE_SCOPE;
	} break;

	case EXPR_NODE_STACK: {
		struct type *obj_type = vm_get_type(vm, node->obj.type);
		stack_push(stack, stack->bp + (size_t)node->obj.data, obj_type->size);
		out_type = node->obj.type;
	} break;

	case EXPR_NODE_GLOBAL: {
		struct type *obj_type = vm_get_type(vm, node->obj.type);
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

	case EXPR_NODE_TYPE_EXPR: {
		if (node->type_expr != NULL) {
			expr_eval_internal(vm, expr, stack, node->type_expr, slot_offset, NULL);
		} else {
			type_id type = TYPE_TEMPLATE_PARAM;
			stack_push(stack, &type, sizeof(type));
		}
		out_type = vm->default_types.type;
	} break;
	}

	if (out) {
		struct type *type;
		struct object res = {0};

		type = vm_get_type(vm, out_type);
		res.data = stack->sp - type->size;
		res.type = out_type;
		*out = res;
	}

	return 0;
}

int
expr_eval(struct vm *vm, struct expr *expr,
		  struct exec_stack *stack,
		  struct expr_node *node, struct object *out)
{
	return expr_eval_internal(vm, expr, stack, node, 0, out);
}

int
expr_eval_simple_offset(struct vm *vm,
						struct stg_module *mod,
				 		struct expr *expr,
				 		struct expr_node *node,
				 		func_type_id slot_offset,
				 		struct object *out)
{
	struct exec_stack stack = {0};
	// struct arena mem = arena_push(&vm->memory);

	arena_alloc_stack(&stack, &vm->memory, 1024); //mem.capacity - mem.head - 1);
	size_t head = vm->memory.head;
	vm->memory.head += 1024;
	stack.mod = mod;

	int err;
	err = expr_eval_internal(vm, expr, &stack, node, slot_offset, out);

	// arena_pop(&vm->memory, mem);
	vm->memory.head = head;

	return err;
}

int
expr_eval_simple(struct vm *vm,
				 struct stg_module *mod,
				 struct expr *expr,
				 struct expr_node *node,
				 struct object *out)
{
	return expr_eval_simple_offset(vm, mod, expr, node, 0, out);
}

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static void
expr_print_slot(struct vm *vm, struct expr *expr, func_type_id offset, func_type_id rel_slot)
{
	func_type_id slot = offset + rel_slot;

	if (expr->slots) {
		struct expr_type_slot *s;
		s = &expr->slots[slot];

		printf("[%u(%u+%i):", slot, offset, rel_slot);
		switch (SLOT_STATE(s->state)) {
			case SLOT_UNBOUND:
				printf("unbound");
				break;

			case SLOT_BOUND:
				print_type_repr(vm, vm_get_type(vm, expr_get_slot_type(expr, slot)));
				break;

			case SLOT_BOUND_REF:
				printf("->");
				expr_print_slot(vm, expr, 0, s->ref);
				break;
		}
		printf("]");
	} else {
		printf("[%u(%u+%i)]", slot, offset, rel_slot);
	}

	if (expr_slot_is_template(expr, slot)) {
		printf("[template]");
	}
}

static void
expr_print_internal(struct vm *vm, struct expr *expr, struct expr_node *node, func_type_id slot_offset, int depth)
{
	if (!node) {
		return;
	}

	_print_indent(depth);

	switch (node->type) {

	case EXPR_NODE_FUNC_DECL: {
		printf("func decl");
	} break;

	case EXPR_NODE_LOOKUP_FUNC:
		printf("%.*s (func)", ALIT(node->lookup.name));
		break;

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
		print_type_repr(vm, vm_get_type(vm, node->obj.type));
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
		if (&node->func_decl.expr != expr) {
			printf(" (orig (");
		} else {
			printf(" ((");
		}
		for (size_t i = 0; i < node->rule.abs.num_params; i++) {
			if (i != 0) {
				printf(", ");
			}
			expr_print_slot(vm, &node->func_decl.expr, 0, node->rule.abs.params[i]);
		}
		printf(") -> ");
		expr_print_slot(vm, &node->func_decl.expr, 0, node->rule.abs.ret);
		printf(") -> ");
		expr_print_slot(vm, expr, slot_offset, node->rule.out);
		printf(")");
		break;

	case EXPR_NODE_FUNC_CALL: // [APP]
		printf(" (");
		expr_print_slot(vm, expr, slot_offset, node->rule.app.func);
		printf(") (");
		for (size_t i = 0; i < node->rule.app.num_args; i++) {
			if (i != 0) {
				printf(", ");
			}
			expr_print_slot(vm, expr, slot_offset, node->rule.app.args[i]);
		}
		printf(") -> ");
		expr_print_slot(vm, expr, slot_offset, node->rule.out);

		break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL:
	case EXPR_NODE_LOOKUP_FUNC:
	case EXPR_NODE_GLOBAL:
	case EXPR_NODE_STACK:
	case EXPR_NODE_SCOPE:
	case EXPR_NODE_LIT_INT:
	case EXPR_NODE_LIT_STR: // [VAR]
		printf(" () -> ");
		expr_print_slot(vm, expr, slot_offset, node->rule.out);
		break;

	case EXPR_NODE_TYPE_EXPR:
		printf(" ");
		expr_print_slot(vm, expr, slot_offset, node->rule.out);
		printf(" <- ");
		expr_print_slot(vm, expr, slot_offset, node->rule.type);
	}

	if ((node->flags & EXPR_TYPED) == EXPR_TEMPLATE) {
		printf(" TEMPLATE");
	} else if ((node->flags & EXPR_TYPED) != 0) {
		printf(" TYPED");
	}

	if ((node->flags & EXPR_CONST) != 0) {
		printf(" CONST");
	}

	printf("\n");

	switch (node->type) {
	case EXPR_NODE_FUNC_DECL: {
		struct expr *func_expr;
		
		if (node->func_decl.use_external_expr) {
			func_expr = expr;
		} else {
			func_expr = &node->func_decl.expr;
		}
		for (size_t i = 0; i < node->func_decl.num_params; i++) {
			_print_indent(depth + 1);
			printf("param %.*s type\n", ALIT(node->func_decl.params[i].name));
			expr_print_internal(vm, func_expr,
								node->func_decl.params[i].type, slot_offset, depth + 2);
		}

		_print_indent(depth + 1);
		printf("ret type\n");
		expr_print_internal(vm, func_expr, node->func_decl.ret_type, slot_offset, depth + 2);

		_print_indent(depth + 1);
		printf("body\n");
		expr_print_internal(vm, func_expr, node->func_decl.expr.body, slot_offset, depth + 2);
  	} break;

	case EXPR_NODE_FUNC_CALL:
		_print_indent(depth + 1);
		printf("func\n");
		expr_print_internal(vm, expr, node->func_call.func, slot_offset, depth + 2);

		_print_indent(depth + 1);
		printf("args\n");
		expr_print_internal(vm, expr, node->func_call.args, slot_offset, depth + 2);
		break;

	case EXPR_NODE_LOOKUP_GLOBAL:
	case EXPR_NODE_LOOKUP_LOCAL:
		expr_print_internal(vm, expr, node->lookup.scope, slot_offset, depth + 1);
		break;

	case EXPR_NODE_TYPE_EXPR:
		if (node->type_expr) {
			expr_print_internal(vm, expr, node->type_expr, slot_offset, depth + 1);
		}
		break;

	default:
		break;
	}

	expr_print_internal(vm, expr, node->next_arg, slot_offset, depth);
}

void
expr_print(struct vm *vm, struct expr *expr)
{
	expr_print_internal(vm, expr, expr->body, 0, 0);
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
