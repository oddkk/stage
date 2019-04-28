#include "mod.h"
#include "../../expr.h"
#include <stdlib.h>
#include <string.h>

static struct string type_func_repr(struct vm *vm, struct arena *mem, struct type *type);
static bool type_func_unify(struct vm *vm, struct objstore *, type_id lhs, type_id rhs, type_id *out);
static struct string type_builtin_func_repr(struct vm *vm, struct arena *mem, struct type *type);
static void obj_eval_builtin_func(struct vm *vm, struct exec_stack *stack, void *data);
// static struct expr_node *type_func_expr_builtin_func(struct stg_module *mod, struct object obj);
static struct expr_node *type_func_expr_native_func(struct stg_module *mod, struct object obj);
static struct string type_native_func_repr(struct vm *vm, struct arena *mem, struct type *type);
static int obj_specialise_native_func(struct stg_module *,
                                      struct object, type_id, struct object *);
static void obj_eval_native_func(struct vm *vm, struct exec_stack *stack, void *data);
static struct string obj_native_func_repr(struct vm *vm, struct arena *mem, struct object *object);
static struct string obj_builtin_func_repr(struct vm *vm, struct arena *mem, struct object *object);
static type_id type_func_subtypes_iter(struct vm *vm, struct type *type, size_t *iter);

struct type_base base_generic_func_base = {
	.name = STR("func"),
	.repr = type_func_repr,
	.subtypes_iter = type_func_subtypes_iter,
};

struct type_base base_native_func_base = {
	.name = STR("native func"),
	.repr = type_native_func_repr,
	.call_expr = type_func_expr_native_func,
	.obj_repr = obj_native_func_repr,
	.subtypes_iter = type_func_subtypes_iter,
	.eval = obj_eval_native_func,
	.specialise = obj_specialise_native_func,
};

struct type_base base_builtin_func_base = {
	.name = STR("builtin func"),
	.repr = type_builtin_func_repr,
	// .call_expr = type_func_expr_builtin_func,
	.obj_repr = obj_builtin_func_repr,
	.subtypes_iter = type_func_subtypes_iter,
	.eval = obj_eval_builtin_func,
};


static struct string type_func_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct type_func *func = type->data;
	struct string res = arena_string_init(mem);

	struct type *ret_type;

	ret_type = vm_get_type(vm, func->ret);

	arena_string_append(mem, &res, STR("("));

	for (size_t i = 0; i < func->num_params; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		if (func->param_names) {
			arena_string_append(mem, &res, func->param_names[i]->name);
			arena_string_append(mem, &res, STR(": "));
		}

		struct type *item_type;
		item_type = vm_get_type(vm, func->param_types[i]);
		arena_string_append_type_repr(&res, vm, mem, item_type);
	}

	arena_string_append(mem, &res, STR(") -> "));
	arena_string_append_type_repr(&res, vm, mem, ret_type);

	return res;
}

static bool type_func_unify(struct vm *vm, struct objstore *store,
							type_id lhs, type_id rhs, type_id *out)
{
	struct type *lhs_type = vm_get_type(vm, lhs);
	struct type_func *lhs_func = lhs_type->data;

	struct type *rhs_type = vm_get_type(vm, rhs);
	struct type_func *rhs_func = rhs_type->data;


	if (lhs_func->num_params != rhs_func->num_params) {
		return false;
	}

	size_t num_params = lhs_func->num_params;
	struct atom **param_names = NULL;

	bool lhs_specialized = (lhs_type->base != &base_generic_func_base);
	bool rhs_specialized = (rhs_type->base != &base_generic_func_base);

	if (lhs_specialized && rhs_specialized &&
		rhs_type->base != rhs_type->base) {
		printf("Cannot unify a generic function with a specialized one.\n");
		return false;
	}

	if (lhs_func->param_names && rhs_func->param_names) {
		bool equal_param_names = true;
		for (size_t i = 0; i < num_params; i++) {
			if (lhs_func->param_names[i] == rhs_func->param_names[i]) {
				equal_param_names = false;
				break;
			}
		}

		if (equal_param_names) {
			param_names = lhs_func->param_names;
		} else {
			if (lhs_specialized) {
				param_names = lhs_func->param_names;
			} else {
				param_names = rhs_func->param_names;
			}
		}

	} else if (lhs_func->param_names) {
		param_names = lhs_func->param_names;
	} else if (rhs_func->param_names) {
		param_names = rhs_func->param_names;
	}

	type_id out_params[num_params];
	// TODO: Avoid creating new a new type if nothing has changed.

	for (size_t i = 0; i < num_params; i++) {
		if (!unify_types(vm, store,
						 lhs_func->param_types[i],
						 rhs_func->param_types[i],
						 &out_params[i])) {
			return false;
		}
	}

	type_id out_ret;

	if (!unify_types(vm, store,
					 lhs_func->ret,
					 rhs_func->ret,
					 &out_ret)) {
		return false;
	}

	enum type_function_kind kind;
	kind = TYPE_FUNCTION_GENERIC;


	if (lhs_specialized) {
		if (lhs_type->base == &base_builtin_func_base) {
			kind = TYPE_FUNCTION_BUILTIN;
		} else if (lhs_type->base == &base_native_func_base) {
			kind = TYPE_FUNCTION_NATIVE;
		}
	} else if (rhs_specialized) {
		if (rhs_type->base == &base_builtin_func_base) {
			kind = TYPE_FUNCTION_BUILTIN;
		} else if (rhs_type->base == &base_native_func_base) {
			kind = TYPE_FUNCTION_NATIVE;
		}
	}

	*out = type_register_function(vm, store, param_names, out_params,
								  num_params, out_ret, kind);

	return true;
}

static struct string type_builtin_func_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("builtin "));

	struct string type_repr;

	struct arena tmp_mem = arena_push(mem);
	type_repr = type_func_repr(vm, &tmp_mem, type);
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, &res, type_repr);

	return res;
}


static void obj_eval_builtin_func(struct vm *vm, struct exec_stack *stack, void *data)
{
	struct obj_builtin_func_data func;
	stack_pop(stack, &func, sizeof(struct obj_builtin_func_data));
	func.func(vm, stack, func.data);
}

// static struct expr_node *type_func_expr_builtin_func(struct stg_module *mod, struct object obj)
// {
// 	struct obj_builtin_func_data *func;
// 	func = obj.data;
// 	return NULL;
// }

static struct expr_node *type_func_expr_native_func(struct stg_module *mod, struct object obj)
{
	struct obj_native_func_data *func;
	func = obj.data;
	if (func->storage == NATIVE_FUNC_STORAGE_NODES) {
		// TODO: Make sure func->node.node is the actual function declaration.
		return func->node.decl_node;
	} else {
		return NULL;
	}
}

static struct string type_native_func_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("native "));

	struct string type_repr;

	struct arena tmp_mem = arena_push(mem);
	type_repr = type_func_repr(vm, &tmp_mem, type);
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, &res, type_repr);

	return res;
}

static void obj_eval_native_func(struct vm *vm, struct exec_stack *stack, void *data)
{
	struct obj_native_func_data func;
	stack_pop(stack, &func, sizeof(struct obj_native_func_data));

	switch (func.storage) {

	case NATIVE_FUNC_STORAGE_INSTR:
		vm_exec(vm, stack, func.instr.data, func.instr.length);
		break;

	case NATIVE_FUNC_STORAGE_NODES:
		assert(func.node.decl_node->type == EXPR_NODE_FUNC_DECL);
		expr_eval(vm, &func.node.decl_node->func_decl.expr, stack,
				func.node.decl_node->func_decl.expr.body, NULL);
		break;

	default:
		panic("Invalid native func storage.");
		break;
	}
}

static int obj_specialise_native_func(struct stg_module *mod,
                                      struct object obj, type_id target_type,
                                      struct object *result)
{
	struct obj_native_func_data *func;
	func = obj.data;

	struct type_func *target;
	target = (struct type_func *)vm_get_type(mod->vm, target_type)->data;

	if (func->storage == NATIVE_FUNC_STORAGE_NODES) {
		struct expr_node *decl;
		decl = func->node.decl_node;
		assert(decl->type == EXPR_NODE_FUNC_DECL);

		if (target->num_params != decl->func_decl.num_params) {
			printf("Can not specialise, mismatching number of arguments.\n");
			return -1;
		}

		struct expr_node *new_decl = calloc(1, sizeof(struct expr_node));
		*new_decl = *decl;

		struct expr_type_slot outer_slot = {0};
		struct expr outer_expr = {0};
		outer_expr.body = new_decl;
		outer_expr.num_type_slots = 1;

		// Instead of calling expr_finalize we set the slot manually to avoid
		// calling calloc.
		outer_expr.slots = &outer_slot;

		new_decl->rule.out = 0;

		struct expr *spec_expr;
		spec_expr = &new_decl->func_decl.expr;

		// TODO: Clean up alloced memory
		spec_expr->slots = calloc(spec_expr->num_type_slots, sizeof(struct expr_type_slot));
		memcpy(spec_expr->slots, decl->func_decl.expr.slots,
		       spec_expr->num_type_slots * sizeof(struct expr_type_slot));

		new_decl->func_decl.scope.template_param_objects
			= calloc(new_decl->func_decl.scope.num_template_params, sizeof(struct object));

		for (size_t i = 0; i < target->num_params; i++) {
			struct expr_node *param = new_decl->func_decl.params[i].type;
			expr_bind_type(mod, spec_expr, param->rule.type, target->param_types[i]);
		}

		expr_bind_type(mod, spec_expr, new_decl->func_decl.ret_type->rule.type, target->ret);

		for (int i = 0; i < 3; i++) {
			int err;
			err = expr_typecheck(mod, &outer_expr);

			if (err < 0) {
				free(spec_expr->slots);
				free(new_decl);
				return -1;
			} else if (err == 0) {
				struct object r = {0};
				struct obj_native_func_data data;

				r.type = expr_get_slot_type(&outer_expr, new_decl->rule.out);
				r.data = (void *)&data;

				data.storage = NATIVE_FUNC_STORAGE_NODES;
				data.node.decl_node = new_decl;

				*result = register_object(mod->vm, &mod->store, r);
				return 0;
			}
		}

		free(spec_expr->slots);
		free(new_decl->func_decl.scope.template_param_objects);
		free(new_decl);
		return 1;
	} else {
		return -1;
	}
}

static struct string obj_native_func_repr(struct vm *vm, struct arena *mem, struct object *object)
{
	struct type *type = vm_get_type(vm, object->type);
	return type_native_func_repr(vm, mem, type);
}

static struct string obj_builtin_func_repr(struct vm *vm, struct arena *mem, struct object *object)
{
	struct type *type = vm_get_type(vm, object->type);
	return type_builtin_func_repr(vm, mem, type);
}

static type_id type_func_subtypes_iter(struct vm *vm, struct type *type, size_t *iter)
{
	struct type_func *func = (struct type_func *)type->data;

	type_id result;

	if (*iter < func->num_params) {
		result = func->param_types[*iter];
	} else if (*iter == func->num_params) {
		result = func->ret;
	} else {
		result = TYPE_SUBTYPES_END;
	}

	*iter += 1;

	return result;
}

type_id type_register_function(struct vm *vm, struct objstore *store,
							   struct atom **param_names,
							   type_id *param_types, size_t num_params,
							   type_id ret, enum type_function_kind kind)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("function"));

	switch (kind) {
	case TYPE_FUNCTION_GENERIC:
		type.size = 0;
		type.base = &base_generic_func_base;
		break;

	case TYPE_FUNCTION_BUILTIN:
		type.size = sizeof(struct obj_builtin_func_data);
		type.base = &base_builtin_func_base;
		break;

	case TYPE_FUNCTION_NATIVE:
		type.size = sizeof(struct obj_native_func_data);
		type.base = &base_native_func_base;
		break;
	}

	struct type_func *data;
	data = calloc(1, sizeof(struct type_func));

	if (param_names) {
		data->param_names = calloc(num_params, sizeof(struct atom *));
		memcpy(data->param_names, param_names, num_params * sizeof(struct atom *));
	}

	data->param_types = calloc(num_params, sizeof(type_id));
	memcpy(data->param_types, param_types, num_params * sizeof(type_id));

	data->num_params = num_params;
	data->ret = ret;

	type.data = data;

	type.num_template_params = 0;

	for (size_t i = 0; i < data->num_params; i++) {
		struct type *param_type = vm_get_type(vm, data->param_types[i]);

		type.num_template_params += param_type->num_template_params;
	}

	struct type *ret_type = vm_get_type(vm, ret);
	type.num_template_params += ret_type->num_template_params;

	return register_type(store, type);
}

struct object
obj_register_builtin_func(struct vm *vm, struct objstore *store,
						  struct atom **param_names,
						  type_id *params, size_t num_params,
						  type_id ret_type, vm_builtin_func value,
						  void *data)
{
	struct object result = {0};
	struct obj_builtin_func_data obj_data = {0};

	obj_data.func = value;
	obj_data.data = data;

	type_id type = type_register_function(vm, store, param_names, params,
										  num_params, ret_type,
										  TYPE_FUNCTION_BUILTIN);

	result.type = type;
	result.data = &obj_data;

	return register_object(vm, store, result);
}

void
base_register_func(struct stg_module *mod)
{
	type_base_register_unifier(&base_generic_func_base,
							   &base_generic_func_base,
							   type_func_unify);

	type_base_register_unifier(&base_native_func_base,
							   &base_native_func_base,
							   type_func_unify);
	type_base_register_unifier(&base_generic_func_base,
							   &base_native_func_base,
							   type_func_unify);

	type_base_register_unifier(&base_builtin_func_base,
							   &base_builtin_func_base,
							   type_func_unify);
	type_base_register_unifier(&base_generic_func_base,
							   &base_builtin_func_base,
							   type_func_unify);
}
