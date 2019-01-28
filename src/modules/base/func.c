#include "mod.h"
#include "../../expr.h"
#include <stdlib.h>
#include <string.h>

static struct string type_func_repr(struct vm *vm, struct arena *mem, struct type *type);
static bool type_func_unify(struct vm *vm, struct objstore *, type_id lhs, type_id rhs, type_id *out);
static struct string type_builtin_func_repr(struct vm *vm, struct arena *mem, struct type *type);
static void obj_eval_builtin_func(struct vm *vm, struct exec_stack *stack, void *data);
static struct string type_native_func_repr(struct vm *vm, struct arena *mem, struct type *type);
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
	.obj_repr = obj_native_func_repr,
	.subtypes_iter = type_func_subtypes_iter,
	.eval = obj_eval_native_func,
};

struct type_base base_builtin_func_base = {
	.name = STR("builtin func"),
	.repr = type_builtin_func_repr,
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
		expr_eval(vm, func.node.expr, stack, func.node.node, NULL);
		break;

	default:
		panic("Invalid native func storage.");
		break;
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

/* obj_id obj_register_builtin_func_from_tuple(struct vm *vm, struct objstore *store, */
/* 											type_id params, type_id ret_type, */
/* 											vm_builtin_func value, void *data) */
/* { */
/* 	struct type *params_type = vm_get_type(vm, params); */
/* 	assert(params_type->base == &vm->default_types.tuple_base); */
/* 	struct type_tuple *tuple = params_type->data; */
/* 	assert(tuple->named == true); */

/* 	return obj_register_builtin_func(vm, store, */
/* 									 tuple->names, */
/* 									 tuple->types, */
/* 									 tuple->num_items, */
/* 									 ret_type, */
/* 									 value, data); */
/* } */


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
