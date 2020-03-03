#include "mod.h"
#include "../module.h"
#include "../utils.h"
#include "../ast.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

static struct string
base_type_func_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct stg_func_type *func_info = type->data;
	struct string res = {0};

	struct type *ret_type;

	ret_type = vm_get_type(vm, func_info->return_type);

	arena_string_append(mem, &res, STR("("));

	for (size_t i = 0; i < func_info->num_params; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		struct type *item_type;
		item_type = vm_get_type(vm, func_info->params[i]);
		arena_string_append_type_repr(&res, vm, mem, item_type);
	}

	arena_string_append(mem, &res, STR(") -> "));
	arena_string_append_type_repr(&res, vm, mem, ret_type);

	return res;
}

bool
base_type_func_equals(struct vm *vm, struct type *lhs, struct type *rhs)
{
	struct stg_func_type *lhs_info;
	struct stg_func_type *rhs_info;

	lhs_info = (struct stg_func_type *)lhs->data;
	rhs_info = (struct stg_func_type *)rhs->data;

	if (!type_equals(vm,
				lhs_info->return_type,
				rhs_info->return_type)) {
		return false;
	}

	if (lhs_info->num_params != rhs_info->num_params) {
		return false;
	}

	for (size_t i = 0; i < lhs_info->num_params; i++) {
		if (!type_equals(vm,
					lhs_info->params[i],
					rhs_info->params[i])) {
			return false;
		}
	}

	return true;
}

static struct type_base func_type_base = {
	.name = STR("function"),
	.repr = base_type_func_repr,
	.equals = base_type_func_equals,
	// todo: object repr
};

static ffi_type *ffi_type_stg_func_members[] = {
	&ffi_type_uint64,
	&ffi_type_pointer,
	NULL
};

static ffi_type ffi_type_stg_func = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = ffi_type_stg_func_members,
};

type_id
stg_register_func_type(struct stg_module *mod,
		type_id ret_type, type_id *param_types, size_t num_params)
{
	struct stg_func_type *data;

	data = calloc(1, sizeof(struct stg_func_type));

	data->return_type = ret_type;
	data->num_params = num_params;
	data->params = calloc(num_params, sizeof(type_id));

	for (size_t i = 0; i < num_params; i++) {
		data->params[i] = param_types[i];
	}

	struct type type = {0};
	type.base = &func_type_base;
	type.data = data;
	// TODO: Should functions have an object def.
	// type.type_def = mod->vm->default_cons.func;
	type.size = sizeof(struct stg_func_object);
	type.ffi_type = &ffi_type_stg_func;

	return stg_register_type(mod, type);
}

void *
stg_func_ffi_cif(struct vm *vm, type_id func_tid, enum func_flags flags)
{
	struct type *type = vm_get_type(vm, func_tid);
	assert(type->base == &func_type_base);
	struct stg_func_type *func_info = type->data;

	void **final_cif;
	size_t closure_offset;
	switch (flags & (FUNC_HEAP | FUNC_CLOSURE)) {
		default:
			final_cif = &func_info->ffi_cif;
			closure_offset = 0;
			break;

		case FUNC_HEAP:
			final_cif = &func_info->ffi_cif_heap;
			closure_offset = 1;
			break;

		case FUNC_CLOSURE:
			final_cif = &func_info->ffi_cif_closure;
			closure_offset = 1;
			break;

		case FUNC_HEAP | FUNC_CLOSURE:
			final_cif = &func_info->ffi_cif_heap_closure;
			closure_offset = 2;
			break;
	}

	void *cif = *final_cif;

	if (!cif) {
		ffi_type **param_types; // [func_info->num_params];
		ffi_type *ret_type;

		param_types = calloc(
				func_info->num_params + closure_offset,
				sizeof(ffi_type *));

		size_t pre_param_i = 0;
		if ((flags & FUNC_HEAP) != 0) {
			param_types[pre_param_i] = &ffi_type_pointer;
			pre_param_i += 1;
		}
		if ((flags & FUNC_CLOSURE) != 0) {
			param_types[pre_param_i] = &ffi_type_pointer;
			pre_param_i += 1;
		}
		assert(closure_offset == pre_param_i);

		for (size_t i = 0; i < func_info->num_params; i++) {
			struct type *param_type;
			param_type = vm_get_type(vm, func_info->params[i]);
			if (!param_type->ffi_type) {
				printf("Type '");
				print_type_repr(vm, param_type);
				printf("' is missing a ffi_type.\n");
				free(param_types);
				abort();
				return NULL;
			}
			param_types[closure_offset+i] = param_type->ffi_type;
		}

		struct type *return_type;
		return_type = vm_get_type(vm, func_info->return_type);
		if (!return_type->ffi_type) {
			printf("Type '");
			print_type_repr(vm, return_type);
			printf("' is missing a ffi_type.\n");
			free(param_types);
			abort();
			return NULL;
		}
		ret_type = return_type->ffi_type;

		cif = calloc(1, sizeof(ffi_cif));

		int err;
		err = ffi_prep_cif(cif, FFI_DEFAULT_ABI,
				func_info->num_params+closure_offset, ret_type, param_types);
		if (err != FFI_OK) {
			printf("Failed to prepare call interface (%i).\n", err);
			free(cif);
			free(param_types);
			cif = NULL;

			return NULL;
		}

		*final_cif = cif;
	}

	return cif;
}

bool
stg_type_is_func(struct vm *vm, type_id tid)
{
	struct type *type = vm_get_type(vm, tid);
	return type->base == &func_type_base;
}

struct object
stg_register_func_object(
		struct vm *vm, struct objstore *store,
		func_id func_id, void *closure)
{
	struct stg_func_object func_obj = {0};

	func_obj.func = func_id;
	func_obj.closure = closure;

	struct func *func;
	func = vm_get_func(vm, func_id);

	struct object obj = {0};
	obj.data = &func_obj;
	obj.type = func->type;

	return register_object(vm, store, obj);
}

void
stg_func_closure_pack(struct vm *vm, void *in_data, void *out,
		void **params, size_t num_params)
{
	struct stg_func_closure_data *data = in_data;
	struct stg_func_object *func_obj = out;

	assert(data->num_members == num_params);

	uint8_t *closure_data = NULL;

	// TODO: Make a proper allocation and garbage collection system for run
	// time memory.
	if (num_params > 0) {
		closure_data = calloc(data->size, 1);

		for (size_t i = 0; i < num_params; i++) {
			memcpy(&closure_data[data->members[i].offset],
					params[i], data->members[i].size);
		}
	}

	func_obj->func = data->func;
	func_obj->closure = closure_data;
}
