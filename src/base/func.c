#include "mod.h"
#include "../module.h"
#include "../utils.h"
#include "../ast.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

#define FUNC_PARAM_RET 0
#define FUNC_PARAM_PARAMS 1

static struct object
base_func_unpack(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, int param_id, struct object obj)
{
	assert_type_equals(ctx->vm, obj.type, ctx->types.type);
	type_id tid = *(type_id *)obj.data;
	struct type *type = vm_get_type(ctx->vm, tid);
	struct stg_func_type *data = (struct stg_func_type *)type->data;

	switch (param_id) {
		case FUNC_PARAM_RET:
			{
				struct object res;
				res.type = ctx->types.type;
				res.data = &data->return_type;

				return res;
			}

		case FUNC_PARAM_PARAMS:
			{
				struct object res;

				res.type = data->params_type;
				res.data = data->params;

				return res;
			}
	}

	panic("Invalid param %i requested from function type.", param_id);
	return OBJ_NONE;
}

static struct object
base_func_pack(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_object_def *def, ast_slot_id obj_slot)
{
	struct ast_env_slot func_type_slot;
	func_type_slot = ast_env_slot(ctx, &mod->env, obj_slot);

	if (func_type_slot.kind == AST_SLOT_CONST_TYPE) {
		struct object obj = {0};
		obj.type = ctx->types.type;
		obj.data = &func_type_slot.const_type;
		return obj;
	}

	assert(func_type_slot.kind == AST_SLOT_CONS);

	ast_slot_id param_type_list_slot_id;
	param_type_list_slot_id =
		ast_unpack_arg_named(ctx, &mod->env, obj_slot,
				AST_BIND_NEW,
				ctx->atoms.func_cons_arg_params);

	struct ast_env_slot param_type_list_slot;
	param_type_list_slot = ast_env_slot(ctx, &mod->env,
			param_type_list_slot_id);

	assert(param_type_list_slot.kind == AST_SLOT_CONS_ARRAY);

	struct ast_array *param_array;
	param_array = &param_type_list_slot.cons_array;

	size_t num_params = param_array->num_members;
	type_id ret_type;
	type_id param_types[num_params];

	for (size_t i = 0; i < num_params; i++) {
		struct object param_type_obj;
		int err;

		err = ast_slot_pack(ctx, mod, &mod->env,
				param_array->members[i], &param_type_obj);
		if (err) {
			// ast_print_slot(ctx, &mod->env, param_array->members[i]);
			printf("\nFailed to pack func param type.\n");
			return OBJ_NONE;
		}

		assert_type_equals(ctx->vm, param_type_obj.type, ctx->types.type);

		param_types[i] = *(type_id *)param_type_obj.data;
	}

	ast_slot_id ret_type_slot_id;
	ret_type_slot_id = ast_unpack_arg_named(ctx, &mod->env, obj_slot,
			AST_BIND_NEW,
			ctx->atoms.func_cons_arg_ret);

	struct object ret_type_obj;
	int err;

	err = ast_slot_pack(ctx, mod, &mod->env,
			ret_type_slot_id, &ret_type_obj);
	if (err) {
		printf("Failed to pack func ret type.\n");
		return OBJ_NONE;
	}

	assert_type_equals(ctx->vm, ret_type_obj.type, ctx->types.type);

	ret_type = *(type_id *)ret_type_obj.data;
	
	type_id func_type_id =
		stg_register_func_type(mod->stg_mod, ret_type,
				param_types, num_params);

	struct object func_type_obj;
	func_type_obj.type = ctx->types.type;
	func_type_obj.data = &func_type_id;
	return register_object(ctx->vm, env->store, func_type_obj);
}

void
base_bootstrap_register_func(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_object_def *func_type_def =
		ast_object_def_register(&mod->store);

	ast_slot_id func_params_type = ast_bind_slot_cons(
			ctx, &func_type_def->env, AST_BIND_NEW,
			ctx->cons.array);

	ast_slot_id func_params_T =
		ast_unpack_arg_named(ctx, &func_type_def->env,
				func_params_type, AST_BIND_NEW,
				ctx->atoms.array_cons_arg_type);
	func_params_T = ast_bind_slot_templ(
			ctx, &func_type_def->env, func_params_T, AST_SLOT_TYPE);

	ast_slot_id func_params_N =
		ast_unpack_arg_named(ctx, &func_type_def->env,
				func_params_type, AST_BIND_NEW,
				ctx->atoms.array_cons_arg_count);
	func_params_N = ast_bind_slot_templ(
			ctx, &func_type_def->env, func_params_N,
			ast_bind_slot_const_type(ctx, &func_type_def->env,
				ast_env_slot(ctx, &func_type_def->env, func_params_N).type,
				ctx->types.integer));

	ast_slot_id func_params = ast_bind_slot_templ(
			ctx, &func_type_def->env, AST_BIND_NEW, func_params_type);

	ast_slot_id func_ret = ast_bind_slot_templ(
			ctx, &func_type_def->env, AST_BIND_NEW,
			AST_SLOT_TYPE);

	// NOTE: Setting TYPE_NONE for the params type is a hack to allow this a
	// parametric type for this parameter.
	struct ast_object_def_param func_type_params[] = {
		{FUNC_PARAM_RET,    ctx->atoms.func_cons_arg_ret,    ctx->types.type, func_ret},
		{FUNC_PARAM_PARAMS, ctx->atoms.func_cons_arg_params, TYPE_NONE, func_params},
	};

	ast_object_def_finalize(func_type_def,
			func_type_params, ARRAY_LENGTH(func_type_params),
			AST_SLOT_TYPE);

	func_type_def->pack = base_func_pack;
	func_type_def->unpack = base_func_unpack;

	mod->vm->default_cons.func = func_type_def;
	ctx->cons.func = func_type_def;
}

static struct string
base_type_func_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct stg_func_type *func_info = type->data;
	struct string res = arena_string_init(mem);

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
	data->params_type =
		stg_register_array_type(mod,
				mod->vm->default_types.type, num_params);

	for (size_t i = 0; i < num_params; i++) {
		data->params[i] = param_types[i];
	}

	struct type type = {0};
	type.base = &func_type_base;
	type.data = data;
	type.type_def = mod->vm->default_cons.func;
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
