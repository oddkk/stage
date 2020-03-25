// Provides a basic single type parameter monade type with return and bind.
//
// The template mainly provides the following functions, in addition to the
// unary wrapping type's functions:
//
// void *CNAME_register_native(struct stg_native_module *)
// Registers the native functions as `<CNAME>_monad_<FUNC NAME>`, where FUNC
// NAME is return and bind.
//
// void CNAME_monad_copy(struct stg_exec *ctx, MONAD_DATA_TYPE *data)
// Copies the content of data.
//
// void CNAME_monad_call(struct vm *, struct stg_exec *, struct object,
// 					struct object *out)
// Calls the monad.

// A prefix applied to all members of this template.
#ifndef CNAME
#error "CNAME macro must be defined when importing the native monad template."
#endif

// REAL_NAME should be the name of the monad used in the language.
#ifndef REAL_NAME
#error "REAL_NAME must be defined when importing the native monad template."
#endif

// The datatype named to by MONAD_DATA_TYPE is expected to contain the
// following members as its first.
// void (*call)(struct vm *, struct stg_exec *, void *data, void *out);
// void (*copy)(struct stg_exec *, void *data);
// void *data;
// size_t data_size;
#ifndef MONAD_DATA_TYPE
#error "MONAD_TYPE must be defined when importing the native monad template."
#endif

// The datatype named to by TYPE_INFO_TYPE is expected to contain a member
// `type_id type` and will be used as the type's data.
#ifndef TYPE_INFO_TYPE
#error "TYPE_INFO_TYPE must be defined when importing the native monad template."
#endif

#ifndef MONAD_CALLBACK_PARAMS
#error "MONAD_CALLBACK_PARAMS must be defined when importing the native monad template."
#endif

#ifndef EXPOSE_FUNCS
#define EXPOSE_FUNCS 0
#endif

#include <ffi.h>

#define FUNC_CONCAT1(prefix, suffix) prefix ## _ ## suffix
#define FUNC_CONCAT(prefix, suffix) FUNC_CONCAT1(prefix, suffix)
#define MONAD_FUNC(name) FUNC_CONCAT(CNAME, name)

#define STRINGIFY1(name) STR(#name)
#define STRINGIFY(name) STRINGIFY1(name)

#if EXPOSE_FUNCS
#define MONAD_FUNC_EXPOSED
#else
#define MONAD_FUNC_EXPOSED static
#endif

MONAD_FUNC_EXPOSED void
MONAD_FUNC(monad_copy)(struct stg_exec *ctx, MONAD_DATA_TYPE *data);

static void
MONAD_FUNC(obj_copy)(struct stg_exec *ctx, void *type_data, void *obj_data)
{
	MONAD_DATA_TYPE *data = obj_data;
	MONAD_FUNC(monad_copy)(ctx, data);
}

MONAD_FUNC_EXPOSED void
MONAD_FUNC(monad_copy)(struct stg_exec *ctx, MONAD_DATA_TYPE *data)
{
	void *new_closure = NULL;
	if (data->data_size > 0) {
		new_closure = stg_alloc(ctx, 1, data->data_size);
		memcpy(new_closure, data->data, data->data_size);
	}

	data->data = new_closure;

	if (data->copy) {
		data->copy(ctx, data->data);
	}
}

static ffi_type *MONAD_FUNC(ffi_type_members)[] = {
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_uint64,
	NULL,
};

static ffi_type MONAD_FUNC(ffi_type) = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = MONAD_FUNC(ffi_type_members),
};

#define UWT_TYPE_INFO_TYPE TYPE_INFO_TYPE
#define UWT_OBJ_COPY_FUNC MONAD_FUNC(obj_copy)
#define UWT_OBJ_FFI_TYPE &MONAD_FUNC(ffi_type)
#define UWT_OBJ_DATA_TYPE MONAD_DATA_TYPE
#include "unary_wrapping_type_template.h"
#undef UWT_OBJ_COPY_FUNC
#undef UWT_OBJ_FFI_TYPE
#undef UWT_OBJ_DATA_TYPE
#undef UWT_TYPE_INFO_TYPE


struct MONAD_FUNC(return_data) {
	void *value;
	size_t size;
	obj_copy value_copy;
	void *type_data;
};

static MONAD_CALLBACK_RET
MONAD_FUNC(return_callback)(MONAD_CALLBACK_PARAMS);

static void
MONAD_FUNC(return_copy)(struct stg_exec *ctx, void *data)
{
	struct MONAD_FUNC(return_data) *closure = data;

	void *new_value = NULL;

	if (closure->size > 0) {
		new_value = stg_alloc(ctx, 1, closure->size);
		memcpy(new_value, closure->value, closure->size);
	}

	closure->value = new_value;

	if (closure->value_copy) {
		closure->value_copy(
				ctx, closure->type_data,
				closure->value);
	}
}

static void
MONAD_FUNC(monad_return)(void **args, size_t num_args, void *ret)
{
	assert(num_args == 4);
	struct stg_exec *heap = *(struct stg_exec **)args[0];
	struct stg_module *mod = *(struct stg_module **)args[1];
	void *value = args[2];
	type_id value_type_id = *(type_id *)args[3];

	MONAD_DATA_TYPE data = {0};
	data.call = MONAD_FUNC(return_callback);
	data.copy = MONAD_FUNC(return_copy);
	data.data_size = sizeof(struct MONAD_FUNC(return_data));
	data.data = stg_alloc(heap, 1, data.data_size);

	struct type *value_type;
	value_type = vm_get_type(mod->vm, value_type_id);

	struct MONAD_FUNC(return_data) *closure;
	closure = data.data;
	closure->size = value_type->size;
	closure->value = stg_alloc(heap, 1, closure->size);
	closure->value_copy = value_type->base->obj_copy;
	memcpy(closure->value, value, closure->size);

	memcpy(ret, &data, sizeof(MONAD_DATA_TYPE));
}

struct MONAD_FUNC(bind_data) {
	struct stg_func_object func;
	MONAD_DATA_TYPE monad;

	size_t in_type_size;
	type_id in_type;

	size_t out_type_size;
	type_id out_type;
	type_id out_monad_type;
};

static MONAD_CALLBACK_RET
MONAD_FUNC(bind_callback)(MONAD_CALLBACK_PARAMS);

static void
MONAD_FUNC(bind_copy)(struct stg_exec *ctx, void *data)
{
	struct MONAD_FUNC(bind_data) *closure = data;

	MONAD_FUNC(monad_copy)(ctx, &closure->monad);
}

static MONAD_DATA_TYPE
MONAD_FUNC(monad_bind)(struct stg_exec *heap,
		struct stg_module *mod,
		MONAD_DATA_TYPE monad,
		struct stg_func_object func,
		type_id type_in_id, type_id type_out_id)
{
	MONAD_DATA_TYPE data = {0};
	data.call = MONAD_FUNC(bind_callback);
	data.copy = MONAD_FUNC(bind_copy);
	data.data_size = sizeof(struct MONAD_FUNC(bind_data));
	data.data = stg_alloc(heap, 1, data.data_size);

	struct MONAD_FUNC(bind_data) *closure;
	closure = data.data;
	closure->func  = func;
	closure->monad = monad;

	struct type *func_type;
	func_type = vm_get_type(mod->vm,
			vm_get_func(mod->vm, func.func)->type);
	struct stg_func_type *func_info;
	func_info = func_type->data;

	assert(func_info->num_params == 1);

	closure->out_monad_type =
		MONAD_FUNC(register_type)(mod, type_out_id);

	assert_type_equals(mod->vm,
			type_in_id, func_info->params[0]);
	assert_type_equals(mod->vm,
			closure->out_monad_type, func_info->return_type);

	struct type *type_out;
	type_out = vm_get_type(mod->vm, type_out_id);

	closure->out_type_size = type_out->size;
	closure->out_type = type_out_id;

	struct type *type_in;
	type_in = vm_get_type(mod->vm, type_in_id);

	closure->in_type_size = type_in->size;
	closure->in_type = type_in_id;

	return data;
}

static void
MONAD_FUNC(register_native)(struct stg_native_module *mod)
{
#define native_func(name, flags) \
	stg_native_register_func(mod, \
			STRINGIFY(MONAD_FUNC(name)), \
			(void *)MONAD_FUNC(name), \
			flags)

	native_func(monad_return,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE|STG_NATIVE_FUNC_REFS);
	native_func(monad_bind, STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);

#undef native_func
}

#undef FUNC_CONCAT1
#undef FUNC_CONCAT
#undef MONAD_FUNC
#undef EXPOSE_FUNCS
#undef MONAD_FUNC_EXPOSED
#undef STRINGIFY1
#undef STRINGIFY
