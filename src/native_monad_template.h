// Provides a basic single type parameter monade type with return, fmap, join,
// and bind.
//
// The template mainly provides the following functions:
//
// void *CNAME_register_native(struct stg_native_module *)
// Registers the native functions as `<CNAME>_monad_<FUNC NAME>`, where FUNC
// NAME is return, fmap, join, and bind.
//
// type_id CNAME_register_type(struct stg_module *mod, type_id res_type)
// Creates a new type with the given type as its member.
//
// struct object_cons *CNAME_register_cons(struct stg_module *mod)
// Creates the monad's type constructor. The constructor should be created
// during the register hook. The caller is responsible for storing the returned
// cons in a way such that it can be retreived through CNAME_cons_from_vm.
//
// void CNAME_monad_copy(struct stg_exec *ctx, MONAD_DATA_TYPE *data)
// Copies the content of data.
//
// void CNAME_monad_call(struct vm *, struct stg_exec *, struct object,
// 					struct object *out)
// Calls the monad.
//
// bool CNAME_type_is_inst(struct vm *, type_id tid)
// Returns true if the given type is an instance of this monad.
//
// type_id CNAME_return_type(struct vm *, type_id tid)
// Returns the type of the monad type's member type.

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

#include <ffi.h>

#define FUNC_CONCAT1(prefix, suffix) prefix ## _ ## suffix
#define FUNC_CONCAT(prefix, suffix) FUNC_CONCAT1(prefix, suffix)
#define MONAD_FUNC(name) FUNC_CONCAT(CNAME, name)

#define STRINGIFY1(name) STR(#name)
#define STRINGIFY(name) STRINGIFY1(name)

// The caller is expected to have defined this function to return this monad's
// type cons.
static struct object_cons *
MONAD_FUNC(cons_from_vm)(struct vm *vm);

static bool
MONAD_FUNC(type_equals)(struct vm *vm, struct type *lhs, struct type *rhs)
{
	TYPE_INFO_TYPE *lhs_info;
	TYPE_INFO_TYPE *rhs_info;

	lhs_info = (TYPE_INFO_TYPE *)lhs->data;
	rhs_info = (TYPE_INFO_TYPE *)rhs->data;

	return type_equals(vm, lhs_info->type, rhs_info->type);
}

static struct string
MONAD_FUNC(type_repr)(struct vm *vm, struct arena *mem, struct type *type)
{
	TYPE_INFO_TYPE *info;
	info = (TYPE_INFO_TYPE *)type->data;

	struct string res = {0};
	arena_string_append(mem, &res, STR(REAL_NAME "["));

	struct type *item_type;
	item_type = vm_get_type(vm, info->type);
	arena_string_append_type_repr(&res, vm, mem, item_type);

	arena_string_append(mem, &res, STR("]"));

	return res;
}

static void
MONAD_FUNC(monad_copy)(struct stg_exec *ctx, MONAD_DATA_TYPE *data);

static void
MONAD_FUNC(obj_copy)(struct stg_exec *ctx, void *type_data, void *obj_data)
{
	MONAD_DATA_TYPE *data = obj_data;
	MONAD_FUNC(monad_copy)(ctx, data);
}

static void
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

static struct type_base MONAD_FUNC(type_base) = {
	.name = STR(REAL_NAME),
	.equals = MONAD_FUNC(type_equals),
	.repr = MONAD_FUNC(type_repr),
	.obj_copy = MONAD_FUNC(obj_copy),
};

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

static type_id
MONAD_FUNC(register_type)(struct stg_module *mod, type_id res_type);

static int
MONAD_FUNC(type_pack)(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, void **params, size_t num_params)
{
	assert(num_params == 1);

	type_id tid = *(type_id *)params[0];

	type_id result_type;
	result_type = MONAD_FUNC(register_type)(
			mod, tid);

	memcpy(out, &result_type, sizeof(type_id));

	return 0;
}

static type_id
MONAD_FUNC(type_pack_type)(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	return ctx->vm->default_types.type;
}

int
MONAD_FUNC(type_unpack)(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, struct object obj, int param_id)
{
	assert_type_equals(ctx->vm,
			obj.type, ctx->vm->default_types.type);

	type_id tid = *(type_id *)obj.data;

	struct type *type;
	type = vm_get_type(ctx->vm, tid);
	// TODO: Properly report type mismatch error.
	if (type->base != &MONAD_FUNC(type_base)) {
		stg_error(ctx->err, STG_NO_LOC,
				"Expected " REAL_NAME " type, got %.*s.",
				LIT(type->base->name));
		return -1;
	}

	TYPE_INFO_TYPE *info = type->data;
	memcpy(out, &info->type, sizeof(type_id));

	return 0;
}

static type_id
MONAD_FUNC(register_type)(struct stg_module *mod, type_id res_type)
{
	TYPE_INFO_TYPE *info;
	info = arena_alloc(&mod->mem, sizeof(TYPE_INFO_TYPE));

	info->type = res_type;

	struct type type = {0};
	type.base = &MONAD_FUNC(type_base);
	type.data = info;
	type.size = sizeof(MONAD_DATA_TYPE);
	type.type_def = MONAD_FUNC(cons_from_vm)(mod->vm);
	type.ffi_type = &MONAD_FUNC(ffi_type);

	return stg_register_type(mod, type);
}

static struct object_cons *
MONAD_FUNC(register_cons)(struct stg_module *mod)
{
	struct object_cons *cons;
	cons = arena_alloc(&mod->mem,
			sizeof(struct object_cons));

	cons->num_params = 1;
	cons->params = arena_allocn(&mod->mem,
			cons->num_params, sizeof(struct object_cons_param));

	cons->params[0].name = mod_atoms(mod, "T");
	cons->params[0].type = mod->vm->default_types.type;

	cons->ct_pack      = MONAD_FUNC(type_pack);
	cons->ct_pack_type = MONAD_FUNC(type_pack_type);
	cons->ct_unpack    = MONAD_FUNC(type_unpack);

	return cons;
}

struct MONAD_FUNC(fmap_data) {
	struct stg_func_object func;
	MONAD_DATA_TYPE monad;

	size_t in_type_size;
	type_id in_type;

	size_t out_type_size;
	type_id out_type;
};

static void
MONAD_FUNC(fmap_unsafe)(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct MONAD_FUNC(fmap_data) *closure = data;

	uint8_t in_buffer[closure->in_type_size];

	closure->monad.call(vm, ctx,
			closure->monad.data, in_buffer);

	struct object arg;
	arg.type = closure->in_type;
	arg.data = in_buffer;


	struct object ret;
	ret.type = closure->out_type;
	ret.data = out;

	vm_call_func_obj(
			vm, ctx, closure->func,
			&arg, 1, &ret);
}

static void
MONAD_FUNC(fmap_copy)(struct stg_exec *ctx, void *data)
{
	struct MONAD_FUNC(fmap_data) *closure = data;
	MONAD_FUNC(monad_copy)(ctx, &closure->monad);
}

static MONAD_DATA_TYPE
MONAD_FUNC(monad_fmap)(struct stg_exec *heap,
		struct stg_module *mod,
		struct stg_func_object func,
		MONAD_DATA_TYPE monad,
		type_id type_in_id, type_id type_out_id)
{
	MONAD_DATA_TYPE data = {0};
	data.call = MONAD_FUNC(fmap_unsafe);
	data.copy = MONAD_FUNC(fmap_copy);
	data.data_size = sizeof(struct MONAD_FUNC(fmap_data));
	data.data = stg_alloc(heap, 1, data.data_size);

	struct MONAD_FUNC(fmap_data) *closure;
	closure = data.data;
	closure->func  = func;
	closure->monad = monad;

	struct type *func_type;
	func_type = vm_get_type(mod->vm,
			vm_get_func(mod->vm, func.func)->type);
	struct stg_func_type *func_info;
	func_info = func_type->data;

	assert(func_info->num_params == 1);

	assert_type_equals(mod->vm,
			type_in_id, func_info->params[0]);
	assert_type_equals(mod->vm,
			type_out_id, func_info->return_type);

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

struct MONAD_FUNC(join_data) {
	MONAD_DATA_TYPE monad;

	type_id data_type;
};

static void
MONAD_FUNC(join_copy)(struct stg_exec *ctx, void *data)
{
	struct MONAD_FUNC(join_data) *closure = data;
	MONAD_FUNC(monad_copy)(ctx, &closure->monad);
}

static void
MONAD_FUNC(join_unsafe)(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct MONAD_FUNC(join_data) *closure = data;

	MONAD_DATA_TYPE inner_monad = {0};

	closure->monad.call(vm, ctx,
			closure->monad.data, &inner_monad);

	inner_monad.call(vm, ctx,
			inner_monad.data, out);
}

static MONAD_DATA_TYPE 
MONAD_FUNC(monad_join)(struct stg_exec *heap,
		struct stg_module *mod,
		MONAD_DATA_TYPE monad,
		type_id data_type_id)
{
	MONAD_DATA_TYPE data = {0};
	data.call = MONAD_FUNC(join_unsafe);
	data.copy = MONAD_FUNC(join_copy);
	data.data_size = sizeof(struct MONAD_FUNC(join_data));
	data.data = stg_alloc(heap, 1, data.data_size);

	struct MONAD_FUNC(join_data) *closure = data.data;
	closure->monad = monad;
	closure->data_type = data_type_id;

	return data;
}

struct MONAD_FUNC(return_data) {
	void *value;
	size_t size;
	obj_copy value_copy;
	void *type_data;
};

static void
MONAD_FUNC(return_unsafe)(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct MONAD_FUNC(return_data) *closure = data;

	memcpy(out, closure->value, closure->size);
}

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
	data.call = MONAD_FUNC(return_unsafe);
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

static void
MONAD_FUNC(bind_unsafe)(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct MONAD_FUNC(bind_data) *closure = data;

	uint8_t in_buffer[closure->in_type_size];

	closure->monad.call(vm, ctx,
			closure->monad.data, in_buffer);

	struct object arg;
	arg.type = closure->in_type;
	arg.data = in_buffer;


	MONAD_DATA_TYPE out_monad = {0};
	struct object out_monad_obj;
	out_monad_obj.type = closure->out_monad_type;
	out_monad_obj.data = &out_monad;

	vm_call_func_obj(
			vm, ctx, closure->func,
			&arg, 1, &out_monad_obj);

	out_monad.call(vm, ctx, out_monad.data, out);
}

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
	data.call = MONAD_FUNC(bind_unsafe);
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
	native_func(monad_join, STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
	native_func(monad_fmap, STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);

#undef native_func

	// stg_native_register_func(mod, STRINGIFY(MONAD_FUNC(monad_return)),
	// 		STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE|STG_NATIVE_FUNC_REFS);
	// stg_native_register_func(mod, STRINGIFY(MONAD_FUNC(monad_bind)),
	// 		STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
	// stg_native_register_func(mod, STRINGIFY(MONAD_FUNC(monad_join)),
	// 		STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
	// stg_native_register_func(mod, STRINGIFY(MONAD_FUNC(monad_fmap)),
	// 		STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
}

static type_id
MONAD_FUNC(type_is_inst)(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	return type->base == &MONAD_FUNC(type_base);
}
static type_id
MONAD_FUNC(return_type)(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	assert(type->base == &MONAD_FUNC(type_base));

	TYPE_INFO_TYPE *info;
	info = type->data;

	return info->type;
}

static void
MONAD_FUNC(monad_call)(
		struct vm *vm, struct stg_exec *ctx,
		struct object obj, struct object *out)
{
	type_id ret_type_id;
	ret_type_id = MONAD_FUNC(return_type)(vm, obj.type);

	struct type *ret_type;
	ret_type = vm_get_type(vm, ret_type_id);

	assert_type_equals(vm, out->type, ret_type_id);
	assert(out->data || ret_type->size == 0);

	MONAD_DATA_TYPE *data = obj.data;

	data->call(vm, ctx, data->data, out->data);
}

#undef FUNC_CONCAT1
#undef FUNC_CONCAT
#undef MONAD_FUNC
