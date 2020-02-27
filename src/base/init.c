#include "mod.h"
#include "../ast.h"
#include "../module.h"
#include "../native.h"
#include "../utils.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

static bool
init_type_equals(struct vm *vm, struct type *lhs, struct type *rhs)
{
	struct stg_init_type_info *lhs_info;
	struct stg_init_type_info *rhs_info;

	lhs_info = (struct stg_init_type_info *)lhs->data;
	rhs_info = (struct stg_init_type_info *)rhs->data;

	return type_equals(vm, lhs_info->type, rhs_info->type);
}

static struct string
init_type_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct stg_init_type_info *info;
	info = (struct stg_init_type_info *)type->data;

	struct string res = arena_string_init(mem);
	arena_string_append(mem, &res, STR("Init["));

	struct type *item_type;
	item_type = vm_get_type(vm, info->type);
	arena_string_append_type_repr(&res, vm, mem, item_type);

	arena_string_append(mem, &res, STR("]"));

	return res;
}

static void
init_obj_copy(struct stg_exec *ctx, void *type_data, void *obj_data)
{
	struct stg_init_data *data = obj_data;

	void *new_closure = stg_alloc(ctx, 1, data->data_size);
	memcpy(new_closure, data->data, data->data_size);

	data->data = new_closure;

	if (data->copy) {
		data->copy(ctx, data->data);
	}
}

static struct type_base init_type_base = {
	.name = STR("Init"),
	.equals = init_type_equals,
	.repr = init_type_repr,
	.obj_copy = init_obj_copy,
};

static ffi_type *ffi_type_stg_init_members[] = {
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_uint64,
	NULL,
};

static ffi_type ffi_type_stg_init = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = ffi_type_stg_init_members,
};

static int
init_type_pack(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, void **params, size_t num_params)
{
	assert(num_params == 1);

	type_id tid = *(type_id *)params[0];

	type_id result_type;
	result_type = stg_register_init_type(
			mod, tid);

	memcpy(out, &result_type, sizeof(type_id));

	return 0;
}

static type_id
init_type_pack_type(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	return ctx->types.type;
}

int
init_type_unpack(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, struct object obj, int param_id)
{
	assert_type_equals(ctx->vm,
			obj.type, ctx->types.type);

	type_id tid = *(type_id *)obj.data;

	struct type *type;
	type = vm_get_type(ctx->vm, tid);
	// TODO: Properly report type mismatch error.
	if (type->base != &init_type_base) {
		stg_error(ctx->err, STG_NO_LOC,
				"Expected Init type, got %.*s.",
				LIT(type->base->name));
		return -1;
	}

	struct stg_init_type_info *info = type->data;
	memcpy(out, &info->type, sizeof(type_id));

	return 0;
}

type_id
stg_register_init_type(struct stg_module *mod, type_id res_type)
{
	struct stg_init_type_info *info;
	info = calloc(1, sizeof(struct stg_init_type_info));

	struct stg_module *base_mod;
	base_mod = vm_get_module(mod->vm, mod_atoms(mod, "base"));
	assert(base_mod);

	struct stg_base_mod_info *mod_info;
	mod_info = base_mod->data;

	info->type = res_type;

	struct type type = {0};
	type.base = &init_type_base;
	type.data = info;
	type.size = sizeof(struct stg_init_data);
	type.type_def = mod_info->init_cons;
	type.ffi_type = &ffi_type_stg_init;

	return stg_register_type(mod, type);
}

void
base_init_register_init(struct ast_context *ctx, struct stg_module *mod)
{
	struct stg_base_mod_info *mod_info;
	mod_info = mod->data;

	{
		struct object_cons *cons;
		cons = calloc(1,
				sizeof(struct object_cons));

		cons->num_params = 1;
		cons->params = calloc(cons->num_params,
				sizeof(struct object_cons_param));

		cons->params[0].name = mod_atoms(mod, "T");
		cons->params[0].type = mod->vm->default_types.type;

		cons->ct_pack      = init_type_pack;
		cons->ct_pack_type = init_type_pack_type;
		cons->ct_unpack    = init_type_unpack;

		mod_info->init_cons = cons;
	}

	{
		struct object res = {0};
		res.type = ctx->types.cons;
		res.data = &mod_info->init_cons;
		res = register_object(ctx->vm, &mod->store, res);

		struct atom *cons_name = vm_atoms(ctx->vm, "Init");

		struct ast_node *expr;
		expr = ast_init_node_lit(
				ctx, AST_NODE_NEW, STG_NO_LOC, res);

		ast_namespace_add_decl(ctx, &mod->mod, mod->mod.root,
				cons_name, expr);
	}
}

struct init_print_int_data {
	int64_t value;
};

static void
init_print_int_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct init_print_int_data *closure;
	closure = data;

	printf(" = %zi\n", closure->value);
}

static struct stg_init_data
init_monad_print_int(struct stg_exec *heap, int64_t val)
{
	struct stg_init_data data = {0};
	data.call = init_print_int_unsafe;
	data.data_size = sizeof(struct init_print_int_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct init_print_int_data *closure;
	closure = data.data;
	closure->value = val;

	return data;
}

struct init_fmap_data {
	struct stg_func_object func;
	struct stg_init_data monad;

	size_t in_type_size;
	type_id in_type;

	size_t out_type_size;
	type_id out_type;
};

static void
init_fmap_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct init_fmap_data *closure = data;

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
init_fmap_copy(struct stg_exec *ctx, void *data)
{
	struct init_fmap_data *closure = data;

	if (closure->monad.copy) {
		closure->monad.copy(ctx, closure->monad.data);
	}
}

static struct stg_init_data
init_monad_fmap(struct stg_exec *heap,
		struct stg_module *mod,
		struct stg_func_object func,
		struct stg_init_data monad,
		type_id type_in_id, type_id type_out_id)
{
	struct stg_init_data data = {0};
	data.call = init_fmap_unsafe;
	data.copy = init_fmap_copy;
	data.data_size = sizeof(struct init_fmap_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct init_fmap_data *closure;
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

struct init_return_data {
	void *value;
	size_t size;
	obj_copy value_copy;
	void *type_data;
};

static void
init_return_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct init_return_data *closure = data;

	memcpy(out, closure->value, closure->size);
}

static void
init_return_copy(struct stg_exec *ctx, void *data)
{
	struct init_return_data *closure = data;

	if (closure->value_copy) {
		closure->value_copy(
				ctx, closure->type_data,
				closure->value);
	}
}

static void
init_monad_return(void **args, size_t num_args, void *ret)
{
	assert(num_args == 4);
	struct stg_exec *heap = *(struct stg_exec **)args[0];
	struct stg_module *mod = *(struct stg_module **)args[1];
	void *value = args[2];
	type_id value_type_id = *(type_id *)args[3];

	struct stg_init_data data = {0};
	data.call = init_return_unsafe;
	data.copy = init_return_copy;
	data.data_size = sizeof(struct init_return_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct type *value_type;
	value_type = vm_get_type(mod->vm, value_type_id);

	struct init_return_data *closure;
	closure = data.data;
	closure->size = value_type->size;
	closure->value = stg_alloc(heap, 1, closure->size);
	closure->value_copy = value_type->base->obj_copy;
	memcpy(closure->value, value, closure->size);

	memcpy(ret, &data, sizeof(struct stg_init_data));
}

struct init_join_data {
	struct stg_init_data monad;

	type_id data_type;
};

static void
init_join_copy(struct stg_exec *ctx, void *data)
{
	struct init_join_data *closure = data;

	if (closure->monad.copy) {
		closure->monad.copy(ctx, closure->monad.data);
	}
}

static void
init_join_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct init_join_data *closure = data;

	struct stg_init_data inner_monad = {0};

	closure->monad.call(vm, ctx,
			closure->monad.data, &inner_monad);

	inner_monad.call(vm, ctx,
			inner_monad.data, out);
}

static struct stg_init_data 
init_monad_join(struct stg_exec *heap,
		struct stg_module *mod,
		struct stg_init_data monad,
		type_id data_type_id)
{
	struct stg_init_data data = {0};
	data.call = init_join_unsafe;
	data.copy = init_join_copy;
	data.data_size = sizeof(struct init_join_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct init_join_data *closure = data.data;
	closure->monad = monad;
	closure->data_type = data_type_id;

	return data;
}

struct init_io_data {
	struct stg_io_data monad;

	type_id data_type;
};

static void
init_io_copy(struct stg_exec *ctx, void *data)
{
	struct init_io_data *closure = data;

	if (closure->monad.copy) {
		closure->monad.copy(ctx, closure->monad.data);
	}
}

static void
init_io_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct init_io_data *closure = data;
	closure->monad.call(vm, ctx,
			closure->monad.data, out);
}

static struct stg_init_data
init_monad_io(struct stg_exec *heap,
		struct stg_module *mod,
		struct stg_io_data monad,
		type_id data_type_id)
{
	struct stg_init_data data = {0};
	data.call = init_io_unsafe;
	data.copy = init_io_copy;
	data.data_size = sizeof(struct init_io_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct init_io_data *closure = data.data;
	closure->monad = monad;
	closure->data_type = data_type_id;

	return data;
}

void
base_init_register_native(struct stg_native_module *mod)
{
	stg_native_register_funcs(mod, init_monad_return,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE|STG_NATIVE_FUNC_REFS);
	stg_native_register_funcs(mod, init_monad_join,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
	stg_native_register_funcs(mod, init_monad_fmap,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
	stg_native_register_funcs(mod, init_monad_io,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
	stg_native_register_funcs(mod, init_monad_print_int,
			STG_NATIVE_FUNC_HEAP);
}

void
stg_unsafe_call_init(
		struct vm *vm, struct stg_exec *ctx,
		struct object obj, struct object *out)
{
	type_id ret_type_id;
	ret_type_id = stg_init_get_return_type(vm, obj.type);

	struct type *ret_type;
	ret_type = vm_get_type(vm, ret_type_id);

	assert_type_equals(vm, out->type, ret_type_id);
	assert(out->data || ret_type->size == 0);

	struct stg_init_data *data = obj.data;

	data->call(vm, ctx, data->data, out->data);
}

bool
stg_type_is_init(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);

	return type->base == &init_type_base;
}

type_id
stg_init_get_return_type(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	assert(type->base == &init_type_base);

	struct stg_init_type_info *info;
	info = type->data;

	return info->type;
}
