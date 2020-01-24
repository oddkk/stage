#include "mod.h"
#include "../ast.h"
#include "../module.h"
#include "../native.h"
#include "../utils.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

static bool
io_type_equals(struct vm *vm, struct type *lhs, struct type *rhs)
{
	struct stg_io_type_info *lhs_info;
	struct stg_io_type_info *rhs_info;

	lhs_info = (struct stg_io_type_info *)lhs->data;
	rhs_info = (struct stg_io_type_info *)rhs->data;

	return type_equals(vm, lhs_info->type, rhs_info->type);
}

static struct string
io_type_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct stg_io_type_info *info;
	info = (struct stg_io_type_info *)type->data;

	struct string res = arena_string_init(mem);
	arena_string_append(mem, &res, STR("IO["));

	struct type *item_type;
	item_type = vm_get_type(vm, info->type);
	arena_string_append_type_repr(&res, vm, mem, item_type);

	arena_string_append(mem, &res, STR("]"));

	return res;
}

static void
io_obj_copy(struct stg_exec *ctx, void *type_data, void *obj_data)
{
	struct stg_io_data *data = obj_data;

	void *new_closure = stg_alloc(ctx, 1, data->data_size);
	memcpy(new_closure, data->data, data->data_size);

	data->data = new_closure;

	if (data->copy) {
		data->copy(ctx, data->data);
	}
}

static struct type_base io_type_base = {
	.name = STR("IO"),
	.equals = io_type_equals,
	.repr = io_type_repr,
	.obj_copy = io_obj_copy,
};

static ffi_type *ffi_type_stg_io_members[] = {
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_uint64,
	NULL,
};

static ffi_type ffi_type_stg_io = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = ffi_type_stg_io_members,
};

static int
io_type_pack(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, void **params, size_t num_params)
{
	assert(num_params == 1);

	type_id tid = *(type_id *)params[0];

	type_id result_type;
	result_type = stg_register_io_type(
			mod, tid);

	memcpy(out, &result_type, sizeof(type_id));

	return 0;
}

static type_id
io_type_pack_type(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	return ctx->types.type;
}

int
io_type_unpack(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, struct object obj, int param_id)
{
	assert_type_equals(ctx->vm,
			obj.type, ctx->types.type);

	type_id tid = *(type_id *)obj.data;

	struct type *type;
	type = vm_get_type(ctx->vm, tid);
	// TODO: Properly report type mismatch error.
	if (type->base != &io_type_base) {
		return -1;
	}

	struct stg_io_type_info *info = type->data;
	memcpy(out, &info->type, sizeof(type_id));

	return 0;
}

type_id
stg_register_io_type(struct stg_module *mod, type_id res_type)
{
	struct stg_io_type_info *info;
	info = calloc(1, sizeof(struct stg_io_type_info));

	struct stg_module *base_mod;
	base_mod = vm_get_module(mod->vm, STR("base"));
	assert(base_mod);

	struct stg_base_mod_info *mod_info;
	mod_info = base_mod->data;

	info->type = res_type;

	struct type type = {0};
	type.base = &io_type_base;
	type.data = info;
	type.size = sizeof(struct stg_io_data);
	type.type_def = mod_info->io_cons;
	type.ffi_type = &ffi_type_stg_io;

	return stg_register_type(mod, type);
}

void
base_init_register_io(struct ast_context *ctx, struct stg_module *mod)
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

		cons->ct_pack      = io_type_pack;
		cons->ct_pack_type = io_type_pack_type;
		cons->ct_unpack    = io_type_unpack;

		mod_info->io_cons = cons;
	}

	{
		struct object res = {0};
		res.type = ctx->types.cons;
		res.data = &mod_info->io_cons;
		res = register_object(ctx->vm, &mod->store, res);

		struct atom *cons_name = vm_atoms(ctx->vm, "IO");

		struct ast_node *expr;
		expr = ast_init_node_lit(
				ctx, AST_NODE_NEW, STG_NO_LOC, res);

		ast_namespace_add_decl(ctx, &mod->mod, mod->mod.root,
				cons_name, expr);
	}
}

struct io_return_data {
	void *value;
	size_t size;
	obj_copy value_copy;
	void *type_data;
};

static void
io_return_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct io_return_data *closure = data;

	memcpy(out, closure->value, closure->size);
}

static void
io_return_copy(struct stg_exec *ctx, void *data)
{
	struct io_return_data *closure = data;

	if (closure->value_copy) {
		closure->value_copy(
				ctx, closure->type_data,
				closure->value);
	}
}

static void
io_monad_return(void **args, size_t num_args, void *ret)
{
	assert(num_args == 4);
	struct stg_exec *heap = *(struct stg_exec **)args[0];
	struct stg_module *mod = *(struct stg_module **)args[1];
	void *value = args[2];
	type_id value_type_id = *(type_id *)args[3];

	struct stg_io_data data = {0};
	data.call = io_return_unsafe;
	data.copy = io_return_copy;
	data.data_size = sizeof(struct io_return_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct type *value_type;
	value_type = vm_get_type(mod->vm, value_type_id);

	struct io_return_data *closure;
	closure = data.data;
	closure->size = value_type->size;
	closure->value = stg_alloc(heap, 1, closure->size);
	closure->value_copy = value_type->base->obj_copy;
	memcpy(closure->value, value, closure->size);

	memcpy(ret, &data, sizeof(struct stg_io_data));
}

struct io_bind_data {
	struct stg_func_object func;
	struct stg_io_data monad;

	size_t in_type_size;
	type_id in_type;

	size_t out_type_size;
	type_id out_type;
	type_id out_monad_type;
};

static void
io_bind_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct io_bind_data *closure = data;

	uint8_t in_buffer[closure->in_type_size];

	closure->monad.call(vm, ctx,
			closure->monad.data, in_buffer);

	struct object arg;
	arg.type = closure->in_type;
	arg.data = in_buffer;


	struct stg_io_data out_monad = {0};
	struct object out_monad_obj;
	out_monad_obj.type = closure->out_monad_type;
	out_monad_obj.data = &out_monad;

	vm_call_func_obj(
			vm, ctx, closure->func,
			&arg, 1, &out_monad_obj);

	out_monad.call(vm, ctx, out_monad.data, out);
}

static void
io_bind_copy(struct stg_exec *ctx, void *data)
{
	struct io_bind_data *closure = data;

	if (closure->monad.copy) {
		closure->monad.copy(ctx, closure->monad.data);
	}
}

static struct stg_io_data
io_monad_bind(struct stg_exec *heap,
		struct stg_module *mod,
		struct stg_io_data monad,
		struct stg_func_object func,
		type_id type_in_id, type_id type_out_id)
{
	struct stg_io_data data = {0};
	data.call = io_bind_unsafe;
	data.copy = io_bind_copy;
	data.data_size = sizeof(struct io_bind_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct io_bind_data *closure;
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
		stg_register_io_type(mod, type_out_id);

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

struct io_print_int_data {
	int64_t value;
};

static void
io_print_int_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct io_print_int_data *closure = data;

	printf(" => %zu\n", closure->value);
}

static struct stg_io_data
io_monad_print_int(struct stg_exec *heap, struct stg_module *mod, int64_t val)
{
	struct stg_io_data data = {0};
	data.call = io_print_int_unsafe;
	data.data_size = sizeof(struct io_print_int_data);
	data.data = stg_alloc(heap, 1, sizeof(struct io_print_int_data));

	struct io_print_int_data *closure;
	closure = data.data;
	closure->value = val;

	return data;
}

void
base_io_register_native(struct stg_native_module *mod)
{
	stg_native_register_funcs(mod, io_monad_return,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE|STG_NATIVE_FUNC_REFS);
	stg_native_register_funcs(mod, io_monad_bind,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
	stg_native_register_funcs(mod, io_monad_print_int,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
}

type_id
stg_io_get_return_type(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	assert(type->base = &io_type_base);

	struct stg_io_type_info *info;
	info = type->data;

	return info->type;
}

void
stg_unsafe_call_io(
		struct vm *vm, struct stg_exec *ctx,
		struct object obj, struct object *out)
{
	type_id ret_type_id;
	ret_type_id = stg_io_get_return_type(vm, obj.type);

	struct type *ret_type;
	ret_type = vm_get_type(vm, ret_type_id);

	assert_type_equals(vm, out->type, ret_type_id);
	assert(out->data || ret_type->size == 0);

	struct stg_io_data *data = obj.data;

	data->call(vm, ctx, data->data, out->data);
}
