#include "mod.h"
#include "monad.h"
#include <ast.h>
#include <native.h>
#include <base/mod.h>

static struct object_cons *
msg_cons_from_vm(struct vm *vm)
{
	struct stg_module *msg_mod;
	msg_mod = vm_get_module(vm, vm_atoms(vm, "message"));
	assert(msg_mod);

	struct msg_context *mod_info;
	mod_info = msg_mod->data;

	return mod_info->msg_type_cons;
}

#define CNAME msg
#define REAL_NAME "Msg"
#define MONAD_DATA_TYPE struct msg_monad_data
#define TYPE_INFO_TYPE struct msg_monad_type_info
#define MONAD_CALLBACK_RET void
#define MONAD_CALLBACK_PARAMS struct vm *vm, struct stg_exec *ctx, void *data, void *out
#define EXPOSE_FUNCS 1
#include <native_monad_template.h>
#undef CNAME
#undef REAL_NAME
#undef MONAD_DATA_TYPE
#undef TYPE_INFO_TYPE
#undef MONAD_CALLBACK_RET
#undef MONAD_CALLBACK_PARAMS
#undef EXPOSE_FUNCS

static void
msg_return_callback(struct vm *vm, struct stg_exec *ctx,
		void *data, void *out)
{
	struct msg_return_data *closure = data;

	memcpy(out, closure->value, closure->size);
}

static void
msg_bind_callback(struct vm *vm, struct stg_exec *ctx,
		void *data, void *out)
{
	struct msg_bind_data *closure = data;

	uint8_t in_buffer[closure->in_type_size];

	closure->monad.call(vm, ctx,
			closure->monad.data, in_buffer);

	struct object arg;
	arg.type = closure->in_type;
	arg.data = in_buffer;


	struct msg_monad_data out_monad = {0};
	struct object out_monad_obj;
	out_monad_obj.type = closure->out_monad_type;
	out_monad_obj.data = &out_monad;

	vm_call_func_obj(
			vm, ctx, closure->func,
			&arg, 1, &out_monad_obj);

	out_monad.call(vm, ctx, out_monad.data, out);
}

struct msg_monad_io_data {
	struct stg_io_data monad;
	type_id data_type;
};

void
msg_monad_io_callback(struct vm *vm, struct stg_exec *ctx,
		void *data, void *out)
{
	struct msg_monad_io_data *closure = data;
	closure->monad.call(vm, ctx,
			closure->monad.data, out);
}

void
msg_monad_io_copy(struct stg_exec *ctx, void *data)
{
	struct msg_monad_io_data *closure = data;
	stg_monad_io_copy(ctx, &closure->monad);
}

static struct msg_monad_data
msg_monad_io(struct stg_exec *heap,
		struct stg_io_data monad,
		type_id data_type_id)
{
	struct msg_monad_data data = {0};
	data.call = msg_monad_io_callback;
	data.copy = msg_monad_io_copy;
	data.data_size = sizeof(struct msg_monad_io_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct msg_monad_io_data *closure = data.data;
	closure->monad = monad;
	closure->data_type = data_type_id;

	return data;
}

// Used in mod.c.
void
msg_monad_register(struct stg_module *mod)
{
	struct msg_context *ctx;
	ctx = mod->data;

	ctx->msg_type_cons =
		msg_register_cons(mod);

	stg_mod_register_native_cons(mod,
			mod_atoms(mod, "Msg"), ctx->msg_type_cons);
}

// Used in mod.c.
void
msg_monad_register_native(struct stg_native_module *mod)
{
	msg_register_native(mod);

	stg_native_register_funcs(mod, msg_monad_io,
			STG_NATIVE_FUNC_HEAP);
}

void
msg_monad_call(
		struct vm *vm, struct stg_exec *ctx,
		struct object obj, struct object *out)
{
	type_id ret_type_id;
	ret_type_id = msg_return_type(vm, obj.type);

	struct type *ret_type;
	ret_type = vm_get_type(vm, ret_type_id);

	assert_type_equals(vm, out->type, ret_type_id);
	assert(out->data || ret_type->size == 0);

	struct stg_io_data *data = obj.data;

	data->call(vm, ctx, data->data, out->data);
}

