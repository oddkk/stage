#include "mod.h"
#include "monad.h"
#include "trigger.h"
#include <ast.h>
#include <native.h>
#include <base/mod.h>

#include <ffi.h>

static ffi_type *msg_trigger_ffi_members[] = {
	&ffi_type_uint32,
	NULL,
};

static ffi_type msg_trigger_ffi_type = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = msg_trigger_ffi_members,
};

static struct object_cons *
msg_trigger_cons_from_vm(struct vm *vm)
{
	struct stg_module *msg_mod;
	msg_mod = vm_get_module(vm, vm_atoms(vm, "message"));
	assert(msg_mod);

	struct msg_context *mod_info;
	mod_info = msg_mod->data;

	return mod_info->msg_trigger_cons;
}

#define CNAME msg_trigger
#define REAL_NAME "Trigger"
#define UWT_OBJ_COPY_FUNC NULL
#define UWT_OBJ_FFI_TYPE &msg_trigger_ffi_type
#define UWT_OBJ_DATA_TYPE struct msg_trigger_data
#define UWT_TYPE_INFO_TYPE struct msg_trigger_type_info
#define EXPOSE_FUNCS 1
#include <unary_wrapping_type_template.h>
#undef UWT_OBJ_COPY_FUNC
#undef UWT_OBJ_FFI_TYPE
#undef UWT_OBJ_DATA_TYPE
#undef UWT_TYPE_INFO_TYPE
#undef CNAME
#undef REAL_NAME
#undef EXPOSE_FUNCS

struct msg_trigger_init_data {
	struct msg_trigger_data trigger;
	struct stg_func_object pipe;
	type_id type_in, type_out;
};

static void
msg_trigger_init_callback(
		struct stg_init_context *ctx, struct stg_exec *heap,
		void *data, void *out)
{
	struct msg_trigger_init_data *closure;
	closure = data;

	struct msg_system *sys;
	sys = msg_get_system(ctx->vm);

	int err;
	err = msg_trigger_subscribe(
			sys, closure->trigger.trigger,
			closure->pipe);
}

static struct stg_init_data
msg_trigger_register_init(
		struct stg_exec *heap,
		struct msg_trigger_data trigger,
		struct stg_func_object pipe,
		type_id type_in, type_id type_out)
{
	struct msg_trigger_init_data *data;
	data = stg_alloc(heap, 1, sizeof(struct msg_trigger_init_data ));

	data->trigger = trigger;
	data->pipe = pipe;
	data->type_in = type_in;
	data->type_out = type_out;

	struct stg_init_data out_monad = {0};

	out_monad.call = msg_trigger_init_callback;
	out_monad.copy = NULL;
	out_monad.data = data;
	out_monad.data_size = sizeof(struct msg_trigger_init_data);

	return out_monad;
}

static void
msg_trigger_register_on_start(struct stg_module *mod)
{
	struct msg_context *ctx;
	ctx = mod->data;

	struct msg_trigger_data data = {0};
	data.trigger = ctx->on_start_msg;

	struct object obj = {0};
	obj.type = msg_trigger_register_type(
			mod, mod->vm->default_types.unit);
	obj.data = &data;

	stg_mod_register_native_object(mod,
			mod_atoms(mod, "on_start"), obj);
}

// Used in mod.c.
void
msg_trigger_register(struct stg_module *mod)
{
	struct msg_context *ctx;
	ctx = mod->data;

	ctx->msg_trigger_cons =
		msg_trigger_register_cons(mod);
	stg_mod_register_native_cons(mod,
			mod_atoms(mod, "Trigger"), ctx->msg_trigger_cons);

	msg_trigger_register_on_start(mod);
}

// Used in mod.c.
void
msg_trigger_register_native(struct stg_native_module *mod)
{
	stg_native_register_funcs(mod, msg_trigger_register_init,
			STG_NATIVE_FUNC_HEAP);

}
