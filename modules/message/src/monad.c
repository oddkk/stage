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
#define REAL_NAME "Message"
#define MONAD_DATA_TYPE struct msg_monad_data
#define TYPE_INFO_TYPE struct msg_monad_type_info
#define MONAD_EXTRA_PARAMS struct msg_system *msg_sys
#define MONAD_EXTRA_ARGS msg_sys
#define EXPOSE_FUNCS 1
#include <native_monad_template.h>

static void
msg_on_start_unsafe(struct vm *vm, struct stg_exec *ctx,
		void *data, void *out, struct msg_system *sys)
{
}

static void
msg_monad_on_start_register(struct stg_module *mod)
{
	struct msg_monad_data data = {0};
	data.call = msg_on_start_unsafe;
	data.copy = NULL;
	data.data = NULL;
	data.data_size = 0;

	struct object obj = {0};
	obj.type = msg_register_type(
			mod, mod->vm->default_types.unit);
	obj.data = &data;

	stg_mod_register_native_object(mod,
			mod_atoms(mod, "onStart"), obj);
}

// Used in mod.c.
void
msg_monad_register(struct stg_module *mod)
{
	struct msg_context *ctx;
	ctx = mod->data;

	ctx->msg_type_cons =
		msg_register_cons(mod);

	msg_monad_on_start_register(mod);
}

// Used in mod.c.
void
msg_monad_register_native(struct stg_native_module *mod)
{
	msg_register_native(mod);
}
