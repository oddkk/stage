#include "mod.h"
#include "message.h"
#include "monad.h"
#include <ast.h>
#include <module.h>
#include <native.h>
#include <utils.h>
#include <base/mod.h>
#include <stdlib.h>
#include <ffi.h>

// Defined in monad.c.
void
msg_monad_register(struct stg_module *mod);

void
msg_monad_register_native(struct stg_native_module *mod);

int
mod_message_register(struct stg_module *mod)
{
	struct msg_context *ctx;
	ctx = calloc(1, sizeof(struct msg_context));
	// ctx = arena_alloc(&mod->mem, sizeof(struct msg_context));
	mod->data = ctx;

	ctx->sys.vm = mod->vm;
	ctx->sys.mod = mod;

	ctx->on_start_msg = msg_pipe_entrypoint(
			&ctx->sys, mod->vm->default_types.unit);

	msg_monad_register(mod);

	stg_mod_register_native_cons(mod,
			mod_atoms(mod, "Msg"), ctx->msg_type_cons);

	{
		/*
		struct object res = {0};
		res.type = msg_register_type(
				mod, mod->vm->default_types.unit);
		res.data = &ctx->on_start_msg;

		stg_mod_register_native_object(mod,
				mod_atoms(mod, "onStart"), res);
		*/
	}

	return 0;
}

int
mod_message_post_init(struct stg_module *mod)
{
	struct msg_context *ctx = mod->data;

	msg_system_compile(&ctx->sys);

	return 0;
}

int
mod_message_start(struct stg_module *mod)
{
	struct msg_context *ctx = mod->data;

	struct object obj = {0};
	obj.type = mod->vm->default_types.unit;

	msg_post(&ctx->sys, ctx->on_start_msg, obj);

	return 0;
}

void
mod_message_destroy(struct stg_module *mod)
{
}

int
mod_message_load(struct stg_native_module *mod)
{
	mod->hook_register    = mod_message_register;
	mod->hook_post_init   = mod_message_post_init;
	mod->hook_destroy     = mod_message_destroy;
	mod->hook_start       = mod_message_start;

	msg_monad_register_native(mod);

	return 0;
}

STAGE_MODULE(message, mod_message_load);
