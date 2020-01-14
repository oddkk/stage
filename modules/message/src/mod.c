#include "mod.h"
#include "message.h"
#include <module.h>
#include <native.h>
#include <utils.h>
#include <base/mod.h>
#include <stdlib.h>
#include <ffi.h>

static struct type_base message_type_base;

static void
message_type_unpack(struct vm *vm, void *data,
		void *out, void *obj, int param_id)
{
	assert(param_id == 0);

	struct type *type;
	type = vm_get_type(
			vm, *(type_id *)obj);
	assert(type->base == &message_type_base);

	struct msg_message_type_info *info =
		(struct msg_message_type_info *)type->data;

	memcpy(out, &info->type, sizeof(type_id));
}

static void
message_type_pack(struct vm *vm, void *data,
		void *out, void **params, size_t num_params)
{
	struct stg_module *mod = data;

	assert(num_params == 1);

	type_id msg_type = *(type_id *)params[0];
	type_id res;
	res = msg_register_message_type(mod, msg_type);

	memcpy(out, &res, sizeof(type_id));
}

static type_id
message_type_pack_type(struct vm *vm,
		void *data, void **params, size_t num_params)
{
	assert(num_params == 1);
	return vm->default_types.type;
}

static bool
message_type_equals(struct vm *vm, struct type *lhs, struct type *rhs)
{
	struct msg_message_type_info *lhs_info;
	struct msg_message_type_info *rhs_info;

	lhs_info = (struct msg_message_type_info *)lhs->data;
	rhs_info = (struct msg_message_type_info *)rhs->data;

	return type_equals(vm, lhs_info->type, rhs_info->type);
}

static struct string
message_type_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct msg_message_type_info *info;
	info = (struct msg_message_type_info *)type->data;

	struct string res = arena_string_init(mem);
	arena_string_append(mem, &res, STR("Message("));

	struct type *item_type;
	item_type = vm_get_type(vm, info->type);
	arena_string_append_type_repr(&res, vm, mem, item_type);

	arena_string_append(mem, &res, STR(")"));

	return res;
}

static struct type_base message_type_base = {
	.name = STR("Message"),
	.equals = message_type_equals,
	.repr = message_type_repr,
};

struct msg_context {
	struct msg_system sys;
	struct object_cons *message_type_cons;

	msg_node_id on_start_msg;
	func_id endpoint_cons_func;
};

type_id
msg_register_message_type(struct stg_module *mod, type_id msg_type)
{
	struct msg_message_type_info *info;

	info = calloc(1, sizeof(struct msg_message_type_info));

	struct stg_module *msg_mod;
	msg_mod = vm_get_module(mod->vm, STR("message"));
	assert(msg_mod);

	struct msg_context *ctx;
	ctx = msg_mod->data;

	info->type = msg_type;

	struct type type = {0};
	type.base = &message_type_base;
	type.data = info;
	type.size = sizeof(msg_node_id);
	type.type_def = ctx->message_type_cons;
	type.ffi_type = &ffi_type_uint32;

	return stg_register_type(mod, type);
}

static void
msg_print_int_callback(int64_t val)
{
	printf(" => %zi\n", val);
}

static void
msg_print_string_callback(struct string val)
{
	printf(" => %.*s\n", LIT(val));
}

struct msg_endpoint_cons_info {
	struct stg_module *mod;
	struct stg_func_object func;
};

static void
msg_endpoint_cons(struct msg_endpoint_cons_info *info, msg_node_id in)
{
	struct msg_context *ctx = info->mod->data;

	msg_node_id res;
	res = msg_pipe_endpoint(&ctx->sys,
			info->func.func, info->func.closure);

	msg_pipe_connect(&ctx->sys, in, res);
}

int
mod_message_init(struct ast_context *ast_ctx, struct stg_module *mod)
{
	struct msg_context *ctx;
	ctx = calloc(1, sizeof(struct msg_context));
	mod->data = ctx;

	ctx->sys.vm = mod->vm;
	ctx->sys.mod = mod;

	ctx->on_start_msg = msg_pipe_entrypoint(
			&ctx->sys, mod->vm->default_types.unit);

	{
		type_id endpoint_cons_params[1];
		endpoint_cons_params[0] =
			msg_register_message_type(mod,
					mod->vm->default_types.integer);

		type_id endpoint_cons_return;
		endpoint_cons_return =
			mod->vm->default_types.unit;

		struct func func = {0};
		func.kind = FUNC_NATIVE;
		func.flags = FUNC_IMPURE | FUNC_CLOSURE;
		func.type = stg_register_func_type(
				mod, endpoint_cons_return,
				endpoint_cons_params, 1);
		func.native = (void *)msg_endpoint_cons;

		ctx->endpoint_cons_func =
			stg_register_func(mod, func);
	}

	{
		struct object_cons *msg_type_def;
		msg_type_def = calloc(1, sizeof(struct object_cons));
		msg_type_def->num_params = 1;
		msg_type_def->params = calloc(
				msg_type_def->num_params,
				sizeof(struct object_cons_param));

		msg_type_def->params[0].name = vm_atoms(ast_ctx->vm, "T");
		msg_type_def->params[0].type = ast_ctx->types.type;

		msg_type_def->pack      = message_type_pack;
		msg_type_def->pack_type = message_type_pack_type;
		msg_type_def->unpack    = message_type_unpack;

		msg_type_def->data      = mod;

		ctx->message_type_cons = msg_type_def;

		struct ast_module *ast_mod = &mod->mod;

		struct object res = {0};
		res.type = ast_ctx->types.cons;
		res.data = &ctx->message_type_cons;
		res = register_object(ast_ctx->vm, ast_mod->env.store, res);

		struct atom *cons_name = vm_atoms(ast_ctx->vm, "Message");

		struct ast_node *expr;
		expr = ast_init_node_lit(
				ast_ctx, AST_NODE_NEW, STG_NO_LOC, res);

		ast_namespace_add_decl(ast_ctx, ast_mod, ast_mod->root,
				cons_name, expr);
	}

	{
		struct object res = {0};
		res.type = msg_register_message_type(
				mod, ast_ctx->types.unit);
		res.data = &ctx->on_start_msg;

		res = register_object(ast_ctx->vm, mod->mod.env.store, res);

		struct ast_node *expr;
		expr = ast_init_node_lit(
				ast_ctx, AST_NODE_NEW, STG_NO_LOC, res);

		struct atom *start_msg_name = vm_atoms(ast_ctx->vm, "onStart");

		ast_namespace_add_decl(ast_ctx, &mod->mod, mod->mod.root,
				start_msg_name, expr);
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
mod_message_free(struct stg_module *mod)
{
}

/*
static void
msg_print_int(struct stg_module *mod, msg_node_id in)
{
	struct msg_context *ctx = mod->data;

	msg_node_id end;
	end = msg_pipe_endpoint(&ctx->sys, msg_print_int_callback, mod);

	msg_pipe_connect(&ctx->sys, in, end);
}
*/

static msg_node_id
msg_map(struct stg_module *mod, msg_node_id in, struct stg_func_object map_func)
{
	struct msg_context *ctx = mod->data;

	msg_node_id res;
	res = msg_pipe_map(&ctx->sys, map_func);

	msg_pipe_connect(&ctx->sys, in, res);

	return res;
}

static msg_node_id
msg_filter(struct stg_module *mod, msg_node_id in, struct stg_func_object filter_func)
{
	struct msg_context *ctx = mod->data;

	msg_node_id res;
	res = msg_pipe_filter(&ctx->sys, filter_func);

	msg_pipe_connect(&ctx->sys, in, res);

	return res;
}

static struct stg_func_object
msg_endpoint(struct stg_module *mod, struct stg_func_object map_func)
{
	struct msg_context *ctx = mod->data;

	struct msg_endpoint_cons_info *info;
	info = calloc(1, sizeof(struct msg_endpoint_cons_info));
	info->mod = mod;
	info->func = map_func;

	struct stg_func_object cons_func = {0};
	cons_func.func = ctx->endpoint_cons_func;
	cons_func.closure = info;

	return cons_func;
}


int
mod_message_load(struct stg_native_module *mod)
{
	mod->hook_init      = mod_message_init;
	mod->hook_post_init = mod_message_post_init;
	mod->hook_free      = mod_message_free;
	mod->hook_start     = mod_message_start;

	stg_native_register_funcs(mod, msg_print_int_callback,
			STG_NATIVE_FUNC_IMPURE);
	stg_native_register_funcs(mod, msg_print_string_callback,
			STG_NATIVE_FUNC_IMPURE);
	stg_native_register_funcs(mod, msg_map,
			STG_NATIVE_FUNC_IMPURE | STG_NATIVE_FUNC_MODULE_CLOSURE);
	stg_native_register_funcs(mod, msg_filter,
			STG_NATIVE_FUNC_IMPURE | STG_NATIVE_FUNC_MODULE_CLOSURE);
	stg_native_register_funcs(mod, msg_endpoint,
			STG_NATIVE_FUNC_IMPURE | STG_NATIVE_FUNC_MODULE_CLOSURE);

	return 0;
}

STAGE_MODULE(message, mod_message_load);
