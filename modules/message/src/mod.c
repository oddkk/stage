#include "mod.h"
#include "message.h"
#include <module.h>
#include <native.h>
#include <utils.h>
#include <base/mod.h>
#include <stdlib.h>
#include <ffi.h>

static struct type_base msg_type_base;

static void
msg_type_unpack(struct vm *vm, void *data,
		void *out, void *obj, int param_id)
{
	assert(param_id == 0);

	struct type *type;
	type = vm_get_type(
			vm, *(type_id *)obj);
	assert(type->base == &msg_type_base);

	struct msg_type_info *info =
		(struct msg_type_info *)type->data;

	memcpy(out, &info->type, sizeof(type_id));
}

static void
msg_type_pack(struct vm *vm, void *data,
		void *out, void **params, size_t num_params)
{
	struct stg_module *mod = data;

	assert(num_params == 1);

	type_id msg_type = *(type_id *)params[0];
	type_id res;
	res = msg_register_msg_type(mod, msg_type);

	memcpy(out, &res, sizeof(type_id));
}

static type_id
msg_type_pack_type(struct vm *vm,
		void *data, void **params, size_t num_params)
{
	assert(num_params == 1);
	return vm->default_types.type;
}

static bool
msg_type_equals(struct vm *vm, struct type *lhs, struct type *rhs)
{
	struct msg_type_info *lhs_info;
	struct msg_type_info *rhs_info;

	lhs_info = (struct msg_type_info *)lhs->data;
	rhs_info = (struct msg_type_info *)rhs->data;

	return type_equals(vm, lhs_info->type, rhs_info->type);
}

static struct string
msg_type_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct msg_type_info *info;
	info = (struct msg_type_info *)type->data;

	struct string res = arena_string_init(mem);
	arena_string_append(mem, &res, STR("Msg("));

	struct type *item_type;
	item_type = vm_get_type(vm, info->type);
	arena_string_append_type_repr(&res, vm, mem, item_type);

	arena_string_append(mem, &res, STR(")"));

	return res;
}

static struct type_base msg_type_base = {
	.name = STR("Msg"),
	.equals = msg_type_equals,
	.repr = msg_type_repr,
};

struct msg_context {
	struct msg_system sys;
	struct object_cons *msg_type_cons;

	msg_node_id on_start_msg;
	func_id endpoint_cons_func;
};

static ffi_type *ffi_type_msg_functor_members[] = {
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_uint64,
	NULL,
};

static ffi_type ffi_type_msg_functor = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = ffi_type_msg_functor_members,
};

type_id
msg_register_msg_type(struct stg_module *mod, type_id msg_type)
{
	struct msg_type_info *info;

	info = calloc(1, sizeof(struct msg_type_info));

	struct stg_module *msg_mod;
	msg_mod = vm_get_module(mod->vm, mod_atoms(mod, "message"));
	assert(msg_mod);

	struct msg_context *ctx;
	ctx = msg_mod->data;

	info->type = msg_type;

	struct type type = {0};
	type.base = &msg_type_base;
	type.data = info;
	type.size = sizeof(msg_node_id);
	type.type_def = ctx->msg_type_cons;
	type.ffi_type = &ffi_type_msg_functor;

	return stg_register_type(mod, type);
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
mod_message_pre_compile(struct ast_context *ast_ctx, struct stg_module *mod)
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
			msg_register_msg_type(mod,
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

		msg_type_def->pack      = msg_type_pack;
		msg_type_def->pack_type = msg_type_pack_type;
		msg_type_def->unpack    = msg_type_unpack;

		msg_type_def->data      = mod;

		ctx->msg_type_cons = msg_type_def;

		struct ast_module *ast_mod = &mod->mod;

		struct object res = {0};
		res.type = ast_ctx->types.cons;
		res.data = &ctx->msg_type_cons;
		res = register_object(ast_ctx->vm, ast_mod->env.store, res);

		struct atom *cons_name = vm_atoms(ast_ctx->vm, "Msg");

		struct ast_node *expr;
		expr = ast_init_node_lit(
				ast_ctx, AST_NODE_NEW, STG_NO_LOC, res);

		ast_namespace_add_decl(ast_ctx, ast_mod, ast_mod->root,
				cons_name, expr);
	}

	{
		struct object res = {0};
		res.type = msg_register_msg_type(
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

struct msg_functor_data
msg_copy_functor(struct stg_exec *heap, struct msg_functor_data functor)
{
	struct msg_functor_data out;
	
	out = functor;
	out.data = stg_alloc(heap, 1, out.data_size);
	memcpy(out.data, functor.data, out.data_size);

	if (out.copy) {
		out.copy(heap, out.data);
	}

	return out;
}

msg_node_id
msg_call_functor(struct vm *vm, struct msg_system *sys,
		struct msg_functor_data msg)
{
	assert(msg.call);
	return msg.call(vm, sys, msg.data);
}

struct msg_functor_map_data {
	struct msg_functor_data msg;
	struct stg_func_object func;
};

static msg_node_id
msg_functor_map_call(
		struct vm *vm, struct msg_system *sys,
		void *data)
{
	struct msg_functor_map_data *map_data;
	map_data = data;

	msg_node_id in;
	in = msg_call_functor(vm, sys, map_data->msg);

	msg_node_id node;
	node = msg_pipe_map(sys, map_data->func);

	msg_pipe_connect(sys, in, node);

	return node;
}

static void
msg_functor_map_copy(
	struct stg_exec *heap, void *data)
{
	struct msg_functor_map_data *map_data;
	map_data = data;

	map_data->msg =
		msg_copy_functor(heap, map_data->msg);
}

static struct msg_functor_data
msg_functor_map(struct stg_exec *heap,
		struct stg_module *mod,
		struct msg_functor_data msg,
		struct stg_func_object func,
		type_id type_in, type_id type_out)
{
	struct msg_functor_data out_msg = {0};

	out_msg.call = msg_functor_map_call;
	out_msg.copy = msg_functor_map_copy;
	out_msg.data_size = sizeof(struct msg_functor_map_data);
	out_msg.data = stg_alloc(heap, 1, out_msg.data_size);

	struct msg_functor_map_data *map_data;
	map_data = out_msg.data;
	map_data->msg = msg;
	map_data->func = func;

	return out_msg;
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
	mod->hook_pre_compile = mod_message_pre_compile;
	mod->hook_post_init   = mod_message_post_init;
	mod->hook_destroy     = mod_message_destroy;
	mod->hook_start       = mod_message_start;

	stg_native_register_funcs(mod, msg_functor_map,
			STG_NATIVE_FUNC_HEAP | STG_NATIVE_FUNC_MODULE_CLOSURE);

	return 0;
}

STAGE_MODULE(message, mod_message_load);
