#include "mod.h"
#include "message.h"
#include <module.h>
#include <native.h>
#include <utils.h>
#include <base/mod.h>
#include <stdlib.h>
#include <ffi.h>

static struct type_base message_type_base;

#define MESSAGE_TYPE_PARAM_TYPE 0

static struct object
message_type_unpack(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, int param_id, struct object obj)
{
	assert_type_equals(ctx->vm, obj.type, ctx->types.type);
	struct type *type = vm_get_type(ctx->vm, *(type_id *)obj.data);
	assert(type->base == &message_type_base);

	struct msg_message_type_info *info =
		(struct msg_message_type_info *)type->data;

	if (param_id == MESSAGE_TYPE_PARAM_TYPE) {
		struct object res = {0};

		res.type = ctx->types.type;
		res.data = &info->type;

		return res;
	}

	panic("Invalid param %i requested from Channel.", param_id);
	return OBJ_NONE;
}

static struct object
message_type_pack(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_object_def *def, ast_slot_id obj_slot)
{
	int err;

	ast_slot_id msg_type_slot;
	msg_type_slot =
		ast_unpack_arg_named(ctx, &mod->env, obj_slot,
				AST_BIND_NEW, vm_atoms(ctx->vm, "T"));

	struct object msg_type_obj;
	err = ast_slot_pack(ctx, mod, env,
			msg_type_slot, &msg_type_obj);
	if (err) {
		printf("Failed to pack array member type slot.\n");
		return OBJ_NONE;
	}

	assert_type_equals(ctx->vm, msg_type_obj.type, ctx->types.type);

	type_id msg_type = *(type_id *)msg_type_obj.data;

	type_id res;
	res = msg_register_message_type(mod->stg_mod, msg_type);

	struct object type_obj = {0};
	type_obj.type = ctx->types.type;
	type_obj.data = &res;

	return register_object(ctx->vm, env->store, type_obj);
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

	struct ast_object_def *message_type_cons;

	msg_node_id on_start_msg;
};

type_id
msg_register_message_type(struct stg_module *mod, type_id msg_type)
{
	struct msg_message_type_info *info;

	info = calloc(1, sizeof(struct msg_message_type_info));

	info->type = msg_type;

	struct type type = {0};
	type.base = &message_type_base;
	type.data = info;
	type.size = sizeof(msg_node_id);
	type.type_def = mod->vm->default_cons.array;
	type.ffi_type = &ffi_type_uint32;

	return stg_register_type(mod, type);
}

int
mod_message_init(struct ast_context *ast_ctx, struct stg_module *mod)
{
	struct msg_context *ctx;
	ctx = calloc(1, sizeof(struct msg_context));
	mod->data = ctx;

	ctx->sys.vm = mod->vm;

	ctx->on_start_msg = msg_pipe_entrypoint(
			&ctx->sys, mod->vm->default_types.unit);

	{
		struct ast_object_def *msg_type_def;
		msg_type_def = ast_object_def_register(&mod->store);

		ast_slot_id arg_type = ast_bind_slot_templ(
				ast_ctx, &msg_type_def->env, AST_BIND_NEW,
				vm_atoms(ast_ctx->vm, "T"), AST_SLOT_TYPE);


		struct ast_object_def_param msg_type_params[] = {
			{MESSAGE_TYPE_PARAM_TYPE, vm_atoms(ast_ctx->vm, "T"), ast_ctx->types.type, arg_type},
		};

		ast_object_def_finalize(msg_type_def,
				msg_type_params, ARRAY_LENGTH(msg_type_params),
				AST_SLOT_TYPE);

		msg_type_def->pack   = message_type_pack;
		msg_type_def->unpack = message_type_unpack;

		ctx->message_type_cons = msg_type_def;

		struct ast_module *ast_mod = &mod->mod;

		struct object res = {0};
		res.type = ast_ctx->types.cons;
		res.data = &ctx->message_type_cons;
		res = register_object(ast_ctx->vm, ast_mod->env.store, res);

		struct atom *cons_name = vm_atoms(ast_ctx->vm, "Message");

		struct ast_node *expr;
		expr = ast_init_node_lit(ast_ctx, &ast_mod->env,
				AST_NODE_NEW, STG_NO_LOC, res);

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
		expr = ast_init_node_lit(ast_ctx, &mod->mod.env,
				AST_NODE_NEW, STG_NO_LOC, res);

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

	int64_t data = 0;
	struct object obj = {0};
	obj.type = mod->vm->default_types.integer;
	obj.data = &data;

	msg_post(&ctx->sys, ctx->on_start_msg, obj);

	return 0;
}

void
mod_message_free(struct stg_module *mod)
{
}

/*
static void
msg_print_callback(void *data, struct object obj)
{
	struct stg_module *mod = data;

	printf(" => ");
	print_obj_repr(mod->vm, obj);
	printf("\n");
}
*/

static void
msg_print(struct stg_module *mod, msg_node_id in)
{
	// struct msg_context *ctx = mod->data;

	// msg_node_id end;
	// end = msg_pipe_endpoint(&ctx->sys, msg_print_callback, mod);
	printf("register print from %i\n", in);
}

static msg_node_id
msg_map(struct stg_module *mod, msg_node_id in, func_id map_func)
{
	// struct msg_context *ctx = mod->data;

	printf("register map from %i over func %li\n", in, map_func);

	// msg_node_id res;
	// res = msg_pipe_map(&ctx->sys, );

	return 0;
}

int
mod_message_load(struct stg_native_module *mod)
{
	mod->hook_init      = mod_message_init;
	mod->hook_post_init = mod_message_post_init;
	mod->hook_free      = mod_message_free;
	mod->hook_start     = mod_message_start;

	stg_native_register_funcs(mod, msg_print);
	stg_native_register_funcs(mod, msg_map);

	return 0;
}

STAGE_MODULE(message, mod_message_load);
