#include "mod.h"
#include "channel.h"
#include <module.h>
#include <native.h>
#include <utils.h>
#include <base/mod.h>
#include <stdlib.h>

static struct type_base channel_type_base;

#define CHANNEL_TYPE_PARAM_TYPE 0

static struct object
channel_type_unpack(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, int param_id, struct object obj)
{
	assert_type_equals(ctx->vm, obj.type, ctx->types.type);
	struct type *type = vm_get_type(ctx->vm, *(type_id *)obj.data);
	assert(type->base == &channel_type_base);

	struct cnl_channel_type_info *info =
		(struct cnl_channel_type_info *)type->data;

	if (param_id == CHANNEL_TYPE_PARAM_TYPE) {
		struct object res = {0};

		res.type = ctx->types.type;
		res.data = &info->type;

		return res;
	}

	panic("Invalid param %i requested from Channel.", param_id);
	return OBJ_NONE;
}

static struct object
channel_type_pack(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_object_def *def, ast_slot_id obj_slot)
{
	int err;

	ast_slot_id cnl_type_slot;
	cnl_type_slot =
		ast_unpack_arg_named(ctx, &mod->env, obj_slot,
				AST_BIND_NEW, vm_atoms(ctx->vm, "T"));

	struct object cnl_type_obj;
	err = ast_slot_pack(ctx, mod, env,
			cnl_type_slot, &cnl_type_obj);
	if (err) {
		printf("Failed to pack array member type slot.\n");
		return OBJ_NONE;
	}

	assert_type_equals(ctx->vm, cnl_type_obj.type, ctx->types.type);

	type_id cnl_type = *(type_id *)cnl_type_obj.data;

	type_id res;
	res = cnl_register_channel_type(mod->stg_mod, cnl_type);

	struct object type_obj = {0};
	type_obj.type = ctx->types.type;
	type_obj.data = &res;

	return register_object(ctx->vm, env->store, type_obj);
}

static bool
channel_type_equals(struct vm *vm, struct type *lhs, struct type *rhs)
{
	struct cnl_channel_type_info *lhs_info;
	struct cnl_channel_type_info *rhs_info;

	lhs_info = (struct cnl_channel_type_info *)lhs->data;
	rhs_info = (struct cnl_channel_type_info *)rhs->data;

	return type_equals(vm, lhs_info->type, rhs_info->type);
}

static struct string
channel_type_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct cnl_channel_type_info *info;
	info = (struct cnl_channel_type_info *)type->data;

	struct string res = arena_string_init(mem);
	arena_string_append(mem, &res, STR("Channel("));

	struct type *item_type;
	item_type = vm_get_type(vm, info->type);
	arena_string_append_type_repr(&res, vm, mem, item_type);

	arena_string_append(mem, &res, STR(")"));

	return res;
}

static struct type_base channel_type_base = {
	.name = STR("Channel"),
	.equals = channel_type_equals,
	.repr = channel_type_repr,
};

struct cnl_context {
	struct channel_system sys;
	struct ast_object_def *channel_type_cons;
};

type_id
cnl_register_channel_type(struct stg_module *mod, type_id cnl_type)
{
	struct cnl_context *ctx = mod->data;

	struct cnl_channel_type_info *info;

	info = calloc(1, sizeof(struct cnl_channel_type_info));

	info->type = cnl_type;

	struct type type = {0};
	type.base = &channel_type_base;
	type.data = info;
	type.size = sizeof(channel_id);
	type.type_def = ctx->channel_type_cons;

	return stg_register_type(mod, type);
}

int
mod_channel_init(struct ast_context *ctx, struct stg_module *mod)
{
	struct cnl_context *cctx;
	cctx = calloc(1, sizeof(struct cnl_context));
	mod->data = cctx;

	{
		struct ast_object_def *cnl_type_def =
			ast_object_def_register(&mod->store);

		ast_slot_id arg_type = ast_bind_slot_templ(
				ctx, &cnl_type_def->env, AST_BIND_NEW, AST_SLOT_TYPE);


		struct ast_object_def_param cnl_type_params[] = {
			{CHANNEL_TYPE_PARAM_TYPE, vm_atoms(ctx->vm, "T"), ctx->types.type, arg_type},
		};

		ast_object_def_finalize(cnl_type_def,
				cnl_type_params, ARRAY_LENGTH(cnl_type_params),
				AST_SLOT_TYPE);

		cnl_type_def->pack   = channel_type_pack;
		cnl_type_def->unpack = channel_type_unpack;

		cctx->channel_type_cons = cnl_type_def;

		struct ast_module *ast_mod = &mod->mod;

		struct object res = {0};
		res.type = ctx->types.cons;
		res.data = &cctx->channel_type_cons;
		res = register_object(ctx->vm, ast_mod->env.store, res);

		struct atom *cons_name = vm_atoms(ctx->vm, "Channel");

		struct ast_node *expr;
		expr = ast_init_node_lit(ctx, &ast_mod->env,
				AST_NODE_NEW, STG_NO_LOC, res);

		ast_namespace_add_decl(ctx, ast_mod, ast_mod->root,
				cons_name, expr);
	}

	channel_system_init(&cctx->sys, 1024, mod->vm);

	return 0;
}

int
mod_channel_start(struct stg_module *mod)
{
	struct cnl_context *ctx = mod->data;
	channel_system_start(&ctx->sys);
	return 0;
}

void
mod_channel_free(struct stg_module *mod)
{
	struct cnl_context *ctx = mod->data;

	channel_system_destroy(&ctx->sys);

	free(mod->data);
}

int
mod_channel_load(struct stg_native_module *mod)
{
	mod->hook_init = mod_channel_init;
	mod->hook_free = mod_channel_free;
	mod->hook_start = mod_channel_start;

	return 0;
}

STAGE_MODULE(channel, mod_channel_load);
