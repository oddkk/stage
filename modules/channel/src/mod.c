#include "mod.h"
#include "channel.h"
#include <module.h>
#include <native.h>
#include <utils.h>
#include <base/mod.h>
#include <stdlib.h>

static struct type_base channel_type_base;

static void
channel_type_unpack(struct vm *vm, void *data,
		void *out, void *obj, int param_id)
{
	assert(param_id == 0);

	struct type *type;
	type = vm_get_type(
			vm, *(type_id *)obj);
	assert(type->base == &channel_type_base);

	struct cnl_channel_type_info *info =
		(struct cnl_channel_type_info *)type->data;

	memcpy(out, &info->type, sizeof(type_id));
}

static void
channel_type_pack(struct vm *vm, void *data,
		void *out, void **params, size_t num_params)
{
	struct stg_module *mod = data;

	assert(num_params == 1);

	type_id cnl_type = *(type_id *)params[0];
	type_id res;
	res = cnl_register_channel_type(mod, cnl_type);

	memcpy(out, &res, sizeof(type_id));
}

static type_id
channel_type_pack_type(struct vm *vm,
		void *data, void **params, size_t num_params)
{
	assert(num_params == 1);
	return vm->default_types.type;
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
	struct object_cons *channel_type_cons;
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
		struct object_cons *cnl_type_def;
		cnl_type_def = calloc(1, sizeof(struct object_cons));

		cnl_type_def->num_params = 1;
		cnl_type_def->params = calloc(1, sizeof(struct object_cons_param));

		cnl_type_def->params[0].name = vm_atoms(ctx->vm, "T");
		cnl_type_def->params[0].type = ctx->types.type;

		cnl_type_def->pack      = channel_type_pack;
		cnl_type_def->pack_type = channel_type_pack_type;
		cnl_type_def->unpack    = channel_type_unpack;

		cnl_type_def->data = mod;

		cctx->channel_type_cons = cnl_type_def;

		struct ast_module *ast_mod = &mod->mod;

		struct object res = {0};
		res.type = ctx->types.cons;
		res.data = &cctx->channel_type_cons;
		res = register_object(ctx->vm, ast_mod->env.store, res);

		struct atom *cons_name = vm_atoms(ctx->vm, "Channel");

		struct ast_node *expr;
		expr = ast_init_node_lit(
				ctx, AST_NODE_NEW, STG_NO_LOC, res);

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
