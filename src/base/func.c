#include "mod.h"
#include "../module.h"
#include "../utils.h"
#include "../ast.h"

void
base_bootstrap_register_func(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_object_def *func_type_def =
		ast_object_def_register(&mod->store);

	ast_slot_id func_params_type = ast_bind_slot_cons(
			ctx, &func_type_def->env, AST_BIND_NEW,
			NULL, ctx->cons.array);

	ast_slot_id func_params_T =
		ast_unpack_arg_named(ctx, &func_type_def->env,
				func_params_type, ctx->atoms.array_cons_arg_type);
	func_params_T = ast_bind_slot_templ(
			ctx, &func_type_def->env, func_params_T,
			ctx->atoms.array_cons_arg_type, AST_SLOT_TYPE);

	ast_slot_id func_params_N =
		ast_unpack_arg_named(ctx, &func_type_def->env,
				func_params_type, ctx->atoms.array_cons_arg_count);
	func_params_N = ast_bind_slot_templ(
			ctx, &func_type_def->env, func_params_N, ctx->atoms.array_cons_arg_count,
			ast_bind_slot_const_type(ctx, &func_type_def->env,
				ast_env_slot(ctx, &func_type_def->env, func_params_N).type,
				NULL, ctx->types.integer));

	struct ast_object_def_param func_type_params[] = {
		{0, ctx->atoms.func_cons_arg_ret,    AST_SLOT_TYPE},
		{1, ctx->atoms.func_cons_arg_params, func_params_type},
	};

	ast_object_def_finalize(func_type_def,
			func_type_params, ARRAY_LENGTH(func_type_params),
			AST_SLOT_TYPE);

	mod->vm->default_cons.func = func_type_def;
	ctx->cons.func = func_type_def;
}
