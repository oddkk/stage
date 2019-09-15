#include "mod.h"
#include "../module.h"
#include "../utils.h"
#include "../ast.h"

void
base_bootstrap_register_array(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_object_def *array_type_def =
		ast_object_def_register(&mod->store);

	struct ast_object_def_param array_type_params[] = {
		{0, ctx->atoms.array_cons_arg_type, AST_SLOT_TYPE},
		{1, ctx->atoms.array_cons_arg_count,
			ast_bind_slot_const_type(
					ctx, &array_type_def->env, AST_BIND_NEW,
					NULL, ctx->types.integer)},
	};

	ast_object_def_finalize(array_type_def,
			array_type_params, ARRAY_LENGTH(array_type_params),
			AST_SLOT_TYPE);

	mod->vm->default_cons.array = array_type_def;
	ctx->cons.array = array_type_def;
}
