#include "mod.h"
#include "../module.h"
#include "../utils.h"
#include "../ast.h"
#include <stdlib.h>

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

static struct type_base array_type_base = {
	.name = STR("array"),
	// todo: type and object repr
};

type_id
stg_register_array_type(struct stg_module *mod, type_id member_type, size_t length)
{
	struct stg_array_type *data;

	data = calloc(1, sizeof(struct stg_array_type));

	data->member_type = member_type;
	data->length = length;

	struct type *member_type_inst = vm_get_type(mod->vm, member_type);

	struct type type = {0};
	type.base = &array_type_base;
	type.data = data;
	type.size = member_type_inst->size * data->length;

	return stg_register_type(mod, type);
}
