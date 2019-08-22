#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "modules/base/mod.h"

struct ast_context
ast_init_context(struct stg_error_context *err, struct atom_table *atom_table, struct vm *vm, type_id type, type_id integer)
{
	struct ast_context ctx;

	ctx.err = err;

	ctx.atoms.type                 = atom_create(atom_table, STR("Type"));

	ctx.atoms.func_cons_arg_ret    = atom_create(atom_table, STR("ret"));
	ctx.atoms.func_cons_arg_params = atom_create(atom_table, STR("params"));

	ctx.atoms.array_cons_arg_type  = atom_create(atom_table, STR("T"));
	ctx.atoms.array_cons_arg_count = atom_create(atom_table, STR("N"));

	ctx.types.type = type;
	ctx.types.integer = integer;

	ctx.vm = vm;

	{
		struct ast_object_def *array_type_def =
			ast_object_def_register(&vm->modules[0]->store);

		struct ast_object_def_param array_type_params[] = {
			{ctx.atoms.array_cons_arg_type, AST_SLOT_TYPE},
			{ctx.atoms.array_cons_arg_count,
				ast_bind_slot_const_type(
						&ctx, &array_type_def->env, AST_BIND_NEW,
						NULL, integer)},
		};

		ast_object_def_finalize(array_type_def,
				array_type_params, ARRAY_LENGTH(array_type_params),
				AST_SLOT_TYPE);

		ctx.cons.array = array_type_def;
	}

	{
		struct ast_object_def *func_type_def =
			ast_object_def_register(&vm->modules[0]->store);

		ast_slot_id func_params_T = ast_bind_slot_templ(
				&ctx, &func_type_def->env, AST_BIND_NEW,
				ctx.atoms.array_cons_arg_type, AST_SLOT_TYPE);

		ast_slot_id func_params_N = ast_bind_slot_templ(
				&ctx, &func_type_def->env, AST_BIND_NEW, ctx.atoms.array_cons_arg_count,
				ast_bind_slot_const_type(&ctx, &func_type_def->env, AST_BIND_NEW,
						NULL, integer));

		struct ast_object_arg func_params_args[] = {
			{ctx.atoms.array_cons_arg_type,  func_params_T},
			{ctx.atoms.array_cons_arg_count, func_params_N},
		};

		ast_slot_id func_params_type = ast_bind_slot_cons(
				&ctx, &func_type_def->env, AST_BIND_NEW,
				NULL, ctx.cons.array,
				func_params_args, ARRAY_LENGTH(func_params_args));

		struct ast_object_def_param func_type_params[] = {
			{ctx.atoms.func_cons_arg_ret,    AST_SLOT_TYPE},
			{ctx.atoms.func_cons_arg_params, func_params_type},
		};

		ast_object_def_finalize(func_type_def,
				func_type_params, ARRAY_LENGTH(func_type_params),
				AST_SLOT_TYPE);

		ctx.cons.func = func_type_def;
	}

	return ctx;
}

struct ast_object_def *
ast_object_def_register(struct objstore *store)
{
	struct ast_object_def *obj;

	obj = calloc(1, sizeof(struct ast_object_def));
	obj->env.store = store;

	return obj;
}

void
ast_object_def_finalize(struct ast_object_def *obj,
		struct ast_object_def_param *params, size_t num_params,
		ast_slot_id ret_type)
{
	assert(obj != NULL);

	obj->num_params = num_params;
	obj->params = calloc(sizeof(struct ast_object_def_param), num_params);
	memcpy(obj->params, params, sizeof(struct ast_object_def_param) * num_params);

	obj->ret_type = ret_type;
}
