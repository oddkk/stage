#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "dlist.h"
#include "module.h"
#include "base/mod.h"

struct ast_context
ast_init_context(struct stg_error_context *err, struct atom_table *atom_table, struct vm *vm)
{
	struct ast_context ctx;

	ctx.err = err;

	ctx.atoms.type                 = atom_create(atom_table, STR("Type"));

	ctx.types.unit = vm->default_types.unit;
	ctx.types.type = vm->default_types.type;
	ctx.types.cons = vm->default_types.cons;
	ctx.types.inst = vm->default_types.inst;
	ctx.types.string = vm->default_types.string;
	ctx.types.integer = vm->default_types.integer;

	ctx.vm = vm;

	return ctx;
}

int
ast_namespace_add_decl(struct ast_context *ctx, struct ast_module *mod,
		struct ast_node *ns, struct atom *name, struct ast_node *expr)
{
	struct ast_node *target;
	target = ast_init_node_lookup(
			ctx, AST_NODE_NEW, STG_NO_LOC, name);

	int bind_id;
	bind_id = ast_node_composite_bind(
			ctx, ns, target, expr, false);

	int err;
	err = ast_node_composite_add_member(
			ctx, ns, name, NULL, bind_id);
	if (err) {
		return err;
	}

	return 0;
}

void
ast_namespace_add_free_expr(struct ast_context *ctx, struct ast_module *mod,
		struct ast_node *ns, struct ast_node *expr)
{
	ast_node_composite_add_free_expr(
			ctx, ns, expr);
}


struct ast_node *
ast_namespace_add_ns(struct ast_context *ctx,
		struct ast_node *ns, struct atom *name)
{
	assert(ns->kind == AST_NODE_COMPOSITE);

	for (size_t i = 0; i < ns->composite.num_members; i++) {
		if (ns->composite.members[i].name == name) {
			return ns->composite.members[i].type;
		}
	}

	struct ast_node *ns_type;
	// TODO: Add a location to make error messages more helpful.
	ns_type = ast_init_node_composite(
			ctx, AST_NODE_NEW, STG_NO_LOC);

	int err;
	err = ast_node_composite_add_member(
			ctx, ns, name, ns_type, AST_NO_TYPE_GIVING_BIND);
	assert(!err);

	return ns_type;
}

void
ast_module_add_dependency(struct ast_context *ctx,
		struct ast_module *mod, struct atom *name)
{
	for (size_t i = 0; i < mod->num_dependencies; i++) {
		if (name == mod->dependencies[i].name) {
			return;
		}
	}

	struct ast_module_dependency new_dep = {0};

	new_dep.name = name;

	size_t dep_id;
	dep_id = dlist_append(
			mod->dependencies,
			mod->num_dependencies,
			&new_dep);
}

int
ast_module_finalize(struct ast_context *ctx, struct ast_module *mod)
{
	int err;
	err = ast_node_discover_potential_closures(
			ctx, NULL, true, mod->root);

	type_id type;
	type = ast_dt_finalize_composite(ctx, mod,
			mod->root, NULL, 0);

	func_id init_func;

	if (type == TYPE_UNSET) {
		return -1;
	}

	{
		struct ast_node *mod_init_func;
		struct ast_node *mod_inst;
		struct ast_node *mod_cons_obj;
		struct ast_node *mod_ret;

		struct type *mod_type;
		mod_type = vm_get_type(ctx->vm, type);

		assert(mod_type->obj_def);

		struct object cons_obj = {0};
		cons_obj.type = ctx->types.type;
		cons_obj.data = &type;

		struct object type_obj = {0};
		type_obj.type = ctx->types.type;
		type_obj.data = &type;

		mod_ret = ast_init_node_lit(ctx,
				AST_NODE_NEW, STG_NO_LOC, type_obj);

		mod_cons_obj = ast_init_node_lit(ctx,
				AST_NODE_NEW, STG_NO_LOC, cons_obj);

		mod_inst = ast_init_node_inst(ctx,
				AST_NODE_NEW, STG_NO_LOC, mod_cons_obj, NULL, 0);

		mod_init_func = ast_init_node_func(ctx,
				AST_NODE_NEW, STG_NO_LOC,
				NULL, NULL, 0,
				mod_ret, mod_inst);

		int err;
		err = ast_node_typecheck(ctx, mod,
				mod_init_func, NULL, 0, TYPE_UNSET);
		if (err) {
			printf("Failed typechecking initialize function for module '%.*s'.\n",
					LIT(mod->stg_mod->info.name));
			return -1;
		}

		struct bc_env *bc_env;
		bc_env = ast_func_gen_bytecode(ctx, mod,
				NULL, NULL, 0, mod_init_func);
		if (!bc_env) {
			printf("Failed codegen for module '%.*s'.\n",
					LIT(mod->stg_mod->info.name));
			return -1;
		}

		struct func func = {0};

		func.type = stg_register_func_type(
				mod->stg_mod, type, NULL, 0);

		func.kind = FUNC_BYTECODE;
		func.bytecode = bc_env;


		init_func = stg_register_func(mod->stg_mod, func);
	}

	{
		struct type *mod_obj_type;
		mod_obj_type = vm_get_type(ctx->vm, type);
		uint8_t mod_obj_buffer[mod_obj_type->size];
		struct object mod_obj = {0};

		mod_obj.data = mod_obj_buffer;
		mod_obj.type = type;

		struct stg_exec exec_ctx;
		exec_ctx = vm_init_exec_context(ctx->vm);
		vm_call_func(ctx->vm, &exec_ctx, init_func, NULL, 0, &mod_obj);

		mod->instance =
			register_object(ctx->vm, mod->env.store, mod_obj);

		vm_release_exec_context(ctx->vm, &exec_ctx);
	}

	return 0;
}
