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
ast_namespace_add_import(struct ast_context *ctx, struct ast_module *mod,
		struct ast_node *ns, struct atom *name)
{
	ast_module_add_dependency(ctx, mod, ns, name);
}


void
ast_namespace_use(struct ast_context *ctx,
		struct ast_module *mod, struct ast_node *ns,
		ast_slot_id object)
{
	panic("TODO: Implement use\n");
	/*
	struct ast_env_slot slot = ast_env_slot(ctx, &mod->env, object);

	assert(slot.kind == AST_SLOT_CONS);

	dlist_append(ns->used_objects, ns->num_used_objects, &object);
	*/
}

void
ast_module_add_dependency(struct ast_context *ctx,
		struct ast_module *mod, struct ast_node *container, struct atom *name)
{
	for (size_t i = 0; i < mod->num_dependencies; i++) {
		if (name == mod->dependencies[i].name) {
			panic("Module %.*s imported multiple times.",
					ALIT(name));
			return;
		}
	}

	struct ast_module_dependency new_dep = {0};

	new_dep.name = name;
	new_dep.container = container;

	size_t dep_id;
	dep_id = dlist_append(
			mod->dependencies,
			mod->num_dependencies,
			&new_dep);
}

int
ast_module_resolve_dependencies(struct ast_context *ctx,
		struct ast_module *mod)
{
	for (size_t i = 0; i < mod->num_dependencies; i++) {
		struct ast_module_dependency *dep;
		dep = &mod->dependencies[i];

		struct object type_obj = {0};
		type_obj.type = ctx->types.type;
		type_obj.data = &dep->mod->type;

		type_obj = register_object(
				ctx->vm, mod->env.store, type_obj);

		struct ast_node *type;
		type = ast_init_node_lit(
				ctx, AST_NODE_NEW,
				STG_NO_LOC, type_obj);

		if (dep->mod->type == TYPE_UNSET) {
			return -1;
		}

		int err;
		err = ast_node_composite_add_member(
				ctx, dep->container, dep->name, type,
				AST_NO_TYPE_GIVING_BIND);
		assert(!err);
	}

	return 0;
}

int
ast_module_finalize(struct ast_context *ctx, struct ast_module *mod)
{
	int err;
	err = ast_node_discover_potential_closures(
			ctx, NULL, true, mod->root);

	type_id type;
	type = ast_dt_finalize_composite(ctx, mod,
			&mod->env, mod->root, NULL, 0);

	mod->type = type;

	/*
	printf("Final type for %.*s: ", LIT(mod->stg_mod->info.name));
	print_type_repr(ctx->vm, vm_get_type(ctx->vm, type));
	printf("\n");
	*/

	return 0;
}
