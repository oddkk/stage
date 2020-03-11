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

int
ast_module_finalize(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *root)
{
	int err;
	err = ast_node_discover_potential_closures(
			ctx, NULL, true, root);

	type_id type;
	type = ast_dt_finalize_composite(ctx, mod,
			root, NULL, 0);

	if (type == TYPE_UNSET) {
		return -1;
	}

	return stg_instantiate_static_object(
			ctx, mod, type, &mod->instance);
}
