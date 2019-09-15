#include "mod.h"
#include "../vm.h"
#include "../module.h"
#include "../native.h"
#include <stdlib.h>

static int
stg_base_bootstrap_init(struct ast_context *ctx, struct stg_module *mod) {
	assert(mod->id == 0);
	assert(ctx == NULL);

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		base->name = STR("unset");

		struct type unset = {0};
		unset.name = atom_create(mod->atom_table, STR("unset"));
		unset.base = base;
		unset.size = 0;

		type_id unset_id = register_type(&mod->store, unset);

		assert(unset_id == TYPE_UNSET);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		base->name = STR("none");

		struct type none = {0};
		none.name = atom_create(mod->atom_table, STR("none"));
		none.base = base;
		none.size = 0;

		type_id none_id = store_register_type(&mod->store, none);
		assert(none_id == TYPE_NONE);
	}

	base_bootstrap_register_type(mod);
	base_bootstrap_register_integer(mod);

	struct ast_context tmp_ctx;
	tmp_ctx = ast_init_context(NULL, &mod->vm->atom_table, mod->vm);

	base_bootstrap_register_array(&tmp_ctx, mod);
	base_bootstrap_register_func(&tmp_ctx, mod);

	return 0;
}

static struct stg_module_info base_bootstrap_mod_info = {
	.name    = STR("base_bootstrap"),
	.version = {0, 1},

	.init = stg_base_bootstrap_init,
};

static int
stg_base_init(struct ast_context *ctx, struct stg_module *mod) {
	struct ast_node *int_type_expr;
	int_type_expr = ast_init_node_slot(ctx, &mod->mod.env,
			AST_NODE_NEW, STG_NO_LOC,
			ast_bind_slot_const_type(ctx, &mod->mod.env,
				AST_BIND_NEW, mod_atoms(mod, "int"),
				mod->vm->default_types.integer));
	ast_namespace_add_decl(ctx, &mod->mod,
			&mod->mod.root, mod_atoms(mod, "int"), int_type_expr);

	struct ast_node *type_type_expr;
	type_type_expr = ast_init_node_slot(ctx, &mod->mod.env,
			AST_NODE_NEW, STG_NO_LOC, AST_SLOT_TYPE);
	ast_namespace_add_decl(ctx, &mod->mod,
			&mod->mod.root, mod_atoms(mod, "type"), type_type_expr);

	return 0;
}

void
stg_base_load(struct vm *vm)
{
	// We manually register the base_bootstrap module this early because it is
	// required for the compiler to function. At this point, there exists no
	// ast_context because we are missing the types and constructors that are
	// being created in base_bootstrap.
	vm_register_module(vm, NULL, NULL, &base_bootstrap_mod_info);

	struct stg_native_module *mod;
	mod = vm_add_precompiled_native_module(vm, STR("base"));

	mod->hook_init = stg_base_init;
}
