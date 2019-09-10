#include "mod.h"
#include "../vm.h"
#include "../module.h"
#include "../native.h"
#include <stdlib.h>

static int
stg_base_init(struct stg_module *mod) {
	assert(mod->id == 0);

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("unset"));

		struct type unset = {0};
		unset.name = atom_create(mod->atom_table, STR("unset"));
		unset.base = base;
		unset.size = 0;

		type_id unset_id = register_type(&mod->store, unset);
		// type_base_register_unifier(base, NULL, type_unset_unify);

		assert(unset_id == TYPE_UNSET);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("none"));

		struct type none = {0};
		none.name = atom_create(mod->atom_table, STR("none"));
		none.base = base;
		none.size = 0;

		type_id none_id = register_type(&mod->store, none);
		assert(none_id == TYPE_NONE);
	}

	base_register_type(mod);

	return 0;
}

static struct stg_module_info base_bootstrap_mod_info = {
	.name    = STR("base_bootstrap"),
	.version = {0, 1},

	.init = stg_base_init,
};

void
stg_base_load(struct vm *vm)
{
	// We manually register the base_bootstrap module this early because it is
	// required for the compiler to function.
	vm_register_module(vm, &base_bootstrap_mod_info);

	struct stg_native_module *mod;
	mod = vm_add_precompiled_native_module(vm, STR("base"));
}
