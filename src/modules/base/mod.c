#include "mod.h"
#include <stdlib.h>

void base_register_integer(struct stg_module *mod);
void base_register_type(struct stg_module *mod);
void base_register_bool(struct stg_module *mod);
void base_register_str(struct stg_module *mod);
void base_register_func(struct stg_module *mod);
void base_register_tuple(struct stg_module *mod);
void base_register_array(struct stg_module *mod);
void base_register_enum(struct stg_module *mod);
void base_register_functions(struct stg_module *mod);

static bool
type_unset_unify(struct vm *vm, struct objstore *store,
				 type_id lhs, type_id rhs,
				 type_id *out_type)
{
	if (lhs != TYPE_UNSET) {
		*out_type = lhs;
	} else {
		*out_type = rhs;
	}

	return true;
}

int mod_base_init(struct stg_module *mod)
{
	assert(mod->id == 0);

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("unset"));

		struct type unset = {0};
		unset.name = atom_create(mod->atom_table, STR("unset"));
		unset.base = base;
		unset.size = 0;

		type_id unset_id = register_type(&mod->store, unset);
		type_base_register_unifier(base, NULL, type_unset_unify);

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
	base_register_func(mod);
	base_register_enum(mod);
	base_register_bool(mod);
	base_register_str(mod);
	base_register_integer(mod);
	base_register_tuple(mod);
	base_register_array(mod);

	base_register_functions(mod);

	return 0;
}

void mod_base_free(struct stg_module *mod)
{
}

struct stg_module_info mod_base = {
	.name    = STR(MOD_BASE),
	.version = {0, 1},

	.init = mod_base_init,
	.free = mod_base_free,
};
