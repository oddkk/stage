#include "mod.h"
#include "../vm.h"
#include "../module.h"
#include "../native.h"
#include <stdlib.h>
#include <ffi.h>

static int64_t
print_int(int64_t val)
{
	printf("= %li\n", val);
	return val;
}

static void
func_unset_call()
{
	panic("Called an unset function.");
}

static int
stg_base_bootstrap_pre_compile(struct stg_module *mod) {
	assert(mod->id == 0);

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		base->name = STR("unset");

		struct type unset = {0};
		unset.name = mod_atoms(mod, "unset");
		unset.base = base;
		unset.size = 0;

		type_id unset_id = store_register_type(&mod->store, unset);

		assert(unset_id == TYPE_UNSET);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		base->name = STR("none");

		struct type none = {0};
		none.name = mod_atoms(mod, "none");
		none.base = base;
		none.size = 0;

		type_id none_id = store_register_type(&mod->store, none);
		assert(none_id == TYPE_NONE);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		base->name = STR("Unit");

		struct type unit = {0};
		unit.name = mod_atoms(mod, "unit");
		unit.base = base;
		unit.size = 0;
		unit.ffi_type = &ffi_type_void;

		mod->vm->default_types.unit = store_register_type(&mod->store, unit);
	}

	{
		struct func func_unset = {0};

		func_unset.native = (void *)func_unset_call;
		func_unset.type = stg_register_func_type(mod,
				mod->vm->default_types.unit, NULL, 0);

		func_id fid;
		fid = store_register_func(&mod->store, func_unset);
		assert(fid == 0);
	}

	base_bootstrap_register_type(mod);
	base_bootstrap_register_cons(mod);
	base_bootstrap_register_integer(mod);
	base_bootstrap_register_string(mod);
	base_bootstrap_boolean_pre_compile(mod);

	return 0;
}

static int
stg_base_register(struct stg_module *mod)
{
	struct stg_base_mod_info *info;
	info = calloc(1, sizeof(struct stg_base_mod_info));
	mod->data = info;

	stg_mod_register_native_type(mod,
			mod_atoms(mod, "int"),
			mod->vm->default_types.integer);

	stg_mod_register_native_type(mod,
			mod_atoms(mod, "Type"),
			mod->vm->default_types.type);

	stg_mod_register_native_type(mod,
			mod_atoms(mod, "Unit"),
			mod->vm->default_types.unit);

	struct object unit_inst = {0};
	unit_inst.type = mod->vm->default_types.unit;
	unit_inst.data = NULL;

	stg_mod_register_native_object(mod,
			mod_atoms(mod, "unit"),
			unit_inst);

	stg_mod_register_native_type(mod,
			mod_atoms(mod, "String"),
			mod->vm->default_types.string);

	base_boolean_register(mod);

	base_init_register_init(mod);
	base_init_register_io(mod);
	stg_list_register(mod);

	return 0;
}

void
stg_base_load(struct vm *vm)
{
	struct stg_module *base_mod;
	base_mod = vm_request_module(vm,
			VM_REQUEST_PINNED,
			vm_atoms(vm, "base"),
			VM_REQUEST_MOD_NO_LOC);

	// We manually register the base_bootstrap module this early because it is
	// required for the compiler to function.
	stg_base_bootstrap_pre_compile(base_mod);

	struct stg_native_module *mod;
	mod = vm_add_precompiled_native_module(vm, STR("base"));

	base_integer_register_native(mod);
	base_string_register_native(mod);
	base_init_register_native(mod);
	base_io_register_native(mod);
	stg_native_register_funcs(mod, print_int, STG_NATIVE_FUNC_IMPURE);
	stg_list_register_native(mod);

	mod->hook_register = stg_base_register;
}
