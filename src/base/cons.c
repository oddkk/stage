#include "mod.h"
#include "../module.h"
#include "../ast.h"
#include <ffi.h>

struct type_base base_cons_base = {
	.name = STR("cons"),
};

struct type_base base_inst_base = {
	.name = STR("inst"),
};

void
base_bootstrap_register_cons(struct stg_module *mod)
{
	{
		struct type t;
		t = init_plain_type(
				&base_cons_base,
				mod_atoms(mod, "cons"),
				struct object_cons *);
		// This type should not be used in runtime expressions.
		// t.ffi_type = &ffi_type_pointer;

		type_id tid;
		tid = stg_register_type(mod, t);

		mod->vm->default_types.cons = tid;
	}

	{
		struct type t;
		t = init_plain_type(
				&base_inst_base,
				mod_atoms(mod, "inst"),
				struct object_inst *);
		// This type should not be used in runtime expressions.
		// t.ffi_type = &ffi_type_pointer;

		type_id tid;
		tid = stg_register_type(mod, t);

		mod->vm->default_types.inst = tid;
	}
}
