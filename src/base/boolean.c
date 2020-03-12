#include "mod.h"
#include "../module.h"
#include "../native.h"
#include <ffi.h>

static struct string
obj_boolean_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	int value = *(int *)obj->data;
	return arena_sprintf(mem, "%s", value != 0 ? "true" : "false");
}

static struct type_base base_boolean_base = {
	.name = STR("Bool"),
	.obj_repr = obj_boolean_repr,
};

void
base_bootstrap_boolean_pre_compile(struct stg_module *mod)
{
	struct type t;
	t = init_plain_type(
			&base_boolean_base,
			mod_atoms(mod, "Bool"),
			int);
	t.ffi_type = &ffi_type_sint;

	type_id tid;
	tid = stg_register_type(mod, t);

	mod->vm->default_types.boolean = tid;
}

void
base_boolean_register(struct stg_module *mod)
{
	stg_mod_register_native_type(mod,
			mod_atoms(mod, "Bool"),
			mod->vm->default_types.boolean);

	int true_val = 1;
	struct object true_obj = {0};
	true_obj.type = mod->vm->default_types.boolean;
	true_obj.data = &true_val;
	stg_mod_register_native_object(mod,
			mod_atoms(mod, "true"), true_obj);

	int false_val = 0;
	struct object false_obj = {0};
	false_obj.type = mod->vm->default_types.boolean;
	false_obj.data = &false_val;
	stg_mod_register_native_object(mod,
			mod_atoms(mod, "false"), false_obj);
}
