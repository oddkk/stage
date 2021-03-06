#include "mod.h"
#include "../module.h"
#include <ffi.h>

static struct string
obj_type_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	type_id tid = *(type_id *)obj->data;
	struct type *type = vm_get_type(vm, tid);
	struct string res = {0};

	arena_string_append(mem, &res, STR("Type("));
	arena_string_append_type_repr(&res, vm, mem, type);
	arena_string_append(mem, &res, STR(")"));

	return res;
}

static bool
obj_type_equals(struct vm *vm, void *type_data, void *lhs_data, void *rhs_data)
{
	type_id lhs, rhs;
	lhs = *(type_id *)lhs_data;
	rhs = *(type_id *)rhs_data;

	return type_equals(vm, lhs, rhs);
}

static struct type_base base_type_base = {
	.name = STR("Type"),
	.obj_repr = obj_type_repr,
	.obj_equals = obj_type_equals,
};

void
base_bootstrap_register_type(struct stg_module *mod)
{
	struct type t;
	t = init_plain_type(
			&base_type_base,
			mod_atoms(mod, "Type"),
			type_id);
	t.ffi_type = &ffi_type_uint64;

	modtype_id local_tid;
	local_tid = store_register_type(&mod->store, t);

	type_id tid;
	tid = TYPE_ID(mod->id, local_tid);

	struct object obj;
	obj.type = tid;
	obj.data = &tid;

	struct object new_obj;
	new_obj = register_object(mod->vm, &mod->store, obj);

	mod->vm->default_types.type = tid;
}
