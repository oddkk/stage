#include "mod.h"
#include "../../string.h"

static struct string obj_type_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	type_id tid = *(type_id *)obj->data;
	struct type *type = vm_get_type(vm, tid);
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("type("));
	arena_string_append_type_repr(&res, vm, mem, type);
	arena_string_append(mem, &res, STR(")"));

	return res;
}

struct type_base base_type_base = {
	.name = STR("type"),
	.obj_repr = obj_type_repr,
};

struct object
obj_register_type(struct vm *vm, struct objstore *store, type_id value)
{
	struct object result = {0};

	result.type = vm->default_types.type;
	result.data = &value;

	return register_object(vm, store, result);
}

type_id type_obj_get(struct vm *vm, struct object obj)
{
	assert(obj.type == vm->default_types.type);
	return *(type_id *)obj.data;
}

void base_register_type(struct stg_module *mod)
{
	// NOTE: We can not use stg_register_builtin_type because
	// vm->default_types.type is not yet defined.
	/* type_id tid; */
	/* tid = stg_register_builtin_type(mod, &base_type_base, */
	/* 								STG_TYPE_DATA(STG_TYPE)); */

	struct stg_builtin_type type;
	type = STG_TYPE_DATA(STG_TYPE);

	type_base_init_unfilled(&base_type_base);

	assert(string_equal(mod->info.name, type.mod));

	struct type t = {0};
	t.name = atom_create(mod->atom_table, type.name);
	t.base = &base_type_base;
	t.size = type.size;

	modtype_id local_tid;
	local_tid = register_type(&mod->store, t);

	type_id tid;
	tid = TYPE_ID(mod->id, local_tid);

	struct object obj;
	obj.type = tid;
	obj.data = &tid;

	struct object new_obj;
	new_obj = register_object(mod->vm, &mod->store, obj);

	// TODO: Register object in ast.

	mod->vm->default_types.type = tid;
}
