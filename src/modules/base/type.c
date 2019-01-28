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
	type_id tid;
	tid = stg_register_builtin_type(mod, &base_type_base,
									STG_TYPE_DATA(STG_TYPE));
	mod->vm->default_types.type = tid;
}
