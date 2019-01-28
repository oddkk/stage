#include "mod.h"

static struct string obj_string_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	struct string value = *(struct string *)obj->data;
	return arena_sprintf(mem, "\"%.*s\"", LIT(value));
}

struct type_base base_str_base = {
	.name = STR("str"),
	.obj_repr = obj_string_repr,
};

struct object
obj_register_string(struct vm *vm, struct objstore *store,
					struct string value)
{
	struct object result = {0};

	result.type = vm->default_types.string;
	result.data = &value;

	return register_object(store, result);
}


void base_register_str(struct stg_module *mod)
{
	mod->vm->default_types.string =
		stg_register_builtin_type(mod, &base_str_base,
								STG_TYPE_DATA(STG_STR));
}
