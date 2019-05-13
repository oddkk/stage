#include "mod.h"

#include <stdlib.h>

BUILTIN_PURE(array, obj_array_type_constructor, STG_TYPE, (STG_TYPE, type), (STG_INT, length))
{
	return type_register_array(vm, &mod->store, type, length);
}

static struct string
type_array_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct type_array *array = type->data;
	struct string res = arena_string_init(mem);

	struct type *member_type;
	member_type = vm_get_type(vm, array->type);
	arena_string_append_type_repr(&res, vm, mem, member_type);

	arena_string_append_sprintf(mem, &res, "[%zu]", array->length);

	return res;
}

static struct string
obj_array_repr(struct vm *vm, struct arena *mem, struct object *object)
{
	struct type *type = vm_get_type(vm, object->type);
	struct type_array *array = type->data;
	struct type *member_type = vm_get_type(vm, array->type);
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("["));

	struct object member;
	member.type = array->type;

	for (size_t i = 0; i < array->length; i++) {
		member.data = &((uint8_t *)object->data)[i * member_type->size];

		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		arena_string_append_obj_repr(&res, vm, mem, &member);
	}

	arena_string_append(mem, &res, STR("]"));

	return res;
}

static type_id type_array_subtypes_iter(struct vm *vm, struct type *type, size_t *iter)
{
	struct type_array *array = (struct type_array *)type->data;

	if (*iter == 0) {
		*iter += 1;
		return array->type;
	} else {
		return TYPE_SUBTYPES_END;
	}
}


struct type_base base_array_base = {
	.name = STR("array"),
	.repr = type_array_repr,
	.obj_repr = obj_array_repr,
	.subtypes_iter = type_array_subtypes_iter,
};

type_id
type_register_array(struct vm *vm, struct objstore *store,
					type_id subtype_id, size_t length)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("array"));
	type.base = &base_array_base;

	// TODO: This should not be calloc, but we do not have a better system for
	// storing the data for types yet.
	struct type_array *array = calloc(1, sizeof(struct type_array));
	array->type = subtype_id;
	array->length = length;

	struct type *subtype = vm_get_type(vm, subtype_id);

	type_id result;

	type.num_template_params = subtype->num_template_params;
	type.size = subtype->size * length;
	type.data = (void *)array;

	result = register_type(store, type);

	return result;
}

void base_register_array(struct stg_module *mod)
{
	stg_register_builtin_func(mod, obj_array_type_constructor, NULL);
}
