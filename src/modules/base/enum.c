#include "mod.h"
#include "string.h"

static struct string type_enum_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct type_enum *type_enum = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("Enum {"));

	for (size_t i = 0; i < type_enum->num_items; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		struct type_enum_item *item;
		item = &type_enum->items[i];

		arena_string_append(mem, &res, item->name->name);

		if (item->type != TYPE_NONE) {
			struct type *item_type;
			item_type = vm_get_type(vm, item->type);
			arena_string_append(mem, &res, STR("("));
			arena_string_append_type_repr(&res, vm, mem, item_type);
			arena_string_append(mem, &res, STR(")"));
		}
	}

	arena_string_append(mem, &res, STR("}"));


	return res;
}

static struct string obj_enum_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	int64_t value = *(int64_t *)obj->data;
	struct type *type = vm_get_type(vm, obj->type);
	struct type_enum *type_enum = type->data;

	struct string res = arena_string_init(mem);

	if (value < type_enum->num_items) {
		struct type_enum_item *item = &type_enum->items[value];
		arena_string_append(mem, &res, item->name->name);

		if (item->type != TYPE_NONE) {
			struct object item_obj = {0};
			item_obj.type = item->type;
			item_obj.data = (uint8_t *)obj->data + sizeof(int64_t);

			arena_string_append(mem, &res, STR(" "));
			arena_string_append_obj_repr(&res, vm, mem, &item_obj);
		}

	} else {
		arena_string_append(mem, &res, STR("(invalid)"));
	}

	return res;
}

static type_id type_enum_subtypes_iter(struct vm *vm, struct type *type, size_t *iter)
{
	struct type_enum *enum_data = (struct type_enum *)type->data;

	for (; *iter < enum_data->num_items; *iter += 1) {
		if (enum_data->items[*iter].type != TYPE_NONE) {
			return enum_data->items[*iter].type;
		}
	}

	return TYPE_SUBTYPES_END;

}

struct type_base base_enum_base = {
	.name = STR("enum"),
	.repr = type_enum_repr,
	.obj_repr = obj_enum_repr,
	.subtypes_iter = type_enum_subtypes_iter,
};

type_id type_register_enum(struct vm *vm, struct objstore *store, struct type_enum *t)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("enum"));
	type.base = &base_enum_base;
	type.data = (void *)t;

	size_t size = 0;
	int num_template_params = 0;
	for (size_t i = 0; i < t->num_items; i++) {
		struct type_enum_item *itm = &t->items[i];

		itm->value = i;
		itm->owner = t;

		struct type *item_type = vm_get_type(vm, itm->type);
		num_template_params += item_type->num_template_params;

		size += item_type->size;
	}

	type.num_template_params = num_template_params;
	t->size = size;
	type.size = t->size + sizeof(int64_t);

	return register_type(store, type);
}


void
base_register_enum(struct stg_module *mod)
{
}
