#include "mod.h"

static struct string type_tuple_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct type_tuple *tuple = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("("));

	for (size_t i = 0; i < tuple->num_items; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		if (tuple->named) {
			arena_string_append(mem, &res, tuple->names[i]->name);
			arena_string_append(mem, &res, STR(": "));
		}

		struct type *item_type;
		item_type = vm_get_type(vm, tuple->types[i]);
		arena_string_append_type_repr(&res, vm, mem, item_type);
	}

	arena_string_append(mem, &res, STR(")"));

	return res;
}

static struct string obj_tuple_repr(struct vm *vm, struct arena *mem, struct object *object)
{
	struct type *type = vm_get_type(vm, object->type);
	struct type_tuple *tuple = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("("));

	size_t offset = 0;

	for (size_t i = 0; i < tuple->num_items; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		if (tuple->named) {
			arena_string_append(mem, &res, tuple->names[i]->name);
			arena_string_append(mem, &res, STR(" = "));
		}

		struct object item_obj = {0};
		item_obj.data = (uint8_t *)object->data + offset;
		item_obj.type = tuple->types[i];

		arena_string_append_obj_repr(&res, vm, mem, &item_obj);

		struct type *item_type;
		item_type = vm_get_type(vm, tuple->types[i]);
		offset += item_type->size;
	}

	arena_string_append(mem, &res, STR(")"));

	return res;
}

static type_id type_tuple_subtypes_iter(struct vm *vm, struct type *type, size_t *iter)
{
	struct type_tuple *tuple = (struct type_tuple *)type->data;

	if (*iter < tuple->num_items) {
		*iter += 1;
		return tuple->types[*iter];
	} else {
		return TYPE_SUBTYPES_END;
	}
}

struct type_base base_tuple_base = {
	.name = STR("tuple"),
	.repr = type_tuple_repr,
	.obj_repr = obj_tuple_repr,
	.subtypes_iter = type_tuple_subtypes_iter,
};

struct tuple_member_data {
	uint16_t member_size;
	uint16_t tuple_size;
	uint16_t offset;
};

static_assert(sizeof(struct tuple_member_data) <= sizeof(void *),
			  "For now, tuple member data should fit in a void *.");

static void tuple_access_member(struct vm *vm, struct exec_stack *stack, void *data)
{
	struct tuple_member_data tuple_member = *(struct tuple_member_data *)&data;
	uint8_t buffer[tuple_member.tuple_size];

	stack_pop(stack, buffer, tuple_member.tuple_size);

	stack_push(stack, &buffer[tuple_member.offset], tuple_member.member_size);
}

type_id type_register_named_tuple(struct vm *vm, struct objstore *store,
								  struct type_tuple_item *items,
								  size_t num_items)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("tuple"));
	type.base = &base_tuple_base;

	struct type_tuple *tuple = arena_alloc(&vm->memory, sizeof(struct type_tuple));

	tuple->names = arena_alloc(&vm->memory, num_items * sizeof(struct atom *));
	tuple->types = arena_alloc(&vm->memory, num_items * sizeof(type_id));
	tuple->num_items = num_items;
	tuple->named = true;

	size_t size = 0;
	bool num_template_params = 0;

	for (size_t i = 0; i < num_items; i++) {
		tuple->names[i] = items[i].name;
		tuple->types[i] = items[i].type;

		struct type *subtype = vm_get_type(vm, items[i].type);

		size += subtype->size;
		num_template_params += num_template_params;

		// @TODO: Check for duplicate members.
	}

	type_id result;

	type.num_template_params = num_template_params;
	type.size = size;
	type.data = (void *)tuple;

	struct scope *obj_scope = NULL;

	if (num_template_params == 0) {
		obj_scope = scope_push(&vm->root_scope);
		type.object_scope = obj_scope;
	}

	result = register_type(store, type);

	if (obj_scope) {
		size_t offset = 0;

		for (size_t i = 0; i < num_items; i++) {
			struct object access_func;

			struct atom *param_name;
			type_id param_type;
			param_name = atom_create(&vm->atom_table, STR("self"));
			param_type = result;

			type_id ret_type = items[i].type;

			struct type *subtype = vm_get_type(vm, items[i].type);
			struct tuple_member_data data = {0};

			data.tuple_size = type.size;
			data.member_size = subtype->size;
			data.offset = offset;

			access_func =
				obj_register_builtin_func(vm, store, &param_name, &param_type, 1,
										  ret_type, tuple_access_member,
										  *(void **)&data);

			int err;
			err = scope_insert(obj_scope,
							   items[i].name,
							   SCOPE_ANCHOR_ABSOLUTE,
							   access_func,
							   NULL);

			if (err < 0) {
				printf("Tuple has duplicate member '%.*s'.",
					   ALIT(items[i].name));
				// @TODO: Deallocate
				return TYPE_NONE;
			}

			offset += subtype->size;
		}
	}

	return result;
}

type_id type_register_unnamed_tuple(struct vm *vm, struct objstore *store,
									type_id *items, size_t num_items)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("tuple"));
	type.base = &base_tuple_base;

	struct type_tuple *tuple = arena_alloc(&vm->memory, sizeof(struct type_tuple));

	tuple->types = arena_alloc(&vm->memory, num_items * sizeof(type_id));
	tuple->num_items = num_items;
	tuple->named = false;

	size_t size = 0;
	bool num_template_params = 0;

	for (size_t i = 0; i < num_items; i++) {
		tuple->types[i] = items[i];

		struct type *subtype = vm_get_type(vm, items[i]);

		size += subtype->size;
		num_template_params += num_template_params;
	}

	type.num_template_params = num_template_params;
	type.size = size;
	type.data = (void *)tuple;

	return register_type(store, type);
}



void
base_register_tuple(struct stg_module *mod)
{
}
