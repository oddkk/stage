#include "type.h"
#include "stage.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int type_count_scalars(struct stage *stage, struct type *type)
{
	int count = 0;
	switch (type->kind) {

	case TYPE_KIND_SCALAR:
		return 1;

	case TYPE_KIND_STRING:
		return -1;

	case TYPE_KIND_TYPE:
		return -1;

	case TYPE_KIND_TUPLE:
		for (size_t i = 0; i < type->tuple.length; i++) {
			type_id child_type_id;
			struct type *child;
			int ret;

			child_type_id = type->tuple.types[i];
			child = get_type(stage, child_type_id);
			if (!child) {
				return -1;
			}

			ret = type_count_scalars(stage, child);
			if (ret == -1) {
				return ret;
			}
			count += ret;
		}
		return count;

	case TYPE_KIND_NAMED_TUPLE:
		for (size_t i = 0; i < type->named_tuple.length; i++) {
			type_id child_type_id;
			struct type *child;
			int ret;

			child_type_id = type->named_tuple.members[i].type;
			child = get_type(stage, child_type_id);
			if (!child) {
				return -1;
			}

			ret = type_count_scalars(stage, child);
			if (ret == -1) {
				return ret;
			}
			count += ret;
		}
		return count;
	}
	print_error("type", "Type '%.*s' has invalid kind %i", ALIT(type->name),
		    type->kind);
	return -1;
}

void print_type(struct stage *stage, struct type *type)
{
	if (!type->name) {
		expand_type(stage, type, false);
	}

	printf("%.*s", ALIT(type->name));
}

void print_type_id(struct stage *stage, type_id id)
{
	if (id == TYPE_TEMPLATE) {
		printf("(template)");
	} else {
		print_type(stage, get_type(stage, id));
	}
}

void expand_type(struct stage *stage, struct type *type, bool recurse_expand)
{
	switch (type->kind) {
	case TYPE_KIND_SCALAR:
		printf("int{%i..%i}", type->scalar.min, type->scalar.max);
		break;

	case TYPE_KIND_STRING:
		printf("string");
		break;

	case TYPE_KIND_TYPE:
		printf("type");
		break;

	case TYPE_KIND_TUPLE:
		printf("(");
		for (size_t i = 0; i < type->tuple.length; i++) {
			type_id child_type_id;
			struct type *child;

			child_type_id = type->tuple.types[i];
			child = stage->types[child_type_id];

			if (i != 0) {
				printf(", ");
			}

			if (recurse_expand) {
				expand_type(stage, child, recurse_expand);
			} else {
				print_type(stage, child);
			}
		}
		printf(")");
		break;

	case TYPE_KIND_NAMED_TUPLE:
		printf("(");
		for (size_t i = 0; i < type->tuple.length; i++) {
			type_id child_type_id;
			struct type *child;
			struct atom *name;

			name = type->named_tuple.members[i].name;
			child_type_id = type->named_tuple.members[i].type;
			child = stage->types[child_type_id];

			if (i != 0) {
				printf(", ");
			}

			printf("%.*s: ", ALIT(name));
			if (recurse_expand) {
				expand_type(stage, child, recurse_expand);
			} else {
				print_type(stage, child);
			}
		}
		printf(")");
		break;
	}
}

void expand_type_id(struct stage *stage, type_id id, bool recurse_expand)
{
	if (id == TYPE_TEMPLATE) {
		printf("(template)");
	} else {
		expand_type(stage, get_type(stage, id), recurse_expand);
	}
}

int register_type_name(struct stage *stage, type_id type, struct scoped_hash *scope, struct atom *name)
{
	int result;
	assert(scope);
	assert(name);

	result = scoped_hash_insert(scope, name, SCOPE_ENTRY_TYPE, type, NULL, NULL);

	return result;
}

struct type *register_type(struct stage *stage, struct type def)
{
	struct type *type;
	int error;

	if (stage->num_types + 1 >= stage->cap_types) {
		struct type **new_array;
		stage->cap_types += 10;

		// TODO: Use a different allocation schema.
		new_array = realloc(stage->types,
				    stage->cap_types * sizeof(struct type *));
		if (!new_array) {
			print_error("register type",
				    "Failed to allocate memory for type list");
			return 0;
		}
		stage->types = new_array;
	}

	type = arena_alloc(&stage->memory, sizeof(struct type));
	*type = def;
	type->id = stage->num_types++;

	stage->types[type->id] = type;

	if (type->name) {
		error =
			id_lookup_table_insert(&stage->types_lookup, type->name->name,
					type->id);
	}

	type->num_scalars = type_count_scalars(stage, type);

	return type;
}

struct type *register_scalar_type(struct stage *stage, struct atom *name,
				  scalar_value min, scalar_value max)
{
	struct type new_type;

	zero_memory(&new_type, sizeof(struct type));

	new_type.name = name;
	new_type.kind = TYPE_KIND_SCALAR;
	new_type.scalar.min = min;
	new_type.scalar.max = max;

	return register_type(stage, new_type);
}

struct type *register_tuple_type(struct stage *stage, struct atom *name,
				 type_id * subtypes, size_t num_subtypes)
{
	struct type new_type;

	zero_memory(&new_type, sizeof(struct type));

	new_type.name = name;
	new_type.kind = TYPE_KIND_TUPLE;

	new_type.tuple.length = num_subtypes;
	new_type.tuple.types =
	    arena_alloc(&stage->memory,
			sizeof(type_id) * new_type.tuple.length);

	memcpy(new_type.tuple.types, subtypes,
	       sizeof(type_id) * new_type.tuple.length);

	return register_type(stage, new_type);
}

struct type *register_named_tuple_type(struct stage *stage, struct atom *name,
				 struct named_tuple_member * members, size_t num_members)
{
	struct type new_type;

	zero_memory(&new_type, sizeof(struct type));

	new_type.name = name;
	new_type.kind = TYPE_KIND_NAMED_TUPLE;

	new_type.named_tuple.length = num_members;
	new_type.named_tuple.members =
		arena_alloc(&stage->memory, sizeof(struct named_tuple_member) * num_members);

	memcpy(new_type.tuple.types, members, sizeof(struct named_tuple_member) * num_members);

	return register_type(stage, new_type);
}

int assign_value(struct value *dest, struct type *dest_type, struct value *src,
		 struct type *src_type)
{
	return -1;
}

void register_default_types(struct stage *stage)
{
	stage->standard_types.integer =
	    register_scalar_type(stage,
				 atom_create(&stage->atom_table, STR("int")),
				 SCALAR_MIN, SCALAR_MAX)->id;
	register_type_name(stage, stage->standard_types.integer, &stage->root_scope,
					   atom_create(&stage->atom_table, STR("int")));

	struct type string_type;
	string_type.kind = TYPE_KIND_STRING;
	string_type.name = atom_create(&stage->atom_table, STR("string"));
	stage->standard_types.string = register_type(stage, string_type)->id;


	struct type type_type;
	type_type.kind = TYPE_KIND_TYPE;
	type_type.name = atom_create(&stage->atom_table, STR("type"));
	stage->standard_types.type = register_type(stage, type_type)->id;
	register_type_name(stage, stage->standard_types.type, &stage->root_scope, type_type.name);
}

void print_scalar(scalar_value val)
{
	if (val == SCALAR_OFF) {
		printf("off");
	} else {
		printf("%i", val);
	}
}
