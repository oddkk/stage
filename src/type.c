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
	case TYPE_KIND_TUPLE:
		for (size_t i = 0; i < type->tuple.num_types; i++) {
			type_id child_type_id;
			struct type *child;
			int ret;

			child_type_id = type->tuple.types[i];
			child = stage->types[child_type_id];
			ret = type_count_scalars(stage, child);
			if (ret == -1) {
				return ret;
			}
			count += ret;
		}
		return count;
	}
	print_error("type", "Type '%.*s' has invalid kind %i", LIT(type->name),
		    type->kind);
	return -1;
}

void print_type(struct stage *stage, struct type *type)
{
	if (!type->name.length) {
		expand_type(stage, type, false);
	}

	printf("%.*s", LIT(type->name));
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
	case TYPE_KIND_TUPLE:
		printf("(");
		for (size_t i = 0; i < type->tuple.num_types; i++) {
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
	}
}

struct type *register_type(struct stage *stage, struct type def)
{
	struct type *type;
	int old_id, error;

	if (def.name.length == 0) {
		print_error("register type", "Type must have name.",
			    LIT(def.name));
		return 0;
	}

	old_id = id_lookup_table_lookup(&stage->types_lookup, def.name);
	if (old_id >= 0) {
		print_error("register type",
			    "Cannot register type '%.*s' because "
			    "the name is already registered.", LIT(def.name));
		return 0;
	}

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
	error =
	    id_lookup_table_insert(&stage->types_lookup, type->name, type->id);

	type->num_scalars = type_count_scalars(stage, type);

	return type;
}

struct type *register_scalar_type(struct stage *stage, struct string name,
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

struct type *register_tuple_type(struct stage *stage, struct string name,
				 type_id * subtypes, size_t num_subtypes)
{
	struct type new_type;

	zero_memory(&new_type, sizeof(struct type));

	new_type.name = name;
	new_type.kind = TYPE_KIND_TUPLE;

	new_type.tuple.num_types = num_subtypes;
	new_type.tuple.types =
	    arena_alloc(&stage->memory,
			sizeof(type_id) * new_type.tuple.num_types);

	memcpy(new_type.tuple.types, subtypes,
	       sizeof(type_id) * new_type.tuple.num_types);

	return register_type(stage, new_type);
}

int assign_value(struct value *dest, struct type *dest_type, struct value *src,
		 struct type *src_type)
{
	return -1;
}

void register_default_types(struct stage *stage)
{
	struct type new_type;

	stage->standard_types.integer =
	    register_scalar_type(stage, STR("int"), SCALAR_MIN, SCALAR_MAX)->id;

	scoped_hash_insert(&stage->root_scope,
			   atom_create(&stage->atom_table, STR("int")),
			   SCOPE_ENTRY_TYPE, stage->standard_types.integer,
			   NULL, 0);

	new_type.kind = TYPE_KIND_STRING;
	new_type.name = STR("string");
	stage->standard_types.string = register_type(stage, new_type)->id;

	/* new_type.kind = TYPE_KIND_ANY; */
	/* new_type.name = STR("any"); */
	/* stage->standard_types.string = */
	/*      register_type(stage, new_type)->id; */
}

void print_scalar(scalar_value val)
{
	if (val == SCALAR_OFF) {
		printf("off");
	} else {
		printf("%i", val);
	}
}
