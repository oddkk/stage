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
		return 1;

	case TYPE_KIND_TYPE:
		return 1;

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

	case TYPE_KIND_ARRAY: {
		struct type *child;
		child = get_type(stage, type->array.type);
		return type->array.length * child->num_scalars;
	}
	}
	print_error("type", "Type '%.*s' has invalid kind %i", ALIT(type->name),
		    type->kind);
	return -1;
}

void print_type(FILE *fp, struct stage *stage, struct type *type)
{
	if (!type->name) {
		expand_type(fp, stage, type, false);
	} else {
		fprintf(fp, "%.*s", ALIT(type->name));
	}
}

void print_type_id(FILE *fp, struct stage *stage, type_id id)
{
	if (id == TYPE_TEMPLATE) {
		fprintf(fp, "(template)");
	} else {
		print_type(fp, stage, get_type(stage, id));
	}
}

void expand_type(FILE *fp, struct stage *stage, struct type *type, bool recurse_expand)
{
	switch (type->kind) {
	case TYPE_KIND_SCALAR:
		fprintf(fp, "int{%i..%i}", type->scalar.min, type->scalar.max);
		break;

	case TYPE_KIND_STRING:
		fprintf(fp, "string");
		break;

	case TYPE_KIND_TYPE:
		fprintf(fp, "type");
		break;

	case TYPE_KIND_TUPLE:
		fprintf(fp, "(");
		for (size_t i = 0; i < type->tuple.length; i++) {
			type_id child_type_id;
			struct type *child;

			child_type_id = type->tuple.types[i];
			child = stage->types[child_type_id];

			if (i != 0) {
				fprintf(fp, ", ");
			}

			if (recurse_expand) {
				expand_type(fp, stage, child, recurse_expand);
			} else {
				print_type(fp, stage, child);
			}
		}
		fprintf(fp, ")");
		break;

	case TYPE_KIND_NAMED_TUPLE:
		fprintf(fp, "(");
		for (size_t i = 0; i < type->tuple.length; i++) {
			type_id child_type_id;
			struct type *child;
			struct atom *name;

			name = type->named_tuple.members[i].name;
			child_type_id = type->named_tuple.members[i].type;
			child = stage->types[child_type_id];

			if (i != 0) {
				fprintf(fp, ", ");
			}

			fprintf(fp, "%.*s: ", ALIT(name));
			if (recurse_expand) {
				expand_type(fp, stage, child, recurse_expand);
			} else {
				print_type(fp, stage, child);
			}
		}
		fprintf(fp, ")");
		break;

	case TYPE_KIND_ARRAY: {
		struct type *child;
		child = get_type(stage, type->array.type);
		if (recurse_expand) {
			expand_type(fp, stage, child, recurse_expand);
		} else {
			print_type(fp, stage, child);
		}
		fprintf(fp, "[%zu]", type->array.length);

	} break;
	}
}

void expand_type_id(FILE *fp, struct stage *stage, type_id id, bool recurse_expand)
{
	if (id == TYPE_TEMPLATE) {
		fprintf(fp, "(template)");
	} else {
		expand_type(fp, stage, get_type(stage, id), recurse_expand);
	}
}

int register_type_name(struct stage *stage, type_id type, struct scoped_hash *scope, struct atom *name)
{
	assert(scope);
	assert(name);

	struct scope_entry *entry;
	entry = scoped_hash_insert(scope, name, SCOPE_ENTRY_TYPE, NULL, NULL);
	if (!entry) {
		struct scope_entry conflict;
		int err;
		err = scoped_hash_lookup(scope, name, &conflict);
		assert(!err);
		print_error("register type",
					"Cannot register '%.*s', because there already "
					"exists a %s with the same name in this scope.",
					ALIT(name), humanreadable_scope_entry(conflict.kind));
		return -1;
	}
	entry->id = type;

	return 0;
}

struct type *register_type(struct stage *stage, struct type def)
{
	struct type *type;

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

	type->num_scalars = type_count_scalars(stage, type);

	switch (type->kind) {
	case TYPE_KIND_SCALAR:
	case TYPE_KIND_STRING:
	case TYPE_KIND_TYPE:
		break;

	case TYPE_KIND_ARRAY: {
		if (type->array.type == TYPE_TEMPLATE) {
			type->templated = true;
			break;
		}

		struct type *child;
		child = get_type(stage, type->array.type);
		if (child->templated) {
			type->templated = true;
		}
	} break;

	case TYPE_KIND_TUPLE:
		for (size_t i = 0; i < type->tuple.length; i++) {
			if (type->named_tuple.members[i].type == TYPE_TEMPLATE) {
				type->templated = true;
				break;
			}
			struct type *member = get_type(stage, type->tuple.types[i]);
			if (member->templated) {
				type->templated = true;
				break;
			}
		}
		break;

	case TYPE_KIND_NAMED_TUPLE:
		for (size_t i = 0; i < type->named_tuple.length; i++) {
			if (type->named_tuple.members[i].type == TYPE_TEMPLATE) {
				type->templated = true;
				break;
			}
			struct type *member = get_type(stage, type->named_tuple.members[i].type);
			if (member->templated) {
				type->templated = true;
				break;
			}
		}
		break;
	}

	return type;
}

struct type *register_scalar_type(struct stage *stage, struct atom *name,
				  scalar_value min, scalar_value max)
{
	struct type new_type = {0};

	new_type.name = name;
	new_type.kind = TYPE_KIND_SCALAR;
	new_type.scalar.min = min;
	new_type.scalar.max = max;

	return register_type(stage, new_type);
}

struct type *register_tuple_type(struct stage *stage, struct atom *name,
				 type_id * subtypes, size_t num_subtypes)
{
	struct type new_type = {0};

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
	struct type new_type = {0};

	new_type.name = name;
	new_type.kind = TYPE_KIND_NAMED_TUPLE;

	new_type.named_tuple.length = num_members;
	new_type.named_tuple.members =
		arena_alloc(&stage->memory, sizeof(struct named_tuple_member) * num_members);

	memcpy(new_type.tuple.types, members, sizeof(struct named_tuple_member) * num_members);

	return register_type(stage, new_type);
}

struct type *register_array_type(struct stage *stage, struct atom *name,
								 type_id type, size_t length)
{
	struct type new_type = {0};

	new_type.name = name;
	new_type.kind = TYPE_KIND_ARRAY;

	new_type.array.type = type;
	new_type.array.length = length;

	return register_type(stage, new_type);
}

int register_typed_member_in_scope(struct stage *stage, struct atom *name,
								   type_id tid, struct scoped_hash *scope,
								   enum scope_entry_kind kind, int start_id)
{
	struct type *type;

	type = get_type(stage, tid);

	struct scoped_hash *new_scope = NULL;

	switch (type->kind) {
	case TYPE_KIND_TUPLE:
		printf("@TODO: Implement register in scope for unnamed tuples.\n");
		break;

	case TYPE_KIND_NAMED_TUPLE: {
		int subindex = 0;
		new_scope = scoped_hash_push(scope, kind, start_id);

		for (size_t i = 0; i < type->named_tuple.length; i++) {
			struct named_tuple_member *member;
			int num_scalars;

			member = &type->named_tuple.members[i];

			num_scalars
				= register_typed_member_in_scope(stage, member->name,
												 member->type, new_scope,
												 kind, start_id + subindex);

			subindex += num_scalars;
		}
		assert(subindex == type->num_scalars);
	} break;

	case TYPE_KIND_ARRAY:
		printf("@TODO: Implement register in scope for arrays.\n");
		break;

	default:
		break;
	}

	struct scope_entry *entry;
	entry = scoped_hash_insert(scope, name, kind, NULL, new_scope);
	if (!entry) {
		printf("Failed to register typed member\n");
		return -1;
	}

	entry->id = start_id;
	entry->length = type->num_scalars;
	entry->type = type->id;

	return type->num_scalars;
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

struct value_ref alloc_value(struct stage *stage, type_id tid)
{
	struct type *type = get_type(stage, tid);
	assert(type != NULL);

	struct value_ref result = {0};
	result.type = type->id;
	result.data = calloc(type->num_scalars, sizeof(scalar_value));

	return result;
}


int type_find_member(struct type_iterator *out,
					 struct stage *stage,
					 struct type_iterator iter,
					 struct atom *name)
{
	struct type *type = get_type(stage, iter.type);

	if (!type) {
		return -1;
	}

	switch (type->kind) {
	case TYPE_KIND_SCALAR:
	case TYPE_KIND_STRING:
	case TYPE_KIND_TYPE:
		return -2;

	case TYPE_KIND_TUPLE:
		printf("@TODO: Name of unnamed tuples\n");
		return -2;

	case TYPE_KIND_ARRAY:
		printf("@TODO: Name of array indicies\n");
		return -2;

	case TYPE_KIND_NAMED_TUPLE: {
		int subindex = iter.subindex;
		for (size_t i = 0; i < type->named_tuple.length; i++) {
			if (type->named_tuple.members[i].name == name) {
				out->type = type->named_tuple.members[i].type;
				out->subindex = subindex;
				return 0;
			}
			struct type *member_type;
			member_type = get_type(stage, type->named_tuple.members[i].type);
			subindex += member_type->num_scalars;
		}
	}

	}

	return -2;
}

int print_typed_value_internal(struct stage *stage, type_id tid, scalar_value *values, size_t num_values)
{
	struct type *type;

	type = get_type(stage, tid);
	int scalars_read = 0;

	switch (type->kind) {
	case TYPE_KIND_SCALAR:
		print_scalar(values[0]);
		scalars_read = 1;
		break;

	case TYPE_KIND_STRING:
		printf("(@TODO: strings)");
		break;

	case TYPE_KIND_TYPE:
		printf("(type ");
		if (values[0] == SCALAR_OFF) {
			printf("none");
		} else {
			print_type_id(stdout, stage, values[0]);
		}
		scalars_read = 1;
		printf(")");
		break;

	case TYPE_KIND_TUPLE:
		printf("(");
		for (size_t i = 0; i < type->tuple.length; i++) {
			if (i > 0) {
				printf(",\n");
			}
			scalars_read
				+= print_typed_value_internal(stage, type->tuple.types[i],
											  &values[scalars_read],
											  num_values - scalars_read);
		}
		printf(")");
		break;

	case TYPE_KIND_NAMED_TUPLE:
		printf("(");
		for (size_t i = 0; i < type->named_tuple.length; i++) {
			if (i > 0) {
				printf(",\n");
			}
			printf("%.*s : ", ALIT(type->named_tuple.members[i].name));
			scalars_read
				+= print_typed_value_internal(stage, type->named_tuple.members[i].type,
											  &values[scalars_read],
											  num_values - scalars_read);
		}
		printf(")");
		break;

	case TYPE_KIND_ARRAY: {
		printf("[");
		for (size_t i = 0; i < type->array.length; i++) {
			if (i > 0) {
				printf(",\n");
			}
			scalars_read
				+= print_typed_value_internal(stage, type->array.type,
											  &values[scalars_read],
											  num_values - scalars_read);
		}
		printf("]");
	} break;
	}

	return scalars_read;
}

void print_typed_value(struct stage *stage, type_id tid, scalar_value *values, size_t num_values)
{
	print_typed_value_internal(stage, tid, values, num_values);
}

char *humanreadable_type_kind(enum type_kind kind)
{
	switch (kind) {
	case TYPE_KIND_SCALAR:      return "scalar";
	case TYPE_KIND_STRING:      return "string";
	case TYPE_KIND_TYPE:        return "type";
	case TYPE_KIND_TUPLE:       return "tuple";
	case TYPE_KIND_NAMED_TUPLE: return "named tuple";
	case TYPE_KIND_ARRAY:       return "array";
	}
	assert(!"Invalid type kind.");
}
