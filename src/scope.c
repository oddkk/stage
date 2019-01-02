#include "scope.h"
#include "atom.h"
#include "dlist.h"
#include "utils.h"
#include <stdlib.h>
#include <string.h>

struct scope *scope_push(struct scope *parent)
{
	struct scope *scope;

	scope = calloc(1, sizeof(struct scope));

	scope->parent = parent;
	scope->lookup.string_arena = parent->lookup.string_arena;
	scope->lookup.page_arena = parent->lookup.page_arena;

	return scope;
}

int scope_insert(struct scope *parent,
				 struct atom *name,
				 obj_id object_id,
				 struct scope *child_scope)
{
	int id;
	struct scope_entry *entry;

	id = dlist_alloc(parent->entries, parent->num_entries);
	entry = &parent->entries[id];

	entry->id = object_id;
	entry->name = name;
	entry->parent = parent;
	entry->scope = child_scope;
	entry->overloadable = false;

	int err;
	err = id_lookup_table_insert(&parent->lookup, name->name, id);

	if (err) {
		parent->num_entries -= 1;
		return -1;
	}

	return 0;
}

int scope_insert_overloadable(struct scope *parent,
							  struct atom *name,
							  obj_id object_id)
{
	int err;
	err = id_lookup_table_lookup(&parent->lookup, name->name);

	struct scope_entry *entry;

	if (err < 0) {
		int id;

		id = dlist_alloc(parent->entries, parent->num_entries);
		entry = &parent->entries[id];

		entry->overloadable = true;
		entry->name = name;
		entry->parent = parent;
		entry->scope = NULL;

		int err;
		err = id_lookup_table_insert(&parent->lookup, name->name, id);

		// We already tested that the entry did not exist, and it
		// should still not exist.
		assert(err == 0);
	} else {
		entry = &parent->entries[err];
	}

	if (!entry->overloadable) {
		return -3;
	}

	entry->num_overloads += 1;

	if (entry->num_overloads <= SCOPE_ENTRY_INLINED_OVERLOADS) {
		entry->inlined_overloads[entry->num_overloads - 1] = object_id;

		return 0;
	}

	if (entry->num_overloads == SCOPE_ENTRY_INLINED_OVERLOADS + 1) {
		obj_id *overloads = calloc(entry->num_overloads, sizeof(obj_id));
		memcpy(overloads, entry->inlined_overloads,
			   (entry->num_overloads + 1) * sizeof(obj_id));
		entry->overloads = overloads;

	} else {
		entry->overloads = realloc(entry->overloads,
								   entry->num_overloads * sizeof(obj_id));
	}

	entry->overloads[entry->num_overloads - 1] = object_id;

	return 0;
}

int scope_local_lookup(struct scope *scope,
					   struct atom *name,
					   struct scope_entry *result)
{
	int err;

	assert(name);

	err = id_lookup_table_lookup(&scope->lookup, name->name);
	if (err < 0) {
		return -1;
	}

	*result = scope->entries[err];

	return 0;
}

int scope_lookup(struct scope *scope,
				 struct atom *name,
				 struct scope_entry *result)
{
	int err;

	err = scope_local_lookup(scope, name, result);
	if (err < 0) {
		if (scope->parent) {
			return scope_lookup(scope->parent, name, result);
		} else {
			return err;
		}
	}

	return 0;
}

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

#include "vm.h"

static void _scope_print(struct vm *vm, struct scope *scope, int depth)
{
	for (size_t i = 0; i < scope->num_entries; i++) {
		struct scope_entry *entry = &scope->entries[i];
		_print_indent(depth);
		printf("%.*s", ALIT(entry->name));

		obj_id *objs = NULL;
		size_t num_objs = 0;

		num_objs = scope_objects(*entry, &objs);

		for (size_t i = 0; i < num_objs; i++) {
			printf(" ");
			print_type_repr(vm, get_object(&vm->store, objs[i]));
		}

		printf("\n");

		if (entry->scope) {
			_scope_print(vm, entry->scope, depth + 1);
		}
	}
}

void scope_print(struct vm *vm, struct scope *scope)
{
	printf("root\n");
	_scope_print(vm, scope, 1);
}

obj_id scope_lookup_id(struct scope *scope,
					   struct atom *name)
{
	struct scope_entry res;
	int err;
	err = scope_lookup(scope, name, &res);

	if (err) {
		return OBJ_NONE;
	}

	return res.id;
}

size_t scope_objects(struct scope_entry entry, obj_id **objs)
{
	size_t num_objs = 0;

	if (entry.overloadable) {
		num_objs = entry.num_overloads;

		if (entry.num_overloads > SCOPE_ENTRY_INLINED_OVERLOADS) {
			*objs = entry.overloads;
		} else {
			*objs = entry.inlined_overloads;
		}
	} else if (entry.id != OBJ_NONE) {
		num_objs = 1;
		*objs = &entry.id;
	} else {
		num_objs = 0;
		*objs = NULL;
	}

	return num_objs;
}
