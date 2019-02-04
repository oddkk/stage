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
				 enum scope_object_anchor anchor,
				 struct object object,
				 struct scope *child_scope)
{
	int id;
	struct scope_entry *entry;

	id = dlist_alloc(parent->entries, parent->num_entries);
	entry = &parent->entries[id];

	entry->object = object;
	entry->anchor = anchor;
	entry->name = name;
	entry->parent = parent;
	entry->scope = child_scope;
	entry->overloadable = false;
	entry->next_overload = -1;

	int err;
	err = id_lookup_table_insert(&parent->lookup, name->name, id);

	if (err) {
		parent->num_entries -= 1;
		return -1;
	}

	return id;
}

int scope_insert_overloadable(struct scope *parent,
							  struct atom *name,
							  enum scope_object_anchor anchor,
							  struct object object)
{
	int err;
	err = id_lookup_table_lookup(&parent->lookup, name->name);

	struct scope_entry *previous_overload = NULL;

	int id;
	struct scope_entry *entry;

	id = dlist_alloc(parent->entries, parent->num_entries);
	entry = &parent->entries[id];

	if (err >= 0) {
		previous_overload = &parent->entries[err];

		if (!previous_overload->overloadable) {
			printf("%.*s not overloadable\n", ALIT(name));
			parent->num_entries -= 1;
			return -3;
		}

		while (previous_overload->next_overload >= 0) {
			previous_overload = &parent->entries[previous_overload->next_overload];
		}
	}

	entry->overloadable = true;
	entry->name = name;
	entry->parent = parent;
	entry->scope = NULL;
	entry->next_overload = -1;

	entry->object = object;
	entry->anchor = anchor;

	if (previous_overload) {
		previous_overload->next_overload = id;
	}

	if (err < 0) {
		// The element did not exist in this scope.
		err = id_lookup_table_insert(&parent->lookup, name->name, id);
	}

	return id;
}

void scope_use(struct scope *target, struct scope *other)
{
	dlist_append(target->used_scopes, target->num_used_scopes, &other);
}

int scope_local_lookup(struct scope *scope,
					   struct atom *name,
					   struct scope_entry *result)
{
	int err;

	assert(name);

	err = id_lookup_table_lookup(&scope->lookup, name->name);
	if (err < 0) {
		for (size_t i = 0; i < scope->num_used_scopes; i++) {
			err = scope_local_lookup(scope->used_scopes[i], name, result);
			if (err >= 0) {
				return 0;
			}
		}
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
#include "modules/base/mod.h"

static void _scope_print(struct vm *vm, struct scope *scope, int depth)
{
	for (size_t i = 0; i < scope->num_entries; i++) {
		struct scope_entry *entry = &scope->entries[i];
		_print_indent(depth);
		printf("%.*s ", ALIT(entry->name));
		print_obj_repr(vm, entry->object);
		printf("\n");

		if (entry->object.type == vm->default_types.type) {
			type_id tid = type_obj_get(vm, entry->object);
			struct type *type = vm_get_type(vm, tid);

			if (type->object_scope) {
				_print_indent(depth + 1);
				printf("object scope:\n");
				_scope_print(vm, type->object_scope, depth + 2);
			}
		}

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

/* obj_id scope_lookup_id(struct scope *scope, */
/* 					   struct atom *name) */
/* { */
/* 	struct scope_entry res; */
/* 	int err; */
/* 	err = scope_lookup(scope, name, &res); */

/* 	if (err) { */
/* 		return OBJ_NONE; */
/* 	} */

/* 	return res.id; */
/* } */

int
scope_iterate_overloads(struct scope *scope, struct atom *name,
						struct scope_iter *iter)
{
	int err = 1;

	if (!iter->scope) {
		iter->scope = scope;
		iter->next_use_index = 0;
		iter->entry = NULL;
	}

	while (iter->scope && err != 0) {
		err = scope_iterate_local_overloads(iter->scope, name, iter);

		if (err == 0) {
			return 0;
		}

		iter->scope = iter->scope->parent;
		iter->next_use_index = 0;
		iter->entry = NULL;
	}

	return -1;
}

int
scope_iterate_local_overloads(struct scope *scope, struct atom *name,
							  struct scope_iter *iter)
{
	assert(scope != NULL);

	if (!iter->scope) {
		iter->scope = scope;
	}

	if (iter->entry == NULL) {
		int err;
		err = id_lookup_table_lookup(&scope->lookup, name->name);

		if (err >= 0) {
			iter->entry = &scope->entries[err];
			return 0;
		}
	} else {
		struct scope_entry *entry = iter->entry;
		struct scope *parent = entry->parent;

		if (entry->next_overload >= 0) {
			assert(entry->next_overload < parent->num_entries);
			iter->entry = &parent->entries[entry->next_overload];

			return 0;
		}
	}


	while (iter->next_use_index < iter->scope->num_used_scopes) {
		iter->entry = NULL;

		struct scope *next_use_scope =
			iter->scope->used_scopes[iter->next_use_index];

		iter->next_use_index += 1;

		int err;
		err = scope_iterate_local_overloads(next_use_scope, name, iter);

		if (err == 0) {
			return 0;
		}
	}

	iter->entry = NULL;
	return 1;
}
