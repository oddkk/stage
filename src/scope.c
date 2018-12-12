#include "scope.h"
#include "atom.h"
#include "dlist.h"
#include "utils.h"
#include <stdlib.h>

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
				 struct scope *child_scope)
{
	int id;
	struct scope_entry *entry;

	id = dlist_alloc(parent->entries, parent->num_entries);
	entry = &parent->entries[id];

	entry->id = id;
	entry->name = name;
	entry->parent = parent;
	entry->scope = child_scope;

	int err;
	err = id_lookup_table_insert(&parent->lookup, name->name, id);

	if (err) {
		return -1;
	}

	parent->num_entries -= 1;

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
