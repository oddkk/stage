#include "scoped_hash.h"
#include "dlist.h"
#include "atom.h"

int scoped_hash_insert(struct scoped_hash *scope, struct atom *name, enum scope_entry_kind kind, int id, struct config_node *node, struct scoped_hash *child_scope)
{
	struct scope_entry new_entry;
	int entry_id;
	int err;

	new_entry.name = name;
	new_entry.kind = kind;
	new_entry.id = id;
	new_entry.scope = child_scope;

	err = dlist_append(scope->entries, scope->num_entries, &new_entry);
	if (err < 0) {
		return -1;
	}
	entry_id = scope->num_entries - 1;

	err = id_lookup_table_insert(&scope->lookup, name->name, entry_id);
	if (err < 0) {
		return -2;
	}

	return 0;
}

int scoped_hash_local_lookup(struct scoped_hash *scope, struct atom *name, struct scope_entry *result)
{
	int err;

	err = id_lookup_table_lookup(&scope->lookup, name->name);
	if (err < 0) {
		return -1;
	}

	*result = scope->entries[err];

	return 0;
}

int scoped_hash_lookup(struct scoped_hash *scope, struct atom *name, struct scope_entry *result)
{
	int err;

	err = scoped_hash_local_lookup(scope, name, result);

	if (err < 0) {
		if (scope->parent) {
			return scoped_hash_lookup(scope->parent, name, result);
		} else {
			return err;
		}
	}

	return 0;
}

int instanced_scoped_hash_lookup(struct instanced_scoped_hash scope, struct atom *name, struct scope_entry *result)
{
	int err;

	err = scoped_hash_lookup(scope.self, name, result);
	if (err < 0) {
		if (scope.parent) {
			return scoped_hash_local_lookup(scope.parent, name, result);
		} else {
			return err;
		}
	}

	return 0;
}

int instanced_scoped_hash_local_lookup(struct instanced_scoped_hash scope, struct atom *name, struct scope_entry *result)
{
	int err;

	err = scoped_hash_local_lookup(scope.self, name, result);

	return err;
}

struct scoped_hash *scoped_hash_push(struct scoped_hash *parent)
{
	struct scoped_hash *new_child;
	int child_id;

	// TODO: Make this use a more suited allocator?
	new_child = arena_alloc(parent->lookup.page_arena, sizeof(struct scoped_hash));

	if (!new_child) {
		return 0;
	}

	new_child->parent = parent;
	new_child->lookup.string_arena = parent->lookup.string_arena;
	new_child->lookup.page_arena = parent->lookup.page_arena;

	dlist_append(parent->children, parent->num_children, &new_child);
	child_id = parent->num_children - 1;

	return new_child;
}

