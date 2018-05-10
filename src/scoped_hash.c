#include "scoped_hash.h"
#include "dlist.h"
#include "atom.h"
#include "utils.h"

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

	assert(name);

	err = id_lookup_table_lookup(&scope->lookup, name->name);
	if (err < 0) {
		if (scope->instance) {
			return scoped_hash_local_lookup(scope->instance, name, result);
		}

		return -1;
	}

	*result = scope->entries[err];

	return 0;
}

int scoped_hash_lookup_owner(struct scoped_hash *scope, struct atom *name,
							 struct scope_entry *result, struct scoped_hash **owner)
{
	int err;

	err = scoped_hash_local_lookup(scope, name, result);

	if (err < 0) {
		if (scope->parent) {
			return scoped_hash_lookup_owner(scope->parent, name, result, owner);
		} else {
			return err;
		}
	}

	if (owner) {
		*owner = scope;
	}

	return 0;
}

int scoped_hash_lookup(struct scoped_hash *scope, struct atom *name,
					   struct scope_entry *result)
{
	return scoped_hash_lookup_owner(scope, name, result, NULL);
}

struct scoped_hash *scoped_hash_push(struct scoped_hash *parent, enum scope_entry_kind kind, int id)
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
	new_child->kind = kind;
	new_child->id = id;

	dlist_append(parent->children, parent->num_children, &new_child);
	child_id = parent->num_children - 1;

	return new_child;
}

