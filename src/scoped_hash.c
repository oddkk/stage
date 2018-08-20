#include "scoped_hash.h"
#include "dlist.h"
#include "atom.h"
#include "utils.h"

struct scope_entry *scoped_hash_insert(struct scoped_hash *scope, struct atom *name,
									   enum scope_entry_kind kind,
									   struct config_node *node,
									   struct scoped_hash *child_scope)
{
	struct scope_entry new_entry = {0};
	int entry_id;
	int err;

	new_entry.name = name;
	new_entry.kind = kind;
	new_entry.scope = child_scope;

	err = dlist_append(scope->entries, scope->num_entries, &new_entry);
	if (err < 0) {
		return NULL;
	}
	entry_id = scope->num_entries - 1;


	if (child_scope) {
		child_scope->entry_id = entry_id;
	}

	if (name) {
		err = id_lookup_table_insert(&scope->lookup, name->name, entry_id);
		if (err < 0) {
			return NULL;
		}
	} else if (!scope->array) {
		print_error("scoped hash insert", "Missing name for entry.");
		return NULL;
	}

	return &scope->entries[entry_id];
}

struct scoped_hash *scoped_hash_namespace(struct scoped_hash *parent,
										  struct atom *name)
{
	struct scoped_hash *child_scope;
	int err;

	struct scope_entry entry;
	err = scoped_hash_local_lookup(parent, name, &entry);

	if (!err) {
		if (entry.kind != SCOPE_ENTRY_NAMESPACE) {
			printf("'%.*s' is not a namespace.\n", ALIT(name));
			return NULL;
		}

		return entry.scope;
	}

	child_scope = scoped_hash_push(parent, SCOPE_ENTRY_NAMESPACE, 0);

	struct scope_entry *res;
	res = scoped_hash_insert(parent, name, SCOPE_ENTRY_NAMESPACE, NULL, child_scope);
	if (!res) {
		return NULL;
	}

	return child_scope;
}

int scoped_hash_local_lookup(struct scoped_hash *scope, struct atom *name,
			     struct scope_entry *result)
{
	int err;

	assert(name);

	err = id_lookup_table_lookup(&scope->lookup, name->name);
	if (err < 0) {
		if (scope->instance) {
			return scoped_hash_local_lookup(scope->instance, name,
							result);
		}

		return -1;
	}

	*result = scope->entries[err];

	return 0;
}

int scoped_hash_lookup_owner(struct scoped_hash *scope, struct atom *name,
			     struct scope_entry *result,
			     struct scoped_hash **owner)
{
	int err;

	err = scoped_hash_local_lookup(scope, name, result);

	if (err < 0) {
		if (scope->parent) {
			return scoped_hash_lookup_owner(scope->parent, name,
							result, owner);
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

int scoped_hash_lookup_index(struct scoped_hash *scope, size_t i, struct scope_entry *result)
{
	if (!scope->array) {
		return -1;
	}

	if (i >= scope->num_entries) {
		return -2;
	}

	*result = scope->entries[i];
	return 0;
}

struct scoped_hash *scoped_hash_push(struct scoped_hash *parent,
				     enum scope_entry_kind kind, int id)
{
	struct scoped_hash *new_child;
	int child_id;

	// TODO: Make this use a more suited allocator?
	new_child =
	    arena_alloc(parent->lookup.page_arena, sizeof(struct scoped_hash));

	if (!new_child) {
		printf("Could not allocate new scoped hash. Out of memory.\n");
		return 0;
	}

	new_child->parent = parent;
	new_child->lookup.string_arena = parent->lookup.string_arena;
	new_child->lookup.page_arena = parent->lookup.page_arena;
	new_child->kind = kind;
	new_child->id = id;
	new_child->entry_id = -1;

	dlist_append(parent->children, parent->num_children, &new_child);
	child_id = parent->num_children - 1;

	return new_child;
}

void scoped_hash_print(struct scoped_hash *hash, int indent)
{
	for (size_t i = 0; i < hash->num_entries; i++) {
		for (int pad = 0; pad < indent; pad++) {
			printf("  ");
		}
		printf("%.*s\n", ALIT(hash->entries[i].name));
		if (hash->entries[i].scope) {
			scoped_hash_print(hash->entries[i].scope, indent + 1);
		}
	}
}

char *humanreadable_scope_entry(enum scope_entry_kind kind)
{
	switch (kind) {
	case SCOPE_ENTRY_NONE: return "(none)";
	case SCOPE_ENTRY_NAMESPACE: return "namespace";
	case SCOPE_ENTRY_TYPE: return "type";
	case SCOPE_ENTRY_DEVICE: return "device";
	case SCOPE_ENTRY_DEVICE_TYPE: return "device type";
	case SCOPE_ENTRY_DEVICE_CHANNEL: return "device channel";
	/* case SCOPE_ENTRY_DEVICE_INPUT: return "device input"; */
	/* case SCOPE_ENTRY_DEVICE_OUTPUT: return "device output"; */
	case SCOPE_ENTRY_DEVICE_ATTRIBUTE: return "device attribute";
	}
}
