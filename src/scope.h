#ifndef STAGE_SCOPE_H
#define STAGE_SCOPE_H

#include "idlookuptable.h"
#include "atom.h"

struct vm;
struct objstore;
struct scope;
typedef unsigned int obj_id;

#define SCOPE_ENTRY_INLINED_OVERLOADS (sizeof(obj_id *) / sizeof(obj_id))

struct scope_entry {
	union {
		// For non-overloadable entries.
		obj_id id;

		// For overloadable entries.
		obj_id inlined_overloads[SCOPE_ENTRY_INLINED_OVERLOADS];
		obj_id *overloads;
	};
	size_t num_overloads;
	bool overloadable;

	struct atom *name;
	struct scope *parent;
	struct scope *scope;
};

struct scope {
	struct id_lookup_table lookup;

	struct scope *parent;
	struct scope_entry *entries;
	size_t num_entries;

	struct objstore *store;
};

struct scope *scope_push(struct scope *parent);

int scope_insert(struct scope *parent,
				 struct atom *name,
				 obj_id id,
				 struct scope *child_scope);

int scope_insert_overloadable(struct scope *parent,
							  struct atom *name,
							  obj_id id);

int scope_local_lookup(struct scope *scope,
					   struct atom *name,
					   struct scope_entry *result);

int scope_lookup(struct scope *scope,
				 struct atom *name,
				 struct scope_entry *result);

obj_id scope_lookup_id(struct scope *scope,
					   struct atom *name);

void scope_print(struct vm *vm, struct scope *scope);

size_t scope_objects(struct scope_entry entry, obj_id **objs);

#endif
