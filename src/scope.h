#ifndef STAGE_SCOPE_H
#define STAGE_SCOPE_H

#include "idlookuptable.h"
#include "atom.h"

struct vm;
struct objstore;
struct scope;
typedef unsigned int obj_id;

struct scope_entry {
	obj_id id;
	int next_overload;
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

// Returns 0 if found overload, in this case *iter is a pointer to
// that element. Returns -1 if no element was found, and 1 if an
// element previously was found, but there are no more matches.
int scope_iterate_overloads(struct scope *scope, struct atom *name,
							struct scope_entry **iter);

int scope_iterate_local_overloads(struct scope *scope, struct atom *name,
								  struct scope_entry **iter);

#endif
