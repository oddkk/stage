#ifndef STAGE_SCOPE_H
#define STAGE_SCOPE_H

#include "idlookuptable.h"
#include "atom.h"

struct scope;
typedef unsigned int obj_id;

struct scope_entry {
	obj_id id;
	struct atom *name;
	struct scope *parent;
	struct scope *scope;
};

struct scope {
	struct id_lookup_table lookup;

	struct scope *parent;
	struct scope_entry *entries;
	size_t num_entries;
};

struct scope *scope_push(struct scope *parent);

int scope_insert(struct scope *parent,
				 struct atom *name,
				 struct scope *child_scope);

int scope_local_lookup(struct scope *scope,
					   struct atom *name,
					   struct scope_entry *result);

int scope_lookup(struct scope *scope,
				 struct atom *name,
				 struct scope_entry *result);

#endif
