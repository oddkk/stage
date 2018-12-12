#ifndef STAGE_VM_H
#define STAGE_VM_H

#include "objstore.h"
#include "scope.h"
#include "atom.h"
#include "arena.h"

struct vm {
	struct objstore store;
	struct scope root_scope;
	struct arena memory;
	struct atom_table atom_table;
};

int vm_init(struct vm *);

#endif
