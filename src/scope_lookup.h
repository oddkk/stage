#ifndef STAGE_SCOPE_LOOKUP_H
#define STAGE_SCOPE_LOOKUP_H

#include "scoped_hash.h"
#include "type.h"

struct stage;

struct scope_lookup_step {
	unsigned int offset;
	unsigned int length;
	unsigned int repetitions;
	unsigned int stride;
};

struct scope_lookup {
	struct scoped_hash *scope;
	struct type *type;

	struct stage *stage;

	bool local_lookup;

	struct scope_lookup_step *steps;
	size_t num_steps;
};

int scope_lookup_ident(struct scope_lookup *, struct atom *name);
int scope_lookup_index(struct scope_lookup *, size_t i);
int scope_lookup_range(struct scope_lookup *, size_t begin, size_t end);

#endif
