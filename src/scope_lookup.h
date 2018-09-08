#ifndef STAGE_SCOPE_LOOKUP_H
#define STAGE_SCOPE_LOOKUP_H

#include "scoped_hash.h"
#include "type.h"
#include "access_pattern.h"
#include <stdio.h>

struct stage;

struct scope_lookup_step {
	unsigned int offset;
	unsigned int length;
	unsigned int repetitions;
	unsigned int stride;
};

struct scope_lookup {
	enum scope_entry_kind hint;
	bool hint_channel_input;
	enum scope_entry_kind kind;
	struct scoped_hash *scope;
	struct type *type;

	struct stage *stage;

	bool local_lookup;

	int owner;

	struct scope_lookup_step *steps;
	size_t num_steps;
};

struct scope_lookup scope_lookup_init(struct stage *, struct scoped_hash *root_scope);

#define SCOPE_LOOKUP_RANGE_END (size_t)(-1)

int scope_lookup_ident(struct scope_lookup *, struct atom *name);
int scope_lookup_index(struct scope_lookup *, size_t i);
int scope_lookup_range(struct scope_lookup *, size_t begin, size_t end);
int scope_lookup_pattern(struct scope_lookup *, struct access_pattern);

struct scope_lookup_range {
	enum scope_entry_kind kind;
	struct type *type;
	size_t begin;
	size_t length;
	int owner;
};

int scope_lookup_result_single(struct scope_lookup ctx, struct scope_lookup_range *result);

#define LOOKUP_FOUND 0
#define LOOKUP_END 1
#define LOOKUP_NOT_FOUND -1

int scope_lookup_iterate(struct scope_lookup ctx, size_t *iter,
						 struct scope_lookup_range *out);

size_t scope_lookup_instances(struct scope_lookup);
size_t scope_lookup_instance_size(struct scope_lookup);

int eval_lookup_result(struct stage *stage, struct scope_lookup_range, struct value_ref *out);

void print_steps(struct scope_lookup ctx);

void describe_lookup_result_type(FILE *fp, struct scope_lookup ctx);

#endif
