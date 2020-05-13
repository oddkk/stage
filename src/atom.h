#ifndef STAGE_ATOM_H
#define STAGE_ATOM_H

#include "str.h"
#include "arena.h"

struct atom {
	stg_hash hash;
	struct string name;
	struct atom *next_in_bucket;
};

struct atom_page;
struct atom_table {
	struct arena *string_arena;
	struct atom_page *first_page;
	struct atom_page *last_page;

	size_t count;

	size_t num_buckets;
	struct atom **buckets;
};

struct atom *atom_create(struct atom_table *table, struct string name);

void atom_table_rehash(struct atom_table *table, size_t new_num_buckets);

void atom_table_print(struct atom_table *table);

void atom_table_destroy(struct atom_table *table);

#define ALIT_NONE_LABEL "(none)"

#define ALIT(atom) (atom ? (int)(atom)->name.length : (int)(sizeof(ALIT_NONE_LABEL)-1)), (char*)(atom ? (atom)->name.text : ALIT_NONE_LABEL)

#endif
