#include "atom.h"
#include "utils.h"
#include "murmurhash.h"
#include <stdlib.h>

#define atom_page_capacity (10)

struct atom_page {
	struct atom_page *next;
	size_t num_used;
	struct atom atoms[atom_page_capacity];
};

static void atom_table_insert(struct atom_table *table, struct atom *new_atom)
{
	struct atom **current;

	current = &table->buckets[new_atom->hash % table->num_buckets];

	while (*current && (*current)->hash <= new_atom->hash) {
		if ((*current)->hash == new_atom->hash) {
			if (string_equal(new_atom->name, (*current)->name)) {
				break;
			}
		}
		current = &(*current)->next_in_bucket;
	}

	new_atom->next_in_bucket = *current;
	*current = new_atom;
}

void atom_table_rehash(struct atom_table *table, size_t new_num_buckets)
{
	if (new_num_buckets == table->num_buckets) {
		return;
	}

	assert(new_num_buckets > table->num_buckets);

	struct atom **tmp_buckets = table->buckets;
	size_t tmp_num_buckets = table->num_buckets;

	table->num_buckets = new_num_buckets;
	table->buckets = calloc(table->num_buckets, sizeof(struct atom *));

	if (tmp_num_buckets == 0 && !tmp_buckets) {
		return;
	}

	for (size_t i = 0; i < tmp_num_buckets; ++i) {
		struct atom *current = tmp_buckets[i];
		while (current) {
			struct atom *next = current->next_in_bucket;
			atom_table_insert(table, current);
			current = next;
		}
	}
}

struct atom *atom_create(struct atom_table *table, struct string name)
{
	assert(table);
	assert(name.text);
	uint32_t name_hash = murmurhash(name.text, name.length, 0);
	struct atom **current;

	current = &table->buckets[name_hash % table->num_buckets];

	while (*current && (*current)->hash <= name_hash) {
		if ((*current)->hash == name_hash) {
			if (string_equal(name, (*current)->name)) {
				return *current;
			}
		}
		current = &(*current)->next_in_bucket;
	}

	// NOTE: The atom did not already exist. Append atom to bucket.

	// NOTE: Get a new atom from the last atom page. If the last page
	// is full, allocate a new one.
	struct atom *new_atom;
	if (!table->last_page
	    || table->last_page->num_used >= atom_page_capacity) {
		struct atom_page *new_page =
		    calloc(1, sizeof(struct atom_page));

		if (!new_page) {
			fprintf(stderr, "Could not allocate new atom page.\n");
			return NULL;
		}

		if (table->last_page) {
			table->last_page->next = new_page;
		} else {
			assert(!table->first_page);
			table->first_page = new_page;
		}
		table->last_page = new_page;
	}
	new_atom = &table->last_page->atoms[table->last_page->num_used++];

	new_atom->hash = name_hash;
	string_duplicate(table->string_arena, &new_atom->name, name);

	new_atom->next_in_bucket = *current;
	*current = new_atom;

	++table->count;

	return new_atom;
}

void atom_table_print(struct atom_table *table)
{
	for (int i = 0; i < table->num_buckets; ++i) {
		struct atom *current = table->buckets[i];

		while (current) {
			printf("%.*s\n", LIT(current->name));
			current = current->next_in_bucket;
		}
	}
}
