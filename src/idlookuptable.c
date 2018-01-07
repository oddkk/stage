#include "idlookuptable.h"
#include "murmurhash.h"
#include "utils.h"

#define MURMURHASH_SEED 1

// TODO: Implement rehash function.

int id_lookup_table_insert(struct id_lookup_table *table, struct string name,
			   unsigned int id)
{
	uint32_t hash;
	struct name_id_pair **current;
	struct name_id_pair *new_entry;
	int error;

	if (!table->buckets || table->num_buckets == 0) {
		table->num_buckets = 32;
		table->buckets = arena_alloc(table->page_arena,
					     table->num_buckets *
					     sizeof(struct name_id_pair **));
	}

	hash = murmurhash(name.text, name.length, MURMURHASH_SEED);

	current = &table->buckets[hash % table->num_buckets];

	// Search the table to figure out if the name already exist. If
	// not, current will point to where the new entry should be
	// inserted.
	while (*current && (*current)->hash <= hash) {
		if ((*current)->hash == hash
		    && string_equal(name, (*current)->name)) {
			return -1;
		}
		current = &(*current)->next_in_bucket;
	}

	if (!table->last_page
	    || table->last_page->num_used >= ID_LOOKUP_TABLE_PAGE_CAPACITY) {
		struct id_lookup_table_page *new_page;

		new_page =
		    arena_alloc(table->page_arena,
				sizeof(struct id_lookup_table_page));

		if (!new_page) {
			print_error("id lookup table",
				    "Could not allocate memory for new page.");
			return -1;
		}

		if (table->last_page) {
			table->last_page->next = new_page;
		} else {
			assert(!table->first_page);
			table->first_page = new_page;
		}
		table->last_page = new_page;
	}

	new_entry = &table->last_page->entries[table->last_page->num_used++];

	new_entry->hash = hash;
	new_entry->id = id;

	error = string_duplicate(table->string_arena, &new_entry->name, name);
	if (error) {
		print_error("id lookup table",
			    "Could not allocate memory for name string.");
		return -1;
	}

	new_entry->next_in_bucket = *current;
	*current = new_entry;

	table->count += 1;

	return 0;
}

int id_lookup_table_lookup(struct id_lookup_table *table, struct string name)
{
	uint32_t hash;
	struct name_id_pair **current;

	if (!table->buckets || table->num_buckets == 0) {
		return -1;
	}

	hash = murmurhash(name.text, name.length, MURMURHASH_SEED);

	current = &table->buckets[hash % table->num_buckets];

	while (*current && (*current)->hash <= hash) {
		if ((*current)->hash == hash
		    && string_equal(name, (*current)->name)) {
			return (*current)->id;
		}
		current = &(*current)->next_in_bucket;
	}

	// The name was not found.
	return -1;
}
