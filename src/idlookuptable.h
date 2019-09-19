#ifndef STAGE_IDLOOKUPTABLE_H
#define STAGE_IDLOOKUPTABLE_H

#include "intdef.h"
#include "arena.h"
#include "str.h"
#define ID_LOOKUP_TABLE_PAGE_CAPACITY (20)

struct name_id_pair {
	int id;
	uint32_t hash;
	struct string name;
	struct name_id_pair *next_in_bucket;
};

struct id_lookup_table_page {
	struct id_lookup_table_page *next;
	size_t num_used;
	struct name_id_pair entries[ID_LOOKUP_TABLE_PAGE_CAPACITY];
};

struct id_lookup_table {
	struct arena *string_arena;
	struct arena *page_arena;
	struct id_lookup_table_page *first_page;
	struct id_lookup_table_page *last_page;

	size_t num_buckets;
	struct name_id_pair **buckets;

	size_t count;
};

/* Attempts to associate name with id.

   Returns 0 if the entry was inserted, -1 if the name already
   appears in the table.
 */
int id_lookup_table_insert(struct id_lookup_table *table, struct string name,
			   unsigned int id);

/* Attempts to lookup the name and return the id associated with it.

   Returns the positive id if the entry was found, or -1 if the name
   did not appear in the table.
 */
int id_lookup_table_lookup(struct id_lookup_table *table, struct string name);

#endif
