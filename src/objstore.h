#ifndef STAGE_OBJSTORE_H
#define STAGE_OBJSTORE_H

#include "intdef.h"
#include "atom.h"

typedef unsigned int obj_id;

struct object_scalar {
	int64_t value;
};

struct object_array {
	obj_id type;
	obj_id first_member;
};

struct object_tuple {
	obj_id type;
	obj_id first_member;
};

struct object_tuple_type {
	size_t num_members;
	struct atom **names;
	obj_id *types;
};


struct object {
	obj_id type;
	void *data;
};

struct objstore {
	struct object **pages;
	size_t num_pages;
	size_t page_size;
	size_t elements_per_page;
	size_t last_page_num_used;
};

obj_id register_object(struct objstore *store, struct object obj);

static inline struct object *get_object(struct objstore *store, obj_id id) {
	return &store->pages[id / store->elements_per_page][id % store->elements_per_page];
}

void free_objstore(struct objstore *store);

#endif
