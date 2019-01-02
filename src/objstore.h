#ifndef STAGE_OBJSTORE_H
#define STAGE_OBJSTORE_H

#include "intdef.h"
#include "atom.h"

typedef unsigned int obj_id;
typedef unsigned int type_id;
typedef size_t template_id;

struct vm;
struct type;
struct object;

// TYPE_UNSET
#define TYPE_SUBTYPES_END ((type_id)0)

typedef struct string (*type_repr)(struct vm *vm, struct arena *mem, struct object *obj);
typedef type_id (*type_subtypes_iter)(struct vm *vm, struct type *type, size_t *iter);
typedef void (*type_free)(struct vm *vm, struct type *type);

struct type_base {
	struct string name;
	type_repr repr;
	type_free free;
	type_subtypes_iter subtypes_iter;
};

struct type {
	struct atom *name;
	struct type_base *base;
	void *data;

	size_t size;

	int num_template_params;
};

struct object {
	type_id type;
	void *data;
};

struct objstore {
	size_t page_size;

	struct object **pages;
	size_t num_pages;
	size_t elements_per_page;
	size_t last_page_num_used;

	uint8_t **data_pages;
	size_t num_data_pages;
	size_t last_data_page_used;

	// TODO: Better data structure?
	struct type *types;
	size_t num_types;
};

type_id register_type(struct objstore *store, struct type type);
void print_type_repr(struct vm *vm, struct object obj);

obj_id register_object(struct objstore *store, struct object obj);

static inline struct object get_object(struct objstore *store, obj_id id) {
	struct object *obj;

	obj = &store->pages[id / store->elements_per_page][id % store->elements_per_page];

	return *obj;
}

void free_objstore(struct objstore *store);


#endif
