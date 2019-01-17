#ifndef STAGE_OBJSTORE_H
#define STAGE_OBJSTORE_H

#include "intdef.h"
#include "atom.h"
#include "utils.h"

typedef unsigned int obj_id;
typedef unsigned int type_id;
typedef size_t template_id;

struct vm;
struct type;
struct scope;
struct object;
struct exec_stack;

// TYPE_UNSET
#define TYPE_SUBTYPES_END ((type_id)0)

typedef struct string (*type_repr)(struct vm *vm, struct arena *mem, struct type *);
typedef struct string (*obj_repr)(struct vm *vm, struct arena *mem, struct object *);
typedef type_id (*type_subtypes_iter)(struct vm *vm, struct type *type, size_t *iter);
typedef void (*type_free)(struct vm *vm, struct type *type);
typedef void (*type_eval)(struct vm *, struct exec_stack *, void *);

struct type_base {
	struct string name;
	type_repr repr;
	obj_repr obj_repr;
	type_free free;
	type_eval eval;
	type_subtypes_iter subtypes_iter;

	bool abstract;
};

void type_base_init(struct type_base *, struct string name);

struct type {
	struct atom *name;
	struct type_base *base;
	struct scope *object_scope;
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
void print_type_repr(struct vm *vm, struct type *);
void print_obj_repr(struct vm *vm, struct object);

obj_id register_object(struct objstore *store, struct object obj);

static inline struct object get_object(struct objstore *store, obj_id id) {
	struct object *obj;

	obj = &store->pages[id / store->elements_per_page][id % store->elements_per_page];

	return *obj;
}

static inline struct type *get_type(struct objstore *store, type_id id) {
	struct type *type;

	assert(id < store->num_types);
	type = &store->types[id];

	return type;
}

void free_objstore(struct objstore *store);


#endif
