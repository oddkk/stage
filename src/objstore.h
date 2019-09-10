#ifndef STAGE_OBJSTORE_H
#define STAGE_OBJSTORE_H

#include "intdef.h"
#include "atom.h"
#include "utils.h"

typedef unsigned int obj_id;
typedef uint64_t type_id;
typedef uint32_t modtype_id;
typedef size_t template_id;

#define TYPE_ID(modid, typeid) (((type_id)modid << 32) | ((type_id)typeid & 0xffffffff))
#define TYPE_ID_MOD(typeid) (uint32_t)((typeid >> 32) & 0xffffffff)
#define TYPE_ID_TYPE(typeid) (modtype_id)(typeid & 0xffffffff)

struct vm;
struct type;
struct scope;
struct object;
struct objstore;
struct exec_stack;
struct stg_module;

// TYPE_UNSET
#define TYPE_SUBTYPES_END ((type_id)0)

typedef struct string (*type_repr)(struct vm *vm, struct arena *mem, struct type *);
typedef struct string (*obj_repr)(struct vm *vm, struct arena *mem, struct object *);
typedef void (*type_free)(struct vm *vm, struct type *type);

struct type_base {
	struct string name;
	type_repr repr;
	obj_repr obj_repr;
	type_free free;
};

struct type {
	struct atom *name;
	struct type_base *base;
	void *data;

	size_t size;
};

struct type
_init_plain_type(struct type_base *, struct atom *name, size_t size);

#define init_plain_type(base, name, datatype) \
	_init_plain_type(base, name, sizeof(datatype))

struct object {
	type_id type;
	void *data;
};

struct objstore {
	size_t page_size;
	uint32_t mod_id;

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
bool unify_types(struct vm *vm, struct objstore *, type_id lhs, type_id rhs, type_id *out);

void print_type_repr(struct vm *vm, struct type *);
void print_obj_repr(struct vm *vm, struct object);

struct object
register_object(struct vm *, struct objstore *store, struct object obj);

static inline struct object get_object(struct objstore *store, obj_id id) {
	struct object *obj;

	obj = &store->pages[id / store->elements_per_page][id % store->elements_per_page];

	return *obj;
}

static inline struct type *store_get_type(struct objstore *store, type_id id) {
	struct type *type;

	assert(id < store->num_types);
	type = &store->types[id];

	return type;
}

void free_objstore(struct objstore *store);

void
arena_string_append_type_repr(struct string *str, struct vm *vm,
							  struct arena *mem, struct type *type);

void
arena_string_append_obj_repr(struct string *str, struct vm *vm,
							 struct arena *mem, struct object *object);

#endif
