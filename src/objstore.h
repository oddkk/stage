#ifndef STAGE_OBJSTORE_H
#define STAGE_OBJSTORE_H

#include "intdef.h"
#include "atom.h"
#include "utils.h"

typedef unsigned int obj_id;
typedef uint64_t type_id;
typedef uint32_t modtype_id;
typedef uint64_t func_id;
typedef uint32_t modfunc_id;
typedef size_t template_id;

#define TYPE_ID(modid, typeid) (((type_id)modid << 32) | ((type_id)typeid & 0xffffffff))
#define TYPE_ID_MOD(typeid) (uint32_t)((typeid >> 32) & 0xffffffff)
#define TYPE_ID_TYPE(typeid) (modtype_id)(typeid & 0xffffffff)

#define FUNC_ID(modid, funcid) (((func_id)modid << 32) | ((func_id)funcid & 0xffffffff))
#define FUNC_ID_MOD(funcid) (uint32_t)((funcid >> 32) & 0xffffffff)
#define FUNC_ID_TYPE(funcid) (modfunc_id)(funcid & 0xffffffff)

#define FUNC_UNSET ((func_id)0)

struct vm;
struct type;
struct scope;
struct object;
struct objstore;
struct stg_module;
struct ast_array_def;
struct ast_object_def;

// TYPE_UNSET
#define TYPE_SUBTYPES_END ((type_id)0)

typedef struct string (*type_repr)(struct vm *vm, struct arena *mem, struct type *);
typedef struct string (*obj_repr)(struct vm *vm, struct arena *mem, struct object *);
typedef bool (*type_equals_func)(struct vm *vm, struct type *lhs, struct type *rhs);
typedef void (*type_free)(struct vm *vm, struct type *type);


struct type_base {
	struct string name;
	type_repr repr;
	obj_repr obj_repr;
	type_free free;
	type_equals_func equals;

	struct ast_array_def *array_def;
};

typedef struct _ffi_type ffi_type;

struct type {
	struct atom *name;
	struct type_base *base;
	struct ast_object_def *obj_def;
	struct ast_object_def *type_def;
	ffi_type *ffi_type;
	void *data;

	size_t size;
};

struct type
_init_plain_type(struct type_base *, struct atom *name, size_t size);

#define init_plain_type(base, name, datatype) \
	_init_plain_type(base, name, sizeof(datatype))

bool
type_equals(struct vm *, type_id lhs, type_id rhs);

void
_assert_type_equals_failed(struct vm *, type_id lhs, type_id rhs,
		const char *file, int line, const char *func);

#define assert_type_equals(vm, lhs, rhs) \
	do { if (!type_equals((vm), (lhs), (rhs))) { \
		_assert_type_equals_failed((vm), (lhs), (rhs), __FILE__, __LINE__, __func__); \
	} } while (0)

enum func_kind {
	FUNC_NATIVE,
};

struct func {
	struct atom *name;
	enum func_kind kind;
	type_id type;

	union {
		void *native;
	};
};

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

	// TODO: Better data structure?
	struct func *funcs;
	size_t num_funcs;
};

modtype_id
store_register_type(struct objstore *store, struct type type);

modfunc_id
store_register_func(struct objstore *store, struct func func);

type_id
func_return_type(struct vm *, type_id func_type);

size_t
func_num_params(struct vm *, type_id func_type);

type_id
func_param_type(struct vm *, type_id func_type, size_t param_i);

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

static inline struct func *store_get_func(struct objstore *store, func_id id) {
	struct func *func;

	assert(id < store->num_funcs);
	func = &store->funcs[id];

	return func;
}

void free_objstore(struct objstore *store);

void
arena_string_append_type_repr(struct string *str, struct vm *vm,
							  struct arena *mem, struct type *type);

void
arena_string_append_obj_repr(struct string *str, struct vm *vm,
							 struct arena *mem, struct object *object);

#endif
