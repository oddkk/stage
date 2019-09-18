#include "objstore.h"
#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>
#include "dlist.h"
#include "utils.h"
#include "vm.h"
#include "base/mod.h"

static struct string default_type_repr(struct vm *vm, struct arena *mem,
									   struct type *type)
{
	return type->base->name;
}

static struct string default_obj_repr(struct vm *vm, struct arena *mem,
									  struct object *object)
{
	struct type *type = vm_get_type(vm, object->type);
	return type->base->name;
}

struct type
_init_plain_type(struct type_base *base, struct atom *name, size_t size)
{
	struct type t = {0};

	t.name = name;
	t.base = base;
	t.size = size;

	return t;
}

bool
type_equals(struct vm *vm, type_id lhs_id, type_id rhs_id)
{
	if (lhs_id == rhs_id) {
		return true;
	}

	struct type *lhs = vm_get_type(vm, lhs_id);
	struct type *rhs = vm_get_type(vm, rhs_id);

	return (
		lhs->base == rhs->base &&
		lhs->base->equals &&
		lhs->base->equals(vm, lhs, rhs)
	);
}

void
_assert_type_equals_failed(struct vm *vm, type_id lhs, type_id rhs,
		const char *file, int line, const char *func)
{
	printf("stage: %s:%i: %s: Assertion type_equals(<",
			file, line, func);
	print_type_repr(vm, vm_get_type(vm, lhs));
	printf(">, <");
	print_type_repr(vm, vm_get_type(vm, rhs));
	printf(">) failed.\n");
	abort();
}

struct object
register_object(struct vm *vm, struct objstore *store, struct object obj) {
	if (store->page_size == 0) {
		store->page_size = sysconf(_SC_PAGESIZE);
		store->elements_per_page = store->page_size / sizeof(struct object);
	}

	if (store->last_page_num_used == 0 ||
		store->last_page_num_used >= store->elements_per_page) {
		size_t new_size = (store->num_pages + 1) * sizeof(struct object *);
		struct object **new_pages = realloc(store->pages, new_size);
		if (!new_pages) {
			perror("realloc");
			return OBJ_NONE;
		}

		new_pages[store->num_pages] = mmap(NULL, store->page_size,
										   PROT_READ|PROT_WRITE,
										   MAP_PRIVATE|MAP_ANONYMOUS,
										   -1, 0);

		if (new_pages[store->num_pages] == MAP_FAILED) {
			perror("mmap");
			return OBJ_NONE;
		}

		store->pages = new_pages;
		store->num_pages = store->num_pages + 1;
		store->last_page_num_used = 0;
	}

	obj_id id = (store->num_pages - 1) * store->elements_per_page
		+ store->last_page_num_used;

	store->last_page_num_used += 1;

	struct type *type = vm_get_type(vm, obj.type);

	if (store->data_pages == NULL ||
		store->last_page_num_used + type->size >= store->page_size) {
		size_t new_size = (store->num_data_pages + 1) * sizeof(uint8_t *);
		uint8_t **new_pages = realloc(store->data_pages, new_size);

		if (!new_pages) {
			perror("realloc");
			return OBJ_NONE;
		}

		new_pages[store->num_data_pages] = mmap(NULL, store->page_size,
										   PROT_READ|PROT_WRITE,
										   MAP_PRIVATE|MAP_ANONYMOUS,
										   -1, 0);

		if (new_pages[store->num_data_pages] == MAP_FAILED) {
			perror("mmap");
			return OBJ_NONE;
		}

		store->data_pages = new_pages;
		store->num_data_pages = store->num_data_pages + 1;
		store->last_data_page_used = 0;
	}


	struct object o = {0};
	o.type = obj.type;

	if (type->size > 0) {
		size_t addr = ((store->num_data_pages - 1) * store->page_size
					   + store->last_data_page_used);
		store->last_data_page_used += type->size;

		o.data = &store->data_pages[addr / store->page_size][addr % store->page_size];

		memcpy((void *)o.data, (void *)obj.data, type->size);
	}

	store->pages[id / store->elements_per_page][id % store->elements_per_page] = o;

	return o;
}

void free_objstore(struct objstore *store) {
	for (size_t i = 0; i < store->num_pages; i++) {
		int err;
		err = munmap(store->pages[i], store->page_size);
		if (err != 0) {
			perror("munmap");
		}
	}

	if (store->pages) {
		free(store->pages);
	}

	if (store->types) {
		free(store->types);
		store->types = NULL;
	}

	if (store->funcs) {
		free(store->funcs);
		store->funcs = NULL;
	}
}

modtype_id
store_register_type(struct objstore *store, struct type type)
{
	modtype_id mtid;
	mtid = dlist_append(store->types, store->num_types, &type);
	return TYPE_ID(store->mod_id, mtid);
}

modfunc_id
store_register_func(struct objstore *store, struct func func)
{
	modfunc_id mfid;
	mfid = dlist_append(store->funcs, store->num_funcs, &func);
	return FUNC_ID(store->mod_id, mfid);
}

type_id
func_return_type(struct vm *vm, type_id func_type_id)
{
	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	return func_info->return_type;
}

size_t
func_num_params(struct vm *vm, type_id func_type_id)
{
	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	return func_info->num_params;
}

type_id
func_param_type(struct vm *vm, type_id func_type_id, size_t param_i)
{
	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	assert(param_i < func_info->num_params);
	return func_info->params[param_i];
}

void print_type_repr(struct vm *vm, struct type *type)
{
	struct arena mem = arena_push(&vm->memory);
	/* struct type *type = &vm->store.types[obj.type]; */
	struct string res;

	if (type->base->repr) {
		res = type->base->repr(vm, &mem, type);
	} else {
		res = default_type_repr(vm, &mem, type);
	}
	printf("%.*s", LIT(res));

	arena_pop(&vm->memory, mem);
}

void print_obj_repr(struct vm *vm, struct object obj)
{
	struct arena mem = arena_push(&vm->memory);
	struct type *type = vm_get_type(vm, obj.type);
	struct string res;

	if (type->base->obj_repr) {
		res = type->base->obj_repr(vm, &mem, &obj);
	} else {
		res = default_obj_repr(vm, &mem, &obj);
	}
	printf("%.*s", LIT(res));

	arena_pop(&vm->memory, mem);
}

void
arena_string_append_type_repr(struct string *str, struct vm *vm,
							  struct arena *mem, struct type *type)
{
	struct string type_repr;

	struct arena tmp_mem = arena_push(mem);
	if (type->base->repr) {
		type_repr = type->base->repr(vm, &tmp_mem, type);
	} else {
		type_repr = default_type_repr(vm, &tmp_mem, type);
	}
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, str, type_repr);
}

void
arena_string_append_obj_repr(struct string *str, struct vm *vm,
							 struct arena *mem, struct object *object)
{
	struct string repr;
	struct type *type;
	type = vm_get_type(vm, object->type);

	struct arena tmp_mem = arena_push(mem);
	if (type->base->obj_repr) {
		repr = type->base->obj_repr(vm, &tmp_mem, object);
	} else {
		repr = default_obj_repr(vm, &tmp_mem, object);
	}
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, str, repr);
}

