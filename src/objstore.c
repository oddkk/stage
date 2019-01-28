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

static type_id default_type_subtypes_iter(struct vm *vm, struct type *type,
										  size_t *iter)
{
	return TYPE_SUBTYPES_END;
}

void type_base_init(struct type_base *base, struct string name)
{
	base->name = name;
	base->repr = default_type_repr;
	base->obj_repr = default_obj_repr;
	base->subtypes_iter = default_type_subtypes_iter;
}

void type_base_init_unfilled(struct type_base *base)
{
	if (base->repr == NULL) {
		base->repr = default_type_repr;
	}

	if (base->obj_repr == NULL) {
		base->obj_repr = default_obj_repr;
	}

	if (base->subtypes_iter == NULL) {
		base->subtypes_iter = default_type_subtypes_iter;
	}
}

void
type_base_register_unifier(struct type_base *type1,
						   struct type_base *type2,
						   type_unify unifier)
{
	if (type1 == NULL) {
		struct type_base *tmp;
		tmp = type1;
		type1 = type2;
		type2 = tmp;
	}

	assert(type1 != NULL);

	// Make sure no more than one pair of unifiers
	for (size_t i = 0; i < type1->num_unifiers; i++) {
		assert(type1->unifiers[i].other != type2);
	}

	if (type2) {
		for (size_t i = 0; i < type2->num_unifiers; i++) {
			assert(type2->unifiers[i].other != type1);
		}
	}

	struct type_unifier uni = {0};
	uni.unify = unifier;
	uni.other = type2;

	dlist_append(type1->unifiers, type1->num_unifiers, &uni);

	if (type2 != NULL) {
		uni.other = type1;
		dlist_append(type2->unifiers, type2->num_unifiers, &uni);
	}
}

struct object
register_object(struct objstore *store, struct object obj) {
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

	assert(obj.type <= store->num_types);
	struct type *type = &store->types[obj.type];

	if (store->data_pages == NULL ||
		store->last_page_num_used + type->size >= store->page_size) {
		size_t new_size = (store->num_data_pages + 1) * sizeof(uint8_t);
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
}

type_id register_type(struct objstore *store, struct type type)
{
	modtype_id mtid;
	mtid = dlist_append(store->types, store->num_types, &type);
	return TYPE_ID(store->mod_id, mtid);
}

bool unify_types(struct vm *vm, struct objstore *store, type_id lhs, type_id rhs, type_id *out)
{
	if (lhs == rhs) {
		 *out = lhs;
		 return true;
	}

	struct type *lhs_type = vm_get_type(vm, lhs);
	struct type *rhs_type = vm_get_type(vm, rhs);

	struct type_unifier *unifier = NULL;

	for (size_t i = 0; i < lhs_type->base->num_unifiers; i++) {
		struct type_unifier *uni;
		uni = &lhs_type->base->unifiers[i];

		if (uni->other == rhs_type->base ||
			uni->other == NULL) {
			unifier = uni;
			break;
		}
	}

	if (!unifier) {
		for (size_t i = 0; i < rhs_type->base->num_unifiers; i++) {
			struct type_unifier *uni;
			uni = &rhs_type->base->unifiers[i];

			if (uni->other == NULL) {
				unifier = uni;
				break;
			}

			assert(uni->other != lhs_type->base ||
				!"unifiers where not applied symmetrically");
		}
	}

	if (!unifier) {
		return false;
	}

	return unifier->unify(vm, store, lhs, rhs, out);
}

void print_type_repr(struct vm *vm, struct type *type)
{
	struct arena mem = arena_push(&vm->memory);
	/* struct type *type = &vm->store.types[obj.type]; */
	struct string res;

	res = type->base->repr(vm, &mem, type);
	printf("%.*s", LIT(res));

	arena_pop(&vm->memory, mem);
}

void print_obj_repr(struct vm *vm, struct object obj)
{
	struct arena mem = arena_push(&vm->memory);
	struct type *type = vm_get_type(vm, obj.type);
	struct string res;

	res = type->base->obj_repr(vm, &mem, &obj);
	printf("%.*s", LIT(res));

	arena_pop(&vm->memory, mem);
}

void
arena_string_append_type_repr(struct string *str, struct vm *vm,
							  struct arena *mem, struct type *type)
{
	struct string type_repr;

	struct arena tmp_mem = arena_push(mem);
	type_repr = type->base->repr(vm, &tmp_mem, type);
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
	repr = type->base->obj_repr(vm, &tmp_mem, object);
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, str, repr);
}

