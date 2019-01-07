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

obj_id register_object(struct objstore *store, struct object obj) {
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
			return 0;
		}

		new_pages[store->num_pages] = mmap(NULL, store->page_size,
										   PROT_READ|PROT_WRITE,
										   MAP_PRIVATE|MAP_ANONYMOUS,
										   -1, 0);

		if (new_pages[store->num_pages] == MAP_FAILED) {
			perror("mmap");
			return 0;
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
			return 0;
		}

		new_pages[store->num_data_pages] = mmap(NULL, store->page_size,
										   PROT_READ|PROT_WRITE,
										   MAP_PRIVATE|MAP_ANONYMOUS,
										   -1, 0);

		if (new_pages[store->num_data_pages] == MAP_FAILED) {
			perror("mmap");
			return 0;
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

	return id;
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
	return dlist_append(store->types, store->num_types, &type);
}

void print_type_repr(struct vm *vm, struct object obj)
{
	arena_point p = arena_push(&vm->memory);

	struct type *type = &vm->store.types[obj.type];

	struct string res;
	res = type->base->repr(vm, &vm->memory, &obj);

	printf("%.*s", LIT(res));

	arena_pop(&vm->memory, p);
}