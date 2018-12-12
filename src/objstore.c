#include "objstore.h"
#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <unistd.h>
#include <sys/mman.h>

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
										   MAP_ANONYMOUS, -1, 0);

		if (new_pages[store->num_pages] == MAP_FAILED) {
			perror("mmap");
			return 0;
		}

		store->pages = new_pages;
		store->num_pages = store->num_pages + 1;
		store->last_page_num_used = 0;
	}

	obj_id id = store->num_pages * store->elements_per_page
		+ store->last_page_num_used;

	store->last_page_num_used += 1;

	*get_object(store, id) = obj;

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
