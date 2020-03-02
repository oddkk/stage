#include "arena.h"
#include "utils.h"
#include <string.h>
#include <stdlib.h>

// For sysconf
#include <unistd.h>

// For mmap
#include <sys/mman.h>

#include <valgrind/memcheck.h>

static struct stg_memory_page
stg_memory_alloc_page(struct stg_memory *mem, size_t hint_size)
{
	size_t num_pages;
	// Round the number of pages up to fit at least the hint size.
	num_pages = (hint_size + mem->sys_page_size - 1) / mem->sys_page_size;
	if (num_pages == 0) {
		num_pages = 1;
	}

	size_t real_size = num_pages * mem->sys_page_size;
	struct stg_memory_page page = {0};

	page.size = real_size;
	page.data = mmap(
			NULL, real_size,
			PROT_READ|PROT_WRITE,
			MAP_PRIVATE|MAP_ANONYMOUS,
			-1, 0);

	if (page.data == MAP_FAILED) {
		perror("mmap");
		page.size = 0;
		page.data = NULL;
		return page;
	}

	memset(page.data, 0, page.size);
	VALGRIND_MAKE_MEM_NOACCESS(page.data, page.size);

	// printf("alloc page data %p\n", page.data);

	return page;
}

struct stg_memory_page *
stg_memory_request_page(struct stg_memory *mem, size_t hint_size)
{
	struct stg_memory_page **page;
	page = &mem->free_list;

	for (size_t n = 0; n < 100 && *page; n++) {
		if ((*page)->size > hint_size) {
			struct stg_memory_page *res;
			res = *page;
			*page = res->next;
			res->next = NULL;
			VALGRIND_MAKE_MEM_UNDEFINED(res->data, res->size);
			return res;
		}
		page = &(*page)->next;
	}

	if (!mem->head_index_page ||
			mem->head_num_pages_alloced >= mem->head_index_page->num_pages) {
		struct stg_memory_page *index_page;
		if (mem->free_list) {
			index_page = mem->free_list;
			mem->free_list = index_page->next;
			mem->head_num_pages_alloced = 1;
		} else {
			struct stg_memory_page new_index_page;
			new_index_page = stg_memory_alloc_page(mem, 0);
			new_index_page.in_use = true;

			VALGRIND_MAKE_MEM_DEFINED(new_index_page.data, new_index_page.size);

			struct stg_memory_index_page *index;
			index = new_index_page.data;

			index->next = NULL;
			index->num_pages =
				(new_index_page.size - sizeof(struct stg_memory_index_page)) /
				sizeof(struct stg_memory_page);

			index->pages[0] = new_index_page;

			mem->head_index_page = index;
			mem->head_num_pages_alloced = 1;
			mem->num_pages_alloced += 1;
		}
	}

	assert(mem->head_num_pages_alloced < mem->head_index_page->num_pages);

	struct stg_memory_page new_page;
	new_page = stg_memory_alloc_page(mem, hint_size);
	new_page.in_use = true;
	new_page.next = NULL;

	VALGRIND_MAKE_MEM_UNDEFINED(new_page.data, new_page.size);

	struct stg_memory_index_page *current_index;
	current_index = mem->head_index_page;

	size_t page_i = mem->head_num_pages_alloced;
	mem->head_num_pages_alloced += 1;

	current_index->pages[page_i] = new_page;

	return &current_index->pages[page_i];
}

void
stg_memory_release_page(struct stg_memory *mem, struct stg_memory_page *page)
{
	memset(page->data, 0, page->size);
	page->next = mem->free_list;
	page->in_use = false;
	mem->free_list = page;

	VALGRIND_MAKE_MEM_NOACCESS(page->data, page->size);
}

int
stg_memory_init(struct stg_memory *mem)
{
	memset(mem, 0, sizeof(struct stg_memory));

	mem->sys_page_size = sysconf(_SC_PAGESIZE);

	struct stg_memory_page index_page;
	index_page = stg_memory_alloc_page(mem, 0);
	index_page.in_use = true;

	if (!index_page.data) {
		return -1;
	}

	VALGRIND_MAKE_MEM_DEFINED(index_page.data, index_page.size);

	struct stg_memory_index_page *index;
	index = index_page.data;

	index->next = NULL;
	index->num_pages =
		(index_page.size - sizeof(struct stg_memory_index_page)) /
		sizeof(struct stg_memory_page);

	index->pages[0] = index_page;

	mem->head_index_page = index;
	mem->head_num_pages_alloced = 1;
	mem->num_pages_alloced = 1;

	mem->free_list = NULL;

	return 0;
}

void
stg_memory_destroy(struct stg_memory *mem)
{
	struct stg_memory_index_page *index;
	index = mem->head_index_page;
	while (index) {
		struct stg_memory_index_page *next_index;
		// Cache a reference to the next index page because index might be
		// unmapped as a page of itself.
		next_index = index->next;

		// We free the pages in reverse allocation order to make sure we free
		// the index pages after all their pages have been freed.
		for (ssize_t i = index->num_pages; i >= 0; i--) {
			struct stg_memory_page *page;
			page = &index->pages[i];
			if (page->data) {
				int err;
				err = munmap(page->data, page->size);

				if (err) {
					perror("munmap");
				}
			}
		}

		index = next_index;
	}

	memset(mem, 0, sizeof(struct stg_memory));
}

int arena_init(struct arena *arena, struct stg_memory *mem)
{
	memset(arena, 0, sizeof(struct arena));
	arena->mem = mem;

	return 0;
}

void arena_destroy(struct arena *arena)
{
	struct stg_memory_page *page;
	page = arena->head_page;
	while (page) {
		struct stg_memory_page *next_page;
		next_page = page->next;

		stg_memory_release_page(arena->mem, page);

		page = next_page;
	}
}

void *arena_alloc(struct arena *arena, size_t length)
{
	void *result;
	result = arena_alloc_no_zero(arena, length);
	memset(result, 0, length);

	return result;
}

void *arena_alloc_no_zero(struct arena *arena, size_t length)
{
	if (!arena->head_page || arena->head_page_head + length > arena->head_page->size) {
		struct stg_memory_page *new_page;
		new_page = stg_memory_request_page(arena->mem, length);

		if (arena->head_page) {
			arena->head_page_i += 1;
		}

		new_page->next = arena->head_page;
		arena->head_page = new_page;

		arena->head_page_head = 0;
	}

	void *result;
	result = (uint8_t *)arena->head_page->data + arena->head_page_head;

	// printf("arena alloc %p (%zu) from %p (%zu) + %zu\n",
	// 		result, length, arena->head_page->data, arena->head_page->size, arena->head_page_head);

	arena->head_page_head += length;

	return result;
}


struct arena arena_push(struct arena *arena)
{
	struct arena tmp = {0};

	tmp.data = arena->data + arena->head;
	tmp.capacity = arena->capacity - arena->head;
	arena->head = arena->capacity;

	return tmp;
}

void arena_pop(struct arena *arena, struct arena tmp)
{
	assert(tmp.data >= arena->data &&
		   tmp.data < arena->data + arena->capacity);

	arena->head = tmp.data - arena->data;
}

arena_mark
arena_checkpoint(struct arena *arena)
{
	return arena->head;
}

void
arena_reset(struct arena *arena, arena_mark mark)
{
	assert(arena->head >= mark);
	arena->head = mark;
}

void arena_print_usage(struct arena *arena)
{
	printf("%lu / %lu", arena->head, arena->capacity);
}
