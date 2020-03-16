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

	return page;
}

static struct stg_memory_page *
stg_memory_try_alloc_from_free_list(struct stg_memory *mem, size_t hint_size)
{
	struct stg_memory_page **page;
	page = &mem->free_list;

	for (size_t n = 0; n < 100 && *page; n++) {
		if ((*page)->size > hint_size) {
			struct stg_memory_page *res;
			res = *page;
			*page = res->next;
			res->next = NULL;
			assert(!res->in_use);
			res->in_use = true;

			// VALGRIND_MAKE_MEM_UNDEFINED(res->data, res->size);
			return res;
		}
		page = &(*page)->next;
	}

	return NULL;
}

// Note that this routine only allocates the stg_memory_page structure. It does
// not allocate the memory pointed to by that structure.
static struct stg_memory_page *
stg_memory_request_page_slot(struct stg_memory *mem)
{
	if (!mem->head_index_page ||
			mem->head_num_pages_alloced >= mem->head_index_page->num_pages) {
		struct stg_memory_page index_page;
		index_page = stg_memory_alloc_page(mem, 0);
		index_page.next = NULL;
		index_page.in_use = true;

		VALGRIND_MAKE_MEM_UNDEFINED(index_page.data, index_page.size);

		struct stg_memory_index_page *index;
		index = index_page.data;

		index->next = mem->head_index_page;
		index->num_pages =
			(index_page.size - sizeof(struct stg_memory_index_page)) /
			sizeof(struct stg_memory_page);

		index->pages[0] = index_page;

		mem->head_index_page = index;
	}

	struct stg_memory_page *slot = NULL;
	slot = &mem->head_index_page->pages[mem->head_num_pages_alloced];
	mem->head_num_pages_alloced += 1;

	return slot;
}

struct stg_memory_page *
stg_memory_request_page(struct stg_memory *mem, size_t hint_size)
{
	struct stg_memory_page *page;

	page = stg_memory_try_alloc_from_free_list(mem, hint_size);

	if (!page) {
		page = stg_memory_request_page_slot(mem);

		*page = stg_memory_alloc_page(mem, hint_size);
		if (!page->data) {
			return NULL;
		}
		assert(!page->in_use);
		page->in_use = true;
		page->next = NULL;
	}

	return page;
}

void
stg_memory_release_page(struct stg_memory *mem, struct stg_memory_page *page)
{
	VALGRIND_MAKE_MEM_UNDEFINED(page->data, page->size);
	memset(page->data, 0, page->size);
	page->next = mem->free_list;
	assert(page->in_use);
	page->in_use = false;
	mem->free_list = page;

	VALGRIND_MAKE_MEM_NOACCESS(page->data, page->size);
}

int
stg_memory_init(struct stg_memory *mem)
{
	memset(mem, 0, sizeof(struct stg_memory));

	mem->sys_page_size = sysconf(_SC_PAGESIZE);
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
		size_t index_num_pages = index->num_pages;
		if (index == mem->head_index_page) {
			index_num_pages = mem->head_num_pages_alloced;
		}
		for (ssize_t i = index_num_pages-1; i >= 0; i--) {
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

void
paged_list_init(struct paged_list *list,
		struct stg_memory *mem, size_t element_size)
{
	memset(list, 0, sizeof(struct paged_list));
	list->mem = mem;
	list->element_size = element_size;
	list->elements_per_page = mem->sys_page_size / element_size;
	if (list->elements_per_page == 0) {
		list->elements_per_page = 1;
	}
}

void
paged_list_destroy(struct paged_list *list)
{
	for (size_t i = 0; i < list->num_pages; i++) {
		stg_memory_release_page(list->mem, list->pages[i]);
	}

	free(list->pages);
	memset(list, 0, sizeof(struct paged_list));
}

size_t
paged_list_push(struct paged_list *list)
{
	size_t num_pages = list->num_pages;
	size_t num_alloced = list->length;
	size_t per_page = list->elements_per_page;

	if ((num_alloced + per_page) / per_page > num_pages) {
		size_t new_size;
		struct stg_memory_page **new_pages;

		new_size = (num_pages + 1) * sizeof(struct stg_memory_page *);
		new_pages = realloc(list->pages, new_size);
		if (!new_pages) {
			perror("realloc");
			panic("realloc");
			return 0;
		}

		list->pages = new_pages;

		list->pages[num_pages] =
			stg_memory_request_page(
					list->mem, per_page * list->element_size);

		list->num_pages += 1;
	}

	size_t id;
	id = list->length;
	list->length += 1;

	void *data = paged_list_get(list, id);
	VALGRIND_MAKE_MEM_UNDEFINED(data, list->element_size);
	memset(data, 0, list->element_size);

	return id;
}

void *
paged_list_get(struct paged_list *list, size_t id)
{
	assert(id < list->length);
	struct stg_memory_page *page;
	page = list->pages[id / list->elements_per_page];
	return (uint8_t *)page->data + (id % list->elements_per_page) * list->element_size;
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

	memset(arena, 0, sizeof(struct arena));
}

void *arena_alloc(struct arena *arena, size_t length)
{
	void *result;
	result = arena_alloc_no_zero(arena, length);
	memset(result, 0, length);

	return result;
}

void *arena_allocn(struct arena *arena, size_t nmemb, size_t size)
{
	size_t res;
	// TODO: Make this cross platform and cross compiler compliant.
	if (__builtin_mul_overflow(nmemb, size, &res)) {
		panic("Attempted to allocate memory with a size that exeedes 64-bit integers.");
		return NULL;
	}

	return arena_alloc(arena, res);
}

void *arena_alloc_no_zero(struct arena *arena, size_t length)
{
	assert((arena->flags & ARENA_RESERVED) == 0);

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
	VALGRIND_MAKE_MEM_UNDEFINED(result, length);
	arena->head_page_head += length;

	return result;
}

arena_mark
arena_checkpoint(struct arena *arena)
{
	struct _arena_mark mark = {0};

	assert((arena->flags & (ARENA_NO_CHECKPOINT|ARENA_RESERVED)) == 0);

	mark.page_i = arena->head_page_i;
	mark.page_head = arena->head_page_head;

	return mark;
}

void
arena_reset(struct arena *arena, arena_mark mark)
{
	assert(arena->head_page_i > mark.page_i ||
			(arena->head_page_i == mark.page_i &&
			 arena->head_page_head >= mark.page_head));

	while (arena->head_page_i > mark.page_i) {
		struct stg_memory_page *page;
		page = arena->head_page;
		arena->head_page = page->next;
		arena->head_page_i -= 1;

		stg_memory_release_page(arena->mem, page);
	}

	arena->head_page_head = mark.page_head;

	if (arena->head_page) {
		void *head_ptr = (uint8_t *)arena->head_page->data + arena->head_page_head;
		size_t unused_size = arena->head_page->size - arena->head_page_head;

		VALGRIND_MAKE_MEM_UNDEFINED(head_ptr, unused_size);
		memset(head_ptr, 0, unused_size);
		VALGRIND_MAKE_MEM_NOACCESS(head_ptr, unused_size);
	}
}

void *arena_reset_and_keep(struct arena *arena, arena_mark mark,
		void *keep, size_t keep_size)
{
	assert(arena->head_page_i > mark.page_i ||
			(arena->head_page_i == mark.page_i &&
			 arena->head_page_head >= mark.page_head));

	struct stg_memory_page *page;
	page = arena->head_page;
	for (size_t page_i = arena->head_page_i;
			page_i > mark.page_i; page_i--) {
		assert(page);
		page = page->next;
	}

	void *result = NULL;
	struct stg_memory_page *new_page = NULL;
	size_t new_page_head = 0;

	if (keep_size <= (page->size - mark.page_head)) {
		result = (uint8_t *)page->data + mark.page_head;
		memmove(result, keep, keep_size);

		new_page_head = mark.page_head + keep_size;
	} else {
		new_page = stg_memory_request_page(arena->mem, keep_size);
		new_page->next = page;

		assert(keep_size <= new_page->size);
		result = new_page->data;
		VALGRIND_MAKE_MEM_UNDEFINED(new_page->data, keep_size);
		memcpy(new_page->data, keep, keep_size);
		new_page_head = keep_size;
	}

	while (arena->head_page_i > mark.page_i) {
		struct stg_memory_page *page;
		page = arena->head_page;
		arena->head_page = page->next;
		arena->head_page_i -= 1;

		stg_memory_release_page(arena->mem, page);
	}

	assert(arena->head_page == page);

	if (new_page) {
		if (arena->head_page) {
			arena->head_page_i += 1;
		}

		arena->head_page = new_page;
	}

	return result;
}

arena_mark
arena_reserve_page(struct arena *arena, void **out_memory, size_t *out_size)
{
	arena_mark cp = arena_checkpoint(arena);
	assert((arena->flags & ARENA_RESERVED) == 0);
	arena->flags |= ARENA_RESERVED;

	if (!arena->head_page) {
		*out_memory = NULL;
		*out_size = 0;

		arena_mark res = {0};
		return res;
	}

	size_t size;
	size = arena->head_page->size - arena->head_page_head;

	void *result;
	result = (uint8_t *)arena->head_page->data + arena->head_page_head;
	VALGRIND_MAKE_MEM_UNDEFINED(result, size);

	arena->head_page_head = arena->head_page->size;

	if (out_size) {
		*out_size = size;
	}

	if (out_memory) {
		memset(result, 0, size);
		*out_memory = result;
	}

	return cp;
}

void
arena_take_reserved(struct arena *arena, arena_mark cp, size_t length)
{
	assert((arena->flags & ARENA_RESERVED) != 0);
	arena->flags &= ~ARENA_RESERVED;

	assert(cp.page_head + length <= (arena->head_page ? arena->head_page->size : 0));
	assert(cp.page_i == arena->head_page_i);

	cp.page_head += length;

	arena_reset(arena, cp);
}
