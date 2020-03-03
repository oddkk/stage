#ifndef STAGE_ARENA_H
#define STAGE_ARENA_H

#include "intdef.h"

struct stg_memory_page {
	size_t size;
	void *data;

	struct stg_memory_page *next;
	bool in_use;
};

struct stg_memory_index_page {
	struct stg_memory_index_page *next;

	size_t num_pages;
	struct stg_memory_page pages[];
};

struct stg_memory {
	struct stg_memory_page *free_list;
	size_t num_pages_alloced;

	size_t head_num_pages_alloced;
	struct stg_memory_index_page *head_index_page;

	size_t sys_page_size;
};

int
stg_memory_init(struct stg_memory *mem);

void
stg_memory_destroy(struct stg_memory *mem);

struct paged_list {
	struct stg_memory *mem;
	struct stg_memory_page **pages;
	size_t num_pages;
	size_t length;
	size_t element_size;
	size_t elements_per_page;
};

void
paged_list_init(struct paged_list *, struct stg_memory *, size_t element_size);

void
paged_list_destroy(struct paged_list *);

size_t
paged_list_push(struct paged_list *);

void *
paged_list_get(struct paged_list *, size_t id);

enum arena_flags {
	ARENA_NO_CHECKPOINT = (1<<0),
	ARENA_RESERVED      = (1<<1),
};

struct arena {
	struct stg_memory *mem;
	struct stg_memory_page *head_page;

	size_t head_page_i;
	size_t head_page_head;

	enum arena_flags flags;
};

int arena_init(struct arena *arena, struct stg_memory *mem);
void arena_destroy(struct arena *arena);
void *arena_alloc(struct arena *arena, size_t length);
void *arena_alloc_no_zero(struct arena *arena, size_t length);

struct _arena_mark {
	size_t page_i;
	size_t page_head;
};

typedef struct _arena_mark arena_mark;

arena_mark arena_checkpoint(struct arena *arena);
void arena_reset(struct arena *arena, arena_mark);
void *arena_reset_and_keep(struct arena *arena, arena_mark, void *keep, size_t keep_size);

arena_mark arena_reserve_page(struct arena *arena, void **out_memory, size_t *out_size);
void arena_take_reserved(struct arena *arena, arena_mark res, size_t length);

#define arena_alloc_struct(arena, struct) arena_alloc(arena, sizeof(struct))
#define arena_alloc_nstruct(arena, struct, n) arena_alloc(arena, sizeof(struct) * n)

#define KILOBYTE(x) (x * 1000)
#define MEGABYTE(x) (KILOBYTE(x) * 1000)

#endif
