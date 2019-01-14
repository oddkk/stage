#ifndef STAGE_ARENA_H
#define STAGE_ARENA_H

#include "intdef.h"

struct arena {
	uint8_t *data;
	size_t head;
	size_t capacity;
};

int arena_init(struct arena *arena, size_t capacity);
void *arena_alloc(struct arena *arena, size_t length);
void *arena_alloc_no_zero(struct arena *arena, size_t length);

typedef size_t arena_point;

struct arena arena_push(struct arena *arena);
void arena_pop(struct arena *arena, struct arena tmp);

void arena_print_usage(struct arena *arena);

#define arena_alloc_struct(arena, struct) arena_alloc(arena, sizeof(struct))
#define arena_alloc_nstruct(arena, struct, n) arena_alloc(arena, sizeof(struct) * n)

#define KILOBYTE(x) (x * 1000)
#define MEGABYTE(x) (KILOBYTE(x) * 1000)

#endif
