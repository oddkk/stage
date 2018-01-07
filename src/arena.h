#ifndef STAGE_ARENA_H
#define STAGE_ARENA_H

#include "intdef.h"

struct arena {
	uint8_t *data;
	uint64_t head;
	uint64_t capacity;
};

int arena_init(struct arena *arena, size_t capacity);
void *arena_alloc(struct arena *arena, size_t length);

#define arena_alloc_struct(arena, struct) arena_alloc(arena, sizeof(struct))
#define arena_alloc_nstruct(arena, struct, n) arena_alloc(arena, sizeof(struct) * n)

#define KILOBYTE(x) (x * 1000)
#define MEGABYTE(x) (KILOBYTE(x) * 1000)

#endif
