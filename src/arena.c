#include "arena.h"
#include "utils.h"
#include <string.h>
#include <stdlib.h>

int arena_init(struct arena *arena, size_t capacity)
{
	arena->capacity = capacity;
	arena->head = 0;
	arena->data = malloc(capacity);

	if (!arena->data) {
		print_error("arena init",
			    "Could not allocate memory for arena.");
		return -1;
	}
	return 0;
}

void *arena_alloc(struct arena *arena, size_t length)
{
	assert(arena->head + length < arena->capacity);

	void *result = &arena->data[arena->head];
	arena->head += length;

	memset(result, 0, length);

	return result;
}

void arena_print_usage(struct arena *arena)
{
	printf("%lu / %lu", arena->head, arena->capacity);
}
