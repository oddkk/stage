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

void *arena_alloc_no_zero(struct arena *arena, size_t length)
{
	assert(arena->head + length < arena->capacity);

	void *result = &arena->data[arena->head];
	arena->head += length;

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

void arena_print_usage(struct arena *arena)
{
	printf("%lu / %lu", arena->head, arena->capacity);
}
