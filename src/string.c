#include "string.h"
#include "arena.h"
#include <string.h>

bool string_equal(struct string lhs, struct string rhs)
{
	if (lhs.length != rhs.length) {
		return false;
	}
	for (size_t i = 0; i < lhs.length; i++) {
		if (lhs.text[i] != rhs.text[i]) {
			return false;
		}
	}
	return true;
}

int string_duplicate(struct arena *arena, struct string *dest,
		     struct string src)
{
	dest->length = src.length;
	dest->text = arena_alloc(arena, src.length);

	if (!dest->text) {
		return -1;
	}

	memcpy(dest->text, src.text, dest->length);

	return 0;
}
