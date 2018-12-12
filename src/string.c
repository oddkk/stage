#include "string.h"
#include "arena.h"
#include <string.h>
#include <stdlib.h>

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
	dest->text = arena_alloc(arena, src.length + 1);

	if (!dest->text) {
		return -1;
	}

	dest->text[src.length] = 0;
	memcpy(dest->text, src.text, dest->length);

	return 0;
}

struct string string_duplicate_cstr(char *str)
{
	struct string result = {0};

	result.length = strlen(str);
	result.text = calloc(sizeof(char), result.length + 1);

	if (!result.text) {
		result.length = 0;
	}

	return result;
}

int64_t string_to_int64_base2(struct string str)
{
	int64_t res = 0;
	for (size_t i = 0; i < str.length; ++i) {
		char c = str.text[i];
		if (c >= '0' || c <= '1') {
			res = res * 2 + c - '0';
		}
	}
	return res;
}

int64_t string_to_int64_base10(struct string str)
{
	int64_t res = 0;
	for (size_t i = 0; i < str.length; ++i) {
		char c = str.text[i];
		if (c >= '0' && c <= '9') {
			res = res * 10 + c - '0';
		} else {
			break;
		}
	}
	return res;
}

int64_t string_to_int64_base16(struct string str)
{
	int64_t res = 0;
	for (size_t i = 0; i < str.length; ++i) {
		char c = str.text[i];
		if (c > '0' && c <= '9') {
			res = res * 16 + c - '0';
		} else if (c > 'a' && c <= 'f') {
			res = res * 16 + c - 'a' + 10;
		} else if (c > 'A' && c <= 'F') {
			res = res * 16 + c - 'A' + 10;
		} else {
			break;
		}
	}
	return res;
}
