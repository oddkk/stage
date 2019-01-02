#include "string.h"
#include "arena.h"
#include "utils.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

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

struct string arena_sprintf(struct arena *arena, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	char *out = (char *)&arena->data[arena->head];

	size_t cap = arena->capacity - arena->head;
	int err;
	err = vsnprintf(out, cap, fmt, ap);

	va_end(ap);

	struct string result = {0};

	if (err < 0) {
		perror("vsnprintf");
		return result;
	}

	if (err >= cap) {
		printf("Warning: String truncated due to insufficient space in arena.\n");
		err = (int)cap;
	}

	result.text = out;
	result.length = err;

	return result;
}

int arena_string_append(struct arena *mem, struct string *str, struct string in)
{
	assert((mem->data + mem->head) == (uint8_t *)(str->text + str->length));

	uint8_t *appendage = arena_alloc(mem, in.length);

	if (!appendage) {
		return -1;
	}

	assert(appendage == (uint8_t *)(str->text + str->length));
	memcpy(appendage, in.text, in.length);
	str->length += in.length;

	return 0;
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
