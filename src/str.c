#include "str.h"
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
	} else {
		memcpy(result.text, str, result.length);
	}

	return result;
}

static inline struct string
arena_vsprintf(struct arena *arena, char *fmt, va_list ap)
{
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

struct string arena_sprintf(struct arena *arena, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);

	struct string result;
	result = arena_vsprintf(arena, fmt, ap);

	va_end(ap);

	return result;
}

struct string arena_string_init(struct arena *mem)
{
	struct string res = {0};
	res.text = arena_alloc(mem, 0);
	res.length = 0;
	return res;
}

int arena_string_append(struct arena *mem, struct string *str, struct string in)
{
	assert((mem->data + mem->head) == (uint8_t *)(str->text + str->length));

	uint8_t *appendage = arena_alloc_no_zero(mem, in.length);

	if (!appendage) {
		return -1;
	}

	assert(appendage == (uint8_t *)(str->text + str->length));
	memmove(appendage, in.text, in.length);
	str->length += in.length;

	appendage[in.length + 1] = 0;

	return 0;
}

void arena_string_append_vsprintf(struct arena *mem, struct string *str, char *fmt, va_list ap)
{
	arena_mark mark = arena_checkpoint(mem);

	struct string result;
	result = arena_vsprintf(mem, fmt, ap);

	arena_reset(mem, mark);

	arena_string_append(mem, str, result);
}

void arena_string_append_sprintf(struct arena *mem, struct string *str, char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	arena_string_append_vsprintf(mem, str, fmt, ap);
	va_end(ap);
}

int64_t string_to_int64_base2(struct string str)
{
	int64_t res = 0;
	for (size_t i = 0; i < str.length; ++i) {
		char c = str.text[i];
		if (c >= '0' || c <= '1') {
			res = res * 2 + c - '0';
		} else {
			break;
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
		if (c >= '0' && c <= '9') {
			res = res * 16 + c - '0';
		} else if (c >= 'a' && c <= 'f') {
			res = res * 16 + c - 'a' + 10;
		} else if (c >= 'A' && c <= 'F') {
			res = res * 16 + c - 'A' + 10;
		} else {
			break;
		}
	}
	return res;
}

int read_character(struct string str, char **it)
{
	int result;

	if (*it >= str.text + str.length) {
		return 0;
	}

	result = **it;
	*it += 1;

	return result;
}

bool string_split(struct string in, struct string *result,
				  struct string *rest, int sep)
{
	char *it = in.text;
	int c;

	if (in.length == 0) {
		return false;
	}

	while ((c = read_character(in, &it)) && c != sep);

	result->text = in.text;
	result->length = it - in.text;

	rest->length = in.length - result->length;
	rest->text = it;

	if (c == sep) {
		result->length -= 1;
	}

	return true;
}
