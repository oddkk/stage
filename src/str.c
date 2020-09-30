#include "str.h"
#include "arena.h"
#include "utils.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "murmurhash.h"

// For stg_alloc
#include "objstore.h"

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

struct string
arena_vsprintf(struct arena *arena, char *fmt, va_list ap)
{
	va_list tmp_ap;
	size_t cap;
	void *buffer;
	int err;

	arena_mark reservation;
	reservation = arena_reserve_page(arena, &buffer, &cap);

	va_copy(tmp_ap, ap);
	err = vsnprintf(buffer, cap, fmt, tmp_ap);
	va_end(tmp_ap);

	if (err < 0) {
		perror("vsnprintf");
		return STR_EMPTY;
	}

	if (err > cap) {
		arena_take_reserved(arena, reservation, 0);

		cap = err + 1;
		buffer = arena_alloc(arena, cap);

		va_copy(tmp_ap, ap);
		vsnprintf(buffer, cap, fmt, tmp_ap);
		va_end(tmp_ap);

	} else {
		arena_take_reserved(arena, reservation, err);
	}

	struct string result = {0};

	result.text = buffer;
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

int arena_string_append(struct arena *mem, struct string *str, struct string in)
{
	char *new_buffer;
	new_buffer = arena_alloc(mem, str->length + in.length + 1);

	if (!new_buffer) {
		return -1;
	}

	memcpy(new_buffer, str->text, str->length);
	memcpy(new_buffer+str->length, in.text, in.length);
	new_buffer[str->length + in.length] = 0;

	str->length = str->length + in.length;
	str->text = new_buffer;

	return 0;
}

void arena_string_append_vsprintf(struct arena *mem, struct string *str, char *fmt, va_list ap)
{
	arena_mark cp = arena_checkpoint(mem);

	struct string result;
	result = arena_vsprintf(mem, fmt, ap);
	arena_string_append(mem, str, result);

	str->text = arena_reset_and_keep(mem, cp, str->text, str->length);
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

struct string
string_replace_all_char(struct arena *mem, struct string str,
		int target, struct string replacement)
{
	size_t new_size = str.length;

	for (size_t i = 0; i < str.length; i++) {
		if (str.text[i] == target) {
			new_size += replacement.length-1;
		}
	}

	struct string result;
	result.length = new_size;
	result.text = arena_alloc(mem, new_size+1);

	size_t cursor = 0;
	for (size_t i = 0; i < str.length; i++) {
		if (str.text[i] == target) {
			memcpy(&result.text[cursor],
					replacement.text,
					replacement.length);
			cursor += replacement.length;
		} else {
			result.text[cursor] = str.text[i];
			cursor += 1;
		}
	}

	return result;
}

struct string
stg_exec_sprintf(struct stg_exec *heap, const char *fmt, ...)
{
	struct string res = {0};

	int err;

	va_list ap;
	va_start(ap, fmt);

#define BUF_CAP 4096
	char buffer[BUF_CAP];
	err = vsnprintf(buffer, BUF_CAP, fmt, ap);

	va_end(ap);

	if (err < 0) {
		perror("vsnprintf");
		return STR_EMPTY;
	}

	res.length = err;
	res.text = stg_alloc(heap, res.length+1, sizeof(char));

	if (res.length >= BUF_CAP) {
		va_start(ap, fmt);
		err = vsnprintf(res.text, res.length+1, fmt, ap);
		va_end(ap);
	} else {
		memcpy(res.text, buffer, res.length+1);
	}

#undef BUF_CAP

	return res;
}

stg_hash
stg_hash_string(struct string str)
{
	stg_hash result = {0};

	// TODO: Make this choose the correct routine for the target platform.
	MurmurHash3_x64_128(str.text, str.length, 0, &result.val);

	return result;
}
