#ifndef STAGE_STRING_H
#define STAGE_STRING_H

#include "intdef.h"

struct string {
	char *text;
	size_t length;
};

#define LIT(x) ((int)(x).length),((char*)(x).text)
#define STR(cstr) (struct string){cstr, sizeof(cstr)-1}
#define STR_BE(begin, end) (struct string){(begin), (end)-(begin)}

bool string_equal(struct string lhs, struct string rhs);

struct arena;
int string_duplicate(struct arena *arena, struct string *dest,
		     struct string src);

struct string string_duplicate_cstr(char *str);

struct string arena_sprintf(struct arena *, char *fmt, ...)
// TODO: Make this cross-compiler compliant.
	__attribute__((__format__ (__printf__, 2, 3)));

struct string arena_string_init(struct arena *);
int arena_string_append(struct arena *, struct string *, struct string);

int64_t string_to_int64_base2(struct string str);
int64_t string_to_int64_base10(struct string str);
int64_t string_to_int64_base16(struct string str);

int read_character(struct string str, char **it);
bool string_split(struct string in, struct string *result,
				  struct string *rest, int sep);

#endif
