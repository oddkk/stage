#ifndef STAGE_STRING_H
#define STAGE_STRING_H

#include "intdef.h"
#include <stdarg.h>

struct string {
	char *text;
	size_t length;
};

#define LIT(x) ((int)(x).length),((char*)(x).text)
#define STR(cstr) (struct string){cstr, sizeof(cstr)-1}
#define STR_BE(begin, end) (struct string){(begin), (end)-(begin)}

#define STR_EMPTY ((struct string){NULL, 0})

bool string_equal(struct string lhs, struct string rhs);

struct arena;
int string_duplicate(struct arena *arena, struct string *dest,
		     struct string src);

struct string string_duplicate_cstr(char *str);

struct string arena_sprintf(struct arena *, char *fmt, ...)
// TODO: Make this cross-compiler compliant.
	__attribute__((__format__ (__printf__, 2, 3)));

int arena_string_append(struct arena *, struct string *, struct string);
void arena_string_append_sprintf(struct arena *, struct string *, char *fmt, ...)
// TODO: Make this cross-compiler compliant.
	__attribute__((__format__ (__printf__, 3, 4)));
void arena_string_append_vsprintf(struct arena *, struct string *, char *fmt, va_list ap);

int64_t string_to_int64_base2(struct string str);
int64_t string_to_int64_base10(struct string str);
int64_t string_to_int64_base16(struct string str);

int read_character(struct string str, char **it);
bool string_split(struct string in, struct string *result,
				  struct string *rest, int sep);

struct string string_replace_all_char(struct arena *, struct string str,
		int target, struct string replacement);

struct stg_exec;

struct string
stg_exec_sprintf(struct stg_exec *, const char *fmt, ...);

typedef struct { uint64_t val[2]; } stg_hash;

stg_hash
stg_hash_string(struct string);

static inline bool
stg_hash_eq(stg_hash lhs, stg_hash rhs)
{
	return
		lhs.val[0] == rhs.val[0] &&
		lhs.val[1] == rhs.val[1];
}

static inline bool
stg_hash_lte(stg_hash lhs, stg_hash rhs)
{
	return
		lhs.val[0] < rhs.val[0] ||
		(lhs.val[0] == rhs.val[0] &&
			lhs.val[1] <= rhs.val[1]);
}

#endif
