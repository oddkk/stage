#ifndef STAGE_STRING_H
#define STAGE_STRING_H

#include "intdef.h"

struct string {
	char *text;
	size_t length;
};

#define LIT(x) ((int)(x).length),((char*)(x).text)
#define STR(cstr) (struct string){cstr, sizeof(cstr)-1}

bool string_equal(struct string lhs, struct string rhs);

struct arena;
int string_duplicate(struct arena *arena, struct string *dest,
		     struct string src);

#endif
