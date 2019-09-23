#ifndef STAGE_UTILS_H
#define STAGE_UTILS_H

#include "intdef.h"
#include <stdio.h>

void print_info(const char *fmt, ...);
void print_error(const char *tag, const char *fmt, ...);
void panic(const char *fmt, ...);

#define assert(expr) \
	do if(!(expr)){ \
		panic(__FILE__ ":%i: Assertion '" #expr "' failed!", __LINE__); \
	} while (0)

void zero_memory(void *data, size_t length);

#define zero_struct(s) zero_memory(&s, sizeof(s));

#ifndef ARRAY_LENGTH
#define ARRAY_LENGTH(a) (sizeof(a) / sizeof(a[0]))
#endif

#endif
