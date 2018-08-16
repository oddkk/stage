#ifndef STAGE_UTILS_H
#define STAGE_UTILS_H

#include "intdef.h"
#include <stdio.h>
#include <assert.h>

void print_info(const char *fmt, ...);
void print_error(const char *tag, const char *fmt, ...);
void panic(const char *fmt, ...);

/* #define assert(expr) \ */
/* 	if(!(expr)){ \ */
/* 		panic("Assertion '" #expr "' failed! (" __FILE__ ":%i)",__LINE__); \ */
/* 	} */

void zero_memory(void *data, size_t length);

#define zero_struct(s) zero_memory(&s, sizeof(s));

#endif
