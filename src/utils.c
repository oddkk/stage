#include "utils.h"
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void print_info(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

void print_error(const char *tag, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	fprintf(stderr, "[%s] ", tag);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	fflush(stdout);
	va_end(ap);
}

void panic(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	fprintf(stderr, "[panic] ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
	fflush(stdout);
	abort();
}

void zero_memory(void *data, size_t length)
{
	memset(data, 0, length);
}
