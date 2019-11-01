#ifndef STAGE_ERROR_H
#define STAGE_ERROR_H

#include "arena.h"
#include "str.h"
#include <stdarg.h>

#define STG_ERROR_IMMEDIATELY_PRINT 0

typedef unsigned int file_id_t;
#define STG_FILE_UNKNOWN ((file_id_t)UINT32_MAX)

struct stg_location {
	size_t byte_from, byte_to;
	unsigned int line_from, col_from;
	unsigned int line_to, col_to;
	file_id_t file_id;
};

#define STG_NO_LOC ((struct stg_location){.file_id=STG_FILE_UNKNOWN})

enum stg_error_level {
	STG_ERROR,
	STG_WARNING,
	STG_INFO,
	STG_APPENDAGE,
};

struct stg_error {
	enum stg_error_level level;
	struct string msg;
	struct stg_location loc;
};

struct stg_error_context {
	struct arena *string_arena;

	// NOTE: File name ids are offset by one to facilitate having the sentinel
	// for unknown be 0.
	struct string *file_names;
	size_t num_files;

	struct stg_error *msgs;
	size_t num_msgs;

	size_t num_errors;
	size_t num_warnings;
};

file_id_t
stg_err_add_file(struct stg_error_context *, struct string file_name);

void
stg_msgv(struct stg_error_context *, struct stg_location, enum stg_error_level,
		const char *fmt, va_list);

__attribute__((__format__ (__printf__, 3, 4))) void
stg_error(struct stg_error_context *, struct stg_location, const char *fmt, ...);

__attribute__((__format__ (__printf__, 3, 4))) void
stg_warning(struct stg_error_context *, struct stg_location, const char *fmt, ...);

__attribute__((__format__ (__printf__, 3, 4))) void
stg_info(struct stg_error_context *, struct stg_location, const char *fmt, ...);

__attribute__((__format__ (__printf__, 3, 4))) void
stg_appendage(struct stg_error_context *, struct stg_location, const char *fmt, ...);

void
print_errors(struct stg_error_context *);

#endif
