#include "errors.h"
#include "term_color.h"
#include "utils.h"
#include "dlist.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>

file_id_t
stg_err_add_file(struct stg_error_context *err, struct string file_name)
{
	assert(err->num_files < UINT32_MAX);

	file_id_t file_id = err->num_files + 1;
	err->num_files += 1;

	struct string *new_file_names;
	new_file_names = realloc(err->file_names, err->num_files * sizeof(struct string));

	if (!new_file_names) {
		panic("Could not allocate file name array.");
		return (file_id_t)-1;
	}

	err->file_names = new_file_names;
	err->file_names[file_id - 1] = file_name;

	return file_id;
}

static const char *
level_prefix(enum stg_error_level lvl)
{
	switch (lvl) {
		case STG_ERROR:    return TC(TC_BRIGHT_RED, "ERROR") ": ";
		case STG_WARNING:  return TC(TC_YELLOW, "WARNING") ": ";
		case STG_INFO:     return TC(TC_BRIGHT_BLUE, "INFO") ": ";
		case STG_APPENDAGE: return "";
	}
}

void
stg_msgv(struct stg_error_context *err, struct stg_location loc,
		enum stg_error_level lvl, const char *fmt, va_list ap)
{
	if (!err) {
		return;
	}
	struct stg_error msg = {0};

	msg.level = lvl;
	msg.loc = loc;

	memset(&msg.msg, 0, sizeof(struct string));

	arena_mark cp = arena_checkpoint(err->transient_arena);

	arena_string_append_vsprintf(err->transient_arena, &msg.msg, (char *)fmt, ap);
	string_duplicate(err->string_arena, &msg.msg, msg.msg);

	arena_reset(err->transient_arena, cp);

	dlist_append(err->msgs, err->num_msgs, &msg);

	switch (lvl) {
		case STG_ERROR: err->num_errors += 1; break;
		case STG_WARNING: err->num_warnings += 1; break;
		case STG_INFO: break;
		case STG_APPENDAGE: break;
	}

#if STG_ERROR_IMMEDIATELY_PRINT
	{
		struct string file_name = STR("(unknown file)");

		if (msg.loc.file_id - 1 < err->num_files && msg.loc.file_id > 0) {
			file_name = err->file_names[msg.loc.file_id - 1];

			char file_name_cstr[file_name.length + 1];
			memcpy(file_name_cstr, file_name.text, file_name.length);
			file_name_cstr[file_name.length] = 0;
		}

		fprintf(stderr, "%s%.*s\n",
				level_prefix(msg.level),
				LIT(file_name)
			   );
		fprintf(stderr, "%.*s\n\n", LIT(msg.msg));
	}
#endif
}

__attribute__((__format__ (__printf__, 3, 4))) void
stg_error(struct stg_error_context *err, struct stg_location loc, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	stg_msgv(err, loc, STG_ERROR, fmt, ap);
	va_end(ap);
}

__attribute__((__format__ (__printf__, 3, 4))) void
stg_warning(struct stg_error_context *err, struct stg_location loc, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	stg_msgv(err, loc, STG_WARNING, fmt, ap);
	va_end(ap);
}

__attribute__((__format__ (__printf__, 3, 4))) void
stg_info(struct stg_error_context *err, struct stg_location loc, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	stg_msgv(err, loc, STG_INFO, fmt, ap);
	va_end(ap);
}

__attribute__((__format__ (__printf__, 3, 4))) void
stg_appendage(struct stg_error_context *err, struct stg_location loc, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	stg_msgv(err, loc, STG_APPENDAGE, fmt, ap);
	va_end(ap);
}

void
print_errors(struct stg_error_context *err)
{
	FILE *out = stderr;
	for (size_t i = 0; i < err->num_msgs; i++) {
		struct stg_error *msg = &err->msgs[i];

		struct string file_name = STR("(unknown file)");

		FILE *fp = NULL;
		bool file_error = false;

		if (msg->loc.file_id - 1 < err->num_files && msg->loc.file_id > 0) {
			file_name = err->file_names[msg->loc.file_id - 1];

			char file_name_cstr[file_name.length + 1];
			memcpy(file_name_cstr, file_name.text, file_name.length);
			file_name_cstr[file_name.length] = 0;

			fp = fopen((const char *)file_name_cstr, "rb");
			if (!fp) {
				file_error = true;
			}
		}

		fprintf(out, "%s%.*s\n",
			level_prefix(msg->level),
			LIT(file_name)
		);

		if (fp) {
			size_t file_length;

			fseek(fp, 0L, SEEK_END);
			file_length = ftell(fp);

#define LINE_CONTEXT_LIMIT 40

			// size_t token_length = msg->loc.byte_to - msg->loc.byte_from;
			bool ellipse_front = true, ellipse_back = true;

			size_t line_begin;
			if (msg->loc.byte_from >= LINE_CONTEXT_LIMIT) {
				line_begin = msg->loc.byte_from - LINE_CONTEXT_LIMIT;
			} else {
				line_begin = 0;
				ellipse_front = false;
			}

			size_t line_end;
			if (msg->loc.byte_to + LINE_CONTEXT_LIMIT > file_length) {
				line_end = msg->loc.byte_to + LINE_CONTEXT_LIMIT;
			} else {
				line_end = file_length;
				ellipse_back = false;
			}

			size_t buffer_begin  = line_begin;
			size_t buffer_length = line_end - line_begin;
			char buffer[buffer_length];

			fseek(fp, line_begin, SEEK_SET);
			fread(buffer, 1, buffer_length, fp);

			for (ssize_t n = msg->loc.byte_from; n >= (ssize_t)buffer_begin; n--) {
				// TODO: Handle all kinds of line break.
				if (buffer[n - buffer_begin] == '\n') {
					line_begin = n + 1;
					ellipse_front = false;
					break;
				}
			}

			for (size_t n = msg->loc.byte_to; n < buffer_begin + buffer_length; n++) {
				// TODO: Handle all kinds of line break.
				if (buffer[n - buffer_begin] == '\n') {
					line_end = n;
					ellipse_back = false;
					break;
				}
			}

			struct string prefix, token, postfix;

			prefix.text = &buffer[line_begin - buffer_begin];
			prefix.length = msg->loc.byte_from - line_begin;

			token.text = &buffer[msg->loc.byte_from - buffer_begin];
			token.length = msg->loc.byte_to - msg->loc.byte_from;

			postfix.text = &buffer[msg->loc.byte_to - buffer_begin];
			postfix.length = line_end - msg->loc.byte_to;

			int linum_len = (int)log10((double)msg->loc.line_to) + 1;

			fprintf(out, TC(TC_BRIGHT_WHITE, "%*u ") " ", linum_len, msg->loc.line_from);

			if (ellipse_front) {
				fprintf(out, " ... ");
			}

			fprintf(out, "%.*s" TC(TC_BRIGHT_BLUE, "%.*s") "%.*s",
				LIT(prefix), LIT(token), LIT(postfix));

			if (ellipse_back) {
				fprintf(out, " ... ");
			}

			fprintf(out, "\n\n");

			fclose(fp);

		} else if (file_error) {
			perror("read file");
		}

		fprintf(out, "%.*s\n\n", LIT(msg->msg));
	}
}

void
stg_error_destroy(struct stg_error_context *ctx)
{
	free(ctx->msgs);
	free(ctx->file_names);
}
