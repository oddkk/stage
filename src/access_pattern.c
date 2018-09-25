#include "access_pattern.h"
#include "dlist.h"
#include <ctype.h>

void access_pattern_ident(struct access_pattern *pattern,  struct atom *name)
{
	struct access_pattern_entry entry = {0};
	entry.kind = ACCESS_IDENT;
	entry.ident = name;

	dlist_append(pattern->entries, pattern->num_entries, &entry);
}

void access_pattern_index(struct access_pattern *pattern,  size_t i)
{
	struct access_pattern_entry entry = {0};
	entry.kind = ACCESS_INDEX;
	entry.index = i;

	dlist_append(pattern->entries, pattern->num_entries, &entry);
}

void access_pattern_range(struct access_pattern *pattern,  size_t begin, size_t end)
{
	struct access_pattern_entry entry = {0};
	entry.kind = ACCESS_RANGE;
	entry.range.begin = begin;
	entry.range.end = end;

	dlist_append(pattern->entries, pattern->num_entries, &entry);
}

enum access_pattern_parse_state {
	ACCESS_PARSE_IDLE,
	ACCESS_PARSE_IDENT,
	ACCESS_PARSE_INDEX,
};

int parse_access_pattern(struct atom_table *atom_table, struct string str, struct access_pattern *out)
{
	char *iter = str.text;
	enum access_pattern_parse_state state;
	state = ACCESS_PARSE_IDENT;

	char *token_begin = iter;

	for (; iter < str.text + str.length; iter++) {
		switch (state) {
		case ACCESS_PARSE_IDLE: {
			switch (*iter) {
			case '.':
				state = ACCESS_PARSE_IDENT;
				token_begin = iter + 1;
				break;

			case '[':
				state = ACCESS_PARSE_INDEX;
				token_begin = iter + 1;
				break;

			default:
				if (isalpha(*iter) || *iter == '_') {
					iter--;
					state = ACCESS_PARSE_IDENT;
				} else {
					printf("Invalid synatx in access pattern. Expected '[' or '.', got '%c'.\n",
						*iter);
					return -1;
				}
			}
		} break;

		case ACCESS_PARSE_IDENT: {
			if (!isalpha(*iter) && *iter != '_') {
				struct string token;
				token.text = token_begin;
				token.length = iter - token_begin;
				access_pattern_ident(out, atom_create(atom_table, token));

				iter--;
				state = ACCESS_PARSE_IDLE;
			}

		} break;

		case ACCESS_PARSE_INDEX:
			if (!isdigit(*iter)) {
				if (*iter != ']') {
					printf("Invalid synatx in access pattern. Expected ']', got '%c'.\n",
						   *iter);
					return -1;
				}

				struct string token;
				token.text = token_begin;
				token.length = iter - token_begin;

				int64_t index;
				index = string_to_int64_base10(token);
				access_pattern_index(out, index);

				state = ACCESS_PARSE_IDLE;
			}
			break;
		}
	}

	switch (state) {
	case ACCESS_PARSE_IDENT: {
		struct string token;
		token.text = token_begin;
		token.length = iter - token_begin;
		access_pattern_ident(out, atom_create(atom_table, token));
	} break;

	case ACCESS_PARSE_INDEX: {
		printf("Invalid synatx in access pattern. Expected ']', got eof.\n");
	} break;

	default:
		break;
	}

	return 0;
}

void print_access_pattern(FILE *fp, struct access_pattern pat)
{
	for (size_t i = 0; i < pat.num_entries; i++) {
		struct access_pattern_entry *entry;
		entry = &pat.entries[i];
		switch (entry->kind) {
		case ACCESS_IDENT:
			if (i != 0) {
				fprintf(fp, ".");
			}
			fprintf(fp, "%.*s", ALIT(entry->ident));
			break;
		case ACCESS_INDEX:
			fprintf(fp, "[%zu]", entry->index);
			break;
		case ACCESS_RANGE:
			fprintf(fp, "[%zu..%zu]", entry->range.begin, entry->range.end);
			break;
		}
	}
}
