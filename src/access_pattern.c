#include "access_pattern.h"
#include "dlist.h"

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

void print_access_pattern(FILE *fp, struct access_pattern pat)
{
	for (size_t i = 0; i < pat.num_entries; i++) {
		struct access_pattern_entry *entry;
		entry = &pat.entries[i];
		switch (entry->kind) {
		case ACCESS_IDENT:
			fprintf(fp, "%.*s", ALIT(entry->ident));
			if (i != 0) {
				fprintf(fp, ".");
			}
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
