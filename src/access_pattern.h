#ifndef STAGE_ACCESS_PATTERN_H
#define STAGE_ACCESS_PATTERN_H

#include <stdio.h>
#include "atom.h"

enum access_pattern_entry_kind {
	ACCESS_IDENT,
	ACCESS_INDEX,
	ACCESS_RANGE,
};

struct access_pattern_entry {
	enum access_pattern_entry_kind kind;
	union {
		struct atom *ident;
		size_t index;
		struct {
			size_t begin;
			size_t end;
		} range;
	};
};

struct access_pattern {
	struct access_pattern_entry *entries;
	size_t num_entries;
};

void access_pattern_ident(struct access_pattern *,  struct atom *name);
void access_pattern_index(struct access_pattern *,  size_t i);
void access_pattern_range(struct access_pattern *,  size_t begin, size_t end);

void print_access_pattern(FILE *fp, struct access_pattern);

#endif
