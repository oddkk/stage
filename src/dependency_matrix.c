#include "dependency_matrix.h"
#include <stdlib.h>
#include "utils.h"

int dependency_matrix_init(struct dependency_matrix *dep_matrix, size_t num_channels)
{
	dep_matrix->num_channels = num_channels;
	dep_matrix->matrix = calloc(((num_channels * num_channels) / 32) + 1, sizeof(uint32_t));
	if (!dep_matrix->matrix) {
		return -1;
	}
	return 0;
}

int dependency_matrix_bind(struct dependency_matrix *dep_matrix, size_t dependent_on, size_t dependency)
{
	size_t pos;

	// @TODO: This should also mark all the transitive clojures as
	// this dependency.

	assert(dependency   < dep_matrix->num_channels &&
		   dependent_on < dep_matrix->num_channels);

	pos = dep_matrix->num_channels * dependency + dependent_on;
	
	dep_matrix->matrix[pos / 32] |= (1 << (pos % 32));

	return 0;
}

int dependency_matrix_list_dependent(struct dependency_matrix *dep_matrix, size_t dependent_on, size_t *i)
{
	size_t row_begin = dep_matrix->num_channels * dependent_on;
	if (*i < row_begin) {
		*i = row_begin;
	} else {
		*i += 1;
	}

	while (*i < (row_begin + dep_matrix->num_channels)) {
		uint32_t block_id = *i / 32;
		uint32_t current = dep_matrix->matrix[block_id];

		if (current > 0) {
			current >>= (*i) % 32;
			while ((*i / 32) == block_id) {
				if ((current & 0x1) == 0x1) {
					return *i;
				}

				*i += 1;
				current >>= 1;
			}
		} else {
			*i += 32;
		}
	}

	return -1;
}

int dependency_matrix_list_dependencies(struct dependency_matrix *dep_matrix, size_t depends_on, size_t *i)
{
	for (; *i < dep_matrix->num_channels; ++(*i)) {
		size_t pos = depends_on + (*i) * dep_matrix->num_channels;
		if (dep_matrix->matrix[pos / 32] & (1 << (pos % 32))) {
			size_t c = *i;
			*i += 1;
			return c;
		}
	}

	return -1;
}

void dependency_matrix_print(struct dependency_matrix *dep_matrix)
{
	printf("   ");
	for (size_t x = 0; x < dep_matrix->num_channels; ++x) {
		printf("%-2zu", x);
	}
	printf("\n");
	for (size_t y = 0; y < dep_matrix->num_channels; ++y) {
		printf("%-2zu", y);
		for (size_t x = 0; x < dep_matrix->num_channels; ++x) {
			size_t pos = y * dep_matrix->num_channels + x;
			uint32_t block = dep_matrix->matrix[pos / 32];

			if (((block >> (pos % 32)) & 0x1) > 0) {
				printf(" x");
			} else {
				printf("  ");
			}
		}
		printf("\n");
	}
}

void dependency_matrix_describe(struct dependency_matrix *dep_matrix)
{
	for (size_t i = 0; i < dep_matrix->num_channels; ++i) {
		size_t iter = 0;
		bool header_printed = false;
		int channel;

		while ((channel = dependency_matrix_list_dependencies(dep_matrix, i, &iter)) >= 0) {
			if (!header_printed) {
				printf("channel %zu:\n", i);
				header_printed = true;
			}
			printf(" - %i\n", channel);
		}
		if (header_printed) {
			printf("\n");
		}
	}
}
