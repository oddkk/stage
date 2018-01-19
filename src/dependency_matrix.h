#ifndef STAGE_DEPENDENCY_MATRIX_H
#define STAGE_DEPENDENCY_MATRIX_H

#include "intdef.h"

struct dependency_matrix {
	uint32_t *matrix;
	size_t num_channels;
};

int dependency_matrix_init(struct dependency_matrix *dep_matrix, size_t num_channels);
int dependency_matrix_bind(struct dependency_matrix *dep_matrix, size_t from, size_t to);
int dependency_matrix_list_dependent(struct dependency_matrix *dep_matrix, size_t dependent_on, size_t *i);

void dependency_matrix_print(struct dependency_matrix *dep_matrix);

#endif
