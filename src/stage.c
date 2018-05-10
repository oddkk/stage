#include "stage.h"
#include "intdef.h"
#include "string.h"
#include "arena.h"
#include "utils.h"
#include "type.h"
#include "channel.h"

#include <stdlib.h>
#include <string.h>

int stage_init(struct stage *stage)
{
	struct scalar_type int_type;
	int error;

	int_type.min = SCALAR_MIN;
	int_type.max = SCALAR_MAX;

	zero_memory(stage, sizeof(struct stage));
	error = arena_init(&stage->memory, MEGABYTE(10));

	if (error) {
		return -1;
	}

	stage->device_types_lookup.string_arena = &stage->memory;
	stage->device_types_lookup.page_arena = &stage->memory;
	stage->types_lookup.string_arena = &stage->memory;
	stage->types_lookup.page_arena = &stage->memory;

	stage->cap_channels = 32;
	stage->channels =
	    arena_alloc(&stage->memory,
			sizeof(struct channel) * stage->cap_channels);
	dependency_matrix_init(&stage->channel_deps, stage->cap_channels);

	stage->atom_table.string_arena = &stage->memory;
	atom_table_rehash(&stage->atom_table, 64);

	stage->root_scope.parent = 0;
	stage->root_scope.lookup.page_arena = &stage->memory;
	stage->root_scope.lookup.string_arena = &stage->memory;



	return 0;
}

struct device *get_device(struct stage *stage, device_id dev_id)
{
	struct device *res;

	if (dev_id >= stage->num_devices) {
		print_error("get type", "Invalid device id '%i'.", dev_id);
		return NULL;
	}
	res = stage->devices[dev_id];

	return res;
}

struct device_type *get_device_type(struct stage *stage, device_id dev_type_id)
{
	struct device_type *res;

	if (dev_type_id >= stage->num_device_types) {
		print_error("get type", "Invalid device type id '%i'.", dev_type_id);
		return NULL;
	}
	res = stage->device_types[dev_type_id];

	return res;
}

struct type *get_type(struct stage *stage, type_id tid)
{
	struct type *res;

	if (tid >= stage->num_types) {
		print_error("get type", "Invalid type id '%i'.", tid);
		return NULL;
	}
	res = stage->types[tid];

	return res;
}
