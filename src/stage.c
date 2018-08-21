#include "stage.h"
#include "intdef.h"
#include "string.h"
#include "arena.h"
#include "utils.h"
#include "type.h"
#include "channel.h"
#include "devices/devices.h"
#include "device.h"
#include "device_type.h"

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

	stage->cap_channels = 1024;
	stage->channels =
	    arena_alloc(&stage->memory,
			sizeof(struct channel) * stage->cap_channels);
	dependency_matrix_init(&stage->channel_deps, stage->cap_channels);

	stage->atom_table.string_arena = &stage->memory;
	atom_table_rehash(&stage->atom_table, 64);

	stage->root_scope.parent = 0;
	stage->root_scope.lookup.page_arena = &stage->memory;
	stage->root_scope.lookup.string_arena = &stage->memory;
	stage->root_scope.owner = -1;

	register_default_types(stage);
	register_device_types(stage);

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
		print_error("get type", "Invalid device type id '%i'.",
			    dev_type_id);
		return NULL;
	}
	res = stage->device_types[dev_type_id];

	return res;
}

struct type *get_type(struct stage *stage, type_id tid)
{
	struct type *res;

	if (tid >= stage->num_types) {
		if (tid == TYPE_TEMPLATE) {
			print_error("get type", "Type is a template that is not assigned.");
		} else {
			print_error("get type", "Invalid type id '%i'.", tid);
		}
		return NULL;
	}
	res = stage->types[tid];

	return res;
}

struct atom *stage_atom(struct stage *stage, struct string str)
{
	return atom_create(&stage->atom_table, str);
}

void register_device_tick_callback(struct stage *stage,
				   struct device *dev,
				   uint64_t tick, tick_callback callback)
{
	struct device_tick_callback *cb;

	cb = arena_alloc(&stage->memory, sizeof(struct device_tick_callback));

	cb->device = dev;
	cb->callback = callback;
	cb->next = stage->first_callback;
	stage->first_callback = cb;
}

void stage_tick(struct stage *stage)
{
	struct device_tick_callback *cb;
	stage->tick += 1;

	cb = stage->first_callback;
	while (cb) {
		cb->callback(stage, cb->device);
		cb = cb->next;
	}
}

struct atom *satom(struct stage *stage, struct string name)
{
	return atom_create(&stage->atom_table, name);
}

static bool print_full_entry_name_internal(struct stage *stage,
					   struct scoped_hash *entry)
{
	if (entry->parent) {
		if (print_full_entry_name_internal(stage, entry->parent)) {
			printf(".");
		}
	}

	switch (entry->kind) {
	case SCOPE_ENTRY_NONE:
		return false;

	case SCOPE_ENTRY_DEVICE:{
			struct device *dev;
			dev = get_device(stage, entry->id);
			printf("%.*s", ALIT(dev->name));
		} break;

	case SCOPE_ENTRY_DEVICE_TYPE:{
			struct device_type *dev_type;
			dev_type = get_device_type(stage, entry->id);
			printf("%.*s", ALIT(dev_type->name));
		} break;

	/* case SCOPE_ENTRY_DEVICE_INPUT:{ */
	/* 		struct device_type *dev_type; */
	/* 		assert(entry->parent); */
	/* 		dev_type = get_device_type(stage, entry->parent->id); */
	/* 		assert(entry->id < dev_type->num_inputs); */
	/* 		printf("%.*s", ALIT(dev_type->inputs[entry->id].name)); */
	/* 	} break; */

	/* case SCOPE_ENTRY_DEVICE_OUTPUT:{ */
	/* 		struct device_type *dev_type; */
	/* 		assert(entry->parent); */
	/* 		dev_type = get_device_type(stage, entry->parent->id); */
	/* 		assert(entry->id < dev_type->num_outputs); */
	/* 		printf("%.*s", ALIT(dev_type->outputs[entry->id].name)); */
	/* 	} break; */

	default:
		printf("(@TODO complete print_full_entry_name)");
		break;

	}

	return true;
}

void print_full_entry_name(struct stage *stage, struct scoped_hash *entry)
{
	print_full_entry_name_internal(stage, entry);
}
