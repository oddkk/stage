#ifndef STAGE_STAGE_H
#define STAGE_STAGE_H

/* #include "device_type.h" */
/* #include "device.h" */
/* #include "channel.h" */
#include "arena.h"
#include "atom.h"
#include "idlookuptable.h"
#include "scoped_hash.h"
#include "dependency_matrix.h"

typedef unsigned int device_type_id;
typedef unsigned int device_id;
typedef unsigned int type_id;
typedef int channel_id;

struct device_type;
struct device;
struct type;
struct channel;

struct stage {
	struct arena memory;

	struct id_lookup_table device_types_lookup;

	struct device_type **device_types;
	size_t num_device_types;
	size_t cap_device_types;

	struct device **devices;
	size_t num_devices;
	size_t cap_devices;

	struct id_lookup_table types_lookup;
	struct type **types;
	size_t num_types;
	size_t cap_types;

	struct {
		type_id integer;
		type_id string;
		//type_id any;
	} standard_types;

	struct channel *channels;
	size_t num_channels;
	size_t cap_channels;

	struct dependency_matrix channel_deps;

	struct scoped_hash root_scope;

	struct atom_table atom_table;

	uint64_t tick;
	// Tick duration in nanoseconds (10e-9 s)
	uint64_t tick_period;
};

int stage_init(struct stage *stage);

struct device *get_device(struct stage *stage, device_id);
struct device_type *get_device_type(struct stage *stage, device_type_id);
struct type *get_type(struct stage *stage, type_id);

#endif
