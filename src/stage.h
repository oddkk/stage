#ifndef STAGE_STAGE_H
#define STAGE_STAGE_H

/* #include "device_type.h" */
/* #include "device.h" */
/* #include "channel.h" */
#include "arena.h"
#include "idlookuptable.h"

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
};

#endif
