#include "device.h"
#include "stage.h"
#include "utils.h"
#include <stdlib.h>

struct device *register_device(struct stage *stage, device_type_id type,
			       struct device_attribute *attributes,
			       size_t num_attributes)
{
	struct device *device;
	struct device_type *device_type;

	if (type >= stage->num_device_types) {
		print_error("register device",
			    "Attempting to register device with non-existent type id %i.",
			    type);
		return NULL;
	}

	device_type = stage->device_types[type];

	if (stage->num_devices + 1 >= stage->cap_devices) {
		struct device **new_array;
		stage->cap_devices += 10;

		// TODO: Use a different allocation schema.
		new_array = realloc(stage->devices,
				    stage->cap_devices *
				    sizeof(struct device *));
		if (!new_array) {
			print_error("register device",
				    "Failed to allocate memory for device list");
			return NULL;
		}
		stage->devices = new_array;
	}

	device = arena_alloc(&stage->memory, sizeof(struct device));
	device->type = type;

	device->id = stage->num_devices++;
	stage->devices[device->id] = device;

	if (device_type->num_attributes > 0) {
		device->attributes =
		    arena_alloc(&stage->memory,
				sizeof(struct attribute_value) *
				device_type->num_attributes);

		for (size_t i = 0; i < device_type->num_attributes; i++) {
			struct device_attribute_def *def;
			struct attribute_value *attr;

			def = &device_type->attributes[i];
			attr = &device->attributes[i];

			attr->type = def->type;
			attr->value = def->def;
		}

		for (size_t i = 0; i < num_attributes; i++) {
			struct device_attribute *attr;
			int pos;

			attr = &attributes[i];
			pos =
			    id_lookup_table_lookup(&device_type->attribute_ids,
						   attr->name);

			if (pos >= 0) {
				/* struct attribute_value *attr_val = &device->attributes[pos]; */
				/* assign_value(&attr_val->value, &attr_val->type, */
				/*                       &attr->value, &attr->type); */
				/* device->attributes[pos].type = attr->type; */
				/* device->attributes[pos].value = attr->value; */
			}
		}
	}

	device->output_begin = allocate_device_output_channels(stage, device->id);

	if (device->output_begin < 0) {
		print_error("register device", "Failed to allocate output channels for device %i "
					"with type %i.", device->id, device->type);
	}

	device->input_begin = allocate_device_input_channels(stage, device->id);

	if (device->output_begin < 0) {
		print_error("register device", "Failed to allocate input channels for device %i "
					"with type %i.", device->id, device->type);
	}

	return device;
}

struct attribute_value *device_get_attr(struct stage *stage, struct device *device, struct string attr_name)
{
	struct device_type *type;
	int attr_id;

	type = stage->device_types[device->type];
	attr_id = id_lookup_table_lookup(&type->attribute_ids, attr_name);

	if (attr_id < 0) {
		return 0;
	}

	return &device->attributes[attr_id];
}
