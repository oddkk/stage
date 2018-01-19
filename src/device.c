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
	int err;

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

	device->scope.self = device_type->scope;

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
			struct scope_entry attr_entry;

			attr = &attributes[i];

			err = scoped_hash_lookup(device_type->scope, attr->name, &attr_entry);
			if (err) {
				print_error("register device", "Device of type '%.*s' does not hav an attribute '%.*s'.\n",
							ALIT(device_type->name), ALIT(attr->name));
				continue;
			}
			if (attr_entry.kind != SCOPE_ENTRY_DEVICE_ATTRIBUTE) {
				print_error("register device", "Can not assign an attribute value to '%.*s' because it is not an attribute.\n",
							ALIT(attr->name));
				continue;
			}

			printf("Adding attribute %.*s\n", ALIT(attr->name));

			/* pos = */
			/*     id_lookup_table_lookup(&device_type->attribute_ids, */
			/* 			   attr->name); */

			/* if (pos >= 0) { */
			/* 	/\* struct attribute_value *attr_val = &device->attributes[pos]; *\/ */
			/* 	/\* assign_value(&attr_val->value, &attr_val->type, *\/ */
			/* 	/\*                       &attr->value, &attr->type); *\/ */
			/* 	/\* device->attributes[pos].type = attr->type; *\/ */
			/* 	/\* device->attributes[pos].value = attr->value; *\/ */
			/* } */
		}
	}

	err = allocate_device_channels(stage, device->id);
	if (err) {
		// @TODO: Deallocate
		return 0;
	}

	return device;
}

struct attribute_value *device_get_attr(struct stage *stage, struct device *device, struct atom *attr_name)
{
	struct device_type *type; // @TODO: Is this necessary?
	struct scope_entry entry;
	int err;

	type = stage->device_types[device->type];

	err = instanced_scoped_hash_lookup(device->scope, attr_name, &entry);
	if (err) {
		print_error("device get attr", "The device of type '%.*s' has no attribute '%.*s'.",
					ALIT(type->name), ALIT(attr_name));
		return NULL;
	}

	if (entry.kind != SCOPE_ENTRY_DEVICE_ATTRIBUTE) {
		print_error("device get attr", "The element '%.*s' on the device '%.*s' is not an attribute.",
					ALIT(attr_name), ALIT(type->name));
		return NULL;
	}
	assert(entry.id < type->num_attributes);
	return &device->attributes[entry.id];
}

void describe_device(struct stage *stage, struct device *dev)
{
	channel_id current_channel;
	struct device_type *dev_type;
	FILE *fp = stdout;

	assert(dev->type < stage->num_device_types);
	dev_type = stage->device_types[dev->type];

	fprintf(fp, "device %.*s\n", ALIT(dev->name));
	fprintf(fp, " type: %.*s\n", ALIT(dev_type->name));

	fprintf(fp, " attributes:\n");
	for (size_t i = 0; i < dev_type->num_attributes; ++i) {
		struct device_attribute_def *attr;
		struct attribute_value *value;
		attr = &dev_type->attributes[i];
		value = &dev->attributes[i];
		fprintf(fp, " - %i: %.*s\n", attr->id, ALIT(attr->name));
	}

	fprintf(fp, " inputs:\n");

	current_channel = dev->input_begin;
	for (size_t i = 0; i < dev_type->num_inputs; ++i) {
		struct device_channel_def *input;
		channel_id channel_begin;

		input = &dev_type->inputs[i];
		fprintf(fp, " - %i: %.*s", input->id, ALIT(input->name));

		channel_begin= current_channel;

		while (stage->channels[current_channel].device.id == dev->id &&
			   stage->channels[current_channel].device.channel_id == i) {
			current_channel += 1;
		}

		if (channel_begin == current_channel - 1) {
			fprintf(fp, " (%i)", channel_begin);
		} else if (current_channel > channel_begin) {
			fprintf(fp, " (%i..%i)", channel_begin, current_channel - 1);
		}

		fprintf(fp, "\n");
	}

	fprintf(fp, " output:\n");
	current_channel = dev->output_begin;
	for (size_t i = 0; i < dev_type->num_outputs; ++i) {
		struct device_channel_def *output;
		channel_id channel_begin;

		output = &dev_type->outputs[i];
		fprintf(fp, " - %i: %.*s", output->id, ALIT(output->name));

		channel_begin = current_channel;

		while (stage->channels[current_channel].device.id == dev->id &&
			   stage->channels[current_channel].device.channel_id == i) {
			current_channel += 1;
		}

		if (channel_begin == current_channel - 1) {
			fprintf(fp, " (%i)", channel_begin);
		} else if (current_channel > channel_begin) {
			fprintf(fp, " (%i..%i)", channel_begin, current_channel - 1);
		}

		fprintf(fp, "\n");
	}
}
