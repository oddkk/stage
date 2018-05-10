#include "device.h"
#include "stage.h"
#include "utils.h"
#include <stdlib.h>

struct device *register_device_scoped(struct stage *stage, device_type_id type, struct atom *name,
									  struct scoped_hash *parent_scope,
									  struct device_attribute *attributes,
									  size_t num_attributes, void *data)
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
	device->name = name;
	device->data = data;

	device->id = stage->num_devices++;
	stage->devices[device->id] = device;

	device->scope = scoped_hash_push(parent_scope, SCOPE_ENTRY_DEVICE, device->id);
	device->scope->instance = device_type->scope;

	if (device->name) {
		scoped_hash_insert(parent_scope, device->name, SCOPE_ENTRY_DEVICE,
						   device->id, NULL, device->scope);
	}

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

			attr->value = def->def;
		}

		for (size_t i = 0; i < num_attributes; i++) {
			struct device_attribute *attr;
			struct scope_entry attr_entry;

			attr = &attributes[i];

			err = scoped_hash_local_lookup(device_type->scope, attr->name, &attr_entry);
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

			struct attribute_value *attr_val;
			attr_val = &device->attributes[attr_entry.id];

			attr_val->value = attr->value;
		}
	}

	err = allocate_device_channels(stage, device->id);
	if (err) {
		// @TODO: Deallocate
		return 0;
	}

	if (device_type->device_init) {
		err = device_type->device_init(stage, device_type, device);
		if (err) {
			// @TODO: Deallocate
			return 0;
		}
	}

	return device;
}

struct device *register_device(struct stage *stage, device_type_id type, struct atom *name,
							   struct device_attribute *attributes,
							   size_t num_attributes, void *data)
{
	return register_device_scoped(stage, type, name,
								  &stage->root_scope,
								  attributes, num_attributes,
								  data);
}

struct attribute_value *device_get_attr(struct stage *stage, struct device *device, struct atom *attr_name)
{
	struct device_type *type; // @TODO: Is this necessary?
	struct scope_entry entry;
	int err;

	type = stage->device_types[device->type];

	err = scoped_hash_local_lookup(device->scope, attr_name, &entry);
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

channel_id device_get_input_channel_id(struct stage *stage, struct device *device, struct atom *name)
{
	struct device_type *type; // @TODO: Is this necessary?
	struct scope_entry entry;
	channel_id result;
	int err;

	type = stage->device_types[device->type];

	err = scoped_hash_local_lookup(device->scope, name, &entry);
	if (err) {
		print_error("device get input", "The device of type '%.*s' has no input '%.*s'.",
					ALIT(type->name), ALIT(name));
		return -1;
	}

	if (entry.kind != SCOPE_ENTRY_DEVICE_INPUT) {
		print_error("device get input", "The element '%.*s' on the device '%.*s' is not an input.",
					ALIT(name), ALIT(type->name));
		return -1;
	}
	assert(entry.id < type->num_inputs);

	result = find_device_channel(stage, device, DEVICE_CHANNEL_INPUT, entry.id, 0);

	if (result >= 0) {
		return result;
	} else {
		print_error("device get input", "The device '%.*s' (id %i) of type '%.*s' (id %i) has an attribute "
					"'%.*s' (id %i), but no such channel is registered!",
					ALIT(device->name), device->id, ALIT(type->name), type->id,
					ALIT(entry.name), entry.id);
		return -1;
	}
}

channel_id device_get_input_channel_id_by_name(struct stage *stage, struct device *device, struct string name)
{
	struct atom *atom;

	atom = atom_create(&stage->atom_table, name);
	return device_get_input_channel_id(stage, device, atom);
}

channel_id device_get_output_channel_id(struct stage *stage, struct device *device, struct atom *name)
{
	struct device_type *type; // @TODO: Is this necessary?
	struct scope_entry entry;
	channel_id result;
	int err;

	type = stage->device_types[device->type];

	err = scoped_hash_local_lookup(device->scope, name, &entry);

	if (err) {
		print_error("device get output", "The device of type '%.*s' has no output '%.*s'.",
					ALIT(type->name), ALIT(name));
		return -1;
	}

	if (entry.kind != SCOPE_ENTRY_DEVICE_OUTPUT) {
		print_error("device get output", "The element '%.*s' on the device '%.*s' is not an output.",
					ALIT(name), ALIT(type->name));
		return -1;
	}
	assert(entry.id < type->num_outputs);

	result = find_device_channel(stage, device, DEVICE_CHANNEL_OUTPUT, entry.id, 0);

	if (result >= 0) {
		return result;
	} else {
		print_error("device get output", "The device '%.*s' (id %i) of type '%.*s' (id %i) has an attribute "
					"'%.*s' (id %i), but no such channel is registered!",
					ALIT(device->name), device->id, ALIT(type->name), type->id,
					ALIT(entry.name), entry.id);
		return -1;
	}
}

channel_id device_get_output_channel_id_by_name(struct stage *stage, struct device *device, struct string name)
{
	struct atom *atom;

	atom = atom_create(&stage->atom_table, name);
	return device_get_output_channel_id(stage, device, atom);
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
		fprintf(fp, " - %i: %.*s = %i\n", attr->id, ALIT(attr->name), value->value);
	}

	fprintf(fp, " inputs:\n");

	current_channel = dev->input_begin;
	for (size_t i = 0; i < dev_type->num_inputs; ++i) {
		struct device_channel_def *input;
		channel_id channel_begin;
		channel_id c_channel;

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

		c_channel = channel_begin;
		while (c_channel < stage->num_channels &&
			   stage->channels[c_channel].device.id == dev->id &&
			   stage->channels[c_channel].device.channel_id == i) {
			fprintf(fp, "  - %i <- ", c_channel);
			channel_describe_connection(stage, c_channel);
			c_channel += 1;
		}
	}

	fprintf(fp, " output:\n");
	current_channel = dev->output_begin;
	for (size_t i = 0; i < dev_type->num_outputs; ++i) {
		struct device_channel_def *output;
		channel_id channel_begin;
		channel_id c_channel;

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

		c_channel = channel_begin;
		while (c_channel < stage->num_channels &&
			   stage->channels[c_channel].device.id == dev->id &&
			   stage->channels[c_channel].device.channel_id == i) {
			fprintf(fp, "  - %i <- ", c_channel);
			channel_describe_connection(stage, c_channel);
			c_channel += 1;
		}
	}
}
