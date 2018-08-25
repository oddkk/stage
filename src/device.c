#include "device.h"
#include "stage.h"
#include "utils.h"
#include <stdlib.h>


struct device *register_device(struct stage *stage, device_type_id type,
							   struct scoped_hash *parent_scope,
							   struct atom *name,
							   struct value_ref args)
{
	return register_device_with_context(stage, type, parent_scope, name, args, NULL);
}

struct device *register_device_with_context(struct stage *stage, device_type_id type,
											struct scoped_hash *parent_scope,
											struct atom *name,
											struct value_ref args,
											void *context)
{
	struct device *device;
	struct device_type *device_type;

	device_type = get_device_type(stage, type);
	if (!device_type) {
		return NULL;
	}

	if (!device_type->finalized) {
		print_error("register device",
					"Cannot register a device of a not-finalized device type.");
		return NULL;
	}

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
	device->data = NULL;

	device->id = stage->num_devices++;
	stage->devices[device->id] = device;

	device->scope =
	    scoped_hash_push(parent_scope, SCOPE_ENTRY_DEVICE, device->id);
	device->scope->owner = device->id;
	//device->scope->instance = device_type->scope;

	if (device->name) {
		struct scope_entry *entry;
		entry =
			scoped_hash_insert(parent_scope, device->name,
							   SCOPE_ENTRY_DEVICE, NULL,
							   device->scope);
		if (!entry) {
			print_error("register device", "Failed to insert device into scope.");
			return NULL;
		}
		entry->id = device->id;
	}

	int err = 0;

	if (device_type->params.type != 0) {

		err = resolve_templated_type_value(stage, device_type->params, args,
										   &device->args);

		if (err) {
			return NULL;
		}

		struct type *args_type;
		args_type = get_type(stage, device->args.type);

		assert(args_type->kind == TYPE_KIND_NAMED_TUPLE);

		size_t args_num_scalars = 0;
		for (size_t i = 0; i < args_type->named_tuple.length; i++) {
			struct named_tuple_member *arg;
			arg = &args_type->named_tuple.members[i];

			err = register_typed_member_in_scope(stage,
												 arg->name, arg->type,
												 device->scope,
												 SCOPE_ENTRY_DEVICE_ATTRIBUTE,
												 args_num_scalars);

			if (err < 0) {
				return NULL;
			}

			args_num_scalars += err;
		}
	}

	err = 0;


	if (device_type->num_inputs > 0) {
		device->input_types = calloc(device_type->num_inputs, sizeof(type_id));
		if (!device->input_types) {
			printf("Out of memory!");
			// @TODO: Deallocate
			return NULL;
		}

		for (size_t i = 0; i < device_type->num_inputs; i++) {
			device->input_types[i] = device_type->inputs[i].type;
		}
	}

	if (device_type->num_outputs > 0) {
		device->output_types = calloc(device_type->num_outputs, sizeof(type_id));
		if (!device->output_types) {
			printf("Out of memory!");
			// @TODO: Deallocate
			return NULL;
		}

		for (size_t i = 0; i < device_type->num_outputs; i++) {
			device->output_types[i] = device_type->outputs[i].type;
		}
	}

	if (device_type->takes_context && device_type->device_context_template_init != NULL) {
		err = device_type->device_context_template_init(stage, device_type, device, context);
	} else if (device_type->device_template_init != NULL) {
		err = device_type->device_template_init(stage, device_type, device);
	}

	if (err) {
		printf("Could not initialize device templates for '%.*s'!\n",
			   ALIT(device_type->name));
		// @TODO: Deallocate
		return NULL;
	}

	if (device_type->num_inputs > 0) {
		for (size_t i = 0; i < device_type->num_inputs; i++) {
			struct type *type;

			type = get_type(stage, device->input_types[i]);
			if (!type) {
				print_error("finalize device",
							"Missing type for the input '%.*s' for device "
							"'%.*s' was not resolved.",
							ALIT(device_type->inputs[i].name),
							ALIT(device->name));
				err = -1;
			} else if (type->templated) {
				print_error("finalize device",
							"The type for the input '%.*s' for device "
							"'%.*s' was not resolved.",
							ALIT(device_type->inputs[i].name),
							ALIT(device->name));
				err = -1;
			}
		}
	}

	if (device_type->num_outputs > 0) {
		for (size_t i = 0; i < device_type->num_outputs; i++) {
			struct type *type;

			type = get_type(stage, device->output_types[i]);
			if (!type) {
				print_error("finalize device",
							"Missing type for the output '%.*s' for device "
							"'%.*s' was not resolved.",
							ALIT(device_type->outputs[i].name),
							ALIT(device->name));
				err = -1;
			} else if (type->templated) {
				print_error("finalize device",
							"The type for the output '%.*s' for device "
							"'%.*s' was not resolved.",
							ALIT(device_type->outputs[i].name),
							ALIT(device->name));
				err = -1;
			}
		}
	}

	if (err) {
		return NULL;
	}

	err = allocate_device_channels(stage, device->id);
	if (err) {
		printf("Could not allocated channels for device '%.*s'!\n",
				ALIT(device_type->name));
		// @TODO: Deallocate
		return NULL;
	}

	if (device_type->takes_context && device_type->device_context_init != NULL) {
		err = device_type->device_context_init(stage, device_type, device, context);
	} else if (device_type->device_init != NULL) {
		err = device_type->device_init(stage, device_type, device);
	}

	if (err) {
		printf("Could not initialize device '%.*s'!\n",
			   ALIT(device_type->name));
		// @TODO: Deallocate
		return NULL;
	}

	return device;

}

/* int device_assign_input_type_by_name(struct stage *stage, */
/* 									 struct device *dev, */
/* 									 struct atom *name, */
/* 									 type_id type) */
/* { */
/* 	struct device_type *dev_type; */
/* 	dev_type = get_device_type(stage, dev->type); */

/* 	channel_id id; */
/* 	id = device_type_get_input_id(stage, dev_type, name); */

/* 	if (id < 0) { */
/* 		return -1; */
/* 	} */

/* 	if (dev->input_types[id] != TYPE_TEMPLATE) { */
/* 		if (dev_type->inputs[id].type == TYPE_TEMPLATE) { */
/* 			printf("The type of the templated input '%.*s' was already assigned to.\n", */
/* 				   ALIT(dev_type->inputs[id].name)); */
/* 		} else { */
/* 			printf("Attempted to assign a new type to the non-templated input '%.*s'.\n", */
/* 				   ALIT(dev_type->inputs[id].name)); */
/* 		} */

/* 		return -1; */
/* 	} */

/* 	dev->input_types[id] = type; */

/* 	return 0; */
/* } */

struct scope_lookup device_lookup(struct stage *stage, struct device *device)
{
	return scope_lookup_init(stage, device->scope);
}

struct value_ref device_get_arg_from_lookup(struct stage *stage,
											struct device *device,
											struct scope_lookup_range range)
{
	struct value_ref result = {0};

	struct type *args_type;
	args_type = get_type(stage, device->args.type);

	assert(range.owner != device->id);
	assert(range.begin + range.length < args_type->num_scalars);

	result.type = range.type->id;
	result.data = &device->args.data[range.begin];

	return result;
}

channel_id device_get_channel_id(struct stage * stage,
				       struct device * device,
				       struct atom * name)
{
	assert(stage != NULL);
	assert(device != NULL);
	assert(name != NULL);

	struct device_type *type;	// @TODO: Is this necessary?
	struct scope_entry entry;
	int err;

	type = stage->device_types[device->type];

	err = scoped_hash_local_lookup(device->scope, name, &entry);
	if (err) {
		print_error("device get channel",
			    "The device of type '%.*s' has no input '%.*s'.",
			    ALIT(type->name), ALIT(name));
		return -1;
	}

	if (entry.kind != SCOPE_ENTRY_DEVICE_CHANNEL) {
		print_error("device get channel",
			    "The element '%.*s' on the device '%.*s' is not an input.",
			    ALIT(name), ALIT(type->name));
		return -1;
	}

	return entry.id;
}

channel_id device_get_input_channel_id(struct stage * stage,
				       struct device * device,
				       struct atom * name)
{
	channel_id channel;

	channel = device_get_channel_id(stage, device, name);

	if (channel < 0) {
		return channel;
	}

	if (channel > stage->num_channels) {
		return -1;
	}

	struct channel *cnl;
	cnl = &stage->channels[channel];

	if (cnl->device_channel != DEVICE_CHANNEL_INPUT) {
		struct device_type *type;

		type = get_device_type(stage, device->type);
		print_error("device get channel",
			    "The element '%.*s' on the device '%.*s' is not an input.",
			    ALIT(name), ALIT(type->name));
		return -1;
	}

	return channel;
}

channel_id device_get_input_channel_id_by_name(struct stage * stage,
					       struct device * device,
					       struct string name)
{
	struct atom *atom;

	atom = atom_create(&stage->atom_table, name);
	return device_get_input_channel_id(stage, device, atom);
}

channel_id device_get_output_channel_id(struct stage * stage,
				       struct device * device,
				       struct atom * name)
{
	channel_id channel;

	channel = device_get_channel_id(stage, device, name);

	if (channel < 0) {
		return channel;
	}

	if (channel > stage->num_channels) {
		return -1;
	}

	struct channel *cnl;
	cnl = &stage->channels[channel];

	if (cnl->device_channel != DEVICE_CHANNEL_OUTPUT) {
		struct device_type *type;

		type = get_device_type(stage, device->type);
		print_error("device get channel",
			    "The element '%.*s' on the device '%.*s' is not an output.",
			    ALIT(name), ALIT(type->name));
		return -1;
	}

	return channel;
}

channel_id device_get_output_channel_id_by_name(struct stage * stage,
						struct device * device,
						struct string name)
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

	fprintf(fp, "device ");
	print_full_entry_name(stage, dev->scope);
	fprintf(fp, "\n type: %.*s\n", ALIT(dev_type->name));

	fprintf(fp, " arguments:\n");
	if (dev->args.type) {
		print_value_ref(stage, dev->args);
		fprintf(fp, "\n");
	}

	fprintf(fp, " inputs:\n");

	current_channel = dev->input_begin;
	for (size_t i = 0; i < dev_type->num_inputs; ++i) {
		struct device_channel_def *input;
		channel_id channel_begin;
		channel_id c_channel;

		input = &dev_type->inputs[i];
		fprintf(fp, " - %i: %.*s", input->id, ALIT(input->name));

		channel_begin = current_channel;

		while (stage->channels[current_channel].device.id == dev->id &&
		       stage->channels[current_channel].device.channel_id == i &&
			   stage->channels[current_channel].device_channel == DEVICE_CHANNEL_INPUT) {
			current_channel += 1;
		}

		if (channel_begin == current_channel - 1) {
			fprintf(fp, " (%i)", channel_begin);
		} else if (current_channel > channel_begin) {
			fprintf(fp, " (%i..%i)", channel_begin,
				current_channel - 1);
		}

		fprintf(fp, "\n");

		c_channel = channel_begin;
		while (c_channel < stage->num_channels &&
		       stage->channels[c_channel].device.id == dev->id &&
		       stage->channels[c_channel].device.channel_id == i &&
			   stage->channels[c_channel].device_channel == DEVICE_CHANNEL_INPUT) {
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

		struct channel *cnl = &stage->channels[current_channel];

		while (cnl->device.id == dev->id &&
		       cnl->device.channel_id == i &&
			   cnl->device_channel != DEVICE_CHANNEL_OUTPUT) {
			current_channel += 1;
			cnl = &stage->channels[current_channel];
		}

		if (channel_begin == current_channel - 1) {
			fprintf(fp, " (%i)", channel_begin);
		} else if (current_channel > channel_begin) {
			fprintf(fp, " (%i..%i)", channel_begin,
				current_channel - 1);
		}

		fprintf(fp, "\n");

		c_channel = channel_begin;
		while (c_channel < stage->num_channels &&
		       stage->channels[c_channel].device.id == dev->id &&
		       stage->channels[c_channel].device.channel_id == i &&
			   stage->channels[current_channel].device_channel == DEVICE_CHANNEL_OUTPUT) {
			fprintf(fp, "  - %i <- ", c_channel);
			channel_describe_connection(stage, c_channel);
			c_channel += 1;
		}
	}
}
