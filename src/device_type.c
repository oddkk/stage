#include "device_type.h"
#include "dlist.h"
#include "utils.h"
#include "stage.h"

#include <stdio.h>
#include <stdlib.h>

struct device_attribute_def *device_type_add_attribute(struct stage *stage,
													   struct device_type
													   *dev_type,
													   struct string name,
													   type_id type,
													   struct value def)
{
	int err;
	struct device_attribute_def attr;

	attr.id = dev_type->num_attributes;
	attr.name = atom_create(&stage->atom_table, name);;
	attr.type = type;
	attr.def = def;

	err = scoped_hash_insert(dev_type->scope, attr.name, SCOPE_ENTRY_DEVICE_ATTRIBUTE, attr.id, NULL, NULL);

	if (err) {
		print_error("device type add attribute",
			    "Failed to add attribute '%.*s' to device type '%.*s' because "
			    "this attribute already exists.", LIT(name), ALIT(dev_type->name));
		return NULL;
	}

	dlist_append(dev_type->attributes, dev_type->num_attributes, &attr);

	return &dev_type->attributes[attr.id];
}

struct device_channel_def *device_type_add_input(struct stage *stage,
											   struct device_type *dev_type,
											   struct string name, type_id type)
{
	int err;
	struct device_channel_def input;
	input.id = dev_type->num_inputs;
	input.name = atom_create(&stage->atom_table, name);
	input.type = type;

	err = scoped_hash_insert(dev_type->scope, input.name, SCOPE_ENTRY_DEVICE_INPUT, input.id, NULL, NULL);

	if (err) {
		print_error("device type add input",
			    "Failed to add input '%.*s' to device type '%.*s' because "
			    "this input already exists.", LIT(name),
			    ALIT(dev_type->name));
		return NULL;
	}

	dlist_append(dev_type->inputs, dev_type->num_inputs, &input);

	return &dev_type->inputs[input.id];
}

struct device_channel_def *device_type_add_output(struct stage *stage, struct device_type *dev_type,
												 struct string name,
												 type_id type)
{
	int err;
	struct device_channel_def output;
	output.id = dev_type->num_outputs;
	output.name = atom_create(&stage->atom_table, name);
	output.type = type;

	err = scoped_hash_insert(dev_type->scope, output.name, SCOPE_ENTRY_DEVICE_OUTPUT, output.id, NULL, NULL);

	if (err) {
		print_error("device type add output",
			    "Failed to add output '%.*s' to device type '%.*s' because "
			    "this output already exists.", LIT(name),
			    ALIT(dev_type->name));
		return NULL;
	}

	dlist_append(dev_type->outputs, dev_type->num_outputs, &output);

	return &dev_type->outputs[output.id];
}

struct device_type *register_device_type(struct stage *stage,
										 struct string name)
{
	return register_device_type_scoped(stage, name, &stage->root_scope);
}

struct device_type *register_device_type_scoped(struct stage *stage,
										 struct string name, struct scoped_hash *parent_scope)
{
	struct device_type *dev_type;
	int old_id, err;

	if (name.length == 0) {
		print_error("register device type",
			    "Device type must have name.", LIT(name));
		return 0;
	}

	old_id = id_lookup_table_lookup(&stage->device_types_lookup, name);
	if (old_id >= 0) {
		print_error("register device type",
			    "Cannot register device '%.*s' because "
			    "the name is already registered.", LIT(name));
		return 0;
	}

	if (stage->num_device_types + 1 >= stage->cap_device_types) {
		struct device_type **new_array;
		stage->cap_device_types += 10;

		// TODO: Use a different allocation schema.
		new_array = realloc(stage->device_types,
				    stage->cap_device_types *
				    sizeof(struct device_type *));
		if (!new_array) {
			print_error("register device type",
				    "Failed to allocate memory for device type list");
			return 0;
		}
		stage->device_types = new_array;
	}

	dev_type = arena_alloc(&stage->memory, sizeof(struct device_type));

	dev_type->id = stage->num_device_types++;
	dev_type->name = atom_create(&stage->atom_table, name);
	dev_type->scope = scoped_hash_push(parent_scope);

	stage->device_types[dev_type->id] = dev_type;

	err =
	    id_lookup_table_insert(&stage->device_types_lookup, name,
				   dev_type->id);

	if (err) {
		return 0;
	}

	err = scoped_hash_insert(&stage->root_scope,
							   atom_create(&stage->atom_table, name),
							   SCOPE_ENTRY_DEVICE_TYPE, dev_type->id,
							   NULL, NULL);

	if (err) {
		return 0;
	}

	return dev_type;
}

void describe_device_type(struct stage *stage, struct device_type *dev_type)
{
	FILE *fp = stdout;
	fprintf(fp, "device type %.*s\n", ALIT(dev_type->name));

	fprintf(stdout, " attributes:\n");
	for (size_t i = 0; i < dev_type->num_attributes; ++i) {
		struct device_attribute_def *attr;
		attr = &dev_type->attributes[i];
		fprintf(fp, " - %i: %.*s\n", attr->id, ALIT(attr->name));
	}

	fprintf(stdout, " inputs:\n");
	for (size_t i = 0; i < dev_type->num_inputs; ++i) {
		struct device_channel_def *input;
		input = &dev_type->inputs[i];
		fprintf(fp, " - %i: %.*s\n", input->id, ALIT(input->name));
	}

	fprintf(stdout, " output:\n");
	for (size_t i = 0; i < dev_type->num_outputs; ++i) {
		struct device_channel_def *output;
		output = &dev_type->outputs[i];
		fprintf(fp, " - %i: %.*s\n", output->id, ALIT(output->name));
	}
}
