#include "device_type.h"
#include "dlist.h"
#include "utils.h"
#include "stage.h"

#include <stdio.h>
#include <stdlib.h>

struct device_attribute_def *device_type_add_attribute(struct stage *stage, struct device_type
						       *dev_type,
						       struct string name,
							scalar_value def,
							type_id type)
{
	struct device_attribute_def attr;

	if (dev_type->finalized) {
		printf("Cannot alter a finalized device type.\n");
		return NULL;
	}

	attr.id = dev_type->num_attributes;
	attr.name = atom_create(&stage->atom_table, name);;
	attr.def = def;
	attr.type = type;

	struct scope_entry *entry;
	entry =
	    scoped_hash_insert(dev_type->scope, attr.name,
			       SCOPE_ENTRY_DEVICE_ATTRIBUTE, NULL, NULL);
	if (!entry) {
		print_error("device type add attribute",
			    "Failed to add attribute '%.*s' to device type '%.*s' because "
			    "this attribute already exists.", LIT(name),
			    ALIT(dev_type->name));
		return NULL;
	}

	entry->id = attr.id;

	dlist_append(dev_type->attributes, dev_type->num_attributes, &attr);

	return &dev_type->attributes[attr.id];
}

struct device_channel_def *device_type_add_input(struct stage *stage,
						 struct device_type *dev_type,
						 struct string name,
						 type_id type)
{
	struct device_channel_def input;
	bool self = false;

	if (dev_type->finalized) {
		printf("Cannot alter a finalized device type.\n");
		return NULL;
	}

	input.id = dev_type->num_inputs;
	input.type = type;

	if (!name.text) {
		name = STR("self_input");
		self = true;
	}

	input.name = atom_create(&stage->atom_table, name);

	struct scope_entry *entry;
	entry =
	    scoped_hash_insert(dev_type->scope, input.name,
			       SCOPE_ENTRY_DEVICE_INPUT, NULL, NULL);
	if (!entry) {
		print_error("device type add input",
			    "Failed to add input '%.*s' to device type '%.*s' because "
			    "this input already exists.", LIT(name),
			    ALIT(dev_type->name));
		return NULL;
	}

	entry->id = input.id;

	dlist_append(dev_type->inputs, dev_type->num_inputs, &input);

	if (self) {
		dev_type->self_input = input.id;
	}

	return &dev_type->inputs[input.id];
}

struct device_channel_def *device_type_add_output(struct stage *stage,
						  struct device_type *dev_type,
						  struct string name,
						  type_id type)
{
	struct device_channel_def output;
	bool self = false;

	if (dev_type->finalized) {
		printf("Cannot alter a finalized device type.\n");
		return NULL;
	}

	output.id = dev_type->num_outputs;
	output.type = type;

	if (!name.text) {
		name = STR("self_output");
		self = true;
	}

	output.name = atom_create(&stage->atom_table, name);

	struct scope_entry *entry;
	entry =
	    scoped_hash_insert(dev_type->scope, output.name,
			       SCOPE_ENTRY_DEVICE_OUTPUT, NULL, NULL);
	if (!entry) {
		print_error("device type add output",
			    "Failed to add output '%.*s' to device type '%.*s' because "
			    "this output already exists.", LIT(name),
			    ALIT(dev_type->name));
		return NULL;
	}
	entry->id = output.id;

	dlist_append(dev_type->outputs, dev_type->num_outputs, &output);

	if (self) {
		dev_type->self_output = output.id;
	}

	return &dev_type->outputs[output.id];
}

int device_type_get_input_id(struct stage *stage,
							 struct device_type *type,
							 struct atom *name)
{
	for (size_t i = 0; i < type->num_inputs; i++) {
		if (type->inputs[i].name == name) {
			return i;
		}
	}

	return -1;
}

int device_type_get_output_id(struct stage *stage,
							  struct device_type *type,
							  struct atom *name)
{
	for (size_t i = 0; i < type->num_outputs; i++) {
		if (type->outputs[i].name == name) {
			return i;
		}
	}

	return -1;
}

struct device_type *register_device_type(struct stage *stage,
					 struct string name)
{
	return register_device_type_scoped(stage, name, &stage->root_scope);
}

struct device_type *register_device_type_scoped(struct stage *stage,
						struct string name,
						struct scoped_hash
						*parent_scope)
{
	struct device_type *dev_type;
	int err;

	if (name.length == 0) {
		print_error("register device type",
			    "Device type must have name.", LIT(name));
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
	dev_type->scope =
	    scoped_hash_push(parent_scope, SCOPE_ENTRY_DEVICE_TYPE,
			     dev_type->id);
	dev_type->self_input = -1;
	dev_type->self_output = -1;

	stage->device_types[dev_type->id] = dev_type;

	struct scope_entry *entry;
	entry = scoped_hash_insert(parent_scope, satom(stage, name),
							   SCOPE_ENTRY_DEVICE_TYPE, NULL,
							   dev_type->scope);
	if (!entry) {
		struct scope_entry conflict;
		err = scoped_hash_lookup(parent_scope, satom(stage, name), &conflict);
		assert(!err);
		print_error("register device type",
					"Cannot register '%.*s', because there already "
					"exists a %s with the same name in this scope.",
					LIT(name), humanreadable_scope_entry(conflict.kind));
		return 0;
	}
	entry->id = dev_type->id;
	entry->length = 1;

	return dev_type;
}

void finalize_device_type(struct device_type *dev_type)
{
	dev_type->finalized = true;
}

void describe_device_type(struct stage *stage, struct device_type *dev_type)
{
	FILE *fp = stdout;
	fprintf(fp, "device type %.*s\n", ALIT(dev_type->name));

	fprintf(stdout, " attributes:\n");
	for (size_t i = 0; i < dev_type->num_attributes; ++i) {
		struct device_attribute_def *attr;
		attr = &dev_type->attributes[i];
		fprintf(fp, " - %i: %.*s ", attr->id, ALIT(attr->name));
		print_type_id(stage, attr->type);
		fprintf(fp, "\n");
	}

	fprintf(stdout, " inputs:\n");
	for (size_t i = 0; i < dev_type->num_inputs; ++i) {
		struct device_channel_def *input;
		input = &dev_type->inputs[i];
		fprintf(fp, " - %i: %.*s ", input->id, ALIT(input->name));
		print_type_id(stage, input->type);
		fprintf(fp, "\n");
	}

	fprintf(stdout, " output:\n");
	for (size_t i = 0; i < dev_type->num_outputs; ++i) {
		struct device_channel_def *output;
		output = &dev_type->outputs[i];
		fprintf(fp, " - %i: %.*s ", output->id, ALIT(output->name));
		print_type_id(stage, output->type);
		fprintf(fp, "\n");
	}
}
