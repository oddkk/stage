#include "device_type.h"
#include "dlist.h"
#include "utils.h"
#include "stage.h"

#include <stdlib.h>

struct device_attribute_def *device_type_add_attribute(struct device_type
						       *dev_type,
						       struct string name,
						       type_id type,
						       struct value def)
{
	int error;
	struct device_attribute_def attr;

	attr.id = dev_type->num_attributes;
	attr.name = name;
	attr.type = type;
	attr.def = def;

	error = id_lookup_table_insert(&dev_type->attribute_ids, name, attr.id);

	if (error) {
		print_error("device type add attribute",
			    "Failed to add attribute '%.*s' to device type '%.*s' because "
			    "this attribute already exists.", LIT(name),
			    LIT(dev_type->name));
		return NULL;
	}

	dlist_append(dev_type->attributes, dev_type->num_attributes, &attr);

	return &dev_type->attributes[attr.id];
}

struct device_input_def *device_type_add_input(struct device_type *dev_type,
					       struct string name, type_id type)
{
	int error;
	struct device_input_def input;
	input.id = dev_type->num_attributes;
	input.name = name;
	input.type = type;

	error = id_lookup_table_insert(&dev_type->input_ids, name, input.id);

	if (error) {
		print_error("device type add input",
			    "Failed to add input '%.*s' to device type '%.*s' because "
			    "this input already exists.", LIT(name),
			    LIT(dev_type->name));
		return NULL;
	}

	dlist_append(dev_type->inputs, dev_type->num_inputs, &input);

	return &dev_type->inputs[input.id];
}

struct device_output_def *device_type_add_output(struct device_type *dev_type,
						 struct string name,
						 type_id type)
{
	int error;
	struct device_output_def output;
	output.id = dev_type->num_attributes;
	output.name = name;
	output.type = type;

	error = id_lookup_table_insert(&dev_type->output_ids, name, output.id);

	if (error) {
		print_error("device type add output",
			    "Failed to add output '%.*s' to device type '%.*s' because "
			    "this output already exists.", LIT(name),
			    LIT(dev_type->name));
		return NULL;
	}

	dlist_append(dev_type->outputs, dev_type->num_outputs, &output);

	return &dev_type->outputs[output.id];
}

struct device_type *register_device_type(struct stage *stage,
					 struct string name)
{
	struct device_type *dev_type;
	int old_id, error;

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
	dev_type->name = name;
	dev_type->attribute_ids.page_arena = &stage->memory;
	dev_type->attribute_ids.string_arena = &stage->memory;

	dev_type->input_ids.page_arena = &stage->memory;
	dev_type->input_ids.string_arena = &stage->memory;

	dev_type->output_ids.page_arena = &stage->memory;
	dev_type->output_ids.string_arena = &stage->memory;

	stage->device_types[dev_type->id] = dev_type;

	error =
	    id_lookup_table_insert(&stage->device_types_lookup, name,
				   dev_type->id);

	if (error) {
		return 0;
	}

	return dev_type;
}
