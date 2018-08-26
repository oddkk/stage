#include "device_type.h"
#include "dlist.h"
#include "utils.h"
#include "stage.h"

#include <stdio.h>
#include <stdlib.h>

struct device_channel_def *device_type_add_input(struct stage *stage,
						 struct device_type *dev_type,
						 struct string name,
						 type_id type)
{
	struct type_template_context ctx = {0};
	ctx.type = type;
	return device_type_add_input_template(stage, dev_type, name, ctx);
}

struct device_channel_def *device_type_add_input_template(struct stage *stage,
						 struct device_type *dev_type,
						 struct string name,
						 struct type_template_context type)
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
	struct type_template_context ctx = {0};
	ctx.type = type;
	return device_type_add_output_template(stage, dev_type, name, ctx);
}

struct device_channel_def *device_type_add_output_template(struct stage *stage,
						  struct device_type *dev_type,
						  struct string name,
						  struct type_template_context type)
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
										 struct string name,
										 struct type_template_context params)
{
	return register_device_type_scoped(stage, name, params,
									   &stage->root_scope);
}

struct device_type *register_device_type_scoped(struct stage *stage,
												struct string name,
												struct type_template_context params,
												struct scoped_hash *parent_scope)
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
	dev_type->params = params;
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

struct type_template_context make_device_type_params_type(struct stage *stage,
														  struct device_type_param *params,
														  size_t num_params)
{
	struct type_template_context ctx = {0};
	struct type new_type = {0};

	new_type.kind = TYPE_KIND_NAMED_TUPLE;
	new_type.named_tuple.length = num_params;
	new_type.named_tuple.members
		= calloc(num_params, sizeof(struct named_tuple_member));

	for (size_t i = 0; i < num_params; i++) {
		struct named_tuple_member *member;
		member = &new_type.named_tuple.members[i];
		member->name = satom(stage, params[i].name);

		if (params[i].type) {
			member->type = params[i].type;
		} else if (params[i].template.length) {
			struct access_pattern pattern = {0};
			parse_access_pattern(&stage->atom_table, params[i].template, &pattern);

			struct type *new_type;
			new_type = register_template_type(stage, NULL, pattern, &ctx);
			member->type = new_type->id;
		} else {
			assert(!"[make_device_type_params_type] Either type or template must be set.");
		}
	}

	struct type *result;
	result = register_type(stage, new_type);

	if (!result) {
		struct type_template_context empty_ctx = {0};
		return empty_ctx;
	}

	ctx.type = result->id;

	return ctx;
}

void describe_device_type(struct stage *stage, struct device_type *dev_type)
{
	FILE *fp = stdout;
	fprintf(fp, "device type %.*s\n", ALIT(dev_type->name));

	fprintf(stdout, " parameters:\n");
	print_type_id(fp, stage, dev_type->params.type);
	fprintf(fp, "\n");

	fprintf(stdout, " inputs:\n");
	for (size_t i = 0; i < dev_type->num_inputs; ++i) {
		struct device_channel_def *input;
		input = &dev_type->inputs[i];
		fprintf(fp, " - %i: %.*s ", input->id, ALIT(input->name));
		print_type_id(fp, stage, input->type.type);
		fprintf(fp, "\n");
	}

	fprintf(stdout, " output:\n");
	for (size_t i = 0; i < dev_type->num_outputs; ++i) {
		struct device_channel_def *output;
		output = &dev_type->outputs[i];
		fprintf(fp, " - %i: %.*s ", output->id, ALIT(output->name));
		print_type_id(fp, stage, output->type.type);
		fprintf(fp, "\n");
	}
}
