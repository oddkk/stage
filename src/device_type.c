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

struct device_type *register_device_type_two_phase(struct stage *stage,
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

	if (dev_type->params.type != 0) {

		struct type *params_type;
		params_type = get_type(stage, dev_type->params.type);

		assert(params_type->kind == TYPE_KIND_NAMED_TUPLE);

		size_t params_num_scalars = 0;
		for (size_t i = 0; i < params_type->named_tuple.length; i++) {
			struct named_tuple_member *param;
			param = &params_type->named_tuple.members[i];

			err = register_typed_member_in_scope(stage,
												 param->name, param->type,
												 dev_type->scope,
												 SCOPE_ENTRY_DEVICE_TYPE_ATTRIBUTE,
												 params_num_scalars);

			if (err < 0) {
				return NULL;
			}

			params_num_scalars += err;
		}
	}

	err = 0;

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

void finalize_device_type_two_phase(struct device_type *dev_type)
{
	dev_type->finalized = true;
}

static struct type_template_context make_device_type_params_type(struct stage *stage,
														  struct device_type_param *params,
														  size_t num_params)
{
	struct type_template_context ctx = {0};
	struct type new_type = {0};

	if (num_params == 0) {
		return ctx;
	}

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


struct device_type *register_device_type(struct stage *stage,
										 struct device_type_def def)
{
	struct type_template_context params = {0};
	params = make_device_type_params_type(stage, def.params.params,
										  def.params.num_params);

	assert(def.name.length > 0);

	int err;

	struct access_pattern name_pattern = {0};
	err = parse_access_pattern(&stage->atom_table, def.name, &name_pattern);
	if (err) {
		return NULL;
	}
	if (name_pattern.num_entries < 1) {
		fprintf(stderr, "The device type must have a name.\n");
		return NULL;
	}


	struct access_pattern_entry *name_part;
	name_part = &name_pattern.entries[name_pattern.num_entries - 1];
	if (name_part->kind != ACCESS_IDENT) {
		fprintf(stderr,
				"Expected the final part of the device name "
				"'%.*s' to be an identifier.\n",
				LIT(def.name));
		return NULL;
	}

	struct scoped_hash *scope;
	struct access_pattern namespace_pattern;
	namespace_pattern = name_pattern;
	namespace_pattern.num_entries -= 1;

	scope = get_or_create_namespace(&stage->root_scope, namespace_pattern);

	struct device_type *device_type;

	device_type = register_device_type_two_phase(stage, name_part->ident->name,
												 params, scope);

	for (size_t i = 0; i < def.channels.num_channels; i++) {
		struct device_channel_def *cnl;
		struct device_type_channel *channel;
		channel = &def.channels.channels[i];

		if (channel->kind != DEVICE_CHANNEL_INPUT &&
			channel->kind != DEVICE_CHANNEL_OUTPUT) {
			fprintf(stderr, "Device type channel must be either INPUT or OUTPUT.\n");
			return NULL;
		}

		if (channel->type) {
			if (channel->kind == DEVICE_CHANNEL_INPUT) {
				cnl = device_type_add_input(stage, device_type,
											channel->name, channel->type);
			} else {
				cnl = device_type_add_output(stage, device_type,
											 channel->name, channel->type);
			}
		} else if (channel->template.length) {
			struct type_template_context ctx = {0};
			struct access_pattern pattern = {0};
			parse_access_pattern(&stage->atom_table,
								 channel->template,
								 &pattern);

			struct type *new_type;
			new_type = register_template_type(stage, NULL,
											  pattern, &ctx);
			ctx.type = new_type->id;

			if (channel->kind == DEVICE_CHANNEL_INPUT) {
				cnl = device_type_add_input_template(stage, device_type,
													 channel->name, ctx);
			} else {
				cnl = device_type_add_output_template(stage, device_type,
													  channel->name, ctx);
			}

		} else if (channel->custom_template.type != 0) {
			if (channel->kind == DEVICE_CHANNEL_INPUT) {
				cnl = device_type_add_input_template(stage, device_type,
													 channel->name, channel->custom_template);
			} else {
				cnl = device_type_add_output_template(stage, device_type,
													  channel->name, channel->custom_template);
			}

		} else {
			assert(!"[make_device_type_params_type] Either type or template must be set.");
		}

		if (channel->self) {
			if (channel->kind == DEVICE_CHANNEL_INPUT) {
				if (device_type->self_input != -1) {
					fprintf(stderr, "Multiple input channels marked as self.\n");
					return NULL;
				}

				device_type->self_input = cnl->id;
			} else {
				if (device_type->self_output != -1) {
					fprintf(stderr, "Multiple input channels marked as self.\n");
					return NULL;
				}

				device_type->self_output = cnl->id;
			}
		}
	}

	if ((def.template_init != NULL || def.init != NULL) &&
		(def.context_template_init != NULL || def.context_init != NULL)) {
		printf("Use either init and template_init or "
			   "context_init and context_template_init. "
			   "Not both.\n");
		return NULL;
	}

	if (def.template_init || def.init) {
		device_type->device_template_init = def.template_init;
		device_type->device_init = def.init;
		device_type->takes_context = false;

	} else if (def.context_template_init || def.context_init) {
		device_type->device_context_template_init = def.context_template_init;
		device_type->device_context_init = def.context_init;
		device_type->takes_context = true;
	}

	device_type->device_free = def.free;

	finalize_device_type_two_phase(device_type);

	return device_type;
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
