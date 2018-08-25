#include "../stage.h"
#include "../device.h"
#include "../utils.h"
#include <stdio.h>
#include <stdlib.h>

struct device_debug_print_data {
	channel_id in;
	scalar_value *last_value[2];
	uint8_t buffer;

	struct type *type;
};

static void device_print_tick(struct stage *stage, struct device *dev) {
	struct device_debug_print_data *data;
	bool changed = false;

	data = dev->data;
	for (size_t i = 0; i < data->type->num_scalars; i++) {
		data->last_value[data->buffer][i] = eval_channel(stage, data->in + i);
		if (data->last_value[data->buffer][i] != data->last_value[data->buffer ^ 0x1][i]) {
			changed = true;
		}
	}

	if (changed) {
		printf("value %i (", data->in);
		print_full_entry_name(stage, dev->scope);
		printf("): ");
		for (size_t i = 0; i < data->type->num_scalars; i++) {
			if (i != 0) {
				printf(", ");
			}
			print_scalar(data->last_value[data->buffer][i]);
		}
		printf("\n");
	}

	data->buffer ^= 0x1;
}

static int device_print_template_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	scalar_value input_type;
	int err = 0;

	// @TODO: Implement
	input_type = stage->standard_types.integer; // device_get_attr(stage, dev, SATOM(stage, "T"));
	/* err = device_assign_input_type_by_name(stage, dev, SATOM(stage, "in"), input_type); */

	return err;
}

static int device_print_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_debug_print_data *data = calloc(1, sizeof(struct device_debug_print_data));
	/* scalar_value input_type; */

	// @TODO: Implement
	/* input_type = device_get_attr(stage, dev, SATOM(stage, "T")); */
	data->type = get_type(stage, stage->standard_types.integer); //input_type);
	/* data->buffer = 0; */

	data->last_value[0] = calloc(data->type->num_scalars, sizeof(scalar_value));
	data->last_value[1] = calloc(data->type->num_scalars, sizeof(scalar_value));

	data->in = device_get_input_channel_id_by_name(stage, dev, STR("in"));

	dev->data = data;

	register_device_tick_callback(stage, dev, 1, device_print_tick);

	return 0;
}

struct device_type *register_device_type_print(struct stage *stage)
{
	struct device_type *print;
	struct device_channel_def *channel_in;

	struct type_template_context params_type = {0};
	struct device_type_param params[] = {
		{ .name=STR("T"), .type=stage->standard_types.type },
	};

	params_type = make_device_type_params_type(stage, params, ARRAY_LENGTH(params));

	print = register_device_type(stage, STR("debug_print"), params_type);
	print->device_init = device_print_init;
	print->device_template_init = device_print_template_init;

	struct type_template_context params_template = {0};
	struct access_pattern type_pattern = {0};
	access_pattern_ident(&type_pattern, SATOM(stage, "T"));
	struct type *T;
	T = register_template_type(stage, NULL, type_pattern, &params_template);

	channel_in = device_type_add_input(stage, print, STR("in"), T->id);

	print->self_input = channel_in->id;

	finalize_device_type(print);

	return print;
}

