#include "../stage.h"
#include "../device.h"
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

static int device_print_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_debug_print_data *data = calloc(1, sizeof(struct device_debug_print_data));
	struct attribute_value *input_type;

	input_type = device_get_attr(stage, dev, atom_create(&stage->atom_table, STR("T")));
	data->type = get_type(stage, input_type->value);
	data->buffer = 0;

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

	print = register_device_type(stage, STR("debug_print"));
	print->device_init = device_print_init;

	device_type_add_attribute(stage, print, STR("T"),
							  stage->standard_types.integer,
							  stage->standard_types.type);

	channel_in = device_type_add_input(stage, print, STR("in"), stage->standard_types.integer);

	print->self_input = channel_in->id;

	return print;
}
