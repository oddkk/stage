#include "../stage.h"
#include "../device.h"
#include <stdio.h>
#include <stdlib.h>

struct device_debug_print_data {
	channel_id in;
	scalar_value last_value;
};

static void device_print_tick(struct stage *stage, struct device *dev) {
	struct device_debug_print_data *data;
	scalar_value in;

	data = dev->data;
	in = eval_channel(stage, data->in);

	if (in != data->last_value) {
		printf("value: ");
		print_scalar(in);
		printf("\n");
	}

	data->last_value = in;
}

static int device_print_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_debug_print_data *data = calloc(1, sizeof(struct device_debug_print_data));

	data->in = device_get_input_channel_id_by_name(stage, dev, STR("in"));

	data->last_value = 0;
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

	channel_in = device_type_add_input(stage, print, STR("in"), stage->standard_types.integer);

	print->self_input = channel_in->id;

	return print;
}
