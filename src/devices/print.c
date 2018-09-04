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

static void device_print_free(struct stage *stage, struct device *dev)
{
	struct device_debug_print_data *data = dev->data;

	free(data->last_value[0]);
	free(data->last_value[1]);
	free(data);
}

struct device_type *register_device_type_print(struct stage *stage)
{
	struct device_type_param params[] = {
		{ .name=STR("T"), .type=stage->standard_types.type },
	};
	struct device_type_channel channels[] = {
		{ .kind=DEVICE_CHANNEL_INPUT, .name=STR("in"), .template=STR("T"), .self=true },
	};

	struct device_type_def device = {
		.name = STR("basic.print"),
		.init = device_print_init,
		.free = device_print_free,

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),
	};

	return register_device_type(stage, device);
}

