#include "../stage.h"
#include "../device.h"
#include <stdlib.h>

scalar_value device_tick_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	return stage->tick;
}

int device_tick_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	channel_id channel_out;
	channel_out = device_get_output_channel_id_by_name(stage, dev, STR("out"));

	if (channel_out < 0) {
		return -1;
	}

	channel_bind_callback(stage, channel_out, device_tick_eval);

	return 0;
}

struct device_type *register_device_type_tick(struct stage *stage)
{
	struct device_type *tick;
	struct device_channel_def *channel_out;

	tick = register_device_type(stage, STR("tick"));
	tick->device_init = device_tick_init;

	channel_out = device_type_add_output(stage, tick, STR("out"),
										 stage->standard_types.integer);

	tick->self_output = channel_out->id;
	return tick;
}
