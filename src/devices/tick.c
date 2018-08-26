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
	struct device_type_channel channels[] = {
		{
			.kind=DEVICE_CHANNEL_OUTPUT,
			.name=STR("out"),
			.type=stage->standard_types.integer,
			.self=true
		},
	};

	struct device_type_def device = {
		.name = STR("basic.tick"),
		.init = device_tick_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
	};

	return register_device_type(stage, device);
}
