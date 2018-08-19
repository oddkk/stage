#include "../stage.h"
#include "../device.h"

scalar_value device_blink_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	return (stage->tick % 1000 > 500);
}

int device_blink_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	channel_id channel_out;
	channel_out = device_get_output_channel_id_by_name(stage, dev, STR("out"));

	if (channel_out < 0) {
		return -1;
	}

	channel_bind_callback(stage, channel_out, device_blink_eval);

	return 0;
}

struct device_type *register_device_type_blink(struct stage *stage)
{
	struct device_type *blink;
	struct device_channel_def *channel_out;

	blink = register_device_type(stage, STR("blink"));
	blink->device_init = device_blink_init;

	channel_out = device_type_add_output(stage, blink, STR("out"),
										 stage->standard_types.integer);

	blink->self_output = channel_out->id;

	finalize_device_type(blink);

	return blink;
}
