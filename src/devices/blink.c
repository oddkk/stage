#include "../stage.h"
#include "../device.h"
#include <stdlib.h>

struct device_blink_data {
	channel_id channel_out;
	channel_id channel_on_value;
	channel_id channel_off_value;
};


scalar_value device_blink_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_blink_data *data;

	device = get_device(stage, cnl->device.id);
	data = (struct device_blink_data*)device->data;

	if (stage->tick % 1000 > 500) {
		return eval_channel(stage, data->channel_on_value);
	} else {
		return eval_channel(stage, data->channel_off_value);
	}
}

int device_blink_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_blink_data *data = calloc(1, sizeof(struct device_blink_data));

	dev->data = data;

	data->channel_out = device_get_output_channel_id_by_name(stage, dev, STR("out"));

	data->channel_on_value   = device_get_input_channel_id_by_name(stage, dev, STR("on_value"));
	data->channel_off_value  = device_get_input_channel_id_by_name(stage, dev, STR("off_value"));

	if (data->channel_out        < 0 ||
		data->channel_on_value   < 0 ||
		data->channel_off_value  < 0) {
		printf("Channel not found.\n");
		return -1;
	}

	channel_bind_callback(stage, data->channel_out, device_blink_eval);

	return 0;
}

struct device_type *register_device_type_blink(struct stage *stage)
{
	struct device_type *blink;
	struct device_channel_def *channel_out;
	struct type_template_context params = {0};

	blink = register_device_type(stage, STR("blink"), params);
	blink->device_init = device_blink_init;

	channel_out = device_type_add_output(stage, blink, STR("out"),
										 stage->standard_types.integer);
	device_type_add_input(stage, blink, STR("on_value"),
						  stage->standard_types.integer);
	device_type_add_input(stage, blink, STR("off_value"),
						  stage->standard_types.integer);


	blink->self_output = channel_out->id;

	finalize_device_type(blink);

	return blink;
}
