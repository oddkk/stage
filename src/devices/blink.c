#include "../stage.h"
#include "../device.h"
#include <stdlib.h>

struct device_blink_data {
	channel_id channel_out;
	channel_id channel_on_value;
	channel_id channel_off_value;
	channel_id channel_duration;
	channel_id channel_period;
	channel_id channel_reset;

	uint64_t begin;
	bool reset_high;
};


scalar_value device_blink_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_blink_data *data;
	scalar_value period;
	scalar_value duration;

	device = get_device(stage, cnl->device.id);
	data = (struct device_blink_data*)device->data;

	period = eval_channel(stage, data->channel_period);

	scalar_value reset = eval_channel(stage, data->channel_reset);

	if (!data->reset_high && reset > 0) {
		data->begin = stage->tick;
	}
	data->reset_high = reset > 0;

	duration = eval_channel(stage, data->channel_duration);

	if (duration < 0) {
		duration = period / 2;
	}

	if (period > 0 && (stage->tick - data->begin) % period <= duration) {
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
	data->channel_duration   = device_get_input_channel_id_by_name(stage, dev, STR("duration"));

	data->channel_period     = device_get_input_channel_id_by_name(stage, dev, STR("period"));
	data->channel_reset      = device_get_input_channel_id_by_name(stage, dev, STR("reset"));

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
	struct device_type_param params[] = {
		{ .name=STR("T"), .type=stage->standard_types.type },
	};
	struct device_type_channel channels[] = {
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("out"),       .template=STR("T"), .self=true },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("on_value"),  .template=STR("T"), },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("off_value"), .template=STR("T"), },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("duration"),  .type=stage->standard_types.integer, },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("period"),    .type=stage->standard_types.integer, },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("reset"),     .type=stage->standard_types.integer, },
	};

	struct device_type_def device = {
		.name = STR("basic.blink"),
		.init = device_blink_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),
	};

	return register_device_type(stage, device);
}
