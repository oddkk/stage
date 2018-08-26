#include "../stage.h"
#include "../device.h"
#include <stdio.h>
#include <stdlib.h>

struct device_toggle_data {
	channel_id channel_in;
	channel_id channel_out;
	channel_id channel_threshold;
	channel_id channel_hysterisis;
	channel_id channel_on_value;
	channel_id channel_off_value;

	bool state;
	bool input_high;
};

scalar_value device_toggle_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_toggle_data *data;

	device = get_device(stage, cnl->device.id);
	data = (struct device_toggle_data*)device->data;

	scalar_value in;
	scalar_value threshold;
	scalar_value hysterisis;
	in         = eval_channel(stage, data->channel_in);
	threshold  = eval_channel(stage, data->channel_threshold);

	if (threshold != SCALAR_OFF) {
		hysterisis = eval_channel(stage, data->channel_hysterisis);

		if (hysterisis == SCALAR_OFF) {
			hysterisis = 0;
		}

		if (!data->input_high && in >= threshold + hysterisis) {
			data->input_high = true;
			data->state = !data->state;

		} else if (data->input_high && in < threshold - hysterisis) {
			data->input_high = false;
		}
	} else {
		if (in != SCALAR_OFF) {
			data->input_high = true;
			data->state = !data->state;

		} else {
			data->input_high = false;
		}
	}

	if (data->state) {
		return eval_channel(stage, data->channel_on_value);
	} else {
		return eval_channel(stage, data->channel_off_value);
	}
}

int device_toggle_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_toggle_data *data = calloc(1, sizeof(struct device_toggle_data));

	dev->data = data;

	data->channel_out = device_get_output_channel_id_by_name(stage, dev, STR("out"));

	data->channel_in         = device_get_input_channel_id_by_name(stage, dev, STR("in"));
	data->channel_on_value   = device_get_input_channel_id_by_name(stage, dev, STR("on_value"));
	data->channel_off_value  = device_get_input_channel_id_by_name(stage, dev, STR("off_value"));
	data->channel_threshold  = device_get_input_channel_id_by_name(stage, dev, STR("threshold"));
	data->channel_hysterisis = device_get_input_channel_id_by_name(stage, dev, STR("hysterisis"));

	if (data->channel_out        < 0 ||
		data->channel_in         < 0 ||
		data->channel_on_value   < 0 ||
		data->channel_off_value  < 0 ||
		data->channel_threshold  < 0 ||
		data->channel_hysterisis < 0) {
		printf("Channel not found.\n");
		return -1;
	}

	channel_bind_callback(stage, data->channel_out, device_toggle_eval);

	return 0;
}

struct device_type *register_device_type_toggle(struct stage *stage)
{
	struct device_type_param params[] = {
		{ .name=STR("T"), .type=stage->standard_types.type },
	};
	struct device_type_channel channels[] = {
		{
			.kind=DEVICE_CHANNEL_INPUT,
			.name=STR("in"),
			.type=stage->standard_types.integer,
			.self=true
		},
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("threshold"),  .type=stage->standard_types.integer, },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("hysterisis"), .type=stage->standard_types.integer, },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("on_value"),   .template=STR("T") },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("off_value"),  .template=STR("T") },
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("out"),        .template=STR("T"), .self=true },
	};

	struct device_type_def device = {
		.name = STR("basic.toggle"),
		.init = device_toggle_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),
	};

	return register_device_type(stage, device);
}
