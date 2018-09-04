#include "../stage.h"
#include "../device.h"
#include <stdlib.h>

struct device_mux {
	channel_id channel_in;
	channel_id channel_out;
	channel_id channels;
	size_t num_channels;
	size_t channel_width;
};

static scalar_value device_mux_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_mux *data;
	scalar_value in;

	device = get_device(stage, cnl->device.id);
	data = (struct device_mux *)device->data;
	in = eval_channel(stage, data->channel_in);

	if (in < 0) {
		in = 0;
	}
	if (in >= data->num_channels) {
		in = data->num_channels - 1;
	}

	return eval_channel(stage,
						data->channels +
						(in * data->channel_width) +
						cnl->device.channel_subindex);
}

static int device_mux_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_mux *data = calloc(1, sizeof(struct device_mux));

	dev->data = data;

	data->channel_in
		= device_get_input_channel_id_by_name(stage, dev, STR("select"));
	data->channel_out
		= device_get_output_channel_id_by_name(stage, dev, STR("out"));

	scalar_value num_channels;
	struct value_ref num_channels_val = {0};
	num_channels_val.type = stage->standard_types.integer;
	num_channels_val.data = &num_channels;

	device_get_attr(stage, dev, STR("N"), &num_channels_val);
	data->num_channels = num_channels;

	struct type *case_type;
	case_type = device_get_type_from_attr(stage, dev, STR("T"));

	data->channels
		= device_get_input_channel_id_by_name(stage, dev, STR("in"));
	data->channel_width = case_type->num_scalars;

	for (size_t i = 0; i < data->channel_width; i++) {
		channel_bind_callback(stage, data->channel_out + i, device_mux_eval);
		struct channel *cnl;
		cnl = get_channel(stage, data->channel_out + i);
		cnl->device.channel_subindex = i;
	}

	return 0;
}

struct device_type *register_device_type_mux(struct stage *stage)
{
	struct device_type_param params[] = {
		{ .name=STR("T"), .type=stage->standard_types.type },
		{ .name=STR("N"), .type=stage->standard_types.integer },
	};

	struct type_template_context case_template = {0};
	struct type *case_member_type;
	case_member_type = register_template_type_str(stage, NULL, STR("T"),
												  &case_template);

	struct type *case_type;
	case_type = register_template_length_array_type_str(stage, NULL,
														case_member_type->id,
														STR("N"),
														&case_template);

	case_template.type = case_type->id;

	struct device_type_channel channels[] = {
		{
			.kind=DEVICE_CHANNEL_INPUT,
			.name=STR("select"),
			.type=stage->standard_types.integer,
			.self=true,
		},

		{
			.kind=DEVICE_CHANNEL_INPUT,
			.name=STR("in"),
			.custom_template=case_template,
		},

		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("out"), .template=STR("T") },
	};

	struct device_type_def device = {
		.name = STR("basic.mux"),
		.init = device_mux_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),
	};

	return register_device_type(stage, device);
}
