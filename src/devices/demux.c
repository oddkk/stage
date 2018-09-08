#include "../stage.h"
#include "../device.h"
#include <stdlib.h>

struct device_demux {
	channel_id channel_select;
	channel_id channel_out;
	channel_id channels_in;
	channel_id channels_off_value;
	size_t num_channels;
	size_t channel_width;
};

static scalar_value device_demux_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_demux *data;
	scalar_value select;

	device = get_device(stage, cnl->device.id);
	data = (struct device_demux *)device->data;
	select = eval_channel(stage, data->channel_select);

	if (select < 0) {
		select = 0;
	}
	if (select >= data->num_channels) {
		select = data->num_channels - 1;
	}

	scalar_value index = cnl->device.channel_subindex / data->channel_width;
	scalar_value subindex = cnl->device.channel_subindex % data->channel_width;

	if (index == select) {
		return eval_channel(stage, data->channels_in + subindex);
	} else {
		return eval_channel(stage,
							data->channels_off_value + subindex);
	}
}

static int device_demux_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_demux *data = calloc(1, sizeof(struct device_demux));

	dev->data = data;

	data->channel_select
		= device_get_input_channel_id_by_name(stage, dev, STR("select"));
	data->channel_out
		= device_get_output_channel_id_by_name(stage, dev, STR("out"));

	scalar_value num_channels;
	struct value_ref num_channels_val = {0};
	num_channels_val.type = stage->standard_types.integer;
	num_channels_val.data = &num_channels;

	device_get_attr(stage, dev, STR("N"), &num_channels_val);
	data->num_channels = num_channels;

	struct type *out_type;
	out_type = device_get_type_from_attr(stage, dev, STR("T"));

	data->channels_in
		= device_get_input_channel_id_by_name(stage, dev, STR("in"));
	data->channels_off_value
		= device_get_input_channel_id_by_name(stage, dev, STR("off_value"));

	data->channel_width = out_type->num_scalars;

	for (size_t i = 0; i < data->channel_width * data->num_channels; i++) {
		channel_bind_callback(stage, data->channel_out + i, device_demux_eval);
		struct channel *cnl;
		cnl = get_channel(stage, data->channel_out + i);
		cnl->device.channel_subindex = i;
	}

	return 0;
}

struct device_type *register_device_type_demux(struct stage *stage)
{
	struct device_type_param params[] = {
		{ .name=STR("T"), .type=stage->standard_types.type },
		{ .name=STR("N"), .type=stage->standard_types.integer },
	};

	struct type_template_context out_template = {0};
	struct type *out_member_type;
	out_member_type = register_template_type_str(stage, NULL, STR("T"),
												  &out_template);

	struct type *out_type;
	out_type = register_template_length_array_type_str(stage, NULL,
														out_member_type->id,
														STR("N"),
														&out_template);

	out_template.type = out_type->id;

	struct device_type_channel channels[] = {
		{
			.kind=DEVICE_CHANNEL_INPUT,
			.name=STR("select"),
			.type=stage->standard_types.integer,
		},

		{ .kind=DEVICE_CHANNEL_INPUT, .name=STR("in"),        .template=STR("T") },
		{ .kind=DEVICE_CHANNEL_INPUT, .name=STR("off_value"), .template=STR("T") },

		{
			.kind=DEVICE_CHANNEL_OUTPUT,
			.name=STR("out"),
			.custom_template=out_template,
			.self=true,
		},
	};

	struct device_type_def device = {
		.name = STR("basic.demux"),
		.init = device_demux_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),
	};

	return register_device_type(stage, device);
}
