#include "../stage.h"
#include "../device.h"
#include <stdlib.h>

struct device_radio {
	channel_id channel_out;

	channel_id channel_radio;
	scalar_value last;
	size_t num_inputs;
};

static scalar_value device_radio_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_radio *data;

	device = get_device(stage, cnl->device.id);
	data = (struct device_radio *)device->data;

	scalar_value result = data->last;

	for (size_t i = 0; i < data->num_inputs; i++) {
		scalar_value value;
		value = eval_channel(stage, data->channel_radio + i);
		if (value > 0) {
			result = i;
		}
	}

	data->last = result;
	return result;
}

static int device_radio_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_radio *data = calloc(1, sizeof(struct device_radio));
	dev->data = data;

	data->channel_radio
		= device_get_input_channel_id_by_name(stage, dev, STR("radio"));

	data->channel_out
		= device_get_output_channel_id_by_name(stage, dev, STR("out"));

	scalar_value num_inputs;
	struct value_ref num_inputs_val = {0};
	num_inputs_val.type = stage->standard_types.integer;
	num_inputs_val.data = &num_inputs;

	device_get_attr(stage, dev, STR("N"), &num_inputs_val);
	data->num_inputs = num_inputs;

	channel_bind_callback(stage, data->channel_out, device_radio_eval);

	return 0;
}

static void device_radio_free(struct stage *stage, struct device *dev)
{
	free(dev->data);
}

struct device_type *register_device_type_radio(struct stage *stage)
{
	struct device_type_param params[] = {
		{ .name=STR("N"), .type=stage->standard_types.integer },
	};

	struct type_template_context input_template = {0};
	struct type *input_type;
	input_type = register_template_length_array_type_str(stage, NULL,
														 stage->standard_types.integer,
														 STR("N"),
														 &input_template);

	input_template.type = input_type->id;

	struct device_type_channel channels[] = {
		{
			.kind=DEVICE_CHANNEL_INPUT,
			.name=STR("radio"),
			.custom_template=input_template,
			.self=true,
		},

		{
			.kind=DEVICE_CHANNEL_OUTPUT,
			.name=STR("out"),
			.type=stage->standard_types.integer,
			.self=true,
		},
	};

	struct device_type_def device = {
		.name = STR("basic.radio"),
		.init = device_radio_init,
		.free = device_radio_free,

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),
	};

	return register_device_type(stage, device);
}
