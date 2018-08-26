#include "../stage.h"
#include "../device.h"
#include "../utils.h"
#include <stdlib.h>

struct device_add_data {
	channel_id channel_out;
	channel_id channel_left;
	channel_id channel_right;
};

scalar_value device_add_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	scalar_value lhs, rhs;

	device = get_device(stage, cnl->device.id);

	lhs = eval_channel(stage, device->input_begin);
	rhs = eval_channel(stage, device->input_begin + 1);

	if (lhs == SCALAR_OFF || rhs == SCALAR_OFF) {
		return SCALAR_OFF;
	}

	return lhs + rhs;
}

int device_add_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_add_data *data = calloc(1, sizeof(struct device_add_data));

	data->channel_out   = device_get_output_channel_id_by_name(stage, dev, STR("out"));
	data->channel_left  = device_get_input_channel_id_by_name(stage, dev, STR("left"));
	data->channel_right = device_get_input_channel_id_by_name(stage, dev, STR("right"));

	if (data->channel_out < 0 || data->channel_left < 0 || data->channel_right < 0) {
		return -1;
	}

	dependency_matrix_bind(&stage->channel_deps,
						   data->channel_out, data->channel_left);
	dependency_matrix_bind(&stage->channel_deps,
						   data->channel_out, data->channel_right);

	channel_bind_callback(stage, data->channel_out, device_add_eval);

	return 0;
}

struct device_type *register_device_type_add(struct stage *stage)
{
	struct device_type_param params[] = {
		{ .name=STR("T"), .type=stage->standard_types.type },
	};
	struct device_type_channel channels[] = {
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("left"),  .template=STR("T") },
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("right"), .template=STR("T") },
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("out"),   .template=STR("T"), .self=true },
	};

	struct device_type_def device = {
		.name = STR("basic.add"),
		.init = device_add_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),
	};

	return register_device_type(stage, device);
}
