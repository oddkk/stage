#include "../stage.h"
#include "../device.h"
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
	struct device_type *add;
	struct device_channel_def *channel_out;

	add = register_device_type(stage, STR("add"));
	add->device_init = device_add_init;

	device_type_add_input(stage, add, STR("left"),
						  stage->standard_types.integer);
	device_type_add_input(stage, add, STR("right"),
						  stage->standard_types.integer);

	channel_out = device_type_add_output(stage, add, STR("out"),
										 stage->standard_types.integer);

	add->self_output = channel_out->id;

	finalize_device_type(add);

	return add;
}
