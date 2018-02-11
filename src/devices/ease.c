#include "../stage.h"
#include "../device.h"
#include <stdlib.h>

struct device_ease {
	scalar_value current;

	channel_id channel_in;
	channel_id channel_out;
};

static scalar_value lerp(scalar_value A, scalar_value B, float t)
{
	scalar_value result;

	result = (1.0 - t) * (float)A + t * (float)B;

	return result;
}

static scalar_value device_ease_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_ease *data;
	scalar_value in;

	device = get_device(stage, cnl->device.id);
	data = (struct device_ease*)device->data;
	in = eval_channel(stage, data->channel_in);

	data->current = lerp(data->current, in, 0.25f);

	return in;
}

static int device_ease_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_ease *data = calloc(1, sizeof(struct device_ease));

	data->channel_in
		= device_get_input_channel_id_by_name(stage, dev, STR("in"));
	data->channel_out
		= device_get_output_channel_id_by_name(stage, dev, STR("out"));

	channel_bind_callback(stage, data->channel_out, device_ease_eval);

	dependency_matrix_bind(&stage->channel_deps,
						   data->channel_out, data->channel_in);

	data->current = 0;
	dev->data = data;

	return 0;
}

struct device_type *register_device_type_ease(struct stage *stage)
{
	struct device_type *ease;

	ease = register_device_type(stage, STR("ease"));
	ease->device_init = device_ease_init;

	device_type_add_input(stage, ease, STR("in"), stage->standard_types.integer);
	device_type_add_output(stage, ease, STR("out"), stage->standard_types.integer);

	return ease;
}
