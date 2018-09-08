#include "../stage.h"
#include "../device.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

struct device_ease {
	float *current;
	struct type *type;

	channel_id channel_in;
	channel_id channel_out;
};

static float lerp(float A, float B, float t)
{
	float result;

	result = (1.0f - t) * A + t * B;

	return result;
}

static scalar_value device_ease_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_ease *data;
	scalar_value in;

	device = get_device(stage, cnl->device.id);
	data = (struct device_ease*)device->data;

	size_t i = cnl->device.channel_subindex;

	in = eval_channel(stage, data->channel_in + i);
	data->current[i] = lerp(data->current[i], in, 0.0025f);

	return roundf(data->current[i]);
}

static int device_ease_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_ease *data = calloc(1, sizeof(struct device_ease));

	data->type = device_get_type_from_attr(stage, dev, STR("T"));

	data->current = calloc(data->type->num_scalars, sizeof(float));

	data->channel_in
		= device_get_input_channel_id_by_name(stage, dev, STR("in"));
	data->channel_out
		= device_get_output_channel_id_by_name(stage, dev, STR("out"));

	for (size_t i = 0; i < data->type->num_scalars; i++) {
		channel_bind_callback(stage, data->channel_out + i, device_ease_eval);
		struct channel *cnl;
		cnl = get_channel(stage, data->channel_out + i);
		cnl->device.channel_subindex = i;
	}

	dev->data = data;

	return 0;
}

struct device_type *register_device_type_ease(struct stage *stage)
{

	struct device_type_param params[] = {
		{ .name=STR("T"), .type=stage->standard_types.type },
	};
	struct device_type_channel channels[] = {
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("in"),  .template=STR("T"), .self=true },
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("out"), .template=STR("T"), .self=true },
	};

	struct device_type_def device = {
		.name = STR("basic.ease"),
		.init = device_ease_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),
	};

	return register_device_type(stage, device);
}
