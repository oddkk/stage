#include "../stage.h"
#include "../device.h"
#include "../utils.h"

int device_constant_init(struct stage *stage, struct device_type *dev_type, struct device *dev)
{
	channel_id out_channel;
	int err;

	out_channel = device_get_output_channel_id_by_name(stage, dev, STR("out"));
	// out_channel = device_get_channel(stage, dev, STR("out"));
	if (out_channel < 0) {
		return -1;
	}

	struct type *type;
	struct value_ref value;

	type = device_get_type_from_attr(stage, dev, STR("T"));
	value = alloc_value(stage, type->id);

	err = device_get_attr(stage, dev, STR("value"), &value);
	if (err) {
		return -1;
	}

	channel_bind_constant(stage, out_channel, value);

	return 0;
}

struct device_type *register_device_type_constant(struct stage *stage)
{
	struct device_type_param params[] = {
		{ .name=STR("T"),     .type=stage->standard_types.type },
		{ .name=STR("value"), .template=STR("T") },
	};

	struct device_type_channel channels[] = {
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("out"), .template=STR("T"), .self=true },
	};

	struct device_type_def constant = {
		.name = STR("basic.constant"),

		DEVICE_TYPE_DEF_CHANNELS(channels),
		DEVICE_TYPE_DEF_PARAMS(params),

		.init = device_constant_init,
	};

	return register_device_type(stage, constant);
}
