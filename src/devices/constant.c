#include "../stage.h"
#include "../device.h"

int device_constant_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	channel_id value_channel;

	value_channel = device_get_output_channel_id_by_name(stage, dev, STR("value"));

	if (value_channel < 0) {
		return -1;
	}

	channel_bind_constant(stage, value_channel, 4);

	return 0;
}

struct device_type *register_device_type_constant(struct stage *stage)
{
	struct device_type *constant;

	constant = register_device_type(stage, STR("constant"));
	constant->device_init = device_constant_init;

	device_type_add_output(stage, constant, STR("value"),
						   stage->standard_types.integer);

	return constant;
}
