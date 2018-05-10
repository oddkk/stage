#include "../stage.h"
#include "../device.h"

int device_constant_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct attribute_value *val;
	channel_id out_channel;

	out_channel = device_get_output_channel_id_by_name(stage, dev, STR("out"));

	if (out_channel < 0) {
		return -1;
	}

	val = device_get_attr(stage, dev, stage_atom(stage, STR("value")));

	channel_bind_constant(stage, out_channel, val->value);

	return 0;
}

struct device_type *register_device_type_constant(struct stage *stage)
{
	struct device_type *constant;

	constant = register_device_type(stage, STR("constant"));
	constant->device_init = device_constant_init;

	device_type_add_attribute(stage, constant, STR("value"), 0);
	device_type_add_output(stage, constant, STR("out"),
						   stage->standard_types.integer);

	return constant;
}
