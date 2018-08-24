#include "../stage.h"
#include "../device.h"
#include "../utils.h"

int device_constant_init(struct stage *stage, struct device_type *dev_type, struct device *dev)
{
	struct value_ref val;
	channel_id out_channel;

	out_channel = device_get_output_channel_id_by_name(stage, dev, STR("out"));

	if (out_channel < 0) {
		return -1;
	}

	type_id type;

	// @TODO: Implement
	type = stage->standard_types.integer;

	val = alloc_value(stage, type);
	/* val = device_get_attr(stage, dev, stage_atom(stage, STR("value"))); */
	/* channel_bind_constant(stage, out_channel, val); */

	return 0;
}

struct device_type *register_device_type_constant(struct stage *stage)
{
	struct device_type *constant;
	struct device_channel_def *channel_out;

	type_id params_type;
	struct device_type_param params[] = {
		{ .name=STR("T"),     .type=stage->standard_types.type },

		// @TODO: This should be templated.
		{ .name=STR("value"), .type=stage->standard_types.integer },
	};

	params_type = make_device_type_params_type(stage, params, ARRAY_LENGTH(params));


	constant = register_device_type(stage, STR("constant"), params_type);
	constant->device_init = device_constant_init;

	channel_out = device_type_add_output(stage, constant, STR("out"),
										 stage->standard_types.integer);

	constant->self_output = channel_out->id;

	finalize_device_type(constant);

	return constant;
}
