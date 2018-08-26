#include "../stage.h"
#include "../device.h"
#include "../utils.h"

int device_constant_init(struct stage *stage, struct device_type *dev_type, struct device *dev)
{
	channel_id out_channel;
	struct value_ref value;
	int err;

	out_channel = device_get_output_channel_id_by_name(stage, dev, STR("out"));
	// out_channel = device_get_channel(stage, dev, STR("out"));
	if (out_channel < 0) {
		return -1;
	}

	err = device_get_attr(stage, dev, STR("value"), &value);
	if (err) {
		return -1;
	}

	channel_bind_constant(stage, out_channel, value);

	return 0;
}

struct device_type *register_device_type_constant(struct stage *stage)
{
	struct device_type *constant;
	struct device_channel_def *channel_out;

	struct type_template_context params_type = {0};
	struct device_type_param params[] = {
		{ .name=STR("T"),     .type=stage->standard_types.type },
		{ .name=STR("value"), .template=STR("T") },
	};

	/* struct device_type_channel channels[] = { */
	/* 	{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("out"), .template=STR("T"), .self=true }, */
	/* } */

/* #define DEVICE_TYPE_CHANNELS(cnls) .channels={.channels=(cnls), .num_channels=ARRAY_LENGTH(cnls)} */
/* #define DEVICE_TYPE_PARAMS(par) .params={.params=(par), .num_params=ARRAY_LENGTH(par)} */

/* 	struct device_type_def constant = { */
/* 		.name = STR("basic.constant"), */

/* 		DEVICE_TYPE_CHANNELS(channels), */
/* 		DEVICE_TYPE_PARAMS(params), */

/* 		.init = device_constant_init, */
/* 	}; */

/* 	register_device_type(stage, constant); */

	/* constant = */
	/* 	register_device_type(stage,    STR("constant"), */
	/* 						 params,   ARRAY_LENGTH(params), */
	/* 						 channels, ARRAY_LENGTH(channels), */
	/* 						 STR("basic")); */
	/* constant->device_init = device_constant_init; */

	params_type = make_device_type_params_type(stage, params, ARRAY_LENGTH(params));

	constant = register_device_type(stage, STR("constant"), params_type);
	constant->device_init = device_constant_init;

	struct type_template_context out_type = {0};
	struct access_pattern out_template_pattern = {0};

	int err;
	err = parse_access_pattern(&stage->atom_table, STR("T"), &out_template_pattern);
	assert(!err);

	out_type.type = register_template_type(stage, NULL, out_template_pattern, &out_type)->id;

	channel_out = device_type_add_output_template(stage, constant, STR("out"),
												  out_type);
	constant->self_output = channel_out->id;

	finalize_device_type(constant);

	return constant;
}
