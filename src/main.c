#include "stage.h"
#include "channel.h"
#include "device.h"
#include "device_type.h"
#include "utils.h"

#include <stdio.h>
#include <stdlib.h>

scalar_value device_add_eval(struct stage *stage, struct device *device, struct device_type *type, int output, int output_subindex) {
	scalar_value lhs, rhs;

	lhs = eval_channel(stage, device->input_begin);
	rhs = eval_channel(stage, device->input_begin + 1);

	return lhs + rhs;
}

int main(int argc, char *argv[])
{
	struct stage stage;
	struct scalar_type int_type;
	int error;

	int_type.min = SCALAR_MIN;
	int_type.max = SCALAR_MAX;

	zero_memory(&stage, sizeof(struct stage));
	error = arena_init(&stage.memory, MEGABYTE(10));

	if (error) {
		return -1;
	}

	stage.device_types_lookup.string_arena = &stage.memory;
	stage.device_types_lookup.page_arena = &stage.memory;
	stage.types_lookup.string_arena = &stage.memory;
	stage.types_lookup.page_arena = &stage.memory;

	stage.cap_channels = 300;
	stage.channels =
	    arena_alloc(&stage.memory,
			sizeof(struct channel) * stage.cap_channels);

	channel_id cnl = allocate_channels(&stage, int_type, 2);
	channel_id cnl2 = cnl + 1;

	if (channel_bind(&stage, cnl, cnl2) != 0) {
		return -1;
	}

	if (channel_bind_constant(&stage, cnl, 2) != 0) {
		return -1;
	}

	printf("Eval %i: ", cnl);
	print_scalar(eval_channel(&stage, cnl2));
	printf("\n");

	register_default_types(&stage);

	struct device_type *device_add;
	device_add = register_device_type(&stage, STR("add"));
	device_add->eval = device_add_eval;

	device_type_add_input(device_add, STR("left"),
	                   stage.standard_types.integer);
	device_type_add_input(device_add, STR("right"),
	                   stage.standard_types.integer);

	device_type_add_output(device_add, STR("out"),
	                    stage.standard_types.integer);
	device_type_add_output(device_add, STR("out2"),
	                    stage.standard_types.integer);


	struct device *dev;

	dev = register_device(&stage, device_add->id, 0, 0);

	channel_bind_constant(&stage, dev->input_begin, 2);
	channel_bind_constant(&stage, dev->input_begin + 1, 2);
	
	printf("Eval output: ");
	print_scalar(eval_channel(&stage, dev->output_begin));
	printf("\n");

	return 0;
}
