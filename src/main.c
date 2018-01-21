#include "stage.h"
#include "channel.h"
#include "device.h"
#include "device_type.h"
#include "utils.h"
#include "config.h"

#include <stdio.h>
#include <stdlib.h>

/* enable clock_* functions */
#define __USE_POSIX199309
#define __USE_XOPEN2K
#include <time.h>
#include <errno.h>

#define NSEC (1000000000)

scalar_value device_add_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	scalar_value lhs, rhs;

	device = get_device(stage, cnl->device.id);

	lhs = eval_channel(stage, device->input_begin);
	rhs = eval_channel(stage, device->input_begin + 1);

	return lhs + rhs;
}

int device_add_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct atom *out = atom_create(&stage->atom_table, STR("out"));
	struct atom *left = atom_create(&stage->atom_table, STR("left"));
	struct atom *right = atom_create(&stage->atom_table, STR("right"));

	channel_id cnl_out, cnl_left, cnl_right;

	cnl_out   = device_get_output_channel_id(stage, dev, out);
	cnl_left  = device_get_input_channel_id(stage, dev, left);
	cnl_right = device_get_input_channel_id(stage, dev, right);

	if (cnl_out < 0 || cnl_left < 0 || cnl_right < 0) {
		return -1;
	}

	dependency_matrix_bind(&stage->channel_deps,
						   cnl_out, cnl_left);
	dependency_matrix_bind(&stage->channel_deps,
						   cnl_out, cnl_right);

	channel_bind_callback(stage, dev->output_begin, device_add_eval);

	return 0;
}

scalar_value device_cycle_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	return stage->tick % 100;
}

int device_cycle_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	channel_bind_callback(stage, dev->output_begin, device_cycle_eval);

	return 0;
}

static struct timespec timespec_add(struct timespec begin, struct timespec end)
{
	struct timespec temp;
	temp.tv_sec = begin.tv_sec + end.tv_sec;
	temp.tv_nsec = begin.tv_nsec + end.tv_nsec;
	temp.tv_sec += temp.tv_nsec / NSEC;
	temp.tv_nsec %= NSEC;
	return temp;
}

static struct timespec read_time()
{
	struct timespec time;
	int error;

	error = clock_gettime(CLOCK_MONOTONIC, &time);

	/*
	   NOTE: We should already have checked that we support
	   CLOCK_MONOTONIC so we should not receive EINVAL error. It
	   can return EFAULT if the second param is outside the memory we
	   can access. This would be a programming error.
	 */
	assert(!error);

	return time;
}

static bool check_clock_support()
{
	struct timespec time;
	int error;
	error = clock_gettime(CLOCK_MONOTONIC, &time);

	if (error == EINVAL) {
		print_error("clock",
			    "CLOCK_MONOTONIC_RAW is not supported on this system.\n");
		return false;
	}
	/*
	   NOTE: clock_gettime can return EFAULT if the second param is
	   outside the memory we can access. If that is the case, it is a
	   programming error.
	 */
	assert(!error);

	return true;
}

int main(int argc, char *argv[])
{
	int err;
	struct stage stage;
	struct timespec tick_begin;
	struct timespec frame_duration;

	if (!check_clock_support()) {
		panic("No alternative clock supported yet.");
	}

	err = stage_init(&stage);
	if (err) {
		return err;
	}

	stage.tick_period = NSEC / 100;

	register_default_types(&stage);

	struct device_type *device_add, *device_cycle;
	device_add = register_device_type(&stage, STR("add"));
	device_add->device_init = device_add_init;
	/* device_add->eval = device_add_eval; */

	device_type_add_input(&stage, device_add, STR("left"),
	                   stage.standard_types.integer);
	device_type_add_input(&stage, device_add, STR("right"),
	                   stage.standard_types.integer);

	device_type_add_output(&stage, device_add, STR("out"),
	                    stage.standard_types.integer);

	describe_device_type(&stage, device_add);
	printf("\n");

	device_cycle = register_device_type(&stage, STR("cycle"));
	device_cycle->device_init = device_cycle_init;

	device_type_add_output(&stage, device_cycle, STR("out"), stage.standard_types.integer);
	device_type_add_attribute(&stage, device_cycle, STR("min"), stage.standard_types.integer, (struct value){.scalar=0});
	device_type_add_attribute(&stage, device_cycle, STR("max"), stage.standard_types.integer, (struct value){.scalar=100});

	describe_device_type(&stage, device_cycle);
	printf("\n");

	struct device *dev_test, *dev_test2, *dev_test3;
	struct device_attribute cycle_attrs[] = {
		{.name=atom_create(&stage.atom_table, STR("min")), .value={.scalar=0},},
		{.name=atom_create(&stage.atom_table, STR("max")), .value={.scalar=0},},
	};

	dev_test = register_device(&stage, device_add->id, NULL, 0);
	dev_test->name = atom_create(&stage.atom_table, STR("hello"));

	dev_test2 = register_device(&stage, device_add->id, NULL, 0);
	dev_test2->name = atom_create(&stage.atom_table, STR("hello2"));

	dev_test3 = register_device(&stage, device_cycle->id, cycle_attrs, 2);
	dev_test3->name = atom_create(&stage.atom_table, STR("cycler"));

	channel_bind(&stage, dev_test->output_begin, dev_test2->input_begin);
	channel_bind(&stage, dev_test3->output_begin, dev_test->input_begin);
	channel_bind_constant(&stage, dev_test->input_begin+1, 2);
	channel_bind_constant(&stage, dev_test2->input_begin+1, 3);

	describe_device(&stage, dev_test);
	printf("\n");
	describe_device(&stage, dev_test2);
	printf("\n");
	describe_device(&stage, dev_test3);

	printf("\n");

	frame_duration.tv_sec = stage.tick_period / NSEC;
	frame_duration.tv_nsec = stage.tick_period % NSEC;
	tick_begin = read_time();

	while (true) {
		struct timespec tick_end_desired;
		int clock_err;
		
		printf("%c[2K", 27);
		printf("\rEval %i: ", dev_test->output_begin);
		print_scalar(eval_channel(&stage, dev_test2->output_begin));
		fflush(stdout);

		tick_end_desired =
		    timespec_add(tick_begin, frame_duration);

		clock_err =
			clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
							&tick_end_desired, 0);
		if (clock_err) {
			perror("clock_nanosleep");
		}

		tick_begin = read_time();
		stage.tick += 1;
	}

	return 0;

	/* parse_config_file(STR("config/main.conf"), &stage.atom_table, &stage.memory, &node); */

	/* apply_config(&stage, node); */

	/* config_print_tree(node); */

	/* printf("config_node: %lu\n", sizeof(struct config_node)); */

	/* return 0; */
}
