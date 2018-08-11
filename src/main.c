// TODO:
//  * Make separate "apply lists" for device types, to allow device
//    creation outside config
//  * Fix referencing elements of tuples, both named and unnamed
//  * Implement const fields in config (fields that are only used for
//    calculations)
//  * Make types "first-class" (a type type)?
//  * Allow "templated" types in device types for channels and
//    attributes (type types). Concider if channels should be created
//    at init instead of as part of device type definition.
//  * Clean up config.c

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
	struct config_node *node;

	if (!check_clock_support()) {
		panic("No alternative clock supported yet.");
	}

	err = stage_init(&stage);
	if (err) {
		return err;
	}

	parse_config_file(STR("config/simple.conf"), &stage.atom_table,
			  &stage.memory, &node);

	config_print_tree(node);

	apply_config(&stage, node);

#if 1
	printf
	    ("============================ types ============================\n");
	for (int i = 0; i < stage.num_types; i++) {
		struct type *type;

		type = get_type(&stage, i);

		print_type(&stage, type);
		printf(" ");
		expand_type(&stage, type, false);
		//expand_type(&stage, type, true);
		printf("\n");
	}
	printf("\n");

	printf
	    ("======================== devices_types ========================\n");
	for (int i = 0; i < stage.num_device_types; i++) {
		struct device_type *dev_type;

		dev_type = get_device_type(&stage, i);

		describe_device_type(&stage, dev_type);
		printf("\n");
	}

	printf
	    ("=========================== devices ===========================\n");
	for (int i = 0; i < stage.num_devices; i++) {
		struct device *dev;

		dev = get_device(&stage, i);

		describe_device(&stage, dev);
		printf("\n");
	}
#endif

	stage.tick_period = NSEC / 1000;

	frame_duration.tv_sec = stage.tick_period / NSEC;
	frame_duration.tv_nsec = stage.tick_period % NSEC;
	tick_begin = read_time();

	while (true) {
		struct timespec tick_end_desired;
		int clock_err;

		stage_tick(&stage);

		tick_end_desired = timespec_add(tick_begin, frame_duration);

		clock_err =
		    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
				    &tick_end_desired, 0);
		if (clock_err) {
			perror("clock_nanosleep");
		}

		tick_begin = read_time();
	}

	return 0;
}
