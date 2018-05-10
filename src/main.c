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


	parse_config_file(STR("config/main.conf"), &stage.atom_table, &stage.memory, &node);
	/* config_print_tree(node); */
	apply_config(&stage, node);

	/* printf("config_node: %lu\n", sizeof(struct config_node)); */


	stage.tick_period = NSEC / 1000;

	frame_duration.tv_sec = stage.tick_period / NSEC;
	frame_duration.tv_nsec = stage.tick_period % NSEC;
	tick_begin = read_time();

	while (false) {
		struct timespec tick_end_desired;
		int clock_err;

		stage_tick(&stage);

		tick_end_desired =
		    timespec_add(tick_begin, frame_duration);

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
