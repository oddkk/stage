#include "utils.h"
#include "vm.h"
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

/* enable clock_* functions */
#ifndef __USE_POSIX199309
#define __USE_POSIX199309
#endif

#ifndef __USE_XOPEN2K
#define __USE_XOPEN2K
#endif

#include <time.h>
#include <errno.h>

#ifdef STAGE_TEST
#define main _main
#endif

#define NSEC (1000000000)

static bool should_quit = false;

void stage_signal_handler(int sig)
{
	if (sig == SIGINT) {
		should_quit = true;
	}
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

extern struct stg_module_info mod_base;
extern struct stg_module_info mod_channel;

int main(int argc, char *argv[])
{
	int err;
	struct timespec tick_begin;
	struct timespec frame_duration;

	if (!check_clock_support()) {
		panic("No alternative clock supported yet.");
	}

	struct vm vm;

	err = vm_init(&vm);
	if (err) {
		printf("Failed to initialize vm.\n");
		return -1;
	}

	vm_register_module(&vm, &mod_base);
	vm_register_module(&vm, &mod_channel);

	err = cfg_compile(&vm, STR("./config/"));
	if (err) {
		printf("Failed to compile config.\n");
		return -1;
	}

	return 0;

	uint64_t tick_period = NSEC / 1000;

	frame_duration.tv_sec = tick_period / NSEC;
	frame_duration.tv_nsec = tick_period % NSEC;
	tick_begin = read_time();

	signal(SIGINT, stage_signal_handler);

	while (!should_quit) {
		struct timespec tick_end_desired;
		int clock_err;

		tick_end_desired = timespec_add(tick_begin, frame_duration);

		clock_err =
		    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
				    &tick_end_desired, 0);
		if (clock_err && clock_err != EINTR) {
			perror("clock_nanosleep");
		}

		tick_begin = read_time();
	}

	return 0;
}

#ifdef STAGE_TEST
#undef main
#endif
