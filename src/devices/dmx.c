#ifdef STAGE_DMX

#define _POSIX_C_SOURCE 199309L

/* enable clock_* functions */
#ifndef __USE_POSIX199309
#define __USE_POSIX199309
#endif

#ifndef __USE_XOPEN2K
#define __USE_XOPEN2K
#endif


#include "../stage.h"
#include "../device.h"
#include "../utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#include <time.h>
#include <errno.h>

#define NSEC (1000000000)

#include <ftdi.h>

#define DMX_NUM_CHANNELS 512

// @TODO: Figure out why this is not included.
extern int clock_nanosleep (clockid_t __clock_id, int __flags,
			    const struct timespec *__req,
			    struct timespec *__rem);

struct device_dmx_data {
	pthread_t thread;
	bool should_quit;
	channel_id channel_in;

	struct ftdi_context *ftdi;

	scalar_value values[3][DMX_NUM_CHANNELS];

	int free_buffer;

	int producer_buffer;
	int consumer_buffer;
};

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

static void *device_dmx_thread(void *user_data)
{
	struct device_dmx_data *data = (struct device_dmx_data *)user_data;
	uint8_t buffer[DMX_NUM_CHANNELS + 1] = {0};
	uint64_t frame_period = NSEC / 30;

	struct timespec frame_duration;
	frame_duration.tv_sec = frame_period / NSEC;
	frame_duration.tv_nsec = frame_period % NSEC;

	ftdi_setrts(data->ftdi, 0);

	struct timespec frame_begin;
	frame_begin = read_time();

	while (!data->should_quit) {
		int new_buffer;
		new_buffer =
			__atomic_exchange_n(&data->free_buffer,
								data->consumer_buffer,
								__ATOMIC_RELAXED);
		data->consumer_buffer = new_buffer;

		for (size_t i = 0; i < DMX_NUM_CHANNELS; i++) {
			buffer[i + 1] = data->values[data->consumer_buffer][i];
		}

		ftdi_set_line_property2(data->ftdi, BITS_8, STOP_BIT_2, NONE, BREAK_ON);
		ftdi_set_line_property2(data->ftdi, BITS_8, STOP_BIT_2, NONE, BREAK_OFF);

		size_t bytes_written = 0;
		while (bytes_written < DMX_NUM_CHANNELS + 1) {
			int err;
			err = ftdi_write_data(data->ftdi, buffer, DMX_NUM_CHANNELS + 1);
			if (err < 0) {
				print_error("dmx", "Failed to write dmx output: %s.",
							ftdi_get_error_string(data->ftdi));
				break;
			}
			bytes_written += err;
		}

		struct timespec frame_end_desired;
		frame_end_desired = timespec_add(frame_begin, frame_duration);

		int clock_err;
		clock_err =
		    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
				    &frame_end_desired, 0);
		if (clock_err && clock_err != EINTR) {
			perror("clock_nanosleep");
		}

		frame_begin = read_time();
	}

	return NULL;
}

static void device_dmx_tick(struct stage *stage, struct device *dev)
{
	struct device_dmx_data *data = (struct device_dmx_data *)dev->data;

	for (size_t i = 0; i < DMX_NUM_CHANNELS; i++) {
		data->values[data->producer_buffer][i]
			= eval_channel(stage, data->channel_in + i);
	}

	int new_buffer;
	new_buffer =
		__atomic_exchange_n(&data->free_buffer,
							data->producer_buffer,
							__ATOMIC_RELAXED);
	data->producer_buffer = new_buffer;
}

static int device_dmx_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_dmx_data *data = calloc(1, sizeof(struct device_dmx_data));
	dev->data = data;

	data->consumer_buffer = 0;
	data->producer_buffer = 1;
	data->free_buffer = 2;

	data->channel_in = device_get_input_channel_id_by_name(stage, dev, STR("in"));

	data->ftdi = ftdi_new();
	if (!data->ftdi) {
		print_error("dmx", "Failed to initialize ftdi.");
		free(data);
		return -1;
	}

	int err;

	err = ftdi_usb_open(data->ftdi, 0x0403, 0x6001);
	if (err) {
		print_error("dmx", "Failed to open dmx device: %s.", ftdi_get_error_string(data->ftdi));
		ftdi_free(data->ftdi);
		free(data);
		return -1;
	}

	err = ftdi_set_baudrate(data->ftdi, 250000);
	if (err) {
		print_error("dmx", "Failed to set baud rate: %s.", ftdi_get_error_string(data->ftdi));
		ftdi_free(data->ftdi);
		free(data);
		return -1;
	}

	err = pthread_create(&data->thread, NULL, device_dmx_thread, data);
	if (err) {
		print_error("dmx", "Failed to open dmx device: %s.", ftdi_get_error_string(data->ftdi));
		ftdi_free(data->ftdi);
		free(data);
		return -1;
	}

	register_device_tick_callback(stage, dev, 1, device_dmx_tick);

	return 0;
}

static void device_dmx_free(struct stage *stage, struct device *dev)
{
	struct device_dmx_data *data = (struct device_dmx_data *)dev->data;

	int err;

	data->should_quit = true;
	pthread_join(data->thread, NULL);

	err = ftdi_usb_close(data->ftdi);
	if (err) {
		print_error("dmx", "Failed to close dmx device: %s. Continuing anyways.",
					ftdi_get_error_string(data->ftdi));
	}

	ftdi_free(data->ftdi);

	free(data);
}

struct device_type *register_device_type_dmx(struct stage *stage)
{
	struct device_type_channel channels[] = {
		{
			.kind = DEVICE_CHANNEL_INPUT,
			.name = STR("in"),
			.type = register_array_type(stage, NULL,
										stage->standard_types.integer, 512)->id,
		},
	};

	struct device_type_def device = {
		.name = STR("basic.lighting.dmx"),
		.init = device_dmx_init,
		.free = device_dmx_free,

		DEVICE_TYPE_DEF_CHANNELS(channels),
	};

	return register_device_type(stage, device);
}

#endif
