#include "../stage.h"
#include "../device.h"
#include "../scoped_hash.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/soundcard.h>
#include <poll.h>

struct device_launchpad_data {
	channel_id channel_button_color;
	scalar_value last_button_color;
	uint64_t button_state;
	int fd;
};

static void device_launchpad_tick(struct stage *stage, struct device *dev)
{
	struct device_launchpad_data *data = (struct device_launchpad_data *)dev->data;
	struct pollfd fds = {
		.fd = data->fd,
		.events = POLLIN,
		.revents = 0,
	};

	/* printf("tick\n"); */

	uint8_t buffer[3];
	int poll_err;
	while ((poll_err = poll(&fds, 1, 0)) > 0) {
		int err;
		err = read(data->fd, buffer, sizeof(buffer));
		if (err < 0) {
			perror("read");
			break;
		}

		// Ensure the both the upper 8 bits and the lower 8 bits both
		// constrain to [0,7].
		if (buffer[0] == 0x90 && (buffer[1] & 0x88) == 0x00) {
			int x, y;
			x = buffer[1] & 0x07;
			y = (buffer[1] & 0x70) >> 4;

			uint64_t set = !!buffer[2];

			data->button_state ^= (-set ^ data->button_state) & (1UL << (x+y*8));
		}
	}

	if (poll_err < 0) {
		perror("poll");
		return;
	}

	scalar_value button_color;
	button_color = eval_channel(stage, data->channel_button_color);

	if (button_color != data->last_button_color) {
		uint8_t packet[4] = {0x90, 0x00, button_color & 0x03};

		write(data->fd, packet, sizeof(packet));

		data->last_button_color = button_color;
	}
}

static scalar_value device_launchpad_out(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_launchpad_data *data;
	device = get_device(stage, cnl->device.id);
	data = (struct device_launchpad_data *)device->data;

	// @TODO: Multiple outputs
	return !!(data->button_state & 0x1);
}

static int device_launchpad_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_launchpad_data *data = calloc(1, sizeof(struct device_launchpad_data));
	dev->data = data;

	data->fd = open("/dev/midi3", O_RDWR, 0);
	if (data->fd < 0) {
		perror("open");
		return -1;
	}

	channel_id button_down;
	button_down = device_get_output_channel_id_by_name(stage, dev, STR("button_down"));

	channel_bind_callback(stage, button_down, device_launchpad_out);
	register_device_tick_callback(stage, dev, 1, device_launchpad_tick);

	data->channel_button_color
		= device_get_input_channel_id_by_name(stage, dev, STR("button_color"));

	if (data->channel_button_color < 0) {
		return -1;
	}

	return 0;
}

struct device_type *register_device_type_midi(struct stage *stage) {
	struct scoped_hash *ns_midi;
	struct scoped_hash *ns_novation;

	ns_midi = scoped_hash_insert_namespace(&stage->root_scope,
										   SATOM(stage, "midi"),
										   NULL);

	ns_novation = scoped_hash_insert_namespace(ns_midi,
											   SATOM(stage, "novation"),
											   NULL);

	struct device_type *launchpad;

	launchpad
		= register_device_type_scoped(stage,
									  STR("launchpad"),
									  ns_novation);

	struct device_channel_def *button_down;

	button_down
		= device_type_add_output(stage, launchpad, STR("button_down"),
								 stage->standard_types.integer);

	device_type_add_input(stage, launchpad, STR("button_color"),
						  stage->standard_types.integer);

	launchpad->device_init = device_launchpad_init;

	finalize_device_type(launchpad);

	return launchpad;
}
