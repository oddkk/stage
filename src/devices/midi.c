#include "../stage.h"
#include "../device.h"
#include "../scoped_hash.h"
#include "../utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/soundcard.h>
#include <poll.h>

struct device_launchpad_data {
	channel_id channel_button_color;
	uint64_t button_state;
	int fd;

	scalar_value last_button_color[64];
};

static void device_launchpad_tick(struct stage *stage, struct device *dev)
{
	struct device_launchpad_data *data = (struct device_launchpad_data *)dev->data;
	struct pollfd fds = {
		.fd = data->fd,
		.events = POLLIN,
		.revents = 0,
	};

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

	for (size_t i = 0; i < 64; i++) {
		scalar_value r, g;
		r = eval_channel(stage, data->channel_button_color + i * 2);
		g = eval_channel(stage, data->channel_button_color + i * 2 + 1);

		uint8_t button_color = (r & 0x03) | ((g & 0x03) << 4);
		if (button_color != data->last_button_color[i]) {
			uint8_t key = (uint8_t)(i % 8) | ((uint8_t)(i / 8) << 4);
			uint8_t packet[4] = {0x90, key, button_color};

			write(data->fd, packet, sizeof(packet));

			data->last_button_color[i] = button_color;
		}
	}
}

static scalar_value device_launchpad_out(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_launchpad_data *data;
	device = get_device(stage, cnl->device.id);
	data = (struct device_launchpad_data *)device->data;

	scalar_value res = (scalar_value)!!(data->button_state &
										(1UL << (uint64_t)cnl->device.channel_subindex));

	return res;
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

	for (size_t i = 0; i < 64; i++) {
		channel_bind_callback(stage, button_down + i, device_launchpad_out);
	}
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

	ns_midi
		= scoped_hash_namespace(&stage->root_scope, SATOM(stage, "midi"));

	ns_novation
		= scoped_hash_namespace(ns_midi, SATOM(stage, "novation"));

	struct device_type *launchpad;

	launchpad
		= register_device_type_scoped(stage,
									  STR("launchpad"),
									  ns_novation);

	struct type *button_state_array_type;
	button_state_array_type
		= register_array_type(stage, NULL, stage->standard_types.integer, 8);
	button_state_array_type
		= register_array_type(stage, NULL, button_state_array_type->id, 8);

	struct device_channel_def *button_down;
	button_down
		= device_type_add_output(stage, launchpad, STR("button_down"),
								 button_state_array_type->id);

	struct named_tuple_member button_color_members[] = {
		{.name = SATOM(stage, "red"),   .type = stage->standard_types.integer},
		{.name = SATOM(stage, "green"), .type = stage->standard_types.integer},
	};
	struct type *button_color_type;
	button_color_type
		= register_named_tuple_type(stage, SATOM(stage, "button_color"),
									button_color_members,
									ARRAY_LENGTH(button_color_members));
	register_type_name(stage,
					   button_color_type->id,
					   ns_novation,
					   button_color_type->name);

	struct type *button_color_array_type;
	button_color_array_type
		= register_array_type(stage, NULL, button_color_type->id, 8);
	button_color_array_type
		= register_array_type(stage, NULL, button_color_array_type->id, 8);

	device_type_add_input(stage, launchpad, STR("button_color"),
						  button_color_array_type->id);

	launchpad->device_init = device_launchpad_init;

	finalize_device_type(launchpad);

	return launchpad;
}
