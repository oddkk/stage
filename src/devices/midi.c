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
	channel_id channel_right_button_color;
	channel_id channel_top_button_color;
	uint64_t button_state;
	uint8_t right_button_state;
	uint8_t top_button_state;
	int fd;

	scalar_value last_button_color[64];
	scalar_value last_right_button_color[8];
	scalar_value last_top_button_color[8];
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
			// Grid buttons
			int x, y;
			x = buffer[1] & 0x0f;
			y = (buffer[1] & 0x70) >> 4;

			uint64_t set = !!buffer[2];

			data->button_state ^= (-set ^ data->button_state) & (1UL << (x+y*8));
		} else if (buffer[0] == 0x90 && (buffer[1] & 0x8f) == 0x08) {
			// Right-most buttons
			int y;
			y = (buffer[1] & 0x70) >> 4;

			uint64_t set = !!buffer[2];

			data->right_button_state ^= (-set ^ data->right_button_state) & (1 << y);
		} else if (buffer[0] == 0xb0 && (buffer[1] & 0xf8) == 0x68) {
			// Top-most buttons
			int x;
			x = buffer[1] & 0x07;

			uint64_t set = !!buffer[2];

			data->top_button_state ^= (-set ^ data->top_button_state) & (1 << x);
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
			uint8_t packet[3] = {0x90, key, button_color};

			write(data->fd, packet, sizeof(packet));

			data->last_button_color[i] = button_color;
		}
	}

	for (size_t i = 0; i < 8; i++) {
		scalar_value r, g;
		r = eval_channel(stage, data->channel_right_button_color + i * 2);
		g = eval_channel(stage, data->channel_right_button_color + i * 2 + 1);

		uint8_t button_color = (r & 0x03) | ((g & 0x03) << 4);
		if (button_color != data->last_right_button_color[i]) {
			uint8_t key = ((uint8_t)0x8) | ((uint8_t)(i) << 4);
			uint8_t packet[3] = {0x90, key, button_color};

			write(data->fd, packet, sizeof(packet));

			data->last_right_button_color[i] = button_color;
		}
	}

	for (size_t i = 0; i < 8; i++) {
		scalar_value r, g;
		r = eval_channel(stage, data->channel_top_button_color + i * 2);
		g = eval_channel(stage, data->channel_top_button_color + i * 2 + 1);

		uint8_t button_color = (r & 0x03) | ((g & 0x03) << 4);
		if (button_color != data->last_top_button_color[i]) {
			uint8_t key = ((uint8_t)i) | (uint8_t)(0x68);
			uint8_t packet[3] = {0xb0, key, button_color};

			write(data->fd, packet, sizeof(packet));

			data->last_top_button_color[i] = button_color;
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

static scalar_value device_launchpad_right_out(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_launchpad_data *data;
	device = get_device(stage, cnl->device.id);
	data = (struct device_launchpad_data *)device->data;

	scalar_value res = (scalar_value)!!(data->right_button_state &
										(1 << (uint8_t)cnl->device.channel_subindex));

	return res;
}

static scalar_value device_launchpad_top_out(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_launchpad_data *data;
	device = get_device(stage, cnl->device.id);
	data = (struct device_launchpad_data *)device->data;

	scalar_value res = (scalar_value)!!(data->top_button_state &
										(1 << (uint8_t)cnl->device.channel_subindex));

	return res;
}

static int device_launchpad_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_launchpad_data *data = calloc(1, sizeof(struct device_launchpad_data));
	dev->data = data;

	data->fd = open("/dev/midi1", O_RDWR, 0);
	if (data->fd < 0) {
		perror("open");
		return -1;
	}

	// Reset launchpad
	uint8_t reset_packet[3] = {0xb0, 0x00, 0x00};
	write(data->fd, reset_packet, sizeof(reset_packet));

	channel_id button_down;
	button_down = device_get_output_channel_id_by_name(stage, dev, STR("button_down"));

	for (size_t i = 0; i < 64; i++) {
		channel_bind_callback(stage, button_down + i, device_launchpad_out);
		struct channel *cnl;
		cnl = get_channel(stage, button_down + i);
		cnl->device.channel_subindex = i;
	}


	channel_id right_button_down;
	right_button_down = device_get_output_channel_id_by_name(stage, dev, STR("right_button_down"));

	for (size_t i = 0; i < 8; i++) {
		channel_bind_callback(stage, right_button_down + i, device_launchpad_right_out);
		struct channel *cnl;
		cnl = get_channel(stage, right_button_down + i);
		cnl->device.channel_subindex = i;
	}


	channel_id top_button_down;
	top_button_down = device_get_output_channel_id_by_name(stage, dev, STR("top_button_down"));

	for (size_t i = 0; i < 8; i++) {
		channel_bind_callback(stage, top_button_down + i, device_launchpad_top_out);
		struct channel *cnl;
		cnl = get_channel(stage, top_button_down + i);
		cnl->device.channel_subindex = i;
	}


	register_device_tick_callback(stage, dev, 1, device_launchpad_tick);

	data->channel_button_color
		= device_get_input_channel_id_by_name(stage, dev, STR("button_color"));

	data->channel_right_button_color
		= device_get_input_channel_id_by_name(stage, dev, STR("right_button_color"));

	data->channel_top_button_color
		= device_get_input_channel_id_by_name(stage, dev, STR("top_button_color"));

	if (data->channel_button_color < 0) {
		return -1;
	}

	return 0;
}

struct device_type *register_device_type_midi(struct stage *stage)
{
	struct scoped_hash *ns;

	ns = scoped_hash_namespace(&stage->root_scope, SATOM(stage, "midi"));
	ns = scoped_hash_namespace(ns, SATOM(stage, "novation"));

	struct type *button_state_array_type;
	struct type *button_state_grid_type;
	button_state_array_type
		= register_array_type(stage, NULL, stage->standard_types.integer, 8);
	button_state_grid_type
		= register_array_type(stage, NULL, button_state_array_type->id, 8);

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
					   ns,
					   button_color_type->name);

	struct type *button_color_array_type;
	button_color_array_type
		= register_array_type(stage, NULL, button_color_type->id, 8);

	struct type *button_color_grid_type;
	button_color_grid_type
		= register_array_type(stage, NULL, button_color_array_type->id, 8);

	struct device_type_channel channels[] = {
		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("button_color"), .type=button_color_grid_type->id },
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("button_down"),  .type=button_state_grid_type->id },

		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("right_button_color"), .type=button_color_array_type->id },
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("right_button_down"),  .type=button_state_array_type->id },

		{ .kind=DEVICE_CHANNEL_INPUT,  .name=STR("top_button_color"), .type=button_color_array_type->id },
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("top_button_down"),  .type=button_state_array_type->id },
	};

	struct device_type_def device = {
		.name = STR("midi.novation.launchpad"),
		.init = device_launchpad_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
	};

	return register_device_type(stage, device);
}
