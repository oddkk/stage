#include "mod.h"
#include "../channel/channel.h"

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/soundcard.h>
#include <poll.h>

struct mod_midi_data {
	pthread_t thread;
	bool should_quit;
	int fd;

	uint64_t buttons_down;
	uint8_t top_buttons_down;
	uint8_t right_buttons_down;

	struct channel_system *cnls;
};

static void *
mod_midi_device_thread(void *in_data)
{
	struct mod_midi_data *data = in_data;

	printf("Started MIDI system.\n");

	struct pollfd fds = {
		.fd = data->fd,
		.events = POLLIN,
		.revents = 0,
	};


	uint8_t buffer[3];
	while (!data->should_quit) {
		int err;

		err = poll(&fds, 1, 1000);
		if (err < 0) {
			perror("poll");
			break;
		} else if (err == 0) {
			continue;
		}

		err = read(data->fd, buffer, sizeof(buffer));
		if (err < 0) {
			perror("read");
		}

		if (buffer[0] == 0x90 && (buffer[1] & 0x88) == 0x00) {
			// Grid buttons
			int x, y;
			x = buffer[1] & 0x0f;
			y = (buffer[1] & 0x70) >> 4;

			bool set = !!buffer[2];

			data->buttons_down ^= (-set ^ data->buttons_down) & (1UL << (x+y*8));

			printf("%i,%i: %i\n", x, y, set);

		} else if (buffer[0] == 0x90 && (buffer[1] & 0x8f) == 0x08) {
			// Right-most buttons
			int y;
			y = (buffer[1] & 0x70) >> 4;

			bool set = !!buffer[2];

			data->right_buttons_down ^= (-set ^ data->right_buttons_down) & (1UL << y);

			printf("r,%i: %i\n", y, set);

		} else if (buffer[0] == 0xb0 && (buffer[1] & 0xf8) == 0x68) {
			// Top-most buttons
			int x;
			x = buffer[1] & 0x07;

			bool set = !!buffer[2];

			data->right_buttons_down ^= (-set ^ data->right_buttons_down) & (1UL << x);

			printf("%i,t: %i\n", x, set);
		}
	}

	printf("MIDI system stopped.\n");
	return NULL;
}

int
mod_midi_init(struct stg_module *mod)
{
	struct mod_midi_data *data;
	data = calloc(1, sizeof(struct mod_midi_data));
	mod->data = data;
	data->cnls = get_channel_system(mod->vm);

	return -1;
}

int
mod_midi_start(struct stg_module *mod)
{
	struct mod_midi_data *data = mod->data;

	data->fd = open("/dev/midi4", O_RDWR, 0);

	if (data->fd < 0) {
		perror("open");
		return 0;
	}

	data->should_quit = false;
	pthread_create(&data->thread, NULL, mod_midi_device_thread, data);

	return -1;
}

void
mod_midi_free(struct stg_module *mod)
{
	struct mod_midi_data *data = mod->data;
	if (data->should_quit) {
		// Someone else is already destroying the system.
		return;
	}

	data->should_quit = true;
	pthread_join(data->thread, NULL);

	close(data->fd);

	free(data);
}

struct stg_module_info mod_midi = {
	.name    = STR(MOD_MIDI),
	.version = {0, 1},

	.init = mod_midi_init,
	.free = mod_midi_free,

	.start = mod_midi_start,
};

