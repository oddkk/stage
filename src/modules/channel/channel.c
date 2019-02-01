#include "channel.h"
#include <string.h>
#include <stdlib.h>

void
channel_system_init(struct channel_system *cnls, size_t cap)
{
	cnls->num_channels = 0;
	cnls->cap_channels = cap;
	cnls->channels = calloc(cnls->cap_channels,
							sizeof(struct channel));
}

channel_id
alloc_channel(struct channel_system *cnls)
{
	assert(cnls->num_channels + 1 < cnls->cap_channels);
	channel_id id = cnls->num_channels;
	cnls->num_channels += 1;

	memset(&cnls->channels[id], 0, sizeof(struct channel));

	return id;
}

void
unbind_channel(struct channel_system *cnls, channel_id cnl_id)
{
	struct channel *cnl = &cnls->channels[cnl_id];
	cnl->kind = CHANNEL_UNBOUND;
}

static bool
channel_has_path(struct channel_system *cnls, channel_id from, channel_id to)
{
	if (from == to) {
		return true;
	}

	struct channel *cnl = &cnls->channels[to];
	switch (cnl->kind) {
	case CHANNEL_UNBOUND:
		return false;

	case CHANNEL_BOUND:
		return channel_has_path(cnls, from, cnl->src);

	case CHANNEL_CONSTANT:
		return false;

	case CHANNEL_CALLBACK:
		for (size_t i = 0; i < cnl->callback.num_inputs; i++) {
			if (channel_has_path(cnls, from, cnl->callback.inputs[i])) {
				return true;
			}
		}
		return false;
	}

	panic("Invalid channel kind.");
	return false;
}

int
bind_channel(struct channel_system *cnls, channel_id cnl_id, channel_id src)
{
	struct channel *cnl = &cnls->channels[cnl_id];

	if (channel_has_path(cnls, cnl_id, src)) {
		printf("Channels have cycle (%zu -> %zu)\n", src, cnl_id);
		return -1;
	}

	cnl->kind = CHANNEL_BOUND;
	cnl->src = src;

	return 0;
}

void
bind_channel_const(struct channel_system *cnls, channel_id cnl_id, int64_t val)
{
	struct channel *cnl = &cnls->channels[cnl_id];
	cnl->kind = CHANNEL_CONSTANT;
	cnl->constant = val;
}

void
bind_channel_callback(struct channel_system *cnls, channel_id cnl_id,
					  channel_id *inputs, size_t num_inputs,
					  channel_callback callback, void *data)
{
	struct channel *cnl = &cnls->channels[cnl_id];
	cnl->kind = CHANNEL_CALLBACK;
	cnl->callback.num_inputs = num_inputs;
	cnl->callback.inputs = inputs;
	cnl->callback.callback = callback;
	cnl->callback.user_data = data;
}

int64_t
eval_channel(struct channel_system *cnls, channel_id cnl_id)
{
	struct channel *cnl = &cnls->channels[cnl_id];

	switch (cnl->kind) {
	case CHANNEL_UNBOUND:
		return 0;

	case CHANNEL_BOUND:
		return eval_channel(cnls, cnl->src);

	case CHANNEL_CONSTANT:
		return cnl->constant;

	case CHANNEL_CALLBACK: {
		int64_t args[cnl->callback.num_inputs];
		for (size_t i = 0; i < cnl->callback.num_inputs; i++) {
			args[i] = eval_channel(cnls, cnl->callback.inputs[i]);
		}
		return cnl->callback.callback(NULL, cnl->callback.user_data,
									  cnl->callback.num_inputs, args);
	} break;
	}

	panic("Invalid channel kind.");
	return 0;
}
