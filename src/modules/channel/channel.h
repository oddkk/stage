#ifndef STG_CHANNEL_CHANNEL_H
#define STG_CHANNEL_CHANNEL_H

#include "mod.h"

enum channel_kind {
	CHANNEL_UNBOUND = 0,
	CHANNEL_BOUND,
	CHANNEL_CONSTANT,
	CHANNEL_CALLBACK,
};

typedef int64_t (*channel_callback)(struct vm *, void *user_data,
									size_t num_inputs, int64_t *inputs);

struct channel {
	enum channel_kind kind;

	union {
		channel_id src;
		int64_t constant;
		struct {
			size_t num_inputs;
			channel_id *inputs;
			channel_callback callback;
			void *user_data;
		} callback;
	};
};

struct channel_system {
	struct channel *channels;
	size_t num_channels;
	size_t cap_channels;
};

void
channel_system_init(struct channel_system *, size_t cap);

channel_id
alloc_channel(struct channel_system *);

void
unbind_channel(struct channel_system *, channel_id);

int
bind_channel(struct channel_system *, channel_id, channel_id);

void
bind_channel_const(struct channel_system *, channel_id, int64_t);

void
bind_channel_callback(struct channel_system *, channel_id,
					  channel_id *inputs, size_t num_inputs,
					  channel_callback callback, void *data);

int64_t
eval_channel(struct channel_system *cnls, channel_id cnl_id);

#endif
