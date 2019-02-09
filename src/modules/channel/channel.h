#ifndef STG_CHANNEL_CHANNEL_H
#define STG_CHANNEL_CHANNEL_H

#include "mod.h"

enum channel_kind {
	CHANNEL_UNBOUND = 0,
	CHANNEL_BOUND,
	CHANNEL_CONSTANT,
	CHANNEL_CALLBACK,
};

/* typedef int64_t (*channel_callback)(struct vm *, void *user_data, */
/* 									size_t num_inputs, int64_t *inputs); */

struct channel {
	enum channel_kind kind;

	type_id out_type;

	union {
		channel_id src;
		struct object constant;
		struct {
			size_t num_inputs;
			type_id func_type;
			struct object func;
			channel_id *inputs;
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
bind_channel(struct channel_system *, channel_id src, channel_id dest);

void
bind_channel_const(struct channel_system *, channel_id, struct object);

void
bind_channel_callback(struct channel_system *, channel_id,
					  channel_id *inputs, size_t num_inputs,
					  struct object callback, void *data);

struct object
eval_channel(struct vm *vm, struct channel_system *cnls, struct exec_stack *stack, channel_id cnl_id);

#endif
