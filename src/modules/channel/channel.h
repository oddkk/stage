#ifndef STG_CHANNEL_CHANNEL_H
#define STG_CHANNEL_CHANNEL_H

#include "mod.h"
#include <pthread.h>

enum channel_kind {
	CHANNEL_UNBOUND = 0,
	CHANNEL_BOUND,
	CHANNEL_CONSTANT,
	CHANNEL_CALLBACK,
};

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

	uintmax_t *downstream_channels;
	uintmax_t *dirty_channels;
	uintmax_t *notify_channels;

	struct vm *vm;

	pthread_t thread;
	bool should_quit;
};

int
channel_system_init(struct channel_system *, size_t cap, struct vm *vm);

int
channel_system_start(struct channel_system *);

void
channel_system_destroy(struct channel_system *);

static inline struct channel *
get_channel(struct channel_system *cnls, channel_id cnl)
{
	assert(cnl < cnls->num_channels);
	return &cnls->channels[cnl];
}

channel_id
alloc_channel(struct channel_system *, type_id type);

void
unbind_channel(struct channel_system *, channel_id);

int
bind_channel(struct stg_module *, struct channel_system *, channel_id src, channel_id dest);

int
bind_channel_const(struct stg_module *, struct channel_system *, channel_id, struct object);

int
bind_channel_callback(struct stg_module *, struct channel_system *, channel_id,
					  channel_id *inputs, size_t num_inputs,
					  struct object callback, void *data);

void
mark_channel_dirty(struct channel_system *, channel_id);

void
mark_channel_notify(struct channel_system *, channel_id);

struct object
eval_channel(struct vm *vm, struct channel_system *cnls, struct exec_stack *stack, channel_id cnl_id);

#endif
