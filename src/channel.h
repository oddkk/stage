#ifndef STAGE_CHANNEL_H
#define STAGE_CHANNEL_H

#include "type.h"

typedef int channel_id;
typedef unsigned int device_id;

struct stage;
struct channel;

typedef scalar_value(*channel_eval_callback) (struct stage *, channel_id,
					      struct channel *);

enum channel_connection_type {
	CHANNEL_CONNECTION_UNCONNECTED = 0,
	CHANNEL_CONNECTION_CHANNEL,
	CHANNEL_CONNECTION_CONSTANT,
	CHANNEL_CONNECTION_CALLBACK,
	CHANNEL_CONNECTION_DEVICE,
};

struct channel {
	struct scalar_type type;
	enum channel_connection_type connection_type;
	bool dirty;
	struct {
		device_id id;
		int channel_id;
		int channel_subindex;
	} device;
	union {
		channel_id connection;
		scalar_value constant;
		channel_eval_callback callback;
	};
};

channel_id allocate_channels(struct stage *stage, struct scalar_type type,
			     unsigned int count);
int allocate_device_channels(struct stage *stage, device_id device);
int channel_unbind(struct stage *, channel_id channel);
int channel_bind(struct stage *, channel_id src, channel_id dest);
int channel_bind_callback(struct stage *, channel_id channel,
			  channel_eval_callback);
int channel_bind_constant(struct stage *, channel_id channel,
			  scalar_value value);
scalar_value eval_channel(struct stage *stage, channel_id);

#endif
