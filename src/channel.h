#ifndef STAGE_CHANNEL_H
#define STAGE_CHANNEL_H

#include "type.h"

typedef int channel_id;
typedef unsigned int device_id;

struct stage;
struct device;
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

enum device_channel_kind {
	DEVICE_CHANNEL_NO = 0,
	DEVICE_CHANNEL_INPUT,
	DEVICE_CHANNEL_OUTPUT,
};

struct channel {
	type_id type;
	size_t type_subindex;
	enum channel_connection_type connection_type;
	bool dirty;
	enum device_channel_kind device_channel;
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
channel_id find_device_channel(struct stage *stage, struct device *dev,
			       enum device_channel_kind kind, int channel,
			       int subindex);

int channel_unbind(struct stage *, channel_id channel);
int channel_bind(struct stage *, channel_id src, channel_id dest);
int channel_bind_callback(struct stage *, channel_id channel,
			  channel_eval_callback);
int channel_bind_constant(struct stage *, channel_id channel,
						  struct value_ref value);
scalar_value eval_channel(struct stage *stage, channel_id);

void channel_describe_connection(struct stage *stage, channel_id);
void channel_describe(struct stage *stage, channel_id);

#endif
