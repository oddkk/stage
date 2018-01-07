#include "channel.h"
#include "stage.h"
#include "device.h"
#include "utils.h"

static channel_id _alloc_channels(struct stage * stage, unsigned int count)
{
	channel_id range_begin;

	if (count == 0) {
		return -1;
	}

	if (stage->num_channels + count >= stage->cap_channels) {
		print_error("channel",
			    "Could not allocate channels because the "
			    "program is out of channels. The current capacity is %i.",
			    stage->cap_channels);
		return -1;
	}

	range_begin = stage->num_channels;
	stage->num_channels += count;

	return range_begin;
}

channel_id allocate_channels(struct stage * stage, struct scalar_type type,
			     unsigned int count)
{
	channel_id range_begin;

	range_begin = _alloc_channels(stage, count);

	for (channel_id i = range_begin; i < range_begin + count; i++) {
		stage->channels[i].type = type;
	}

	return range_begin;
}

static void _alloc_device_output_channels(struct stage * stage, struct type * type, device_id dev_id, int output_id, channel_id * next_channel, int * next_subindex)
{
	switch (type->kind) {
	case TYPE_KIND_SCALAR: {
		channel_id new_channel;
		int new_subindex;
		struct channel *cnl;

		new_channel = *next_channel;
		*next_channel += 1;

		new_subindex = *next_subindex;
		*next_subindex += 1;


		cnl = &stage->channels[new_channel];

		cnl->connection_type = CHANNEL_CONNECTION_DEVICE;
		cnl->device.id = dev_id;
		cnl->device.output_id = output_id;
		cnl->device.output_subindex = new_subindex;
	} break;

	case TYPE_KIND_TUPLE:
		for (size_t i = 0; i < type->tuple.num_types; i++) {
			type_id child_type_id;
			struct type *child;

			child_type_id = type->tuple.types[i];
			child = stage->types[child_type_id];

			_alloc_device_output_channels(stage, child, output_id, dev_id, next_channel, next_subindex);
		}
		break;

	case TYPE_KIND_STRING:
		break;

	default:
		print_error("alloc device channels", "Invalid type kind for type %.*s (%i).", type->name, type->id);
		break;
	}
}

channel_id allocate_device_output_channels(struct stage *stage, device_id dev_id)
{
	struct device *device;
	struct device_type *dev_type;
	channel_id range_begin;
	channel_id next_output;
	int num_scalars = 0;
	int output_id = 0;
	int next_subindex = 0;

	if (dev_id >= stage->num_devices) {
		return -1;
	}

	device = stage->devices[dev_id];

	if (device->type >= stage->num_device_types) {
		return -1;
	}

	dev_type = stage->device_types[device->type];

	for (size_t i = 0; i < dev_type->num_outputs; ++i) {
		struct type *t;

		if (dev_type->outputs[i].type > stage->num_types) {
			print_error("alloc device channels", "Invalid type for device type '%.*s'.",
						LIT(dev_type->name));
			return -1;
		}

		t = stage->types[dev_type->outputs[i].type];

		num_scalars += t->num_scalars;
	}

	range_begin = _alloc_channels(stage, num_scalars);
	next_output = range_begin;

	for (size_t i = 0; i < dev_type->num_outputs; ++i) {
		struct type *t;

		if (dev_type->outputs[i].type > stage->num_types) {
			print_error("alloc device channels", "Invalid type for device type '%.*s'.",
						LIT(dev_type->name));
			return -1;
		}


		t = stage->types[dev_type->outputs[i].type];

		_alloc_device_output_channels(stage, t, dev_id, output_id, &next_output, &next_subindex);

		output_id += 1;
	}

	return range_begin;
}

channel_id allocate_device_input_channels(struct stage *stage, device_id dev_id)
{
	struct device *device;
	struct device_type *dev_type;
	channel_id range_begin;
	channel_id next_output;
	int num_scalars = 0;

	if (dev_id >= stage->num_devices) {
		return -1;
	}

	device = stage->devices[dev_id];

	if (device->type >= stage->num_device_types) {
		return -1;
	}

	dev_type = stage->device_types[device->type];

	for (size_t i = 0; i < dev_type->num_inputs; ++i) {
		struct type *t;

		if (dev_type->inputs[i].type > stage->num_types) {
			print_error("alloc device channels", "Invalid type for device type '%.*s'.",
						LIT(dev_type->name));
			return -1;
		}

		t = stage->types[dev_type->inputs[i].type];

		num_scalars += t->num_scalars;
	}

	range_begin = _alloc_channels(stage, num_scalars);
	next_output = range_begin;

	return range_begin;
}

static bool has_cycle(struct stage *stage, channel_id origin, channel_id dest)
{
	struct channel *channel;

	channel = &stage->channels[origin];

	while (channel->connection_type == CHANNEL_CONNECTION_CHANNEL) {
		if (channel->connection < 0) {
			break;
		}
		if (channel->connection == dest) {
			return true;
		}
		channel = &stage->channels[channel->connection];
	}

	return false;
}

int channel_unbind(struct stage *stage, channel_id channel)
{
	struct channel *cnl;

	if (channel >= stage->num_channels || channel < 0) {
		return -1;
	}

	cnl = &stage->channels[channel];

	switch (cnl->connection_type) {
	case CHANNEL_CONNECTION_CHANNEL:
		cnl->connection = 0;
		break;

	case CHANNEL_CONNECTION_CALLBACK:
		cnl->callback = 0;
		break;

	case CHANNEL_CONNECTION_CONSTANT:
		cnl->constant = 0;
		break;

	case CHANNEL_CONNECTION_UNCONNECTED:
		print_error("unbind channel",
			    "Can not unbind channel %i becuase "
			    "it is not already bound.", channel);
		return -1;

	default:
		print_error("unbind channel",
			    "Channel %i has an invalid connection.", channel);
		return -1;
	}

	cnl->connection_type = CHANNEL_CONNECTION_UNCONNECTED;

	return 0;
}

int channel_bind(struct stage *stage, channel_id src, channel_id dest)
{
	struct channel *dest_channel;

	if (src >= stage->num_channels || src < 0) {
		return -1;
	}
	if (dest >= stage->num_channels || dest < 0) {
		return -1;
	}

	if (src == dest) {
		return -1;
	}

	dest_channel = &stage->channels[dest];

	if (dest_channel->connection_type != CHANNEL_CONNECTION_UNCONNECTED) {
		print_error("bind channel",
			    "Can not bind channel %i to channel %i becuase "
			    "the destination channel is already bound.", src,
			    dest);
		return -1;
	}

	if (has_cycle(stage, src, dest)) {
		print_error("bind channel",
			    "Can not bind channel %i to channel %i becuase "
			    "this would result in a cycle.", src, dest, dest);
		return -1;
	}

	dest_channel->connection = src;
	dest_channel->connection_type = CHANNEL_CONNECTION_CHANNEL;

	return 0;
}

int channel_bind_callback(struct stage *stage, channel_id channel,
			  channel_eval_callback callback)
{
	struct channel *cnl;

	if (channel >= stage->num_channels || channel < 0) {
		return -1;
	}

	cnl = &stage->channels[channel];

	if (cnl->connection_type != CHANNEL_CONNECTION_UNCONNECTED) {
		print_error("bind channel",
			    "Can not bind channel %i to callback because "
			    "this channel is already bound.", channel);
		return -1;
	}

	cnl->connection_type = CHANNEL_CONNECTION_CALLBACK;
	cnl->callback = callback;

	return 0;
}

int channel_bind_constant(struct stage *stage, channel_id channel,
			  scalar_value value)
{
	struct channel *cnl;

	if (channel >= stage->num_channels || channel < 0) {
		return -1;
	}

	cnl = &stage->channels[channel];

	if (cnl->connection_type != CHANNEL_CONNECTION_UNCONNECTED) {
		print_error("bind channel",
			    "Can not bind channel %i to constant because "
			    "this channel is already bound.", channel);
		return -1;
	}

	cnl->connection_type = CHANNEL_CONNECTION_CONSTANT;
	cnl->constant = value;

	return 0;
}

int channel_bind_device(struct stage *stage, channel_id channel,
						device_id dev_id, int output_id)
{
	struct channel *cnl;

	if (channel >= stage->num_channels || channel < 0) {
		return -1;
	}

	cnl = &stage->channels[channel];

	if (cnl->connection_type != CHANNEL_CONNECTION_UNCONNECTED) {
		print_error("bind channel",
			    "Can not bind channel %i to constant because "
			    "this channel is already bound.", channel);
		return -1;
	}

	cnl->connection_type = CHANNEL_CONNECTION_DEVICE;
	cnl->device.id = dev_id;
	cnl->device.output_id = output_id;

	return 0;
}


scalar_value eval_channel(struct stage * stage, channel_id channel)
{
	struct channel *chnl = &stage->channels[channel];
	scalar_value result;

	switch (chnl->connection_type) {
	case CHANNEL_CONNECTION_UNCONNECTED:
		result = SCALAR_OFF;
		break;

	case CHANNEL_CONNECTION_CHANNEL:{
			if (chnl->connection == -1) {
				result = SCALAR_OFF;
			} else {
				result = eval_channel(stage, chnl->connection);
			}
		}
		break;

	case CHANNEL_CONNECTION_CONSTANT:
		result = chnl->constant;
		break;

	case CHANNEL_CONNECTION_CALLBACK: {
			if (!chnl->callback) {
				result = SCALAR_OFF;
				print_error("channel eval",
					    "Missing callback function for channel %i",
					    channel);
				break;
			}
			result = chnl->callback(stage, channel, chnl);
		}
		break;

	case CHANNEL_CONNECTION_DEVICE: {
		struct device *dev;
		struct device_type *type;

		dev = stage->devices[chnl->device.id];
		type = stage->device_types[dev->type];

		if (type->eval) {
			result = type->eval(stage, dev, type,
								chnl->device.output_id, chnl->device.output_subindex);
		} else {
			result = SCALAR_OFF;
		}
	} break;

	default:
		result = SCALAR_OFF;
		print_error("channel eval",
			    "Invalid channel connection type for channel %i.",
			    channel);
		break;
	}

	return result;
}
