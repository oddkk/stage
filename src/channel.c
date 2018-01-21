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

static void init_device_channels_for_type(struct stage *stage, channel_id *begin, device_id dev_id, int channel_id, type_id type_id, enum device_channel_kind kind)
{
	struct type *type;

	type = stage->types[type_id];

	for (int j = 0; j < type->num_scalars; ++j) {
		struct channel *cnl = &stage->channels[*begin];
		cnl->device_channel = kind;
		cnl->device.id = dev_id;
		cnl->device.channel_id = channel_id;
		cnl->device.channel_subindex = j;

		*begin += 1;
	}
}

static void init_device_channels(struct stage *stage, device_id dev_id, struct device_channel_def *channels, size_t num_channels, channel_id begin, enum device_channel_kind kind)
{
	for (size_t i = 0; i < num_channels; ++i) {
		init_device_channels_for_type(stage, &begin, dev_id, i, channels[i].type, kind);
	}
}

int allocate_device_channels(struct stage *stage, device_id dev_id)
{
	struct device *device;
	struct device_type *dev_type;
	size_t num_input_scalars = 0;
	size_t num_output_scalars = 0;
	channel_id channel_begin;

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

		t = get_type(stage, dev_type->inputs[i].type);
		if (!t) {
			return -1;
		}

		num_input_scalars += t->num_scalars;
	}

	for (size_t i = 0; i < dev_type->num_outputs; ++i) {
		struct type *t;

		t = get_type(stage, dev_type->outputs[i].type);
		if (!t) {
			return -1;
		}

		num_output_scalars += t->num_scalars;
	}

	channel_begin = _alloc_channels(stage, num_input_scalars + num_output_scalars);
	init_device_channels(stage, dev_id, dev_type->inputs, dev_type->num_inputs, channel_begin, DEVICE_CHANNEL_INPUT);
	init_device_channels(stage, dev_id, dev_type->outputs, dev_type->num_outputs, channel_begin + num_input_scalars, DEVICE_CHANNEL_OUTPUT);

	device->input_begin = channel_begin;
	device->output_begin = channel_begin + num_input_scalars;

	return 0;
}

channel_id find_device_channel(struct stage *stage, struct device *dev, enum device_channel_kind kind, int channel, int subindex)
{
	struct channel *cnl;
	channel_id dev_begin;

	if (kind == DEVICE_CHANNEL_INPUT) {
		dev_begin = dev->input_begin;
	} else if (kind == DEVICE_CHANNEL_OUTPUT) {
		dev_begin = dev->output_begin;
	} else {
		return -1;
	}

	assert(dev_begin < stage->num_channels);
	cnl = &stage->channels[dev_begin];

	for (channel_id i = dev_begin;
		 i < stage->num_channels &&
		 cnl->device.id == dev->id &&
			 cnl->device.channel_id <= channel;
			 ++i, ++cnl) {
		if (cnl->device.channel_id == channel &&
			cnl->device.channel_subindex == subindex &&
			cnl->device_channel == kind) {
			return i;
		}
	}
	return -1;
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

	dependency_matrix_bind(&stage->channel_deps, dest, src);

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
								chnl->device.channel_id, chnl->device.channel_subindex);
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

void channel_describe(struct stage *stage, channel_id cnl_id)
{
	struct channel *cnl;

	cnl = &stage->channels[cnl_id];

	printf("%i limit={%i..%i}", cnl_id, cnl->type.min, cnl->type.max);

	if (cnl->device_channel) {
		struct device *dev;
		struct device_type *dev_type;

		printf(" device_id=%i", cnl->device.id);

		dev = get_device(stage, cnl->device.id);
		if (dev) {
			printf(" device_name=%.*s", ALIT(dev->name));

			dev_type = get_device_type(stage, dev->type);
			if (dev_type) {
				printf(" device_type=%.*s", ALIT(dev_type->name));

				if (cnl->device_channel == DEVICE_CHANNEL_INPUT) {
					assert(cnl->device.channel_id < dev_type->num_inputs);
					printf(" input=%.*s",
						   ALIT(dev_type->inputs[cnl->device.channel_id].name));
				} else if (cnl->device_channel == DEVICE_CHANNEL_OUTPUT) {
					assert(cnl->device.channel_id < dev_type->num_outputs);
					printf(" output=%.*s",
						   ALIT(dev_type->outputs[cnl->device.channel_id].name));
				}
			} else {
				printf(" (invalid type)");
			}
		} else {
			printf(" (invalid device)");
		}
	}
}

void channel_describe_connection(struct stage *stage, channel_id cnl_id)
{
	struct channel *cnl;

	cnl = &stage->channels[cnl_id];

	switch (cnl->connection_type) {
	case CHANNEL_CONNECTION_UNCONNECTED: printf("unconnected"); break;
	case CHANNEL_CONNECTION_CHANNEL:
		printf("channel ");
		channel_describe(stage, cnl->connection);
	break;
	case CHANNEL_CONNECTION_CONSTANT: printf("constant %i", cnl->constant); break;
	case CHANNEL_CONNECTION_CALLBACK: printf("callback"); break;
	case CHANNEL_CONNECTION_DEVICE: printf("device"); break;
	}
	printf("\n");
}
