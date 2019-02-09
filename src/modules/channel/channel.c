#include "channel.h"
#include "../base/mod.h"
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
bind_channel(struct channel_system *cnls, channel_id src, channel_id dest)
{
	struct channel *cnl = &cnls->channels[dest];

	if (channel_has_path(cnls, dest, src)) {
		printf("Channels have cycle (%zu -> %zu)\n", src, dest);
		return -1;
	}

	cnl->kind = CHANNEL_BOUND;
	cnl->src = src;

	return 0;
}

void
bind_channel_const(struct channel_system *cnls, channel_id cnl_id, struct object val)
{
	struct channel *cnl = &cnls->channels[cnl_id];
	cnl->kind = CHANNEL_CONSTANT;
	cnl->constant = val;
}

void
bind_channel_callback(struct channel_system *cnls, channel_id cnl_id,
					  channel_id *inputs, size_t num_inputs,
					  struct object callback, void *data)
{
	struct channel *cnl = &cnls->channels[cnl_id];
	cnl->kind = CHANNEL_CALLBACK;
	cnl->callback.num_inputs = num_inputs;
	cnl->callback.inputs = inputs;
	cnl->callback.func = callback;
	cnl->callback.user_data = data;
}

struct object
eval_channel(struct vm *vm, struct channel_system *cnls, struct exec_stack *stack, channel_id cnl_id)
{
	struct channel *cnl = &cnls->channels[cnl_id];

	switch (cnl->kind) {
	case CHANNEL_UNBOUND: {
		struct type *type = vm_get_type(vm, cnl->out_type);
		struct object res;
		res.type = cnl->out_type;
		res.data = stack->sp;

		stack_push_void(stack, type->size);
		return res;
	}

	case CHANNEL_BOUND:
		return eval_channel(vm, cnls, stack, cnl->src);

	case CHANNEL_CONSTANT: {
		assert(cnl->constant.type == cnl->out_type);
		struct type *type = vm_get_type(vm, cnl->out_type);
		struct object res;
		res.type = cnl->out_type;
		res.data = stack->sp;

		stack_push(stack, cnl->constant.data, type->size);
		return res;
	}

	case CHANNEL_CALLBACK: {
		uint8_t *prev_bp = stack->bp;
		uint8_t *prev_sp = stack->sp;

		for (ssize_t i = cnl->callback.num_inputs - 1; i >= 0; i--) {
			eval_channel(vm, cnls, stack, cnl->callback.inputs[i]);
		}

		stack->bp = stack->sp;

		struct type *func_type;
		func_type = vm_get_type(vm, cnl->callback.func.type);

		func_type->base->eval(vm, stack, cnl->callback.user_data);

		struct type_func *type_func = func_type->data;
		struct type *ret_type = vm_get_type(vm, type_func->ret);

		struct object res;
		res.type = cnl->out_type;
		res.data = stack->sp - ret_type->size;

		stack->sp = prev_sp;
		stack->bp = prev_bp;

		stack_push(stack, res.data, ret_type->size);

		return res;
	}
	}

	panic("Invalid channel kind.");
	return OBJ_NONE;
}
