#include "channel.h"
#include "base/mod.h"
#include <string.h>
#include <stdlib.h>

struct channel_system *
get_channel_system(struct vm *vm)
{
	struct stg_module *mod;
	mod = vm_get_module(vm, vm_atoms(vm, "channel"));
	assert(mod != NULL);
	return (struct channel_system *)mod->data;
}

#define mask_bits_per_unit (sizeof(uintmax_t) * 8)
#define mask_num_units(num) ((num + mask_bits_per_unit - 1) / mask_bits_per_unit)
#define mask_length(num) (mask_num_units(num) * sizeof(uintmax_t))

static inline uintmax_t *
alloc_channel_bitfield(size_t num)
{
	return calloc(mask_num_units(num), sizeof(uintmax_t));
}

// static inline bool
// channel_is_marked(uintmax_t *mask, channel_id cnl)
// {
// 	return (mask[cnl / mask_bits_per_unit] >> (cnl % mask_bits_per_unit)) & 0x1;
// }

static inline void
mark_channel(uintmax_t *mask, channel_id cnl)
{
	mask[cnl / mask_bits_per_unit] |= (uintmax_t)1 << ((cnl % mask_bits_per_unit));
}

// static inline void
// unmark_channel(uintmax_t *mask, channel_id cnl)
// {
// 	mask[cnl / mask_bits_per_unit] &= ~((uintmax_t)1 << (cnl % mask_bits_per_unit));
// }

void
channel_mark_dependency(struct channel_system *cnls, channel_id src, channel_id drain)
{
	mark_channel(cnls->downstream_channels, (src * cnls->cap_channels) + drain);
}

inline static uintmax_t *
get_downstream_channels(struct channel_system *cnls, channel_id cnl)
{
	return &cnls->downstream_channels[cnl * mask_num_units(cnls->cap_channels)];
}


void
channel_clear_dependency(struct channel_system *cnls, channel_id cnl)
{
	memset(get_downstream_channels(cnls, cnl), 0,
			mask_length(cnls->cap_channels));
}

static inline void
channel_mask_or(uintmax_t *out, uintmax_t *in, size_t len)
{
	for (size_t i = 0; i < mask_num_units(len); i++) {
		out[i] |= in[i];
	}
}

static void *
channel_system_run(void *cnls);

int
channel_system_init(struct channel_system *cnls, size_t cap, struct vm *vm)
{
	cnls->vm = vm;
	cnls->num_channels = 0;
	cnls->cap_channels = cap;
	cnls->channels = calloc(cnls->cap_channels,
							sizeof(struct channel));

	assert(cnls->cap_channels % mask_bits_per_unit == 0);
	cnls->downstream_channels =
		alloc_channel_bitfield(cnls->cap_channels * cnls->cap_channels);

	cnls->dirty_channels =
		alloc_channel_bitfield(cnls->cap_channels);
	cnls->notify_channels =
		alloc_channel_bitfield(cnls->cap_channels);

	return 0;
}

int
channel_system_start(struct channel_system *cnls)
{
	int err;

	err = pthread_create(&cnls->thread, NULL, channel_system_run, cnls);
	if (err) {
		perror("pthread");
		return err;
	}

	return 0;
}

void
channel_system_destroy(struct channel_system *cnls)
{
	if (cnls->should_quit) {
		// Someone else is already destroying the system.
		return;
	}
	cnls->should_quit = true;
	pthread_join(cnls->thread, NULL);

	free(cnls->channels);
	free(cnls->downstream_channels);
	free(cnls->dirty_channels);
	free(cnls->notify_channels);
}

channel_id
alloc_channel(struct channel_system *cnls, type_id type)
{
	assert(cnls->num_channels + 1 < cnls->cap_channels);
	channel_id id = cnls->num_channels;
	cnls->num_channels += 1;

	memset(&cnls->channels[id], 0, sizeof(struct channel));
	cnls->channels[id].out_type = type;
	channel_mark_dependency(cnls, id, id);

	return id;
}

void
unbind_channel(struct channel_system *cnls, channel_id cnl_id)
{
	struct channel *cnl = get_channel(cnls, cnl_id);
	cnl->kind = CHANNEL_UNBOUND;
}

static bool
channel_has_path(struct channel_system *cnls, channel_id from, channel_id to)
{
	if (from == to) {
		return true;
	}

	struct channel *cnl = get_channel(cnls, to);
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

static void
refresh_channel_dep_mask(struct channel_system *cnls, uintmax_t *in_deps, channel_id cnl_id)
{
	uintmax_t *this_deps = get_downstream_channels(cnls, cnl_id);

	channel_mask_or(this_deps, in_deps, cnls->cap_channels);
	channel_mark_dependency(cnls, cnl_id, cnl_id);

	struct channel *cnl = get_channel(cnls, cnl_id);


	switch (cnl->kind) {
	case CHANNEL_UNBOUND:
		return;

	case CHANNEL_BOUND:
		channel_mark_dependency(cnls,
				cnl->src, cnl_id);
		refresh_channel_dep_mask(cnls, this_deps, cnl->src);
		return;

	case CHANNEL_CONSTANT:
		return;

	case CHANNEL_CALLBACK:
		for (size_t i = 0; i < cnl->callback.num_inputs; i++) {
			channel_mark_dependency(cnls,
					cnl->callback.inputs[i], cnl_id);
			refresh_channel_dep_mask(cnls, this_deps,
					cnl->callback.inputs[i]);
		}
		return;
	}

	panic("Invalid channel kind.");
	return;
}

int
bind_channel(struct stg_module *mod, struct channel_system *cnls, channel_id src, channel_id dest)
{
	struct channel *cnl = get_channel(cnls, dest);
	struct channel *src_cnl = get_channel(cnls, src);

	if (channel_has_path(cnls, dest, src)) {
		printf("Channels have cycle (%zu -> %zu)\n", src, dest);
		return -1;
	}

	assert(cnl->kind == CHANNEL_UNBOUND);

	cnl->kind = CHANNEL_BOUND;
	cnl->src = src;

	if (!type_equals(mod->vm, cnl->out_type, src_cnl->out_type)) {
		printf("Channel types does not match (%zu != %zu).\n",
				cnl->out_type, src_cnl->out_type);
		return -1;
	}

	uintmax_t *this_deps = get_downstream_channels(cnls, dest);
	refresh_channel_dep_mask(cnls, this_deps, dest);

	mark_channel_dirty(cnls, dest);

	return 0;
}

int
bind_channel_const(struct stg_module *mod, struct channel_system *cnls, channel_id cnl_id, struct object val)
{
	struct channel *cnl = &cnls->channels[cnl_id];

	assert(cnl->kind == CHANNEL_UNBOUND);

	cnl->kind = CHANNEL_CONSTANT;
	cnl->constant = val;

	if (!type_equals(mod->vm, cnl->out_type, val.type)) {
		printf("Channel types does not match (%zu != %zu).\n",
				cnl->out_type, val.type);
		return -1;
	}

	return 0;
}

int
bind_channel_callback(struct stg_module *mod,
					  struct channel_system *cnls, channel_id cnl_id,
					  channel_id *inputs, size_t num_inputs,
					  func_id func, void *data)
{
	struct channel *cnl = &cnls->channels[cnl_id];

	assert(cnl->kind == CHANNEL_UNBOUND);

	cnl->kind = CHANNEL_CALLBACK;
	cnl->callback.num_inputs = num_inputs;
	cnl->callback.inputs = inputs;
	cnl->callback.func = func;
	cnl->callback.user_data = data;

	struct func *func_inst = vm_get_func(cnls->vm, func);
	struct type *func_type = vm_get_type(cnls->vm, func_inst->type);
	struct stg_func_type *func_data = func_type->data;

	if (!type_equals(mod->vm, cnl->out_type, func_data->return_type)) {
		printf("Channel types does not match (%zu != %zu).\n",
				cnl->out_type, func_data->return_type);
		return -1;
	}

	uintmax_t *this_deps = get_downstream_channels(cnls, cnl_id);
	refresh_channel_dep_mask(cnls, this_deps, cnl_id);

	mark_channel_dirty(cnls, cnl_id);

	return 0;
}

void
mark_channel_dirty(struct channel_system *cnls, channel_id cnl)
{
	channel_mask_or(cnls->dirty_channels,
			get_downstream_channels(cnls, cnl),
			cnls->cap_channels);
}

void
mark_channel_notify(struct channel_system *cnls, channel_id cnl)
{
	mark_channel(cnls->notify_channels, cnl);
}

/*
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

		stack_push(stack, cnl->callback.func.data, func_type->size);

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
*/

static inline int
channel_ffs(uintmax_t val)
{
#if defined(__clang__)
	return __builtin_ctz(val);
#elif defined(__GNUC__)
	return __builtin_fft(val);
#else
#error "channel_ffs not implement for this platform yet."
#endif
}

static void
channel_do_notify(struct channel_system *cnls, channel_id id)
{
	printf("Notify %zu\n", id);

	/*
	struct exec_stack stack;
	struct arena mem = arena_push(&cnls->vm->memory);

	arena_alloc_stack(&stack, &mem, 1024); //mem.capacity - mem.head - 1);

	eval_channel(cnls->vm, cnls, &stack, id);

	arena_pop(&cnls->vm->memory, mem);
	*/
}

static void *
channel_system_run(void *data)
{
	struct channel_system *cnls = data;
	printf("Started channel system.\n");

	while (!cnls->should_quit) {
		for (size_t i = 0; i < mask_num_units(cnls->num_channels); i++) {
			uintmax_t to_notify =
				cnls->dirty_channels[i] & cnls->notify_channels[i];
			uintmax_t remaining = to_notify;
			while (remaining != 0) {
				int b = channel_ffs(remaining);

				channel_do_notify(cnls, (i * sizeof(uintmax_t) * 8) + b);

				remaining &= ~((uintmax_t)1 << b);
			}
			cnls->dirty_channels[i] &= ~to_notify;
		}
	}

	printf("Stopped channel system.\n");

	return NULL;
}
