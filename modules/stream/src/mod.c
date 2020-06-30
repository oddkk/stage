#include "mod.h"
#include "freq.h"
#include "stream.h"
#include <module.h>
#include <native.h>
#include "nodes/nodes.h"

struct stream_mod_info *
stream_mod_get_info(struct vm *vm)
{
	struct stg_module *stream_mod;
	stream_mod = vm_get_module(vm, vm_atoms(vm, "stream"));
	assert(stream_mod);

	struct stream_mod_info *mod_info;
	mod_info = stream_mod->data;
	assert(mod_info);

	return mod_info;
}

static int
mod_stream_register(struct stg_module *mod)
{
	struct stream_mod_info *info;
	info = arena_alloc(&mod->mem, sizeof(struct stream_mod_info));
	mod->data = info;

	stream_register_freq_type(mod);
	stream_register_stream(mod);

	return 0;
}

static int
mod_stream_pre_init(struct stg_module *mod)
{
	struct stream_mod_info *info;
	info = stream_mod_get_info(mod->vm);

	info->sys = arena_alloc(&mod->mem,
			sizeof(struct stream_system));

	stream_system_init(mod->vm, info->sys);

#define NODE_DEF(name) stream_mod_init_node_##name(mod);
	STREAM_DEFAULT_NODES
#undef NODE_DEF

	return 0;
}

static int
mod_stream_start(struct stg_module *mod)
{
	struct stream_system *sys;
	sys = stream_get_system(mod->vm);

	int err;
	err = stream_system_start(sys);
	if (err) {
		return err;
	}

	return 0;
}

static int
mod_stream_stop(struct stg_module *mod)
{
	struct stream_system *sys;
	sys = stream_get_system(mod->vm);

	stream_system_stop(sys);

	return 0;
}

static int
mod_stream_load(struct stg_native_module *mod)
{
	mod->hook_register = mod_stream_register;
	mod->hook_pre_init = mod_stream_pre_init;
	mod->hook_start    = mod_stream_start;
	mod->hook_stop     = mod_stream_stop;

	stream_mod_load_freq_type(mod);
	stream_mod_load_stream(mod);

#define NODE_DEF(name) stream_mod_register_node_##name(mod);
	STREAM_DEFAULT_NODES
#undef NODE_DEF

	return 0;
}

STAGE_MODULE("stream", mod_stream_load);
