#include "mod.h"
#include "channel.h"
#include <module.h>
#include <native.h>
#include <base/mod.h>
#include <stdlib.h>

int
mod_channel_init(struct ast_context *ctx, struct stg_module *mod)
{
	struct channel_system *sys;
	sys = calloc(1, sizeof(struct channel_system));
	mod->data = sys;

	channel_system_init(sys, 1024, mod->vm);
	return 0;
}

int
mod_channel_start(struct stg_module *mod)
{
	struct channel_system *cnls = mod->data;
	channel_system_start(cnls);
	return 0;
}

void
mod_channel_free(struct stg_module *mod)
{
	struct channel_system *cnls = mod->data;

	channel_system_destroy(cnls);

	free(mod->data);
}

int
mod_channel_load(struct stg_native_module *mod)
{
	mod->hook_init = mod_channel_init;
	mod->hook_free = mod_channel_free;
	mod->hook_start = mod_channel_start;

	return 0;
}

STAGE_MODULE(channel, mod_channel_load);

/*
STAGE_MODULE(channel) = {
	.name    = STR("channel"),
	.version = {0, 1},

	.init = mod_channel_init,
	.free = mod_channel_free,

	.start = mod_channel_start,
};
*/
