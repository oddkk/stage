#include "mod.h"
#include "channel.h"
#include "../base/mod.h"
#include <stdlib.h>

static int64_t
print_node_callback(struct vm *vm, void *user_data,
					size_t num_inputs, int64_t *inputs)
{
	struct string *str = user_data;
	assert(num_inputs == 1);
	printf("%.*s = %li\n", LIT(*str), inputs[0]);
	return inputs[0];
}

/* BUILTIN_IMPURE(PrintNode, print_int_node_callback, STG_INT, (STG_INT, in)) */
/* { */
/* } */

BUILTIN_PURE(Print, print_node_construct, CNL_NODE, (STG_STR, name))
{
	struct channel_system *cnls = data;
	channel_id cnl = alloc_channel(cnls);

	channel_id *inputs = calloc(1, sizeof(channel_id));
	inputs[0] = alloc_channel(cnls);

	struct string *str = calloc(1, sizeof(struct string));
	*str = name;

	bind_channel_callback(cnls, cnl, inputs, 1, print_node_callback, str);

	return inputs[0];
}

BUILTIN_PURE(Node, int_node_construct, CNL_NODE, (STG_INT, val))
{
	struct channel_system *cnls = data;
	channel_id cnl = alloc_channel(cnls);

	bind_channel_const(cnls, cnl, val);

	return cnl;
}

BUILTIN_IMPURE(op->, node_bind, CNL_NODE, (CNL_NODE, src), (CNL_NODE, drain))
{
	struct channel_system *cnls = data;
	bind_channel(cnls, src, drain);

	printf("bound (val) %li\n", eval_channel(cnls, drain));
	return 0;
}

struct type_base channel_node_base = {
	.name = STR("node"),
};

int mod_channel_init(struct stg_module *mod)
{
	struct channel_system *sys;
	sys = calloc(1, sizeof(struct channel_system));
	mod->data = sys;

	channel_system_init(sys, 100);

	stg_register_builtin_type(mod, &channel_node_base,
							  STG_TYPE_DATA(CNL_NODE));
	stg_register_builtin_func(mod, print_node_construct, sys);
	stg_register_builtin_func(mod, int_node_construct, sys);
	stg_register_builtin_func(mod, node_bind, sys);
	return 0;
}

void mod_channel_free(struct stg_module *mod)
{
	free(mod->data);
}

struct stg_module_info mod_channel = {
	.name    = STR(MOD_CHANNEL),
	.version = {0, 1},

	.init = mod_channel_init,
	.free = mod_channel_free,
};
