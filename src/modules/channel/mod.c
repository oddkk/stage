#include "mod.h"
#include "channel.h"
#include "../base/mod.h"
#include <stdlib.h>


void bind_channel_builtin(
		struct stg_module *mod, struct channel_system *cnls,
		channel_id cnl_id, struct stg_builtin_func func,
		void *user_data)
{
	struct object callback;
	callback =
		stg_register_builtin_func_obj(mod, func, user_data);

	channel_id *inputs;
	inputs = calloc(func.num_params, sizeof(channel_id));

	for (size_t i = 0; i < func.num_params; i++) {
		type_id param_type_id;

		param_type_id =
			vm_find_type_id(mod->vm,
				func.params[i].type.mod,
				func.params[i].type.name);

		inputs[i] = alloc_channel(cnls, param_type_id);
	}

	bind_channel_callback(cnls, cnl_id, inputs, func.num_params,
			callback, user_data);
}

BUILTIN_IMPURE(PrintNode, print_int_node_callback, STG_INT, (STG_INT, in))
{
	struct string *str = data;
	printf("%.*s = %li\n", LIT(*str), in);
	return in;
}

BUILTIN_IMPURE(Print, print_node_construct, CNL_NODE, (STG_STR, name))
{
	struct channel_system *cnls = data;

	type_id int_tid = vm_find_type_id(vm, STR("base"), STR("int"));
	channel_id cnl = alloc_channel(cnls, int_tid);

	struct string *str = calloc(1, sizeof(struct string));
	*str = name;

	mark_channel_notify(cnls, cnl);
	bind_channel_builtin(mod, cnls, cnl, print_int_node_callback, str);

	return get_channel(cnls, cnl)->callback.inputs[0];
}

BUILTIN_PURE(Node, node_type_constructor, STG_TYPE, (STG_TYPE, T))
{
	/* struct channel_system *cnls = data; */
	/* channel_id cnl = alloc_channel(cnls); */

	/* bind_channel_const(cnls, cnl, val); */

	/* return cnl; */

	return 0;
}

BUILTIN_PURE(Node, int_node_construct, CNL_NODE, (STG_INT, val))
{
	struct channel_system *cnls = data;
	type_id int_tid = vm_find_type_id(vm, STR("base"), STR("int"));
	channel_id cnl = alloc_channel(cnls, int_tid);

	struct object const_val;
	const_val = obj_register_integer(vm, &mod->store, val);

	bind_channel_const(cnls, cnl, const_val);

	return cnl;
}

void
print_channel_downstream(struct channel_system *cnls);

BUILTIN_IMPURE(op->, node_bind, CNL_NODE, (CNL_NODE, src), (CNL_NODE, drain))
{
	struct channel_system *cnls = data;
	bind_channel(cnls, src, drain);
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

	channel_system_init(sys, 1024);

	stg_register_builtin_type(mod, &channel_node_base,
							  STG_TYPE_DATA(CNL_NODE));
	stg_register_builtin_func(mod, print_node_construct, sys);
	stg_register_builtin_func(mod, int_node_construct, sys);
	stg_register_builtin_func(mod, node_type_constructor, sys);
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
