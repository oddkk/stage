#include "../mod.h"
#include "../stream.h"
#include "../system.h"
#include "nodes.h"

#include <native.h>
#include <module.h>

struct stream_node_fmap_data {
	struct stg_func_object fn;
	struct stream_data in;
};

static struct bc_result
stream_node_fmap_gen_bytecode(struct bc_env *env, struct arena *mem, void *in_data)
{
	struct stream_node_fmap_data *node = in_data;

	struct bc_result res;
	bc_var in_var;
	res = node->in.kind->gen_bytecode(env, mem, node->in.data);
	in_var = res.out_var;

	struct bc_instr *instr;
	instr = bc_gen_clcall(env, BC_VAR_NEW,
			node->fn.func, node->fn.closure);
	res.out_var = instr->clcall.target;

	append_bc_instr(&res, instr);

	return res;
}

static void
stream_node_fmap_copy(struct stg_exec *heap, void *data)
{
	struct stream_node_fmap_data *node = data;
	node->in = stream_copy_stream_data(heap, node->in);
}

static struct stream_node_kind_decl stream_funct_decl_fmap = {
	.gen_bytecode = stream_node_fmap_gen_bytecode,
	.copy_node = stream_node_fmap_copy,
};

static struct stream_data
stream_funct_fmap(
		struct stg_exec *heap,
		struct stg_module *mod,
		struct stg_func_object fn,
		struct stream_data in)
{
	struct stream_data res = {0};

	res.kind = stream_get_node_kind(
			mod, mod_atoms(mod, "fmap"));

	res.data_size = sizeof(struct stream_node_fmap_data);
	res.data = stg_alloc(heap, 1, res.data_size);

	struct stream_node_fmap_data *data;
	data = res.data;

	data->fn = fn;
	data->in = in;

	return res;
}

STREAM_NODE_KIND_DECL(fmap, stream_funct_decl_fmap, stream_funct_fmap,
		STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE)
