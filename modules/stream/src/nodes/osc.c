#include "../mod.h"
#include "../stream.h"
#include "../system.h"
#include "nodes.h"

#include <native.h>
#include <module.h>

struct stream_node_osc_data {
	struct stg_func_object fn;
	struct stream_node *freq;
};

static struct bc_result
stream_node_osc_gen_bytecode(struct bc_env *env, struct arena *mem, void *in_data)
{
	struct stream_node_osc_data *node = in_data;

	struct bc_result res;
	bc_var in_var;
	res = node->freq->kind->gen_bytecode(env, mem, node->freq->data);
	in_var = res.out_var;

	append_bc_instr(&res,
			bc_gen_push_arg(env, in_var));

	struct bc_instr *instr;
	if (node->fn.closure) {
		instr = bc_gen_clcall(env, BC_VAR_NEW,
				node->fn.func, node->fn.closure);
		res.out_var = instr->clcall.target;
	} else {
		instr = bc_gen_lcall(env, BC_VAR_NEW,
				node->fn.func);
		res.out_var = instr->lcall.target;
	}

	append_bc_instr(&res, instr);

	return res;
}

static void
stream_node_osc_copy(struct stg_exec *heap, void *data)
{
	struct stream_node_osc_data *node = data;
	node->freq = stream_copy_node_ref(heap, *node->freq);
}

static struct stream_node_kind_decl stream_funct_decl_osc = {
	.gen_bytecode = stream_node_osc_gen_bytecode,
	.copy_node = stream_node_osc_copy,
};

static struct stream_data
stream_funct_osc(
		struct stg_exec *heap,
		struct stg_module *mod,
		struct stg_func_object fn,
		struct stream_data freq)
{
	size_t data_size;
	data_size = sizeof(struct stream_node_osc_data);

	struct stream_node_osc_data *data;
	data = stg_alloc(heap, 1, data_size);

	data->fn = fn;
	data->freq = freq.node;

	struct stream_data res = {0};
	res.node = stream_alloc_node(
			heap, stream_get_node_kinds(mod, "osc"),
			(void *)data, data_size);

	return res;
}

STREAM_NODE_KIND_DECL(osc, stream_funct_decl_osc, stream_funct_osc,
		STG_NATIVE_FUNC_MODULE_CLOSURE|STG_NATIVE_FUNC_HEAP)
