#include "../mod.h"
#include "../stream.h"
#include "../system.h"
#include "nodes.h"
#include "bytecode.h"

#include <native.h>
#include <module.h>

struct stream_node_const {
	type_id type;
	uint8_t data[];
};

static struct bc_result
stream_node_const_gen_bytecode(struct bc_env *env, struct arena *mem, void *in_data)
{
	struct stream_node_const *data = in_data;

	struct object obj = {0};
	obj.data = data->data;
	obj.type = data->type;

	obj = arena_copy_object(env->vm, mem, obj);

	struct bc_result res = {0};
	res.out_var = bc_alloc_var(env, obj.type);

	append_bc_instr(&res, bc_gen_load(env, res.out_var, obj));

	return res;
}

static void
stream_node_const_copy(struct stg_exec *heap, void *in_data)
{
	struct stream_node_const *data = in_data;

	struct type *value_type;
	value_type = vm_get_type(heap->vm, data->type);

	if (value_type->base->obj_copy) {
		value_type->base->obj_copy(heap,
				value_type->data, data->data);
	}
}

static struct stream_node_kind_decl stream_funct_decl_const = {
	.gen_bytecode = stream_node_const_gen_bytecode,
	.copy_node = stream_node_const_copy,
};

static void
stream_funct_const(void **args, size_t num_args, void *ret)
{
	assert(num_args == 5);
	struct stg_exec *heap = *(struct stg_exec **)args[0];
	struct stg_module *mod = *(struct stg_module **)args[1];
	void *value = args[2];
	type_id value_type_id = *(type_id *)args[3];
	// freq_t  freq = *(freq_t *)args[4];

	struct type *value_type;
	value_type = vm_get_type(mod->vm, value_type_id);

	struct stream_data data = {0};
	data.kind = stream_get_node_kind(
			mod, mod_atoms(mod, "const"));

	data.data_size = sizeof(struct stream_node_const) + value_type->size;
	data.data = stg_alloc(heap, 1, data.data_size);

	struct stream_node_const *node_data = data.data;
	node_data->type = value_type_id;

	memcpy(node_data->data, value, value_type->size);

	if (value_type->base->obj_copy) {
		value_type->base->obj_copy(heap,
				value_type->data, node_data->data);
	}

	struct stream_data *out = ret;
	*out = data;
}

STREAM_NODE_KIND_DECL(const, stream_funct_decl_const, stream_funct_const,
		STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE|STG_NATIVE_FUNC_REFS)
