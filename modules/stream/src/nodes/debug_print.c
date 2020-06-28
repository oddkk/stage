#include "../mod.h"
#include "../stream.h"
#include "../system.h"
#include "nodes.h"
#include "bytecode.h"

#include <native.h>
#include <module.h>
#include <init_monad.h>

struct stream_debug_print_monad_data {
	struct stream_data in;
	freq_t freq;
};

static void
stream_debug_print_monad_unsafe(struct stg_init_context *ctx,
		struct stg_exec *heap, void *data, void *out)
{
	struct stream_debug_print_monad_data *closure = data;
	(void)closure;

	printf("reg print\n");

	// TODO
}

static void
stream_debug_print_monad_copy(struct stg_exec *heap, void *data)
{
	struct stream_debug_print_monad_data *closure = data;
	closure->in = stream_copy_stream_data(heap, closure->in);
}

static struct stg_init_data
stream_funct_debug_print(struct stg_exec *heap, struct stream_data in, freq_t freq)
{
	struct stg_init_data data = {0};
	data.call = stream_debug_print_monad_unsafe;
	data.copy = stream_debug_print_monad_copy;
	data.data_size = sizeof(struct stream_debug_print_monad_data);
	data.data = stg_alloc(heap, 1, sizeof(struct stream_debug_print_monad_data));

	struct stream_debug_print_monad_data *closure = {0};
	closure = data.data;
	closure->in = in;
	closure->freq = freq;

	return data;
}

void
stream_mod_init_node_debug_print(struct stg_module *mod)
{
}

void
stream_mod_register_node_debug_print(struct stg_native_module *mod)
{
	stg_native_register_funcs(mod, stream_funct_debug_print,
			STG_NATIVE_FUNC_HEAP);
}
