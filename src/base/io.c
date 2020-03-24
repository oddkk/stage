#include "mod.h"
#include "../ast.h"
#include "../module.h"
#include "../native.h"
#include "../utils.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

static struct object_cons *
io_cons_from_vm(struct vm *vm)
{
	struct stg_module *base_mod;
	base_mod = vm_get_module(vm, vm_atoms(vm, "base"));
	assert(base_mod);

	struct stg_base_mod_info *mod_info;
	mod_info = base_mod->data;

	return mod_info->io_cons;
}

#define CNAME io
#define REAL_NAME "IO"
#define MONAD_DATA_TYPE struct stg_io_data
#define TYPE_INFO_TYPE struct stg_io_type_info

#include "../native_monad_template.h"

struct io_print_int_data {
	int64_t value;
};

static void
io_print_int_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct io_print_int_data *closure = data;

	printf(" => %zu\n", closure->value);
}

static struct stg_io_data
io_monad_print_int(struct stg_exec *heap, struct stg_module *mod, int64_t val)
{
	struct stg_io_data data = {0};
	data.call = io_print_int_unsafe;
	data.data_size = sizeof(struct io_print_int_data);
	data.data = stg_alloc(heap, 1, sizeof(struct io_print_int_data));

	struct io_print_int_data *closure;
	closure = data.data;
	closure->value = val;

	return data;
}

void
base_io_register_native(struct stg_native_module *mod)
{
	io_register_native(mod);

	stg_native_register_funcs(mod, io_monad_print_int,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
}

void
base_init_register_io(struct stg_module *mod)
{
	struct stg_base_mod_info *mod_info;
	mod_info = mod->data;

	mod_info->io_cons = io_register_cons(mod);

	stg_mod_register_native_cons(mod,
			mod_atoms(mod, "IO"), mod_info->io_cons);
}


type_id
stg_io_get_return_type(struct vm *vm, type_id tid)
{
	return io_return_type(vm, tid);
}

bool
stg_io_type_is_inst(struct vm *vm, type_id tid)
{
	return io_type_is_inst(vm, tid);
}

void
stg_unsafe_call_io(
		struct vm *vm, struct stg_exec *ctx,
		struct object obj, struct object *out)
{
	io_monad_call(vm, ctx, obj, out);
}

void
stg_monad_io_copy(struct stg_exec *ctx, struct stg_io_data *data)
{
	io_monad_copy(ctx, data);
}
