#include "mod.h"
#include "../ast.h"
#include "../module.h"
#include "../native.h"
#include "../utils.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

struct init_print_int_data {
	int64_t value;
};

static void
init_print_int_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct init_print_int_data *closure;
	closure = data;

	printf(" = %zi\n", closure->value);
}

static struct stg_init_data
init_monad_print_int(struct stg_exec *heap, int64_t val)
{
	struct stg_init_data data = {0};
	data.call = init_print_int_unsafe;
	data.data_size = sizeof(struct init_print_int_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct init_print_int_data *closure;
	closure = data.data;
	closure->value = val;

	return data;
}

struct init_print_str_data {
	struct string value;
};

static void
init_print_str_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct init_print_str_data *closure;
	closure = data;

	printf("%.*s\n", LIT(closure->value));
}

static struct stg_init_data
init_monad_print_str(struct stg_exec *heap, struct string val)
{
	struct stg_init_data data = {0};
	data.call = init_print_str_unsafe;
	data.data_size = sizeof(struct init_print_str_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct init_print_str_data *closure;
	closure = data.data;
	closure->value = val;

	return data;
}

struct init_io_data {
	struct stg_io_data monad;

	type_id data_type;
};

static void
init_io_copy(struct stg_exec *ctx, void *data)
{
	struct init_io_data *closure = data;
	stg_monad_io_copy(ctx, &closure->monad);
}

static void
init_io_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct init_io_data *closure = data;
	closure->monad.call(vm, ctx,
			closure->monad.data, out);
}

static struct stg_init_data
init_monad_io(struct stg_exec *heap,
		struct stg_module *mod,
		struct stg_io_data monad,
		type_id data_type_id)
{
	struct stg_init_data data = {0};
	data.call = init_io_unsafe;
	data.copy = init_io_copy;
	data.data_size = sizeof(struct init_io_data);
	data.data = stg_alloc(heap, 1, data.data_size);

	struct init_io_data *closure = data.data;
	closure->monad = monad;
	closure->data_type = data_type_id;

	return data;
}


static struct object_cons *
init_cons_from_vm(struct vm *vm)
{
	struct stg_module *base_mod;
	base_mod = vm_get_module(vm, vm_atoms(vm, "base"));
	assert(base_mod);

	struct stg_base_mod_info *mod_info;
	mod_info = base_mod->data;

	return mod_info->init_cons;
}

#define CNAME init
#define REAL_NAME "Init"
#define MONAD_DATA_TYPE struct stg_init_data
#define TYPE_INFO_TYPE struct stg_init_type_info

#include "../native_monad_template.h"

void
base_init_register_init(struct stg_module *mod)
{
	struct stg_base_mod_info *mod_info;
	mod_info = mod->data;

	mod_info->init_cons = init_register_cons(mod);

	stg_mod_register_native_cons(mod,
			mod_atoms(mod, "Init"), mod_info->init_cons);
}

void
base_init_register_native(struct stg_native_module *mod)
{
	init_register_native(mod);

	stg_native_register_funcs(mod, init_monad_io,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);
	stg_native_register_funcs(mod, init_monad_print_int,
			STG_NATIVE_FUNC_HEAP);
	stg_native_register_funcs(mod, init_monad_print_str,
			STG_NATIVE_FUNC_HEAP);
}

void
stg_monad_init_copy(struct stg_exec *ctx, struct stg_init_data *data)
{
	init_monad_copy(ctx, data);
}

type_id
stg_register_init_type(struct stg_module *mod, type_id res_type)
{
	return init_register_type(mod, res_type);
}

void
stg_unsafe_call_init(
		struct vm *vm, struct stg_exec *ctx,
		struct object obj, struct object *out)
{
	init_monad_call(vm, ctx, obj, out);
}

bool
stg_type_is_init(struct vm *vm, type_id tid)
{
	return init_type_is_inst(vm, tid);
}

type_id
stg_init_get_return_type(struct vm *vm, type_id tid)
{
	return init_return_type(vm, tid);
}
