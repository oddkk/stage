#include "mod.h"
#include "../ast.h"
#include "../module.h"
#include "../native.h"
#include "../utils.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

struct stg_init_type_info {
	type_id type;
};

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

static void
init_print_str_copy(struct stg_exec *heap, void *data)
{
	struct init_print_str_data *closure;
	closure = data;

	struct string *str = &closure->value;
	char *new_text = stg_alloc(heap, str->length+1, sizeof(char));
	memcpy(new_text, str->text, str->length);
	new_text[str->length] = '\0';
	str->text = new_text;
}

static struct stg_init_data
init_monad_print_str(struct stg_exec *heap, struct string val)
{
	struct stg_init_data data = {0};
	data.call = init_print_str_unsafe;
	data.copy = init_print_str_copy;
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

#define MONAD_CALLBACK_RET void
#define MONAD_CALLBACK_PARAMS struct vm *vm, struct stg_exec *ctx, void *data, void *out

#include "../native_monad_template.h"

static void
init_return_callback(struct vm *vm, struct stg_exec *ctx,
		void *data, void *out)
{
	struct init_return_data *closure = data;

	memcpy(out, closure->value, closure->size);
}

static void
init_bind_callback(struct vm *vm, struct stg_exec *ctx,
		void *data, void *out)
{
	struct init_bind_data *closure = data;

	uint8_t in_buffer[closure->in_type_size];

	closure->monad.call(vm, ctx,
			closure->monad.data, in_buffer);

	struct object arg;
	arg.type = closure->in_type;
	arg.data = in_buffer;


	MONAD_DATA_TYPE out_monad = {0};
	struct object out_monad_obj;
	out_monad_obj.type = closure->out_monad_type;
	out_monad_obj.data = &out_monad;

	vm_call_func_obj(
			vm, ctx, closure->func,
			&arg, 1, &out_monad_obj);

	out_monad.call(vm, ctx, out_monad.data, out);
}

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
	type_id ret_type_id;
	ret_type_id = init_return_type(vm, obj.type);

	struct type *ret_type;
	ret_type = vm_get_type(vm, ret_type_id);

	assert_type_equals(vm, out->type, ret_type_id);
	assert(out->data || ret_type->size == 0);

	struct stg_init_data *data = obj.data;

	data->call(vm, ctx, data->data, out->data);
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
