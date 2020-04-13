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
#define MONAD_CALLBACK_RET void
#define MONAD_CALLBACK_PARAMS struct vm *vm, struct stg_exec *ctx, void *data, void *out

#include "../native_monad_template.h"

static void
io_return_callback(struct vm *vm, struct stg_exec *ctx,
		void *data, void *out)
{
	struct io_return_data *closure = data;

	memcpy(out, closure->value, closure->size);
}

static void
io_bind_callback(struct vm *vm, struct stg_exec *ctx,
		void *data, void *out)
{
	struct io_bind_data *closure = data;

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

struct io_print_data {
	struct string value;
};

static void
io_print_unsafe(struct vm *vm, struct stg_exec *ctx, void *data, void *out)
{
	struct io_print_data *closure = data;

	printf("%.*s\n", LIT(closure->value));
}

static void
io_print_copy(struct stg_exec *heap, void *data)
{
	struct io_print_data *closure = data;

	struct string *str = &closure->value;
	char *new_text = stg_alloc(heap, str->length+1, sizeof(char));
	memcpy(new_text, str->text, str->length);
	new_text[str->length] = '\0';
	str->text = new_text;
}

static struct stg_io_data
io_monad_print(struct stg_exec *heap, struct stg_module *mod, struct string val)
{
	struct stg_io_data data = {0};
	data.call = io_print_unsafe;
	data.copy = io_print_copy;
	data.data_size = sizeof(struct io_print_data);
	data.data = stg_alloc(heap, 1, sizeof(struct io_print_data));

	struct io_print_data *closure;
	closure = data.data;
	closure->value = val;

	return data;
}

void
base_io_register_native(struct stg_native_module *mod)
{
	io_register_native(mod);

	stg_native_register_funcs(mod, io_monad_print,
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
	type_id ret_type_id;
	ret_type_id = io_return_type(vm, obj.type);

	struct type *ret_type;
	ret_type = vm_get_type(vm, ret_type_id);

	assert_type_equals(vm, out->type, ret_type_id);
	assert(out->data || ret_type->size == 0);

	struct stg_io_data *data = obj.data;

	data->call(vm, ctx, data->data, out->data);
}

void
stg_monad_io_copy(struct stg_exec *ctx, struct stg_io_data *data)
{
	io_monad_copy(ctx, data);
}
