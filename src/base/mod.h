#ifndef STAGE_BASE_MOD_H
#define STAGE_BASE_MOD_H

#include "../vm.h"
#include "../init_monad.h"

void
stg_base_load(struct vm *vm);

void
base_bootstrap_register_type(struct stg_module *mod);

void
base_bootstrap_register_cons(struct stg_module *mod);

void
base_bootstrap_register_integer(struct stg_module *mod);

void
base_bootstrap_register_string(struct stg_module *mod);

void
base_bootstrap_boolean_pre_compile(struct stg_module *);

void
base_integer_register_native(struct stg_native_module *mod);

void
base_boolean_register(struct stg_module *);

void
base_init_register_init(struct stg_module *mod);

void
base_init_register_native(struct stg_native_module *mod);

void
base_init_register_io(struct stg_module *mod);

void
base_io_register_native(struct stg_native_module *mod);

type_id
stg_register_func_type(struct stg_module *, type_id return_type,
		type_id *param_types, size_t num_params);

bool
stg_type_is_func(struct vm *, type_id);

void *
stg_func_ffi_cif(struct vm *, type_id, enum func_flags);

struct object
stg_register_func_object(
		struct vm *, struct objstore *,
		func_id func, void *data);

struct stg_base_mod_info {
	struct object_cons *init_cons;
	struct object_cons *io_cons;
	struct object_cons *list_cons;
};

struct stg_func_closure_member {
	type_id type;
	size_t  offset;
	size_t  size;
};

struct stg_func_closure_data {
	func_id func;
	struct stg_func_closure_member *members;
	size_t num_members;

	// The self closure always has offset 0.
	bool has_self;

	size_t size;
};

void
stg_func_closure_pack(struct vm *, struct stg_exec *,
		void *data, void *out, void **params, size_t num_params);

struct stg_func_type {
	type_id return_type;
	type_id *params;
	size_t num_params;
	void *ffi_cif;
	void *ffi_cif_closure;
	void *ffi_cif_heap;
	void *ffi_cif_heap_closure;
};

struct stg_io_type_info {
	type_id type;
};

typedef void (*stg_io_callback)(
		struct vm *vm, struct stg_exec *, void *data, void *out);

typedef void (*stg_io_copy)(
		struct stg_exec *, void *data);

struct stg_io_data {
	stg_io_callback call;
	stg_io_copy copy;
	void *data;
	size_t data_size;
};

type_id
stg_register_io_type(struct stg_module *mod, type_id res_type);

// This function requires out->type to be set to the proper return type of the
// monad (as returned by stg_io_get_return_type(obj.type)), and out->data to
// point to a buffer sufficiently large to store the resulting object.
void
stg_unsafe_call_io(
		struct vm *vm, struct stg_exec *ctx,
		struct object obj, struct object *out);

void
stg_monad_io_copy(struct stg_exec *new_ctx, struct stg_io_data *);

type_id
stg_io_get_return_type(struct vm *, type_id);

// Defined in list.c
void
stg_list_register(struct stg_module *mod);

void
stg_list_register_native(struct stg_native_module *mod);

#endif
