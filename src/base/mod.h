#ifndef STAGE_BASE_MOD_H
#define STAGE_BASE_MOD_H

#include "../vm.h"

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
base_integer_register_native(struct stg_native_module *mod);

void
base_init_register_cons(struct ast_context *ctx, struct stg_module *mod);

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

struct stg_func_closure_member {
	type_id type;
	size_t  offset;
	size_t  size;
};

struct stg_func_closure_data {
	func_id func;
	struct stg_func_closure_member *members;
	size_t num_members;
	size_t size;
};

void
stg_func_closure_pack(struct vm *, void *data, void *out,
		void **params, size_t num_params);

struct stg_func_type {
	type_id return_type;
	type_id *params;
	size_t num_params;
	void *ffi_cif;
	void *ffi_cif_closure;
	void *ffi_cif_heap;
	void *ffi_cif_heap_closure;
};

#endif
