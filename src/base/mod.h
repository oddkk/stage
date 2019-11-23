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
base_bootstrap_register_array(struct ast_context *, struct stg_module *mod);

void
base_bootstrap_register_func(struct ast_context *, struct stg_module *mod);

void
base_integer_register_native(struct stg_native_module *mod);

void
base_init_register_cons(struct ast_context *ctx, struct stg_module *mod);

type_id
stg_register_array_type(struct stg_module *, type_id member_type, size_t length);

type_id
stg_register_func_type(struct stg_module *, type_id return_type,
		type_id *param_types, size_t num_params);

bool
stg_type_is_func(struct vm *, type_id);

void *
stg_func_ffi_cif(struct vm *, type_id, bool closure);

struct object
stg_register_func_object(
		struct vm *, struct objstore *,
		func_id func, void *data);

struct stg_func_type {
	type_id return_type;
	type_id *params;
	size_t num_params;
	type_id params_type;
	void *ffi_cif;
	void *ffi_cif_closure;
};

struct stg_array_type {
	type_id member_type;
	int64_t length;
};

#endif
