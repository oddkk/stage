#ifndef STG_INIT_MONAD_H
#define STG_INIT_MONAD_H

#include "vm.h"
#include "atom.h"

struct stg_init_context_entry {
	struct atom *name;
	void *data;
};

struct stg_init_context {
	struct vm *vm;
	struct stg_module *mod;

	struct stg_init_context_entry *entries;
	size_t num_entries;
};

void
stg_init_register_entry(struct stg_init_context *ctx,
		struct atom *name, void *data);

void *
stg_init_get_entry(struct stg_init_context *ctx, struct atom *name);

typedef void (*stg_init_callback)(
		struct stg_init_context *, struct stg_exec *, void *data, void *out);

typedef void (*stg_init_copy)(
		struct stg_exec *, void *data);

struct stg_init_data {
	stg_init_callback call;
	stg_init_copy copy;
	void *data;
	size_t data_size;
};

type_id
stg_register_init_type(struct stg_module *mod, type_id res_type);

// This function requires out->type to be set to the proper return type of the
// monad (as returned by stg_init_get_return_type(obj.type)), and out->data to
// point to a buffer sufficiently large to store the resulting object.
void
stg_unsafe_call_init(
		struct stg_init_context *, struct stg_exec *,
		struct object, struct object *out);

void
stg_monad_init_copy(struct stg_exec *new_ctx, struct stg_init_data *);

bool
stg_type_is_init(struct vm *, type_id);

type_id
stg_init_get_return_type(struct vm *, type_id);

#endif
