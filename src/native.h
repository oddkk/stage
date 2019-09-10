#ifndef STAGE_NATIVE_H
#define STAGE_NATIVE_H

#include "string.h"
#include "objstore.h"

struct stg_native_func {
	struct string name;
	void *func;
};

struct stg_native_type {
	struct string name;
	struct type type;
};

struct atom;
struct stg_module;

struct stg_native_module {
	struct atom *name;

	struct stg_native_func *funcs;
	size_t num_funcs;

	struct stg_native_type *types;
	size_t num_types;

	int  (*hook_init)(struct stg_module *);
	void (*hook_free)(struct stg_module *);
};

void
stg_native_register_func(struct stg_native_module *,
		struct string name, void *func);
void
stg_native_register_type(struct stg_native_module *,
		struct string name, struct type);

#endif
