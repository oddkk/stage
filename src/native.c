#include "native.h"
#include "utils.h"
#include <stdlib.h>

void
stg_native_register_func(struct stg_native_module *mod,
		struct string name, void *func)
{
	struct stg_native_func *tmp_funcs;
	size_t tmp_num_funcs;
	size_t func_id;

	func_id = mod->num_funcs;

	tmp_num_funcs = mod->num_funcs + 1;
	tmp_funcs = realloc(mod->funcs,
			tmp_num_funcs * sizeof(struct stg_native_func));

	if (!tmp_funcs) {
		panic("Failed to realloc native funcs list.");
		return;
	}

	mod->num_funcs = tmp_num_funcs;
	mod->funcs = tmp_funcs;

	mod->funcs[func_id].name = name;
	mod->funcs[func_id].func = func;
}

void
stg_native_register_type(struct stg_native_module *mod,
		struct string name, struct type type)
{
	struct stg_native_type *tmp_types;
	size_t tmp_num_types;
	size_t type_id;

	type_id = mod->num_types;

	tmp_num_types = mod->num_types + 1;
	tmp_types = realloc(mod->types,
			tmp_num_types * sizeof(struct stg_native_type));

	if (!tmp_types) {
		panic("Failed to realloc native types list.");
		return;
	}

	mod->num_types = tmp_num_types;
	mod->types = tmp_types;

	mod->types[type_id].name = name;
	mod->types[type_id].type = type;
}
