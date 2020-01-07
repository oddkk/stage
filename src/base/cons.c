#include "mod.h"
#include "../module.h"
#include "../ast.h"
#include <ffi.h>

struct type_base base_cons_base = {
	.name = STR("cons"),
};

void
base_bootstrap_register_cons(struct stg_module *mod)
{
	struct type t;
	t = init_plain_type(
			&base_cons_base,
			mod_atoms(mod, "cons"),
			struct object_cons *);
	// This type should not be used in runtime expressions.
	// t.ffi_type = &ffi_type_pointer;

	type_id tid;
	tid = stg_register_type(mod, t);

	mod->vm->default_types.cons = tid;
}

void
base_init_register_cons(struct ast_context *ctx, struct stg_module *mod)
{
}
