#include "module.h"
#include "str.h"
#include "base/mod.h"
#include "native.h"
#include <stdlib.h>
#include <string.h>

struct atom *
mod_atom(struct stg_module *mod, struct string name)
{
	return atom_create(&mod->vm->atom_table, name);
}

type_id
stg_register_type(struct stg_module *mod, struct type t)
{
	modtype_id local_tid;
	local_tid = store_register_type(&mod->store, t);

	type_id tid;
	tid = TYPE_ID(mod->id, local_tid);

	return tid;
}

func_id
stg_register_func(struct stg_module *mod, struct func f)
{
	modfunc_id local_fid;
	local_fid = store_register_func(&mod->store, f);

	func_id fid;
	fid = TYPE_ID(mod->id, local_fid);

	return fid;
}

struct stg_module *
stg_mod_find_module(struct stg_module *mod, struct atom *name)
{
	for (size_t i = 0; i < mod->num_dependencies; i++) {
		struct stg_module *dep;
		dep = vm_get_module_by_id(mod->vm, mod->dependencies[i]);
		if (dep->name == name) {
			return dep;
		}
	}

	return NULL;
}

int
stg_mod_invoke_pre_compile(struct ast_context *ctx, struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_pre_compile) {
		return mod->native_mod->hook_pre_compile(ctx, mod);
	}

	return 0;
}

int
stg_mod_invoke_pre_init(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_pre_init) {
		return mod->native_mod->hook_pre_init(mod);
	}

	return 0;
}

int
stg_mod_invoke_post_init(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_post_init) {
		return mod->native_mod->hook_post_init(mod);
	}

	return 0;
}

int
stg_mod_invoke_start(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_start) {
		return mod->native_mod->hook_start(mod);
	}

	return 0;
}

void
stg_mod_invoke_destroy(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_destroy) {
		mod->native_mod->hook_destroy(mod);
	}
}
