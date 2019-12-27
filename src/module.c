#include "module.h"
#include "str.h"
#include "base/mod.h"
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
