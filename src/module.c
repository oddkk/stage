#include "module.h"
#include "string.h"
#include "base/mod.h"
#include <stdlib.h>

struct atom *
mod_atom(struct stg_module *mod, struct string name)
{
	return atom_create(&mod->vm->atom_table, name);
}

type_id
stg_register_type(struct stg_module *mod, struct type t)
{
	modtype_id local_tid;
	local_tid = register_type(&mod->store, t);

	type_id tid;
	tid = TYPE_ID(mod->id, local_tid);

	return tid;
}
