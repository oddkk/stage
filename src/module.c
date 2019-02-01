#include "module.h"
#include "string.h"
#include "modules/base/mod.h"
#include <stdlib.h>

void
stg_register_builtin_func(struct stg_module *mod,
						  struct stg_builtin_func func,
						  void *data)
{
	struct atom **param_names;
	type_id *param_types;

	param_names = calloc(func.num_params, sizeof(struct atom *));
	param_types = calloc(func.num_params, sizeof(type_id));

	for (size_t i = 0; i < func.num_params; i++) {
		param_names[i] =
			atom_create(mod->atom_table,
						func.params[i].name);

		param_types[i] =
			stg_resolve_type(mod->vm,
							 func.params[i].type);
	}

	type_id ret_type =
			stg_resolve_type(mod->vm,
							 func.ret_type);


	struct object obj;
	obj = obj_register_builtin_func(mod->vm, &mod->store,
									param_names, param_types,
									func.num_params, ret_type, func.func,
									data);

	struct atom *func_name;
	func_name = atom_create(mod->atom_table, func.name);
	scope_insert(&mod->root_scope, func_name,
				 SCOPE_ANCHOR_ABSOLUTE,
				 obj, NULL);
}

type_id
stg_register_builtin_type(struct stg_module *mod,
						  struct type_base *base,
						  struct stg_builtin_type type)
{
	type_base_init_unfilled(base);

	assert(string_equal(mod->info.name, type.mod));

	struct type t = {0};
	t.name = atom_create(mod->atom_table, type.name);
	t.base = base;
	t.size = type.size;

	modtype_id local_tid;
	local_tid = register_type(&mod->store, t);

	type_id tid;
	tid = TYPE_ID(mod->id, local_tid);

	struct object obj;
	obj.type = mod->vm->default_types.type;
	obj.data = &tid;

	struct object new_obj;
	new_obj = register_object(mod->vm, &mod->store, obj);

	scope_insert(&mod->root_scope, t.name,
				 SCOPE_ANCHOR_ABSOLUTE,
				 new_obj, NULL);

	return tid;
}

type_id
stg_resolve_type(struct vm *vm, struct stg_builtin_type type)
{
	return vm_find_type_id(vm, type.mod, type.name);
}
