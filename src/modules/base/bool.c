#include "mod.h"

struct type_base base_bool_base = {
	.name = STR("bool"),
};

void base_register_bool(struct stg_module *mod)
{
	mod->vm->default_types.boolean =
		stg_register_builtin_type(mod, &base_bool_base,
								  STG_TYPE_DATA(STG_BOOL));
}
