#include "mod.h"

#include "../../string.h"

struct type_base base_cons_base = {
	.name = STR("Cons"),
};

/*
type_id
base_register_cons(struct stg_module *mod, struct ast_object_def def,
		type_id type, cons_callback callback)
{
	stg_register_builtin_type(mod, &base_cons_base,
			STG_TYPE_DATA(STG_CONSTR));
}
*/
