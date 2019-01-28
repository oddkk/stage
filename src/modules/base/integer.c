#include "mod.h"

BUILTIN_PURE(op+, obj_integer_add, STG_INT, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs + rhs;
}

BUILTIN_PURE(op-, obj_integer_sub, STG_INT, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs - rhs;
}

BUILTIN_PURE(op*, obj_integer_mul, STG_INT, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs * rhs;
}

BUILTIN_PURE(op/, obj_integer_div, STG_INT, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs / rhs;
}

BUILTIN_PURE(op==, obj_integer_eq, STG_BOOL, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs == rhs;
}

BUILTIN_PURE(op!=, obj_integer_neq, STG_BOOL, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs != rhs;
}

BUILTIN_PURE(op>, obj_integer_gt, STG_BOOL, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs > rhs;
}

BUILTIN_PURE(op<, obj_integer_lt, STG_BOOL, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs < rhs;
}

BUILTIN_PURE(op>=, obj_integer_gte, STG_BOOL, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs >= rhs;
}

BUILTIN_PURE(op<=, obj_integer_lte, STG_BOOL, (STG_INT, lhs), (STG_INT, rhs))
{
	return lhs <= rhs;
}

static struct string obj_integer_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	int64_t value = *(int64_t *)obj->data;
	return arena_sprintf(mem, "%li", value);
}

struct type_base base_integer_base = {
	.name = STR("int"),
	.obj_repr = obj_integer_repr,
};

struct object
obj_register_integer(struct vm *vm, struct objstore *store, int64_t value)
{
	struct object result = {0};

	result.type = vm->default_types.integer;
	result.data = &value;

	return register_object(vm, store, result);
}

void base_register_integer(struct stg_module *mod)
{
	mod->vm->default_types.integer =
		stg_register_builtin_type(mod, &base_integer_base,
								  STG_TYPE_DATA(STG_INT));

	stg_register_builtin_func(mod, obj_integer_add);
	stg_register_builtin_func(mod, obj_integer_sub);
	stg_register_builtin_func(mod, obj_integer_mul);
	stg_register_builtin_func(mod, obj_integer_div);
	stg_register_builtin_func(mod, obj_integer_eq);
	stg_register_builtin_func(mod, obj_integer_neq);
	stg_register_builtin_func(mod, obj_integer_gt);
	stg_register_builtin_func(mod, obj_integer_lt);
	stg_register_builtin_func(mod, obj_integer_gte);
	stg_register_builtin_func(mod, obj_integer_lte);
}
