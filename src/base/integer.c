#include "mod.h"
#include "../module.h"
#include "../native.h"
#include <ffi.h>

static int64_t
op_add_int_int(int64_t lhs, int64_t rhs)
{
	return lhs + rhs;
}

static int64_t
op_sub_int_int(int64_t lhs, int64_t rhs)
{
	return lhs - rhs;
}

static int64_t
op_mul_int_int(int64_t lhs, int64_t rhs)
{
	return lhs * rhs;
}

static int64_t
op_div_int_int(int64_t lhs, int64_t rhs)
{
	return lhs / rhs;
}

static int
op_eq_int_int(int64_t lhs, int64_t rhs)
{
	return lhs == rhs;
}

static int
op_neq_int_int(int64_t lhs, int64_t rhs)
{
	return lhs != rhs;
}
static struct string
obj_integer_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	int64_t value = *(int64_t *)obj->data;
	return arena_sprintf(mem, "%li", value);
}

struct type_base base_integer_base = {
	.name = STR("int"),
	.obj_repr = obj_integer_repr,
};

void
base_bootstrap_register_integer(struct stg_module *mod)
{
	struct type t;
	t = init_plain_type(
			&base_integer_base,
			mod_atoms(mod, "int"),
			int64_t);
	t.ffi_type = &ffi_type_sint64;

	type_id tid;
	tid = stg_register_type(mod, t);

	mod->vm->default_types.integer = tid;
}

void
base_integer_register_native(struct stg_native_module *mod) {
	stg_native_register_funcs(mod, op_add_int_int, 0);
	stg_native_register_funcs(mod, op_sub_int_int, 0);
	stg_native_register_funcs(mod, op_mul_int_int, 0);
	stg_native_register_funcs(mod, op_div_int_int, 0);
	stg_native_register_funcs(mod, op_eq_int_int,  0);
	stg_native_register_funcs(mod, op_neq_int_int, 0);
}
