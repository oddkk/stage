#include "mod.h"
#include "../module.h"
#include "../native.h"
#include <ffi.h>

static float
op_add_float_float(float lhs, float rhs)
{
	return lhs + rhs;
}

static float
op_sub_float_float(float lhs, float rhs)
{
	return lhs - rhs;
}

static float
op_mul_float_float(float lhs, float rhs)
{
	return lhs * rhs;
}

static float
op_div_float_float(float lhs, float rhs)
{
	return lhs / rhs;
}

static float
op_eq_float_float(float lhs, float rhs)
{
	return lhs == rhs;
}

static float
op_neq_float_float(float lhs, float rhs)
{
	return lhs != rhs;
}

static struct string
float_to_string(struct stg_exec *heap, float val)
{
	return stg_exec_sprintf(heap, "%f", val);
}

static struct string
obj_float_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	float value = *(float *)obj->data;
	return arena_sprintf(mem, "%f", value);
}

struct type_base base_float_base = {
	.name = STR("Float"),
	.obj_repr = obj_float_repr,
};

void
base_bootstrap_register_float(struct stg_module *mod)
{
	struct type t;
	t = init_plain_type(
			&base_float_base,
			mod_atoms(mod, "Float"),
			float);
	t.ffi_type = &ffi_type_float;

	type_id tid;
	tid = stg_register_type(mod, t);

	stg_mod_register_native_type(mod,
			mod_atoms(mod, "Float"),
			tid);
}

void
base_float_register_native(struct stg_native_module *mod)
{
	stg_native_register_funcs(mod, op_add_float_float, 0);
	stg_native_register_funcs(mod, op_sub_float_float, 0);
	stg_native_register_funcs(mod, op_mul_float_float, 0);
	stg_native_register_funcs(mod, op_div_float_float, 0);
	stg_native_register_funcs(mod, op_eq_float_float,  0);
	stg_native_register_funcs(mod, op_neq_float_float, 0);
	stg_native_register_funcs(mod, float_to_string, STG_NATIVE_FUNC_HEAP);
}
