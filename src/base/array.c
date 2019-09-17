#include "mod.h"
#include "../module.h"
#include "../utils.h"
#include "../ast.h"
#include <stdlib.h>
#include <string.h>

void
base_bootstrap_register_array(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_object_def *array_type_def =
		ast_object_def_register(&mod->store);

	struct ast_object_def_param array_type_params[] = {
		{0, ctx->atoms.array_cons_arg_type, AST_SLOT_TYPE},
		{1, ctx->atoms.array_cons_arg_count,
			ast_bind_slot_const_type(
					ctx, &array_type_def->env, AST_BIND_NEW,
					NULL, ctx->types.integer)},
	};

	ast_object_def_finalize(array_type_def,
			array_type_params, ARRAY_LENGTH(array_type_params),
			AST_SLOT_TYPE);

	mod->vm->default_cons.array = array_type_def;
	ctx->cons.array = array_type_def;
}

static struct type_base array_type_base;

struct object base_array_unpack(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_array_def *def, size_t member_id, struct object obj)
{
	struct type *obj_type = vm_get_type(ctx->vm, obj.type);
	assert(&array_type_base == obj_type->base);
	struct stg_array_type *array_info;
	array_info = (struct stg_array_type *)obj_type->data;

	if (member_id >= array_info->length) {
		printf("Attempted to unpack member %zu while the array has length %zu\n",
				member_id, array_info->length);
		return OBJ_NONE;
	}

	struct type *member_type;
	member_type = vm_get_type(ctx->vm, array_info->member_type);

	struct object member = {0};
	member.type = array_info->member_type;
	member.data = (void *)((uint8_t *)obj.data +
		(member_id * member_type->size));

	return member;
}

struct object base_array_pack(
		struct ast_context *ctx, struct ast_module *mod, struct ast_env *env,
		struct ast_array_def *def, ast_slot_id array_slot_id)
{
	struct ast_env_slot array_slot;
	array_slot = ast_env_slot(ctx, env, array_slot_id);

	struct object array_type_obj;
	int err;

	err = ast_slot_pack(ctx, mod, env,
			array_slot.type, &array_type_obj);
	if (err) {
		printf("Failed to pack array type.\n");
		return OBJ_NONE;
	}

	assert_type_equals(ctx->vm, array_type_obj.type, ctx->types.type);
	type_id array_type_id;
	array_type_id = *(type_id *)array_type_obj.type;

	struct type *array_type;
	struct stg_array_type *array_info;
	array_type = vm_get_type(ctx->vm, array_type_id);
	array_info = (struct stg_array_type *)array_type->data;

	assert(array_info->length == array_slot.cons_array.num_members);

	struct type *member_type;
	member_type = vm_get_type(ctx->vm, array_info->member_type);

	uint8_t array_data[array_info->length * member_type->size];

	for (size_t i = 0; i < array_info->length; i++) {
		struct object member;
		// TODO: Avoid this allocation!
		err = ast_slot_pack(ctx, mod, env,
				array_slot.cons_array.members[i], &member);
		if (err) {
			printf("Failed to pack member %zu\n", i);
			return OBJ_NONE;
		}

		assert_type_equals(ctx->vm, member.type, array_info->member_type);

		memcpy(&array_data[i * member_type->size],
				member.data, member_type->size);
	}

	struct object result = {0};
	result.type = array_type_id;
	result.data = array_data;

	return register_object(ctx->vm, env->store, result);
}

static struct ast_array_def array_type_def = {
	.pack = base_array_pack,
	.unpack = base_array_unpack,
};

bool
base_array_type_equals(struct vm *vm, struct type *lhs, struct type *rhs)
{
	struct stg_array_type *lhs_info;
	struct stg_array_type *rhs_info;

	lhs_info = (struct stg_array_type *)lhs->data;
	rhs_info = (struct stg_array_type *)rhs->data;

	return (
		lhs_info->length == rhs_info->length &&
		type_equals(vm, lhs_info->member_type, rhs_info->member_type)
	);
}

static struct type_base array_type_base = {
	.name = STR("array"),
	.array_def = &array_type_def,
	.equals = &base_array_type_equals,
	// TODO: type and object repr
};

type_id
stg_register_array_type(struct stg_module *mod, type_id member_type, size_t length)
{
	struct stg_array_type *data;

	data = calloc(1, sizeof(struct stg_array_type));

	data->member_type = member_type;
	data->length = length;

	struct type *member_type_inst = vm_get_type(mod->vm, member_type);

	struct type type = {0};
	type.base = &array_type_base;
	type.data = data;
	type.size = member_type_inst->size * data->length;

	return stg_register_type(mod, type);
}
