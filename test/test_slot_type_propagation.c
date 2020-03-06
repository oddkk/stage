#include "stage_test.h"

int
test_type_propagation(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_env _env = {0};
	struct ast_env *env = &_env;

	env->store = &mod->store;

	ast_slot_id obj_slot, obj_type_slot;

	obj_slot      = ast_slot_alloc(env);
	obj_type_slot = ast_slot_alloc(env);

	int64_t int_obj_val = 2;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, int_obj);

	ast_slot_require_type(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, obj_type_slot);

	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(ast_slot_value_result(result[obj_slot].result) ==
			AST_SLOT_RES_VALUE_FOUND_OBJ);
	TEST_ASSERT(result[obj_slot].value.obj.type == ctx->types.integer);
	TEST_ASSERT(*(int64_t *)result[obj_slot].value.obj.data == int_obj_val);
	TEST_ASSERT(result[obj_slot].type == ctx->types.integer);

	TEST_ASSERT(ast_slot_value_result(result[obj_type_slot].result) ==
			AST_SLOT_RES_VALUE_FOUND_TYPE);
	TEST_ASSERT(result[obj_type_slot].value.type == ctx->types.integer);
	TEST_ASSERT(result[obj_type_slot].type == ctx->types.type);

	return 0;
}

STG_TEST(test_type_propagation)
