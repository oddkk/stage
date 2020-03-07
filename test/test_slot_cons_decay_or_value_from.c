#include "stage_test.h"
#include "mock_cons.h"

int
test_value_propagation(struct ast_context *ctx, struct stg_module *mod)
{
	printf("test value propagation\n");
	struct ast_env _env = {0};
	struct ast_env *env = &_env;
	env->store = &mod->store;

	int64_t int_obj_val = 1;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;

	ast_slot_id src_value_slot, target_value_slot;

	src_value_slot    = ast_slot_alloc(env);
	target_value_slot = ast_slot_alloc(env);

	ast_slot_require_is_obj(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			src_value_slot, int_obj);

	ast_slot_require_cons_or_value_from(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			target_value_slot, src_value_slot);

	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(ast_slot_value_result(result[target_value_slot].result) ==
			AST_SLOT_RES_VALUE_FOUND_OBJ);
	TEST_ASSERT(result[target_value_slot].type ==
			ctx->types.integer);
	TEST_ASSERT(*(int64_t *)result[target_value_slot].value.obj.data == int_obj_val);

	return 0;
}

int
test_cons_propagation(struct ast_context *ctx, struct stg_module *mod)
{
	printf("test cons propagation\n");
	struct ast_env _env = {0};
	struct ast_env *env = &_env;
	env->store = &mod->store;

	struct object_cons *mock_cons;
	mock_cons = create_mock_cons(ctx);

	struct atom *param_name;
	param_name = vm_atoms(ctx->vm, MOCK_CONS_PARAM_NAME);

	struct object cons_obj = {0};
	cons_obj.type = ctx->types.cons;
	cons_obj.data = &mock_cons;

	ast_slot_id src_cons_slot, target_value_slot;

	src_cons_slot     = ast_slot_alloc(env);
	target_value_slot = ast_slot_alloc(env);

	ast_slot_require_is_obj(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			src_cons_slot, cons_obj);

	ast_slot_require_cons_or_value_from(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			target_value_slot, src_cons_slot);

	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(ast_slot_value_result(result[target_value_slot].result) ==
			AST_SLOT_RES_TYPE_FOUND);

	TEST_ASSERT(ast_slot_cons_result(result[target_value_slot].result) ==
			AST_SLOT_RES_CONS_FOUND);
	TEST_ASSERT(result[target_value_slot].cons == mock_cons);

	return 0;
}

int
test_cons_propagation_with_value(struct ast_context *ctx, struct stg_module *mod)
{
	printf("test cons propagation with value\n");
	struct ast_env _env = {0};
	struct ast_env *env = &_env;
	env->store = &mod->store;

	struct object_cons *mock_cons;
	mock_cons = create_mock_cons(ctx);

	struct atom *param_name;
	param_name = vm_atoms(ctx->vm, MOCK_CONS_PARAM_NAME);

	struct object cons_obj = {0};
	cons_obj.type = ctx->types.cons;
	cons_obj.data = &mock_cons;

	int64_t int_obj_val = 3;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;


	ast_slot_id src_cons_slot, src_mbr_slot, target_value_slot;

	src_cons_slot     = ast_slot_alloc(env);
	src_mbr_slot      = ast_slot_alloc(env);
	target_value_slot = ast_slot_alloc(env);

	ast_slot_require_is_obj(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			src_cons_slot, cons_obj);

	ast_slot_require_member_named(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			target_value_slot, param_name, src_mbr_slot);

	ast_slot_require_is_obj(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			src_mbr_slot, int_obj);

	ast_slot_require_cons_or_value_from(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			target_value_slot, src_cons_slot);

	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(ast_slot_value_result(result[target_value_slot].result) ==
			AST_SLOT_RES_VALUE_FOUND_OBJ);
	TEST_ASSERT(result[target_value_slot].type == ctx->types.integer);
	TEST_ASSERT(*(int64_t *)result[target_value_slot].value.obj.data == int_obj_val+2);

	TEST_ASSERT(ast_slot_cons_result(result[target_value_slot].result) ==
			AST_SLOT_RES_CONS_FOUND);
	TEST_ASSERT(result[target_value_slot].cons == mock_cons);

	return 0;
}
int
test_slot_cons_decay_or_value_from(struct ast_context *ctx, struct stg_module *mod)
{
	int err = 0;

	err += test_value_propagation(ctx, mod);
	err += test_cons_propagation(ctx, mod);
	err += test_cons_propagation_with_value(ctx, mod);

	return err;
}

STG_TEST(test_slot_cons_decay_or_value_from)
