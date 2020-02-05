#include "stage_test.h"

STG_TEST(test_value_mismatch)

int
test_value_mismatch(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_env _env = {0};
	struct ast_env *env = &_env;

	env->store = &mod->store;

	ast_slot_id obj_slot;

	obj_slot = ast_slot_alloc(env);

	int64_t int_obj_1_val = 1;
	struct object int_obj_1 = {0};
	int_obj_1.type = ctx->types.integer;
	int_obj_1.data = &int_obj_1_val;

	int64_t int_obj_2_val = 2;
	struct object int_obj_2 = {0};
	int_obj_2.type = ctx->types.integer;
	int_obj_2.data = &int_obj_2_val;

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, int_obj_1);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, int_obj_2);


	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
	TEST_ASSERT(err);

	TEST_ASSERT(result[obj_slot].result == AST_SLOT_RES_ERROR);

	return 0;
}
