#include "stage_test.h"
#include "mock_cons.h"

int
test_value_cons_propagate(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_env _env = {0};
	struct ast_env *env = &_env;

	env->store = &mod->store;

	struct object_cons *mock_cons;
	mock_cons = create_mock_cons(ctx);

	ast_slot_id obj_slot, cons_slot;

	obj_slot  = ast_slot_alloc(env);
	cons_slot = ast_slot_alloc(env);

	struct object cons_obj = {0};
	cons_obj.type = ctx->types.cons;
	cons_obj.data = &mock_cons;

	ast_slot_require_cons(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, cons_slot);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			cons_slot, cons_obj);


	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(ast_slot_cons_result(result[obj_slot].result) ==
			AST_SLOT_RES_CONS_FOUND);
	TEST_ASSERT(result[obj_slot].cons == mock_cons);

	return 0;
}

STG_TEST(test_value_cons_propagate)
