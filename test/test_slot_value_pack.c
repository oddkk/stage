#include "stage_test.h"
#include "mock_cons.h"

int
test_value_pack(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_env _env = {0};
	struct ast_env *env = &_env;

	env->store = &mod->store;

	struct object_cons *mock_cons;
	mock_cons = create_mock_cons(ctx);

	struct atom *param_name;
	param_name = vm_atoms(ctx->vm, MOCK_CONS_PARAM_NAME);

	ast_slot_id obj_slot, cons_slot, param_slot;

	obj_slot   = ast_slot_alloc(env);
	cons_slot  = ast_slot_alloc(env);
	param_slot = ast_slot_alloc(env);

	int64_t int_obj_val = 1;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;

	struct object cons_obj = {0};
	cons_obj.type = ctx->types.cons;
	cons_obj.data = &mock_cons;

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			cons_slot, cons_obj);

	ast_slot_require_cons(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, cons_slot);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			param_slot, int_obj);

	ast_slot_require_param_named(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, param_name, param_slot);


	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(ast_slot_value_result(result[obj_slot].result) ==
			AST_SLOT_RES_VALUE_FOUND_OBJ);
	TEST_ASSERT(ast_slot_cons_result(result[obj_slot].result) ==
			AST_SLOT_RES_CONS_FOUND);
	TEST_ASSERT(result[obj_slot].value.obj.type == ctx->types.integer);
	TEST_ASSERT(result[obj_slot].cons == mock_cons);

	int64_t res_val = (*(int64_t *)result[obj_slot].value.obj.data);

	TEST_ASSERT(res_val == int_obj_val+2);

	return 0;
}

STG_TEST(test_value_pack)
