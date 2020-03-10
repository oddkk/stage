#include "stage_test.h"
#include "mock_cons.h"

int
test_cons_decay(struct ast_context *ctx, struct stg_module *mod)
{
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

	int64_t int_obj_val = 1;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;


	ast_slot_id control_cons_slot;
	control_cons_slot = ast_slot_alloc(env);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			control_cons_slot, cons_obj);

	ast_slot_id cons_slot;
	cons_slot         = ast_slot_alloc(env);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			cons_slot, cons_obj);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			cons_slot, int_obj);

	ast_slot_id cons_members_slot, cons_member_slot;
	cons_members_slot = ast_slot_alloc(env);
	cons_member_slot  = ast_slot_alloc(env);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			cons_members_slot, cons_obj);

	ast_slot_require_param_named(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			cons_members_slot, param_name, cons_member_slot);

	ast_slot_id typed_slot, typed_type_slot;
	typed_slot = ast_slot_alloc(env);
	typed_type_slot  = ast_slot_alloc(env);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			typed_slot, cons_obj);

	ast_slot_require_type(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			typed_slot, typed_type_slot);

	ast_slot_require_is_type(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			typed_type_slot, ctx->types.integer);

	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, mod, env, result);
	TEST_ASSERT(!err);


	// Check that the control did not decay as nothing conflicted with the
	// cons.
	TEST_ASSERT(ast_slot_value_result(result[control_cons_slot].result) ==
			AST_SLOT_RES_VALUE_FOUND_OBJ);
	TEST_ASSERT(result[control_cons_slot].value.obj.type ==
			ctx->types.cons);
	TEST_ASSERT(result[control_cons_slot].value.obj.data ==
			&mock_cons);


	// Check that the cons did decay as it was conflicting with the integer
	// value.
	TEST_ASSERT(ast_slot_value_result(result[cons_slot].result) ==
			AST_SLOT_RES_VALUE_FOUND_OBJ);
	TEST_ASSERT(result[cons_slot].type ==
			ctx->types.integer);
	TEST_ASSERT(ast_slot_cons_result(result[cons_slot].result) ==
			AST_SLOT_RES_CONS_FOUND);
	TEST_ASSERT(result[cons_slot].cons ==
			mock_cons);

	// Check that the cons with a member did decay.
	TEST_ASSERT(ast_slot_value_result(result[cons_members_slot].result) ==
			AST_SLOT_RES_TYPE_FOUND);
	TEST_ASSERT(result[cons_members_slot].type ==
			ctx->types.integer);
	TEST_ASSERT(ast_slot_cons_result(result[cons_members_slot].result) ==
			AST_SLOT_RES_CONS_FOUND);
	TEST_ASSERT(result[cons_members_slot].cons ==
			mock_cons);

	// Check that the cons with a type did decay.
	TEST_ASSERT(ast_slot_value_result(result[typed_slot].result) ==
			AST_SLOT_RES_TYPE_FOUND);
	TEST_ASSERT(result[typed_slot].type ==
			ctx->types.integer);
	TEST_ASSERT(ast_slot_cons_result(result[typed_slot].result) ==
			AST_SLOT_RES_CONS_FOUND);
	TEST_ASSERT(result[typed_slot].cons ==
			mock_cons);

	return 0;
}

STG_TEST(test_cons_decay)
