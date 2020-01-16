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
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
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

static void
test_cons_pack(struct vm *vm, void *data,
		void *out, void **params, size_t num_params)
{
	assert(num_params == 1);
	int64_t param_val = *(int64_t *)params[0];

	*(int64_t *)out = param_val + 2;
}

static type_id
test_cons_pack_type(struct vm *vm, void *data,
		void **params, size_t num_params)
{
	return vm->default_types.integer;
}

static void
test_cons_unpack(struct vm *vm, void *data,
		void *out, void *obj, int param_id)
{
	assert(param_id == 0);
	int64_t obj_val = *(int64_t *)obj;

	*(int64_t *)out = obj_val - 2;
}

static void
test_cons_impose_constraints(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, struct ast_env *env,
		ast_slot_id ret_type_slot, ast_slot_id *param_slots)
{
	ast_slot_require_type(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			param_slots[0], ret_type_slot);
}

int
test_value_pack(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_env _env = {0};
	struct ast_env *env = &_env;

	env->store = &mod->store;

	struct atom *param_name;
	param_name = vm_atoms(ctx->vm, "a");

	struct object_cons test_cons = {0};
	struct object_cons_param test_cons_params[] = {
		{param_name, ctx->types.integer},
	};

	test_cons.params = test_cons_params;
	test_cons.num_params = sizeof(test_cons_params) / sizeof(struct object_cons_param);
	test_cons.pack = test_cons_pack;
	test_cons.pack_type = test_cons_pack_type;
	test_cons.unpack = test_cons_unpack;
	test_cons.impose_constraints =
		test_cons_impose_constraints;

	ast_slot_id obj_slot, cons_slot, param_slot;

	obj_slot   = ast_slot_alloc(env);
	cons_slot  = ast_slot_alloc(env);
	param_slot = ast_slot_alloc(env);

	int64_t int_obj_val = 1;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;

	struct object_cons *test_cons_ptr = &test_cons;
	struct object cons_obj = {0};
	cons_obj.type = ctx->types.cons;
	cons_obj.data = &test_cons_ptr;

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

	ast_slot_require_member_named(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, param_name, param_slot);


	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(result[obj_slot].result ==
			(AST_SLOT_RES_VALUE_FOUND_OBJ|AST_SLOT_RES_CONS_FOUND));
	TEST_ASSERT(result[obj_slot].value.obj.type == ctx->types.integer);
	TEST_ASSERT(result[obj_slot].cons == &test_cons);

	int64_t res_val = (*(int64_t *)result[obj_slot].value.obj.data);

	TEST_ASSERT(res_val == int_obj_val+2);

	return 0;
}

int
test_value_cons_propagate(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_env _env = {0};
	struct ast_env *env = &_env;

	env->store = &mod->store;

	struct atom *param_name;
	param_name = vm_atoms(ctx->vm, "a");

	struct object_cons test_cons = {0};
	struct object_cons_param test_cons_params[] = {
		{param_name, ctx->types.integer},
	};

	test_cons.params = test_cons_params;
	test_cons.num_params = sizeof(test_cons_params) / sizeof(struct object_cons_param);
	test_cons.pack = test_cons_pack;
	test_cons.pack_type = test_cons_pack_type;
	test_cons.unpack = test_cons_unpack;
	test_cons.impose_constraints =
		test_cons_impose_constraints;

	struct object_cons *test_cons_ptr = &test_cons;

	ast_slot_id obj_slot, cons_slot;

	obj_slot  = ast_slot_alloc(env);
	cons_slot = ast_slot_alloc(env);

	struct object cons_obj = {0};
	cons_obj.type = ctx->types.cons;
	cons_obj.data = &test_cons_ptr;

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
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(ast_slot_cons_result(result[obj_slot].result) ==
			AST_SLOT_RES_CONS_FOUND);
	TEST_ASSERT(result[obj_slot].cons == &test_cons);

	return 0;
}

int
test_value_unpack(struct ast_context *ctx, struct stg_module *mod)
{
	struct ast_env _env = {0};
	struct ast_env *env = &_env;

	env->store = &mod->store;

	struct atom *param_name;
	param_name = vm_atoms(ctx->vm, "a");

	struct object_cons test_cons = {0};
	struct object_cons_param test_cons_params[] = {
		{param_name, ctx->types.integer},
	};

	test_cons.params = test_cons_params;
	test_cons.num_params = sizeof(test_cons_params) / sizeof(struct object_cons_param);
	test_cons.pack = test_cons_pack;
	test_cons.unpack = test_cons_unpack;
	test_cons.impose_constraints =
		test_cons_impose_constraints;

	ast_slot_id obj_slot, cons_slot, param_slot;

	obj_slot   = ast_slot_alloc(env);
	cons_slot  = ast_slot_alloc(env);
	param_slot = ast_slot_alloc(env);

	int64_t int_obj_val = 3;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;

	struct object_cons *test_cons_ptr = &test_cons;
	struct object cons_obj = {0};
	cons_obj.type = ctx->types.cons;
	cons_obj.data = &test_cons_ptr;

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
			obj_slot, int_obj);

	ast_slot_require_member_named(
			env, STG_NO_LOC,
			AST_CONSTR_SRC_FUNC_DECL,
			obj_slot, param_name, param_slot);


	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(ast_slot_value_result(result[param_slot].result) ==
			AST_SLOT_RES_VALUE_FOUND_OBJ);
	TEST_ASSERT(result[param_slot].value.obj.type == ctx->types.integer);

	int64_t res_val = (*(int64_t *)result[param_slot].value.obj.data);

	TEST_ASSERT(res_val == int_obj_val-2);

	return 0;
}

int main()
{
	struct vm vm;
	struct stg_module *mod;
	struct ast_context ctx;

	int err;
	err = stg_test_bootstrap(&vm, &ctx, &mod);
	if (err) {
		return -1;
	}

	int res = 0;

	res += test_type_propagation(&ctx, mod);
	res += test_value_mismatch(&ctx, mod);
	res += test_value_pack(&ctx, mod);
	res += test_value_cons_propagate(&ctx, mod);
	res += test_value_unpack(&ctx, mod);

	return res;
}
