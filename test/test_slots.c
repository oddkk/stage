#include "vm.h"
#include "ast.h"
#include "module.h"
#include "base/mod.h"
#include <string.h>
#include <signal.h>

#define TEST_ASSERT(expr)							\
	do {											\
		if (!(expr)) {								\
			fprintf(stderr,							\
					__FILE__ ":%i: "				\
					"Assertion '" #expr "' failed!\n",\
					__LINE__);						\
			return -1;								\
		}											\
	} while (0);

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
			obj_slot, int_obj);

	ast_slot_require_type(
			env, STG_NO_LOC,
			obj_slot, obj_type_slot);

	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(result[obj_slot].result == AST_SLOT_RESULT_FOUND_OBJ);
	TEST_ASSERT(result[obj_slot].obj.type == ctx->types.integer);
	TEST_ASSERT(*(int64_t *)result[obj_slot].obj.data == int_obj_val);

	TEST_ASSERT(result[obj_type_slot].result == AST_SLOT_RESULT_FOUND_TYPE);
	TEST_ASSERT(result[obj_type_slot].type == ctx->types.integer);

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
			obj_slot, int_obj_1);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			obj_slot, int_obj_2);


	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
	TEST_ASSERT(err);

	TEST_ASSERT(result[obj_slot].result == AST_SLOT_RESULT_ERROR);

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
test_cons_impose_constraints(struct vm *vm, void *data,
		struct ast_env *env, ast_slot_id ret_type_slot, ast_slot_id *param_slots)
{
	ast_slot_require_is_type(
			env, STG_NO_LOC, param_slots[0], ret_type_slot);
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
	test_cons.impose_type_constraints = test_cons_impose_constraints;

	ast_slot_id obj_slot, param_slot;

	obj_slot   = ast_slot_alloc(env);
	param_slot = ast_slot_alloc(env);

	int64_t int_obj_val = 1;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;

	ast_slot_require_cons(
			env, STG_NO_LOC,
			obj_slot, &test_cons);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			param_slot, int_obj);

	ast_slot_require_member_named(
			env, STG_NO_LOC,
			obj_slot, param_name, param_slot);


	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(result[obj_slot].result == AST_SLOT_RESULT_FOUND_OBJ);
	TEST_ASSERT(result[obj_slot].obj.type == ctx->types.integer);

	int64_t res_val = (*(int64_t *)result[obj_slot].obj.data);

	TEST_ASSERT(res_val == int_obj_val+2);

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
	test_cons.impose_type_constraints = test_cons_impose_constraints;

	ast_slot_id obj_slot, param_slot;

	obj_slot   = ast_slot_alloc(env);
	param_slot = ast_slot_alloc(env);

	int64_t int_obj_val = 3;
	struct object int_obj = {0};
	int_obj.type = ctx->types.integer;
	int_obj.data = &int_obj_val;

	ast_slot_require_cons(
			env, STG_NO_LOC,
			obj_slot, &test_cons);

	ast_slot_require_is_obj(
			env, STG_NO_LOC,
			obj_slot, int_obj);

	ast_slot_require_member_named(
			env, STG_NO_LOC,
			obj_slot, param_name, param_slot);


	struct ast_slot_result result[env->num_alloced_slots];
	int err;
	err = ast_slot_try_solve(ctx, &mod->mod, env, result);
	TEST_ASSERT(!err);

	TEST_ASSERT(result[param_slot].result == AST_SLOT_RESULT_FOUND_OBJ);
	TEST_ASSERT(result[param_slot].obj.type == ctx->types.integer);

	int64_t res_val = (*(int64_t *)result[param_slot].obj.data);

	TEST_ASSERT(res_val == int_obj_val-2);

	return 0;
}

int main()
{
	struct vm vm;
	int err;

	err = vm_init(&vm);
	if (err) {
		printf("Failed to initialize vm.\n");
		return -1;
	}

	stg_base_load(&vm);

	struct ast_context ctx;
	ctx = ast_init_context(NULL, &vm.atom_table, &vm);

	struct stg_module_info mod_info = {
		.name = STR("test"),
		.version = {.major = 0, .minor = 1},
	};

	struct stg_module *mod;
	mod = vm_register_module(&vm, &ctx, NULL, &mod_info);

	int res = 0;

	res += test_type_propagation(&ctx, mod);
	res += test_value_mismatch(&ctx, mod);
	res += test_value_pack(&ctx, mod);
	res += test_value_unpack(&ctx, mod);

	return res;
}
