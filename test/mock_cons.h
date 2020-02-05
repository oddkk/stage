#ifndef STG_TEST_MOCK_CONS
#define STG_TEST_MOCK_CONS

#include <stdlib.h>

#define MOCK_CONS_PARAM_NAME "a"

static void
mock_cons_pack(struct vm *vm, void *data,
		void *out, void **params, size_t num_params)
{
	assert(num_params == 1);
	int64_t param_val = *(int64_t *)params[0];

	*(int64_t *)out = param_val + 2;
}

static type_id
mock_cons_pack_type(struct vm *vm, void *data,
		void **params, size_t num_params)
{
	return vm->default_types.integer;
}

static void
mock_cons_unpack(struct vm *vm, void *data,
		void *out, void *obj, int param_id)
{
	assert(param_id == 0);
	int64_t obj_val = *(int64_t *)obj;

	*(int64_t *)out = obj_val - 2;
}

static void
mock_cons_impose_constraints(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, struct ast_env *env,
		ast_slot_id ret_type_slot, ast_slot_id *param_slots)
{
	ast_slot_require_type(
			env, STG_NO_LOC, AST_CONSTR_SRC_FUNC_DECL,
			param_slots[0], ret_type_slot);
}

static struct object_cons *
create_mock_cons(struct ast_context *ctx) {
	struct atom *param_name;
	param_name = vm_atoms(ctx->vm, MOCK_CONS_PARAM_NAME);

	struct object_cons *mock_cons;
	mock_cons = calloc(1, sizeof(struct object_cons));

	mock_cons->num_params = 1;
	mock_cons->params = calloc(
		mock_cons->num_params,
		sizeof(struct object_cons_param));

	mock_cons->params[0].name = param_name;
	mock_cons->params[0].type = ctx->types.integer;

	mock_cons->pack = mock_cons_pack;
	mock_cons->pack_type = mock_cons_pack_type;
	mock_cons->unpack = mock_cons_unpack;
	mock_cons->impose_constraints =
		mock_cons_impose_constraints;

	return mock_cons;
}

#endif
