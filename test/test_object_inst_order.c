#include "stage_test.h"

static inline void
print_act(struct object_inst_action act)
{
	switch (act.op) {
		case OBJ_INST_EXPR:
			printf("expr %i", act.expr.id);
			break;

		case OBJ_INST_BIND:
			printf("bind %i <- %i[%i]",
					act.bind.member_id,
					act.bind.expr_id,
					act.bind.unpack_id);
			break;

		case OBJ_INST_PACK:
			printf("pack %i", act.pack.member_id);
			break;

		default:
			printf("invalid action");
			break;
	}
}

static inline bool
act_equals(struct object_inst_action lhs, struct object_inst_action rhs)
{
	if (lhs.op != rhs.op) {
		return false;
	}

	switch (lhs.op) {
		case OBJ_INST_EXPR:
			return
				lhs.expr.id == rhs.expr.id;

		case OBJ_INST_BIND:
			return
				lhs.bind.member_id == rhs.bind.member_id &&
				lhs.bind.expr_id   == rhs.bind.expr_id   &&
				lhs.bind.unpack_id == rhs.bind.unpack_id;

		case OBJ_INST_PACK:
			return
				lhs.pack.member_id == rhs.pack.member_id;

		default:
			return false;
	}
}

static inline int
expect_act_expr(struct object_inst_action act, int expr_id,
		const char *file, int line)
{
	if (act.op != OBJ_INST_EXPR || act.expr.id != expr_id) {
		struct object_inst_action exp_act = {0};
		exp_act.op = OBJ_INST_EXPR;
		exp_act.expr.id = expr_id;

		printf("%s:%i Expected action '", file, line);
		print_act(exp_act);
		printf("', got '");
		print_act(act);
		printf("'\n");

		return 1;
	}

	return 0;
}

static inline int
expect_act_bind(struct object_inst_action act, int mbr_id, int expr_id, int unpack_id,
		const char *file, int line)
{
	if (act.op != OBJ_INST_BIND ||
			act.bind.member_id != mbr_id ||
			act.bind.expr_id != expr_id ||
			act.bind.unpack_id != unpack_id) {
		struct object_inst_action exp_act = {0};
		exp_act.op = OBJ_INST_BIND;
		exp_act.bind.member_id = mbr_id;
		exp_act.bind.expr_id = expr_id;
		exp_act.bind.unpack_id = unpack_id;

		printf("%s:%i Expected action '", file, line);
		print_act(exp_act);
		printf("', got '");
		print_act(act);
		printf("'\n");

		return 1;
	}

	return 0;
}

static inline int
expect_act_pack(struct object_inst_action act, int mbr_id,
		const char *file, int line)
{
	if (act.op != OBJ_INST_PACK || act.pack.member_id != mbr_id) {
		struct object_inst_action exp_act = {0};
		exp_act.op = OBJ_INST_PACK;
		exp_act.pack.member_id = mbr_id;

		printf("%s:%i Expected action '", file, line);
		print_act(exp_act);
		printf("', got '");
		print_act(act);
		printf("'\n");

		return 1;
	}

	return 0;
}

int
expect_act_any_order(
		struct object_inst_action *exp,
		struct object_inst_action *got,
		size_t num_acts,
		const char *file, int line)
{
	bool exp_found[num_acts];
	bool got_used[num_acts];
	size_t num_found = 0;

	for (size_t i = 0; i < num_acts; i++) {
		exp_found[i] = false;
		got_used[i] = false;
	}

	for (size_t exp_i = 0; exp_i < num_acts; exp_i++) {
		bool found = false;
		for (size_t got_i = 0; got_i < num_acts; got_i++) {
			if (act_equals(exp[exp_i], got[got_i]) && !got_used[got_i]) {
				got_used[got_i] = true;
				found = true;
				break;
			}
		}

		exp_found[exp_i] = found;
		if (found) {
			num_found += 1;
		}
	}

	assert(num_found <= num_acts);

	if (num_found != num_acts) {
		printf("%s:%i Expected in any order:\n", file, line);
		for (size_t exp_i = 0; exp_i < num_acts; exp_i++) {
			printf("  - '");
			print_act(exp[exp_i]);
			printf("'%s\n",
					exp_found[exp_i] ? " found" : "");
		}

		printf("Got:\n");
		for (size_t got_i = 0; got_i < num_acts; got_i++) {
			printf("  - '");
			print_act(got[got_i]);
			printf("'%s\n",
					got_used[got_i] ? " found" : "");
		}
	}

	return num_acts - num_found;
}

#define expect_act_expr(...) expect_act_expr(__VA_ARGS__, __FILE__, __LINE__)
#define expect_act_bind(...) expect_act_bind(__VA_ARGS__, __FILE__, __LINE__)
#define expect_act_pack(...) expect_act_pack(__VA_ARGS__, __FILE__, __LINE__)

int
test_object_inst_order(
		struct vm *vm,
		struct stg_module *mod)
{
	type_id integer = vm->default_types.integer;

	// { a := { b := 2; c := b; } d := a; }
	//  {a{b c}d{b c}}
	// 0 1 2 3 4 5 6

	struct object_cons mbr_a_cons = {0};
	struct object_cons_param mbr_a_cons_params[] = {
		{vm_atoms(vm, "b"), integer},
		{vm_atoms(vm, "c"), integer},
	};
	mbr_a_cons.params = mbr_a_cons_params;
	mbr_a_cons.num_params = ARRAY_LENGTH(mbr_a_cons_params);

	struct type_base mbr_a_type_base = {
		.name = STR("TestCompositeType"),
	};

	struct type mbr_a_type_def = {0};
	mbr_a_type_def.base = &mbr_a_type_base;
	mbr_a_type_def.obj_def = &mbr_a_cons;
	// Omit the remaining fields as they are not needed for this test.

	type_id mbr_a_type;
	mbr_a_type = stg_register_type(mod, mbr_a_type_def);

	struct object_inst inst = {0};
	struct object_cons inst_cons = {0};

	struct object_cons_param inst_cons_params[] = {
		{vm_atoms(vm, "a"), mbr_a_type},
		{vm_atoms(vm, "d"), mbr_a_type},
	};
	inst_cons.params = inst_cons_params;
	inst_cons.num_params = ARRAY_LENGTH(inst_cons_params);

	inst.cons = &inst_cons;

	int64_t int_2_value = 2;
	struct object int_2 = {0};
	int_2.data = &int_2_value;
	int_2.type = integer;

	size_t mbr_c_deps[] = {2};
	size_t mbr_d_deps[] = {1};

	struct func mbr_c_func_def = {0};
	mbr_c_func_def.type = stg_register_func_type(
			mod, integer, &integer, 1);
	func_id mbr_c_func = stg_register_func(
			mod, mbr_c_func_def);

	struct func mbr_d_func_def = {0};
	mbr_d_func_def.type = stg_register_func_type(
			mod, mbr_a_type, &mbr_a_type, 1);
	func_id mbr_d_func = stg_register_func(
			mod, mbr_d_func_def);


	struct object_inst_expr inst_exprs[] = {
		// b := 2;
		{.constant=true, .const_value=int_2},

		// c := b;
		{.constant=false, .func=mbr_c_func, .deps=mbr_c_deps, .num_deps=ARRAY_LENGTH(mbr_c_deps)},

		// d := a;
		{.constant=false, .func=mbr_d_func, .deps=mbr_d_deps, .num_deps=ARRAY_LENGTH(mbr_d_deps)},
	};

	inst.exprs = inst_exprs;
	inst.num_exprs = ARRAY_LENGTH(inst_exprs);

	struct object_inst_bind inst_binds[] = {
		{.target_id=2, .expr_id=0, .unpack_id=0},
		{.target_id=3, .expr_id=1, .unpack_id=0},
		{.target_id=4, .expr_id=2, .unpack_id=0},
	};

	inst.binds = inst_binds;
	inst.num_binds = ARRAY_LENGTH(inst_binds);

	struct type_base top_level_type_base = {
		.name = STR("TestTopLevelCompositeType"),
	};

	struct type top_level_type = {0};
	top_level_type.base = &top_level_type_base;
	top_level_type.obj_def = &inst_cons;

	inst.type = stg_register_type(
			mod, top_level_type);

	struct object_inst_action *actions;
	size_t num_actions;

	int err;
	err = object_inst_order(
			vm, NULL, &inst,
			NULL, 0, NULL, 0,
			&actions, &num_actions,
			STG_NO_LOC);

	TEST_ASSERT(!err);

	TEST_ASSERT(num_actions == 10);

	size_t next_action = 0;

	int res = 0;

#define NEXT_ACT (actions[next_action++])

	res += expect_act_expr(NEXT_ACT, 0);
	res += expect_act_bind(NEXT_ACT, 2, 0, 0);
	res += expect_act_expr(NEXT_ACT, 1);
	res += expect_act_bind(NEXT_ACT, 3, 1, 0);
	res += expect_act_pack(NEXT_ACT, 1);
	res += expect_act_expr(NEXT_ACT, 2);

	struct object_inst_action pack_d_acts_got[2];
	pack_d_acts_got[0] = NEXT_ACT;
	pack_d_acts_got[1] = NEXT_ACT;

	struct object_inst_action pack_d_acts_exp[2] = {
		{.op=OBJ_INST_BIND, .bind={.member_id=5, .expr_id=2, .unpack_id=1}},
		{.op=OBJ_INST_BIND, .bind={.member_id=6, .expr_id=2, .unpack_id=2}},
	};
	res += expect_act_any_order(
			pack_d_acts_exp, pack_d_acts_got, 2,
			__FILE__, __LINE__);

	res += expect_act_pack(NEXT_ACT, 4);
	res += expect_act_pack(NEXT_ACT, 0);

#undef NEXT_ACT

	return res;
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

	res += test_object_inst_order(&vm, mod);

	return res;
}
