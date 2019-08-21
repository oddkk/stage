#include "utils.h"
#include "vm.h"
#include "compile.h"
#include "ast.h"

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

/* enable clock_* functions */
#ifndef __USE_POSIX199309
#define __USE_POSIX199309
#endif

#ifndef __USE_XOPEN2K
#define __USE_XOPEN2K
#endif

#include <time.h>
#include <errno.h>

#ifdef STAGE_TEST
#define main _main
#endif

#define NSEC (1000000000)

static bool should_quit = false;

void stage_signal_handler(int sig)
{
	if (sig == SIGINT) {
		should_quit = true;
	}
}

static struct timespec timespec_add(struct timespec begin, struct timespec end)
{
	struct timespec temp;
	temp.tv_sec = begin.tv_sec + end.tv_sec;
	temp.tv_nsec = begin.tv_nsec + end.tv_nsec;
	temp.tv_sec += temp.tv_nsec / NSEC;
	temp.tv_nsec %= NSEC;
	return temp;
}

static struct timespec read_time()
{
	struct timespec time;
	int error;

	error = clock_gettime(CLOCK_MONOTONIC, &time);

	/*
	   NOTE: We should already have checked that we support
	   CLOCK_MONOTONIC so we should not receive EINVAL error. It
	   can return EFAULT if the second param is outside the memory we
	   can access. This would be a programming error.
	 */
	assert(!error);

	return time;
}

static bool check_clock_support()
{
	struct timespec time;
	int error;
	error = clock_gettime(CLOCK_MONOTONIC, &time);

	if (error == EINVAL) {
		print_error("clock",
			    "CLOCK_MONOTONIC_RAW is not supported on this system.\n");
		return false;
	}
	/*
	   NOTE: clock_gettime can return EFAULT if the second param is
	   outside the memory we can access. If that is the case, it is a
	   programming error.
	 */
	assert(!error);

	return true;
}

extern struct stg_module_info mod_base;
extern struct stg_module_info mod_channel;

struct object
obj_register_integer(struct vm *, struct objstore *,
		int64_t value);

int main(int argc, char *argv[])
{
	int err;
	struct timespec tick_begin;
	struct timespec frame_duration;

	if (!check_clock_support()) {
		panic("No alternative clock supported yet.");
	}

	struct vm vm;

	err = vm_init(&vm);
	if (err) {
		printf("Failed to initialize vm.\n");
		return -1;
	}

	vm_register_module(&vm, &mod_base);
	vm_register_module(&vm, &mod_channel);

	{
		struct ast_context ctx;
		ctx = ast_init_context(NULL, &vm.atom_table, &vm,
				vm.default_types.type,
				vm.default_types.integer);



		struct ast_object_def array_type_def = {0};
		array_type_def.env.store = &vm.modules[0]->store;

		struct ast_object_def_param array_type_params[] = {
			{vm_atoms(&vm, "T"), AST_SLOT_TYPE},
			{vm_atoms(&vm, "N"),
				ast_bind_slot_const_type(&ctx, &array_type_def.env, AST_BIND_NEW,
						vm_atoms(&vm, "int"), vm.default_types.integer)},
		};

		array_type_def.params = array_type_params;
		array_type_def.num_params = ARRAY_LENGTH(array_type_params);

		array_type_def.ret_type = AST_SLOT_TYPE;

		ctx.cons.array = &array_type_def;



		/*
		struct ast_object_def array_def = {0};

		ast_slot_id array_members_T = ast_bind_slot_templ(
				&ctx, &array_def.env, AST_BIND_NEW,
				vm_atoms(&vm, "T"), AST_SLOT_TYPE);

		ast_slot_id array_members_N = ast_bind_slot_templ(
				&ctx, &array_def.env, AST_BIND_NEW,
				vm_atoms(&vm, "N"),
				ast_bind_slot_const_type(&ctx, &array_def.env, AST_BIND_NEW,
						vm_atoms(&vm, "int"), vm.default_types.integer));

		struct ast_object_arg array_cons_args[] = {
			{vm_atoms(&vm, "T"), array_members_T},
			{vm_atoms(&vm, "N"), array_members_N},
		};

		ast_slot_id array_members_type = ast_bind_slot_cons(
				&ctx, &array_def.env, AST_BIND_NEW,
				vm_atoms(&vm, "Array"), &array_type_def,
				array_cons_args, ARRAY_LENGTH(array_cons_args));


		array_def.ret_type = ast_bind_slot_cons(
				&ctx, &array_def.env, AST_BIND_NEW,
				vm_atoms(&vm, "Array"), &array_type_def,
				array_cons_args, ARRAY_LENGTH(array_cons_args));

		struct ast_object_def_param array_params[] = {
			{vm_atoms(&vm, "members"), array_members_type}
		};

		array_def.params = array_params;
		array_def.num_params = ARRAY_LENGTH(array_params);
		*/




		struct ast_object_def func_type_def = {0};
		func_type_def.env.store = &vm.modules[0]->store;

		ast_slot_id func_params_T = ast_bind_slot_templ(
				&ctx, &func_type_def.env, AST_BIND_NEW, vm_atoms(&vm, "T"), AST_SLOT_TYPE);

		ast_slot_id func_params_N = ast_bind_slot_templ(
				&ctx, &func_type_def.env, AST_BIND_NEW, vm_atoms(&vm, "N"),
				ast_bind_slot_const_type(&ctx, &func_type_def.env, AST_BIND_NEW,
						vm_atoms(&vm, "int"), vm.default_types.integer));

		struct ast_object_arg func_params_args[] = {
			{vm_atoms(&vm, "T"), func_params_T},
			{vm_atoms(&vm, "N"), func_params_N},
		};

		ast_slot_id func_params_type = ast_bind_slot_cons(
				&ctx, &func_type_def.env, AST_BIND_NEW,
				vm_atoms(&vm, "Array"), &array_type_def,
				func_params_args, ARRAY_LENGTH(func_params_args));

		struct ast_object_def_param func_type_params[] = {
			{vm_atoms(&vm, "ret"), AST_SLOT_TYPE},
			{vm_atoms(&vm, "params"), func_params_type},
		};

		func_type_def.params = func_type_params;
		func_type_def.num_params = ARRAY_LENGTH(func_type_params);

		func_type_def.ret_type = AST_SLOT_TYPE;

		ctx.cons.func = &func_type_def;


		struct ast_env test_env = {0};
		test_env.store = &vm.modules[0]->store;

		struct ast_node *expr;

		struct ast_func_arg test_nodes[] = {
			{
				.name  = vm_atoms(&vm, "a"),
				.value = ast_init_node_slot(&ctx, &test_env,
						AST_NODE_NEW, STG_NO_LOC,
						ast_bind_slot_const(&ctx, &test_env,
							AST_BIND_NEW, NULL,
							obj_register_integer(&vm, &vm.modules[0]->store, 4))),
			},
			{
				.name  = vm_atoms(&vm, "b"),
				.value = ast_init_node_slot(&ctx, &test_env,
						AST_NODE_NEW, STG_NO_LOC,
						ast_bind_slot_const(&ctx, &test_env,
							AST_BIND_NEW, NULL,
							obj_register_integer(&vm, &vm.modules[0]->store, 2))),
			}
		};

		struct atom *inner_func_arg_names[] = {
			vm_atoms(&vm, "a"),
			vm_atoms(&vm, "b"),
		};

		struct ast_node *inner_func =
			ast_init_node_func(&ctx, &test_env, AST_NODE_NEW, STG_NO_LOC,
					inner_func_arg_names, ARRAY_LENGTH(inner_func_arg_names));

		struct ast_node *inner_func_arg_types[] = {
				ast_init_node_slot(&ctx, &inner_func->func.env,
						AST_NODE_NEW, STG_NO_LOC,
						ast_bind_slot_const_type(&ctx, &inner_func->func.env,
							AST_BIND_NEW, NULL,
							vm_find_type_id(&vm, STR("base"), STR("int")))),
				ast_init_node_slot(&ctx, &inner_func->func.env,
						AST_NODE_NEW, STG_NO_LOC,
						ast_bind_slot_const_type(&ctx, &inner_func->func.env,
							AST_BIND_NEW, NULL,
							vm_find_type_id(&vm, STR("base"), STR("int")))),
		};

		struct ast_func_arg func_test_nodes[] = {
			{
				.name  = vm_atoms(&vm, "lhs"),
				.value = ast_init_node_slot(&ctx, &inner_func->func.env,
						AST_NODE_NEW, STG_NO_LOC,
						ast_env_lookup_or_alloc_free(&ctx, &inner_func->func.env,
							vm_atoms(&vm, "a"),
							ast_bind_slot_wildcard(&ctx, &inner_func->func.env, AST_BIND_NEW,
								NULL, AST_SLOT_TYPE))),
			},
			{
				.name  = vm_atoms(&vm, "rhs"),
				.value = ast_init_node_slot(&ctx, &inner_func->func.env,
						AST_NODE_NEW, STG_NO_LOC,
						ast_env_lookup_or_alloc_free(&ctx, &inner_func->func.env,
							vm_atoms(&vm, "b"),
							ast_bind_slot_wildcard(&ctx, &inner_func->func.env, AST_BIND_NEW,
								NULL, AST_SLOT_TYPE))),
			}
		};

		expr = ast_init_node_call(&ctx, &test_env,
				AST_NODE_NEW, STG_NO_LOC,
				ast_finalize_node_func(&ctx, &test_env, inner_func,
					inner_func_arg_types, ARRAY_LENGTH(inner_func_arg_types), NULL,
					ast_init_node_call(&ctx, &inner_func->func.env, AST_NODE_NEW, STG_NO_LOC,
						ast_init_node_slot(&ctx, &inner_func->func.env, AST_NODE_NEW, STG_NO_LOC,
							ast_env_lookup_or_alloc_free(&ctx, &inner_func->func.env,
								vm_atoms(&vm, "op+"),
								ast_bind_slot_wildcard(&ctx, &inner_func->func.env, AST_BIND_NEW,
									NULL, AST_SLOT_TYPE))),
						func_test_nodes, ARRAY_LENGTH(func_test_nodes))),
				test_nodes, ARRAY_LENGTH(test_nodes));

		ast_env_print(&vm, &test_env);
		printf("\n");
		ast_env_print(&vm, &inner_func->func.env);
		printf("\n");
		ast_print(&ctx, &test_env, expr);
	}

	return 0;

	err = stg_compile(&vm, STR("./config/"));
	if (err) {
		printf("Failed to compile config.\n");
		return -1;
	}

	vm_start(&vm);

	uint64_t tick_period = NSEC / 1000;

	frame_duration.tv_sec = tick_period / NSEC;
	frame_duration.tv_nsec = tick_period % NSEC;
	tick_begin = read_time();

	signal(SIGINT, stage_signal_handler);

	while (!should_quit) {
		struct timespec tick_end_desired;
		int clock_err;

		tick_end_desired = timespec_add(tick_begin, frame_duration);

		clock_err =
		    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
				    &tick_end_desired, 0);
		if (clock_err && clock_err != EINTR) {
			perror("clock_nanosleep");
		}

		tick_begin = read_time();
	}

	vm_destroy(&vm);

	return 0;
}

#ifdef STAGE_TEST
#undef main
#endif
