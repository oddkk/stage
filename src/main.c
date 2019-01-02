// TODO:
//  * Make separate "apply lists" for device types, to allow device
//    creation outside config
//  * Fix referencing elements of tuples, both named and unnamed
//  * Implement const fields in config (fields that are only used for
//    calculations)
//  * Make types "first-class" (a type type)?
//  * Allow "templated" types in device types for channels and
//    attributes (type types). Concider if channels should be created
//    at init instead of as part of device type definition.
//  * Clean up config.c

/* #include "stage.h" */
/* #include "channel.h" */
/* #include "device.h" */
/* #include "device_type.h" */
#include "utils.h"
#include "vm.h"
#include "config.h"
/* #include "websocket.h" */

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

/* static void test_func(struct vm *vm, struct exec_stack *stack) */
/* { */
/* 	printf("I'm called!\n"); */
/* } */

/* static void test_int_add(struct vm *vm, struct exec_stack *stack) */
/* { */
/* 	struct object lhs = stack_pop(stack); // = vm_arg(stack, 0); */
/* 	struct object rhs = stack_pop(stack); // = vm_arg(stack, 1); */

/* 	assert(lhs.type == vm->types.integer); */
/* 	assert(rhs.type == vm->types.integer); */

/* 	int64_t lhs_value = (int64_t)lhs.data; */
/* 	int64_t rhs_value = (int64_t)rhs.data; */
/* 	int64_t result_value = lhs_value + rhs_value; */

/* 	struct object result; */
/* 	result.type = vm->types.integer; */
/* 	result.data = (void *)result_value; */

/* 	stack_push(stack, result); */
/* } */

int main(int argc, char *argv[])
{
	int err;
	struct timespec tick_begin;
	struct timespec frame_duration;

	if (!check_clock_support()) {
		panic("No alternative clock supported yet.");
	}

	/* struct stage stage; */
	/* struct config_node *node; */

	/* err = stage_init(&stage); */
	/* if (err) { */
	/* 	return err; */
	/* } */

	/* struct device *dev; */
	/* dev = register_device_pre_attrs(&stage, 0, &stage.root_scope, SATOM(&stage, "c"), NULL); */
	/* finalize_device(&stage, dev); */

	/* err = parse_config_file(STR("config/alt.stg"), &stage.atom_table, */
	/* 						&stage.memory, &node); */

	/* if (err) { */
	/* 	return err; */
	/* } */

	/* config_tree_print(node); */

	/* err = apply_config(&stage, node); */
	/* if (err) { */
	/* 	return err; */
	/* } */


/* #if 0 */
/* 	printf */
/* 	    ("============================ types ============================\n"); */
/* 	for (int i = 0; i < stage.num_types; i++) { */
/* 		struct type *type; */

/* 		type = get_type(&stage, i); */

/* 		print_type(stdout, &stage, type); */
/* 		printf(" "); */
/* 		expand_type(stdout, &stage, type, false); */
/* 		//expand_type(&stage, type, true); */
/* 		printf("\n"); */
/* 	} */
/* 	printf("\n"); */

/* 	printf */
/* 	    ("======================== devices_types ========================\n"); */
/* 	for (int i = 0; i < stage.num_device_types; i++) { */
/* 		struct device_type *dev_type; */

/* 		dev_type = get_device_type(&stage, i); */

/* 		describe_device_type(&stage, dev_type); */
/* 		printf("\n"); */
/* 	} */

/* 	printf */
/* 	    ("=========================== devices ===========================\n"); */
/* 	for (int i = 0; i < stage.num_devices; i++) { */
/* 		struct device *dev; */

/* 		dev = get_device(&stage, i); */

/* 		describe_device(&stage, dev); */
/* 		printf("\n"); */
/* 	} */
/* #endif */

	/* stage.tick_period = NSEC / 1000; */

	/* struct websocket_context ws = {{0}}; */

	/* websocket_init(&ws, "0.0.0.0", "6060"); */

	/* frame_duration.tv_sec = stage.tick_period / NSEC; */
	/* frame_duration.tv_nsec = stage.tick_period % NSEC; */

	struct vm vm;

	err = vm_init(&vm);
	if (err) {
		printf("Failed to initialize vm.\n");
		return -1;
	}

	/* obj_id test1 = register_object(&vm.store, (struct object){ */
	/* 		.type=vm.default_types.integer, .data=(void*)5L}); */

	/* print_type_repr(&vm, *get_object(&vm.store, test1)); */
	/* printf("\n"); */

	/* obj_id int1 = obj_register_integer(&vm, 5); */
	/* obj_id int2 = obj_register_integer(&vm, 3); */
	/* obj_id add_func; */
	/* obj_id print_func; */

	/* add_func = scope_lookup_id(&vm.root_scope, */
	/* 						   atom_create(&vm.atom_table, STR("op+"))); */
	/* if (add_func == OBJ_NONE) { */
	/* 		panic("add operation not found."); */
	/* } */

	/* print_func = scope_lookup_id(&vm.root_scope, */
	/* 							 atom_create(&vm.atom_table, STR("print"))); */
	/* if (print_func == OBJ_NONE) { */
	/* 		panic("print func not found."); */
	/* } */

	/* void *int1_data = get_object(&vm.store, int1).data; */
	/* void *int2_data = get_object(&vm.store, int2).data; */
	/* void *add_func_data = get_object(&vm.store, add_func).data; */
	/* void *print_func_data = get_object(&vm.store, print_func).data; */

	/* uint8_t instructions[] = { */
	/* 	LIT_VM_INST_PUSH_GLOBAL((uint64_t)int2_data, sizeof(int64_t)), */
	/* 	LIT_VM_INST_PUSH_GLOBAL((uint64_t)int1_data, sizeof(int64_t)), */
	/* 	LIT_VM_INST_PUSH_GLOBAL((uint64_t)add_func_data, sizeof(struct obj_builtin_func_data)), */
	/* 	LIT_VM_INST_CALL(), */
	/* 	LIT_VM_INST_PUSH_GLOBAL((uint64_t)print_func_data, sizeof(struct obj_builtin_func_data)), */
	/* 	LIT_VM_INST_CALL(), */
	/* 	LIT_VM_INST_RETURN(), */
	/* }; */

	/* struct exec_stack stack; */
	/* arena_alloc_stack(&stack, &vm.memory, 1024); */
	/* vm_exec(&vm, &stack, (void *)instructions, sizeof(instructions)); */

	/* printf("stack length: %zu, value: %li\n", stack.sp, (int64_t)stack.memory[0].data); */

	err = cfg_compile(&vm, STR("./config/"));
	if (err) {
		printf("Failed to compile config.\n");
		return -1;
	}

	return 0;

	uint64_t tick_period = NSEC / 1000;

	frame_duration.tv_sec = tick_period / NSEC;
	frame_duration.tv_nsec = tick_period % NSEC;
	tick_begin = read_time();

	signal(SIGINT, stage_signal_handler);

	while (!should_quit) {
		struct timespec tick_end_desired;
		int clock_err;

		/* stage_tick(&stage); */

		tick_end_desired = timespec_add(tick_begin, frame_duration);

		clock_err =
		    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
				    &tick_end_desired, 0);
		if (clock_err && clock_err != EINTR) {
			perror("clock_nanosleep");
		}

		tick_begin = read_time();
	}

	/* stage_destroy(&stage); */

	return 0;
}

#ifdef STAGE_TEST
#undef main
#endif
