#include "utils.h"
#include "vm.h"
#include "compile.h"
#include "ast.h"
#include "base/mod.h"

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#include <time.h>
#include <errno.h>

#include <argp.h>

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

#if 0
struct timespec
timespec_add(struct timespec begin, struct timespec end)
{
	struct timespec temp;
	temp.tv_sec = begin.tv_sec + end.tv_sec;
	temp.tv_nsec = begin.tv_nsec + end.tv_nsec;
	temp.tv_sec += temp.tv_nsec / NSEC;
	temp.tv_nsec %= NSEC;
	return temp;
}

struct timespec
read_time()
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
#endif

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

const char *
argp_program_version = "0.0.1";

static const char
args_doc[] = "FILE";

static const char
doc[] = "Stage -- a simple functional programming language.";

struct stg_arguments {
	struct string project_path;
};

static struct argp_option options[] = {
	{0}
};

static error_t
parse_opt(int key, char *arg, struct argp_state *state)
{
	struct stg_arguments *args = state->input;

	switch (key) {
		case ARGP_KEY_ARG:
			args->project_path =
				string_duplicate_cstr(arg);
			break;

		// Temporarly allows a default project for development.
		// TODO: Remove.
		// case ARGP_KEY_NO_ARGS:
		// 	argp_usage(state);
		// 	break;

		default:
			return ARGP_ERR_UNKNOWN;
	}

	return 0;
}

static struct argp argp = { options, parse_opt, args_doc, doc };

int main(int argc, char *argv[])
{
	int err;

	struct stg_arguments args = {0};

	// Temporarly set default project for development.
	// TODO: Remove.
	args.project_path = STR("./config");

	argp_parse(&argp, argc, argv, 0, 0, &args);

	if (!check_clock_support()) {
		panic("No alternative clock supported yet.");
	}

	struct vm vm;

	err = vm_init(&vm);
	if (err) {
		printf("Failed to initialize vm.\n");
		return -1;
	}

	stg_base_load(&vm);

	struct ast_context ctx;
	ctx = ast_init_context(NULL, &vm.atom_table, &vm);

	struct string module_locations[] = {
		STR("./modules/"),
	};

	struct stg_compile_options compile_opts = {
		.module_locations = module_locations,
		.num_module_locations = ARRAY_LENGTH(module_locations),
	};

	err = stg_compile(&vm, &ctx, compile_opts, args.project_path);
	if (err) {
		return -1;
	}

	struct type *program_obj_type;
	program_obj_type = vm_get_type(&vm, vm.program_object_type);

	uint8_t program_obj_buffer[program_obj_type->size];
	struct object program_obj = {0};

	program_obj.data = program_obj_buffer;
	program_obj.type = vm.program_object_type;

	struct stg_exec exec_ctx;
	exec_ctx = vm_init_exec_context(&vm);
	vm_call_func(&vm, &exec_ctx, vm.init_func, NULL, 0, &program_obj);

	/*
	print_obj_repr(&vm, program_obj);
	printf("\n");
	*/

	struct type *program_type;
	program_type = vm_get_type(&vm, vm.program_object_type);
	assert(program_type->obj_def);

	ssize_t main_unpack_id;
	main_unpack_id = object_cons_simple_lookup(
			&vm, program_type->obj_def, STR("main"));

	if (main_unpack_id >= 0) {
		struct object main_obj = {0};

		int err;
		err = object_cons_descendant_type(
				&vm, program_obj.type,
				main_unpack_id, &main_obj.type);

		struct type *main_obj_type;
		main_obj_type = vm_get_type(&vm, main_obj.type);

		uint8_t buffer[main_obj_type->size];
		main_obj.data = buffer;

		err = object_unpack(
				&vm, program_obj,
				main_unpack_id, &main_obj);
		assert(!err);

		struct object result = {0};
		result.type = vm.default_types.unit;

		stg_unsafe_call_init(&vm, &exec_ctx, main_obj, &result);
	}

	vm_release_exec_context(&vm, &exec_ctx);

#if 0
	for (size_t i = 0; i < vm.num_modules; i++) {
		printf("Module %.*s:\n", LIT(vm.modules[i]->info.name));
		if (vm.modules[i]->mod.root) {
			ast_print(&ctx, vm.modules[i]->mod.root);
		}
		printf("\n");
	}
#endif

	vm_post_init(&vm);
	vm_start(&vm);

#if 0
	struct timespec tick_begin;
	struct timespec frame_duration;
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
#endif

	vm_destroy(&vm);

	return 0;
}

#ifdef STAGE_TEST
#undef main
#endif
