#include "utils.h"
#include "vm.h"
#include "compile.h"
#include "ast.h"
#include "base/mod.h"

#include <stdio.h>
#include <stdlib.h>

#include <time.h>
#include <errno.h>

#include <signal.h>
#include <unistd.h>

#include <argp.h>

#ifdef STAGE_TEST
#define main _main
#endif

#define NSEC (1000000000)

const char *
argp_program_version = "0.0.1";

static const char
args_doc[] = "FILE";

static const char
doc[] = "Stage -- a simple functional programming language.";

struct stg_arguments {
	bool print_module;
	struct string project_path;
};

#define OPT_PRINT_MODULE 1

static struct argp_option options[] = {
	{"print-module", OPT_PRINT_MODULE, 0, 0, "Print the instance object for the module."},
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

		case OPT_PRINT_MODULE:
			args->print_module = true;
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

enum stg_run_flags {
	STG_SHOULD_STOP = 1<<0,
};

static enum stg_run_flags stg_run_flags = 0;

static void
stg_singal_handler(int sig)
{
	switch (sig) {
		case SIGINT:
			stg_run_flags |= STG_SHOULD_STOP;
			break;
	}
}

int main(int argc, char *argv[])
{
	int err;

	struct stg_arguments args = {0};

	// Temporarly set default project for development.
	// TODO: Remove.
	args.project_path = STR("./examples/test.stg");

	argp_parse(&argp, argc, argv, 0, 0, &args);

	struct vm vm;

	err = vm_init(&vm);
	if (err) {
		printf("Failed to initialize vm.\n");
		return -1;
	}

	stg_base_load(&vm);
	vm_request_module(&vm, VM_REQUEST_PINNED,
			vm_atoms(&vm, "main"), args.project_path);

	struct ast_context ctx;
	ast_init_context(&ctx, NULL, &vm);

	struct string module_locations[] = {
		STR("./modules/"),
	};

	struct stg_compile_options compile_opts = {
		.module_locations = module_locations,
		.num_module_locations = ARRAY_LENGTH(module_locations),
	};
	vm.compile_options = compile_opts;

	err = stg_compile(&vm, &ctx);
	if (err) {
		return -1;
	}

	ast_destroy_context(&ctx);

	if (args.print_module) {
		struct stg_module *main_mod;
		main_mod = vm_get_module(&vm, vm_atoms(&vm, "main"));
		print_obj_repr(&vm, main_mod->instance);
		printf("\n");
	}

	vm_start(&vm);

	stg_run_flags |= STG_SHOULD_STOP;

	signal(SIGINT, stg_singal_handler);

	while ((stg_run_flags & STG_SHOULD_STOP) == 0) {
		pause();
	}

	signal(SIGINT, SIG_DFL);

	vm_stop(&vm);
	vm_destroy(&vm);

	return 0;
}

#ifdef STAGE_TEST
#undef main
#endif
