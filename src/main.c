#include "utils.h"
#include "vm.h"
#include "compile.h"
#include "ast.h"
#include "base/mod.h"

#include <stdio.h>
#include <stdlib.h>

#include <time.h>
#include <errno.h>

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
	ctx = ast_init_context(NULL, &vm.atom_table, &vm);

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

	vm_start(&vm);
	vm_destroy(&vm);

	return 0;
}

#ifdef STAGE_TEST
#undef main
#endif
