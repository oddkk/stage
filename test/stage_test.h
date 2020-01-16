#include "vm.h"
#include "ast.h"
#include "module.h"
#include "base/mod.h"
#include <string.h>
#include <signal.h>

#define TEST_ASSERT(expr)							\
	do {											\
		if (!(expr)) {								\
			fprintf(stdout,							\
					__FILE__ ":%i: "				\
					"Assertion '" #expr "' failed!\n",\
					__LINE__);						\
			return -1;								\
		}											\
	} while (0);

static inline int
stg_test_bootstrap(
		struct vm *vm, struct ast_context *ctx,
		struct stg_module **out_mod)
{
	int err;

	err = vm_init(vm);
	if (err) {
		printf("Failed to initialize vm.\n");
		return -1;
	}

	stg_base_load(vm);

	*ctx = ast_init_context(
			NULL, &vm->atom_table, vm);

	struct stg_module_info mod_info = {
		.name = STR("test"),
		.version = {.major = 0, .minor = 1},
	};

	*out_mod = vm_register_module(
			vm, ctx, NULL, &mod_info);

	return 0;
}
