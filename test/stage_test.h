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

	*out_mod = vm_request_module(
			vm, VM_REQUEST_PINNED, NULL,
			VM_REQUEST_MOD_NO_LOC);

	return 0;
}

#define STG_TEST(fun)                               \
int fun(struct ast_context *, struct stg_module *); \
int main()                                          \
{                                                   \
	struct vm vm;                                   \
	struct stg_module *mod;                         \
	struct ast_context ctx;                         \
                                                    \
	int err;                                        \
	err = stg_test_bootstrap(&vm, &ctx, &mod);      \
	if (err) {                                      \
		return -1;                                  \
	}                                               \
                                                    \
	return fun(&ctx, mod);                          \
}
