#include "mod.h"

static void debug_print_integer(struct vm *vm, struct exec_stack *stack, void *data)
{
	int64_t value;
	stack_peek(stack, &value, sizeof(int64_t));

	printf("debug print: %li\n", value);
}

void
base_functions_register(struct stg_module *mod)
{
	(void)debug_print_integer;
}
