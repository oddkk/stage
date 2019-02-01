#include "mod.h"

/* static void debug_print_integer(struct vm *vm, struct exec_stack *stack, void *data) */
/* { */
/* 	int64_t value; */
/* 	stack_peek(stack, &value, sizeof(int64_t)); */

/* 	printf("debug print: %li\n", value); */
/* } */

BUILTIN_PURE(print, debug_print_int, STG_INT, (STG_INT, in))
{
	printf("debug print: %li\n", in);

	return in;
}

BUILTIN_PURE(print, debug_print_str, STG_STR, (STG_STR, in))
{
	printf("debug print: %.*s\n", LIT(in));

	return in;
}

void
base_register_functions(struct stg_module *mod)
{
	stg_register_builtin_func(mod, debug_print_int, NULL);
	stg_register_builtin_func(mod, debug_print_str, NULL);
}
