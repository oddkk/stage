#include "bytecode.h"
#include "vm.h"

#define VAR_LIT(var) ((var) < 0 ? 'p' : 't'), ((var < 0) ? -var : var)

static void
bc_print_var(struct bc_env *env, bc_var var)
{
	char prefix = 't';
	int var_id = var;
	type_id tid;

	if (var < 0) {
		prefix = 'p';
		var_id -= var_id;
	}

	tid = bc_get_var_type(env, var);

	printf("%c%i:", prefix, var_id);
	print_type_repr(env->vm,
			vm_get_type(env->vm, tid));
}

void
bc_print(struct bc_env *env, struct bc_instr *instr)
{
	while (instr) {
		switch (instr->op) {
			case BC_NOP:
				printf("NOP\n");
				break;

			case BC_LOAD:
				printf("LOAD ");
				bc_print_var(env, instr->load.target);
				printf(" = [c%u] ", instr->load.obj);
				assert(instr->load.obj < env->num_consts);
				print_obj_repr(env->vm, env->consts[instr->load.obj]);
				printf("\n");
				break;

			case BC_PUSH_ARG:
				printf("PUSH_ARG ");
				bc_print_var(env, instr->push_arg.var);
				printf("\n");
				break;

			case BC_LCALL:
				printf("LCALL ");
				bc_print_var(env, instr->lcall.target);
				printf(" = %lu\n", instr->lcall.func);
				break;

			case BC_VCALL:
				printf("VCALL ");
				bc_print_var(env, instr->vcall.target);
				printf(" = ");
				bc_print_var(env, instr->vcall.func);
				printf("\n");
				break;

			case BC_RET:
				printf("RET ");
				bc_print_var(env, instr->ret.var);
				printf("\n");
				break;
		}

		instr = instr->next;
	}
}
