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
		var_id = (-var_id) - 1;
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

			case BC_COPY:
				printf("COPY ");
				bc_print_var(env, instr->copy.target);
				printf(" = ");
				bc_print_var(env, instr->copy.src);
				printf(" (");
				print_type_repr(env->vm,
						vm_get_type(env->vm, bc_get_var_type(env, instr->copy.src)));
				printf(")\n");
				break;

			case BC_COPY_CLOSURE:
				printf("COPY_CLOSURE ");
				bc_print_var(env, instr->copy_closure.target);
				printf(" = %u (", instr->copy_closure.closure);
				print_type_repr(env->vm, vm_get_type(env->vm,
							bc_get_closure_type(env, instr->copy_closure.closure)));
				printf(")\n");
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

			case BC_CLCALL:
				printf("CLCALL ");
				bc_print_var(env, instr->clcall.target);
				printf(" = %lu (%p)\n",
						instr->clcall.func,
						instr->clcall.closure);
				break;

			case BC_VCALL:
				printf("VCALL ");
				bc_print_var(env, instr->vcall.target);
				printf(" = ");
				bc_print_var(env, instr->vcall.func);
				printf("\n");
				break;

			case BC_TESTEQ:
				printf("TESTEQ ");
				bc_print_var(env, instr->testeq.target);
				printf(" ");
				bc_print_var(env, instr->testeq.lhs);
				printf(" == ");
				bc_print_var(env, instr->testeq.rhs);
				printf("\n");
				break;

			case BC_JMP:
				printf("JMP %p\n", (void *)instr->jmp);
				break;

			case BC_JMPIF:
				printf("JMPIF %p\n", (void *)instr->jmp);
				break;

			case BC_PACK:
				printf("PACK ");
				bc_print_var(env, instr->pack.target);
				printf(" = %p(%p)\n",
						(void *)instr->pack.func,
						instr->pack.data);
				break;

			case BC_UNPACK:
				printf("UNPACK ");
				bc_print_var(env, instr->unpack.target);
				printf(" = %p(%p, %i)\n",
						(void *)instr->unpack.func,
						instr->unpack.data,
						instr->unpack.param_id);
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

void
bc_tag_labels(struct bc_env *env)
{
	struct bc_instr *ip = env->entry_point;

	size_t next_label = 0;

	assert(!env->labels_tagged);

	while (ip) {
		if (ip->jmp) {
			if (ip->jmp->label < 0) {
				ip->jmp->label = next_label;
				next_label += 1;
			}
		}

		ip = ip->next;
	}

	env->num_labels = next_label;
	env->labels_tagged = true;
}
