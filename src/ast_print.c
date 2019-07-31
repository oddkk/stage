#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "vm.h"

void
ast_env_print(struct vm *vm, struct ast_env *env)
{
	printf("|#  |name      |type |kind      |\n");
	printf("|---|----------|-----|----------|\n");

	for (size_t i = 0; i < env->num_slots; i++) {
		struct ast_env_slot *slot;

		slot = &env->slots[i];

		char *kind_name;

		switch (slot->kind) {
			case AST_SLOT_WILDCARD:   kind_name = "WILDCARD";   break;
			case AST_SLOT_CONST_TYPE: kind_name = "CONST_TYPE"; break;
			case AST_SLOT_CONST:      kind_name = "CONST";      break;
			case AST_SLOT_PARAM:      kind_name = "PARAM";      break;
			case AST_SLOT_TEMPL:      kind_name = "TEMPL";      break;
			case AST_SLOT_FREE:       kind_name = "FREE";       break;
			case AST_SLOT_CONS:       kind_name = "CONS";       break;
		}

		printf("|%3zu|%-10.*s|%5i|%-10s|", i, ALIT(slot->name), slot->type, kind_name);

		switch (slot->kind) {
			case AST_SLOT_WILDCARD:
				break;

			case AST_SLOT_CONST_TYPE:
				print_type_repr(vm, vm_get_type(vm, slot->const_type));
				break;

			case AST_SLOT_CONST:
				print_obj_repr(vm, slot->const_object);
				break;

			case AST_SLOT_PARAM:
				printf("index=%li", slot->param_index);
				break;

			case AST_SLOT_TEMPL:
				break;

			case AST_SLOT_FREE:
				break;

			case AST_SLOT_CONS:
				printf("Cons(");
				for (size_t j = 0; j < slot->cons.def->num_params; j++) {
					if (j != 0)
						printf(", ");
					printf("%.*s=%i",
							ALIT(slot->cons.def->params[j].name),
							slot->cons.args[j]);
				}
				printf(")");
				break;
		}

		printf("\n");
	}
}

