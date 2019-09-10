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

		const char *kind_name;

		kind_name = ast_slot_name(slot->kind);

		printf("|%3zu|%-10.*s|%5i|%-10s|", i, ALIT(slot->name), slot->type, kind_name);

		switch (slot->kind) {
			case AST_SLOT_ERROR:
				break;

			case AST_SLOT_WILDCARD:
				break;

			case AST_SLOT_CONST_TYPE:
				print_type_repr(vm, vm_get_type(vm, slot->const_type));
				printf(" (%li)", slot->const_type);
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
				for (size_t j = 0; j < slot->cons.num_present_args; j++) {
					if (j != 0)
						printf(", ");
					printf("%.*s=%i",
							ALIT(slot->cons.args[j].name),
							slot->cons.args[j].slot);
				}
				printf(")");
				break;

			case AST_SLOT_CONS_ARRAY:
				printf("[");
				for (size_t j = 0; j < slot->cons_array.num_members; j++) {
					if (j != 0)
						printf(", ");
					printf("%i", slot->cons_array.members[j]);
				}
				printf("] type-slot=%i count-slot=%i",
						slot->cons_array.member_type,
						slot->cons_array.member_count);
				break;

			case AST_SLOT_SUBST:
				printf("%i", slot->subst);
				break;
		}

		printf("\n");
	}
}

static void
print_indent(int depth)
{
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static void
print_slot(struct ast_env *env, ast_slot_id slot)
{
	printf("%i", slot);
}

void
ast_print_internal(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, unsigned int depth)
{
	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_UNINIT:
			print_indent(depth);
			if (node->kind == AST_NODE_FUNC_UNINIT) {
				printf("func (uninit)");
			} else {
				printf("func");
			}

			printf(" %i\n", node->func.type);

			for (size_t i = 0; i < node->func.num_params; i++) {
				print_indent(depth + 1);
				printf("param %li '%.*s' type\n", i, ALIT(node->func.params[i].name));
				ast_print_internal(ctx, env, node->func.params[i].type, depth + 2);
			}

			print_indent(depth + 1);
			printf("return type\n");
			ast_print_internal(ctx, env, node->func.return_type, depth + 2);

			print_indent(depth + 1);
			printf("body\n");
			ast_print_internal(ctx, env, node->func.body, depth + 2);
			break;

		case AST_NODE_CALL:
			print_indent(depth);
			printf("call\n");

			print_indent(depth + 1);
			printf("target\n");
			ast_print_internal(ctx, env, node->call.func, depth + 2);

			for (size_t i = 0; i < node->call.num_args; i++) {
				print_indent(depth + 1);
				printf("arg %li '%.*s' value\n", i, ALIT(node->call.args[i].name));
				ast_print_internal(ctx, env, node->call.args[i].value, depth + 2);
			}

			break;

		case AST_NODE_SLOT:
			print_indent(depth);
			printf("slot ");
			print_slot(env, node->slot);
			printf(": ");
			print_slot(env, ast_node_type(ctx, env, node));
			printf("\n");
			break;

		case AST_NODE_LOOKUP:
			print_indent(depth);
			printf("lookup '%.*s' ", ALIT(node->lookup.name));
			print_slot(env, node->lookup.slot);
			printf(": ");
			print_slot(env, ast_node_type(ctx, env, node));
			printf("\n");
			break;
	}
}

void
ast_print(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	ast_print_internal(ctx, env, node, 0);
}

static void
ast_print_namespace(struct ast_context *ctx, struct ast_env *env,
		struct ast_namespace *ns, int indent)
{
	for (size_t i = 0; i < ns->num_names; i++) {
		struct ast_module_name *name = &ns->names[i];

		print_indent(indent);
		printf("%.*s: ", ALIT(name->name));

		switch (name->kind) {
			case AST_MODULE_NAME_DECL:
				printf("decl %i\n", name->decl.value);
				ast_print_internal(ctx, env, name->decl.expr, indent + 1);
				break;

			case AST_MODULE_NAME_NAMESPACE:
				printf("ns\n");
				ast_print_namespace(ctx, env, name->ns, indent+1);
				break;
		}
	}
}

void
ast_print_module(struct ast_context *ctx, struct ast_module *mod)
{
	ast_print_namespace(ctx, &mod->env, &mod->root, 0);
}
