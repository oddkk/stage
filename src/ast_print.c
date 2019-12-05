#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "vm.h"

void
ast_print_slot_internal(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id slot_id, bool print_type)
{
	struct ast_env_slot slot; 
	slot = ast_env_slot(ctx, env, slot_id);

	switch (slot.kind) {
		case AST_SLOT_ERROR:
			printf("(error)");
			break;

		case AST_SLOT_WILDCARD:
			printf("?");
			break;

		case AST_SLOT_CONST_TYPE:
			print_type_repr(ctx->vm, vm_get_type(ctx->vm, slot.const_type));
			break;

		case AST_SLOT_CONST:
			print_obj_repr(ctx->vm, slot.const_object);
			break;

		case AST_SLOT_PARAM:
			printf("param[%li]", slot.param_index);
			break;

		case AST_SLOT_TEMPL:
			printf("(templ)");
			break;

		case AST_SLOT_CLOSURE:
			printf("closure");
			break;


		case AST_SLOT_CONS:
			printf("%p{", (void *)slot.cons.def);
			for (size_t j = 0; j < slot.cons.num_present_args; j++) {
				if (j != 0)
					printf(", ");
				printf("%.*s=", ALIT(slot.cons.args[j].name));
				ast_print_slot_internal(ctx, env, slot.cons.args[j].slot, true);
			}
			printf("}");
			break;

		case AST_SLOT_CONS_ARRAY:
			// printf("Array<type=");
			// ast_print_slot_internal(ctx, env, slot.cons_array.member_type, print_type);
			// printf("count=");
			// ast_print_slot_internal(ctx, env, slot.cons_array.member_count, print_type);
			// printf(">[");
			printf("[");
			for (size_t j = 0; j < slot.cons_array.num_members; j++) {
				if (j != 0)
					printf(", ");
				ast_print_slot_internal(ctx, env, slot.cons_array.members[j], false);
			}
			printf("]");
			break;

		case AST_SLOT_SUBST:
			ast_print_slot_internal(ctx, env, slot.subst, print_type);
			break;
	}

	if (print_type) {
		printf(":");
		ast_print_slot_internal(ctx, env, slot.type, false);
	}
}

void
ast_print_slot(struct ast_context *ctx, struct ast_env *env, ast_slot_id slot_id)
{
	if (!ctx) {
		printf("(no ctx)");
		return;
	}
	ast_print_slot_internal(ctx, env, slot_id, true);
}

void
ast_env_print(struct vm *vm, struct ast_env *env)
{
	printf("|#  |type |kind      |\n");
	printf("|---|-----|----------|\n");

	for (size_t i = 0; i < env->num_slots; i++) {
		struct ast_env_slot *slot;

		slot = &env->slots[i];

		const char *kind_name;

		kind_name = ast_slot_name(slot->kind);

		printf("|%3zu|%5i|%-10s|", i,
				slot->type, kind_name);

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

			case AST_SLOT_CLOSURE:
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
ast_print_name_ref(struct ast_name_ref ref)
{
	switch (ref.kind) {
		case AST_NAME_REF_NOT_FOUND:
			printf("not found");
			break;

		case AST_NAME_REF_MEMBER:
			printf("member %i", ref.member);
			break;

		case AST_NAME_REF_PARAM:
			printf("param %i", ref.param);
			break;

		case AST_NAME_REF_CLOSURE:
			printf("closure %i", ref.closure);
			break;

		case AST_NAME_REF_TEMPL:
			printf("templ %i", ref.templ);
			break;
	}
}

static void
ast_print_internal_closure(struct ast_context *ctx, struct ast_env *env,
		struct ast_closure_target *closure, unsigned int depth)
{
	print_indent(depth);
	printf("closures:\n");
	for (size_t i = 0; i < closure->num_members; i++) {
		print_indent(depth + 1);
		printf("%.*s: ", ALIT(closure->members[i].name));
		ast_print_name_ref(closure->members[i].ref);
		printf("\n");
	}
}

void
ast_print_internal(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, unsigned int depth)
{
	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
			print_indent(depth);
			printf("func");

			printf(" %i\n", node->func.type);

			for (size_t i = 0; i < node->func.num_params; i++) {
				print_indent(depth + 1);
				printf("param %li '%.*s' %i type\n", i,
						ALIT(node->func.params[i].name),
						node->func.params[i].slot);
				ast_print_internal(ctx, env, node->func.params[i].type, depth + 2);
			}

			print_indent(depth + 1);
			printf("return type %i\n", node->func.return_type_slot);
			ast_print_internal(ctx, env, node->func.return_type, depth + 2);

			if (node->kind == AST_NODE_FUNC) {
				print_indent(depth + 1);
				printf("body\n");
				ast_print_internal(ctx, env, node->func.body, depth + 2);
				ast_print_internal_closure(ctx, env,
						&node->func.closure, depth + 1);
			} else if (node->kind == AST_NODE_FUNC_NATIVE) {
				print_indent(depth + 1);
				printf("body native %.*s\n", LIT(node->func.native.name));
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			print_indent(depth);
			if (node->kind == AST_NODE_CALL) {
				printf("call: ");
				print_slot(env, node->call.ret_type);
				printf("\n");
			} else {
				printf("cons: ");
				print_slot(env, node->call.ret_type);
				printf("\n");
				print_indent(depth + 1);
				printf("cons slot ");
				print_slot(env, node->call.cons);
				printf("\n");
			}

			print_indent(depth + 1);
			printf("target\n");
			ast_print_internal(ctx, env, node->call.func, depth + 2);

			for (size_t i = 0; i < node->call.num_args; i++) {
				print_indent(depth + 1);
				printf("arg %li '%.*s' value\n", i, ALIT(node->call.args[i].name));
				ast_print_internal(ctx, env, node->call.args[i].value, depth + 2);
			}
			break;

		case AST_NODE_FUNC_TYPE:
			print_indent(depth);
			printf("func type:\n");
			for (size_t i = 0; i < node->func_type.num_params; i++) {
				print_indent(depth+1);
				printf("param %zu type:\n", i);
				ast_print_internal(ctx, env, node->func_type.param_types[i], depth+2);
			}
			print_indent(depth+1);
			printf("return type:\n");
			ast_print_internal(ctx, env, node->func_type.ret_type, depth+2);
			break;

		case AST_NODE_TEMPL:
			print_indent(depth);
			printf("templ:\n");
			print_indent(depth + 1);
			printf("params:\n");
			for (size_t i = 0; i < node->templ.num_params; i++) {
				print_indent(depth + 2);
				printf("'%.*s' = ", ALIT(node->templ.params[i].name));
				print_slot(env, node->templ.params[i].slot);
				if (node->templ.params[i].type) {
					printf(" type\n");
					ast_print_internal(ctx, env,
							node->templ.params[i].type, depth+3);
				} else {
					printf("\n");
				}
			}
			print_indent(depth + 1);
			printf("body:\n");
			ast_print_internal(ctx, env, node->templ.body, depth + 2);
			break;

		case AST_NODE_COMPOSITE:
			print_indent(depth);
			printf("composite (");
			print_slot(env, node->composite.ret_value);
			printf("):\n");
			for (size_t i = 0; i < node->composite.num_members; i++) {
				print_indent(depth + 1);
				printf("%.*s (", ALIT(node->composite.members[i].name));
				print_slot(env, node->composite.members[i].slot);
				printf("):\n");
				print_indent(depth + 2);
				printf("type:\n");
				if (node->composite.members[i].type) {
					ast_print_internal(ctx, env, node->composite.members[i].type, depth + 3);
				} else {
					print_indent(depth + 3);
					printf("(no type)\n");
				}
			}

			print_indent(depth + 1);
			printf("binds:\n");
			for (size_t i = 0; i < node->composite.num_binds; i++) {
				print_indent(depth + 2);
				printf("- target:\n");
				ast_print_internal(ctx, env,
						node->composite.binds[i].target, depth + 4);
				print_indent(depth + 2);
				printf("  value%s:\n",
						node->composite.binds[i].overridable
						? " (overridable)" : "");
				ast_print_internal(ctx, env,
						node->composite.binds[i].value, depth + 4);
			}

			print_indent(depth + 1);
			printf("free exprs:\n");
			for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
				ast_print_internal(ctx, env,
						node->composite.free_exprs[i], depth + 3);
			}

			ast_print_internal_closure(ctx, env,
					&node->composite.closure, depth + 1);
			break;

		case AST_NODE_VARIANT:
			print_indent(depth);
			printf("variant:\n");
			for (size_t i = 0; i < node->variant.num_variants; i++) {
				print_indent(depth + 1);
				printf("%.*s:\n", ALIT(node->variant.variants[i].name));
				print_indent(depth + 2);
				printf("type:\n");
				ast_print_internal(ctx, env,
						node->variant.variants[i].type, depth + 3);
			}
			break;

		case AST_NODE_ACCESS:
			print_indent(depth);
			printf("access '%.*s' ", ALIT(node->access.name));
			print_slot(env, node->access.slot);
			printf(": ");
			print_slot(env, ast_node_type(ctx, env, node));
			printf("\n");

			print_indent(depth + 1);
			printf("target\n");
			ast_print_internal(ctx, env,
					node->access.target, depth + 2);
			break;

		case AST_NODE_SLOT:
			print_indent(depth);
			printf("slot ");
			print_slot(env, node->slot);
			printf(": ");
			print_slot(env, ast_node_type(ctx, env, node));
			printf("\n");
			break;

		case AST_NODE_LIT:
			print_indent(depth);
			printf("lit ");
			print_slot(env, node->lit.slot);
			printf(": ");
			print_slot(env, ast_node_type(ctx, env, node));
			printf(" = ");
			print_obj_repr(ctx->vm, node->lit.obj);
			printf("\n");
			break;

		case AST_NODE_LOOKUP:
			print_indent(depth);
			printf("lookup '%.*s' ", ALIT(node->lookup.name));
			print_slot(env, node->lookup.slot);
			printf(": ");
			print_slot(env, ast_node_type(ctx, env, node));
			printf(" (");
			ast_print_name_ref(node->lookup.ref);
			printf(")\n");
			break;
	}
}

void
ast_print(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	ast_print_internal(ctx, env, node, 0);
}

void
ast_print_node(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
			printf("(");
			for (size_t i = 0; i < node->func.num_params; i++) {
				printf("%s%.*s: ", (i != 0) ? ", " : "",
						ALIT(node->func.params[i].name));
				ast_print_node(ctx, env, node->func.params[i].type);
			}
			printf(") -> ");
			ast_print_node(ctx, env, node->func.return_type);
			printf(" => ");

			if (node->kind == AST_NODE_FUNC) {
				ast_print_node(ctx, env, node->func.body);
			} else {
				printf("@native(\"%.*s\")", LIT(node->func.native.name));
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			ast_print_node(ctx, env, node->call.func);
			printf("(");
			for (size_t i = 0; i < node->call.num_args; i++) {
				if (i != 0) {
					printf(", ");
				}
				if (node->call.args[i].name) {
					printf("%.*s=", ALIT(node->call.args[i].name));
				}
				ast_print_node(ctx, env, node->call.args[i].value);
			}
			printf(")");
			break;

		case AST_NODE_FUNC_TYPE:
			printf("(");
			for (size_t i = 0; node->func_type.num_params; i++) {
				if (i != 0) {
					printf(", ");
				}
				ast_print_node(ctx, env, node->func_type.param_types[i]);
			}
			printf(") -> ");
			ast_print_node(ctx, env, node->func_type.ret_type);
			break;

		case AST_NODE_ACCESS:
			ast_print_node(ctx, env, node->access.target);
			printf(".%.*s", ALIT(node->access.name));
			break;

		case AST_NODE_TEMPL:
			printf("(");
			for (size_t i = 0; i < node->templ.num_params; i++) {
				printf("%s%.*s: ", (i != 0) ? ", " : "",
						ALIT(node->templ.params[i].name));
				if (node->type) {
					ast_print_node(ctx, env, node->templ.params[i].type);
					printf(" ");
				}
				printf("<");
				ast_print_slot(ctx, env, node->slot);
				printf(">");
			}
			printf(") ->> ?");
			printf(" => ");
			break;

		case AST_NODE_SLOT:
			printf("<");
			ast_print_slot(ctx, env, node->slot);
			printf(">");
			break;

		case AST_NODE_LIT:
			print_obj_repr(ctx->vm, node->lit.obj);
			break;

		case AST_NODE_LOOKUP:
			printf("%.*s", ALIT(node->lookup.name));
			break;

		case AST_NODE_COMPOSITE:
			printf("Struct {");
			printf(" }");
			break;

		case AST_NODE_VARIANT:
			printf("variant");
			break;
	}
}

/*
static void
ast_print_namespace(struct ast_context *ctx, struct ast_env *env,
		struct ast_namespace *ns, int indent)
{
	if (ns->num_used_objects > 0) {
		print_indent(indent);
		printf("use:\n");

		for (size_t i = 0; i < ns->num_used_objects; i++) {
			print_indent(indent + 1);
			printf("- %i\n", ns->used_objects[i]);
		}
	}

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

			case AST_MODULE_NAME_IMPORT:
				printf("import %.*s -> %i\n",
						ALIT(name->import.name),
						name->import.value);
				break;
		}
	}
}

void
ast_print_module(struct ast_context *ctx, struct ast_module *mod)
{
	ast_print_namespace(ctx, &mod->env, &mod->root, 0);
}
*/
