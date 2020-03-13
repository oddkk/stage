#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "vm.h"

static void
print_indent(int depth)
{
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
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

		case AST_NAME_REF_USE:
			printf("use %i[%i]", ref.use.id, ref.use.param);
			break;
	}
}

static void
ast_print_internal_closure(struct ast_context *ctx,
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
ast_print_internal(struct ast_context *ctx,
		struct ast_node *node, unsigned int depth)
{
	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
			print_indent(depth);
			printf("func\n");

			for (size_t i = 0; i < node->func.num_params; i++) {
				print_indent(depth + 1);
				printf("param %li '%.*s' type\n", i,
						ALIT(node->func.params[i].name));
				ast_print_internal(ctx, node->func.params[i].type, depth + 2);
			}

			print_indent(depth + 1);
			printf("return type\n");
			if (node->func.return_type) {
				ast_print_internal(ctx, node->func.return_type, depth + 2);
			} else {
			print_indent(depth + 2);
			printf("(none)\n");
			}

			if (node->kind == AST_NODE_FUNC) {
				print_indent(depth + 1);
				printf("body\n");
				ast_print_internal(ctx, node->func.body, depth + 2);
				ast_print_internal_closure(ctx,
						&node->func.closure, depth + 1);
			} else if (node->kind == AST_NODE_FUNC_NATIVE) {
				print_indent(depth + 1);
				printf("body native %.*s\n", LIT(node->func.native.name));
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
		case AST_NODE_INST:
			print_indent(depth);
			if (node->kind == AST_NODE_CALL) {
				printf("call:\n");
			} else if (node->kind == AST_NODE_CALL) {
				printf("cons:\n");
				print_indent(depth + 1);
				printf("cons %p\n", (void *)node->call.cons);
			} else {
				printf("inst:\n");
				print_indent(depth + 1);
				printf("inst %p, cons %p\n",
						(void *)node->call.inst,
						node->call.inst
						? (void *)node->call.inst->cons
						: NULL);
			}

			print_indent(depth + 1);
			printf("target\n");
			ast_print_internal(ctx, node->call.func, depth + 2);

			for (size_t i = 0; i < node->call.num_args; i++) {
				print_indent(depth + 1);
				printf("arg %li '%.*s' value\n", i, ALIT(node->call.args[i].name));
				ast_print_internal(ctx, node->call.args[i].value, depth + 2);
			}
			break;

		case AST_NODE_FUNC_TYPE:
			print_indent(depth);
			printf("func type:\n");
			for (size_t i = 0; i < node->func_type.num_params; i++) {
				print_indent(depth+1);
				printf("param %zu type:\n", i);
				ast_print_internal(ctx, node->func_type.param_types[i], depth+2);
			}
			print_indent(depth+1);
			printf("return type:\n");
			ast_print_internal(ctx, node->func_type.ret_type, depth+2);
			break;

		case AST_NODE_TEMPL:
			print_indent(depth);
			printf("templ:\n");
			print_indent(depth + 1);
			printf("params:\n");
			for (size_t i = 0; i < node->templ.pattern.num_params; i++) {
				print_indent(depth + 2);
				printf("'%.*s'", ALIT(node->templ.pattern.params[i].name));
				if (node->templ.pattern.params[i].type) {
					printf(" type\n");
					ast_print_internal(ctx,
							node->templ.pattern.params[i].type, depth+3);
				} else {
					printf("\n");
				}
			}
			print_indent(depth + 1);
			printf("body:\n");
			ast_print_internal(ctx, node->templ.pattern.node, depth + 2);
			break;

		case AST_NODE_COMPOSITE:
			print_indent(depth);
			printf("composite:\n");
			for (size_t i = 0; i < node->composite.num_members; i++) {
				print_indent(depth + 1);
				printf("%.*s:\n", ALIT(node->composite.members[i].name));
				print_indent(depth + 2);
				printf("type:\n");
				if (node->composite.members[i].type) {
					ast_print_internal(ctx, node->composite.members[i].type, depth + 3);
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
				ast_print_internal(ctx,
						node->composite.binds[i].target, depth + 4);
				print_indent(depth + 2);
				printf("  value%s:\n",
						node->composite.binds[i].overridable
						? " (overridable)" : "");
				ast_print_internal(ctx,
						node->composite.binds[i].value, depth + 4);
			}

			print_indent(depth + 1);
			printf("free exprs:\n");
			for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
				ast_print_internal(ctx,
						node->composite.free_exprs[i], depth + 3);
			}

			ast_print_internal_closure(ctx,
					&node->composite.closure, depth + 1);
			break;

		case AST_NODE_VARIANT:
			print_indent(depth);
			printf("variant:\n");
			for (size_t i = 0; i < node->variant.num_options; i++) {
				print_indent(depth + 1);
				printf("%.*s:\n", ALIT(node->variant.options[i].name));
				print_indent(depth + 2);
				printf("type:\n");
				if (node->variant.options[i].data_type) {
					ast_print_internal(ctx,
							node->variant.options[i].data_type, depth + 3);
				} else {
					print_indent(depth + 3);
					printf("(none)\n");
				}
			}
			break;

		case AST_NODE_ACCESS:
			print_indent(depth);
			printf("access '%.*s':\n", ALIT(node->access.name));

			print_indent(depth + 1);
			printf("target\n");
			ast_print_internal(ctx,
					node->access.target, depth + 2);
			break;

		case AST_NODE_LIT:
			print_indent(depth);
			printf("lit: ");
			print_obj_repr(ctx->vm, node->lit.obj);
			printf("\n");
			break;

		case AST_NODE_LIT_NATIVE:
			print_indent(depth);
			printf("lit native: %.*s\n", ALIT(node->lit_native.name));
			break;

		case AST_NODE_LOOKUP:
			print_indent(depth);
			printf("lookup '%.*s': (", ALIT(node->lookup.name));
			ast_print_name_ref(node->lookup.ref);
			printf(")\n");
			break;

		case AST_NODE_MOD:
			print_indent(depth);
			printf("mod '%.*s'\n", ALIT(node->mod.name));
			break;

		case AST_NODE_MATCH:
			print_indent(depth);
			printf("match:\n");
			print_indent(depth+1);
			printf("value:\n");
			ast_print_internal(ctx, node->match.value, depth+2);
			print_indent(depth+1);
			printf("cases:\n");
			for (size_t i = 0; i < node->match.num_cases; i++) {
				print_indent(depth+2);
				printf("case %zu:\n", i);
				print_indent(depth+3);
				printf("pattern:\n");
				ast_print_internal(ctx, node->match.cases[i].pattern, depth+4);
				print_indent(depth+3);
				printf("expr:\n");
				ast_print_internal(ctx, node->match.cases[i].expr, depth+4);
			}
			break;
	}
}

void
ast_print(struct ast_context *ctx, struct ast_node *node)
{
	ast_print_internal(ctx, node, 0);
}

void
ast_print_node(struct ast_context *ctx, struct ast_node *node,
		bool print_type_slot)
{
	if (print_type_slot) {
		printf("<%i>", node->typecheck_slot);
	}
	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
			printf("(");
			for (size_t i = 0; i < node->func.num_params; i++) {
				printf("%s%.*s: ", (i != 0) ? ", " : "",
						ALIT(node->func.params[i].name));
				ast_print_node(ctx, node->func.params[i].type, print_type_slot);
			}
			if (node->func.return_type) {
				printf(") -> ");
				ast_print_node(ctx, node->func.return_type, print_type_slot);
			} else {
				printf(")");
			}
			printf(" => ");

			if (node->kind == AST_NODE_FUNC) {
				ast_print_node(ctx, node->func.body, print_type_slot);
			} else {
				printf("@native(\"%.*s\")", LIT(node->func.native.name));
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
		case AST_NODE_INST:
			ast_print_node(ctx, node->call.func, print_type_slot);
			printf((node->kind == AST_NODE_CALL)
					? "("
					: ((node->kind == AST_NODE_CONS)
						? "[" : "{"));
			for (size_t i = 0; i < node->call.num_args; i++) {
				if (i != 0) {
					printf(", ");
				}
				if (node->call.args[i].name) {
					printf("%.*s=", ALIT(node->call.args[i].name));
				}
				ast_print_node(ctx, node->call.args[i].value, print_type_slot);
			}
			printf((node->kind == AST_NODE_CALL)
					? ")"
					: ((node->kind == AST_NODE_CONS)
						? "]" : "}"));
			break;

		case AST_NODE_FUNC_TYPE:
			printf("(");
			for (size_t i = 0; i < node->func_type.num_params; i++) {
				if (i != 0) {
					printf(", ");
				}
				ast_print_node(ctx, node->func_type.param_types[i], print_type_slot);
			}
			printf(") -> ");
			ast_print_node(ctx, node->func_type.ret_type, print_type_slot);
			break;

		case AST_NODE_ACCESS:
			ast_print_node(ctx, node->access.target, print_type_slot);
			printf(".%.*s", ALIT(node->access.name));
			break;

		case AST_NODE_TEMPL:
			printf("(");
			for (size_t i = 0; i < node->templ.pattern.num_params; i++) {
				printf("%s%.*s: ", (i != 0) ? ", " : "",
						ALIT(node->templ.pattern.params[i].name));
				if (node->templ.pattern.params[i].type) {
					ast_print_node(ctx, node->templ.pattern.params[i].type, print_type_slot);
					printf(" ");
				}
			}
			printf(") ->> ?");
			printf(" => ");
			break;

		case AST_NODE_LIT:
			print_obj_repr(ctx->vm, node->lit.obj);
			break;

		case AST_NODE_LIT_NATIVE:
			printf("@native(\"%.*s\")", ALIT(node->lit_native.name));
			break;

		case AST_NODE_LOOKUP:
			printf("%.*s", ALIT(node->lookup.name));
			break;

		case AST_NODE_MOD:
			printf("mod %.*s", ALIT(node->mod.name));
			break;

		case AST_NODE_MATCH:
			printf("match ");
			ast_print_node(ctx, node->match.value, print_type_slot);
			printf(" { ");
			for (size_t i = 0; i < node->match.num_cases; i++) {
				ast_print_node(ctx, node->match.cases[i].pattern, print_type_slot);
				printf(" => ");
				ast_print_node(ctx, node->match.cases[i].expr, print_type_slot);
				printf(";");
			}
			printf(" }");
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
