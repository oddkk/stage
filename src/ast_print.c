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
			printf("member %i[%i]", ref.member.id, ref.member.unpack_id);
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

		case AST_NAME_REF_INIT_EXPR:
			printf("init expr %i", ref.init_expr);
			break;

		case AST_NAME_REF_SELF:
			printf("self %i", ref.self_offset);
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

			print_indent(depth + 1);
			printf("uses:\n");
			for (size_t i = 0; i < node->composite.num_uses; i++) {
				print_indent(depth + 1);
				if (node->composite.uses[i].as_name) {
					printf(" - %.*s:\n", ALIT(node->composite.uses[i].as_name));
				} else {
					printf(" - *:\n");
				}
				ast_print_internal(ctx,
						node->composite.uses[i].target, depth + 3);
			}

			ast_print_internal_closure(ctx,
					&node->composite.closure, depth + 1);
			break;

		case AST_NODE_TYPE_CLASS:
			print_indent(depth);
			printf("type class:\n");
			for (size_t i = 0; i < node->type_class.pattern.num_params; i++) {
				print_indent(depth + 2);
				printf("'%.*s'", ALIT(node->type_class.pattern.params[i].name));
				if (node->type_class.pattern.params[i].type) {
					printf(" type\n");
					ast_print_internal(ctx,
							node->type_class.pattern.params[i].type, depth+4);
				} else {
					printf("\n");
				}
			}

			print_indent(depth + 1);
			printf("members:\n");
			for (size_t i = 0; i < node->type_class.num_members; i++) {
				print_indent(depth + 1);
				printf(" - %.*s:\n", ALIT(node->type_class.members[i].name));
				ast_print_internal(ctx, node->type_class.members[i].type.node, depth + 2);
			}
			// print_indent(depth + 1);
			// printf("body:\n");
			// ast_print_internal(ctx, node->type_class.pattern.node, depth + 2);
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
				ast_print_internal(ctx, node->match.cases[i].pattern.node, depth+4);
				print_indent(depth+3);
				printf("expr:\n");
				ast_print_internal(ctx, node->match.cases[i].expr, depth+4);
			}
			break;

		case AST_NODE_WILDCARD:
			print_indent(depth);
			printf("wildcard\n");
			break;

		case AST_NODE_INIT_EXPR:
			print_indent(depth);
			printf("init expr %i\n", node->init_expr.id);
			break;

		case AST_NODE_DATA_TYPE:
			print_indent(depth);
			printf("data type %i\n", node->data_type.id);
			break;
	}
}

void
ast_print(struct ast_context *ctx, struct ast_node *node)
{
	ast_print_internal(ctx, node, 0);
}

struct string
ast_node_repr(struct ast_context *ctx, struct arena *mem, struct ast_node *node,
		bool print_type_slot)
{
	struct string result = {0};
	arena_mark cp = arena_checkpoint(mem);

	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
			arena_string_append(mem, &result, STR("("));
			for (size_t i = 0; i < node->func.num_params; i++) {
				if (node->func.params[i].type) {
					arena_string_append_sprintf(mem, &result,
							"%s%.*s: ", (i != 0) ? ", " : "",
							ALIT(node->func.params[i].name));
					struct string type_repr;
					type_repr = ast_node_repr(ctx, mem,
							node->func.params[i].type, print_type_slot);
					arena_string_append(mem, &result, type_repr);
				} else {
					arena_string_append_sprintf(mem, &result,
							"%s%.*s", (i != 0) ? ", " : "",
							ALIT(node->func.params[i].name));
				}
			}
			if (node->func.return_type) {
				struct string type_repr = ast_node_repr(ctx, mem,
						node->func.return_type, print_type_slot);
				arena_string_append_sprintf(mem, &result,
						") -> %.*s", LIT(type_repr));
			} else {
				arena_string_append(mem, &result, STR(")"));
			}
			arena_string_append(mem, &result, STR(" => "));

			if (node->kind == AST_NODE_FUNC) {
				arena_string_append(mem, &result,
						ast_node_repr(ctx, mem, node->func.body, print_type_slot));
			} else {
				arena_string_append_sprintf(mem, &result,
						"@native(\"%.*s\")", LIT(node->func.native.name));
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
		case AST_NODE_INST:
			{
				struct string func_repr;
				func_repr = ast_node_repr(ctx, mem,
						node->call.func, print_type_slot);

				const char *brace = "()";
				switch (node->kind) {
					case AST_NODE_CALL: brace = "()"; break;
					case AST_NODE_CONS: brace = "[]"; break;
					case AST_NODE_INST: brace = "{}"; break;
					default: panic("Invalid node.");  break;
				}

				arena_string_append_sprintf(mem, &result, "%c", brace[0]);
				for (size_t i = 0; i < node->call.num_args; i++) {
					struct string value_repr;
					value_repr = ast_node_repr(ctx, mem,
							node->call.args[i].value, print_type_slot);

					struct string name_repr = {0};
					if (node->call.args[i].name) {
						name_repr = arena_sprintf(mem,
								"%.*s=", ALIT(node->call.args[i].name));
					}

					arena_string_append_sprintf(mem, &result,
							"%s%.*s%.*s", (i != 0) ? ", " : "",
							LIT(name_repr), LIT(value_repr));
				}
				arena_string_append_sprintf(mem, &result, "%c", brace[1]);
			}
			break;

		case AST_NODE_FUNC_TYPE:
			{
				arena_string_append(mem, &result, STR("("));
				for (size_t i = 0; i < node->func_type.num_params; i++) {
					struct string type_repr;
					type_repr = ast_node_repr(ctx, mem,
							node->func_type.param_types[i], print_type_slot);

					arena_string_append_sprintf(mem, &result,
							"%s%.*s", (i != 0) ? ", " : "",
							LIT(type_repr));
				}
				struct string ret_type_repr;
				ret_type_repr = ast_node_repr(ctx, mem,
						node->func_type.ret_type, print_type_slot);
				arena_string_append_sprintf(mem, &result,
						") -> %.*s", LIT(ret_type_repr));
			}
			break;

		case AST_NODE_ACCESS:
			result = ast_node_repr(
					ctx, mem, node->access.target, print_type_slot);
			result = arena_sprintf(mem, "%.*s.%.*s",
					LIT(result), ALIT(node->access.name));
			break;

		case AST_NODE_TEMPL:
			arena_string_append(mem, &result, STR("("));
			for (size_t i = 0; i < node->templ.pattern.num_params; i++) {
				arena_string_append_sprintf(mem, &result,
						"%s%.*s: ", (i != 0) ? ", " : "",
						ALIT(node->templ.pattern.params[i].name));
				if (node->templ.pattern.params[i].type) {
					struct string type_repr;
					type_repr = ast_node_repr(ctx, mem,
							node->templ.pattern.params[i].type, print_type_slot);
					arena_string_append_sprintf(mem, &result,
							"%.*s ", LIT(type_repr));
				}
			}
			arena_string_append(mem, &result, STR(") ->> ? => "));
			break;

		case AST_NODE_LIT:
			result = obj_repr_to_string(ctx->vm, mem, node->lit.obj);
			break;

		case AST_NODE_LIT_NATIVE:
			result = arena_sprintf(mem, "@native(\"%.*s\")", ALIT(node->lit_native.name));
			break;

		case AST_NODE_LOOKUP:
			result = arena_sprintf(mem, "%.*s", ALIT(node->lookup.name));
			break;

		case AST_NODE_MOD:
			result = arena_sprintf(mem, "mod %.*s", ALIT(node->mod.name));
			break;

		case AST_NODE_MATCH:
			{
				struct string value_repr;
				value_repr = ast_node_repr(ctx, mem,
						node->match.value, print_type_slot);
				arena_string_append_sprintf(mem, &result,
						"match %.*s { ",
						LIT(value_repr));
				for (size_t i = 0; i < node->match.num_cases; i++) {
					struct string pat_repr, expr_repr;

					pat_repr = ast_node_repr(ctx, mem,
							node->match.cases[i].pattern.node, print_type_slot);
					expr_repr = ast_node_repr(ctx, mem,
							node->match.cases[i].expr, print_type_slot);

					arena_string_append_sprintf(mem, &result,
							"%.*s => %.*s;",
							LIT(pat_repr), LIT(expr_repr));
				}
				arena_string_append(mem, &result, STR(" }"));
			}
			break;

		case AST_NODE_WILDCARD:
			result = STR("_");
			break;

		case AST_NODE_INIT_EXPR:
			result = arena_sprintf(mem, "!%i",
					node->init_expr.id);
			break;

		case AST_NODE_COMPOSITE:
			result = STR("Struct { }");
			break;

		case AST_NODE_TYPE_CLASS:
			result = STR("class { }");
			break;

		case AST_NODE_VARIANT:
			result = STR("variant");
			break;

		case AST_NODE_DATA_TYPE:
			result = arena_sprintf(mem, "data type %i",
					node->data_type.id);
			break;
	}

	if (print_type_slot) {
		result = arena_sprintf(mem, "<%i>%.*s",
				node->typecheck_slot, LIT(result));
	}

	result.text = arena_reset_and_keep(
			mem, cp, result.text, result.length);

	return result;
}

void
ast_print_node(struct ast_context *ctx, struct ast_node *node,
		bool print_type_slot)
{
	struct arena *mem = &ctx->vm->transient;
	arena_mark cp = arena_checkpoint(mem);

	struct string repr;
	repr = ast_node_repr(ctx, mem, node, print_type_slot);
	printf("%.*s", LIT(repr));

	arena_reset(mem, cp);
}
