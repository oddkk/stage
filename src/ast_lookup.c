#include "ast.h"
#include "native.h"
#include <stdlib.h>
#include <string.h>

void
ast_scope_push_composite(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_kind = AST_SCOPE_PARENT_LOCAL;
}

void
ast_scope_push_expr(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_kind = AST_SCOPE_PARENT_LOCAL;
}

void
ast_scope_push_func(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_kind = AST_SCOPE_PARENT_CLOSURE;
}

static void
ast_bind_lookup_node(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, ast_slot_id slot)
{
	assert(node->kind == AST_NODE_LOOKUP);
#if 0
	printf("lookup '%.*s' bind %i -> %i\n", ALIT(node->lookup.name), slot, node->lookup.slot);
	printf("  ");
	ast_print_slot(ctx, env, slot);
	printf(" -> ");
	ast_print_slot(ctx, env, node->lookup.slot);
	printf("\n");
#endif
	// node->kind = AST_NODE_SLOT;
	// node->slot = ast_union_slot(ctx, env, slot, node->lookup.slot);

	node->lookup.value = slot;
}

static ast_slot_id
ast_try_resolve_node_lookup_in_scope(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *scope, struct ast_node *node)
{
	for (size_t i = 0; i < scope->num_names; i++) {
		if (scope->names[i].name == node->lookup.name) {
			return scope->names[i].slot;
		}
	}

	return AST_BIND_FAILED;
}

static bool
ast_resolve_node_lookup(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *root_scope, struct ast_node *node)
{
	assert(node->kind == AST_NODE_LOOKUP);

	for (struct ast_scope *scope = root_scope;
			scope != NULL;
			scope = scope->parent) {
		ast_slot_id slot =
			ast_try_resolve_node_lookup_in_scope(
				ctx, env, scope, node);

		if (slot != AST_BIND_FAILED) {
			ast_bind_lookup_node(ctx, env, node, slot);
			return true;
		}

		if (scope->parent_kind == AST_SCOPE_PARENT_CLOSURE) {
			// TODO: Closures.
			break;
		}
	}

	return false;
}

int
ast_node_resolve_names(struct ast_context *ctx, struct ast_env *env,
		struct stg_native_module *native_mod,
		struct ast_scope *scope, struct ast_node *node)
{
	int err = 0;

	switch (node->kind) {
		case AST_NODE_FUNC_UNINIT:
			panic("Encountered uninitialized func node while resolving names.");
			break;

		case AST_NODE_FUNC:
			{
				struct ast_scope params_scope = {0};


				ast_scope_push_func(&params_scope, scope);

				params_scope.num_names = node->func.num_params;
				struct ast_scope_name params_scope_names[params_scope.num_names];
				params_scope.names = params_scope_names;

				for (size_t i = 0; i < params_scope.num_names; i++) {
					params_scope.names[i].name = node->func.params[i].name;
					params_scope.names[i].slot = node->func.params[i].slot;
				}

				for (size_t i = 0; i < params_scope.num_names; i++) {
					err += ast_node_resolve_names(ctx, env, native_mod,
							scope, node->func.params[i].type);
				}

				err += ast_node_resolve_names(ctx, env, native_mod,
						scope, node->func.return_type);

				err += ast_node_resolve_names(ctx, env, native_mod,
						&params_scope, node->func.body);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			{
				for (size_t i = 0; i < node->func.num_params; i++) {
					err += ast_node_resolve_names(ctx, env, native_mod,
							scope, node->func.params[i].type);
				}

				err += ast_node_resolve_names(ctx, env, native_mod,
						scope, node->func.return_type);

				if (!native_mod) {
					stg_error(ctx->err, node->loc,
							"This module does not have a native module.");
						err += 1;
				} else {
					for (size_t i = 0; i < native_mod->num_funcs; i++) {
						if (string_equal(
									native_mod->funcs[i].name,
									node->func.native.name)) {
							node->func.native.func = native_mod->funcs[i].func;
						}
					}

					if (!node->func.native.func) {
						stg_error(ctx->err, node->loc,
								"This module does not have a native function named '%.*s'.",
								LIT(node->func.native.name));
						err += 1;
					}
				}

			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			err += ast_node_resolve_names(ctx, env, native_mod,
					scope, node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += ast_node_resolve_names(ctx, env, native_mod,
						scope, node->call.args[i].value);
			}
			break;

		case AST_NODE_TEMPL:
			{
				struct ast_scope templates_scope = {0};

				ast_scope_push_func(&templates_scope, scope);

				templates_scope.num_names = node->templ.num_params;
				struct ast_scope_name template_scope_names[templates_scope.num_names];
				templates_scope.names = template_scope_names;

				for (size_t i = 0; i < templates_scope.num_names; i++) {
					templates_scope.names[i].name =
						node->templ.params[i].name;
					templates_scope.names[i].slot =
						node->templ.params[i].slot;
				}

				err += ast_node_resolve_names(ctx, env, native_mod,
						&templates_scope, node->templ.body);
			}
			break;

		case AST_NODE_SLOT:
			break;

		case AST_NODE_LIT:
			break;

		case AST_NODE_LOOKUP:
			{
				struct atom *name = node->lookup.name;
				// NOTE: If the name is found, node will be replaced by a node
				// of kind slot.
				if (!ast_resolve_node_lookup(ctx, env, scope, node)) {
					stg_error(ctx->err, node->loc, "'%.*s' was not found.",
							ALIT(name));
					err += 1;
				}
			}
			break;

		case AST_NODE_COMPOSITE:
			{
				struct ast_scope member_scope = {0};

				ast_scope_push_composite(&member_scope, scope);

				struct ast_scope_name names[node->composite.num_members];
				for (size_t i = 0; i < node->composite.num_members; i++) {
					names[i].name = node->composite.members[i].name;
					names[i].slot = ast_unpack_arg_named(
							ctx, env, node->composite.cons,
							AST_BIND_NEW, names[i].name);
				}

				member_scope.names = names;
				member_scope.num_names = node->composite.num_members;

				for (size_t i = 0; i < node->composite.num_members; i++) {
					err += ast_node_resolve_names(ctx, env, native_mod,
							&member_scope, node->composite.members[i].type);
				}

				for (size_t i = 0; i < node->composite.num_binds; i++) {
					err += ast_node_resolve_names(ctx, env, native_mod,
							&member_scope, node->composite.binds[i].target);
					err += ast_node_resolve_names(ctx, env, native_mod,
							&member_scope, node->composite.binds[i].value);
				}

				for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
					err += ast_node_resolve_names(ctx, env, native_mod,
							&member_scope, node->composite.free_exprs[i]);
				}
			}
			break;

		case AST_NODE_VARIANT:
			// TODO: Popuplate scope.
			// TODO: Do lookup.
			break;
	}

	return err;
}
