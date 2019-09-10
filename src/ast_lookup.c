#include "ast.h"
#include <stdlib.h>

static void
ast_bind_lookup_node(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, ast_slot_id slot)
{
	assert(node->kind == AST_NODE_LOOKUP);
	node->kind = AST_NODE_SLOT;
	node->slot = ast_union_slot(ctx, env, slot, node->lookup.slot);
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

	if (scope->object != AST_SLOT_NOT_FOUND) {
		assert(ast_env_slot(ctx, env, scope->object).cons.def != NULL);

		ast_slot_id slot =
			ast_unpack_arg_named(ctx, env,
					scope->object, node->lookup.name);

		if (slot != AST_BIND_FAILED) {
			return slot;
		}
	}

	if (scope->ns) {
		for (size_t i = 0; i < scope->ns->num_names; i++) {
			struct ast_module_name *name = &scope->ns->names[i];
			if (name->name == node->lookup.name) {
				switch (name->kind) {
					case AST_MODULE_NAME_DECL:
						return name->decl.value;

					case AST_MODULE_NAME_NAMESPACE:
						return name->ns->instance;

					case AST_MODULE_NAME_IMPORT:
						return name->import.value;
				}
			}
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
	}

	for (struct ast_scope *scope = root_scope->parent_func;
			scope != NULL;
			scope = scope->parent_func) {
		ast_slot_id slot =
			ast_try_resolve_node_lookup_in_scope(
				ctx, env, scope, node);

		if (slot != AST_BIND_FAILED) {
			printf("TODO: wildcards. Found '%.*s' as slot %i.\n",
					ALIT(node->lookup.name), slot);
			// ast_bind_lookup_node(ctx, env, node, slot);
			return true;
		}
	}

	for (struct ast_scope *scope = root_scope->parent_ns;
			scope != NULL;
			scope = scope->parent_ns) {
		ast_slot_id slot =
			ast_try_resolve_node_lookup_in_scope(
				ctx, env, scope, node);

		if (slot != AST_BIND_FAILED) {
			ast_bind_lookup_node(ctx, env, node, slot);
			return true;

			/*
			struct ast_env_slot found = ast_env_slot(ctx, env, slot);
			if (found.kind == AST_SLOT_CONST ||
					found.kind == AST_SLOT_CONST_TYPE) {
				return;
			}
			printf("Lookup for '%.*s' discarded slot %i because it "
					"is not a CONST or CONST_TYPE slot.\n",
					ALIT(node->lookup.name), slot);
			*/
		}
	}

	return false;
}

void
ast_node_resolve_names(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *scope, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_FUNC_UNINIT:
			panic("Encountered uninitialized func node while resolving names.");
			break;

		case AST_NODE_FUNC:
			{
				struct ast_scope func_scope = {0};

				func_scope.parent = NULL;
				func_scope.parent_func = scope;
				func_scope.parent_ns = scope->parent_ns;

				func_scope.object = AST_SLOT_NOT_FOUND;
				func_scope.num_names = node->func.num_params;
				func_scope.names = calloc(
						func_scope.num_names, sizeof(struct ast_scope_name));

				for (size_t i = 0; i < func_scope.num_names; i++) {
					func_scope.names[i].name = node->func.params[i].name;
					func_scope.names[i].slot = node->func.params[i].slot;
				}

				for (size_t i = 0; i < func_scope.num_names; i++) {
					ast_node_resolve_names(ctx, env, scope, node->func.params[i].type);
				}

				ast_node_resolve_names(ctx, env, scope, node->func.return_type);
			}
			break;

		case AST_NODE_CALL:
			ast_node_resolve_names(ctx, env, scope, node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				ast_node_resolve_names(ctx, env, scope, node->call.args[i].value);
			}
			break;

		case AST_NODE_SLOT:
			break;

		case AST_NODE_LOOKUP:
			{
				struct atom *name = node->lookup.name;
				// NOTE: If the name is found, node will be replaced by a node
				// of kind slot.
				if (!ast_resolve_node_lookup(ctx, env, scope, node)) {
					stg_error(ctx->err, node->loc, "'%.*s' was not found.",
							ALIT(name));
				} else {
				}
			}
			break;
	}
}

static void
ast_namespace_resolve_names(struct ast_context *ctx, struct ast_module *mod,
		struct ast_scope *scope, struct ast_namespace *ns)
{
	struct ast_env *env = &mod->env;
	struct ast_scope inner_scope = {0};

	inner_scope.parent = NULL;
	inner_scope.parent_func = NULL;
	inner_scope.parent_ns = scope;

	inner_scope.object = AST_SLOT_NOT_FOUND;
	inner_scope.ns = ns;
	inner_scope.num_names = 0;

	for (size_t i = 0; i < ns->num_names; i++) {
		switch (ns->names[i].kind) {
			case AST_MODULE_NAME_DECL:
				ast_node_resolve_names(ctx, env, &inner_scope, ns->names[i].decl.expr);
				break;

			case AST_MODULE_NAME_NAMESPACE:
				ast_namespace_resolve_names(ctx, mod, &inner_scope, ns->names[i].ns);
				break;

			case AST_MODULE_NAME_IMPORT:
				{
					struct ast_module *dep = NULL;

					for (size_t dep_i = 0; dep_i < mod->num_dependencies; dep_i++) {
						if (mod->dependencies[dep_i].name == ns->names[i].import.name) {
							dep = mod->dependencies[dep_i].mod;
						}
					}

					assert(dep != NULL);
				}
				break;
		}
	}
}

void
ast_module_resolve_names(struct ast_context *ctx, struct ast_module *mod)
{
	ast_namespace_resolve_names(ctx, mod, NULL, &mod->root);
}
