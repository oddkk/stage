#include "ast.h"
#include "native.h"
#include <stdlib.h>
#include <string.h>

void
ast_scope_push_namespace(struct ast_scope *target, struct ast_scope *parent,
		struct ast_namespace *ns)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_func = NULL;
	target->last_func = NULL;
	target->last_ns = target;

	target->ns = ns;
	target->object = AST_SLOT_NOT_FOUND;
}

void
ast_scope_push_expr(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_func = parent->parent_func;
	target->last_func = target;
	target->last_ns = parent->last_ns;

	target->object = AST_SLOT_NOT_FOUND;
}

void
ast_scope_push_func(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent->last_ns;
	target->parent_func = parent->last_func;
	target->last_func = target;
	target->last_ns = parent->last_ns;

	target->object = AST_SLOT_NOT_FOUND;
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

	if (scope->object != AST_SLOT_NOT_FOUND) {
		assert(ast_env_slot(ctx, env, scope->object).cons.def != NULL);

		ast_slot_id slot =
			ast_unpack_arg_named(ctx, env, scope->object,
					AST_BIND_NEW, node->lookup.name);

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

		for (size_t i = 0; i < scope->ns->num_used_objects; i++) {
			ast_slot_id obj_id = scope->ns->used_objects[i];
			assert(ast_env_slot(ctx, env, obj_id).cons.def != NULL);

			ast_slot_id slot =
				ast_unpack_arg_named(ctx, env, obj_id,
						AST_BIND_NEW, node->lookup.name);

			if (slot != AST_BIND_FAILED) {
				return slot;
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

		// TODO: Closures
		/*
		if (scope->parent_func) {
			for (struct ast_scope *func_scope = root_scope->parent_func;
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
		}
		*/
	}

	return false;
}

void
ast_node_resolve_names(struct ast_context *ctx, struct ast_env *env,
		struct stg_native_module *native_mod,
		struct ast_scope *scope, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_FUNC_UNINIT:
			panic("Encountered uninitialized func node while resolving names.");
			break;

		case AST_NODE_FUNC:
			{
				struct ast_scope templates_scope = {0};
				struct ast_scope params_scope = {0};

				ast_scope_push_func(&templates_scope, scope);

				if (node->func.num_template_params > 0) {
					templates_scope.num_names = node->func.num_template_params;
					templates_scope.names = calloc(
							templates_scope.num_names, sizeof(struct ast_scope_name));

					for (size_t i = 0; i < templates_scope.num_names; i++) {
						templates_scope.names[i].name =
							node->func.template_params[i].name;
						templates_scope.names[i].slot =
							node->func.template_params[i].slot;
					}
				}

				ast_scope_push_expr(&params_scope, &templates_scope);

				params_scope.num_names = node->func.num_params;
				params_scope.names = calloc(
						params_scope.num_names, sizeof(struct ast_scope_name));

				for (size_t i = 0; i < params_scope.num_names; i++) {
					params_scope.names[i].name = node->func.params[i].name;
					params_scope.names[i].slot = node->func.params[i].slot;
				}

				for (size_t i = 0; i < params_scope.num_names; i++) {
					ast_node_resolve_names(ctx, env, native_mod,
							&templates_scope, node->func.params[i].type);
				}

				ast_node_resolve_names(ctx, env, native_mod,
						&templates_scope, node->func.return_type);

				ast_node_resolve_names(ctx, env, native_mod,
						&params_scope, node->func.body);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			{
				struct ast_scope templates_scope = {0};

				ast_scope_push_func(&templates_scope, scope);

				if (false) {
					templates_scope.num_names = 0;
					templates_scope.names = calloc(
							templates_scope.num_names, sizeof(struct ast_scope_name));

					for (size_t i = 0; i < templates_scope.num_names; i++) {
						templates_scope.names[i].name = node->func.params[i].name;
						templates_scope.names[i].slot = node->func.params[i].slot;
					}
				}

				for (size_t i = 0; i < node->func.num_params; i++) {
					ast_node_resolve_names(ctx, env, native_mod,
							&templates_scope, node->func.params[i].type);
				}

				ast_node_resolve_names(ctx, env, native_mod,
						&templates_scope, node->func.return_type);

				if (!native_mod) {
					stg_error(ctx->err, node->loc,
							"This module does not have a native module.");
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
								"This module does not contain a native function '%.*s'.",
								LIT(node->func.native.name));
					}
				}

			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			ast_node_resolve_names(ctx, env, native_mod,
					scope, node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				ast_node_resolve_names(ctx, env, native_mod,
						scope, node->call.args[i].value);
			}
			break;

		case AST_NODE_TEMPL:
			{
				struct ast_scope templates_scope = {0};

				ast_scope_push_func(&templates_scope, scope);

				if (node->func.num_template_params > 0) {
					templates_scope.num_names = node->func.num_template_params;
					templates_scope.names = calloc(
							templates_scope.num_names, sizeof(struct ast_scope_name));

					for (size_t i = 0; i < templates_scope.num_names; i++) {
						templates_scope.names[i].name =
							node->func.template_params[i].name;
						templates_scope.names[i].slot =
							node->func.template_params[i].slot;
					}
				}

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
				}
			}
			break;

		case AST_NODE_COMPOSITE:
			// TODO: Popuplate scope.
			// TODO: Do lookup.
			break;

		case AST_NODE_VARIANT:
			// TODO: Popuplate scope.
			// TODO: Do lookup.
			break;
	}
}

static void
ast_namespace_resolve_names(struct ast_context *ctx, struct ast_module *mod,
		struct stg_native_module *native_mod,
		struct ast_scope *scope, struct ast_namespace *ns)
{
	struct ast_env *env = &mod->env;
	struct ast_scope inner_scope = {0};

	ast_scope_push_namespace(&inner_scope, scope, ns);

	for (size_t i = 0; i < ns->num_names; i++) {
		switch (ns->names[i].kind) {
			case AST_MODULE_NAME_DECL:
				ast_node_resolve_names(ctx, env, native_mod,
						&inner_scope, ns->names[i].decl.expr);
				break;

			case AST_MODULE_NAME_NAMESPACE:
				ast_namespace_resolve_names(ctx, mod, native_mod,
						&inner_scope, ns->names[i].ns);
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

	for (size_t i = 0; i < ns->num_free_exprs; i++) {
		ast_node_resolve_names(ctx, env, native_mod,
				&inner_scope, ns->free_exprs[i].expr);
	}
}

void
ast_module_resolve_names(struct ast_context *ctx, struct ast_module *mod,
		struct stg_native_module *native_mod)
{
	ast_namespace_resolve_names(ctx, mod, native_mod, NULL, &mod->root);
}
