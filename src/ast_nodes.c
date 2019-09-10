#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "base/mod.h"

ast_slot_id
ast_node_resolve_slot(struct ast_env *env, ast_slot_id *slot)
{
	assert(*slot < (ast_slot_id)env->num_slots);
	while (*slot >= 0 && env->slots[*slot].kind == AST_SLOT_SUBST) {
		assert(*slot < (ast_slot_id)env->num_slots);
		*slot = env->slots[*slot].subst;
	}

	return *slot;
}

struct ast_node *
ast_init_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct atom **param_names, size_t num_params)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_FUNC_UNINIT;
	node->loc = loc;

	node->func.params = calloc(sizeof(struct ast_func_param), num_params);
	// memcpy(node->func.params, params, sizeof(struct ast_func_param) * num_params);
	node->func.num_params = num_params;

	ast_slot_id param_types[num_params];

	for (size_t i = 0; i < num_params; i++) {
		node->func.params[i].slot = ast_bind_slot_param(
				ctx, env, AST_BIND_NEW,
				param_names[i], i,
				ast_bind_slot_wildcard(
					ctx, env, AST_BIND_NEW, NULL, AST_SLOT_TYPE));
		param_types[i] = ast_env_slot(
				ctx, env, node->func.params[i].slot).type;
	}

	node->func.type = ast_bind_slot_cons(ctx, env, AST_BIND_NEW,
			NULL, ctx->cons.func);

	node->func.return_type_slot =
		ast_unpack_arg_named(ctx, env,
			node->func.type,
			ctx->atoms.func_cons_arg_ret);

	node->func.return_type_slot =
		ast_bind_slot_wildcard(ctx, env,
				node->func.return_type_slot,
				NULL, AST_SLOT_TYPE);

	ast_slot_id param_types_slot =
		ast_unpack_arg_named(ctx, env,
			node->func.type,
			ctx->atoms.func_cons_arg_params);

	param_types_slot = ast_bind_slot_cons_array(
			ctx, env, param_types_slot, NULL,
			param_types, num_params,
			AST_SLOT_TYPE);

	return node;
}

struct ast_node *
ast_finalize_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node,
		struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct ast_node *body)
{
	assert(node != NULL && node != AST_NODE_NEW && node->kind == AST_NODE_FUNC_UNINIT);

	assert(
		node &&
		(param_types || num_params == 0) &&
		node->func.num_params == num_params &&
		body
	);

	node->kind = AST_NODE_FUNC;
	node->func.body = body;
	node->func.return_type = return_type;

	if (node->func.return_type) {
		ast_union_slot(ctx, env,
				ast_node_type(ctx, env, node->func.body),
				ast_node_value(ctx, env, node->func.return_type));
	} else {
		node->func.return_type = ast_init_node_slot(ctx, env,
				calloc(sizeof(struct ast_node), 1),
				node->func.body->loc,
				ast_node_type(ctx, env, node->func.body));

	}

	ast_union_slot(ctx, env,
			ast_node_value(ctx, env, node->func.return_type),
			ast_node_resolve_slot(env, &node->func.return_type_slot));

	for (size_t i = 0; i < num_params; i++) {
		node->func.params[i].type = param_types[i];
		ast_union_slot(ctx, env,
				ast_env_slot(ctx, env, node->func.params[i].slot).type,
				ast_node_type(ctx, env, node->func.params[i].type));
	}

	return node;
}

struct ast_node *
ast_init_node_call(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(
		node &&
		(args || num_args == 0)
	);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_CALL;
	node->loc = loc;

	node->call.func = func;
	node->call.args = calloc(sizeof(struct ast_func_arg), num_args);
	memcpy(node->call.args, args, sizeof(struct ast_func_arg) * num_args);
	node->call.num_args = num_args;

	ast_slot_id arg_type_ids[num_args];

	for (size_t i = 0; i < num_args; i++) {
		arg_type_ids[i] = ast_node_type(ctx, env, node->call.args[i].value);
	}

	ast_slot_id func_type = ast_node_type(ctx, env, node->call.func);
	func_type = ast_bind_slot_cons(ctx, env, func_type, NULL, ctx->cons.func);

	ast_slot_id ret_type = ast_unpack_arg_named(ctx, env,
				func_type, ctx->atoms.func_cons_arg_ret);

	ret_type = ast_bind_slot_wildcard(ctx, env,
			ret_type, NULL, AST_SLOT_TYPE);

	ast_slot_id param_types = ast_unpack_arg_named(ctx, env,
				func_type, ctx->atoms.func_cons_arg_params);

	param_types = ast_bind_slot_cons_array(
			ctx, env, param_types, NULL,
			arg_type_ids, num_args,
			AST_SLOT_TYPE);

	return node;
}

struct ast_node *
ast_init_node_slot(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		ast_slot_id slot)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_SLOT;
	node->loc = loc;

	node->slot = slot;

	return node;
}

struct ast_node *
ast_init_node_lookup(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct atom *name, ast_slot_id slot)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_LOOKUP;
	node->loc = loc;

	node->lookup.name = name;
	node->lookup.slot = ast_bind_slot_wildcard(
			ctx, env, slot, NULL, AST_BIND_NEW);

	return node;
}

ast_slot_id
ast_node_type(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_FUNC_UNINIT:
		case AST_NODE_FUNC:
			return ast_node_resolve_slot(env, &node->func.type);

		case AST_NODE_CALL: {
			ast_slot_id func_type;
			struct ast_env_slot func_type_slot;

			func_type = ast_node_type(ctx, env, node->call.func);
			func_type_slot = ast_env_slot(ctx, env, func_type);

			if (func_type_slot.kind != AST_SLOT_CONS) {
				printf("Warning: Expected the call target to be CONS, got %s. (slot %i)\n",
						ast_slot_name(func_type_slot.kind), func_type);
				return AST_BIND_FAILED;
			}

			if (func_type_slot.cons.def != ctx->cons.func) {
				printf("Warning: Expected the call target to be a function (%p), got %p.\n",
						(void *)ctx->cons.func, (void *)func_type_slot.cons.def);
				return AST_BIND_FAILED;
			}

			return ast_unpack_arg_named(ctx, env,
					func_type, ctx->atoms.func_cons_arg_ret);
		} break;

		case AST_NODE_SLOT:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->slot)).type;

		case AST_NODE_LOOKUP:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->lookup.slot)).type;
	}

	panic("Invalid ast node.");
	return AST_BIND_FAILED;
}

ast_slot_id
ast_node_value(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_SLOT:
			return ast_node_resolve_slot(env, &node->slot);

		case AST_NODE_LOOKUP:
			return ast_node_resolve_slot(env, &node->lookup.slot);

		case AST_NODE_FUNC:
		case AST_NODE_CALL:
			panic("TODO: eval");
			break;

		case AST_NODE_FUNC_UNINIT:
			panic("Attempted to resolve value of uninitialized func.");
			break;
	}

	panic("Invalid ast node.");
	return AST_BIND_FAILED;
}

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
