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
