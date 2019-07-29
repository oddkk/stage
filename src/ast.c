#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"

int
ast_object_add_arg(struct ast_object *obj, struct atom *name, ast_slot_id value)
{
	return -1;
}

ast_slot_id
ast_alloc_slot(struct ast_env *ctx, struct atom *name, ast_slot_id type, enum ast_slot_kind kind)
{
	struct ast_env_slot *new_slots;
	size_t new_num_slots;

	ast_slot_id res = ctx->num_slots;

	new_num_slots = ctx->num_slots + 1;
	new_slots = realloc(ctx->slots, sizeof(struct ast_env_slot) * new_num_slots);

	if (!new_slots) {
		panic("Failed to realloc slots.");
		return -1;
	}

	ctx->num_slots = new_num_slots;
	ctx->slots = new_slots;

	memset(&ctx->slots[res], 0, sizeof(struct ast_env_slot));

	ctx->slots[res].name = name;
	ctx->slots[res].type = type;

	return res;
}

ast_slot_id
ast_alloc_slot_wildcard(struct ast_env *ctx, struct atom *name, ast_slot_id type)
{
 	ast_slot_id slot;

 	slot = ast_alloc_slot(ctx, name, type, AST_SLOT_WILDCARD);

 	return slot;
}

ast_slot_id
ast_alloc_slot_const(struct ast_env *ctx, struct atom *name, struct object obj)
{
 	ast_slot_id slot;

 	slot = ast_alloc_slot(ctx, name, obj.type, AST_SLOT_CONST);
 	ctx->slots[slot].const_object = obj;

 	return slot;
}

ast_slot_id
ast_alloc_slot_const_type(struct ast_env *ctx, struct atom *name, type_id type)
{
 	ast_slot_id slot;

 	slot = ast_alloc_slot(ctx, name, AST_SLOT_TYPE, AST_SLOT_CONST_TYPE);
 	ctx->slots[slot].const_type = type;

 	return slot;
}

ast_slot_id
ast_alloc_slot_param(struct ast_env *ctx, struct atom *name, ast_slot_id type)
{
 	ast_slot_id slot;

 	slot = ast_alloc_slot(ctx, name, type, AST_SLOT_PARAM);

 	return slot;
}

ast_slot_id
ast_alloc_slot_free(struct ast_env *ctx, struct atom *name, ast_slot_id type)
{
 	ast_slot_id slot;

 	slot = ast_alloc_slot(ctx, name, type, AST_SLOT_FREE);

 	return slot;
}

struct ast_copy_context {
	struct ast_context *ctx;
	ast_slot_id *slot_map;
};

static ast_slot_id
ast_copy_slot_internal(struct ast_copy_context *ctx, struct ast_env *dest,
		struct ast_env *src, ast_slot_id src_slot);

ast_slot_id
ast_alloc_slot_cons(struct ast_context *ctx, struct ast_env *env,
		struct atom *name, struct ast_object_def *def)
{
 	ast_slot_id slot, ret_type_slot;

	struct ast_env *src = &def->env;
	ast_slot_id slot_map[src->num_slots];

	for (size_t i = 0; i < src->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_copy_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;

	ret_type_slot = ast_copy_slot_internal(
			&cpy_ctx, env, src, def->ret_type);

 	slot = ast_alloc_slot(env, name,
			ret_type_slot, AST_SLOT_CONS);

	struct ast_env_slot *cons_slot;

	cons_slot = &env->slots[slot];

	cons_slot->cons.def = def;
	cons_slot->cons.num_args = def->num_params;
	cons_slot->cons.args = calloc(cons_slot->cons.num_args,
			sizeof(struct ast_object_arg));

	for (size_t i = 0; i < def->num_params; i++) {
		cons_slot->cons.args[i].name = def->params[i].name;
		cons_slot->cons.args[i].slot =
			ast_alloc_slot_wildcard(
					env, def->params[i].name,
					ast_copy_slot_internal(&cpy_ctx, env,
						src, def->params[i].type));
	}

	return slot;
}

int
ast_slot_cons_add_arg(struct ast_env *env, ast_slot_id obj,
		struct atom *arg_name, ast_slot_id arg_value)
{
	return -1;
}

static ast_slot_id
ast_copy_slot_internal(struct ast_copy_context *ctx, struct ast_env *dest,
		struct ast_env *src, ast_slot_id src_slot)
{
	if (src_slot == AST_SLOT_TYPE) {
		return AST_SLOT_TYPE;
	}

	assert(src_slot >= 0 && src_slot < src->num_slots);

	if (ctx->slot_map[src_slot] != AST_SLOT_NOT_FOUND) {
		return ctx->slot_map[src_slot];
	}

	struct ast_env_slot slot = ast_env_slot(ctx->ctx, src, src_slot);

	ast_slot_id result = AST_SLOT_NOT_FOUND;

	switch (slot.kind) {
		case AST_SLOT_WILDCARD:
			result = ast_alloc_slot_wildcard(dest, slot.name,
					ast_copy_slot_internal(ctx, dest, src, slot.type));
			break;

		case AST_SLOT_CONST_TYPE:
			result = ast_alloc_slot_const_type(
					dest, slot.name, slot.const_type);
			break;

		case AST_SLOT_CONST:
			result = ast_alloc_slot_const(
					dest, slot.name, slot.const_object);
			break;

		case AST_SLOT_PARAM:
			result = ast_alloc_slot_param(
					dest, slot.name,
					ast_copy_slot_internal(ctx, dest, src, slot.type));
			break;

		case AST_SLOT_FREE:
			result = ast_alloc_slot_free(
					dest, slot.name,
					ast_copy_slot_internal(ctx, dest, src, slot.type));
			break;

		case AST_SLOT_CONS: {
			struct ast_env_slot *new_slot;

			result = ast_alloc_slot_cons(
					ctx->ctx, dest, slot.name, slot.cons.def);

			new_slot = &dest->slots[result];

			for (size_t i = 0; i < slot.cons.num_args; i++) {
				ast_slot_id new_arg_slot;

				new_arg_slot = ast_copy_slot_internal(ctx, dest,
						src, slot.cons.args[i].slot);

				ast_object_add_arg(&new_slot->cons,
						slot.cons.args[i].name, new_arg_slot);
			}
		} break;
	}
	assert(result != AST_SLOT_NOT_FOUND);

	ctx->slot_map[src_slot] = result;

	return result;
}

ast_slot_id
ast_copy_slot(struct ast_context *ctx, struct ast_env *dest,
		struct ast_env *src, ast_slot_id src_slot)
{
	ast_slot_id slot_map[src->num_slots];

	for (size_t i = 0; i < src->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_copy_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;

	return ast_copy_slot_internal(
			&cpy_ctx, dest, src, src_slot);
}

ast_slot_id
ast_env_lookup(struct ast_env *env, struct atom *name)
{
	for (ast_slot_id i = 0; i < env->num_slots; i++) {
		if (env->slots[i].name == name) {
			return i;
		}
	}

	return AST_SLOT_NOT_FOUND;
}

ast_slot_id
ast_env_lookup_or_alloc_free(struct ast_env *env, struct atom *name, ast_slot_id type)
{
	ast_slot_id slot;

	slot = ast_env_lookup(env, name);
	if (slot == AST_SLOT_NOT_FOUND) {
		slot = ast_alloc_slot_free(env, name, type);
	}

	return slot;
}

struct ast_env_slot
ast_env_slot(struct ast_context *ctx, struct ast_env *env, ast_slot_id slot)
{
	if (slot >= 0) {
		assert(slot < env->num_slots);
		return env->slots[slot];
	} else if (slot == AST_SLOT_TYPE) {
		return (struct ast_env_slot) {
			.name = ctx->atoms.type,
			.type = AST_SLOT_TYPE,
			.kind = AST_SLOT_CONST_TYPE,
			.const_type = ctx->types.type,
		};
	} else {
		panic("Lookup invalid slot.");
		return (struct ast_env_slot) {0};
	}
}

struct ast_node *
ast_init_node_func(struct ast_node *node, struct stg_location loc,
		struct ast_func_param *params, size_t num_params,
		struct ast_node *return_type, struct ast_node *body)
{
	assert(
		node &&
		(params || num_params == 0) &&
		return_type &&
		body
	);

	memset(node, 0, sizeof(struct ast_node));
	node->type = AST_NODE_FUNC;
	node->loc = loc;

	node->func.body = body;
	node->func.params = calloc(sizeof(struct ast_func_param), num_params);
	memcpy(node->func.params, params, sizeof(struct ast_func_param) * num_params);
	node->func.num_params = num_params;
	node->func.return_type = return_type;

	return node;
}

struct ast_node *
ast_init_node_call(struct ast_node *node, struct stg_location loc,
		struct ast_func_arg *args, size_t num_args)
{
	assert(
		node &&
		(args || num_args == 0)
	);

	memset(node, 0, sizeof(struct ast_node));
	node->type = AST_NODE_CALL;
	node->loc = loc;

	node->call.args = calloc(sizeof(struct ast_func_arg), num_args);
	memcpy(node->call.args, args, sizeof(struct ast_func_arg) * num_args);
	node->call.num_args = num_args;

	return node;
}

struct ast_node *
ast_init_node_slot(struct ast_node *node, struct stg_location loc,
		ast_slot_id slot)
{
	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->type = AST_NODE_CALL;
	node->loc = loc;

	node->slot = slot;

	return node;
}

