#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"

ssize_t
ast_object_lookup_arg(struct ast_object *obj, struct atom *arg_name)
{
	for (size_t i = 0; i < obj->def->num_params; i++) {
		if (obj->def->params[i].name == arg_name) {
			return i;
		}
	}

	return -1;
}

ast_slot_id
ast_alloc_slot(struct ast_env *ctx,
		struct atom *name, ast_slot_id type, enum ast_slot_kind kind)
{
	ast_slot_id res;

	struct ast_env_slot *new_slots;
	size_t new_num_slots;

	res = ctx->num_slots;

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
	ctx->slots[res].kind = kind;

	return res;
}

ast_slot_id
ast_bind_slot_wildcard(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, type, AST_SLOT_WILDCARD);
	}

	return target;
}

ast_slot_id
ast_bind_slot_const(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct object obj)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name,
				ast_bind_slot_const_type(
					ctx, env, AST_BIND_NEW, NULL, obj.type),
				AST_SLOT_CONST);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				// TODO: Union types
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_CONST;

				ast_bind_slot_const_type(ctx, env,
						target_slot.type, NULL, obj.type);
				break;

			default:
				return AST_BIND_FAILED;
		}
	}

	env->slots[target].const_object = obj;

	return target;
}

ast_slot_id
ast_bind_slot_const_type(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, type_id type)
{
	if (target == AST_BIND_NEW) {
		if (type == ctx->types.type) {
			target = AST_SLOT_TYPE;
		} else {
			target = ast_alloc_slot(env, name, AST_SLOT_TYPE, AST_SLOT_CONST_TYPE);
		}
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_CONST_TYPE;

				ast_bind_slot_const_type(ctx, env,
						target_slot.type, NULL, ctx->types.type);
				break;

			case AST_SLOT_CONST_TYPE:
				if (target_slot.const_type != type) {
					return AST_BIND_FAILED;
				}
				break;

			default:
				return AST_BIND_FAILED;
		}
	}

	if (target != AST_SLOT_TYPE) {
		env->slots[target].const_type = type;
	}

	return target;
}

ast_slot_id
ast_bind_slot_param(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, int64_t param_index, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, type, AST_SLOT_PARAM);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				// TODO: Union types
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_PARAM;
				break;

			default:
				return AST_BIND_FAILED;
		}
	}

	env->slots[target].param_index = param_index;

	return target;
}

ast_slot_id
ast_bind_slot_templ(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, type, AST_SLOT_TEMPL);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				// TODO: Union types
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_TEMPL;
				break;

			default:
				return AST_BIND_FAILED;
		}
	}

	return target;
}


ast_slot_id
ast_bind_slot_free(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, type, AST_SLOT_FREE);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				// TODO: Union types
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_FREE;
				break;

			default:
				return AST_BIND_FAILED;
		}
	}

	return target;
}

struct ast_union_context {
	struct ast_context *ctx;
	ast_slot_id *slot_map;
};

static ast_slot_id
ast_union_slot_internal(struct ast_union_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot);

ast_slot_id
ast_bind_slot_cons(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct ast_object_def *def,
		struct ast_object_arg *args, size_t num_args)
{
	if (target != AST_BIND_NEW &&
			target != AST_SLOT_TYPE &&
			env->slots[target].kind != AST_SLOT_WILDCARD) {
		return AST_BIND_FAILED;
	}

	ast_slot_id ret_type_slot = AST_BIND_NEW;

	struct ast_env *src = &def->env;

	// Copy in the return type and parameters and make sure they remain bound
	// to each other.
	ast_slot_id slot_map[src->num_slots];

	for (size_t i = 0; i < src->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;

	if (target != AST_BIND_NEW &&
			target != AST_SLOT_TYPE &&
			env->slots[target].kind == AST_SLOT_WILDCARD) {
		ret_type_slot = env->slots[target].type;
	}

	ret_type_slot = ast_union_slot_internal(
			&cpy_ctx, env, ret_type_slot, src, def->ret_type);

	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name,
				ret_type_slot, AST_SLOT_CONS);
	} else {
		// Other kinds should have been filtered out earlier.
		assert(env->slots[target].kind == AST_SLOT_WILDCARD);

		env->slots[target].kind = AST_SLOT_CONS;
	}

	struct ast_env_slot *cons_slot;

	cons_slot = &env->slots[target];

	cons_slot->cons.def = def;
	cons_slot->cons.args = calloc(def->num_params,
			sizeof(ast_slot_id));

	for (size_t i = 0; i < def->num_params; i++) {
		ssize_t arg_i = -1;
		for (size_t j = 0; j < num_args; j++) {
			if (args[j].name == def->params[i].name) {
				arg_i = j;
				break;
			}
		}

		if (arg_i >= 0) {
			cons_slot->cons.args[i] = args[arg_i].slot;
			// Bind the type of the definition to the provided argument.
			ast_union_slot_internal(&cpy_ctx, env,
					ast_env_slot(ctx, env, cons_slot->cons.args[i]).type,
					src, def->params[i].type);
		} else {
			// TODO: Somehow merge the given arguments with the existing if
			// this is a not a new target.
			cons_slot->cons.args[i] =
				ast_bind_slot_wildcard(
						ctx, env, AST_BIND_NEW, def->params[i].name,
						ast_union_slot_internal(&cpy_ctx, env, AST_BIND_NEW,
							src, def->params[i].type));
		}
	}

	return target;
}

ast_slot_id
ast_cons_arg_slot(struct ast_env *env, ast_slot_id slot,
		struct atom *arg_name)
{
	assert(slot >= 0 && slot < env->num_slots);

	struct ast_object *obj = &env->slots[slot].cons;
	ssize_t arg_i = ast_object_lookup_arg(obj, arg_name);

	if (arg_i < 0) {
		return AST_SLOT_NOT_FOUND;
	}

	return obj->args[arg_i];
}

static ast_slot_id
ast_union_slot_internal(struct ast_union_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot)
{
	if (src_slot == AST_SLOT_TYPE) {
		return AST_SLOT_TYPE;
	}

	assert(src_slot >= 0 && src_slot < src->num_slots);

	if (ctx->slot_map[src_slot] != AST_SLOT_NOT_FOUND) {
		return ctx->slot_map[src_slot];
	}

	struct ast_env_slot slot = ast_env_slot(ctx->ctx, src, src_slot);

	ast_slot_id type_target = AST_BIND_NEW;
	if (target != AST_BIND_NEW) {
		struct ast_env_slot target_slot = ast_env_slot(ctx->ctx, dest, target);

		type_target = target_slot.type;
	}

	ast_slot_id result = AST_SLOT_NOT_FOUND;

	switch (slot.kind) {
		case AST_SLOT_WILDCARD:
			result = ast_bind_slot_wildcard(
					ctx->ctx, dest, target, slot.name,
					ast_union_slot_internal(ctx, dest, type_target, src, slot.type));
			break;

		case AST_SLOT_CONST_TYPE:
			result = ast_bind_slot_const_type(
					ctx->ctx, dest, target, slot.name, slot.const_type);
			break;

		case AST_SLOT_CONST:
			result = ast_bind_slot_const(
					ctx->ctx, dest, target, slot.name, slot.const_object);
			break;

		case AST_SLOT_PARAM:
			result = ast_bind_slot_param(
					ctx->ctx, dest, target, slot.name, slot.param_index,
					ast_union_slot_internal(
						ctx, dest, type_target, src, slot.type));
			break;

		case AST_SLOT_TEMPL:
			result = ast_bind_slot_templ(
					ctx->ctx, dest, target, slot.name,
					ast_union_slot_internal(
						ctx, dest, type_target, src, slot.type));
			break;


		case AST_SLOT_FREE:
			result = ast_bind_slot_free(
					ctx->ctx, dest, target, slot.name,
					ast_union_slot_internal(ctx, dest, type_target, src, slot.type));
			break;

		case AST_SLOT_CONS: {
			size_t num_args = slot.cons.def->num_params;
			struct ast_object_arg args[num_args];

			for (size_t i = 0; i < num_args; i++) {
				args[i].name = slot.cons.def->params[i].name;
				args[i].slot = ast_union_slot_internal(ctx,
						dest, AST_BIND_NEW,
						src, slot.cons.args[i]);
			}

			result = ast_bind_slot_cons(
					ctx->ctx, dest, target, slot.name,
					slot.cons.def, args, num_args);
		} break;
	}
	assert(result != AST_SLOT_NOT_FOUND);

	ctx->slot_map[src_slot] = result;

	return result;
}

ast_slot_id
ast_union_slot(struct ast_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot)
{
	ast_slot_id slot_map[src->num_slots];

	for (size_t i = 0; i < src->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;

	return ast_union_slot_internal(
			&cpy_ctx, dest, target, src, src_slot);
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
ast_env_lookup_or_alloc_free(struct ast_context *ctx,
		struct ast_env *env, struct atom *name, ast_slot_id type)
{
	ast_slot_id slot;

	slot = ast_env_lookup(env, name);
	if (slot == AST_SLOT_NOT_FOUND) {
		slot = ast_bind_slot_free(ctx, env, AST_BIND_NEW, name, type);
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
ast_init_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
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
	node->kind = AST_NODE_FUNC;
	node->loc = loc;

	node->func.body = body;
	node->func.params = calloc(sizeof(struct ast_func_param), num_params);
	memcpy(node->func.params, params, sizeof(struct ast_func_param) * num_params);
	node->func.num_params = num_params;
	node->func.return_type = return_type;

	return node;
}

struct ast_node *
ast_init_node_call(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct ast_func_arg *args, size_t num_args)
{
	assert(
		node &&
		(args || num_args == 0)
	);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_CALL;
	node->loc = loc;

	node->call.args = calloc(sizeof(struct ast_func_arg), num_args);
	memcpy(node->call.args, args, sizeof(struct ast_func_arg) * num_args);
	node->call.num_args = num_args;

	return node;
}

struct ast_node *
ast_init_node_slot(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		ast_slot_id slot)
{
	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_SLOT;
	node->loc = loc;

	node->slot = slot;
	node->type = ast_env_slot(ctx, env, slot).type;

	return node;
}

