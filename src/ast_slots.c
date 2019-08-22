#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "modules/base/mod.h"

#define AST_DEBUG_BINDS 0
#define AST_DEBUG_UNION 0
#define AST_DEBUG_SUBST 0

const char *
ast_slot_name(enum ast_slot_kind kind) {
	switch (kind) {
		case AST_SLOT_ERROR:      return "ERROR";

		case AST_SLOT_WILDCARD:   return "WILDCARD";
		case AST_SLOT_CONST_TYPE: return "CONST_TYPE";
		case AST_SLOT_CONST:      return "CONST";
		case AST_SLOT_PARAM:      return "PARAM";
		case AST_SLOT_TEMPL:      return "TEMPL";
		case AST_SLOT_FREE:       return "FREE";
		case AST_SLOT_CONS:       return "CONS";
		case AST_SLOT_CONS_ARRAY: return "CONS_ARRAY";

		case AST_SLOT_SUBST:      return "SUBST";
	}

	return "(invalid)";
}

static struct object
ast_register_integer(struct ast_context *ctx, struct ast_env *env, int64_t value)
{
	struct object result = {0};

	result.type = ctx->types.integer;
	result.data = &value;

	return register_object(ctx->vm, env->store, result);
}

ssize_t
ast_object_lookup_arg(struct ast_object *obj, struct atom *arg_name)
{
	for (size_t i = 0; i < obj->def->num_params; i++) {
		if (obj->def->params[i].name == arg_name) {
			return i;
		}
	}

	return AST_SLOT_NOT_FOUND;
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
		return AST_BIND_FAILED;
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

#if AST_DEBUG_BINDS
	printf("bind %i=wildcard\n", target);
#endif

	return target;
}

ast_slot_id
ast_bind_slot_const(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct object obj)
{
	if (obj.type == ctx->types.type) {
		return ast_bind_slot_const_type(
				ctx, env, target, name,
				// TODO: We should have a procedure to unpack type object data.
				*(type_id *)obj.data);
	}

	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name,
				ast_bind_slot_const_type(
					ctx, env, AST_BIND_NEW, NULL, obj.type),
				AST_SLOT_CONST);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				// TODO: Union types
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_CONST;

				ast_bind_slot_const_type(ctx, env,
						target_slot.type, NULL, obj.type);
				break;

			case AST_SLOT_CONST: {
				if (target_slot.const_object.type != obj.type) {
					printf("Warning: Attempted to bind CONST with type '");
					print_type_repr(ctx->vm, vm_get_type(ctx->vm, obj.type));
					printf("' over CONST with type '");
					print_type_repr(ctx->vm, vm_get_type(ctx->vm, target_slot.const_object.type));
					printf("'. (bind %i)\n", target);
					return AST_BIND_FAILED;
				}

				struct type *type = vm_get_type(ctx->vm, target_slot.const_object.type);

				// TODO: Use user defined comparator.
				if (memcmp(target_slot.const_object.data, obj.data, type->size) != 0) {
					printf("Warning: Attempted to bind CONST '");
					print_obj_repr(ctx->vm, obj);
					printf("' over CONST '");
					print_obj_repr(ctx->vm, target_slot.const_object);
					printf("'. (bind %i)\n", target);
					return AST_BIND_FAILED;
				}

			} break;

			default:
				printf("Warning: Attempted to bind CONST over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return AST_BIND_FAILED;
		}
	}

	env->slots[target].const_object = obj;

#if AST_DEBUG_BINDS
	printf("bind %i=const\n", target);
#endif

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
	} else if (target >= 0 && type == ctx->types.type) {
		ast_substitute(ctx, env, target, AST_SLOT_TYPE);
		target = AST_SLOT_TYPE;
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_CONST_TYPE;

				ast_bind_slot_const_type(ctx, env,
						target_slot.type, NULL, ctx->types.type);
				break;

			case AST_SLOT_CONST_TYPE:
				if (target_slot.const_type != type) {
					printf("Warning: Attempted to bind CONST_TYPE with type '");
					print_type_repr(ctx->vm, vm_get_type(ctx->vm, type));
					printf("' over CONST_TYPE with type '");
					print_type_repr(ctx->vm, vm_get_type(ctx->vm, target_slot.const_type));
					printf("'. (bind %i)\n", target);
					return AST_BIND_FAILED;
				}
				break;

			default:
				printf("Warning: Attempted to bind CONST_TYPE over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return AST_BIND_FAILED;
		}
	}

	if (target != AST_SLOT_TYPE) {
		env->slots[target].const_type = type;
	}

#if AST_DEBUG_BINDS
	printf("bind %i=const_type(%lu)\n", target, type);
#endif

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
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				// TODO: Union types
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_PARAM;
				break;

			default:
				printf("Warning: Attempted to bind PARAM over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return AST_BIND_FAILED;
		}
	}

	env->slots[target].param_index = param_index;

#if AST_DEBUG_BINDS
	printf("bind %i=param(index=%zu)\n", target, param_index);
#endif

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
				env->slots[target].kind = AST_SLOT_TEMPL;
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_SUBST:
				printf("Warning: Attempted to bind TEMPL over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return AST_BIND_FAILED;

			default:
				break;
		}
	}

#if AST_DEBUG_BINDS
	printf("bind %i=templ\n", target);
#endif

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
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				// TODO: Union types
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_FREE;
				break;

			default:
				printf("Warning: Attempted to bind FREE over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return AST_BIND_FAILED;
		}
	}

#if AST_DEBUG_BINDS
	printf("bind %i=free\n", target);
#endif

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

	ast_slot_id type_slot = AST_BIND_NEW;

	if (target == AST_BIND_NEW) {
		type_slot = AST_BIND_NEW;
	} else {
		struct ast_env_slot old_slot = ast_env_slot(ctx, env, target);

		switch (old_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_CONS;
				env->slots[target].cons.args = NULL;
				env->slots[target].cons.def = NULL;
				type_slot = old_slot.type;
				break;

			case AST_SLOT_CONS:
				if (old_slot.cons.def != def) {
					printf("Warning: Attempted to bind CONS of %p over %p. (bind %i)\n",
							(void *)def, (void *)old_slot.cons.def, target);
					return AST_BIND_FAILED;
				}
				type_slot = old_slot.type;
				break;

			default:
				printf("Warning: Attempted to bind CONS over %s. (bind %i)\n",
						ast_slot_name(old_slot.kind), target);
				return AST_BIND_FAILED;
		}
	}

	if (num_args != def->num_params) {
		printf("Warning: Expected %zu parameters, got %zu (bind %i).\n",
				def->num_params, num_args, target);
		return AST_BIND_FAILED;
	}

	ast_slot_id slot_map[def->env.num_slots];

	for (size_t i = 0; i < def->env.num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;

	type_slot = ast_union_slot_internal(
			&cpy_ctx, env, type_slot, &def->env, def->ret_type);

	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, type_slot, AST_SLOT_CONS);
	} else {
		assert(env->slots[target].type == type_slot);
	}

	env->slots[target].cons.def = def;
	if (!env->slots[target].cons.args) {
		env->slots[target].cons.args =
			calloc(def->num_params, sizeof(ast_slot_id));
		for (size_t i = 0; i < def->num_params; i++) {
			env->slots[target].cons.args[i] = AST_BIND_NEW;
		}
	}

	for (size_t i = 0; i < def->num_params; i++) {
		env->slots[target].cons.args[i] = ast_union_slot(ctx,
				env, env->slots[target].cons.args[i],
				env, args[i].slot);
	}


	for (size_t i = 0; i < def->num_params; i++) {
		ast_slot_id arg_res = ast_union_slot_internal(
				&cpy_ctx, env,
				ast_env_slot(ctx, env,
					env->slots[target].cons.args[i]).type,
				&def->env, def->params[i].type);

		if (env->slots[target].cons.args[i] >= 0) {
			env->slots[env->slots[target].cons.args[i]].type = arg_res;
		}
	}

#if AST_DEBUG_BINDS
	{
		struct ast_env_slot *slot = &env->slots[target];
		printf("bind %i=Cons(", target);
		for (size_t j = 0; j < slot->cons.def->num_params; j++) {
			if (j != 0)
				printf(", ");
			printf("%.*s=%i",
					ALIT(slot->cons.def->params[j].name),
					slot->cons.args[j]);
		}
		printf(")\n");
	}
#endif

	return target;
}

ast_slot_id
ast_bind_slot_cons_array(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target, struct atom *name,
		ast_slot_id *members, size_t num_members, ast_slot_id member_type_slot)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, AST_BIND_NEW, AST_SLOT_CONS_ARRAY);
		env->slots[target].cons_array.member_count = AST_BIND_NEW;
		env->slots[target].cons_array.member_type = AST_BIND_NEW;
		env->slots[target].cons_array.members = NULL;
	} else {
		struct ast_env_slot old_slot = ast_env_slot(ctx, env, target);

		switch (old_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_CONS_ARRAY;
				env->slots[target].cons_array.member_count = AST_BIND_NEW;
				env->slots[target].cons_array.member_type = AST_BIND_NEW;
				env->slots[target].cons_array.members = NULL;
				break;

			case AST_SLOT_CONS_ARRAY:
				if (old_slot.cons_array.num_members != num_members) {
					printf("Warning: Attempted to bind CONS_ARRAY with length %zu"
							"over CONS_ARRAY with length %zu. (bind %i)\n",
							num_members, old_slot.cons_array.num_members, target);
					return AST_BIND_FAILED;
				}
				break;

			default:
				printf("Warning: Attempted to bind CONS_ARRAY over %s. (bind %i)\n",
						ast_slot_name(old_slot.kind), target);
				return AST_BIND_FAILED;
		}

		env->slots[target].kind = AST_SLOT_CONS_ARRAY;
	}

	env->slots[target].cons_array.member_count =
		ast_bind_slot_const(ctx, env, env->slots[target].cons_array.member_count,
				NULL, ast_register_integer(ctx, env, num_members));

	env->slots[target].cons_array.member_type =
		ast_union_slot(ctx,
				env, env->slots[target].cons_array.member_type,
				env, member_type_slot);

	if (!env->slots[target].cons_array.members) {
		env->slots[target].cons_array.num_members = num_members;
		env->slots[target].cons_array.members =
			calloc(num_members, sizeof(ast_slot_id));
		for (size_t i = 0; i < num_members; i++) {
			env->slots[target].cons_array.members[i] = AST_BIND_NEW;
		}
	}

	for (size_t i = 0; i < num_members; i++) {
		env->slots[target].cons_array.members[i] = ast_union_slot(ctx,
				env, env->slots[target].cons_array.members[i],
				env, members[i]);
	}

	struct ast_object_arg ret_array_args[] = {
		{ctx->atoms.array_cons_arg_type,  env->slots[target].cons_array.member_type},
		{ctx->atoms.array_cons_arg_count, env->slots[target].cons_array.member_count},
	};

	env->slots[target].type = ast_bind_slot_cons(
			ctx, env, env->slots[target].type, NULL,
			ctx->cons.array, ret_array_args,
			ARRAY_LENGTH(ret_array_args));

#if AST_DEBUG_BINDS
	{
		struct ast_env_slot *slot = &env->slots[target];
		printf("bind %i=[", target);
		for (size_t j = 0; j < slot->cons_array.num_members; j++) {
			if (j != 0)
				printf(", ");
			printf("%i", slot->cons_array.members[j]);
		}
		printf("] type-slot=%i count-slot=%i\n",
				slot->cons_array.member_type,
				slot->cons_array.member_count);
	}
#endif

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
#if AST_DEBUG_UNION
	printf("union (%p)%i -> (%p)%i\n", (void *)src, src_slot, (void *)dest, target);
#endif

	if (src_slot == AST_SLOT_TYPE) {
#if AST_DEBUG_UNION
		printf(" -> -1 (type)\n");
#endif
		return AST_SLOT_TYPE;
	} else if (src_slot == AST_BIND_FAILED) {
#if AST_DEBUG_UNION
		printf(" -> fail\n");
#endif
		return AST_BIND_FAILED;
	}

	if (dest == src && target == src_slot) {
#if AST_DEBUG_UNION
		printf(" -> %i\n", target);
#endif
		return target;
	}

	assert(src_slot >= 0 && src_slot < src->num_slots);

	if (ctx->slot_map[src_slot] != AST_SLOT_NOT_FOUND) {
		struct ast_env_slot mapped_slot;
		mapped_slot = ast_env_slot(ctx->ctx, dest, ctx->slot_map[src_slot]);
		while (mapped_slot.kind == AST_SLOT_SUBST) {
			ctx->slot_map[src_slot] = mapped_slot.subst;
			mapped_slot = ast_env_slot(ctx->ctx, dest, ctx->slot_map[src_slot]);
		}
#if AST_DEBUG_UNION
		printf(" -> %i (map)\n", target);
#endif
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
		case AST_SLOT_ERROR:
#if AST_DEBUG_UNION
			printf(" -> fail\n");
#endif
			return AST_BIND_FAILED;

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

			if (target != AST_BIND_NEW) {
				struct ast_env_slot target_slot;
				target_slot = ast_env_slot(ctx->ctx, dest, target);

				if (target_slot.kind == AST_SLOT_CONS) {
					for (size_t i = 0; i < num_args; i++) {
						args[i].slot = target_slot.cons.args[i];
					}
				}
			} else {
				for (size_t i = 0; i < num_args; i++) {
					args[i].slot = AST_BIND_NEW;
				}
			}

			for (size_t i = 0; i < num_args; i++) {
				args[i].name = slot.cons.def->params[i].name;
				if (src == dest) {
					args[i].slot = slot.cons.args[i];
				} else {
					args[i].slot = ast_union_slot_internal(ctx,
							dest, args[i].slot,
							src, slot.cons.args[i]);
				}
			}

			result = ast_bind_slot_cons(
					ctx->ctx, dest, target, slot.name,
					slot.cons.def, args, num_args);
		} break;

		case AST_SLOT_CONS_ARRAY: {
			size_t num_members = slot.cons_array.num_members;
			ast_slot_id members[num_members];

			if (target != AST_BIND_NEW) {
				struct ast_env_slot target_slot;
				target_slot = ast_env_slot(ctx->ctx, dest, target);

				if (target_slot.kind == AST_SLOT_CONS_ARRAY) {
					for (size_t i = 0; i < num_members; i++) {
						members[i] = target_slot.cons_array.members[i];
					}
				}
			} else {
				for (size_t i = 0; i < num_members; i++) {
					members[i] = AST_BIND_NEW;
				}
			}


			for (size_t i = 0; i < num_members; i++) {
				members[i] = AST_BIND_NEW;

				if (src == dest) {
					members[i] = slot.cons_array.members[i];
				} else {
					members[i] = ast_union_slot_internal(ctx,
							dest, members[i],
							src, slot.cons_array.members[i]);
				}
			}

			ast_slot_id member_type_slot;

			if (src == dest) {
				member_type_slot = slot.cons_array.member_type;
			} else {
				member_type_slot = ast_union_slot_internal(
						ctx, dest, AST_BIND_NEW,
						src, slot.cons_array.member_type);
			}

			result = ast_bind_slot_cons_array(
					ctx->ctx, dest, target, slot.name,
					members, num_members, member_type_slot);
		} break;

		case AST_SLOT_SUBST:
			if (src != dest) {
				printf("src:\n");
				ast_env_print(ctx->ctx->vm, src);
				printf("dest:\n");
			}
			ast_env_print(ctx->ctx->vm, dest);
			printf("failed bind (%p)%i -> (%p)%i\n", (void *)src, src_slot, (void *)dest, target);
			panic("SUBST-slot in union (%i -> %i).", src_slot, slot.subst);
			break;
	}
	assert(result != AST_SLOT_NOT_FOUND);

#if AST_DEBUG_UNION
	printf(" -> %i\n", result);
#endif

	ctx->slot_map[src_slot] = result;

	if (src == dest) {
		ast_substitute(ctx->ctx, dest, result, src_slot);
	}

	return result;
}

ast_slot_id
ast_union_slot(struct ast_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot)
{
	if (dest == src) {
		if (target == src_slot) {
			return target;
		} else if (target == AST_BIND_NEW) {
			return src_slot;
		}
	}

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

void
ast_substitute(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id new_slot, ast_slot_id target)
{
	if (target < 0) {
		return;
	}

	assert(new_slot < (int)env->num_slots);
	assert(target < env->num_slots);

	if (new_slot == target) {
		return;
	}

	assert(env->slots[target].kind != AST_SLOT_SUBST);

	memset(&env->slots[target], 0, sizeof(struct ast_env_slot));
	env->slots[target].kind = AST_SLOT_SUBST;
	env->slots[target].subst = new_slot;
	env->slots[target].type = AST_BIND_FAILED;

#if AST_DEBUG_SUBST
	printf("subst %i -> %i\n", target, new_slot);
#endif

	for (ast_slot_id i = 0; i < env->num_slots; i++) {
		struct ast_env_slot *slot;
		slot = &env->slots[i];

		if (slot->type == target) {
#if AST_DEBUG_SUBST
			printf("  (type on %i) %i -> %i\n", i, slot->type, new_slot);
#endif
			slot->type = new_slot;
		}

		switch (slot->kind) {
			case AST_SLOT_CONS:
				for (size_t arg_i = 0; arg_i < slot->cons.def->num_params; arg_i++) {
					if (slot->cons.args[arg_i] == target) {
#if AST_DEBUG_SUBST
						printf("  (cons arg on %i) %i -> %i\n", i, slot->cons.args[arg_i], new_slot);
#endif
						slot->cons.args[arg_i] = new_slot;
					}
				}
				break;

			case AST_SLOT_CONS_ARRAY:
				if (slot->cons_array.member_type == target) {
#if AST_DEBUG_SUBST
					printf("  (cons_array member_type on %i) %i -> %i\n",
							i, slot->cons_array.member_type, new_slot);
#endif
					slot->cons_array.member_type = new_slot;
				}
				if (slot->cons_array.member_count == target) {
#if AST_DEBUG_SUBST
					printf("  (cons_array member_type on %i) %i -> %i\n",
							i, slot->cons_array.member_count, new_slot);
#endif
					slot->cons_array.member_count = new_slot;
				}
				for (size_t member_i = 0; member_i < slot->cons_array.num_members; member_i++) {
					if (slot->cons_array.members[member_i] == target) {
#if AST_DEBUG_SUBST
					printf("  (cons_array member %zu on %i) %i -> %i\n", member_i,
							i, slot->cons_array.members[member_i], new_slot);
#endif
						slot->cons_array.members[member_i] = new_slot;
					}
				}
				break;

			case AST_SLOT_SUBST:
				if (slot->subst == target) {
#if AST_DEBUG_SUBST
					printf("  (subst on %i) %i -> %i\n",
							i, slot->subst, new_slot);
#endif
					slot->subst = new_slot;
				}
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_WILDCARD:
			case AST_SLOT_CONST_TYPE:
			case AST_SLOT_CONST:
			case AST_SLOT_PARAM:
			case AST_SLOT_TEMPL:
			case AST_SLOT_FREE:
				break;
		}
	}
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
		return (struct ast_env_slot) {
			.name = NULL,
			.type = AST_BIND_FAILED,
			.kind = AST_SLOT_ERROR,
		};
	}
}
