#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "base/mod.h"

#define AST_DEBUG_BINDS 0
#define AST_DEBUG_UNION 0
#define AST_DEBUG_SUBST 0

// If 1, slot 0 will be set to error to debug uninitialized slots.
#define AST_DEBUG_OFFSET_SLOTS 0

const char *
ast_slot_name(enum ast_slot_kind kind) {
	switch (kind) {
		case AST_SLOT_ERROR:      return "ERROR";

		case AST_SLOT_WILDCARD:   return "WILDCARD";
		case AST_SLOT_CONST_TYPE: return "CONST_TYPE";
		case AST_SLOT_CONST:      return "CONST";
		case AST_SLOT_PARAM:      return "PARAM";
		case AST_SLOT_TEMPL:      return "TEMPL";
		case AST_SLOT_MEMBER:     return "MEMBER";
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
	for (size_t i = 0; i < obj->num_present_args; i++) {
		if (obj->args[i].name == arg_name) {
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

#if AST_DEBUG_OFFSET_SLOTS
	bool is_new_env = ctx->num_slots == 0;
	if (is_new_env) {
		ctx->num_slots = 1;
	}
#endif

	res = ctx->num_slots;

	new_num_slots = ctx->num_slots + 1;
	new_slots = realloc(ctx->slots, sizeof(struct ast_env_slot) * new_num_slots);

	if (!new_slots) {
		panic("Failed to realloc slots.");
		return AST_BIND_FAILED;
	}

	ctx->num_slots = new_num_slots;
	ctx->slots = new_slots;

#if AST_DEBUG_OFFSET_SLOTS
	if (is_new_env) {
		memset(&ctx->slots[0], 0, sizeof(struct ast_env_slot));
		ctx->slots[0].kind = AST_SLOT_ERROR;
	}
#endif

	memset(&ctx->slots[res], 0, sizeof(struct ast_env_slot));

	ctx->slots[res].name = name;
	ctx->slots[res].type = type;
	ctx->slots[res].kind = kind;

	assert(
			type >= 0 ||
			type == AST_SLOT_TYPE ||
			type == AST_BIND_FAILED);

	return res;
}

ast_slot_id
ast_bind_slot_error(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, NULL, AST_BIND_FAILED, AST_SLOT_ERROR);
	} else if (target >= 0) {
		assert(target < env->num_slots);
		env->slots[target].kind = AST_SLOT_ERROR;
	} else {
		target = AST_SLOT_ERROR;
	}

	return target;
}

ast_slot_id
ast_bind_slot_wildcard(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		if (type == AST_BIND_NEW) {
			type = ast_bind_slot_wildcard(ctx, env, AST_BIND_NEW,
					NULL, AST_SLOT_TYPE);
		}
		target = ast_alloc_slot(env, name, type, AST_SLOT_WILDCARD);
	} else {
		ast_union_slot(ctx, env, ast_env_slot(ctx, env, target).type, type);
	}

#if AST_DEBUG_BINDS
	printf("bind %i=wildcard ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return target;
}

ast_slot_id
ast_bind_slot_const(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct object obj)
{
	if (type_equals(ctx->vm, obj.type, ctx->types.type)) {
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
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_CONST;

				env->slots[target].type = ast_bind_slot_const_type(ctx, env,
						target_slot.type, NULL, obj.type);
				break;

			case AST_SLOT_CONST:
				{
					if (target_slot.const_object.type != obj.type) {
						printf("Warning: Attempted to bind CONST with type '");
						print_type_repr(ctx->vm, vm_get_type(ctx->vm, obj.type));
						printf("' over CONST with type '");
						print_type_repr(ctx->vm, vm_get_type(ctx->vm,
									target_slot.const_object.type));
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
				}
				break;

			case AST_SLOT_CONS:
				{
					struct type *type = vm_get_type(ctx->vm, obj.type);

					if (!type->obj_def) {
						printf("Warning: Attempted to bind CONS over CONST with "
								"object that does not have a constructor (bind %i).\n",
								target);
						return AST_BIND_FAILED;
					}

					if (!target_slot.cons.def) {
						target = ast_bind_slot_cons(ctx, env, target,
								NULL, type->obj_def);
					} else {
						if (target_slot.cons.def != type->obj_def) {
							printf("Warning: Attempted to bind CONS over CONST with "
									"object that does not match the one in CONS.\n");
							return AST_BIND_FAILED;
						}
					}

					struct ast_object_def *def = type->obj_def;

					for (size_t i = 0; i < def->num_params; i++) {
						struct object member;

						member = def->unpack(ctx, env, def,
								def->params[i].param_id, obj);
						member = register_object(ctx->vm, env->store, member);

						ast_slot_id member_slot;
						member_slot = ast_unpack_arg_named(ctx, env, target,
								AST_BIND_NEW, def->params[i].name);

						ast_slot_id new_member_slot;
						new_member_slot =
							ast_bind_slot_const(ctx, env, member_slot, NULL, member);

						ast_substitute(ctx, env, new_member_slot, member_slot);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return target;

			case AST_SLOT_CONS_ARRAY:
				{
					struct type *type = vm_get_type(ctx->vm, obj.type);
					struct ast_array_def *def = type->base->array_def;

					if (!def) {
						printf("Warning: Attempted to bind CONS_ARRAY over CONST with "
								"object that does not have an array constructor.\n");
						return AST_BIND_FAILED;
					}


					for (size_t i = 0; i < target_slot.cons_array.num_members; i++) {
						struct object member;

						member = def->unpack(ctx, env, def, i, obj);
						member = register_object(ctx->vm, env->store, member);

						ast_slot_id member_slot;
						member_slot = target_slot.cons_array.members[i];

						ast_slot_id new_member_slot;
						new_member_slot =
							ast_bind_slot_const(ctx, env,
									member_slot,
									NULL, member);

						ast_substitute(ctx, env, new_member_slot, member_slot);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return target;

			default:
				printf("Warning: Attempted to bind CONST over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return AST_BIND_FAILED;
		}
	}

	env->slots[target].const_object = obj;

#if AST_DEBUG_BINDS
	printf("bind %i=const ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return target;
}

ast_slot_id
ast_bind_slot_const_type(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, type_id type)
{
	if (target == AST_BIND_NEW) {
		if (type_equals(ctx->vm, type, ctx->types.type)) {
			target = AST_SLOT_TYPE;
		} else {
			target = ast_alloc_slot(env, name, AST_SLOT_TYPE, AST_SLOT_CONST_TYPE);
		}
	} else if (target >= 0 && type_equals(ctx->vm, type, ctx->types.type)) {
		ast_substitute(ctx, env, target, AST_SLOT_TYPE);
#if AST_DEBUG_BINDS
		printf("=== bind const type of type, %i -> -1: \n", target);
#endif
		target = AST_SLOT_TYPE;
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_CONST_TYPE;

				env->slots[target].type = ast_bind_slot_const_type(ctx, env,
						env->slots[target].type, NULL, ctx->types.type);
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

			case AST_SLOT_CONS:
				{
					struct type *type_inst = vm_get_type(ctx->vm, type);

					if (!type_inst->type_def) {
						printf("Warning: Attempted to bind CONS over CONST_TYPE with "
								"object that does not have a constructor. (bind %i)\n",
								target);
						return AST_BIND_FAILED;
					}

					if (!target_slot.cons.def) {
						target = ast_bind_slot_cons(ctx, env, target,
								NULL, type_inst->type_def);
					} else {
						if (target_slot.cons.def != type_inst->type_def) {
							printf("Warning: Attempted to bind CONS over CONST_TYPE with "
									"object that does not match the one in CONS.\n");
							return AST_BIND_FAILED;
						}
					}

					struct ast_object_def *def = type_inst->type_def;
					struct object type_obj = {0};
					type_obj.type = ctx->types.type;
					type_obj.data = &type;

					for (size_t i = 0; i < def->num_params; i++) {
						struct object member;

						member = def->unpack(ctx, env, def,
								def->params[i].param_id, type_obj);
						member = register_object(ctx->vm, env->store, member);

						ast_slot_id member_slot;
						member_slot = ast_unpack_arg_named(ctx, env, target,
								AST_BIND_NEW, def->params[i].name);

						ast_slot_id new_member_slot;
						new_member_slot =
							ast_bind_slot_const(ctx, env, member_slot, NULL, member);

						ast_substitute(ctx, env, new_member_slot, member_slot);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const_type ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return target;

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
	printf("bind %i=const_type(%lu) ", target, type);
	ast_print_slot(ctx, env, target);
	printf("\n");
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
	printf("bind %i=param(index=%zu) ", target, param_index);
	ast_print_slot(ctx, env, target);
	printf("\n");
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
				ast_union_slot(ctx, env, env->slots[target].type, type);
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
	printf("bind %i=templ ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return target;
}

ast_slot_id
ast_bind_slot_member(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct atom *member_name, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, type, AST_SLOT_MEMBER);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_MEMBER;
				ast_union_slot(ctx, env, env->slots[target].type, type);
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_SUBST:
				printf("Warning: Attempted to bind MEMBER over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return AST_BIND_FAILED;

			default:
				break;
		}
	}

	env->slots[target].member_name = member_name;

#if AST_DEBUG_BINDS
	printf("bind %i=member(%.*s) ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return target;
}

struct ast_union_context {
	struct ast_context *ctx;
	ast_slot_id *slot_map;
	size_t slot_map_len;
	bool slot_map_freeable;

	bool copy_mode;
};

static ast_slot_id
ast_union_slot_internal(struct ast_union_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot);

ast_slot_id
ast_bind_slot_cons(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct ast_object_def *def)
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
				env->slots[target].cons.num_present_args = 0;
				env->slots[target].cons.def = NULL;
				type_slot = old_slot.type;
				break;

			case AST_SLOT_CONS:
				if (def && old_slot.cons.def && old_slot.cons.def != def) {
					printf("Warning: Attempted to bind CONS of %p over %p. (bind %i)\n",
							(void *)def, (void *)old_slot.cons.def, target);
					return AST_BIND_FAILED;
				} else if (def == old_slot.cons.def) {
					return target;
				/*
				} else if (def && env->slots[target].cons.num_present_args > def->num_params) {
					printf("Warning: Attempted to bind CONS with %zu parameters to "
							"CONS with %zu arguments. (bind %i)\n",
							def->num_params, env->slots[target].cons.num_present_args,
							target);
					return AST_BIND_FAILED;
				*/
				}
				type_slot = old_slot.type;
				break;

			case AST_SLOT_CONST:
				{
					struct object slot_obj = old_slot.const_object;
					struct type *slot_obj_type =
						vm_get_type(ctx->vm, slot_obj.type);

					if (!slot_obj_type->obj_def) {
						printf("Warning: Attempted to unpack a object that cannot be "
								"unpacked (missing def).\n");
						return AST_BIND_FAILED;
					}

					if (def && slot_obj_type->obj_def != def) {
						printf("Warning: Attempted to bind CONS with def %p over "
								"CONST with def %p.\n",
								(void *)def,
								(void *)slot_obj_type->obj_def);
					}

					// We first rebind the target slot to wildcard to allow us
					// to use bind_slot_cons to correctly instantiate it as a
					// cons slot. Then we apply the const object on top of the
					// cons.
					env->slots[target].kind = AST_SLOT_WILDCARD;
					env->slots[target].const_object.type = 0;
					env->slots[target].const_object.data = NULL;

					target = ast_bind_slot_cons(ctx, env, target, NULL,
							slot_obj_type->obj_def);
					target = ast_bind_slot_const(ctx, env, target, NULL,
							slot_obj);
				}
				return target;

			case AST_SLOT_CONST_TYPE:
				{
					type_id slot_val = old_slot.const_type;
					struct type *slot_val_type =
						vm_get_type(ctx->vm, slot_val);

					if (!slot_val_type->type_def) {
						printf("Warning: Attempted to unpack a type that cannot be "
								"unpacked (missing def).\n");
						return AST_BIND_FAILED;
					}

					if (def && slot_val_type->type_def != def) {
						printf("Warning: Attempted to bind CONS with def %p over "
								"CONST with def %p.\n",
								(void *)def,
								(void *)slot_val_type->type_def);
					}

					// We first rebind the target slot to wildcard to allow us
					// to use bind_slot_cons to correctly instantiate it as a
					// cons slot. Then we apply the const object on top of the
					// cons.
					env->slots[target].kind = AST_SLOT_WILDCARD;
					env->slots[target].const_type = 0;

					target = ast_bind_slot_cons(ctx, env, target, NULL,
							slot_val_type->type_def);
					target = ast_bind_slot_const_type(ctx, env, target, NULL,
							slot_val);
				}
				return target;

			default:
				printf("Warning: Attempted to bind CONS over %s. (bind %i)\n",
						ast_slot_name(old_slot.kind), target);
				return AST_BIND_FAILED;
		}
	}

	size_t num_map_slots = 0;

	if (def) {
		num_map_slots = def->env.num_slots;
	}

	ast_slot_id slot_map[num_map_slots];
	struct ast_union_context cpy_ctx = {0};

	if (def) {
		for (size_t i = 0; i < def->env.num_slots; i++) {
			slot_map[i] = AST_SLOT_NOT_FOUND;
		}

		cpy_ctx.ctx = ctx;
		cpy_ctx.slot_map = slot_map;
		cpy_ctx.slot_map_len = num_map_slots;
		cpy_ctx.copy_mode = true;

		type_slot = ast_union_slot_internal(
				&cpy_ctx, env, type_slot, &def->env, def->ret_type);
	} else {
		type_slot = ast_bind_slot_wildcard(
				ctx, env, type_slot, NULL, AST_SLOT_TYPE);
	}

	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, type_slot, AST_SLOT_CONS);
	} else {
		env->slots[target].type = type_slot;
	}

	env->slots[target].cons.def = def;

#define AST_OBJ_DUPLICATE_CHECK 1

	if (def) {
		bool valid_params = true;

		// Check that all arguments are valid for the given def.
		for (size_t arg_i = 0; arg_i < env->slots[target].cons.num_present_args; arg_i++) {
			bool found = false;
			struct atom *arg_name = env->slots[target].cons.args[arg_i].name;
			printf("arg '%.*s'\n", ALIT(arg_name));

			for (size_t param_i = 0; param_i < def->num_params; param_i++) {
				if (def->params[param_i].name == arg_name) {
#if AST_OBJ_DUPLICATE_CHECK
					if (found) {
						panic("Duplicate argument '%.*s' to object %p. (bind %i)",
								ALIT(arg_name), def, target);
					}
					found = true;
#else
					found = true;
					break;
#endif
				}
			}

			if (!found) {
				printf("Object of type %p got invalid argument '%.*s'. (bind %i)\n",
						(void *)def, ALIT(arg_name), target);
				valid_params = false;
			}
		}

		if (!valid_params) {
			printf("Expected ");
			for (size_t param_i = 0; param_i < def->num_params; param_i++) {
				if (param_i != 0 && param_i == def->num_params-1) {
					printf(" and ");
				} else if (param_i != 0) {
					printf(", ");
				}
				printf("%.*s", ALIT(def->params[param_i].name));
			}
			printf("\n");
			return AST_BIND_FAILED;
		}

		// Bind the missing arguments.
		assert(env->slots[target].cons.num_present_args <= def->num_params);
		struct ast_object_arg *tmp_args =
			realloc(env->slots[target].cons.args,
					def->num_params * sizeof(struct ast_object_arg));

		if (!tmp_args) {
			panic("Failed to realloc object args.");
			return AST_BIND_FAILED;
		}

		size_t num_filled_args = env->slots[target].cons.num_present_args;

		env->slots[target].cons.args = tmp_args;

		for (size_t param_i = 0; param_i < def->num_params; param_i++) {
			struct atom *param_name = def->params[param_i].name;

			bool found = false;
			size_t arg;
			for (size_t arg_i = 0; arg_i < num_filled_args; arg_i++) {
				if (env->slots[target].cons.args[arg_i].name == param_name) {
					found = true;
					arg = arg_i;
					break;
				}
			}

			if (!found) {
				arg = env->slots[target].cons.num_present_args;
				env->slots[target].cons.num_present_args += 1;
				assert(env->slots[target].cons.num_present_args <= def->num_params);

				env->slots[target].cons.args[arg].name = param_name;
				env->slots[target].cons.args[arg].slot = AST_BIND_NEW;
			}

			env->slots[target].cons.args[arg].slot =
				ast_union_slot_internal(&cpy_ctx,
						env, env->slots[target].cons.args[arg].slot,
						&def->env, def->params[param_i].slot);
		}

		assert(env->slots[target].cons.num_present_args == def->num_params);
	}

#if AST_DEBUG_BINDS
	{
		struct ast_env_slot *slot = &env->slots[target];
		printf("bind %i=Cons[%p](", target, (void *)def);
		for (size_t j = 0; j < slot->cons.num_present_args; j++) {
			if (j != 0)
				printf(", ");
			printf("%.*s=%i",
					ALIT(slot->cons.args[j].name),
					slot->cons.args[j].slot);
		}
		printf(") ");
		ast_print_slot(ctx, env, target);
		printf("\n");
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
		target = ast_alloc_slot(env, name,
				ast_bind_slot_wildcard(ctx, env,
					AST_BIND_NEW, NULL, AST_SLOT_TYPE),
				AST_SLOT_CONS_ARRAY);
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

			case AST_SLOT_CONST:
				{
					struct object slot_obj = old_slot.const_object;
					struct type *slot_obj_type =
						vm_get_type(ctx->vm, slot_obj.type);
					struct stg_array_type *array_info =
						(struct stg_array_type *)slot_obj_type->data;

					if (!slot_obj_type->base->array_def) {
						printf("Warning: Attempted to unpack an object as an "
								"array that cannot be unpacked (missing def).\n");
						return AST_BIND_FAILED;
					}

					// We first rebind the target slot to wildcard to allow us
					// to use bind_slot_cons to correctly instantiate it as a
					// cons slot. Then we apply the const object on top of the
					// cons.
					env->slots[target].kind = AST_SLOT_WILDCARD;
					env->slots[target].const_object.type = 0;
					env->slots[target].const_object.data = NULL;

					ast_slot_id member_type;
					member_type = ast_bind_slot_const_type(
							ctx, env, AST_BIND_NEW, NULL, array_info->member_type);

					target = ast_bind_slot_cons_array(ctx, env, target, NULL,
							NULL, array_info->length, member_type);
					target = ast_bind_slot_const(ctx, env, target, NULL,
							slot_obj);
				}
				return target;

			default:
				printf("Warning: Attempted to bind CONS_ARRAY over %s. (bind %i)\n",
						ast_slot_name(old_slot.kind), target);
				return AST_BIND_FAILED;
		}

		env->slots[target].kind = AST_SLOT_CONS_ARRAY;
	}

	env->slots[target].type = ast_bind_slot_cons(
			ctx, env, env->slots[target].type, NULL,
			ctx->cons.array);

	ast_slot_id old_member_count = env->slots[target].cons_array.member_count;
	env->slots[target].cons_array.member_count =
		ast_unpack_arg_named(ctx, env,
				env->slots[target].type,
				AST_BIND_NEW,
				ctx->atoms.array_cons_arg_count);
	assert(old_member_count == AST_BIND_NEW ||
			old_member_count == env->slots[target].cons_array.member_count);

	ast_slot_id old_member_type = env->slots[target].cons_array.member_type;
	env->slots[target].cons_array.member_type =
		ast_unpack_arg_named(ctx, env,
				env->slots[target].type,
				AST_BIND_NEW,
				ctx->atoms.array_cons_arg_type);
	assert(old_member_type == AST_BIND_NEW || old_member_type == AST_SLOT_TYPE ||
			old_member_type == env->slots[target].cons_array.member_type);

	env->slots[target].cons_array.member_count =
		ast_bind_slot_const(ctx, env, env->slots[target].cons_array.member_count,
				NULL, ast_register_integer(ctx, env, num_members));

	env->slots[target].cons_array.member_type =
		ast_union_slot(ctx, env,
				env->slots[target].cons_array.member_type,
				member_type_slot);

	if (!env->slots[target].cons_array.members) {
		env->slots[target].cons_array.num_members = num_members;
		env->slots[target].cons_array.members =
			calloc(num_members, sizeof(ast_slot_id));
		for (size_t i = 0; i < num_members; i++) {
			env->slots[target].cons_array.members[i] = AST_BIND_NEW;
		}
	}

	if (members) {
		for (size_t i = 0; i < num_members; i++) {
			env->slots[target].cons_array.members[i] =
				ast_union_slot(ctx, env,
					env->slots[target].cons_array.members[i],
					members[i]);
		}
	} else {
		for (size_t i = 0; i < num_members; i++) {
			env->slots[target].cons_array.members[i] =
				ast_bind_slot_wildcard(ctx, env,
						env->slots[target].cons_array.members[i],
						NULL, env->slots[target].cons_array.member_type);
		}
	}

#if AST_DEBUG_BINDS
	{
		struct ast_env_slot *slot = &env->slots[target];
		printf("bind %i=[", target);
		for (size_t j = 0; j < slot->cons_array.num_members; j++) {
			if (j != 0)
				printf(", ");
			printf("%i", slot->cons_array.members[j]);
		}
		printf("] type-slot=%i count-slot=%i ",
				slot->cons_array.member_type,
				slot->cons_array.member_count);
		ast_print_slot(ctx, env, target);
		printf("\n");
	}
#endif

	return target;
}

ast_slot_id
ast_unpack_arg_named(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id obj, ast_slot_id target, struct atom *arg_name)
{
	struct ast_env_slot slot = ast_env_slot(ctx, env, obj);

	if (slot.kind == AST_SLOT_CONST) {
		obj = ast_bind_slot_cons(ctx, env, obj, NULL, NULL);

		slot = ast_env_slot(ctx, env, obj);
	} else if (slot.kind != AST_SLOT_CONS) {
		return AST_BIND_FAILED;
	}

	// Check that the object does not have an indirection.
	assert(env->slots[obj].kind == AST_SLOT_CONS);

	for (size_t i = 0; i < slot.cons.num_present_args; i++) {
		if (slot.cons.args[i].name == arg_name) {
			ast_slot_id res;
			res = slot.cons.args[i].slot;
			if (target != AST_BIND_NEW) {
				res = ast_union_slot(ctx, env,
						res, target);
			}
			return res;
		}
	}

	if (slot.cons.def) {
		// If the argument would be present in the object definition, it should
		// already have been found on the object.
		return AST_BIND_FAILED;
	} else {
		struct ast_object_arg *tmp_args;
		size_t tmp_num_args = slot.cons.num_present_args + 1;
		tmp_args = realloc(slot.cons.args, tmp_num_args * sizeof(struct ast_object_arg));

		tmp_args[tmp_num_args - 1].name = arg_name;
		if (target == AST_BIND_NEW) {
			tmp_args[tmp_num_args - 1].slot = ast_bind_slot_wildcard(
					ctx, env, AST_BIND_NEW, NULL, ast_bind_slot_wildcard(
						ctx, env, AST_BIND_NEW, NULL, AST_SLOT_TYPE));
		} else {
			tmp_args[tmp_num_args - 1].slot = target;
			// Make sure the type of the type is type.
			ast_bind_slot_const_type(ctx, env,
					ast_env_slot(ctx, env,
						ast_env_slot(ctx, env, target).type).type,
					NULL, ctx->types.type);
		}

		env->slots[obj].cons.num_present_args = tmp_num_args;
		env->slots[obj].cons.args = tmp_args;

		return tmp_args[tmp_num_args - 1].slot;
	}
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

	if (dest == src && target == src_slot && !ctx->copy_mode) {
#if AST_DEBUG_UNION
		printf(" -> %i\n", target);
#endif
		return target;
	}

	assert(src_slot >= 0 && src_slot < src->num_slots);

	if (src_slot >= ctx->slot_map_len) {
		ast_slot_id *new_slot_map;
		if (ctx->slot_map_freeable) {
			new_slot_map = realloc(
					ctx->slot_map, src->num_slots * sizeof(ast_slot_id));
		} else {
			new_slot_map = calloc(
					src->num_slots, sizeof(ast_slot_id));
			memcpy(new_slot_map, ctx->slot_map,
					ctx->slot_map_len * sizeof(ast_slot_id));
			ctx->slot_map_freeable = true;
		}

		for (size_t i = ctx->slot_map_len; i < src->num_slots; i++) {
			new_slot_map[i] = AST_SLOT_NOT_FOUND;
		}

		ctx->slot_map = new_slot_map;
		ctx->slot_map_len = src->num_slots;
	}

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

	if (target != AST_BIND_NEW && !ctx->copy_mode) {
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

		case AST_SLOT_MEMBER:
			result = ast_bind_slot_member(
					ctx->ctx, dest, target, slot.name, slot.member_name,
					ast_union_slot_internal(
						ctx, dest, type_target, src, slot.type));
			break;


		case AST_SLOT_CONS:
			result = ast_bind_slot_cons(
					ctx->ctx, dest, target, slot.name,
					slot.cons.def);

			for (size_t i = 0; i < slot.cons.num_present_args; i++) {
				ast_slot_id arg_slot =
					ast_unpack_arg_named(ctx->ctx, dest, result,
							AST_BIND_NEW, // TODO: slot.cons.args[i].slot,
							slot.cons.args[i].name);

				ast_union_slot_internal(ctx,
						dest, arg_slot,
						src, slot.cons.args[i].slot);
			}

			// TODO: Union type?
			break;

		case AST_SLOT_CONS_ARRAY: {
			size_t num_members = slot.cons_array.num_members;
			ast_slot_id members[num_members];
			ast_slot_id member_type_slot = AST_BIND_NEW;

			for (size_t i = 0; i < num_members; i++) {
				members[i] = AST_BIND_NEW;
			}

			if (target != AST_BIND_NEW) {
				struct ast_env_slot target_slot;
				target_slot = ast_env_slot(ctx->ctx, dest, target);

				if (target_slot.kind == AST_SLOT_CONS_ARRAY) {
					if (target_slot.cons_array.num_members != num_members) {
						printf("Attempted to bind a CONS_ARRAY of length %zu with "
								"one of length %zu.\n",
								target_slot.cons_array.num_members,
								num_members);
						return AST_BIND_FAILED;
					}

					for (size_t i = 0; i < num_members; i++) {
						members[i] = target_slot.cons_array.members[i];
					}

					member_type_slot = target_slot.cons_array.member_type;
				}
			}

			for (size_t i = 0; i < num_members; i++) {
				members[i] = ast_union_slot_internal(ctx,
						dest, members[i],
						src, slot.cons_array.members[i]);
			}


			member_type_slot = ast_union_slot_internal(
					ctx, dest, member_type_slot,
					src, slot.cons_array.member_type);

			// Because unioning some members might change others, we have to
			// resolve the slots to unwrap substitutions.
			for (size_t i = 0; i < num_members; i++) {
				ast_node_resolve_slot(dest, &members[i]);
			}

			result = ast_bind_slot_cons_array(
					ctx->ctx, dest, target, slot.name,
					members, num_members, member_type_slot);
		} break;

		case AST_SLOT_SUBST:
#if 0
			if (src != dest) {
				printf("src:\n");
				ast_env_print(ctx->ctx->vm, src);
				printf("dest:\n");
			}
			ast_env_print(ctx->ctx->vm, dest);
			printf("failed bind (%p)%i -> (%p)%i\n", (void *)src, src_slot, (void *)dest, target);
			panic("SUBST-slot in union (%i(%i) -> %i).", src_slot, slot.subst, target);
#endif
			return ast_union_slot_internal(ctx,
					dest, target,
					src,  slot.subst);
	}
	assert(result != AST_SLOT_NOT_FOUND);

#if AST_DEBUG_UNION
	printf(" -> %i\n", result);
#endif

	ctx->slot_map[src_slot] = result;

	ast_substitute(ctx->ctx, dest, result, target);

	if (src == dest && !ctx->copy_mode) {
		ast_substitute(ctx->ctx, dest, result, src_slot);
	}

	return result;
}

ast_slot_id
ast_union_slot(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id target, ast_slot_id src_slot)
{
	if (target == src_slot) {
		return target;
	} else if (target == AST_BIND_NEW) {
		return src_slot;
	}

	ast_slot_id slot_map[env->num_slots];

	for (size_t i = 0; i < env->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;
	cpy_ctx.slot_map_len = env->num_slots;
	cpy_ctx.copy_mode = false;

	return ast_union_slot_internal(
			&cpy_ctx, env, target, env, src_slot);
}

ast_slot_id
ast_copy_slot(struct ast_context *ctx,
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
	cpy_ctx.slot_map_len = src->num_slots;
	cpy_ctx.copy_mode = true;

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
				for (size_t arg_i = 0; arg_i < slot->cons.num_present_args; arg_i++) {
					if (slot->cons.args[arg_i].slot == target) {
#if AST_DEBUG_SUBST
						printf("  (cons arg %zu '%.*s' on %i) %i -> %i\n",
								arg_i, ALIT(slot->cons.args[arg_i].name), i,
								slot->cons.args[arg_i].slot, new_slot);
#endif
						slot->cons.args[arg_i].slot = new_slot;
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
			case AST_SLOT_MEMBER:
				break;
		}
	}
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

bool
ast_object_def_from_cons(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *out, ast_slot_id obj)
{
	struct ast_env_slot slot;
	slot = ast_env_slot(ctx, env, obj);
	assert(slot.kind == AST_SLOT_CONS);

	ast_slot_id slot_map[env->num_slots];

	for (size_t i = 0; i < env->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;
	cpy_ctx.slot_map_len = env->num_slots;
	cpy_ctx.copy_mode = true;

	out->num_params = slot.cons.num_present_args;
	out->params = calloc(out->num_params,
			sizeof(struct ast_object_def_param));
	for (size_t i = 0; i < out->num_params; i++) {
		out->params[i].name = slot.cons.args[i].name;
		out->params[i].slot =
			ast_union_slot_internal(&cpy_ctx,
					&out->env, AST_BIND_NEW,
					env,       slot.cons.args[i].slot);
	}

	out->ret_type =
		ast_union_slot_internal(&cpy_ctx,
				&out->env, AST_BIND_NEW,
				env,       slot.type);

	return true;
}

struct ast_node *
ast_node_deep_copy_internal(
		struct ast_union_context *ctx, struct ast_env *dest_env,
		struct ast_env *src_env, struct ast_node *src)
{
	struct ast_node *result;
	result = calloc(1, sizeof(struct ast_node));

#define DCP_NODE(name)                               \
	do {                                             \
	result->name =                                   \
		ast_node_deep_copy_internal(                 \
				ctx, dest_env, src_env, src->name);  \
	} while (0);

#define DCP_SLOT(name)                               \
	do {                                             \
		if (src->name < 0) {                         \
			result->name = src->name;                \
		} else {                                     \
			result->name =                           \
				ast_union_slot_internal(ctx,         \
						dest_env, AST_BIND_NEW,      \
						src_env,  src->name);        \
		}                                            \
	} while (0);

#define DCP_LIT(name)                                \
	do { result->name = src->name; } while (0);

#define DCP_DLIST(array_name, count_name)            \
	do {                                             \
		DCP_LIT(count_name);                         \
		if ((result->count_name) > 0) {              \
			result->array_name = calloc(             \
					result->count_name,              \
					sizeof(*result->array_name));    \
		} else {                                     \
			result->array_name = NULL;               \
		}                                            \
	} while (0);

	DCP_LIT(kind);
	DCP_LIT(loc);

	switch (src->kind) {
	case AST_NODE_FUNC_UNINIT:
		panic("Got uninitialized function in copy slot.");
	case AST_NODE_FUNC_NATIVE:
	case AST_NODE_FUNC:
		if (src->kind == AST_NODE_FUNC_NATIVE) {
			DCP_LIT(func.native);
		} else {
			DCP_NODE(func.body);
		}
		DCP_DLIST(func.params, func.num_params);
		for (size_t i = 0; i < result->func.num_params; i++) {
			DCP_LIT(func.params[i].name);
			DCP_NODE(func.params[i].type);
			DCP_SLOT(func.params[i].slot);
		}

		DCP_DLIST(func.template_params, func.num_template_params);
		for (size_t i = 0; i < result->func.num_template_params; i++) {
			DCP_LIT(func.template_params[i].name);
			DCP_SLOT(func.template_params[i].slot);
			DCP_LIT(func.template_params[i].loc);
		}

		DCP_NODE(func.return_type);
		DCP_SLOT(func.return_type_slot);
		DCP_SLOT(func.param_types_slot);
		DCP_SLOT(func.type);
		DCP_LIT(func.instance);
		break;

		break;

	case AST_NODE_CONS:
		DCP_SLOT(call.cons);
		// fallthrough

	case AST_NODE_CALL:
		DCP_DLIST(call.args, call.num_args);
		for (size_t i = 0; i < result->call.num_args; i++) {
			DCP_LIT(call.args[i].name);
			DCP_NODE(call.args[i].value);
		}

		DCP_NODE(call.func);
		DCP_SLOT(call.ret_type);
		break;

	case AST_NODE_TEMPL:
		DCP_NODE(templ.body);

		DCP_DLIST(templ.params, templ.num_params);
		for (size_t i = 0; i < result->templ.num_params; i++) {
			DCP_LIT(templ.params[i].name);
			DCP_SLOT(templ.params[i].slot);
			DCP_LIT(templ.params[i].loc);
		}

		DCP_SLOT(templ.cons);
		DCP_SLOT(templ.slot);
		DCP_LIT(templ.def);
		break;

	case AST_NODE_SLOT:
		DCP_SLOT(slot);
		break;

	case AST_NODE_LOOKUP:
		DCP_LIT(lookup.name);
		DCP_SLOT(lookup.slot);
		DCP_SLOT(lookup.value);
		break;

	case AST_NODE_COMPOSITE:
		DCP_DLIST(composite.members, composite.num_members);
		for (size_t i = 0; i < result->composite.num_members; i++) {
			DCP_LIT(composite.members[i].name);
			DCP_NODE(composite.members[i].type);
		}

		DCP_DLIST(composite.binds, composite.num_binds);
		for (size_t i = 0; i < result->composite.num_binds; i++) {
			DCP_NODE(composite.binds[i].target);
			DCP_NODE(composite.binds[i].value);
			DCP_LIT(composite.binds[i].overridable);
		}

		DCP_DLIST(composite.free_exprs, composite.num_free_exprs);
		for (size_t i = 0; i < result->composite.num_free_exprs; i++) {
			DCP_NODE(composite.free_exprs[i]);
		}

		DCP_SLOT(composite.ret_value);
		break;

	case AST_NODE_VARIANT:
		DCP_DLIST(variant.variants, variant.num_variants);
		for (size_t i = 0; i < result->variant.num_variants; i++) {
			DCP_LIT(variant.variants[i].name);
			DCP_NODE(variant.variants[i].type);
		}

		DCP_SLOT(variant.ret_value);
		break;
	}

#undef DCP_NODE
#undef DCP_SLOT
#undef DCP_LIT
#undef DCP_DLIST

	return result;
}

// NOTE: This function is here instead of in ast_nodes.c because it needs
// ast_union_slot_internal to keep copy-context between copy calls.
struct ast_node *
ast_node_deep_copy(struct ast_context *ctx, struct ast_env *dest_env,
		struct ast_env *src_env, struct ast_node *src)
{
	ast_slot_id slot_map[src_env->num_slots];

	for (size_t i = 0; i < src_env->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;
	cpy_ctx.slot_map_len = src_env->num_slots;
	cpy_ctx.copy_mode = true;

	return ast_node_deep_copy_internal(
			&cpy_ctx, dest_env, src_env, src);
}
