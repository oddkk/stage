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
		case AST_SLOT_CLOSURE:    return "CLOSURE";
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
	ctx->slots[res].member_id = -1;

	assert(
			type >= 0 ||
			type == AST_SLOT_TYPE ||
			type == AST_BIND_FAILED);

	return res;
}

static const char *
ast_bind_result_code_name(enum ast_bind_result_code code)
{
	switch (code) {
		case AST_BIND_OK:                    return "ok";
		case AST_BIND_TYPE_MISMATCH:         return "type mismatch";
		case AST_BIND_VALUE_MISMATCH:        return "value mismatch";
		case AST_BIND_TYPE_VALUE_MISMATCH:   return "type value mismatch";
		case AST_BIND_ARRAY_LENGTH_MISMATCH: return "array length mismatch";
		case AST_BIND_OBJ_HAS_NO_MEMBERS:    return "object has no members";
		case AST_BIND_TYPE_HAS_NO_MEMBERS:   return "type has no members";
		case AST_BIND_COMPILER_ERROR:        return "compiler error";
	}
	return "invalid code";
}

static ast_slot_id
ast_bind_result_to_slot(struct ast_bind_result res)
{
	if (res.code != AST_BIND_OK) {
		printf("[warning] Failed to bind slot: %s.\n",
				ast_bind_result_code_name(res.code));
		return AST_BIND_FAILED;
	}
	return res.ok.result;
}

static ast_slot_id
ast_bind_require_ok(struct ast_bind_result res)
{
	if (res.code != AST_BIND_OK) {
		panic("Failed to bind slot: %s.",
				ast_bind_result_code_name(res.code));
		return AST_BIND_FAILED;
	}
	return res.ok.result;
}

static struct ast_bind_result
ast_bind_res_as_type(struct ast_bind_result res)
{
	switch (res.code) {
		case AST_BIND_TYPE_VALUE_MISMATCH:
			res.code = AST_BIND_TYPE_MISMATCH;
			return res;

		default:
			return res;
	}
}

#define BIND_OK(res) (struct ast_bind_result){.code=AST_BIND_OK, .ok={.result=res}}

#define BIND_COMPILER_ERROR (struct ast_bind_result){.code=AST_BIND_COMPILER_ERROR}
#define BIND_VAL_MISMATCH(_old, _new) (struct ast_bind_result){\
	.code=AST_BIND_VALUE_MISMATCH, .value_mismatch={.old=_old, .new=_new}}
#define BIND_TYPE_MISMATCH(_old, _new) (struct ast_bind_result){\
	.code=AST_BIND_TYPE_MISMATCH, .type_mismatch={.old=_old, .new=_new}}
#define BIND_TYPE_VAL_MISMATCH(_old, _new) (struct ast_bind_result){\
	.code=AST_BIND_TYPE_VALUE_MISMATCH, .type_mismatch={.old=_old, .new=_new}}
#define BIND_ARRAY_LENGTH_MISMATCH(_old, _new) (struct ast_bind_result){\
	.code=AST_BIND_ARRAY_LENGTH_MISMATCH, .array_length_mismatch={.old=_old, .new=_new}}
#define BIND_OBJ_NO_MEMBERS(type) (struct ast_bind_result){\
	.code=AST_BIND_OBJ_HAS_NO_MEMBERS, .obj_no_members={.obj_type=type}}
#define BIND_TYPE_NO_MEMBERS(type) (struct ast_bind_result){\
	.code=AST_BIND_TYPE_HAS_NO_MEMBERS, .obj_no_members={.obj_type=type}}

#define BIND_EXPECT_OK(res)                           \
	do {                                              \
		struct ast_bind_result _local_result = (res); \
		if (_local_result.code != AST_BIND_OK) {      \
			return _local_result;                     \
		}                                             \
	} while (0);

#define TYPE_BIND_EXPECT_OK(res)                      \
	do {                                              \
		struct ast_bind_result _local_result = (res); \
		if (_local_result.code != AST_BIND_OK) {      \
			return ast_bind_res_as_type(_local_result);                     \
		}                                             \
	} while (0);

struct ast_bind_result
ast_try_bind_slot_error(struct ast_context *ctx,
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

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_wildcard(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		if (type == AST_BIND_NEW) {
			type = ast_bind_require_ok(
					ast_try_bind_slot_wildcard(ctx, env, AST_BIND_NEW,
					NULL, AST_SLOT_TYPE));
		}
		target = ast_alloc_slot(env, name, type, AST_SLOT_WILDCARD);
	} else {
		TYPE_BIND_EXPECT_OK(ast_try_union_slot(
					ctx, env, ast_env_slot(ctx, env, target).type, type));
	}

#if AST_DEBUG_BINDS
	printf("bind %i=wildcard ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_const(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct object obj)
{
	if (type_equals(ctx->vm, obj.type, ctx->types.type)) {
		return ast_try_bind_slot_const_type(
				ctx, env, target, name,
				// TODO: We should have a procedure to unpack type object data.
				*(type_id *)obj.data);
	}

	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name,
				ast_bind_require_ok(
					ast_try_bind_slot_const_type(
					ctx, env, AST_BIND_NEW, NULL, obj.type)),
				AST_SLOT_CONST);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				{
					// TODO: Name?
					env->slots[target].kind = AST_SLOT_CONST;

					struct ast_bind_result res;
					res = ast_try_bind_slot_const_type(ctx, env,
							target_slot.type, NULL, obj.type);
					TYPE_BIND_EXPECT_OK(res);
					env->slots[target].type = res.ok.result;
				}
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
						return BIND_TYPE_MISMATCH(target_slot.const_object.type, obj.type);
					}

					struct type *type = vm_get_type(ctx->vm, target_slot.const_object.type);

					// TODO: Use user defined comparator.
					if (memcmp(target_slot.const_object.data, obj.data, type->size) != 0) {
						printf("Warning: Attempted to bind CONST '");
						print_obj_repr(ctx->vm, obj);
						printf("' over CONST '");
						print_obj_repr(ctx->vm, target_slot.const_object);
						printf("'. (bind %i)\n", target);
						return BIND_VAL_MISMATCH(target_slot.const_object, obj);
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
						return BIND_COMPILER_ERROR;
					}

					if (!target_slot.cons.def) {
						struct ast_bind_result res;
						res = ast_try_bind_slot_cons(ctx, env, target,
								NULL, type->obj_def);
						BIND_EXPECT_OK(res);
						target = res.ok.result;
					} else {
						if (target_slot.cons.def != type->obj_def) {
							printf("Warning: Attempted to bind CONS over CONST with "
									"object that does not match the one in CONS.\n");
							return BIND_COMPILER_ERROR;
						}
					}

					struct ast_object_def *def = type->obj_def;

					for (size_t i = 0; i < def->num_params; i++) {
						struct object member;

						if (def->unpack_func) {
							// NOTE: TYPE_NONE is used for the function cons
							// params param to allow a parametric type
							// (type[$N]). This kind of parameters have to be
							// handled by the old def->unpack function and can
							// not be compiled into bytecode.
							assert(def->params[i].type != TYPE_NONE);

							struct type *param_type;
							param_type = vm_get_type(ctx->vm, def->params[i].type);

							uint8_t buffer[param_type->size];

							def->unpack_func(ctx->vm, def->data, buffer,
									obj.data, def->params[i].param_id);

							member.type = def->params[i].type;
							member.data = buffer;
							member = register_object(ctx->vm, env->store, member);
						} else {
							assert(def->unpack);
							member = def->unpack(ctx, env, def,
									def->params[i].param_id, obj);
							member = register_object(ctx->vm, env->store, member);
						}

						struct ast_bind_result res;

						ast_slot_id member_slot;
						res = ast_try_unpack_arg_named(ctx, env, target,
								AST_BIND_NEW, def->params[i].name);
						BIND_EXPECT_OK(res);
						member_slot = res.ok.result;

						ast_slot_id new_member_slot;
						res = ast_try_bind_slot_const(ctx, env, member_slot, NULL, member);
						BIND_EXPECT_OK(res);
						new_member_slot = res.ok.result;

						ast_substitute(ctx, env, new_member_slot, member_slot);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return BIND_OK(target);

			case AST_SLOT_CONS_ARRAY:
				{
					struct type *type = vm_get_type(ctx->vm, obj.type);
					struct ast_array_def *def = type->base->array_def;

					if (!def) {
						printf("Warning: Attempted to bind CONS_ARRAY over CONST with "
								"object that does not have an array constructor.\n");
						return BIND_COMPILER_ERROR;
					}


					for (size_t i = 0; i < target_slot.cons_array.num_members; i++) {
						struct object member;

						member = def->unpack(ctx, env, def, i, obj);
						member = register_object(ctx->vm, env->store, member);

						ast_slot_id member_slot;
						member_slot = target_slot.cons_array.members[i];

						ast_slot_id new_member_slot;
						struct ast_bind_result res;
						res = ast_try_bind_slot_const(ctx, env,
									member_slot,
									NULL, member);
						BIND_EXPECT_OK(res);
						new_member_slot = res.ok.result;

						ast_substitute(ctx, env, new_member_slot, member_slot);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return BIND_OK(target);

			default:
				panic("Warning: Attempted to bind CONST over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return BIND_COMPILER_ERROR;
		}
	}

	env->slots[target].const_object = obj;

#if AST_DEBUG_BINDS
	printf("bind %i=const ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_const_type(struct ast_context *ctx,
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
				{
					// TODO: Name?
					env->slots[target].kind = AST_SLOT_CONST_TYPE;

					struct ast_bind_result res;
					res = ast_try_bind_slot_const_type(ctx, env,
							env->slots[target].type, NULL, ctx->types.type);
					BIND_EXPECT_OK(res);
					env->slots[target].type = res.ok.result;
				}
				break;

			case AST_SLOT_CONST_TYPE:
				if (target_slot.const_type != type) {
					printf("Warning: Attempted to bind CONST_TYPE with type '");
					print_type_repr(ctx->vm, vm_get_type(ctx->vm, type));
					printf("' over CONST_TYPE with type '");
					print_type_repr(ctx->vm, vm_get_type(ctx->vm, target_slot.const_type));
					printf("'. (bind %i)\n", target);
					return BIND_TYPE_VAL_MISMATCH(target_slot.const_type, type);
				}
				break;

			case AST_SLOT_CONS:
				{
					struct type *type_inst = vm_get_type(ctx->vm, type);

					if (!type_inst->type_def) {
						printf("Warning: Attempted to bind CONS over CONST_TYPE with "
								"object that does not have a constructor. (bind %i)\n",
								target);
						return BIND_COMPILER_ERROR;
					}

					if (!target_slot.cons.def) {
						struct ast_bind_result res;
						res = ast_try_bind_slot_cons(ctx, env, target,
								NULL, type_inst->type_def);
						BIND_EXPECT_OK(res);
						target = res.ok.result;
					} else {
						if (target_slot.cons.def != type_inst->type_def) {
							printf("Warning: Attempted to bind CONS over CONST_TYPE with "
									"object that does not match the one in CONS.\n");
							return BIND_COMPILER_ERROR;
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

						struct ast_bind_result res;

						ast_slot_id member_slot;
						res = ast_try_unpack_arg_named(ctx, env, target,
								AST_BIND_NEW, def->params[i].name);
						BIND_EXPECT_OK(res);
						member_slot = res.ok.result;

						ast_slot_id new_member_slot;
						res = ast_try_bind_slot_const(
								ctx, env, member_slot, NULL, member);
						BIND_EXPECT_OK(res);
						new_member_slot = res.ok.result;

						ast_substitute(ctx, env, new_member_slot, member_slot);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const_type ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return BIND_OK(target);

			default:
				printf("Warning: Attempted to bind CONST_TYPE over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return BIND_COMPILER_ERROR;
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

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_param(struct ast_context *ctx,
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
				return BIND_COMPILER_ERROR;
		}
	}

	env->slots[target].param_index = param_index;

#if AST_DEBUG_BINDS
	printf("bind %i=param(index=%zu) ", target, param_index);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_templ(struct ast_context *ctx,
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
				TYPE_BIND_EXPECT_OK(ast_try_union_slot(
							ctx, env, env->slots[target].type, type));
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_SUBST:
				printf("Warning: Attempted to bind TEMPL over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return BIND_COMPILER_ERROR;

			default:
				break;
		}
	}

#if AST_DEBUG_BINDS
	printf("bind %i=templ ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_member(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name, type, AST_SLOT_MEMBER);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_MEMBER;
				TYPE_BIND_EXPECT_OK(ast_try_union_slot(
							ctx, env, env->slots[target].type, type));
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_SUBST:
				printf("Warning: Attempted to bind MEMBER over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return BIND_COMPILER_ERROR;

			default:
				break;
		}
	}

#if AST_DEBUG_BINDS
	printf("bind %i=member ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_closure(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		if (type == AST_BIND_NEW) {
			struct ast_bind_result res;
			res = ast_try_bind_slot_wildcard(ctx, env,
					AST_BIND_NEW, NULL, AST_SLOT_TYPE);
			type = res.ok.result;
		}
		target = ast_alloc_slot(env, name, type, AST_SLOT_CLOSURE);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_CLOSURE;
				TYPE_BIND_EXPECT_OK(ast_try_union_slot(
							ctx, env, env->slots[target].type, type));
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_SUBST:
				printf("Warning: Attempted to bind CLOSURE over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
				return BIND_COMPILER_ERROR;

			default:
				break;
		}
	}

#if AST_DEBUG_BINDS
	printf("bind %i=closure ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_union_context {
	struct ast_context *ctx;
	ast_slot_id *slot_map;
	size_t slot_map_len;
	bool slot_map_freeable;

	bool copy_mode;
	bool copy_member_id;
};

static struct ast_bind_result
ast_union_slot_internal(struct ast_union_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot);

struct ast_bind_result
ast_try_bind_slot_cons(struct ast_context *ctx,
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
					return BIND_COMPILER_ERROR;
				} else if (def == old_slot.cons.def) {
					return BIND_OK(target);
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
						return BIND_COMPILER_ERROR;
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

					struct ast_bind_result res;
					res = ast_try_bind_slot_cons(ctx, env, target, NULL,
							slot_obj_type->obj_def);
					BIND_EXPECT_OK(res);
					res = ast_try_bind_slot_const(ctx, env, res.ok.result, NULL,
							slot_obj);
					BIND_EXPECT_OK(res);
					target = res.ok.result;
				}
				return BIND_OK(target);

			case AST_SLOT_CONST_TYPE:
				{
					type_id slot_val = old_slot.const_type;
					struct type *slot_val_type =
						vm_get_type(ctx->vm, slot_val);

					if (!slot_val_type->type_def) {
						printf("Warning: Attempted to unpack a type that cannot be "
								"unpacked (missing def).\n");
						return BIND_TYPE_NO_MEMBERS(slot_val);
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

					struct ast_bind_result res;
					res = ast_try_bind_slot_cons(ctx, env, target, NULL,
							slot_val_type->type_def);
					BIND_EXPECT_OK(res);
					res = ast_try_bind_slot_const_type(ctx, env, res.ok.result, NULL,
							slot_val);
					BIND_EXPECT_OK(res);
					target = res.ok.result;
				}
				return BIND_OK(target);

			default:
				printf("Warning: Attempted to bind CONS over %s. (bind %i)\n",
						ast_slot_name(old_slot.kind), target);
				return BIND_COMPILER_ERROR;
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

		struct ast_bind_result res;

		res = ast_union_slot_internal(
				&cpy_ctx, env, type_slot, &def->env, def->ret_type);
		TYPE_BIND_EXPECT_OK(res);
		type_slot = res.ok.result;
	} else {
		struct ast_bind_result res;
		res = ast_try_bind_slot_wildcard(
				ctx, env, type_slot, NULL, AST_SLOT_TYPE);
		TYPE_BIND_EXPECT_OK(res);
		type_slot = res.ok.result;
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
			return BIND_COMPILER_ERROR;
		}

		// Bind the missing arguments.
		assert(env->slots[target].cons.num_present_args <= def->num_params);
		struct ast_object_arg *tmp_args =
			realloc(env->slots[target].cons.args,
					def->num_params * sizeof(struct ast_object_arg));

		if (!tmp_args) {
			panic("Failed to realloc object args.");
			return BIND_COMPILER_ERROR;
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

			struct ast_bind_result res;
			res = ast_union_slot_internal(&cpy_ctx,
						env, env->slots[target].cons.args[arg].slot,
						&def->env, def->params[param_i].slot);
			BIND_EXPECT_OK(res);
			env->slots[target].cons.args[arg].slot = res.ok.result;
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

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_cons_array(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target, struct atom *name,
		ast_slot_id *members, size_t num_members, ast_slot_id member_type_slot)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, name,
				ast_bind_require_ok(
					ast_try_bind_slot_wildcard(ctx, env,
						AST_BIND_NEW, NULL, AST_SLOT_TYPE)),
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
					printf("Warning: Attempted to bind CONS_ARRAY with length %zu "
							"over CONS_ARRAY with length %zu. (bind %i)\n",
							num_members, old_slot.cons_array.num_members, target);
					return BIND_ARRAY_LENGTH_MISMATCH(
							old_slot.cons_array.num_members, num_members);
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
						return BIND_COMPILER_ERROR;
					}

					// We first rebind the target slot to wildcard to allow us
					// to use bind_slot_cons to correctly instantiate it as a
					// cons slot. Then we apply the const object on top of the
					// cons.
					env->slots[target].kind = AST_SLOT_WILDCARD;
					env->slots[target].const_object.type = 0;
					env->slots[target].const_object.data = NULL;

					struct ast_bind_result res;

					ast_slot_id member_type;
					res = ast_try_bind_slot_const_type(
							ctx, env, AST_BIND_NEW,
							NULL, array_info->member_type);
					BIND_EXPECT_OK(res);
					member_type = res.ok.result;

					res = ast_try_bind_slot_cons_array(
							ctx, env, target, NULL, NULL,
							array_info->length, member_type);
					BIND_EXPECT_OK(res);
					res = ast_try_bind_slot_const(
							ctx, env, res.ok.result, NULL,
							slot_obj);
					BIND_EXPECT_OK(res);
					target = res.ok.result;
				}
				return BIND_OK(target);

			default:
				printf("Warning: Attempted to bind CONS_ARRAY over %s. (bind %i)\n",
						ast_slot_name(old_slot.kind), target);
				return BIND_COMPILER_ERROR;
		}

		env->slots[target].kind = AST_SLOT_CONS_ARRAY;
	}

	struct ast_bind_result res;
	res = ast_try_bind_slot_cons(
			ctx, env, env->slots[target].type, NULL,
			ctx->cons.array);
	TYPE_BIND_EXPECT_OK(res);
	env->slots[target].type = res.ok.result;

	ast_slot_id old_member_count = env->slots[target].cons_array.member_count;

	res = ast_try_unpack_arg_named(ctx, env,
				env->slots[target].type,
				AST_BIND_NEW,
				ctx->atoms.array_cons_arg_count);
	TYPE_BIND_EXPECT_OK(res);
	env->slots[target].cons_array.member_count = res.ok.result;

	assert(old_member_count == AST_BIND_NEW ||
			old_member_count == env->slots[target].cons_array.member_count);

	ast_slot_id old_member_type = env->slots[target].cons_array.member_type;
	res = ast_try_unpack_arg_named(ctx, env,
				env->slots[target].type,
				AST_BIND_NEW,
				ctx->atoms.array_cons_arg_type);
	TYPE_BIND_EXPECT_OK(res);

	env->slots[target].cons_array.member_type = res.ok.result;
	assert(old_member_type == AST_BIND_NEW || old_member_type == AST_SLOT_TYPE ||
			old_member_type == env->slots[target].cons_array.member_type);

	res = ast_try_bind_slot_const(
			ctx, env, env->slots[target].cons_array.member_count,
			NULL, ast_register_integer(ctx, env, num_members));
	BIND_EXPECT_OK(res);
	env->slots[target].cons_array.member_count = res.ok.result;

	res = ast_try_union_slot(ctx, env,
				env->slots[target].cons_array.member_type,
				member_type_slot);
	TYPE_BIND_EXPECT_OK(res);
	env->slots[target].cons_array.member_type = res.ok.result;

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
			struct ast_bind_result res;
			res = ast_try_union_slot(ctx, env,
					env->slots[target].cons_array.members[i],
					members[i]);
			BIND_EXPECT_OK(res);
			env->slots[target].cons_array.members[i] = res.ok.result;
		}
	} else {
		for (size_t i = 0; i < num_members; i++) {
			struct ast_bind_result res;
			res = ast_try_bind_slot_wildcard(ctx, env,
					env->slots[target].cons_array.members[i],
					NULL, env->slots[target].cons_array.member_type);
			BIND_EXPECT_OK(res);
			env->slots[target].cons_array.members[i] = res.ok.result;
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

	return BIND_OK(target);
}

ast_slot_id
ast_bind_slot_error(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_error(
				ctx, env, target));
}

ast_slot_id
ast_bind_slot_wildcard(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_wildcard(
				ctx, env, target, name, type));
}

ast_slot_id
ast_bind_slot_const(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct object obj)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_const(
				ctx, env, target, name, obj));
}

ast_slot_id
ast_bind_slot_const_type(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, type_id tid)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_const_type(
				ctx, env, target, name, tid));
}

ast_slot_id
ast_bind_slot_param(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, int64_t param_index, ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_param(
				ctx, env, target, name, param_index, type));
}

ast_slot_id
ast_bind_slot_templ(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_templ(
				ctx, env, target, name, type));
}

ast_slot_id
ast_bind_slot_member(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_member(
				ctx, env, target, name, type));
}

ast_slot_id
ast_bind_slot_closure(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_closure(
				ctx, env, target, name, type));
}

ast_slot_id
ast_bind_slot_cons(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct atom *name, struct ast_object_def *def)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_cons(
				ctx, env, target, name, def));
}

ast_slot_id
ast_bind_slot_cons_array(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target, struct atom *name,
		ast_slot_id *members, size_t num_members, ast_slot_id member_type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_cons_array(
				ctx, env, target, name,
				members, num_members, member_type));
}


struct ast_bind_result
ast_try_unpack_arg_named(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id obj, ast_slot_id target, struct atom *arg_name)
{
	struct ast_env_slot slot = ast_env_slot(ctx, env, obj);

	if (slot.kind == AST_SLOT_CONST ||
			slot.kind == AST_SLOT_CONST_TYPE) {
		struct ast_bind_result res;
		res = ast_try_bind_slot_cons(
				ctx, env, obj, NULL, NULL);
		BIND_EXPECT_OK(res);
		obj = res.ok.result;

		slot = ast_env_slot(ctx, env, obj);
	} else if (slot.kind != AST_SLOT_CONS) {
		return BIND_COMPILER_ERROR;
	}

	// Check that the object does not have an indirection.
	assert(env->slots[obj].kind == AST_SLOT_CONS);

	for (size_t i = 0; i < slot.cons.num_present_args; i++) {
		if (slot.cons.args[i].name == arg_name) {
			ast_slot_id res;
			res = slot.cons.args[i].slot;
			if (target != AST_BIND_NEW) {
				struct ast_bind_result bres;
				bres = ast_try_union_slot(ctx, env,
						res, target);
				BIND_EXPECT_OK(bres);
				res = bres.ok.result;
			}
			return BIND_OK(res);
		}
	}

	if (slot.cons.def) {
		// If the argument would be present in the object definition, it should
		// already have been found on the object.
		return BIND_COMPILER_ERROR;
	} else {
		struct ast_object_arg *tmp_args;
		size_t tmp_num_args = slot.cons.num_present_args + 1;
		tmp_args = realloc(slot.cons.args, tmp_num_args * sizeof(struct ast_object_arg));

		tmp_args[tmp_num_args - 1].name = arg_name;
		if (target == AST_BIND_NEW) {
			struct ast_bind_result res;
			res = ast_try_bind_slot_wildcard(
					ctx, env, AST_BIND_NEW, NULL,
					ast_bind_require_ok(
						ast_try_bind_slot_wildcard(
							ctx, env, AST_BIND_NEW,
							NULL, AST_SLOT_TYPE)));
			BIND_EXPECT_OK(res);
			tmp_args[tmp_num_args - 1].slot = res.ok.result;
		} else {
			tmp_args[tmp_num_args - 1].slot = target;
			// Make sure the type of the type is type.
			TYPE_BIND_EXPECT_OK(ast_try_bind_slot_const_type(ctx, env,
					ast_env_slot(ctx, env,
						ast_env_slot(ctx, env, target).type).type,
					NULL, ctx->types.type));
		}

		env->slots[obj].cons.num_present_args = tmp_num_args;
		env->slots[obj].cons.args = tmp_args;

		return BIND_OK(tmp_args[tmp_num_args - 1].slot);
	}
}

ast_slot_id
ast_unpack_arg_named(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id obj, ast_slot_id target, struct atom *name)
{
	return ast_bind_result_to_slot(
			ast_try_unpack_arg_named(
				ctx, env, obj, target, name));
}

static struct ast_bind_result
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
		return BIND_OK(AST_SLOT_TYPE);
	} else if (src_slot == AST_BIND_FAILED) {
#if AST_DEBUG_UNION
		printf(" -> fail\n");
#endif
		return BIND_OK(AST_BIND_FAILED);
	}

	if (dest == src && target == src_slot && !ctx->copy_mode) {
#if AST_DEBUG_UNION
		printf(" -> %i\n", target);
#endif
		return BIND_OK(target);
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
		return BIND_OK(ctx->slot_map[src_slot]);
	}

	struct ast_env_slot slot = ast_env_slot(ctx->ctx, src, src_slot);

	ast_slot_id type_target = AST_BIND_NEW;

	if (target != AST_BIND_NEW && !ctx->copy_mode) {
		struct ast_env_slot target_slot = ast_env_slot(ctx->ctx, dest, target);

		type_target = target_slot.type;
	}

	struct ast_bind_result result = BIND_COMPILER_ERROR;

	switch (slot.kind) {
		case AST_SLOT_ERROR:
#if AST_DEBUG_UNION
			printf(" -> fail\n");
#endif
			return BIND_OK(AST_BIND_FAILED);

		case AST_SLOT_WILDCARD:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_wildcard(
						ctx->ctx, dest, target, slot.name,
						res.ok.result);
			}
			break;

		case AST_SLOT_CONST_TYPE:
			result = ast_try_bind_slot_const_type(
					ctx->ctx, dest, target, slot.name, slot.const_type);
			break;

		case AST_SLOT_CONST:
			result = ast_try_bind_slot_const(
					ctx->ctx, dest, target, slot.name, slot.const_object);
			break;

		case AST_SLOT_PARAM:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_param(
						ctx->ctx, dest, target, slot.name, slot.param_index,
						res.ok.result);
			}
			break;

		case AST_SLOT_TEMPL:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_templ(
						ctx->ctx, dest, target, slot.name,
						res.ok.result);
			}
			break;

		case AST_SLOT_MEMBER:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_member(
						ctx->ctx, dest, target, slot.name,
						res.ok.result);
			}
			break;

		case AST_SLOT_CLOSURE:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_closure(
						ctx->ctx, dest, target, slot.name,
						res.ok.result);
			}
			break;

		case AST_SLOT_CONS:
			result = ast_try_bind_slot_cons(
					ctx->ctx, dest, target, slot.name,
					slot.cons.def);
			BIND_EXPECT_OK(result);

			for (size_t i = 0; i < slot.cons.num_present_args; i++) {
				ast_slot_id arg_slot =
					ast_bind_require_ok(
							ast_try_unpack_arg_named(
								ctx->ctx, dest, result.ok.result,
								AST_BIND_NEW, // TODO: slot.cons.args[i].slot,
								slot.cons.args[i].name));

				BIND_EXPECT_OK(ast_union_slot_internal(ctx,
						dest, arg_slot,
						src, slot.cons.args[i].slot));
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
						return BIND_ARRAY_LENGTH_MISMATCH(
								target_slot.cons_array.num_members, num_members);
					}

					for (size_t i = 0; i < num_members; i++) {
						members[i] = target_slot.cons_array.members[i];
					}

					member_type_slot = target_slot.cons_array.member_type;
				}
			}

			for (size_t i = 0; i < num_members; i++) {
				struct ast_bind_result res;
				res = ast_union_slot_internal(ctx,
						dest, members[i],
						src, slot.cons_array.members[i]);
				BIND_EXPECT_OK(res);
				members[i] = res.ok.result;
			}


			struct ast_bind_result res;
			res = ast_union_slot_internal(
					ctx, dest, member_type_slot,
					src, slot.cons_array.member_type);
			TYPE_BIND_EXPECT_OK(res);
			member_type_slot = res.ok.result;

			// Because unioning some members might change others, we have to
			// resolve the slots to unwrap substitutions.
			for (size_t i = 0; i < num_members; i++) {
				ast_node_resolve_slot(dest, &members[i]);
			}

			result = ast_try_bind_slot_cons_array(
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
	BIND_EXPECT_OK(result);

#if AST_DEBUG_UNION
	printf(" -> %i\n", result.ok.result);
#endif

	ctx->slot_map[src_slot] = result.ok.result;

	ast_substitute(ctx->ctx, dest, result.ok.result, target);

	if ((src == dest && !ctx->copy_mode) || ctx->copy_member_id) {
		if (slot.member_id >= 0) {
			assert(dest->slots[result.ok.result].member_id < 0);
			dest->slots[result.ok.result].member_id = slot.member_id;
		}
	}

	if (src == dest && !ctx->copy_mode) {
		ast_substitute(ctx->ctx, dest, result.ok.result, src_slot);
	}

	return result;
}

struct ast_bind_result
ast_try_union_slot(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id target, ast_slot_id src_slot)
{
	if (target == src_slot) {
		return BIND_OK(target);
	} else if (target == AST_BIND_NEW) {
		return BIND_OK(src_slot);
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
ast_union_slot(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id target, ast_slot_id src_slot)
{
	return ast_bind_result_to_slot(
			ast_try_union_slot(
				ctx, env, target, src_slot));
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

	return ast_bind_result_to_slot(
			ast_union_slot_internal(
				&cpy_ctx, dest, target, src, src_slot));
}

ast_slot_id
ast_copy_slot_with_member_id(struct ast_context *ctx,
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
	cpy_ctx.copy_member_id = true;

	return ast_bind_result_to_slot(
			ast_union_slot_internal(
				&cpy_ctx, dest, target, src, src_slot));
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
	env->slots[target].member_id = -1;
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
			case AST_SLOT_CLOSURE:
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
			.member_id = -1,
		};
	} else {
		return (struct ast_env_slot) {
			.name = NULL,
			.type = AST_BIND_FAILED,
			.kind = AST_SLOT_ERROR,
			.member_id = -1,
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
			ast_bind_result_to_slot(
				ast_union_slot_internal(&cpy_ctx,
						&out->env, AST_BIND_NEW,
						env,       slot.cons.args[i].slot));
	}

	out->ret_type =
		ast_bind_result_to_slot(
			ast_union_slot_internal(&cpy_ctx,
					&out->env, AST_BIND_NEW,
					env,       slot.type));

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
				ast_bind_result_to_slot(             \
					ast_union_slot_internal(ctx,     \
							dest_env, AST_BIND_NEW,  \
							src_env,  src->name));   \
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

	case AST_NODE_ACCESS:
		DCP_LIT(access.name);
		DCP_NODE(access.target);
		DCP_SLOT(access.slot);
		break;

	case AST_NODE_SLOT:
		DCP_SLOT(slot);
		break;

	case AST_NODE_LIT:
		DCP_LIT(lit);

	case AST_NODE_LOOKUP:
		DCP_LIT(lookup.name);
		DCP_SLOT(lookup.slot);
		DCP_LIT(lookup.ref);
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
