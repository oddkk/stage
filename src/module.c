#include "module.h"
#include "string.h"
#include "base/mod.h"
#include <stdlib.h>
#include <string.h>

struct atom *
mod_atom(struct stg_module *mod, struct string name)
{
	return atom_create(&mod->vm->atom_table, name);
}

type_id
stg_register_type(struct stg_module *mod, struct type t)
{
	modtype_id local_tid;
	local_tid = register_type(&mod->store, t);

	type_id tid;
	tid = TYPE_ID(mod->id, local_tid);

	return tid;
}

static struct object
simple_obj_def_pack(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, ast_slot_id obj)
{
	struct object_decons *decons = def->data;
	struct object ret_type_obj;
	int err;

	struct ast_env_slot obj_slot = ast_env_slot(ctx, env, obj);
	assert(obj_slot.kind == AST_SLOT_CONS);
	assert(obj_slot.cons.def == def);

	err = ast_slot_pack(ctx, env, obj_slot.type, &ret_type_obj);
	if (err) {
		return OBJ_NONE;
	}

	assert(ret_type_obj.type == ctx->types.type);

	type_id ret_tid = *(type_id *)ret_type_obj.data;

	struct type *ret_type = vm_get_type(ctx->vm, ret_tid);

	uint8_t buffer[ret_type->size];
	memset(buffer, 0, sizeof(uint8_t) * ret_type->size);

	assert(obj_slot.cons.num_present_args == def->num_params);

	for (size_t i = 0; i < obj_slot.cons.num_present_args; i++) {
		struct object_decons_member *member;
		member = &decons->members[def->params[i].param_id];

		struct object value;
		err = ast_slot_pack(ctx, env, obj_slot.cons.args[i].slot, &value);
		if (err) {
			return OBJ_NONE;
		}

		assert(value.type == member->type);

		struct type *member_type = vm_get_type(ctx->vm, member->type);

		assert(member->offset + member_type->size <= ret_type->size);
		memcpy(&buffer[member->offset], value.data, member_type->size);
	}

	struct object result;
	result.type = ret_tid;
	result.data = buffer;
	return register_object(ctx->vm, env->store, result);
}

static struct object
simple_obj_def_unpack(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, int param_id, struct object obj)
{
	struct object res = {0};
	struct object_decons *decons = def->data;
	struct object_decons_member *member = &decons->members[param_id];

	res.type = member->type;
	if (member->ref) {
		void *ptr = *(void **)((uint8_t *)obj.data + member->offset);
		res.data = ptr;
	} else {
		res.data = (uint8_t *)obj.data + member->offset;
	}

	return res;
}

struct ast_object_def *
stg_create_simple_object_def(struct ast_context *ctx,
		struct ast_module *mod, struct ast_object_def *def,
		struct object_decons in_decons)
{
	struct object_decons *decons;
	decons = calloc(1, sizeof(struct object_decons));
	decons->target_type = in_decons.target_type;
	decons->num_members = in_decons.num_members;
	decons->members = calloc(decons->num_members,
			sizeof(struct object_decons_member));

	if (def == NULL) {
		def = ast_object_def_register(mod->env.store);
	} else {
		memset(def, 0, sizeof(struct ast_object_def));
		def->env.store = mod->env.store;
	}

	def->num_params = decons->num_members;
	def->params = calloc(def->num_params,
			sizeof(struct ast_object_def_param));

	for (size_t i = 0; i < decons->num_members; i++) {
		decons->members[i] = in_decons.members[i];
		def->params[i].param_id = i;
		def->params[i].name = decons->members[i].name;
		def->params[i].type =
			ast_bind_slot_const_type(ctx, &def->env, AST_BIND_NEW,
					NULL, decons->members[i].type);
	}

	def->pack = simple_obj_def_pack;
	def->unpack = simple_obj_def_unpack;
	def->ret_type =
		ast_bind_slot_const_type(ctx, &def->env, AST_BIND_NEW,
				NULL, decons->target_type);
	def->data = decons;

	return def;
}
