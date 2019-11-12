#include "ast.h"
#include "bytecode.h"
#include "native_bytecode.h"
#include "module.h"
#include "vm.h"
#include "base/mod.h"
#include <stdlib.h>

#define AST_GEN_SHOW_BC 0

static inline void
append_bc_instr(struct ast_gen_bc_result *res, struct bc_instr *instr)
{
	if (res->last) {
		assert(res->first);
		res->last->next = instr;
	} else {
		res->first = instr;
	}
	res->last = instr;
}

static inline void
append_bc_instrs(struct ast_gen_bc_result *res, struct ast_gen_bc_result instrs)
{
	if (!instrs.first) {
		return;
	}
	assert(instrs.first && instrs.last);
	append_bc_instr(res, instrs.first);
	res->last = instrs.last;
}

static void
ast_object_def_alloc_vars(
		struct ast_context *ctx, struct ast_module *mod,
		struct bc_env *bc_env, struct ast_object_def *def,
		bc_var *vars, size_t *var_i, bc_var *locals, bool is_local)
{
	for (size_t i = 0; i < def->num_params; i++) {
		int err;
		type_id mbr_type;

		ast_slot_id type_slot;
		type_slot = ast_env_slot(ctx, &def->env,
				def->params[i].slot).type;

		err = ast_slot_pack_type(ctx, mod,
				&def->env, type_slot, &mbr_type);
		if (err) {
			printf("Failed to pack cons param type.\n");
			continue;
		}

		vars[*var_i] = bc_alloc_var(bc_env, mbr_type);

		if (is_local) {
			locals[i] = vars[*var_i];
		}

		(*var_i) += 1;

		struct type *member_type;
		member_type = vm_get_type(ctx->vm, mbr_type);

		if (member_type->obj_def) {
			ast_object_def_alloc_vars(
					ctx, mod, bc_env,
					member_type->obj_def,
					vars, var_i, NULL, false);
		}
	}
}

struct ast_gen_bc_result
ast_node_gen_bytecode(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_gen_info *info,
		struct bc_env *bc_env, struct ast_node *node)
{
	struct ast_gen_bc_result result = {0};
	switch (node->kind) {
		case AST_NODE_FUNC:
			{
				struct bc_env *func_bc;
				func_bc = ast_func_gen_bytecode(ctx, mod, env, node);

				struct func new_func = {0};
				new_func.kind = FUNC_BYTECODE;
				assert(node->type != TYPE_UNSET);
				new_func.type = node->type;
				new_func.bytecode = func_bc;

				func_id fid;
				fid = stg_register_func(mod->stg_mod, new_func);

				struct object func_obj = {0};
				func_obj.type = new_func.type;
				func_obj.data = &fid;
				func_obj = register_object(ctx->vm, env->store, func_obj);

				struct bc_instr *func_instr;
				func_instr = bc_gen_load(bc_env, BC_VAR_NEW, func_obj);

				append_bc_instr(&result, func_instr);

				result.out_var = func_instr->load.target;
			}
			return result;

		case AST_NODE_FUNC_NATIVE:
			{
				struct func new_func = {0};
				new_func.kind = FUNC_NATIVE;
				assert(node->type != TYPE_UNSET);
				new_func.type = node->type;
				assert(node->func.native.func != NULL);
				new_func.native = node->func.native.func;

				func_id fid;
				fid = stg_register_func(mod->stg_mod, new_func);

				struct object func_obj = {0};
				func_obj.type = new_func.type;
				func_obj.data = &fid;
				func_obj = register_object(ctx->vm, env->store, func_obj);

				struct bc_instr *func_instr;
				func_instr = bc_gen_load(bc_env, BC_VAR_NEW, func_obj);

				append_bc_instr(&result, func_instr);

				result.out_var = func_instr->load.target;
			}
			return result;

		case AST_NODE_CALL:
			{
				bc_var params[node->call.num_args];

				for (size_t i = 0; i < node->call.num_args; i++) {
					struct ast_gen_bc_result arg;
					arg = ast_node_gen_bytecode(ctx, mod, env, info, bc_env,
							node->call.args[i].value);

					append_bc_instrs(&result, arg);
					params[i] = arg.out_var;
				}

				struct ast_gen_bc_result func;
				func = ast_node_gen_bytecode(ctx, mod, env, info, bc_env,
						node->call.func);
				append_bc_instrs(&result, func);

				for (size_t i = 0; i < node->call.num_args; i++) {
					append_bc_instr(&result, 
							bc_gen_push_arg(bc_env, params[i]));
				}

				struct bc_instr *call;
				call = bc_gen_vcall(bc_env, BC_VAR_NEW, func.out_var);

				append_bc_instr(&result, call);

				result.out_var = call->vcall.target;
			}
			return result;

		case AST_NODE_CONS:
			{
				struct ast_env_slot cons_slot;
				cons_slot = ast_env_slot(ctx, env,
						ast_node_resolve_slot(env, &node->call.cons));
				assert(cons_slot.kind == AST_SLOT_CONS);

				struct ast_object_def *def;
				def = cons_slot.cons.def;
				assert(def);

				if (def->pack_func != FUNC_UNSET) {
					size_t num_desc_members;
					num_desc_members =
						ast_object_def_num_descendant_members(ctx, mod, def);
					bc_var local_mbrs[def->num_params];
					bc_var object_mbrs[num_desc_members];

					size_t var_i = 0;
					ast_object_def_alloc_vars(ctx, mod, bc_env,
							def, object_mbrs, &var_i, local_mbrs, true);

					int bind_order[def->num_binds];

					int err;
					err = ast_object_def_order_binds(
							ctx, mod, def, NULL, 0, bind_order);
					// TODO: Resort the def's binds together whith the cons' binds.

					for (size_t i = 0; i < def->num_binds; i++) {
						size_t bind_i = bind_order[i];

						if (def->binds[bind_i].kind != AST_OBJECT_DEF_BIND_CONST) {
							for (size_t val_i = 0;
									val_i < def->binds[bind_i].num_value_params; val_i++) {
								ast_member_id dep;
								dep = def->binds[bind_i].value_params[val_i];
								assert(dep < num_desc_members);

								append_bc_instr(&result,
										bc_gen_push_arg(bc_env, object_mbrs[dep]));
							}
						}

						ast_member_id target;
						target = def->binds[bind_i].target;
						assert(target < num_desc_members);

						switch (def->binds[bind_i].kind) {
							case AST_OBJECT_DEF_BIND_VALUE:
								append_bc_instr(&result,
										bc_gen_lcall(bc_env, object_mbrs[target],
											def->binds[bind_i].value.func));
								break;

							case AST_OBJECT_DEF_BIND_CONST:
								append_bc_instr(&result,
										bc_gen_load(bc_env, object_mbrs[target],
											def->binds[bind_i].const_value));
								break;

							case AST_OBJECT_DEF_BIND_PACK:
								append_bc_instr(&result,
										bc_gen_pack(bc_env, object_mbrs[target],
											def->binds[bind_i].pack->pack_func,
											def->binds[bind_i].pack->data,
											bc_get_var_type(bc_env, object_mbrs[target])));
								break;
						}
					}

					for (size_t i = 0; i < def->num_params; i++) {
						append_bc_instr(&result,
								bc_gen_push_arg(bc_env, local_mbrs[i]));
					}

					append_bc_instr(&result,
							bc_gen_pack(bc_env, BC_VAR_NEW,
								def->pack_func, def->data, node->type));
					result.out_var = result.last->pack.target;
				} else {
					struct object obj;

					// TODO: Remove old pack interface.

					assert(def->pack);

					obj = def->pack(ctx, mod, env,
							def, node->call.cons);

					result.first = result.last =
						bc_gen_load(bc_env, BC_VAR_NEW, obj);
					result.out_var = result.first->load.target;
				}
			}
			return result;

		case AST_NODE_TEMPL:
			panic("TODO: Implement generating bytecode for templ.");
			break;

		case AST_NODE_ACCESS:
			{
				struct ast_gen_bc_result target = {0};

				target = ast_node_gen_bytecode(
						ctx, mod, env, info, bc_env,
						node->access.target);

				append_bc_instrs(&result, target);
				append_bc_instr(&result,
						bc_gen_push_arg(bc_env, target.out_var));

				type_id target_type_id = bc_get_var_type(bc_env, target.out_var);
				struct type *type = vm_get_type(ctx->vm, target_type_id);

				// TODO: Support deconstruction using non-default constructors
				// from pattern matching.
				assert(type->obj_def);

				struct ast_object_def *cons;
				cons = type->obj_def;

				bool found = false;
				int param_id = -1;
				for (size_t i = 0; i < cons->num_params; i++) {
					 if (cons->params[i].name == node->access.name) {
						 found = true;
						 param_id = cons->params[i].param_id;
					 }
				}

				assert(found);

				type_id param_type = TYPE_UNSET;
				int err;

				ast_node_resolve_slot(env, &node->access.slot);
				err = ast_slot_pack_type(ctx, mod, env,
						ast_env_slot(ctx, env, node->access.slot).type,
						&param_type);
				assert(!err);

				append_bc_instr(&result,
						bc_gen_unpack(bc_env, BC_VAR_NEW,
							cons->unpack_func, cons->data, param_id, param_type));
				result.out_var = result.last->unpack.target;
			}
			return result;

		case AST_NODE_SLOT:
			{
				struct ast_env_slot slot;
				slot = ast_env_slot(ctx, env,
						ast_node_resolve_slot(env, &node->slot));

				switch (slot.kind) {
					case AST_SLOT_CONST:
					case AST_SLOT_CONST_TYPE:
					case AST_SLOT_CONS:
					case AST_SLOT_CONS_ARRAY:
						{
							struct object obj;
							int err;

							err = ast_slot_pack(ctx, mod, env, node->slot, &obj);
							if (err) {
								panic("Failed to pack slot in gen bytecode.");
								return (struct ast_gen_bc_result){0};
							}

							result.first = result.last =
								bc_gen_load(bc_env, BC_VAR_NEW, obj);
							result.out_var = result.first->load.target;
						}
						return result;

					case AST_SLOT_PARAM:
						{
							struct object type_obj;
							int err;

							err = ast_slot_pack(ctx, mod, env, slot.type, &type_obj);
							if (err) {
								panic("Failed to pack slot type in gen bytecode.");
								return (struct ast_gen_bc_result){0};
							}

							assert_type_equals(ctx->vm, type_obj.type, ctx->types.type);

							type_id param_type;
							param_type = *(type_id *)type_obj.data;

							result.first = result.last = NULL;
							result.out_var = bc_alloc_param(
									bc_env, slot.param_index, param_type);
						}
						return result;

					case AST_SLOT_MEMBER:
						{
							struct object type_obj;
							int err;

							err = ast_slot_pack(ctx, mod, env, slot.type, &type_obj);
							if (err) {
								panic("Failed to pack slot type in gen bytecode.");
								return (struct ast_gen_bc_result){0};
							}

							assert_type_equals(ctx->vm, type_obj.type, ctx->types.type);

							type_id member_type;
							member_type = *(type_id *)type_obj.data;

							int member_i = -1;

							/*
							 * TODO: Lookup based on member id
							for (size_t i = 0; i < info->num_member_names; i++) {
								if (slot.member_name == info->member_names[i]) {
									member_i = i;
									break;
								}
							}
							*/

							assert(member_i > -1);

							result.first = result.last = NULL;
							result.out_var = bc_alloc_param(
									bc_env, member_i, member_type);
						}
						return result;

					default:
						panic("Invalid slot %s in bytecode gen.",
								ast_slot_name(slot.kind));
						return (struct ast_gen_bc_result ){0};
				}

			}
			break;

		case AST_NODE_LOOKUP:
			switch (node->lookup.ref.kind) {
				case AST_NAME_REF_MEMBER:
					for (size_t i = 0; i < info->num_members; i++) {
						if (info->members[i] == node->lookup.ref.member) {
							result.first = result.last = NULL;
							result.out_var = bc_alloc_param(
									bc_env, i, node->type);
							return result;
						}
					}
					break;

				case AST_NAME_REF_PARAM:
					result.first = result.last = NULL;
					result.out_var = bc_alloc_param(
							bc_env, node->lookup.ref.param, node->type);
					return result;

				case AST_NAME_REF_CLOSURE:
					assert(node->lookup.ref.closure < info->num_closures);
					if (info->closures[node->lookup.ref.closure].req ==
							AST_NAME_DEP_REQUIRE_VALUE) {
						struct bc_instr *lit_instr;
						lit_instr = bc_gen_load(bc_env, BC_VAR_NEW,
								info->closures[node->lookup.ref.closure].value);

						append_bc_instr(&result, lit_instr);

						result.out_var = lit_instr->load.target;

						return result;
					} else {
						panic("TODO: Closures");
					}
					break;

				case AST_NAME_REF_NOT_FOUND:
					panic("Got failed lookup in code gen.");
					break;
			}
			break;

		case AST_NODE_LIT:
			{
				struct bc_instr *lit_instr;
				lit_instr = bc_gen_load(bc_env, BC_VAR_NEW, node->lit.obj);

				append_bc_instr(&result, lit_instr);

				result.out_var = lit_instr->load.target;
			}
			return result;

		case AST_NODE_COMPOSITE:
			{
				type_id type;

				type = node->composite.type;

				if (type == TYPE_UNSET) {
					printf("Failed to initialize composite type.\n");
					return (struct ast_gen_bc_result){0};
				}

				struct object obj;
				obj.type = ctx->types.type;
				obj.data = &type;

				struct bc_instr *lit_instr;
				lit_instr = bc_gen_load(bc_env, BC_VAR_NEW,
						register_object(ctx->vm, env->store, obj));

				append_bc_instr(&result, lit_instr);

				result.out_var = lit_instr->load.target;
			}
			return result;

		case AST_NODE_VARIANT:
			break;
	}

	printf("Invalid ast node in gen byte code.\n");
	return (struct ast_gen_bc_result){0};
}

struct bc_env *
ast_func_gen_bytecode(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node)
{
	assert(node->kind == AST_NODE_FUNC);

	struct bc_env *bc_env = calloc(1, sizeof(struct bc_env));
	bc_env->vm = ctx->vm;
	bc_env->store = ctx->vm->instr_store;

	assert(stg_type_is_func(ctx->vm, node->type));

	struct type *target_func_type;
	target_func_type = vm_get_type(ctx->vm, node->type);

	struct stg_func_type *target_func_info;
	target_func_info = target_func_type->data;

	assert(target_func_info->num_params == node->func.num_params);

	for (size_t i = 0; i < node->func.num_params; i++) {
		type_id param_tid;
		param_tid = target_func_info->params[i];

		bc_alloc_param(bc_env, i, param_tid);
	}

	struct ast_gen_info info = {0};

	struct ast_gen_bc_result func_instr;
	func_instr = ast_node_gen_bytecode(ctx, mod, env, &info,
			bc_env, node->func.body);

	append_bc_instr(&func_instr,
			bc_gen_ret(bc_env, func_instr.out_var));

	bc_env->entry_point = func_instr.first;

#if AST_GEN_SHOW_BC
	printf("\nbc:\n");
	bc_print(bc_env, bc_env->entry_point);
#endif

	bc_env->nbc = calloc(1, sizeof(struct nbc_func));
	nbc_compile_from_bc(bc_env->nbc, bc_env);

#if AST_GEN_SHOW_BC
	printf("\nnbc:\n");
	nbc_print(bc_env->nbc);
#endif

	return bc_env;
}

struct bc_env *
ast_composite_bind_gen_bytecode(
		struct ast_context *ctx, struct ast_module *mod, struct ast_env *env,
		ast_member_id *members, type_id *member_types, size_t num_members,
		struct ast_typecheck_closure *closures, size_t num_closures, struct ast_node *expr)
{
	struct bc_env *bc_env = calloc(1, sizeof(struct bc_env));
	bc_env->vm = ctx->vm;
	bc_env->store = ctx->vm->instr_store;

	/*
	 * TODO: Assert that the returned value in byte code has the same type we
	 * expect.
	*/

	struct ast_gen_info info = {0};
	info.members = members;
	info.member_types = member_types;
	info.num_members = num_members;

	info.closures = closures;
	info.num_closures = num_closures;

	for (size_t i = 0; i < num_members; i++) {
		bc_alloc_param(bc_env, i, member_types[i]);
	}

	struct ast_gen_bc_result func_instr;
	func_instr = ast_node_gen_bytecode(ctx, mod, env, &info,
			bc_env, expr);

	append_bc_instr(&func_instr,
			bc_gen_ret(bc_env, func_instr.out_var));

	bc_env->entry_point = func_instr.first;

#if AST_GEN_SHOW_BC
	printf("\nbc:\n");
	bc_print(bc_env, bc_env->entry_point);
#endif

	bc_env->nbc = calloc(1, sizeof(struct nbc_func));
	nbc_compile_from_bc(bc_env->nbc, bc_env);

#if AST_GEN_SHOW_BC
	printf("\nnbc:\n");
	nbc_print(bc_env->nbc);
#endif

	return bc_env;
}
