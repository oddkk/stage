#include "ast.h"
#include "bytecode.h"
#include "native_bytecode.h"
#include "module.h"
#include "native.h"
#include "vm.h"
#include "base/mod.h"
#include <stdlib.h>
#include <string.h>

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

struct ast_typecheck_closure
ast_gen_resolve_closure(struct bc_env *bc_env,
		struct ast_gen_info *info, struct ast_name_ref ref)
{
	switch (ref.kind) {
		case AST_NAME_REF_NOT_FOUND:
			panic("Got unresolved name ref in code gen.");
			break;

		case AST_NAME_REF_MEMBER:
			for (size_t i = 0; i < info->num_members; i++) {
				if (info->members[i] == ref.member) {
					struct ast_typecheck_closure res = {0};

					if (info->const_member_values[i].type != TYPE_UNSET) {
						res.req = AST_NAME_DEP_REQUIRE_VALUE;
						res.value = info->const_member_values[i];
					} else {
						res.req = AST_NAME_DEP_REQUIRE_TYPE;
						res.type = info->member_types[i];
					}

					return res;
				}
			}

			panic("Member not found when resolving closure.");
			return (struct ast_typecheck_closure){0};

		case AST_NAME_REF_PARAM:
			{
				struct ast_typecheck_closure res = {0};
				res.req = AST_NAME_DEP_REQUIRE_TYPE;
				res.type = bc_get_var_type(bc_env, -1 - ref.param);
				return res;
			}

		case AST_NAME_REF_CLOSURE:
			assert(ref.closure < info->num_closures);
			return info->closures[ref.closure];

		case AST_NAME_REF_TEMPL:
			{
				assert(ref.templ < info->num_templ_values);

				struct ast_typecheck_closure res = {0};
				res.req = AST_NAME_DEP_REQUIRE_VALUE;
				res.lookup_failed = false;
				res.value = info->templ_values[ref.templ];
				return res;
			}
			break;
	}

	return (struct ast_typecheck_closure){0};
}

#define AST_GEN_ERROR ((struct ast_gen_bc_result){.err=-1})
#define AST_GEN_EXPECT_OK(res) do { if ((res).err) { return res; } } while (0);

static struct ast_gen_bc_result
ast_name_ref_gen_bytecode(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_gen_info *info,
		struct bc_env *bc_env, struct ast_name_ref ref,
		struct stg_location loc, type_id type)
{
	struct ast_gen_bc_result result = {0};

	switch (ref.kind) {
		case AST_NAME_REF_MEMBER:
			for (size_t i = 0; i < info->num_members; i++) {
				if (info->members[i] == ref.member) {
					result.first = result.last = NULL;
					result.out_var = bc_alloc_param(
							bc_env, i, type);
					return result;
				}
			}
			break;

		case AST_NAME_REF_PARAM:
			result.first = result.last = NULL;
			result.out_var = bc_alloc_param(
					bc_env, ref.param, type);
			return result;

		case AST_NAME_REF_CLOSURE:
			assert(ref.closure < info->num_closures);
			if (info->closures[ref.closure].lookup_failed) {
				stg_error(ctx->err, loc,
						"Name not found.");
				return AST_GEN_ERROR;
			} else if (info->closures[ref.closure].req ==
					AST_NAME_DEP_REQUIRE_VALUE) {
				struct bc_instr *lit_instr;
				lit_instr = bc_gen_load(bc_env, BC_VAR_NEW,
						info->closures[ref.closure].value);

				append_bc_instr(&result, lit_instr);

				result.out_var = lit_instr->load.target;

				return result;
			} else {
				bc_closure closure_ref;
				assert(info->closure_refs);
				closure_ref = info->closure_refs[ref.closure];
				assert(closure_ref != AST_BC_CLOSURE_PRUNED);

				struct bc_instr *closure_instr;
				closure_instr = bc_gen_copy_closure(
						bc_env, BC_VAR_NEW, closure_ref);

				append_bc_instr(&result, closure_instr);
				result.out_var = closure_instr->copy_closure.target;

				return result;

				// stg_error(ctx->err, loc,
				// 		"TODO: Closures");
				// return AST_GEN_ERROR;
			}
			break;

		case AST_NAME_REF_TEMPL:
			panic("TODO: Pass template information to gen.");
			result.first = result.last = NULL;
			result.out_var = bc_alloc_param(
					bc_env, ref.param, type);
			return result;

		case AST_NAME_REF_NOT_FOUND:
			panic("Got failed lookup in code gen.");
			break;
	}

	printf("Name ref not handled in code gen\n");
	return AST_GEN_ERROR;
}

static struct ast_gen_bc_result
ast_unpack_gen_bytecode(struct ast_context *ctx, struct ast_module *mod,
		struct bc_env *bc_env, bc_var value, size_t descendent)
{
	struct ast_gen_bc_result result = {0};

	result.out_var = value;

	type_id current_type = bc_get_var_type(bc_env, value);
	while (descendent > 0) {
		struct type *type;
		type = vm_get_type(bc_env->vm, current_type);

		struct object_cons *def;
		def = type->obj_def;

		// As descendent is > 0, we expect this child to have children.
		assert(def);

		size_t offset = 1;
		for (size_t i = 0; i < def->num_params; i++) {
			struct type *mbr_type;
			mbr_type = vm_get_type(bc_env->vm,
					def->params[i].type);

			size_t num_desc;
			if (mbr_type->obj_def) {
				num_desc = object_cons_num_descendants(
						ctx->vm, mbr_type->obj_def);
			} else {
				num_desc = 1;
			}

			assert(descendent >= offset);
			if (descendent < offset + num_desc) {
				assert(def->unpack);

				append_bc_instr(&result,
						bc_gen_push_arg(bc_env, result.out_var));
				append_bc_instr(&result,
						bc_gen_unpack(bc_env, BC_VAR_NEW,
							def->unpack, def->data, i,
							def->params[i].type));

				current_type = def->params[i].type;
				descendent -= offset;
				break;
			} else {
				offset += num_desc;
			}
		}
	}

	return result;
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
				size_t num_closures = node->func.closure.num_members;
				struct ast_typecheck_closure closure[num_closures];
				bc_closure closure_refs[num_closures];
				bc_var closure_vars[num_closures];
				struct stg_func_closure_member closure_members[num_closures];

				bc_closure num_found_closures = 0;
				size_t closure_offset = 0;
				for (size_t i = 0; i < num_closures; i++) {
					struct ast_closure_member *cls;
					cls = &node->func.closure.members[i];
					closure[i] = ast_gen_resolve_closure(
							bc_env, info, cls->ref);

					// Ensure we got a value if we require const.
					assert(!cls->require_const ||
							closure[i].req == AST_NAME_DEP_REQUIRE_VALUE);

					if (closure[i].req == AST_NAME_DEP_REQUIRE_TYPE) {
						bc_closure closure_ref;
						closure_ref = num_found_closures;
						num_found_closures += 1;

						closure_refs[i] = closure_ref;

						struct type *mbr_type;
						mbr_type = vm_get_type(ctx->vm,
								closure[i].type);

						closure_members[closure_ref].type   = closure[i].type;
						closure_members[closure_ref].size   = mbr_type->size;
						closure_members[closure_ref].offset = closure_offset;
						closure_offset += closure_members[closure_ref].size;

						struct ast_gen_bc_result value_instrs;
						value_instrs = ast_name_ref_gen_bytecode(
								ctx, mod, env, info, bc_env,
								cls->ref, node->loc, closure[i].type);
						append_bc_instrs(&result, value_instrs);
						closure_vars[closure_ref] = value_instrs.out_var;

						// TODO: Get bc reference to the value
					} else {
						// The value of this closure is constant, so we can
						// prune it.
						closure_refs[i] = AST_BC_CLOSURE_PRUNED;
					}
				}
				size_t post_prune_num_closures = num_found_closures;

				struct bc_env *func_bc;
				func_bc = ast_func_gen_bytecode(
						ctx, mod, env, closure,
						closure_refs, num_closures, node);
				if (!func_bc) {
					return AST_GEN_ERROR;
				}

				struct func new_func = {0};
				new_func.kind = FUNC_BYTECODE;
				assert(node->type != TYPE_UNSET);
				new_func.type = node->type;
				new_func.bytecode = func_bc;

				func_id fid;
				fid = stg_register_func(mod->stg_mod, new_func);

				struct object func_obj = {0};
				func_obj = stg_register_func_object(
						ctx->vm, env->store, fid, NULL);

				struct stg_func_closure_data *data;
				data = calloc(1, sizeof(struct stg_func_closure_data));
				data->num_members = post_prune_num_closures;
				data->members = calloc(data->num_members,
						sizeof(struct stg_func_closure_member));
				memcpy(data->members, closure_members,
						data->num_members * sizeof(struct stg_func_closure_member));
				data->func = fid;
				data->size = closure_offset;

				for (size_t i = 0; i < post_prune_num_closures; i++) {
					append_bc_instr(&result,
							bc_gen_push_arg(bc_env, closure_vars[i]));
				}

				// TODO: Pack closure with function id.
				struct bc_instr *func_instr;
				func_instr = bc_gen_pack(bc_env, BC_VAR_NEW,
						stg_func_closure_pack, data, node->type);
				append_bc_instr(&result, func_instr);
				result.out_var = func_instr->pack.target;
			}
			return result;

		case AST_NODE_FUNC_NATIVE:
			{
				struct func new_func = {0};
				new_func.kind = FUNC_NATIVE;
				assert(node->type != TYPE_UNSET);
				new_func.type = node->type;

				// We should already have checked that this native function
				// exists during lookup.
				struct stg_native_module *native_mod;
				native_mod = node->func.native.native_mod;
				assert(native_mod);

				struct stg_native_func *native_func = NULL;

				for (size_t i = 0; i < native_mod->num_funcs; i++) {
					if (string_equal(
								native_mod->funcs[i].name,
								node->func.native.name)) {
						native_func = &native_mod->funcs[i];
						break;
					}
				}

				assert(native_func);
				assert(native_func->func);
				new_func.native = native_func->func;

				void *closure = NULL;

				if ((native_func->flags & STG_NATIVE_FUNC_IMPURE) != 0) {
					new_func.flags |= FUNC_IMPURE;
				}

				if ((native_func->flags & STG_NATIVE_FUNC_MODULE_CLOSURE) != 0) {
					struct stg_module *owning_mod = NULL;
					owning_mod = vm_get_module_by_native(
							ctx->vm, native_mod);

					closure = owning_mod;
					new_func.flags |= FUNC_CLOSURE;
				}

				func_id fid;
				fid = stg_register_func(mod->stg_mod, new_func);

				struct object func_obj = {0};
				func_obj = stg_register_func_object(
						ctx->vm, env->store, fid, closure);

				struct bc_instr *func_instr;
				func_instr = bc_gen_load(bc_env, BC_VAR_NEW, func_obj);

				append_bc_instr(&result, func_instr);

				result.out_var = func_instr->load.target;
			}
			return result;

		case AST_NODE_CALL:
			{
				bc_var params[node->call.num_args];
				int err = 0;

				for (size_t i = 0; i < node->call.num_args; i++) {
					struct ast_gen_bc_result arg;
					arg = ast_node_gen_bytecode(ctx, mod, env, info, bc_env,
							node->call.args[i].value);
					if (arg.err) {
						err = -1;
						continue;
					}

					append_bc_instrs(&result, arg);
					params[i] = arg.out_var;
				}

				struct ast_gen_bc_result func;
				func = ast_node_gen_bytecode(ctx, mod, env, info, bc_env,
						node->call.func);
				if (func.err) {
					err = -1;
				}

				if (err) {
					return AST_GEN_ERROR;
				}

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

		case AST_NODE_INST:
			{
				struct object_inst *inst;
				inst = node->call.inst;

				struct object_inst_bind       extra_binds[node->call.num_args];
				struct object_inst_extra_expr extra_exprs[node->call.num_args];

				memset(extra_binds, 0,
						sizeof(struct object_inst_bind) * node->call.num_args);
				memset(extra_exprs, 0,
						sizeof(struct object_inst_extra_expr) * node->call.num_args);

				for (size_t i = 0; i < node->call.num_args; i++) {
					extra_binds[i].unpack_id = 0;
					extra_binds[i].expr_id = inst->num_exprs + i;
					extra_binds[i].loc = node->call.args[i].value->loc;

					extra_binds[i].target_id = -1;
					for (size_t mbr_i = 0; mbr_i < inst->cons->num_params; mbr_i++) {
						if (inst->cons->params[mbr_i].name == node->call.args[i].name) {
							extra_binds[i].target_id = mbr_i;
							break;
						}
					}
					assert(extra_binds[i].target_id >= 0);

					extra_exprs[i].deps = NULL;
					extra_exprs[i].num_deps = 0;
					extra_exprs[i].overridable = false;
					extra_exprs[i].loc = node->call.args[i].value->loc;

					extra_exprs[i].type = node->call.args[i].value->type;
					assert(extra_exprs[i].type != TYPE_UNSET);
				}

				struct object_inst_action *actions = NULL;
				size_t num_actions = 0;
				int err;
				err = object_inst_order(
						ctx->vm, inst,
						extra_exprs, node->call.num_args,
						extra_binds, node->call.num_args,
						&actions, &num_actions);
				if (err) {
					printf("Failed to instantiate object.");
					return AST_GEN_ERROR;
				}

				const size_t num_exprs =
					inst->num_exprs + node->call.num_args;
				bc_var expr_vars[num_exprs];

				for (size_t i = 0; i < num_exprs; i++) {
					expr_vars[i] = BC_VAR_NEW;
				}

				const size_t num_desc_members =
					object_cons_num_descendants(
							ctx->vm, inst->cons);
				bc_var member_vars[num_desc_members];

				for (size_t i = 0; i < num_desc_members; i++) {
					member_vars[i] = BC_VAR_NEW;
				}

				for (size_t i = 0; i < num_actions; i++) {
					struct object_inst_action *act;
					act = &actions[i];

					switch (act->op) {
						case OBJ_INST_EXPR:
							{
								assert(act->expr.id < num_exprs);
								assert(expr_vars[act->expr.id] != BC_VAR_NEW);
								assert(act->expr.num_deps == 0);

								int expr_id = act->expr.id;

								if (expr_id < inst->num_exprs) {
									struct object_inst_expr *expr;
									expr = &inst->exprs[expr_id];

									if (expr->constant) {
										struct bc_instr *instr;
										instr = bc_gen_load(
												bc_env, BC_VAR_NEW,
												expr->const_value);
										assert(expr_vars[i] == BC_VAR_NEW);
										expr_vars[i] = instr->load.target;
										append_bc_instr(&result, instr);
									} else {
										for (size_t i = 0; i < act->expr.num_deps; i++) {
											assert(act->expr.deps[i] < num_desc_members);
											assert(member_vars[act->expr.deps[i]] != BC_VAR_NEW);
											append_bc_instr(&result,
													bc_gen_push_arg(
														bc_env, member_vars[act->expr.deps[i]]));
										}

										struct bc_instr *call_instr;
										call_instr = bc_gen_lcall(
													bc_env, BC_VAR_NEW, expr->func);
										append_bc_instr(&result, call_instr);
										expr_vars[expr_id] = call_instr->lcall.target;
									}
								} else {
									expr_id -= inst->num_exprs;
									assert(expr_id < node->call.num_args);

									// TODO: Allow referencing other members.
									/*
									for (size_t i = 0; i < act->expr.num_deps; i++) {
										assert(act->expr.deps[i] < num_desc_members);
										assert(member_vars[act->expr.deps[i]] != BC_VAR_NEW);
									}
									*/

									struct ast_gen_bc_result expr;
									expr = ast_node_gen_bytecode(
											ctx, mod, env, info, bc_env,
											node->call.args[expr_id].value);

									append_bc_instrs(&result, expr);
									assert(expr_vars[expr_id] == BC_VAR_NEW);
									expr_vars[expr_id] = expr.out_var;
								}
							}
							break;

						case OBJ_INST_BIND:
							{
								bc_var expr_var = expr_vars[act->bind.expr_id];
								assert(expr_var != BC_VAR_NEW);
								assert(member_vars[act->bind.member_id] == BC_VAR_NEW);

								struct ast_gen_bc_result instrs;
								instrs = ast_unpack_gen_bytecode(
										ctx, mod, bc_env, expr_var,
										act->bind.unpack_id);

								append_bc_instrs(&result, instrs);
								member_vars[act->bind.member_id] = instrs.out_var;
							}
							break;

						case OBJ_INST_PACK:
							{
								assert(member_vars[act->pack.member_id] == BC_VAR_NEW);

								type_id mbr_type_id;
								int err;
								err = object_cons_descendant_type(
										ctx->vm, node->type,
										act->pack.member_id, &mbr_type_id);
								struct type *mbr_type;
								mbr_type = vm_get_type(ctx->vm, mbr_type_id);

								struct object_cons *mbr_cons;
								mbr_cons = mbr_type->obj_def;


								assert(mbr_cons);

								int local_mbrs[mbr_cons->num_params];
								object_cons_local_descendent_ids(
										ctx->vm, mbr_cons, local_mbrs);

								int first_child = act->pack.member_id + 1;
								for (size_t i = 0; i < mbr_cons->num_params; i++) {
									int param_id = first_child + local_mbrs[i];
									assert(param_id < num_desc_members);
									assert(member_vars[param_id] != BC_VAR_NEW);

									append_bc_instr(&result,
											bc_gen_push_arg(
												bc_env, member_vars[param_id]));
								}

								struct bc_instr *pack_instr;
								pack_instr =
									bc_gen_pack(
											bc_env, BC_VAR_NEW,
											mbr_cons->pack, mbr_cons->data,
											mbr_type_id);
								append_bc_instr(&result, pack_instr);

								member_vars[act->pack.member_id] = pack_instr->pack.target;
							}
							break;
					}
				}

				free(actions);

				int local_param_ids[inst->cons->num_params];
				object_cons_local_descendent_ids(
						ctx->vm, inst->cons, local_param_ids);

				for (size_t i = 0; i < inst->cons->num_params; i++) {
					int param_id = local_param_ids[i];
					assert(param_id < num_desc_members);
					assert(member_vars[param_id] != BC_VAR_NEW);

					append_bc_instr(&result,
							bc_gen_push_arg(
								bc_env, member_vars[param_id]));
				}

				struct bc_instr *pack_instr;
				pack_instr =
					bc_gen_pack(
							bc_env, BC_VAR_NEW,
							inst->cons->pack, inst->cons->data,
							node->type);
				append_bc_instr(&result, pack_instr);

				result.out_var = pack_instr->pack.target;
			}
			return result;

		case AST_NODE_CONS:
			{
				struct object_cons *def;
				def = node->call.cons;
				assert(def);

				if (node->call.num_args != def->num_params) {
					stg_error(ctx->err, node->loc,
							"Expected %zu parameters, got %zu.",
							def->num_params, node->call.num_args);
					return AST_GEN_ERROR;
				}

				bc_var arg_vars[def->num_params];

				for (size_t i = 0; i < def->num_params; i++) {
					struct ast_gen_bc_result arg;
					arg = ast_node_gen_bytecode(
							ctx, mod, env, info, bc_env,
							node->call.args[i].value);

					append_bc_instrs(&result, arg);

					arg_vars[i] = arg.out_var;
				}

				for (size_t i = 0; i < def->num_params; i++) {
					append_bc_instr(&result,
							bc_gen_push_arg(bc_env, arg_vars[i]));
				}

				struct bc_instr *pack_instr;
				pack_instr = bc_gen_pack(bc_env, BC_VAR_NEW,
							def->pack, def->data, node->type);
				append_bc_instr(&result, pack_instr);

				result.out_var = pack_instr->pack.target;
			}
			return result;

		case AST_NODE_FUNC_TYPE:
			{
				assert(node->func_type.func_type != TYPE_UNSET);
				struct object obj = {0};
				obj.type = ctx->types.type;
				obj.data = &node->func_type.func_type;

				obj = register_object(ctx->vm, env->store, obj);

				result.first = result.last =
					bc_gen_load(bc_env, BC_VAR_NEW, obj);
				result.out_var = result.first->load.target;
			}
			return result;

		case AST_NODE_TEMPL:
			{
				struct object obj = {0};
				obj.type = ctx->types.cons;
				obj.data = &node->templ.cons;

				obj = register_object(ctx->vm, env->store, obj);

				result.first = result.last =
					bc_gen_load(bc_env, BC_VAR_NEW, obj);
				result.out_var = result.first->load.target;
			}
			return result;

		case AST_NODE_ACCESS:
			{
				struct ast_gen_bc_result target = {0};

				target = ast_node_gen_bytecode(
						ctx, mod, env, info, bc_env,
						node->access.target);
				AST_GEN_EXPECT_OK(target);

				append_bc_instrs(&result, target);
				append_bc_instr(&result,
						bc_gen_push_arg(bc_env, target.out_var));

				type_id target_type_id = bc_get_var_type(bc_env, target.out_var);
				struct type *type = vm_get_type(ctx->vm, target_type_id);

				// TODO: Support deconstruction using non-default constructors
				// from pattern matching.
				assert(type->obj_def);

				struct object_cons *cons;
				cons = type->obj_def;

				bool found = false;
				int param_id = -1;
				for (size_t i = 0; i < cons->num_params; i++) {
					 if (cons->params[i].name == node->access.name) {
						 found = true;
						 param_id = i;
					 }
				}
				assert(found);

				append_bc_instr(&result,
						bc_gen_unpack(bc_env, BC_VAR_NEW,
							cons->unpack, cons->data, param_id, node->type));
				result.out_var = result.last->unpack.target;
			}
			return result;

		case AST_NODE_LOOKUP:
			return ast_name_ref_gen_bytecode(
					ctx, mod, env, info, bc_env,
					node->lookup.ref, node->loc, node->type);
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
					return AST_GEN_ERROR;
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
			{
				type_id type;
				type = node->variant.type;

				if (type == TYPE_UNSET) {
					printf("Failed to initialize variant type.\n");
					return AST_GEN_ERROR;
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
	}

	printf("Invalid ast node in gen byte code.\n");
	return AST_GEN_ERROR;
}

struct bc_env *
ast_func_gen_bytecode(
		struct ast_context *ctx, struct ast_module *mod, struct ast_env *env,
		struct ast_typecheck_closure *closures, bc_closure *closure_refs,
		size_t num_closures, struct ast_node *node)
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

	info.closures = closures;
	info.closure_refs = closure_refs;
	info.num_closures = num_closures;

	size_t num_unpruned_closures = 0;
	for (size_t i = 0; i < num_closures; i++) {
		if (closure_refs[i] != AST_BC_CLOSURE_PRUNED) {
			num_unpruned_closures += 1;
		}
	}

	type_id closure_types[num_unpruned_closures];

	for (size_t i = 0; i < num_unpruned_closures; i++) {
		closure_types[i] = TYPE_UNSET;
	}

	for (size_t i = 0; i < num_closures; i++) {
		if (closure_refs[i] != AST_BC_CLOSURE_PRUNED) {
			assert(closure_refs[i] < num_unpruned_closures);
			assert(closures[i].req == AST_NAME_DEP_REQUIRE_TYPE);
			assert(closure_types[i] == TYPE_UNSET);

			closure_types[i] = closures[i].type;
		}
	}

	for (size_t i = 0; i < num_unpruned_closures; i++) {
		assert(closure_types[i] != TYPE_UNSET);
	}
	bc_env->num_closures = num_unpruned_closures;
	bc_env->closure_types = closure_types;

	struct ast_gen_bc_result func_instr;
	func_instr = ast_node_gen_bytecode(ctx, mod, env, &info,
			bc_env, node->func.body);
	if (func_instr.err) {
		return NULL;
	}

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
		ast_member_id *members, type_id *member_types,
		struct object *const_member_values, size_t num_members,
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
	info.const_member_values = const_member_values;
	info.num_members = num_members;

	info.closures = closures;
	info.num_closures = num_closures;

	for (size_t i = 0; i < num_members; i++) {
		bc_alloc_param(bc_env, i, member_types[i]);
	}

	struct ast_gen_bc_result func_instr;
	func_instr = ast_node_gen_bytecode(ctx, mod, env, &info,
			bc_env, expr);
	if (func_instr.err) {
		return NULL;
	}

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
ast_type_expr_gen_bytecode(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *expr,
		struct ast_typecheck_closure *closures, size_t num_closures)
{
	struct bc_env *bc_env = calloc(1, sizeof(struct bc_env));
	bc_env->vm = ctx->vm;
	bc_env->store = ctx->vm->instr_store;

	struct ast_gen_info info = {0};

	info.closures = closures;
	info.closure_refs = NULL;
	info.num_closures = num_closures;

	struct ast_gen_bc_result func_instr;
	func_instr = ast_node_gen_bytecode(ctx, mod, env, &info,
			bc_env, expr);
	if (func_instr.err) {
		return NULL;
	}

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
ast_gen_value_unpack_func(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, type_id value_type, size_t descendent)
{
	struct bc_env *bc_env = calloc(1, sizeof(struct bc_env));
	bc_env->vm = ctx->vm;
	bc_env->store = ctx->vm->instr_store;

	bc_var param_var;
	param_var = bc_alloc_param(bc_env, 0, value_type);
	struct ast_gen_bc_result result;
	result = ast_unpack_gen_bytecode(
			ctx, mod, bc_env, param_var, descendent);

	append_bc_instr(&result,
			bc_gen_ret(bc_env, result.out_var));

	bc_env->entry_point = result.first;

#if AST_GEN_SHOW_BC
	printf("\nunpack bc:\n");
	bc_print(bc_env, bc_env->entry_point);
#endif

	bc_env->nbc = calloc(1, sizeof(struct nbc_func));
	nbc_compile_from_bc(bc_env->nbc, bc_env);

#if AST_GEN_SHOW_BC
	printf("\nunpack nbc:\n");
	nbc_print(bc_env->nbc);
#endif

	return bc_env;
}
