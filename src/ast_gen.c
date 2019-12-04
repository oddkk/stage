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

				for (size_t i = 0; i < num_closures; i++) {
					struct ast_closure_member *cls;
					cls = &node->func.closure.members[i];
					closure[i] = ast_gen_resolve_closure(
							bc_env, info, cls->ref);

					// Ensure we got a value if we require const.
					assert(!cls->require_const ||
							closure[i].req == AST_NAME_DEP_REQUIRE_VALUE);
				}

				struct bc_env *func_bc;
				func_bc = ast_func_gen_bytecode(
						ctx, mod, env, closure, num_closures, node);
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

					int bind_order[def->num_binds + node->call.num_args];

					struct ast_object_def_bind cons_binds[node->call.num_args];
					memset(&cons_binds, 0,
							sizeof(struct ast_object_def_bind) * node->call.num_args);

					bool cons_is_named = true;
					for (size_t i = 0; i < node->call.num_args; i++) {
						assert(i == 0 || cons_is_named == !!node->call.args[i].name);
						cons_is_named = !!node->call.args[i].name;
					}

					for (size_t i = 0; i < node->call.num_args; i++) {
						if (cons_is_named) {
							bool found = false;
							for (size_t j = 0; j < def->num_params; j++) {
								if (def->params[j].name == node->call.args[i].name) {
									cons_binds[i].target = def->params[j].param_id;
									found = true;
									break;
								}
							}

							assert(found);
						} else {
							cons_binds[i].target = def->params[i].param_id;
						}

						cons_binds[i].value_params = NULL;
						cons_binds[i].num_value_params = 0;
					}

					int err;
					err = ast_object_def_order_binds(
							ctx, mod, def, cons_binds, node->call.num_args, bind_order);
					if (err) {
						return AST_GEN_ERROR;
					}

					for (size_t i = 0; i < def->num_binds + node->call.num_args; i++) {
						size_t bind_i = bind_order[i];

						if(bind_i < def->num_binds) {
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
						} else {
							bind_i -= def->num_binds;
							assert(bind_i < node->call.num_args);
							struct ast_gen_bc_result arg;
							arg = ast_node_gen_bytecode(ctx, mod, env, info, bc_env,
									node->call.args[bind_i].value);
							AST_GEN_EXPECT_OK(arg);

							append_bc_instrs(&result, arg);
							append_bc_instr(&result,
									bc_gen_copy(bc_env,
										object_mbrs[cons_binds[bind_i].target], arg.out_var));
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
					if (obj.type == TYPE_UNSET) {
						return AST_GEN_ERROR;
					}

					result.first = result.last =
						bc_gen_load(bc_env, BC_VAR_NEW, obj);
					result.out_var = result.first->load.target;
				}
			}
			return result;

		case AST_NODE_FUNC_TYPE:
			{
				struct object obj = {0};

				int err;
				err = ast_slot_pack(ctx, mod, env, node->func_type.slot, &obj);
				if (err) {
					return AST_GEN_ERROR;
				}

				assert_type_equals(ctx->vm, obj.type, ctx->types.type);

				result.first = result.last =
					bc_gen_load(bc_env, BC_VAR_NEW, obj);
				result.out_var = result.first->load.target;
			}
			return result;

		case AST_NODE_TEMPL:
			{
				struct object obj = {0};
				obj.type = ctx->types.cons;
				obj.data = &node->templ.def;

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
								return AST_GEN_ERROR;
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
								return AST_GEN_ERROR;
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
								return AST_GEN_ERROR;
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
						return AST_GEN_ERROR;
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
					if (info->closures[node->lookup.ref.closure].lookup_failed) {
						stg_error(ctx->err, node->loc,
								"Name not found.");
						return AST_GEN_ERROR;
					} else if (info->closures[node->lookup.ref.closure].req ==
							AST_NAME_DEP_REQUIRE_VALUE) {
						struct bc_instr *lit_instr;
						lit_instr = bc_gen_load(bc_env, BC_VAR_NEW,
								info->closures[node->lookup.ref.closure].value);

						append_bc_instr(&result, lit_instr);

						result.out_var = lit_instr->load.target;

						return result;
					} else {
						stg_error(ctx->err, node->loc,
								"TODO: Closures");
						return AST_GEN_ERROR;
					}
					break;

				case AST_NAME_REF_TEMPL:
					panic("TODO: Pass template information to gen.");
					result.first = result.last = NULL;
					result.out_var = bc_alloc_param(
							bc_env, node->lookup.ref.param, node->type);
					return result;

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
			break;
	}

	printf("Invalid ast node in gen byte code.\n");
	return AST_GEN_ERROR;
}

struct bc_env *
ast_func_gen_bytecode(
		struct ast_context *ctx, struct ast_module *mod, struct ast_env *env,
		struct ast_typecheck_closure *closures, size_t num_closures, struct ast_node *node)
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
	info.num_closures = num_closures;

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
ast_gen_value_unpack_func(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, type_id value_type, size_t descendent)
{
	struct bc_env *bc_env = calloc(1, sizeof(struct bc_env));
	bc_env->vm = ctx->vm;
	bc_env->store = ctx->vm->instr_store;

	struct ast_gen_bc_result result = {0};

	result.out_var = bc_alloc_param(bc_env, 0, value_type);


	type_id current_type = value_type;
	while (descendent > 0) {
		struct type *type;
		type = vm_get_type(bc_env->vm, current_type);

		struct ast_object_def *def;
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
				num_desc = ast_object_def_num_descendant_members(
						ctx, mod, mbr_type->obj_def);
			} else {
				num_desc = 1;
			}

			assert(descendent >= offset);
			if (descendent < offset + num_desc) {

				assert(def->unpack_func);
				append_bc_instr(&result,
						bc_gen_push_arg(bc_env, result.out_var));
				append_bc_instr(&result,
						bc_gen_unpack(bc_env, BC_VAR_NEW,
							def->unpack_func, def->data,
							def->params[i].param_id, def->params[i].type));

				current_type = def->params[i].type;
				descendent -= offset;
				break;
			} else {
				offset += num_desc;
			}
		}
	}

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
