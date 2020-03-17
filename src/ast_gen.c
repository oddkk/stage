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

#define AST_GEN_ERROR ((struct ast_gen_bc_result){.err=-1})
#define AST_GEN_EXPECT_OK(res) do { if ((res).err) { return res; } } while (0);

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

static bool
ast_gen_dt_param_ref_equals(struct ast_gen_dt_ref lhs, struct ast_gen_dt_ref rhs)
{
	if (lhs.kind != rhs.kind) {
		return false;
	}

	switch (lhs.kind) {
		case AST_GEN_DT_PARAM_MEMBER:
			return lhs.member == rhs.member;

		case AST_GEN_DT_PARAM_INIT_EXPR:
			return lhs.init_expr == rhs.init_expr;
	}

	panic("Invalid gen_dt_param_req");
	return false;
}

static struct ast_gen_bc_result
ast_gen_get_dt_param(struct bc_env *bc_env,
		struct ast_gen_info *info, struct ast_gen_dt_ref ref)
{
	for (size_t i = 0; i < info->num_dt_params; i++) {
		struct ast_gen_dt_param *param;
		param = &info->dt_params[i];

		if (ast_gen_dt_param_ref_equals(param->ref, ref)) {
			struct ast_gen_bc_result result = {0};
			if (param->is_const) {
				append_bc_instr(&result,
						bc_gen_load(bc_env, BC_VAR_NEW,
							param->const_val));
				result.out_var = result.last->load.target;
			} else {
				result.first = result.last = NULL;
				result.out_var = bc_alloc_param(
						bc_env, i, param->type);
			}

			return result;
		}
	}

	panic("DT parameter was not found.");
	return AST_GEN_ERROR;
}

static struct ast_typecheck_closure
ast_gen_dt_resolve_closure(
		struct bc_env *bc_env,
		struct ast_gen_info *info,
		struct ast_gen_dt_ref ref)
{
	for (size_t i = 0; i < info->num_dt_params; i++) {
		struct ast_gen_dt_param *param;
		param = &info->dt_params[i];

		if (ast_gen_dt_param_ref_equals(param->ref, ref)) {
			struct ast_typecheck_closure res = {0};
			if (param->is_const) {
				res.req = AST_NAME_DEP_REQUIRE_VALUE;
				res.value = param->const_val;
			} else {
				res.req = AST_NAME_DEP_REQUIRE_TYPE;
				res.type = param->type;
			}

			return res;
		}
	}

	panic("DT parameter was not found.");
	return (struct ast_typecheck_closure){0};
}

static struct ast_typecheck_closure
ast_gen_resolve_closure(struct bc_env *bc_env,
		struct stg_module *mod, struct ast_gen_info *info, struct ast_name_ref ref)
{
	switch (ref.kind) {
		case AST_NAME_REF_NOT_FOUND:
			panic("Got unresolved name ref in code gen.");
			break;

		case AST_NAME_REF_MEMBER:
			{
				struct ast_gen_dt_ref dt_ref = {0};
				dt_ref.kind = AST_GEN_DT_PARAM_MEMBER;
				dt_ref.member = ref.member;

				return ast_gen_dt_resolve_closure(
						bc_env, info, dt_ref);
			}

		case AST_NAME_REF_INIT_EXPR:
			{
				struct ast_gen_dt_ref dt_ref = {0};
				dt_ref.kind = AST_GEN_DT_PARAM_INIT_EXPR;
				dt_ref.init_expr = ref.init_expr;

				return ast_gen_dt_resolve_closure(
						bc_env, info, dt_ref);
			}

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
				if (info->num_pattern_params > 0) {
					assert(ref.templ < info->num_pattern_params);

					struct ast_typecheck_closure res = {0};
					res.req = AST_NAME_DEP_REQUIRE_TYPE;
					res.lookup_failed = false;
					res.type = bc_get_var_type(bc_env,
							info->pattern_params[ref.templ]);
					return res;
				} else {
					assert(ref.templ < info->num_templ_values);

					struct ast_typecheck_closure res = {0};
					res.req = AST_NAME_DEP_REQUIRE_VALUE;
					res.lookup_failed = false;
					res.value = info->templ_values[ref.templ];
					return res;
				}
			}
			break;

		case AST_NAME_REF_USE:
			{
				assert(ref.use.id < info->num_use);

				struct object use_value;
				use_value = info->const_use_values[ref.use.id];

				type_id target_type_id;
				int err;
				err = object_cons_descendant_type(
						bc_env->vm, use_value.type,
						ref.use.param, &target_type_id);
				if (err) {
					printf("Failed to resolve the use target's type.\n");
					return (struct ast_typecheck_closure){0};
				}

				struct type *target_type;
				target_type = vm_get_type(bc_env->vm, target_type_id);

				uint8_t buffer[target_type->size];
				memset(buffer, 0, target_type->size);
				struct object obj = {0};
				obj.type = target_type_id;
				obj.data = buffer;

				err = object_unpack(
						bc_env->vm,
						use_value,
						ref.use.param, &obj);
				if (err) {
					printf("Failed to unpack use target.\n");
					return (struct ast_typecheck_closure){0};
				}
				// TODO: Is this necessary?
				obj = register_object(bc_env->vm, &mod->store, obj);

				struct ast_typecheck_closure res = {0};
				// TODO: Allow use of non-constant values.
				res.req = AST_NAME_DEP_REQUIRE_VALUE;
				res.lookup_failed = false;
				res.value = obj;
				return res;
			}
	}

	return (struct ast_typecheck_closure){0};
}

static struct ast_gen_bc_result
ast_unpack_gen_bytecode(struct ast_context *ctx, struct stg_module *mod,
		struct bc_env *bc_env, bc_var value, size_t descendent);

static struct ast_gen_bc_result
ast_name_ref_gen_bytecode(struct ast_context *ctx, struct stg_module *mod,
		struct ast_gen_info *info,
		struct bc_env *bc_env, struct ast_name_ref ref,
		struct stg_location loc, type_id type)
{
	struct ast_gen_bc_result result = {0};

	switch (ref.kind) {
		case AST_NAME_REF_MEMBER:
			{
				struct ast_gen_dt_ref dt_ref = {0};
				dt_ref.kind = AST_GEN_DT_PARAM_MEMBER;
				dt_ref.member = ref.member;

				return ast_gen_get_dt_param(
						bc_env, info, dt_ref);
			}

		case AST_NAME_REF_INIT_EXPR:
			{
				struct ast_gen_dt_ref dt_ref = {0};
				dt_ref.kind = AST_GEN_DT_PARAM_INIT_EXPR;
				dt_ref.init_expr = ref.init_expr;

				return ast_gen_get_dt_param(
						bc_env, info, dt_ref);
			}

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
			}
			break;

		case AST_NAME_REF_TEMPL:
			assert(ref.templ < info->num_pattern_params);
			result.first = result.last = NULL;
			assert(info->pattern_params[ref.templ] != BC_VAR_NEW);
			result.out_var = info->pattern_params[ref.templ];
			return result;

		case AST_NAME_REF_USE:
			{
				assert(ref.use.id < info->num_use);
				// TODO: Support use of non-constant values.
				assert(info->const_use_values[ref.use.id].type != TYPE_UNSET);
				append_bc_instr(&result,
						bc_gen_load(bc_env, BC_VAR_NEW,
							info->const_use_values[ref.use.id]));
				bc_var value_var;
				value_var = result.last->load.target;

				struct ast_gen_bc_result unpack_instrs;
				unpack_instrs = ast_unpack_gen_bytecode(
							ctx, mod, bc_env,
							value_var, ref.use.param);

				assert_type_equals(bc_env->vm, type,
						bc_get_var_type(bc_env, unpack_instrs.out_var));

				append_bc_instrs(&result, unpack_instrs);

				result.out_var = unpack_instrs.out_var;
			}
			return result;

		case AST_NAME_REF_NOT_FOUND:
			panic("Got failed lookup in code gen.");
			break;
	}

	printf("Name ref not handled in code gen\n");
	return AST_GEN_ERROR;
}

static struct ast_gen_bc_result
ast_unpack_gen_bytecode(struct ast_context *ctx, struct stg_module *mod,
		struct bc_env *bc_env, bc_var value, size_t descendent)
{
	struct ast_gen_bc_result result = {0};

	result.out_var = value;

	type_id current_type = bc_get_var_type(bc_env, value);
	while (descendent > 0) {
		struct type *type;
		type = vm_get_type(bc_env->vm, current_type);

		// As descendent is > 0, we expect this child to have children.
		assert(type->obj_inst);

		struct object_cons *def;
		def = type->obj_inst->cons;

		size_t offset = 1;
		for (size_t i = 0; i < def->num_params; i++) {
			struct type *mbr_type;
			mbr_type = vm_get_type(bc_env->vm,
					def->params[i].type);

			size_t num_desc;
			if (mbr_type->obj_inst) {
				num_desc = object_cons_num_descendants(
						ctx->vm, mbr_type->obj_inst->cons) + 1;
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

				result.out_var = result.last->unpack.target;
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

static struct ast_gen_bc_result
ast_pattern_true_or_fail(
		struct bc_env *bc_env, bc_var in,
		struct bc_instr *pattern_match_fail)
{
	struct ast_gen_bc_result result = {0};

	append_bc_instr(&result,
			bc_gen_push_arg(bc_env, in));
	append_bc_instr(&result,
			bc_gen_lnot(bc_env, BC_VAR_NEW));

	append_bc_instr(&result,
			bc_gen_push_arg(bc_env,
				result.last->lnot.target));
	append_bc_instr(&result,
			bc_gen_jmpif(bc_env,
				pattern_match_fail));

	return result;
}

static struct ast_gen_bc_result
ast_pattern_gen_match_expr(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_gen_info *info, struct bc_env *bc_env,
		struct ast_node *node, bc_var in,
		struct bc_instr *pattern_match_fail)
{
	struct ast_gen_bc_result expr;
	expr = ast_node_gen_bytecode(
			ctx, mod, info, bc_env, node);
	AST_GEN_EXPECT_OK(expr);

	struct bc_instr *match_instr;
	match_instr = bc_gen_testeq(bc_env, BC_VAR_NEW, in, expr.out_var);
	append_bc_instr(&expr, match_instr);

	append_bc_instrs(&expr,
		ast_pattern_true_or_fail(
			bc_env, match_instr->testeq.target,
			pattern_match_fail));

	return expr;
}

static struct ast_gen_bc_result
ast_pattern_gen_match_unpack(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_gen_info *info, struct bc_env *bc_env,
		struct ast_node *node, bc_var in,
		bc_var *params_out, size_t num_params,
		struct bc_instr *pattern_match_fail)
{
	struct ast_gen_bc_result result = {0};
	type_id in_type = bc_get_var_type(bc_env, in);

	assert_type_equals(ctx->vm, in_type, node->type);

	switch (node->kind) {
		case AST_NODE_CALL:
			if (node->call.func_val.func == FUNC_UNSET) {
				return ast_pattern_gen_match_expr(
						ctx, mod, info, bc_env, node, in,
						pattern_match_fail);
			}

			{
				struct func *func;
				func = vm_get_func(ctx->vm, node->call.func_val.func);
				// We can only match on cons functions
				if (func->kind != FUNC_CONS) {
					return ast_pattern_gen_match_expr(
							ctx, mod, info, bc_env, node, in,
							pattern_match_fail);
				}

				if (func->cons->can_unpack) {
					append_bc_instr(&result,
						bc_gen_push_arg(bc_env, in));
					append_bc_instr(&result,
						bc_gen_test_unpack(bc_env, BC_VAR_NEW,
								func->cons->can_unpack,
								func->cons->data));

					append_bc_instrs(&result,
							ast_pattern_true_or_fail(
								bc_env, result.last->test_unpack.target,
								pattern_match_fail));
				}

				assert(node->call.num_args == func->cons->num_params);

				for (size_t i = 0; i < func->cons->num_params; i++) {
					append_bc_instr(&result,
						bc_gen_push_arg(bc_env, in));
					append_bc_instr(&result,
						bc_gen_unpack(bc_env, BC_VAR_NEW,
								func->cons->unpack,
								func->cons->data, i,
								func->cons->params[i].type));

					bc_var param_var;
					param_var = result.last->unpack.target;

					struct ast_gen_bc_result sub_match;
					sub_match = ast_pattern_gen_match_unpack(
							ctx, mod, info, bc_env,
							node->call.args[i].value, param_var,
							params_out, num_params,
							pattern_match_fail);
					AST_GEN_EXPECT_OK(sub_match);

					append_bc_instrs(&result,
							sub_match);
				}
			}
			break;

		case AST_NODE_CONS:
			panic("TODO: Pattern match cons.");
			return ast_pattern_gen_match_expr(
					ctx, mod, info, bc_env, node, in,
					pattern_match_fail);

		case AST_NODE_INST:
			panic("TODO: Pattern match inst.");
			return ast_pattern_gen_match_expr(
					ctx, mod, info, bc_env, node, in,
					pattern_match_fail);

		case AST_NODE_LOOKUP:
			if (node->lookup.ref.kind != AST_NAME_REF_TEMPL) {
				return ast_pattern_gen_match_expr(
						ctx, mod, info, bc_env, node, in,
						pattern_match_fail);
			}

			assert(node->lookup.ref.templ < num_params);
			if (params_out[node->lookup.ref.templ] == BC_VAR_NEW) {
				append_bc_instr(&result,
						bc_gen_copy(bc_env, params_out[node->lookup.ref.templ], in));
				params_out[node->lookup.ref.templ] = result.last->copy.target;
			}
			break;

		case AST_NODE_WILDCARD:
			// Wildcards accept all values.
			break;

		default:
			return ast_pattern_gen_match_expr(
					ctx, mod, info, bc_env, node, in,
					pattern_match_fail);
	}

	return result;
}

struct ast_gen_bc_result
ast_node_gen_bytecode(struct ast_context *ctx, struct stg_module *mod,
		struct ast_gen_info *info, struct bc_env *bc_env, struct ast_node *node)
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
							bc_env, mod, info, cls->ref);

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
								ctx, mod, info, bc_env,
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
						ctx, mod, closure,
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
				fid = stg_register_func(mod, new_func);

				struct object func_obj = {0};
				func_obj = stg_register_func_object(
						ctx->vm, &mod->store, fid, NULL);

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

				if ((native_func->flags & STG_NATIVE_FUNC_HEAP) != 0) {
					new_func.flags |= FUNC_HEAP;
				}

				if ((native_func->flags & STG_NATIVE_FUNC_REFS) != 0) {
					new_func.flags |= FUNC_REFS;
				}

				func_id fid;
				fid = stg_register_func(mod, new_func);

				struct object func_obj = {0};
				func_obj = stg_register_func_object(
						ctx->vm, &mod->store, fid, closure);

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
					arg = ast_node_gen_bytecode(ctx, mod, info, bc_env,
							node->call.args[i].value);
					if (arg.err) {
						err = -1;
						continue;
					}

					append_bc_instrs(&result, arg);
					params[i] = arg.out_var;
				}

				struct ast_gen_bc_result func;
				func = ast_node_gen_bytecode(ctx, mod, info, bc_env,
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
				assert(inst);

				struct object_inst_bind       extra_binds[node->call.num_args];
				struct object_inst_extra_expr extra_exprs[node->call.num_args];

				memset(extra_binds, 0,
						sizeof(struct object_inst_bind) * node->call.num_args);
				memset(extra_exprs, 0,
						sizeof(struct object_inst_extra_expr) * node->call.num_args);

				int local_descs[inst->cons->num_params];
				object_cons_local_descendent_ids(
						ctx->vm, inst->cons, local_descs);

				for (size_t i = 0; i < node->call.num_args; i++) {
					extra_binds[i].unpack_id = 0;
					extra_binds[i].expr_id = inst->num_exprs + i;
					extra_binds[i].loc = node->call.args[i].value->loc;
					extra_binds[i].overridable = false;

					extra_binds[i].target_id = -1;
					for (size_t mbr_i = 0; mbr_i < inst->cons->num_params; mbr_i++) {
						if (inst->cons->params[mbr_i].name == node->call.args[i].name) {
							extra_binds[i].target_id = local_descs[mbr_i];
							break;
						}
					}
					assert(extra_binds[i].target_id >= 0);

					extra_exprs[i].deps = NULL;
					extra_exprs[i].num_deps = 0;
					extra_exprs[i].loc = node->call.args[i].value->loc;

					extra_exprs[i].type = node->call.args[i].value->type;
					assert(extra_exprs[i].type != TYPE_UNSET);
				}

				struct object_inst_action *actions = NULL;
				size_t num_actions = 0;
				int err;
				err = object_inst_order(
						ctx->vm, ctx->err, inst,
						extra_exprs, node->call.num_args,
						extra_binds, node->call.num_args,
						&actions, &num_actions,
						node->loc);
				if (err) {
					printf("Failed to instantiate object.\n");
					return AST_GEN_ERROR;
				}

				const size_t num_exprs =
					inst->num_exprs + node->call.num_args;
				bc_var expr_vars[num_exprs];

				for (size_t i = 0; i < num_exprs; i++) {
					expr_vars[i] = BC_VAR_NEW;
				}

				// The resulting object itself gets member id 0.
				const size_t num_desc_members =
					1 + object_cons_num_descendants(
							ctx->vm, inst->cons);
				bc_var member_vars[num_desc_members];

				for (size_t i = 0; i < num_desc_members; i++) {
					member_vars[i] = BC_VAR_NEW;
				}

				for (size_t act_i = 0; act_i < num_actions; act_i++) {
					struct object_inst_action *act;
					act = &actions[act_i];

					switch (act->op) {
						case OBJ_INST_EXPR:
							{
								assert(act->expr.id < num_exprs);
								assert(expr_vars[act->expr.id] == BC_VAR_NEW);

								int expr_id = act->expr.id;

								if (expr_id < inst->num_exprs) {
									struct object_inst_expr *expr;
									expr = &inst->exprs[expr_id];

									if (expr->constant) {
										struct bc_instr *instr;
										instr = bc_gen_load(
												bc_env, BC_VAR_NEW,
												expr->const_value);
										assert(expr_vars[expr_id] == BC_VAR_NEW);
										expr_vars[expr_id] = instr->load.target;
										append_bc_instr(&result, instr);
									} else {
										for (size_t i = 0; i < act->expr.num_deps; i++) {
											switch (act->expr.deps[i].kind) {
												case OBJECT_INST_DEP_MEMBER:
													{
														assert(act->expr.deps[i].member < num_desc_members);
														assert(member_vars[act->expr.deps[i].member] != BC_VAR_NEW);
														append_bc_instr(&result,
																bc_gen_push_arg(
																	bc_env, member_vars[act->expr.deps[i].member]));
													}
													break;

												case OBJECT_INST_DEP_INIT_EXPR:
													panic("TODO: Init exprs");
													break;
											}
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
											ctx, mod, info, bc_env,
											node->call.args[expr_id].value);
									AST_GEN_EXPECT_OK(expr);

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
								mbr_cons = mbr_type->obj_inst->cons;


								assert(mbr_cons);

								int local_mbrs[mbr_cons->num_params];
								object_cons_local_descendent_ids(
										ctx->vm, mbr_cons, local_mbrs);

								for (size_t i = 0; i < mbr_cons->num_params; i++) {
									int param_id = act->pack.member_id + local_mbrs[i];
									assert(param_id < num_desc_members);
									assert(member_vars[param_id] != BC_VAR_NEW);

									append_bc_instr(&result,
											bc_gen_push_arg(
												bc_env, member_vars[param_id]));
								}

								assert(mbr_cons->pack);

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

				// The top level object has ID 0.
				assert(member_vars[0] != BC_VAR_NEW);
				result.out_var = member_vars[0];
			}
			return result;

		case AST_NODE_CONS:
			{
				assert(node->call.cons_value.type != TYPE_UNSET);

				struct bc_instr *lit_instr;
				lit_instr = bc_gen_load(
						bc_env, BC_VAR_NEW,
						node->call.cons_value);

				append_bc_instr(&result, lit_instr);

				result.out_var = lit_instr->load.target;
			}
			return result;

		case AST_NODE_FUNC_TYPE:
			{
				assert(node->func_type.func_type != TYPE_UNSET);
				struct object obj = {0};
				obj.type = ctx->vm->default_types.type;
				obj.data = &node->func_type.func_type;

				obj = register_object(ctx->vm, &mod->store, obj);

				result.first = result.last =
					bc_gen_load(bc_env, BC_VAR_NEW, obj);
				result.out_var = result.first->load.target;
			}
			return result;

		case AST_NODE_TEMPL:
			{
				struct object obj = {0};
				obj.type = ctx->vm->default_types.cons;
				obj.data = &node->templ.cons;

				obj = register_object(ctx->vm, &mod->store, obj);

				result.first = result.last =
					bc_gen_load(bc_env, BC_VAR_NEW, obj);
				result.out_var = result.first->load.target;
			}
			return result;

		case AST_NODE_ACCESS:
			{
				if (node->access.const_target_value_type != TYPE_UNSET) {
					struct type *target_type;
					target_type = vm_get_type(ctx->vm,
							node->access.const_target_value_type);
					struct object static_obj;
					static_obj = target_type->static_object;

					struct type *val_type;
					val_type = vm_get_type(ctx->vm, static_obj.type);

					if (!val_type->obj_inst) {
						stg_error(ctx->err, node->access.target->loc,
								"This type does not have a static scope.");
						return AST_GEN_ERROR;
					}

					ssize_t unpack_id;
					unpack_id = object_cons_find_param_unpack_id(
							ctx->vm, val_type->obj_inst->cons,
							node->access.name);

					if (unpack_id < 0) {
						stg_error(ctx->err, node->loc,
								"This type has no static member '%.*s'.",
								ALIT(node->access.name));
						return AST_GEN_ERROR;
					}

					type_id res_type_id;

					int err;
					err = object_cons_descendant_type(
							ctx->vm, static_obj.type, unpack_id, &res_type_id);
					assert(!err);

					struct type *res_type;
					res_type = vm_get_type(ctx->vm, res_type_id);

					uint8_t buffer[res_type->size];
					struct object res;
					res.type = res_type_id;
					res.data = buffer;

					err = object_unpack(ctx->vm, static_obj,
							unpack_id, &res);
					assert(!err);

					res = register_object(ctx->vm, &mod->store, res);

					append_bc_instr(&result,
							bc_gen_load(bc_env, BC_VAR_NEW, res));
					result.out_var = result.last->load.target;

				} else {
					struct ast_gen_bc_result target = {0};

					target = ast_node_gen_bytecode(
							ctx, mod, info, bc_env,
							node->access.target);
					AST_GEN_EXPECT_OK(target);

					append_bc_instrs(&result, target);
					append_bc_instr(&result,
							bc_gen_push_arg(bc_env, target.out_var));

					type_id target_type_id = bc_get_var_type(bc_env, target.out_var);

					if (type_equals(ctx->vm, target_type_id, ctx->vm->default_types.type)) {
						stg_error(ctx->err, node->loc,
								"Access to a type's static scope requires the "
								"type to be constant. Got a non-constant type.");
						return AST_GEN_ERROR;
					}


					struct type *type = vm_get_type(ctx->vm, target_type_id);

					// TODO: Support deconstruction using non-default constructors
					// from pattern matching.
					assert(type->obj_inst);

					struct object_cons *cons;
					cons = type->obj_inst->cons;

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
			}
			return result;

		case AST_NODE_LOOKUP:
			return ast_name_ref_gen_bytecode(
					ctx, mod, info, bc_env,
					node->lookup.ref, node->loc, node->type);
			break;

		case AST_NODE_INIT_EXPR:
			{
				struct ast_name_ref ref = {0};
				ref.kind = AST_NAME_REF_INIT_EXPR;
				ref.init_expr = node->init_expr.id;

				return ast_name_ref_gen_bytecode(
						ctx, mod, info, bc_env,
						ref, node->loc, node->type);
			}

		case AST_NODE_LIT:
			{
				struct bc_instr *lit_instr;
				lit_instr = bc_gen_load(bc_env, BC_VAR_NEW, node->lit.obj);

				append_bc_instr(&result, lit_instr);

				result.out_var = lit_instr->load.target;
			}
			return result;

		case AST_NODE_LIT_NATIVE:
			{
				struct object obj;

				int err;
				err = stg_mod_lookup_native_object(
						mod, node->lit_native.name, &obj);
				// Missing objects should have been caught by the typecheck.
				assert(!err);

				struct bc_instr *lit_instr;
				lit_instr = bc_gen_load(bc_env, BC_VAR_NEW, obj);

				append_bc_instr(&result, lit_instr);

				result.out_var = lit_instr->load.target;
			}
			return result;

		case AST_NODE_MOD:
			{
				struct stg_module *mod_ref = NULL;
				mod_ref = stg_mod_find_module(
						mod, node->mod.name);
				assert(mod_ref);

				struct bc_instr *mod_instr;
				mod_instr = bc_gen_load(bc_env, BC_VAR_NEW, mod_ref->instance);

				append_bc_instr(&result, mod_instr);

				result.out_var = mod_instr->load.target;
			}
			return result;

		case AST_NODE_MATCH:
			{
				struct ast_gen_bc_result value_instrs;
				value_instrs = ast_node_gen_bytecode(
						ctx, mod, info, bc_env,
						node->match.value);
				append_bc_instrs(&result, value_instrs);
				bc_var value = value_instrs.out_var;

				bc_var res = bc_alloc_var(bc_env, node->type);

				struct bc_instr *end_instr;
				end_instr = bc_gen_nop(bc_env);

				// TODO: A match inside a match will currently not receive the
				// pattern parameters.
				bc_var *prev_pattern_params    = info->pattern_params;
				size_t prev_num_pattern_params = info->num_pattern_params;

				info->pattern_params = NULL;
				info->num_pattern_params = 0;

				for (size_t i = 0; i < node->match.num_cases; i++) {
					struct ast_match_case *match_case = &node->match.cases[i];
					bc_var params_vars[match_case->pattern.num_params];
					for (size_t i = 0; i < match_case->pattern.num_params; i++) {
						params_vars[i] = BC_VAR_NEW;
					}

					struct bc_instr *next_case;
					next_case = bc_gen_nop(bc_env);

					struct ast_gen_bc_result pattern_instrs;
					pattern_instrs = ast_pattern_gen_match_unpack(
							ctx, mod, info, bc_env, match_case->pattern.node, value,
							params_vars, match_case->pattern.num_params,
							next_case);
					AST_GEN_EXPECT_OK(pattern_instrs);
					append_bc_instrs(&result, pattern_instrs);

					info->pattern_params = params_vars;
					info->num_pattern_params = match_case->pattern.num_params;

					struct ast_gen_bc_result res_instrs;
					res_instrs = ast_node_gen_bytecode(
							ctx, mod, info, bc_env,
							match_case->expr);
					AST_GEN_EXPECT_OK(res_instrs);
					append_bc_instrs(&result, res_instrs);

					info->pattern_params = NULL;
					info->num_pattern_params = 0;


					append_bc_instr(&result,
							bc_gen_copy(bc_env, res,
								res_instrs.out_var));

					append_bc_instr(&result,
							bc_gen_jmp(bc_env, end_instr));

					// If the match fails we will jump here to try the next case.
					append_bc_instr(&result, next_case);

				}

				info->pattern_params     = prev_pattern_params;
				info->num_pattern_params = prev_num_pattern_params;

				append_bc_instr(&result, end_instr);

				result.out_var = res;
			}
			return result;

		case AST_NODE_WILDCARD:
			// Wildcards can not appear as part of normal expressions.
			break;

		case AST_NODE_COMPOSITE:
			{
				type_id type;

				type = node->composite.type;

				if (type == TYPE_UNSET) {
					return AST_GEN_ERROR;
				}

				struct object obj;
				obj.type = ctx->vm->default_types.type;
				obj.data = &type;

				struct bc_instr *lit_instr;
				lit_instr = bc_gen_load(bc_env, BC_VAR_NEW,
						register_object(ctx->vm, &mod->store, obj));

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
				obj.type = ctx->vm->default_types.type;
				obj.data = &type;

				struct bc_instr *lit_instr;
				lit_instr = bc_gen_load(bc_env, BC_VAR_NEW,
						register_object(ctx->vm, &mod->store, obj));

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
		struct ast_context *ctx, struct stg_module *mod,
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

	size_t pruned_i = 0;
	for (size_t i = 0; i < num_closures; i++) {
		if (closure_refs[i] != AST_BC_CLOSURE_PRUNED) {
			assert(closure_refs[i] < num_unpruned_closures);
			assert(closures[i].req == AST_NAME_DEP_REQUIRE_TYPE);
			assert(closure_types[pruned_i] == TYPE_UNSET);

			closure_types[pruned_i] = closures[i].type;
			pruned_i += 1;
		}
	}

	for (size_t i = 0; i < num_unpruned_closures; i++) {
		assert(closure_types[i] != TYPE_UNSET);
	}
	bc_env->num_closures = num_unpruned_closures;
	bc_env->closure_types = closure_types;

	struct ast_gen_bc_result func_instr;
	func_instr = ast_node_gen_bytecode(ctx, mod, &info,
			bc_env, node->func.body);
	if (func_instr.err) {
		return NULL;
	}

	assert_type_equals(ctx->vm,
			bc_get_var_type(bc_env, func_instr.out_var),
			node->func.body->type);

	append_bc_instr(&func_instr,
			bc_gen_ret(bc_env, func_instr.out_var));

	bc_env->entry_point = func_instr.first;

#if AST_GEN_SHOW_BC
	printf("\nbc:\n");
	bc_print(bc_env, bc_env->entry_point);
#endif

	bc_env->nbc = arena_alloc(&mod->mem, sizeof(struct nbc_func));
	nbc_compile_from_bc(&mod->vm->transient, &mod->mem, bc_env->nbc, bc_env);

#if AST_GEN_SHOW_BC
	printf("\nnbc:\n");
	nbc_print(bc_env->nbc);
#endif

	return bc_env;
}

struct bc_env *
ast_composite_bind_gen_bytecode(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_gen_dt_param *dt_params, size_t num_dt_params,
		struct object *const_use_values, size_t num_use,
		struct ast_typecheck_closure *closures, size_t num_closures, struct ast_node *expr)
{
	struct bc_env *bc_env = arena_alloc(&mod->mem, sizeof(struct bc_env));
	bc_env->vm = ctx->vm;
	bc_env->store = ctx->vm->instr_store;

	/*
	 * TODO: Assert that the returned value in byte code has the same type we
	 * expect.
	*/

	struct ast_gen_info info = {0};

	info.dt_params = dt_params;
	info.num_dt_params = num_dt_params;

	info.const_use_values = const_use_values;
	info.num_use = num_use;

	info.closures = closures;
	info.num_closures = num_closures;

	for (size_t i = 0; i < num_dt_params; i++) {
		bc_alloc_param(bc_env, i, dt_params[i].type);
	}

	struct ast_gen_bc_result func_instr;
	func_instr = ast_node_gen_bytecode(ctx, mod, &info,
			bc_env, expr);
	if (func_instr.err) {
		return NULL;
	}

	assert_type_equals(ctx->vm,
			bc_get_var_type(bc_env, func_instr.out_var),
			expr->type);

	append_bc_instr(&func_instr,
			bc_gen_ret(bc_env, func_instr.out_var));

	bc_env->entry_point = func_instr.first;

#if AST_GEN_SHOW_BC
	printf("\nbc:\n");
	bc_print(bc_env, bc_env->entry_point);
#endif

	bc_env->nbc = arena_alloc(&mod->mem, sizeof(struct nbc_func));
	nbc_compile_from_bc(&mod->vm->transient, &mod->mem, bc_env->nbc, bc_env);

#if AST_GEN_SHOW_BC
	printf("\nnbc:\n");
	nbc_print(bc_env->nbc);
#endif

	return bc_env;
}

struct bc_env *
ast_type_expr_gen_bytecode(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *expr,
		struct ast_typecheck_closure *closures, size_t num_closures)
{
	struct bc_env *bc_env = arena_alloc(&mod->mem, sizeof(struct bc_env));
	bc_env->vm = ctx->vm;
	bc_env->store = ctx->vm->instr_store;

	struct ast_gen_info info = {0};

	info.closures = closures;
	info.closure_refs = NULL;
	info.num_closures = num_closures;

	struct ast_gen_bc_result func_instr;
	func_instr = ast_node_gen_bytecode(ctx, mod, &info,
			bc_env, expr);
	if (func_instr.err) {
		return NULL;
	}

	assert_type_equals(ctx->vm,
			bc_get_var_type(bc_env, func_instr.out_var),
			ctx->vm->default_types.type);

	append_bc_instr(&func_instr,
			bc_gen_ret(bc_env, func_instr.out_var));

	bc_env->entry_point = func_instr.first;

#if AST_GEN_SHOW_BC
	printf("\nbc:\n");
	bc_print(bc_env, bc_env->entry_point);
#endif

	bc_env->nbc = arena_alloc(&mod->mem, sizeof(struct nbc_func));
	nbc_compile_from_bc(&mod->vm->transient, &mod->mem, bc_env->nbc, bc_env);

#if AST_GEN_SHOW_BC
	printf("\nnbc:\n");
	nbc_print(bc_env->nbc);
#endif

	return bc_env;
}

struct bc_env *
ast_gen_value_unpack_func(
		struct ast_context *ctx, struct stg_module *mod,
		type_id value_type, size_t descendent)
{
	struct bc_env *bc_env = arena_alloc(&mod->mem, sizeof(struct bc_env));
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

	bc_env->nbc = arena_alloc(&mod->mem, sizeof(struct nbc_func));
	nbc_compile_from_bc(&mod->vm->transient, &mod->mem, bc_env->nbc, bc_env);

#if AST_GEN_SHOW_BC
	printf("\nunpack nbc:\n");
	nbc_print(bc_env->nbc);
#endif

	return bc_env;
}
