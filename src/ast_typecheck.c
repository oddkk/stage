#include "ast.h"
#include "vm.h"
#include "module.h"
#include "utils.h"
#include "error.h"
#include "term_color.h"
#include "base/mod.h"
#include "type_class.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool
ast_name_ref_equals(struct ast_name_ref lhs, struct ast_name_ref rhs)
{
	if (lhs.kind != rhs.kind) {
		return false;
	}
	switch (lhs.kind) {
		case AST_NAME_REF_NOT_FOUND:
			return true;
		case AST_NAME_REF_MEMBER:
			return lhs.member.id == rhs.member.id
				&& lhs.member.unpack_id == rhs.member.unpack_id;
		case AST_NAME_REF_PARAM:
			return lhs.param == rhs.param;
		case AST_NAME_REF_CLOSURE:
			return lhs.closure == rhs.closure;
		case AST_NAME_REF_TEMPL:
			return lhs.templ == rhs.templ;
		case AST_NAME_REF_USE:
			return lhs.use.id == rhs.use.id &&
				lhs.use.param == rhs.use.param;
		case AST_NAME_REF_INIT_EXPR:
			return lhs.init_expr == rhs.init_expr;
		case AST_NAME_REF_SELF:
			return lhs.self_offset == rhs.self_offset;
	}

	return false;
}

struct ast_typecheck_dep *
ast_find_dep(struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_name_ref ref)
{
	for (size_t j = 0; j < num_deps; j++) {
		if (ast_name_ref_equals(deps[j].ref, ref)) {
			return &deps[j];
		}
	}

	return NULL;
}

// If NULL is passed as env, the value member of all out_deps will be set to -1.
void
ast_constr_fill_closure_deps(struct ast_context *ctx, struct ast_env *env,
		struct ast_typecheck_dep *out_deps, struct ast_closure_target *closure,
		struct ast_typecheck_dep *deps, size_t num_deps)
{
	for (size_t i = 0; i < closure->num_members; i++) {
		struct ast_closure_member *mbr;
		mbr = &closure->members[i];
		out_deps[i].ref.kind = AST_NAME_REF_CLOSURE;
		out_deps[i].ref.closure = i;

		struct ast_typecheck_dep *in_dep;
		in_dep = ast_find_dep(deps, num_deps, mbr->ref);
		assert(in_dep);

		out_deps[i].lookup_failed = in_dep->lookup_failed;
		if (env) {
			out_deps[i].value = ast_slot_alloc(env);
		} else {
			out_deps[i].value = -1;
		}

		if (in_dep->determined) {
			if (mbr->require_const) {
				assert(in_dep->determined &&
						in_dep->req == AST_NAME_DEP_REQUIRE_VALUE);
				out_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;

				if (env) {
					// TODO: Location
					ast_slot_require_is_obj(
							env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
							out_deps[i].value, in_dep->val);
				}

				out_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;
				out_deps[i].determined = true;
				out_deps[i].val = in_dep->val;
			} else {
				assert(in_dep->determined);
				out_deps[i].req = AST_NAME_DEP_REQUIRE_TYPE;

				if (in_dep->req == AST_NAME_DEP_REQUIRE_TYPE) {
					if (env) {
						ast_slot_id type_slot;
						type_slot = ast_slot_alloc(env);

						// TODO: Location
						ast_slot_require_is_type(
								env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
								type_slot, in_dep->type);

						ast_slot_require_type(
								env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
								out_deps[i].value, type_slot);
					}

					out_deps[i].type = in_dep->type;
				} else {
					if (env) {
						ast_slot_require_is_obj(
								env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
								out_deps[i].value, in_dep->val);
					}

					out_deps[i].val = in_dep->val;
				}

				out_deps[i].req = in_dep->req;
				out_deps[i].determined = true;
			}
		} else {
			out_deps[i].determined = false;
			if (env) {
				out_deps[i].value = in_dep->value;
			}
		}

		assert(env || out_deps[i].value == -1);
	}
}

// closure_values is expected to be an array of length closure->num_members.
void
ast_fill_closure(struct ast_closure_target *closure,
		struct ast_typecheck_closure *closure_values,
		struct ast_typecheck_dep *deps, size_t num_deps)
{
	for (size_t i = 0; i < closure->num_members; i++) {
		struct ast_typecheck_dep *dep;
		dep = ast_find_dep(deps, num_deps,
				closure->members[i].ref);
		assert(dep);

		if (dep->lookup_failed) {
			closure_values[i].req = AST_NAME_DEP_REQUIRE_VALUE;
			closure_values[i].lookup_failed = true;
		} else if (closure->members[i].require_const || (
					dep->determined &&
					dep->req == AST_NAME_DEP_REQUIRE_VALUE)) {
			closure_values[i].req = AST_NAME_DEP_REQUIRE_VALUE;

			assert(dep->determined && dep->req == AST_NAME_DEP_REQUIRE_VALUE);
			closure_values[i].value = dep->val;
		} else {
			assert(dep->determined && dep->req == AST_NAME_DEP_REQUIRE_TYPE);
			closure_values[i].req = AST_NAME_DEP_REQUIRE_TYPE;
			closure_values[i].type = dep->type;
		}
	}
}

static ast_slot_id
ast_func_proto_constraints(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_env *env, struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_node *ret_type, struct ast_node **param_types,
		size_t num_params, struct stg_location decl_loc)
{
	ast_slot_id param_type_slots[num_params];
	ast_slot_id ret_type_slot;

	for (size_t i = 0; i < num_params; i++) {
		if (param_types[i]) {
			param_type_slots[i] =
				ast_node_constraints(
						ctx, mod, env, deps, num_deps,
						param_types[i]);
		} else {
			param_type_slots[i] =
				ast_slot_alloc(env);
		}
	}

	if (ret_type) {
		ret_type_slot =
			ast_node_constraints(
					ctx, mod, env, deps, num_deps,
					ret_type);
	} else {
		ret_type_slot =
			ast_slot_alloc(env);
	}

	ast_slot_id func_type;
	func_type = ast_slot_alloc(env);

	ast_slot_require_is_func_type(
			env, decl_loc, AST_CONSTR_SRC_FUNC_DECL,
			ctx->vm->default_types.type, func_type, ret_type_slot,
			param_type_slots, num_params);

	return func_type;
}

void
ast_node_resolve_datatypes(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_node *node)
{
	// TODO: The deps are not being transformed when passing into functions.

	switch (node->kind) {
		case AST_NODE_TEMPL:
			if (!node->templ.cons) {
				size_t num_body_deps = node->templ.closure.num_members;
				struct ast_typecheck_dep body_deps[num_body_deps];
				memset(body_deps, 0, sizeof(struct ast_typecheck_dep) * num_body_deps);
				ast_constr_fill_closure_deps(ctx, NULL,
						body_deps, &node->templ.closure,
						deps, num_deps);

				node->templ.cons = ast_node_create_templ(
						ctx, mod, node,
						deps, num_deps,
						body_deps, num_body_deps);

				if (!node->templ.cons) {
					node->templ.failed = true;
				}
			}
			break;

		case AST_NODE_TYPE_CLASS:
			stg_type_class_from_ast_node(
					ctx, mod, node, deps, num_deps);
			break;

		case AST_NODE_VARIANT:
			if (node->variant.type == TYPE_UNSET) {
				node->variant.type =
					ast_dt_finalize_variant(
							ctx, mod,
							node->variant.options,
							node->variant.num_options,
							deps, num_deps);

				if (node->variant.type == TYPE_UNSET) {
					node->variant.failed = true;
				}
			}
			break;

			/*
		case AST_NODE_COMPOSITE:
			if (node->composite.type == TYPE_UNSET) {
				struct ast_closure_target *closure;
				closure = &node->composite.closure;

				struct ast_typecheck_closure closure_values[closure->num_members];
				memset(closure_values, 0,
						sizeof(struct ast_typecheck_closure) * closure->num_members);

				ast_fill_closure(closure, closure_values,
						deps, num_deps);

				node->composite.type =
					ast_dt_finalize_composite(
							ctx, mod, node,
							closure_values, closure->num_members);

				if (node->composite.type == TYPE_UNSET) {
					node->composite.failed = true;
				}
			}
			break;

		case AST_NODE_DATA_TYPE:
			{
				struct ast_node *dt_node;
				dt_node = ast_module_node_get_data_type(
						ctx->vm, node);

				ast_node_resolve_datatypes(
						ctx, mod, deps, num_deps, dt_node);
			}
			*/

		default:
			break;
	}

#define VISIT_NODE(child) \
	ast_node_resolve_datatypes( \
			ctx, mod, deps, num_deps, child);
	AST_NODE_VISIT(node, false, true, false);
#undef VISIT_NODE
}

ast_slot_id
ast_node_constraints(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_env *env, struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_node *node)
{
	node->typecheck_slot = -1;

	switch (node->kind) {
		case AST_NODE_FUNC:
			{
				ast_slot_id body_slot, type_slot;

				struct ast_node *param_type_nodes[node->func.num_params];

				for (size_t i = 0; i < node->func.num_params; i++) {
					param_type_nodes[i] = node->func.params[i].type;
				}

				type_slot = ast_func_proto_constraints(
						ctx, mod, env, deps, num_deps,
						node->func.return_type,
						param_type_nodes, node->func.num_params,
						node->loc);

				size_t num_body_deps =
					node->func.closure.num_members +
					node->func.num_params;

				struct ast_typecheck_dep body_deps[num_body_deps];
				memset(body_deps, 0, sizeof(struct ast_typecheck_dep) * num_body_deps);

				for (size_t i = 0; i < node->func.num_params; i++) {
					body_deps[i].req = AST_NAME_DEP_REQUIRE_TYPE;
					body_deps[i].ref.kind = AST_NAME_REF_PARAM;
					body_deps[i].ref.param = i;
					body_deps[i].lookup_failed = false;
					body_deps[i].determined = false;

					ast_slot_id param_type_slot = ast_slot_alloc(env);
					// TODO: Location
					// Param i is index i+1 becaues index 0 is the return
					// value.
					ast_slot_require_param_index(
							env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
							type_slot, i+1, param_type_slot);

					body_deps[i].value = ast_slot_alloc(env);
					ast_slot_require_type(env,
							node->func.params[i].type
							? node->func.params[i].type->loc
							: node->loc,
							AST_CONSTR_SRC_FUNC_DECL,
							body_deps[i].value, param_type_slot);
				}

				ast_constr_fill_closure_deps(ctx, env,
						body_deps + node->func.num_params, &node->func.closure,
						deps, num_deps);

				ast_slot_id ret_type_slot;
				ret_type_slot = ast_slot_alloc(env);

				body_slot = ast_node_constraints(
						ctx, mod, env, body_deps, num_body_deps, node->func.body);
				ast_slot_require_type(
						env, node->loc, AST_CONSTR_SRC_FUNC_DECL,
						body_slot, ret_type_slot);

				// Index 0 is the return type of the function.
				ast_slot_require_param_index(
						env, node->loc, AST_CONSTR_SRC_FUNC_DECL,
						type_slot, 0, ret_type_slot);

				ast_slot_id func_value_slot;
				func_value_slot = ast_slot_alloc(env);

				ast_slot_require_type(
						env, node->loc, AST_CONSTR_SRC_FUNC_DECL,
						func_value_slot, type_slot);

				node->typecheck_slot = func_value_slot;
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			{
				ast_slot_id type_slot;

				struct ast_node *param_type_nodes[node->func.num_params];

				for (size_t i = 0; i < node->func.num_params; i++) {
					param_type_nodes[i] = node->func.params[i].type;
				}

				type_slot = ast_func_proto_constraints(
						ctx, mod, env, deps, num_deps,
						node->func.return_type,
						param_type_nodes, node->func.num_params,
						node->loc);

				ast_slot_id func_value_slot;
				func_value_slot = ast_slot_alloc(env);

				ast_slot_require_type(
						env, node->loc, AST_CONSTR_SRC_FUNC_DECL,
						func_value_slot, type_slot);

				node->typecheck_slot = func_value_slot;
			}
			break;

		case AST_NODE_CALL:
			{
				ast_slot_id in_func_slot, func_slot, func_type_slot;

				in_func_slot = ast_node_constraints(
						ctx, mod, env, deps, num_deps,
						node->call.func);

				func_slot = in_func_slot;

				// TODO: Fix cons_or_value_from.
				// func_slot = ast_slot_alloc(env);
				// ast_slot_require_cons_or_value_from(
				// 		env, node->loc, AST_CONSTR_SRC_CALL_ARG,
				// 		func_slot, in_func_slot);

				func_type_slot = ast_slot_alloc(env);
				ast_slot_require_type(
						env, node->loc, AST_CONSTR_SRC_CALL_ARG,
						func_slot, func_type_slot);

				ast_slot_id arg_values[node->call.num_args];
				ast_slot_id arg_types[node->call.num_args];

				for (size_t i = 0; i < node->call.num_args; i++) {
					arg_values[i] = ast_node_constraints(
							ctx, mod, env, deps, num_deps,
							node->call.args[i].value);
					arg_types[i] = ast_slot_alloc(env);
					ast_slot_require_type(
							env, node->loc, AST_CONSTR_SRC_CALL_ARG,
							arg_values[i], arg_types[i]);
				}

				ast_slot_id ret_slot, ret_type_slot;

				ret_slot = ast_slot_alloc(env);
				ret_type_slot = ast_slot_alloc(env);
				ast_slot_require_type(
						env, node->loc, AST_CONSTR_SRC_CALL_ARG,
						ret_slot, ret_type_slot);

				ast_slot_require_is_func_type(
						env, node->loc, AST_CONSTR_SRC_CALL_ARG,
						ctx->vm->default_types.type, func_type_slot, ret_type_slot, arg_types,
						node->call.num_args);

				// printf("func call (%i -> %i : %i)(...) -> %i:%i\n",
				// 		in_func_slot, func_slot, func_type_slot, ret_slot, ret_type_slot);

				node->typecheck_slot = ret_slot;
			}
			break;

		case AST_NODE_INST:
			{
				ast_slot_id res_slot, cons_slot;
				res_slot = ast_slot_alloc(env);
				cons_slot = ast_node_constraints(
						ctx, mod, env, deps, num_deps,
						node->call.func);

				ast_slot_require_inst(
						env, node->loc, AST_CONSTR_SRC_CONS_ARG,
						res_slot, cons_slot);


				for (size_t i = 0; i < node->call.num_args; i++) {
					ast_slot_id arg_slot;
					arg_slot = ast_node_constraints(
							ctx, mod, env, deps, num_deps,
							node->call.args[i].value);

					ast_slot_require_member_named(
							env, node->call.args[i].value->loc,
							AST_CONSTR_SRC_CONS_ARG, res_slot,
							node->call.args[i].name, arg_slot);

				}

				node->typecheck_slot = res_slot;
			}
			break;

		case AST_NODE_CONS:
			{
				ast_slot_id res_slot, cons_slot;
				res_slot = ast_slot_alloc(env);
				cons_slot = ast_node_constraints(
						ctx, mod, env, deps, num_deps,
						node->call.func);

				ast_slot_require_cons(
						env, node->loc, AST_CONSTR_SRC_CONS_ARG,
						res_slot, cons_slot);


				for (size_t i = 0; i < node->call.num_args; i++) {
					ast_slot_id arg_slot;
					arg_slot = ast_node_constraints(
							ctx, mod, env, deps, num_deps,
							node->call.args[i].value);

					struct atom *name;
					name = node->call.args[i].name;

					if (name) {
						ast_slot_require_param_named(
								env, node->call.args[i].value->loc,
								AST_CONSTR_SRC_CONS_ARG, res_slot,
								name, arg_slot);
					} else {
						ast_slot_require_param_index(
								env, node->call.args[i].value->loc,
								AST_CONSTR_SRC_CONS_ARG, res_slot,
								i, arg_slot);
					}

				}

				node->typecheck_slot = res_slot;
			}
			break;

		case AST_NODE_ACCESS:
			{
				ast_slot_id target_slot, member_slot;

				target_slot = ast_node_constraints(
						ctx, mod, env, deps, num_deps,
						node->access.target);

				member_slot = ast_slot_alloc(env);
				ast_slot_require_member_named(
						env, node->loc, AST_CONSTR_SRC_ACCESS,
						target_slot, node->access.name, member_slot);

				node->typecheck_slot = member_slot;
			}
			break;

		case AST_NODE_TEMPL:
			{
				ast_slot_id res_slot;
				res_slot = ast_slot_alloc(env);

				for (size_t i = 0; i < node->templ.pattern.num_params; i++) {
					if (node->templ.pattern.params[i].type) {
						ast_node_constraints(
								ctx, mod, env, deps, num_deps,
								node->templ.pattern.params[i].type);
					}
				}

				if (node->templ.cons) {
					struct object cons_obj = {0};
					cons_obj.type = ctx->vm->default_types.cons;
					cons_obj.data = &node->templ.cons;
					cons_obj = register_object(
							ctx->vm, env->store, cons_obj);

					ast_slot_require_is_obj(
							env, node->loc, AST_CONSTR_SRC_TEMPL_PARAM_DECL,
							res_slot, cons_obj);
				} else if (node->templ.failed) {
					ast_slot_value_error(
							env, node->loc, AST_CONSTR_SRC_TEMPL_PARAM_DECL,
							res_slot);
				}

				node->typecheck_slot = res_slot;
			}
			break;

		case AST_NODE_LIT:
			{
				ast_slot_id res_slot;
				res_slot = ast_slot_alloc(env);

				ast_slot_require_is_obj(
						env, node->loc, AST_CONSTR_SRC_LIT,
						res_slot, node->lit.obj);

				node->typecheck_slot = res_slot;
			}
			break;

		case AST_NODE_LIT_NATIVE:
			{
				ast_slot_id res_slot;
				res_slot = ast_slot_alloc(env);

				struct object obj = {0};

				int err;
				err = stg_mod_lookup_native_object(
						mod, node->lit_native.name, &obj);
				if (err) {
					stg_error(ctx->err, node->loc,
							"Module '%.*s' has no native object '%.*s'.",
							ALIT(mod->name), ALIT(node->lit_native.name));
					ast_slot_value_error(
							env, node->loc, AST_CONSTR_SRC_LIT,
							res_slot);
				} else {
					ast_slot_require_is_obj(
							env, node->loc, AST_CONSTR_SRC_LIT,
							res_slot, obj);
				}

				node->typecheck_slot = res_slot;
			}
			break;

		case AST_NODE_FUNC_TYPE:
			{
				ast_slot_id type_slot;

				struct ast_node *param_type_nodes[node->func_type.num_params];

				for (size_t i = 0; i < node->func_type.num_params; i++) {
					param_type_nodes[i] = node->func_type.param_types[i];
				}

				type_slot = ast_func_proto_constraints(
						ctx, mod, env, deps, num_deps,
						node->func_type.ret_type,
						param_type_nodes, node->func_type.num_params,
						node->loc);

				node->typecheck_slot = type_slot;
			}
			break;

		case AST_NODE_LOOKUP:
		{
			struct ast_typecheck_dep *res;
			res = ast_find_dep(deps, num_deps, node->lookup.ref);
			assert(res);

			ast_slot_id res_slot;
			res_slot = ast_slot_alloc(env);

			if (res->lookup_failed) {
				stg_error(ctx->err, node->loc,
						"'%.*s' was not found.", ALIT(node->lookup.name));
				ast_slot_value_error(
						env, node->loc, AST_CONSTR_SRC_LOOKUP,
						res_slot);
			} else {
				ast_slot_require_equals(
						env, node->loc, AST_CONSTR_SRC_LOOKUP,
						res_slot, res->value);
			}

			node->typecheck_slot = res_slot;
		}
		break;

		case AST_NODE_MOD:
		{
			ast_slot_id res_slot;
			res_slot = ast_slot_alloc(env);

			struct stg_module *mod_ref = NULL;
			mod_ref = stg_mod_find_module(
					mod, node->mod.name);

			if (!mod_ref) {
				panic("Attempted to access module that was not registered as a dependency.");
				ast_slot_value_error(
						env, node->loc, AST_CONSTR_SRC_MOD, res_slot);
			} else {
				assert(mod_ref->instance.type != TYPE_UNSET);

				ast_slot_require_is_obj(
						env, node->loc, AST_CONSTR_SRC_MOD,
						res_slot, mod_ref->instance);
			}

			node->typecheck_slot = res_slot;
		}
		break;

		case AST_NODE_MATCH:
		{
			ast_slot_id value_slot, value_type_slot;

			value_slot = ast_node_constraints(
					ctx, mod, env, deps, num_deps,
					node->match.value);

			value_type_slot = ast_slot_alloc(env);
			ast_slot_require_type(env, node->loc,
					AST_CONSTR_SRC_MATCH_VALUE,
					value_slot, value_type_slot);

			ast_slot_id res_slot, res_type_slot;

			res_slot = ast_slot_alloc(env);
			res_type_slot = ast_slot_alloc(env);

			ast_slot_require_type(env, node->loc,
					AST_CONSTR_SRC_MATCH_RESULT,
					res_slot, res_type_slot);

			for (size_t case_i = 0; case_i < node->match.num_cases; case_i++) {
				struct ast_match_case *match_case;
				match_case = &node->match.cases[case_i];
				ast_slot_id pat_slot, expr_slot;

				size_t num_pat_deps = num_deps + match_case->pattern.num_params;
				struct ast_typecheck_dep pat_deps[num_pat_deps];
				memset(pat_deps, 0, sizeof(struct ast_typecheck_dep) * num_pat_deps);

				for (size_t i = 0; i < match_case->pattern.num_params; i++) {
					pat_deps[i].req = AST_NAME_DEP_REQUIRE_TYPE;
					pat_deps[i].ref.kind = AST_NAME_REF_TEMPL;
					pat_deps[i].ref.templ = i;
					pat_deps[i].value = ast_slot_alloc(env);

					if (match_case->pattern.params[i].type) {
						ast_slot_id type_slot;
						type_slot = ast_node_constraints(
								ctx, mod, env, deps, num_deps,
								match_case->pattern.params[i].type);
						ast_slot_require_type(env, node->loc,
								AST_CONSTR_SRC_MATCH_VALUE,
								pat_deps[i].value, type_slot);
					}
				}

				memcpy(&pat_deps[match_case->pattern.num_params],
						deps, sizeof(struct ast_typecheck_dep) * num_deps);

				// TODO: Support pattern matching.
				pat_slot = ast_node_constraints(
						ctx, mod, env, pat_deps, num_pat_deps,
						match_case->pattern.node);
				ast_slot_require_type(env, node->loc,
						AST_CONSTR_SRC_MATCH_VALUE,
						pat_slot, value_type_slot);

				expr_slot = ast_node_constraints(
						ctx, mod, env, pat_deps, num_pat_deps,
						match_case->expr);

				ast_slot_require_type(env, node->loc,
						AST_CONSTR_SRC_MATCH_RESULT,
						expr_slot, res_type_slot);
			}

			// TODO: We should make sure all cases in the value's domain are
			// covered.

			node->typecheck_slot = res_slot;
		}
		break;

		case AST_NODE_WILDCARD:
		{
			ast_slot_id slot;
			slot = ast_slot_alloc(env);
			node->typecheck_slot = slot;
		}
		break;

		case AST_NODE_INIT_EXPR:
		{
			struct ast_name_ref ref = {0};
			ref.kind = AST_NAME_REF_INIT_EXPR;
			ref.init_expr = node->init_expr.id;

			struct ast_typecheck_dep *res;
			res = ast_find_dep(deps, num_deps, ref);
			assert(res && !res->lookup_failed);

			ast_slot_id slot, type_slot;
			slot      = ast_slot_alloc(env);
			type_slot = ast_slot_alloc(env);

			ast_slot_require_type(
					env, node->loc, AST_CONSTR_SRC_LOOKUP,
					slot, type_slot);

			ast_slot_require_type(
					env, node->loc, AST_CONSTR_SRC_LOOKUP,
					res->value, type_slot);

			node->typecheck_slot = slot;
		}
		break;

		case AST_NODE_COMPOSITE:
		{
			ast_slot_id res_slot;
			res_slot = ast_slot_alloc(env);

			if (node->composite.type != TYPE_UNSET) {
				ast_slot_require_is_type(
						env, node->loc, AST_CONSTR_SRC_DT_DECL,
						res_slot, node->composite.type);
			} else if (node->composite.failed) {
				ast_slot_value_error(
						env, node->loc, AST_CONSTR_SRC_DT_DECL,
						res_slot);
			} else {
				panic("Attempted to use the type of an not-yet-solved composite.");
			}

			node->typecheck_slot = res_slot;
		}
		break;

		case AST_NODE_TYPE_CLASS:
		{
			ast_slot_id res_slot;
			res_slot = ast_slot_alloc(env);

			if (node->type_class.cons != NULL) {
				struct object cons_obj = {0};
				cons_obj.type = ctx->vm->default_types.cons;
				cons_obj.data = &node->type_class.cons;
				cons_obj = register_object(
						ctx->vm, &mod->store, cons_obj);
				ast_slot_require_is_obj(
						env, node->loc, AST_CONSTR_SRC_DT_DECL,
						res_slot, cons_obj);
			} else if (node->type_class.failed) {
				ast_slot_value_error(
						env, node->loc, AST_CONSTR_SRC_DT_DECL,
						res_slot);
			} else {
				panic("Attempted to use the type of an not-yet-solved type class.");
			}

			node->typecheck_slot = res_slot;
		}
		break;

		case AST_NODE_VARIANT:
		{
			ast_slot_id res_slot;
			res_slot = ast_slot_alloc(env);

			if (node->variant.type != TYPE_UNSET) {
				ast_slot_require_is_type(
						env, node->loc, AST_CONSTR_SRC_DT_DECL,
						res_slot, node->variant.type);
			} else if (node->variant.failed) {
				ast_slot_value_error(
						env, node->loc, AST_CONSTR_SRC_DT_DECL,
						res_slot);
			} else {
				panic("Attempted to use the type of an not-yet-solved variant.");
			}

			node->typecheck_slot = res_slot;
		}
		break;

		case AST_NODE_DATA_TYPE:
		{
			struct ast_node *dt_node;
			dt_node = ast_module_node_get_data_type(
					ctx->vm, node);

			node->typecheck_slot = ast_node_constraints(
					ctx, mod, env, deps, num_deps, dt_node);
		}
		break;

	}

	if (node->typecheck_slot < 0) {
		panic("Unhandled node %s in ast_node_constraints.",
				ast_node_name(node->kind));
		return -1;
	}

	return node->typecheck_slot;
}

#define DBG_ERROR_FMT "<%zu:%i>"
#define DBG_ERROR_ARG(env,slot) , ((env)->invoc_id), (slot)

static inline void
ast_node_resolve_handle_value_result(
		struct ast_context *ctx, struct ast_env *env, int *errors,
		struct ast_node *node, struct ast_slot_result *res,
		const char *expectation)
{
	ast_slot_id slot_id = node->typecheck_slot;

	switch (ast_slot_value_result(res->result)) {
		case AST_SLOT_RES_VALUE_UNKNOWN:
		case AST_SLOT_RES_TYPE_FOUND:
			stg_error(ctx->err, node->loc,
					"Not enough type information to resolve the value "
					"of this expression." DBG_ERROR_FMT
					DBG_ERROR_ARG(env, slot_id));
			*errors += 1;
			break;

		case AST_SLOT_RES_VALUE_FOUND_OBJ:
			{
				struct string got_str;
				got_str = obj_repr_to_alloced_string(
						ctx->vm, res->value.obj);
				stg_error(ctx->err, node->loc,
						"Expected %s, got %.*s." DBG_ERROR_FMT,
						expectation, LIT(got_str)
						DBG_ERROR_ARG(env, slot_id));
				free(got_str.text);
				*errors += 1;
			}
			break;

		case AST_SLOT_RES_VALUE_FOUND_TYPE:
			{
				struct string got_str;
				got_str = obj_repr_to_alloced_string(
						ctx->vm, res->value.obj);
				stg_error(ctx->err, node->loc,
						"Expected %s, got Type(%.*s)." DBG_ERROR_FMT,
						expectation, LIT(got_str)
						DBG_ERROR_ARG(env, slot_id));
				free(got_str.text);
				*errors += 1;
			}
			break;

		default:
			panic("Invalid slot bind result.");
			break;
	}
}

static inline void
ast_node_resolve_handle_cons_result(
		struct ast_context *ctx, struct ast_env *env, int *errors,
		struct ast_node *node, struct ast_slot_result *res,
		const char *expectation)
{
	ast_slot_id slot_id = node->typecheck_slot;

	switch (ast_slot_cons_result(res->result)) {
		case AST_SLOT_RES_CONS_UNKNOWN:
			stg_error(ctx->err, node->loc,
					"Not enough type information to resolve the "
					"%s of this expression." DBG_ERROR_FMT,
					expectation
					DBG_ERROR_ARG(env, slot_id));
			*errors += 1;
			break;

		case AST_SLOT_RES_CONS_FOUND:
			stg_error(ctx->err, node->loc,
					"Expected %s instantiation, got constructor." DBG_ERROR_FMT,
					expectation
					DBG_ERROR_ARG(env, slot_id));
			*errors += 1;
			break;

		case AST_SLOT_RES_CONS_FOUND_FUNC_TYPE:
			stg_error(ctx->err, node->loc,
					"Expected %s, got function type constructor." DBG_ERROR_FMT,
					expectation
					DBG_ERROR_ARG(env, slot_id));
			*errors += 1;
			break;

		default:
			panic("Invalid slot bind result.");
			break;
	}
}

static inline void
ast_node_resolve_handle_inst_result(
		struct ast_context *ctx, struct ast_env *env, int *errors,
		struct ast_node *node, struct ast_slot_result *res)
{
	ast_slot_id slot_id = node->typecheck_slot;

	switch (ast_slot_inst_result(res->result)) {
		case AST_SLOT_RES_INST_UNKNOWN:
			stg_error(ctx->err, node->loc,
					"Not enough type information to resolve the "
					"object instantiation of this expression." DBG_ERROR_FMT
					DBG_ERROR_ARG(env, slot_id));
			*errors += 1;
			break;

		case AST_SLOT_RES_INST_FOUND:
			// This should never be reached.
			stg_error(ctx->err, node->loc,
					"Got object instantiation unexpectedly." DBG_ERROR_FMT
					DBG_ERROR_ARG(env, slot_id));
			*errors += 1;
			break;

		default:
			panic("Invalid slot bind result.");
			break;
	}
}

static int
ast_node_resolve_types(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_slot_result *slots, struct ast_node *node)
{
	int errors = 0;

	struct ast_slot_result *res;
	res = &slots[node->typecheck_slot];

	if (res->result != AST_SLOT_RES_ERROR) {
		switch (node->kind) {

			case AST_NODE_CALL:
				{
					ast_slot_id func_slot_id;
					struct ast_slot_result *func_res;

					// We have to check the result of the func slot here
					// because the slot id is removed by the recursive call to
					// ast_node_resolve_types.
					func_slot_id = node->call.func->typecheck_slot;
					func_res = &slots[func_slot_id];

					switch (ast_slot_value_result(func_res->result)) {
						case AST_SLOT_RES_VALUE_FOUND_OBJ:
							if (stg_type_is_func(ctx->vm, func_res->value.obj.type)) {
								node->call.func_val =
									*(struct stg_func_object *)func_res->value.obj.data;
							}
							break;

						default:
							break;
					}
				}
				break;

			case AST_NODE_ACCESS:
				{
					ast_slot_id target_slot_id;
					struct ast_slot_result *target_res;

					// We have to check the result of the target slot here
					// because the slot id is removed by the recursive call to
					// ast_node_resolve_types.
					target_slot_id = node->access.target->typecheck_slot;
					target_res = &slots[target_slot_id];

					switch (ast_slot_value_result(target_res->result)) {
						case AST_SLOT_RES_VALUE_FOUND_TYPE:
							node->access.const_target_value_type = target_res->value.type;
							break;

						default:
							break;
					}
				}
				break;

			case AST_NODE_TYPE_CLASS:
				{
					if (res->result == AST_SLOT_RES_ERROR) {
						errors += 1;
					} else {
						if (ast_slot_value_result(res->result) <
								AST_SLOT_RES_TYPE_FOUND) {
							ast_node_resolve_handle_value_result(
									ctx, env, &errors, node, res, "");
						} else {
							node->type = res->type;
						}
					}
				}
				// Abort the resolve before we can visit the template's
				// children. These have already been handled in resolve
				// datatype.
				return errors;

			case AST_NODE_VARIANT:
				{
					if (res->result == AST_SLOT_RES_ERROR) {
						errors += 1;
					} else {
						if (ast_slot_value_result(res->result) <
								AST_SLOT_RES_TYPE_FOUND) {
							ast_node_resolve_handle_value_result(
									ctx, env, &errors, node, res, "");
						} else {
							node->type = res->type;
						}
					}
				}
				// Abort the resolve as the variant's types have already been
				// resolved in ast_dt_finalize_variant.
				return errors;

			default:
				break;
		}
	}

#define VISIT_NODE(child) errors += ast_node_resolve_types(ctx, env, slots, child);
	AST_NODE_VISIT(node, false, true, false);
#undef VISIT_NODE

	assert(node->type == TYPE_UNSET);

	if (res->result == AST_SLOT_RES_ERROR) {
		// The error should already have been reported.
		errors += 1;
	} else {
		switch (ast_slot_value_result(res->result)) {
			case AST_SLOT_RES_VALUE_FOUND_OBJ:
			case AST_SLOT_RES_VALUE_FOUND_TYPE:
			case AST_SLOT_RES_TYPE_FOUND:
				node->type = res->type;
				break;

			default:
				ast_node_resolve_handle_value_result(
						ctx, env, &errors, node, res, "");
				break;
		}

		if ((res->result & AST_SLOT_RES_DECAYED) != 0) {
			// We replace the current node with the constructor node to
			// maintain all references to node.
			struct ast_node *cons_node;
			cons_node = node;

			// Alloc a new node to house the previous one that is being
			// decayed.
			node = arena_alloc(ctx->mem, sizeof(struct ast_node));
			*node = *cons_node;
			node->type = ctx->vm->default_types.cons;

			memset(cons_node, 0, sizeof(struct ast_node));
			ast_init_node_cons(ctx, cons_node, node->loc, node, NULL, 0);
			assert(res->cons);
			cons_node->call.cons = res->cons;
			cons_node->typecheck_slot = -1;
			cons_node->type = res->type;

			switch (ast_slot_value_result(res->result)) {
				case AST_SLOT_RES_VALUE_FOUND_OBJ:
					cons_node->call.cons_value = res->value.obj;
					break;

				case AST_SLOT_RES_VALUE_FOUND_TYPE:
					cons_node->call.cons_value.data = &res->value.type;
					cons_node->call.cons_value.type = ctx->vm->default_types.type;
					cons_node->call.cons_value =
						register_object(ctx->vm, env->store, cons_node->call.cons_value);
					break;

				default:
					ast_node_resolve_handle_value_result(
							ctx, env, &errors, node, res, "");
					break;
			}

		}

		switch (node->kind) {
			case AST_NODE_CALL:
				{
					type_id func_type;
					func_type = node->call.func->type;
					if (func_type == TYPE_UNSET) {
						break;
					}

					assert(stg_type_is_func(ctx->vm, func_type));

					struct type *type;
					type = vm_get_type(ctx->vm, func_type);
					struct stg_func_type *func_info;
					func_info = type->data;

					if (func_info->num_params != node->call.num_args) {
						struct string exp_str;
						exp_str = type_repr_to_alloced_string(
								ctx->vm, type);
						stg_error(ctx->err, node->loc,
								"Function %.*s expected %zu arument%s, got %zu.",
								LIT(exp_str), func_info->num_params,
								func_info->num_params != 1 ? "s" : "",
								node->call.num_args);
						free(exp_str.text);
						errors += 1;
					}
				}
				break;

			case AST_NODE_CONS:
				switch (ast_slot_value_result(res->result)) {
					case AST_SLOT_RES_VALUE_FOUND_OBJ:
						node->call.cons_value = res->value.obj;
						break;

					case AST_SLOT_RES_VALUE_FOUND_TYPE:
						{
							struct object obj = {0};
							obj.type = ctx->vm->default_types.type;
							obj.data = &res->value.type;
							obj = register_object(ctx->vm, env->store, obj);
							node->call.cons_value = obj;
						}
						break;

					default:
						ast_node_resolve_handle_value_result(
								ctx, env, &errors, node, res, "");
						break;
				}
				switch (ast_slot_cons_result(res->result)) {
					case AST_SLOT_RES_CONS_FOUND:
						node->call.cons = res->cons;
						break;

					default:
						ast_node_resolve_handle_cons_result(
								ctx, env, &errors, node, res, "constructor");
						break;
				}
				break;

			case AST_NODE_INST:
				switch (ast_slot_inst_result(res->result)) {
					case AST_SLOT_RES_INST_FOUND:
						node->call.inst = res->inst;
						break;

					default:
						ast_node_resolve_handle_inst_result(
								ctx, env, &errors, node, res);
						break;
				}
				break;

			case AST_NODE_FUNC_TYPE:
				switch (ast_slot_value_result(res->result)) {
					case AST_SLOT_RES_VALUE_FOUND_TYPE:
						node->func_type.func_type = res->value.type;
						break;

					default:
						ast_node_resolve_handle_value_result(
								ctx, env, &errors, node, res, "function type");
						break;
				}
				break;

			default:
				break;
		}
	}

	node->typecheck_slot = -1;

	assert(res->type != TYPE_UNSET || errors > 0);
	return errors;
}

void
ast_typecheck_deps_slots(struct ast_env *env,
		struct ast_typecheck_dep *body_deps, size_t num_deps)
{
	for (size_t i = 0; i < num_deps; i++) {
		struct ast_typecheck_dep *dep;
		dep = &body_deps[i];

		dep->value = ast_slot_alloc(env);

		if (!dep->determined) {
			continue;
		}

		if (dep->lookup_failed) {
			// TODO: Location.
			ast_slot_value_error(env, STG_NO_LOC,
					AST_CONSTR_SRC_CLOSURE, dep->value);
			continue;
		}

		switch (dep->req) {
			// TODO: Location.
			case AST_NAME_DEP_REQUIRE_VALUE:
				ast_slot_require_is_obj(env, STG_NO_LOC,
						AST_CONSTR_SRC_CLOSURE,
						dep->value, dep->val);
				break;

			case AST_NAME_DEP_REQUIRE_TYPE:
				{
					ast_slot_id type_slot;
					type_slot = ast_slot_alloc(env);
					ast_slot_require_is_type(env, STG_NO_LOC,
							AST_CONSTR_SRC_CLOSURE,
							type_slot, dep->type);
					ast_slot_require_type(env, STG_NO_LOC,
							AST_CONSTR_SRC_CLOSURE,
							dep->value, type_slot);
				}
				break;
		}
	}

}

int
ast_node_typecheck(struct ast_context *ctx,
		struct stg_module *mod, struct ast_node *node,
		struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_tc_expected exp, struct object *out_value)
{
	struct ast_env env = {0};
	env.store = &mod->store;

#if AST_DEBUG_UNINITIALIZED_SLOT_ID
	// Add a slot 0 to debug invalid references.
	ast_slot_alloc(&env);
#endif

	struct ast_typecheck_dep body_deps[num_deps];
	memcpy(body_deps, deps, num_deps * sizeof(struct ast_typecheck_dep));

	ast_typecheck_deps_slots(&env, body_deps, num_deps);

	ast_node_resolve_datatypes(
			ctx, mod, body_deps, num_deps, node);

	ast_slot_id expr_slot;
	expr_slot = ast_node_constraints(
			ctx, mod, &env, body_deps, num_deps, node);

	for (size_t i = 0; i < num_deps; i++) {
		if (body_deps[i].ref.kind == AST_NAME_REF_SELF &&
				body_deps[i].ref.self_offset == 0) {
			ast_slot_require_equals(
					&env, node->loc, AST_CONSTR_SRC_SELF,
					expr_slot, body_deps[i].value);
		}
	}

	switch (exp.kind) {
		case AST_NODE_TC_EXP_TYPE:
			{
				ast_slot_id expr_type_slot;
				expr_type_slot = ast_slot_alloc(&env);

				ast_slot_require_is_type(
						&env, node->loc, AST_CONSTR_SRC_EXPECTED,
						expr_type_slot, exp.type);
				ast_slot_require_type(
						&env, node->loc, AST_CONSTR_SRC_EXPECTED,
						expr_slot, expr_type_slot);
			}
			break;

		case AST_NODE_TC_EXP_CONS:
			{
				ast_slot_id type_slot, cons_slot;
				type_slot = ast_slot_alloc(&env);
				cons_slot = ast_slot_alloc(&env);


				struct object cons_obj = {0};
				cons_obj.data = &exp.cons;
				cons_obj.type = ctx->vm->default_types.cons;
				cons_obj = stg_register_object(mod, cons_obj);

				ast_slot_require_type(
						&env, node->loc, AST_CONSTR_SRC_EXPECTED,
						expr_slot, type_slot);

				ast_slot_require_is_obj(
						&env, node->loc, AST_CONSTR_SRC_EXPECTED,
						cons_slot, cons_obj);

				ast_slot_require_cons(
						&env, node->loc, AST_CONSTR_SRC_EXPECTED,
						type_slot, cons_slot);

			}
			break;

		case AST_NODE_TC_EXP_NOTHING:
			break;
	}

	struct ast_slot_result result[env.num_alloced_slots];

#if AST_DEBUG_SLOT_SOLVE
	printf("Solving ");
	ast_print_node(ctx, node, true);
	printf("\n");
#endif

	int err;
	err = ast_slot_try_solve(
			ctx, mod, &env, result);

#if AST_DEBUG_SLOT_SOLVE_GRAPH
	printf("#%3zu: ", env.invoc_id);
	ast_print_node(ctx, node, true);
#endif

	if (err < 0) {
#if AST_DEBUG_SLOT_SOLVE
		printf("Failed (solve).\n\n");
#endif
#if AST_DEBUG_SLOT_SOLVE_GRAPH
		printf(" " TC(TC_RED, "failed") " (solve)\n");
#endif
		ast_env_free(&env);
		return -1;
	}

	if (out_value) {
		struct ast_slot_result *res;
		res = &result[expr_slot];

		out_value->type = TYPE_UNSET;
		out_value->data = NULL;

		switch (ast_slot_value_result(res->result)) {
			case AST_SLOT_RES_VALUE_FOUND_OBJ:
				*out_value = res->value.obj;
				break;

			case AST_SLOT_RES_VALUE_FOUND_TYPE:
				out_value->type = ctx->vm->default_types.type;
				out_value->data = &res->value.type;
				*out_value = stg_register_object(mod, *out_value);
				break;

			default:
				break;
		}
	}

	err = ast_node_resolve_types(
			ctx, &env, result, node);
	if (err) {
#if AST_DEBUG_SLOT_SOLVE
		printf("Failed (resolve).\n\n");
#endif
#if AST_DEBUG_SLOT_SOLVE_GRAPH
		printf(TC(TC_RED, " failed") " (resolve)\n");
#endif
		ast_env_free(&env);
		return -1;
	}

#if AST_DEBUG_SLOT_SOLVE
		printf("OK.\n\n");
#endif

#if AST_DEBUG_SLOT_SOLVE_GRAPH
		printf("\n");
#endif

	ast_env_free(&env);

	return 0;
}
