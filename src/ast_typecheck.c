#include "ast.h"
#include "vm.h"
#include "utils.h"
#include "error.h"
#include "base/mod.h"
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
			return lhs.member == rhs.member;
		case AST_NAME_REF_PARAM:
			return lhs.param == rhs.param;
		case AST_NAME_REF_CLOSURE:
			return lhs.closure == rhs.closure;
		case AST_NAME_REF_TEMPL:
			return lhs.templ == rhs.templ;
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

static void
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
		out_deps[i].value = ast_slot_alloc(env);

		if (in_dep->determined) {
			if (mbr->require_const) {
				assert(in_dep->determined &&
						in_dep->req == AST_NAME_DEP_REQUIRE_VALUE);
				out_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;

				// TODO: Location
				ast_slot_require_is_obj(
						env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
						out_deps[i].value, in_dep->val);

				out_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;
				out_deps[i].determined = true;
				out_deps[i].val = in_dep->val;
			} else {
				assert(in_dep->determined);
				out_deps[i].req = AST_NAME_DEP_REQUIRE_TYPE;

				if (in_dep->req == AST_NAME_DEP_REQUIRE_TYPE) {
					ast_slot_id type_slot;
					type_slot = ast_slot_alloc(env);

					// TODO: Location
					ast_slot_require_is_type(
							env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
							type_slot, in_dep->type);

					ast_slot_require_type(
							env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
							out_deps[i].value, type_slot);

					out_deps[i].type = in_dep->type;
				} else {
					ast_slot_require_is_obj(
							env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
							out_deps[i].value, in_dep->val);
					out_deps[i].val = in_dep->val;
				}

				out_deps[i].req = in_dep->req;
				out_deps[i].determined = true;
			}
		} else {
			out_deps[i].determined = false;
			out_deps[i].value = in_dep->value;
		}
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
ast_node_constraints(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_node *node);

static ast_slot_id
ast_func_proto_constraints(
		struct ast_context *ctx, struct ast_module *mod,
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
			func_type, ret_type_slot,
			param_type_slots, num_params);

	return func_type;
}

static ast_slot_id
ast_node_constraints(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_typecheck_dep *deps, size_t num_deps,
		struct ast_node *node)
{
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
					ast_slot_require_member_index(
							env, STG_NO_LOC, AST_CONSTR_SRC_CLOSURE,
							type_slot, i+1, param_type_slot);

					body_deps[i].value = ast_slot_alloc(env);
					ast_slot_require_type(
							env, node->func.params[i].type->loc,
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
				ast_slot_require_member_index(
						env, node->loc, AST_CONSTR_SRC_FUNC_DECL,
						type_slot, 0, ret_type_slot);

				ast_slot_id func_value_slot;
				func_value_slot = ast_slot_alloc(env);

				ast_slot_require_type(
						env, node->loc, AST_CONSTR_SRC_FUNC_DECL,
						func_value_slot, type_slot);

				node->typecheck_slot = func_value_slot;
				return func_value_slot;
			}

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
				return func_value_slot;
			}

		case AST_NODE_CALL:
			{
				ast_slot_id func_slot, func_type_slot;

				func_slot = ast_node_constraints(
						ctx, mod, env, deps, num_deps,
						node->call.func);

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
						func_type_slot, ret_type_slot, arg_types,
						node->call.num_args);

				node->typecheck_slot = ret_slot;
				return ret_slot;
			}

		case AST_NODE_INST:
			{
				ast_slot_id res_slot, cons_slot;
				res_slot = ast_slot_alloc(env);
				cons_slot = ast_node_constraints(
						ctx, mod, env, deps, num_deps,
						node->call.func);

				// TODO: Bind object_inst.
				/*
				ast_slot_require_cons_inst(
						env, node->loc, AST_CONSTR_SRC_CONS_ARG,
						res_slot, cons_slot);
				*/


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
				return res_slot;
			}

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

					ast_slot_require_member_named(
							env, node->call.args[i].value->loc,
							AST_CONSTR_SRC_CONS_ARG, res_slot,
							node->call.args[i].name, arg_slot);

				}

				node->typecheck_slot = res_slot;
				return res_slot;
			}

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
				return member_slot;
			}

		case AST_NODE_TEMPL:
			{
				struct ast_typecheck_dep body_deps[num_deps + node->templ.num_params];
				memset(body_deps, 0, sizeof(struct ast_typecheck_dep) *
						(num_deps + node->templ.num_params));
				ast_constr_fill_closure_deps(ctx, env,
						body_deps, &node->templ.closure,
						deps, num_deps);


				for (size_t i = 0; i < node->templ.num_params; i++) {
					ast_slot_id type_slot;

					if (node->templ.params[i].type) {
						body_deps[num_deps+i].determined = true;
						type_slot = ast_node_constraints(
								ctx, mod, env, deps, num_deps,
								node->templ.params[i].type);
					} else {
						body_deps[num_deps+i].determined = false;
						type_slot = ast_slot_alloc(env);
					}

					ast_slot_id param_slot;

					param_slot = ast_slot_alloc(env);

					ast_slot_require_type(
							env, node->templ.params[i].loc,
							AST_CONSTR_SRC_TEMPL_PARAM_DECL,
							param_slot, type_slot);

					body_deps[num_deps+i].req = AST_NAME_DEP_REQUIRE_VALUE;
					body_deps[num_deps+i].ref.kind = AST_NAME_REF_TEMPL;
					body_deps[num_deps+i].ref.templ = i;
					body_deps[num_deps+i].lookup_failed = false;

					body_deps[num_deps+i].value = param_slot;
				}

				if (!node->templ.cons) {
					// TODO: Implement creating templates.
					// node->templ.cons = ast_node_create_templ(
					//		ctx, mod, env, node, body_deps, num_deps);
					assert(node->templ.cons);
				}

				ast_slot_id res_slot;
				res_slot = ast_slot_alloc(env);

				struct object cons_obj = {0};
				cons_obj.type = ctx->types.cons;
				cons_obj.data = &node->templ.cons;
				cons_obj = register_object(
						ctx->vm, env->store, cons_obj);

				ast_slot_require_is_obj(
						env, node->loc, AST_CONSTR_SRC_TEMPL_PARAM_DECL,
						res_slot, cons_obj);

				node->typecheck_slot = res_slot;
				return res_slot;
			}

		case AST_NODE_LIT:
			{
				ast_slot_id res_slot;
				res_slot = ast_slot_alloc(env);

				ast_slot_require_is_obj(
						env, node->loc, AST_CONSTR_SRC_LIT,
						res_slot, node->lit.obj);

				node->typecheck_slot = res_slot;
				return res_slot;
			}

		case AST_NODE_FUNC_TYPE:
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

				node->typecheck_slot = type_slot;
				return type_slot;
			}

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
			return res_slot;
		}

		case AST_NODE_COMPOSITE:
		{
			ast_slot_id res_slot;
			res_slot = ast_slot_alloc(env);

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
							ctx, mod, env, node,
							closure_values, closure->num_members);

				if (node->composite.type == TYPE_UNSET) {
					ast_slot_value_error(
							env, node->loc, AST_CONSTR_SRC_DT_DECL,
							res_slot);
				}
			}

			if (node->composite.type != TYPE_UNSET) {
				ast_slot_require_is_type(
						env, node->loc, AST_CONSTR_SRC_DT_DECL,
						res_slot, node->composite.type);
			}

			node->typecheck_slot = res_slot;
			return res_slot;
		}

		case AST_NODE_VARIANT:
		{
			ast_slot_id res_slot;
			res_slot = ast_slot_alloc(env);

			if (node->variant.type == TYPE_UNSET) {
				struct ast_closure_target *closure;
				closure = &node->variant.closure;

				struct ast_typecheck_closure closure_values[closure->num_members];
				memset(closure_values, 0,
						sizeof(struct ast_typecheck_closure) * closure->num_members);

				ast_fill_closure(closure, closure_values,
						deps, num_deps);

				node->variant.type =
					ast_dt_finalize_variant(
							ctx, mod, env,
							node->variant.options,
							node->variant.num_options,
							closure_values, closure->num_members);

				if (node->variant.type == TYPE_UNSET) {
					ast_slot_value_error(
							env, node->loc, AST_CONSTR_SRC_DT_DECL,
							res_slot);
				}
			}

			if (node->variant.type != TYPE_UNSET) {
				ast_slot_require_is_type(
						env, node->loc, AST_CONSTR_SRC_DT_DECL,
						res_slot, node->variant.type);
			}

			node->typecheck_slot = res_slot;
			return res_slot;
		}

	}

	panic("Unhandled node %s in ast_node_constraints.",
			ast_node_name(node->kind));
	return AST_SLOT_NOT_FOUND;
}

static int
ast_node_resolve_types(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_slot_result *slots, struct ast_node *node)
{
	int errors = 0;
#define VISIT_NODE(child) errors += ast_node_resolve_types(ctx, env, slots, child);
	AST_NODE_VISIT(node, false, false, true, false);
#undef VISIT_NODE

	assert(node->type == TYPE_UNSET);

	struct ast_slot_result *res;
	res = &slots[node->typecheck_slot];

	if (res->result == AST_SLOT_RES_ERROR) {
		// The error should already have been reported.
		errors += 1;
	} else {
		switch (ast_slot_value_result(res->result)) {
			case AST_SLOT_RES_VALUE_UNKNOWN:
				stg_error(ctx->err, node->loc,
						"Not enough type information to resolve the type of this "
						"expression.");

				errors += 1;
				break;

			case AST_SLOT_RES_VALUE_FOUND_OBJ:
			case AST_SLOT_RES_VALUE_FOUND_TYPE:
			case AST_SLOT_RES_TYPE_FOUND:
				node->type = res->type;
				break;

			default:
				panic("Invalid slot bind result.");
				break;
		}

		switch (node->kind) {
			case AST_NODE_CONS:
				switch (ast_slot_cons_result(res->result)) {
					case AST_SLOT_RES_CONS_UNKNOWN:
						stg_error(ctx->err, node->loc,
								"Not enough type information to resolve the constructor "
								"of this expression.");

						errors += 1;
						break;

					case AST_SLOT_RES_CONS_FOUND:
						// TODO: Assign the constructor to the node.
						// node->cons.cons = res->cons;
						break;

					case AST_SLOT_RES_CONS_FOUND_FUNC_TYPE:
						stg_error(ctx->err, node->loc,
								"Expected constructor, got function type constructor.");

						errors += 1;
						break;

					default:
						panic("Invalid slot bind result.");
						break;
				}
				break;

			default:
				break;
		}
	}

	node->typecheck_slot = AST_SLOT_NOT_FOUND;

	assert(res->type != TYPE_UNSET || errors > 0);
	return errors;
}

int
ast_node_typecheck(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *outer_env, struct ast_node *node,
		struct ast_typecheck_dep *deps, size_t num_deps,
		type_id expected_type)
{
	struct ast_env env = {0};
	env.store = outer_env->store;

	struct ast_typecheck_dep body_deps[num_deps];
	memcpy(body_deps, deps, num_deps * sizeof(struct ast_typecheck_dep));

	for (size_t i = 0; i < num_deps; i++) {
		struct ast_typecheck_dep *dep;
		dep = &body_deps[i];

		dep->value = ast_slot_alloc(&env);

		if (!dep->determined) {
			continue;
		}

		if (dep->lookup_failed) {
			// TODO: Location.
			ast_slot_value_error(&env, STG_NO_LOC,
					AST_CONSTR_SRC_CLOSURE, dep->value);
			continue;
		}

		switch (dep->req) {
			// TODO: Location.
			case AST_NAME_DEP_REQUIRE_VALUE:
				ast_slot_require_is_obj(&env, STG_NO_LOC,
						AST_CONSTR_SRC_CLOSURE,
						dep->value, dep->val);
				break;

			case AST_NAME_DEP_REQUIRE_TYPE:
				{
					ast_slot_id type_slot;
					type_slot = ast_slot_alloc(&env);
					ast_slot_require_is_type(&env, STG_NO_LOC,
							AST_CONSTR_SRC_CLOSURE,
							type_slot, dep->type);
					ast_slot_require_type(&env, STG_NO_LOC,
							AST_CONSTR_SRC_CLOSURE,
							dep->value, type_slot);
				}
				break;
		}
	}

	ast_slot_id expr_slot;
	expr_slot = ast_node_constraints(
			ctx, mod, &env, body_deps, num_deps, node);

	if (expected_type != TYPE_UNSET) {
		ast_slot_id expr_type_slot;
		expr_type_slot = ast_slot_alloc(&env);

		ast_slot_require_is_type(
				&env, node->loc, AST_CONSTR_SRC_EXPECTED,
				expr_type_slot, ctx->types.type);
		ast_slot_require_type(
				&env, node->loc, AST_CONSTR_SRC_EXPECTED,
				expr_slot, expr_type_slot);
	}

	struct ast_slot_result result[env.num_alloced_slots];

#if AST_DEBUG_SLOT_SOLVE
	printf("Solving ");
	ast_print_node(ctx, &env, node, true);
	printf("\n");
#endif

	int err;
	err = ast_slot_try_solve(
			ctx, mod, &env, result);

	if (err < 0) {
#if AST_DEBUG_SLOT_SOLVE
		printf("Failed (solve).\n\n");
#endif
		return -1;
	}

	err = ast_node_resolve_types(
			ctx, &env, result, node);
	if (err) {
#if AST_DEBUG_SLOT_SOLVE
		printf("Failed (resolve).\n\n");
#endif
		return -1;
	}

#if AST_DEBUG_SLOT_SOLVE
		printf("OK.\n\n");
#endif

	ast_env_free(&env);

	return 0;
}
