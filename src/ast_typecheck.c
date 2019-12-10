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

// deps must be an array of length closure->num_members.
static void
ast_fill_closure_deps(struct ast_context *ctx, struct ast_env *env,
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

		if (in_dep->determined) {
			if (mbr->require_const) {
				assert(in_dep->determined &&
						in_dep->req == AST_NAME_DEP_REQUIRE_VALUE);
				out_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;
				out_deps[i].value =
					ast_bind_slot_const(ctx, env, AST_BIND_NEW,
							in_dep->val);

				out_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;
				out_deps[i].determined = true;
				out_deps[i].val = in_dep->val;
			} else {
				assert(in_dep->determined);
				out_deps[i].req = AST_NAME_DEP_REQUIRE_TYPE;

				if (in_dep->req == AST_NAME_DEP_REQUIRE_TYPE) {
					ast_slot_id type_slot;
					type_slot =
						ast_bind_slot_const_type(ctx, env, AST_BIND_NEW,
								in_dep->type);

					out_deps[i].value =
						ast_bind_slot_closure(ctx, env, AST_BIND_NEW,
								type_slot);
					out_deps[i].type = in_dep->type;
				} else {
					out_deps[i].value =
						ast_bind_slot_const(ctx, env, AST_BIND_NEW,
								in_dep->val);
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

static bool
is_slot_func_type(struct ast_context *ctx, struct ast_env *env,
                ast_slot_id slot_id)
{
        struct ast_env_slot slot = ast_env_slot(ctx, env, slot_id);
        switch (slot.kind) {
                case AST_SLOT_CONS:
                        if (slot.cons.def == ctx->cons.func) {
                                return true;
                        }
                        break;

                case AST_SLOT_CONST_TYPE:
                        if (stg_type_is_func(ctx->vm, slot.const_type)) {
                                return true;
                        }
                        break;

                default:
                        break;
        }

        return false;
}

static void
ast_report_bind_error(struct ast_context *ctx, struct stg_location loc,
		struct ast_bind_result bind_res)
{
	switch (bind_res.code) {
		case AST_BIND_TYPE_MISMATCH:
			{
				struct string old_str, new_str;

				old_str = type_repr_to_alloced_string(
						ctx->vm, vm_get_type(ctx->vm, bind_res.type_mismatch.old));
				new_str = type_repr_to_alloced_string(
						ctx->vm, vm_get_type(ctx->vm, bind_res.type_mismatch.new));

				stg_error(ctx->err, loc,
						"Expected type '%.*s', got '%.*s'.",
						LIT(old_str), LIT(new_str));

				free(old_str.text);
				free(new_str.text);
			}
			break;

		case AST_BIND_VALUE_MISMATCH:
			{
				struct string old_str, new_str;

				old_str = obj_repr_to_alloced_string(
						ctx->vm, bind_res.value_mismatch.old);
				new_str = obj_repr_to_alloced_string(
						ctx->vm, bind_res.value_mismatch.new);

				stg_error(ctx->err, loc,
						"Expected value '%.*s', got '%.*s'.",
						LIT(old_str), LIT(new_str));

				free(old_str.text);
				free(new_str.text);
			}
			break;

		case AST_BIND_TYPE_VALUE_MISMATCH:
			{
				struct string old_str, new_str;

				struct object old_obj = {0}, new_obj = {0};

				old_obj.type = ctx->types.type;
				old_obj.data = &bind_res.type_mismatch.old;
				new_obj.type = ctx->types.type;
				new_obj.data = &bind_res.type_mismatch.new;

				old_str = obj_repr_to_alloced_string(
						ctx->vm, old_obj);
				new_str = obj_repr_to_alloced_string(
						ctx->vm, new_obj);

				stg_error(ctx->err, loc,
						"Expected value '%.*s', got '%.*s'.",
						LIT(old_str), LIT(new_str));

				free(old_str.text);
				free(new_str.text);
			}
			break;

		case AST_BIND_ARRAY_LENGTH_MISMATCH:
			stg_error(ctx->err, loc,
					"Expected the array to have length %zu, got %zu.",
					bind_res.array_length_mismatch.old,
					bind_res.array_length_mismatch.new);
			break;

		case AST_BIND_OBJ_HAS_NO_MEMBERS:
			{
				struct string type_name;
				type_name = type_repr_to_alloced_string(
						ctx->vm, vm_get_type(ctx->vm,
							bind_res.obj_no_members.obj_type));

				stg_error(ctx->err, loc,
						"Object of type '%.*s' does not have any members.",
						LIT(type_name));

				free(type_name.text);
			}
			break;

		case AST_BIND_TYPE_HAS_NO_MEMBERS:
			{
				struct string type_name;
				type_name = type_repr_to_alloced_string(
						ctx->vm, vm_get_type(ctx->vm,
							bind_res.type_no_members.obj_type));

				stg_error(ctx->err, loc,
						"This type, '%.*s', does not have any members.",
						LIT(type_name));

				free(type_name.text);
			}
			break;

		case AST_BIND_OBJ_MISSING_MEMBER:
			stg_error(ctx->err, loc,
					"This object does not have a member '%.*s'.",
					ALIT(bind_res.obj_missing_member.name));
			break;

		case AST_BIND_COMPILER_ERROR:
			stg_error(ctx->err, loc,
					"Compiler error.");
			break;

		default:
		case AST_BIND_OK:
			panic("Invalid bind result.");
	}
}

static ast_slot_id
ast_node_bind_slots(struct ast_context *ctx, size_t *num_errors, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node,
		ast_slot_id target, struct ast_typecheck_dep *deps, size_t num_deps);

// param_types is expected to be an array of length node->func.num_params.
static ast_slot_id
ast_node_func_bind_proto(struct ast_context *ctx, size_t *num_errors,
		struct ast_module *mod, struct ast_env *env, struct ast_node *node,
		ast_slot_id target, struct ast_typecheck_dep *deps, size_t num_deps,
		ast_slot_id *param_types)
{
	assert(node->kind == AST_NODE_FUNC ||
			node->kind == AST_NODE_FUNC_NATIVE);

	node->func.type = ast_bind_require_ok(
			ast_try_bind_slot_cons(ctx, env, node->func.type,
				ctx->cons.func));
	target = ast_bind_require_ok(
			ast_try_bind_slot_wildcard(ctx, env, target,
				node->func.type));
	node->func.slot = target;

	for (size_t i = 0 ; i < node->func.num_params; i++) {
		param_types[i] = ast_node_bind_slots(
				ctx, num_errors, mod, env, node->func.params[i].type,
				AST_BIND_NEW, deps, num_deps);
		struct ast_bind_result res;

		// TODO: We should figure out why param_types[i] might be pointing to a
		// substituted slot.
		ast_node_resolve_slot(env, &param_types[i]);

		// Ensure the type of the param types is type.
		res = ast_try_bind_slot_wildcard(
				ctx, env, param_types[i], AST_SLOT_TYPE);
		if (res.code != AST_BIND_OK) {
			ast_report_bind_error(
					ctx, node->loc, res);
			*num_errors += 1;
		} else {
			param_types[i] = res.ok.result;
			res.ok.result = AST_BIND_FAILED;
		}

		res = ast_try_bind_slot_wildcard(
				ctx, env, node->func.params[i].slot,
				param_types[i]);

		if (res.code != AST_BIND_OK) {
			ast_report_bind_error(
					ctx, node->loc, res);
			*num_errors += 1;
		} else {
			node->func.params[i].slot = res.ok.result;
			res.ok.result = AST_BIND_FAILED;
		}
	}

	ast_slot_id param_array_slot;
	param_array_slot = ast_bind_require_ok(
			ast_try_unpack_arg_named(
				ctx, env, ast_node_resolve_slot(env, &node->func.type),
				AST_BIND_NEW, ctx->atoms.func_cons_arg_params));

	struct ast_bind_result res;

	res = ast_try_bind_slot_cons_array(
			ctx, env, param_array_slot,
			param_types, node->func.num_params,
			AST_SLOT_TYPE);

	if (res.code != AST_BIND_OK) {
		ast_report_bind_error(
				ctx, node->loc, res);
		*num_errors += 1;
	} else {
		param_array_slot = res.ok.result;
		res.ok.result = AST_BIND_FAILED;
	}

	node->func.return_type_slot =
		ast_bind_require_ok(
				ast_try_unpack_arg_named(
					ctx, env, node->func.type,
					node->func.return_type_slot,
					ctx->atoms.func_cons_arg_ret));

	res = ast_try_bind_slot_wildcard(
			ctx, env, node->func.return_type_slot,
			AST_SLOT_TYPE);
	if (res.code != AST_BIND_OK) {
		ast_report_bind_error(
				ctx, node->loc, res);
		*num_errors += 1;
	} else {
		node->func.return_type_slot = res.ok.result;
		res.ok.result = AST_BIND_FAILED;
	}


	node->func.return_type_slot = ast_node_bind_slots(
			ctx, num_errors, mod, env, node->func.return_type,
			node->func.return_type_slot,
			deps, num_deps);

	return target;
}

static ast_slot_id
ast_templ_body_preliminary_bind_slots(struct ast_context *ctx, size_t *num_errors,
		struct ast_module *mod, struct ast_env *env, struct ast_node *node,
		ast_slot_id target, struct ast_typecheck_dep *deps, size_t num_deps)
{
	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
			{
				ast_slot_id param_types[node->func.num_params];
				target = ast_node_func_bind_proto(
						ctx, num_errors, mod, env,
						node, target, deps, num_deps,
						param_types);
			}
			break;

		case AST_NODE_COMPOSITE:
			target = ast_bind_slot_wildcard(
					ctx, env, target, AST_SLOT_TYPE);
			break;

		case AST_NODE_VARIANT:
			panic("TODO: Preliminary bind for variant in templ body.");
			break;

		default:
			panic("Invalid template body node.");
			break;
	}

	return target;
}

static ast_slot_id
ast_cons_node_bind_slots(struct ast_context *ctx, size_t *num_errors, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node,
		ast_slot_id target, struct ast_typecheck_dep *deps, size_t num_deps)
{
	assert(node->kind == AST_NODE_CONS);

	ast_slot_id func_slot = ast_node_value(ctx, env, node->call.func);

	struct object cons_obj;

	int err;
	err = ast_slot_pack(ctx, mod, env,
			func_slot, &cons_obj);

	struct ast_object_def *cons;
	if (type_equals(ctx->vm, cons_obj.type, ctx->types.type)) {
		type_id tid;
		tid = *(type_id *)cons_obj.data;

		struct type *type;
		type = vm_get_type(ctx->vm, tid);

		if (!type->obj_def) {
			// TODO: Add type name to error message.
			stg_error(ctx->err, node->call.func->loc,
					"This type can not be used as a constructor.");
			*num_errors += 1;
			return AST_BIND_FAILED;
		}

		cons = type->obj_def;
	} else {
		assert_type_equals(ctx->vm, cons_obj.type, ctx->types.cons);
		cons = *(struct ast_object_def **)cons_obj.data;
	}

	struct ast_bind_result res;
	res = ast_try_bind_slot_cons(
			ctx, env, target, cons);
	if (res.code != AST_BIND_OK) {
		ast_report_bind_error(
				ctx, node->loc, res);
		*num_errors += 1;
	} else {
		node->call.cons = res.ok.result;
		target = node->call.cons;
	}

	bool is_named = false;
	for (size_t i = 0; i < node->call.num_args; i++) {
		bool this_is_named;
		this_is_named = !!node->call.args[i].name;
		assert(i == 0 || is_named == this_is_named);
		is_named = this_is_named;
	}

	node->call.ret_type =
		ast_env_slot(ctx, env, node->call.cons).type;

	struct ast_env_slot slot;
	slot = ast_env_slot(ctx, env,
			ast_node_resolve_slot(env, &node->call.cons));
	assert(slot.kind == AST_SLOT_CONS);

	if (node->call.num_args > slot.cons.def->num_params) {
		stg_error(ctx->err, node->loc,
				"Expected at most %zu arguments, got %zu.",
				slot.cons.def->num_params,
				node->call.num_args);
		*num_errors += 1;
		return AST_BIND_FAILED;
	}

	for (size_t i = 0; i < node->call.num_args; i++) {
		struct atom *name;
		if (is_named) {
			name = node->call.args[i].name;
		} else {
			name = slot.cons.def->params[i].name;
		}

		ast_slot_id arg_slot = AST_BIND_FAILED;
		struct ast_bind_result res;
		res = ast_try_unpack_arg_named(
				ctx, env, node->call.cons,
				AST_BIND_NEW, name);
		if (res.code != AST_BIND_OK) {
			ast_report_bind_error(
					ctx, node->loc, res);
			*num_errors += 1;
		} else {
			arg_slot = res.ok.result;
		}
		ast_node_bind_slots(
				ctx, num_errors, mod, env, node->call.args[i].value,
				arg_slot, deps, num_deps);
	}

	return target;
}

static ast_slot_id
ast_node_bind_slots(struct ast_context *ctx, size_t *num_errors, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node,
		ast_slot_id target, struct ast_typecheck_dep *deps, size_t num_deps)
{
	switch (node->kind) {
	case AST_NODE_FUNC:
	case AST_NODE_FUNC_NATIVE:
		{
			ast_slot_id param_types[node->func.num_params];

			target = ast_node_func_bind_proto(
					ctx, num_errors, mod, env,
					node, target, deps, num_deps,
					param_types);

			if (node->kind == AST_NODE_FUNC) {
				ast_slot_id body_slot;
				body_slot = ast_bind_require_ok(
						ast_try_bind_slot_wildcard(
							ctx, env, AST_BIND_NEW,
							node->func.return_type_slot));

				size_t num_body_deps;

				num_body_deps =
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

					body_deps[i].value =
						ast_bind_require_ok(
								ast_try_bind_slot_param(ctx, env, AST_BIND_NEW,
									i, param_types[i]));
				}

				ast_fill_closure_deps(ctx, env,
						body_deps + node->func.num_params, &node->func.closure,
						deps, num_deps);

				struct ast_closure_target *closure;
				closure = &node->func.closure;

				body_slot = ast_node_bind_slots(
						ctx, num_errors, mod, env, node->func.body,
						body_slot, body_deps, num_body_deps);
			}
		}
		break;

	case AST_NODE_CALL:
		{
			size_t local_errors = 0;
			// Evaluate func type
			ast_slot_id func_slot;
			func_slot = ast_node_bind_slots(
					ctx, &local_errors, mod, env, node->call.func,
					AST_BIND_NEW, deps, num_deps);

			*num_errors += local_errors;

			if (local_errors) {
				// Go through the parameters to report errors.
				for (size_t i = 0; i < node->call.num_args; i++) {
					ast_node_bind_slots(
							ctx, num_errors, mod, env,
							node->call.args[i].value,
							AST_BIND_NEW, deps, num_deps);
				}
				return AST_BIND_FAILED;
			}

			ast_slot_id func_type_slot;
			func_type_slot = ast_env_slot(ctx, env, func_slot).type;
				
			type_id func_type;
			int err;
			err = ast_slot_pack_type(ctx, mod, env,
					func_type_slot, &func_type);
			if (err) {
				panic("Failed to pack call func.");
				return AST_BIND_FAILED;
			}

			if (!stg_type_is_func(ctx->vm, func_type)) {
				if (func_type == ctx->types.cons) {
					struct object cons_obj;
					err = ast_slot_pack(ctx, mod, env,
							func_slot, &cons_obj);

					assert_type_equals(ctx->vm, cons_obj.type, ctx->types.cons);

					struct ast_object_def *cons;
					cons = *(struct ast_object_def **)cons_obj.data;

					if (is_slot_func_type(ctx, &cons->env, cons->ret_type)) {
						struct ast_node *func_cons;
						func_cons = ast_init_node_cons(
								ctx, env, AST_NODE_NEW,
								node->call.func->loc,
								node->call.func, NULL, 0);

						ast_cons_node_bind_slots(
								ctx, num_errors, mod,
								env, func_cons, AST_BIND_NEW,
								deps, num_deps);

						node->call.func = func_cons;

						func_slot = ast_node_value(ctx, env, node->call.func);
						func_type_slot = ast_node_type(ctx, env, node->call.func);
					} else {
						// TODO: Print the (partial) type of the constructor.
						stg_error(ctx->err, node->call.func->loc,
								"This return value of this constructor is not callable.");

						*num_errors += 1;
						return AST_BIND_FAILED;
					}
				} else {
					struct string type_name;

					type_name = type_repr_to_alloced_string(
							ctx->vm, vm_get_type(ctx->vm, func_type));

					stg_error(ctx->err, node->call.func->loc,
							"Object of type '%.*s' is not callable.",
							LIT(type_name));

					free(type_name.text);

					*num_errors += 1;
					return AST_BIND_FAILED;
				}
			}

			// bind all param types to func type
			struct ast_bind_result res;
			res = ast_try_unpack_arg_named(
					ctx, env,
					ast_node_resolve_slot(env, &func_type_slot),
					ast_node_resolve_slot(env, &node->call.ret_type),
					ctx->atoms.func_cons_arg_ret);
			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
				return AST_BIND_FAILED;
			} else {
				node->call.ret_type = res.ok.result;
				res.ok.result = AST_BIND_FAILED;
			}

			ast_slot_id param_types_slot =
				ast_bind_require_ok(
						ast_try_unpack_arg_named(
							ctx, env, func_type_slot, AST_BIND_NEW,
							ctx->atoms.func_cons_arg_params));

			res = ast_try_bind_slot_cons_array(
					ctx, env, param_types_slot, NULL,
					node->call.num_args, AST_SLOT_TYPE);
			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				param_types_slot = res.ok.result;
				res.ok.result = AST_BIND_FAILED;
			}

			res = ast_try_bind_slot_wildcard(
					ctx, env, target, node->call.ret_type);
			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				target = res.ok.result;
			}

			struct ast_env_slot param_types;
			param_types = ast_env_slot(ctx, env, param_types_slot);
			assert(param_types.kind == AST_SLOT_CONS_ARRAY);

			if (param_types.cons_array.num_members != node->call.num_args) {
				stg_error(ctx->err, node->loc,
						"Expected %zu arguments, got %zu.",
						param_types.cons_array.num_members,
						node->call.num_args);
				*num_errors += 1;
				return AST_BIND_FAILED;
			}

			for (size_t i = 0; i < node->call.num_args; i++) {
				ast_slot_id arg_type_slot;
				arg_type_slot = param_types.cons_array.members[i];

				ast_slot_id arg_slot;
				arg_slot = ast_bind_require_ok(
						ast_try_bind_slot_wildcard(ctx, env,
							AST_BIND_NEW, arg_type_slot));

				ast_node_bind_slots(
						ctx, num_errors, mod, env, node->call.args[i].value,
						arg_slot, deps, num_deps);
			}
		}
		break;

	case AST_NODE_CONS:
		{
			size_t local_errors = 0;
			// Evaluate func type
			ast_slot_id func_slot;
			func_slot = ast_node_bind_slots(
					ctx, &local_errors, mod, env, node->call.func,
					AST_BIND_NEW, deps, num_deps);

			*num_errors += local_errors;

			if (local_errors) {
				// Go through the parameters to report errors.
				for (size_t i = 0; i < node->call.num_args; i++) {
					ast_node_bind_slots(
							ctx, num_errors, mod, env,
							node->call.args[i].value,
							AST_BIND_NEW, deps, num_deps);
				}
				return AST_BIND_FAILED;
			}

			target = ast_cons_node_bind_slots(
					ctx, num_errors, mod,
					env, node, target,
					deps, num_deps);
		}
		break;

	case AST_NODE_FUNC_TYPE:
		{
			struct ast_bind_result res;

			res = ast_try_bind_slot_cons(
					ctx, env, target, ctx->cons.func);

			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				target = res.ok.result;
			}

			node->func_type.slot = target;

			ast_slot_id param_types_slot = AST_BIND_FAILED;
			res = ast_try_unpack_arg_named(
					ctx, env, target, AST_BIND_NEW,
					ctx->atoms.func_cons_arg_params);
			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				param_types_slot = res.ok.result;
			}

			res = ast_try_bind_slot_cons_array(
					ctx, env, param_types_slot, NULL,
					node->func_type.num_params, AST_SLOT_TYPE);
			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				param_types_slot = res.ok.result;
				res.ok.result = AST_BIND_FAILED;
			}

			struct ast_env_slot param_types;
			param_types = ast_env_slot(ctx, env, param_types_slot);

			for (size_t i = 0; i < node->func_type.num_params; i++) {
				ast_slot_id param_type_slot;
				param_type_slot = param_types.cons_array.members[i];

				ast_node_bind_slots(
						ctx, num_errors, mod, env,
						node->func_type.param_types[i],
						param_type_slot, deps, num_deps);
			}

			res = ast_try_unpack_arg_named(
					ctx, env, target, AST_BIND_NEW,
					ctx->atoms.func_cons_arg_ret);

			ast_slot_id ret_type_slot = AST_BIND_FAILED;

			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				ret_type_slot = res.ok.result;
			}

			ast_node_bind_slots(
					ctx, num_errors, mod, env,
					node->func_type.ret_type,
					ret_type_slot, deps, num_deps);
		}
		break;

	case AST_NODE_ACCESS:
		{
			ast_slot_id slot;
			slot = ast_node_bind_slots(
					ctx, num_errors, mod, env, node->access.target,
					AST_BIND_NEW, deps, num_deps);

			if (slot != AST_BIND_FAILED) {
				struct ast_bind_result res;
				res = ast_try_unpack_arg_named(ctx, env,
						slot, target, node->access.name);
				if (res.code != AST_BIND_OK) {
					ast_report_bind_error(
							ctx, node->loc, res);
					*num_errors += 1;
					target = AST_BIND_FAILED;
				} else {
					target = res.ok.result;
				}
				node->access.slot = target;
			}
		}
		break;

	case AST_NODE_TEMPL:
		{
			struct ast_typecheck_dep body_deps[num_deps + node->templ.num_params];
			memset(body_deps, 0, sizeof(struct ast_typecheck_dep) *
					(num_deps + node->templ.num_params));
			ast_fill_closure_deps(ctx, env,
					body_deps, &node->templ.closure,
					deps, num_deps);


			for (size_t i = 0; i < node->templ.num_params; i++) {
				ast_slot_id type_slot;
				type_slot = ast_bind_slot_wildcard(
						ctx, env, AST_BIND_NEW,
						AST_SLOT_TYPE);

				if (node->templ.params[i].type) {
					body_deps[num_deps+i].determined = true;

					type_slot = ast_node_bind_slots(
							ctx, num_errors, mod, env,
							node->templ.params[i].type,
							type_slot, deps, num_deps);
				} else {
					body_deps[num_deps+i].determined = false;
				}

				node->templ.params[i].slot =
					ast_bind_slot_templ(
							ctx, env, node->templ.params[i].slot,
							type_slot);

				body_deps[num_deps+i].req = AST_NAME_DEP_REQUIRE_VALUE;
				body_deps[num_deps+i].ref.kind = AST_NAME_REF_TEMPL;
				body_deps[num_deps+i].ref.templ = i;
				body_deps[num_deps+i].lookup_failed = false;
				body_deps[num_deps+i].value = node->templ.params[i].slot;
			}

			ast_slot_id body_slot;
			body_slot = ast_templ_body_preliminary_bind_slots(
					ctx, num_errors, mod, env,
					node->templ.body, target,
					body_deps, num_deps + node->templ.num_params);

			/*
			ast_slot_id body_slot;
			body_slot = ast_node_bind_slots(
					ctx, num_errors, mod, env,
					node->templ.body, AST_BIND_NEW,
					body_deps, num_deps + node->templ.num_params);
			*/

			if (!node->templ.def) {
				node->templ.def = ast_node_create_templ(
						ctx, mod, env, node, body_deps, num_deps);
				assert(node->templ.def);
			}

			struct object obj = {0};
			obj.type = ctx->types.cons;
			obj.data = &node->templ.def;

			obj = register_object(ctx->vm, env->store, obj);

			struct ast_bind_result res;
			res = ast_try_bind_slot_const(
					ctx, env, target, obj);

			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				target = res.ok.result;
			}

			node->templ.slot = target;
		}
		break;

	case AST_NODE_SLOT:
		{
			struct ast_bind_result bind_res;
			bind_res = ast_try_union_slot(ctx, env, target, node->slot);
			if (bind_res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, bind_res);
				*num_errors += 1;
			} else {
				target = bind_res.ok.result;
			}
			node->slot = target;
		}
		break;

	case AST_NODE_LIT:
		{
			struct ast_bind_result bind_res;
			bind_res = ast_try_bind_slot_const(ctx, env, target,
					node->lit.obj);
			if (bind_res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, bind_res);
				*num_errors += 1;
			} else {
				target = bind_res.ok.result;
			}
			node->lit.slot = target;
		}
		break;

	case AST_NODE_LOOKUP:
		{
			struct ast_typecheck_dep *res;
			res = ast_find_dep(deps, num_deps, node->lookup.ref);
			assert(res);
			if (res->lookup_failed) {
				stg_error(ctx->err, node->loc,
						"Object not found.");
				*num_errors += 1;
				target = AST_BIND_FAILED;
			} else {
				assert(res->value == AST_SLOT_TYPE || res->value >= 0);

				struct ast_bind_result bind_res;
				bind_res = ast_try_union_slot(ctx, env, target, res->value);
				if (bind_res.code != AST_BIND_OK) {
					ast_report_bind_error(
							ctx, node->loc, bind_res);
					*num_errors += 1;
					target = AST_BIND_FAILED;
				} else {
					target = bind_res.ok.result;
				}
			}

			node->lookup.slot = target;
		}
		break;

	case AST_NODE_COMPOSITE:
		{
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
					*num_errors += 1;
				}
			}

			struct ast_bind_result res;
			res = ast_try_bind_slot_const_type(
					ctx, env, target, node->composite.type);
			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				target = res.ok.result;
			}
			node->composite.ret_value = target;
		}
		break;

	case AST_NODE_VARIANT:
		{
			if (node->variant.type == TYPE_UNSET) {
				node->variant.type =
					ast_dt_finalize_variant(
							ctx, mod, env,
							node->variant.options,
							node->variant.num_options,
							deps, num_deps);

				if (node->variant.type == TYPE_UNSET) {
					*num_errors += 1;
				}
			}

			struct ast_bind_result res;
			res = ast_try_bind_slot_const_type(
					ctx, env, target, node->variant.type);
			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				target = res.ok.result;
			}

			res = ast_try_union_slot(
					ctx, env, node->variant.ret_value, target);
			if (res.code != AST_BIND_OK) {
				ast_report_bind_error(
						ctx, node->loc, res);
				*num_errors += 1;
			} else {
				target = res.ok.result;
			}

			node->variant.ret_value = target;
		}
		break;
	}

	return target;
}

static int
ast_node_resolve_types(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node)
{
	int errors = 0;
#define VISIT_NODE(child) errors += ast_node_resolve_types(ctx, mod, env, child);
	AST_NODE_VISIT(node, false, false, true, false);
#undef VISIT_NODE

	assert(node->type == TYPE_UNSET);

	ast_slot_id type_slot;
	type_slot = ast_node_type(ctx, env, node);

	int err;
	err = ast_slot_pack_type(
			ctx, mod, env, type_slot, &node->type);
	if (err) {
		errors += 1;
	}

	return errors;
}

int
ast_node_typecheck(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node,
		struct ast_typecheck_dep *deps, size_t num_deps,
		type_id expected_type)
{
	size_t num_errors = 0;
	int err;

	ast_slot_id result = AST_BIND_NEW;

	if (expected_type != TYPE_UNSET) {
		ast_slot_id type_slot;

		type_slot = ast_bind_slot_const_type(
				ctx, env, AST_BIND_NEW, expected_type);

		result = ast_bind_slot_wildcard(
				ctx, env, AST_BIND_NEW,
				type_slot);
	}

	result = ast_node_bind_slots(
			ctx, &num_errors, mod,
			env, node, result,
			deps, num_deps);

	if (num_errors > 0) {
		assert(ctx->err->num_errors > 0);
		return -1;
	}

	err = ast_node_resolve_types(ctx, mod, env, node);

	if (err) {
		return -1;
	}

	return 0;
}
