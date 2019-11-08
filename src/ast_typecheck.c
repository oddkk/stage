#include "ast.h"
#include "vm.h"
#include "utils.h"
#include "error.h"
#include "base/mod.h"
#include <stdio.h>
#include <stdlib.h>

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
	}

	return false;
}

static struct ast_typecheck_dep *
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

		if (mbr->require_const) {
			assert(in_dep->determined && 
					in_dep->req == AST_NAME_DEP_REQUIRE_VALUE);
			out_deps[i].req = AST_NAME_DEP_REQUIRE_VALUE;
			out_deps[i].value =
				ast_bind_slot_const(ctx, env, AST_BIND_NEW,
						NULL, in_dep->val);

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
							NULL, in_dep->type);

				out_deps[i].value =
					ast_bind_slot_closure(ctx, env, AST_BIND_NEW,
							NULL, type_slot);
				out_deps[i].type = in_dep->type;
			} else {
				out_deps[i].value =
					ast_bind_slot_const(ctx, env, AST_BIND_NEW,
							NULL, in_dep->val);
				out_deps[i].val = in_dep->val;
			}

			out_deps[i].req = in_dep->req;
			out_deps[i].determined = true;
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


static ast_slot_id
ast_node_bind_slots(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node,
		ast_slot_id target, struct ast_typecheck_dep *deps, size_t num_deps)
{
	switch (node->kind) {
	case AST_NODE_FUNC:
	case AST_NODE_FUNC_NATIVE:
		{
			node->func.type = ast_bind_slot_cons(ctx, env, AST_BIND_NEW,
					NULL, ctx->cons.func);
			target = ast_bind_slot_wildcard(ctx, env, target,
						NULL, node->func.type);
			node->func.slot = target;

			ast_slot_id param_types[node->func.num_params];

			for (size_t i = 0 ; i < node->func.num_params; i++) {
				param_types[i] = ast_node_bind_slots(
						ctx, mod, env, node->func.params[i].type,
						AST_BIND_NEW, deps, num_deps);
				node->func.params[i].slot =
					ast_bind_slot_wildcard(ctx, env, AST_BIND_NEW,
							NULL, param_types[i]);
			}

			ast_slot_id param_array_slot;
			param_array_slot = ast_unpack_arg_named(
					ctx, env, node->func.type, AST_BIND_NEW,
					ctx->atoms.func_cons_arg_params);

			param_array_slot = ast_bind_slot_cons_array(
					ctx, env, param_array_slot, NULL,
					param_types, node->func.num_params,
					AST_SLOT_TYPE);

			node->func.return_type_slot =
				ast_unpack_arg_named(
						ctx, env, node->func.type, AST_BIND_NEW,
						ctx->atoms.func_cons_arg_ret);

			node->func.return_type_slot = ast_node_bind_slots(
					ctx, mod, env, node->func.return_type,
					node->func.return_type_slot,
					deps, num_deps);

			if (node->kind == AST_NODE_FUNC) {
				ast_slot_id body_slot;
				body_slot = ast_bind_slot_wildcard(
						ctx, env, AST_BIND_NEW, NULL,
						node->func.return_type_slot);

				size_t num_body_deps;

				num_body_deps =
					node->func.closure.num_members +
					node->func.num_params;

				struct ast_typecheck_dep body_deps[num_body_deps];

				for (size_t i = 0; i < node->func.num_params; i++) {
					body_deps[i].req = AST_NAME_DEP_REQUIRE_TYPE;
					body_deps[i].ref.kind = AST_NAME_REF_PARAM;
					body_deps[i].ref.param = i;

					body_deps[i].value =
						ast_bind_slot_param(ctx, env, AST_BIND_NEW,
								NULL, i, param_types[i]);
				}

				ast_fill_closure_deps(ctx, env,
						body_deps + node->func.num_params, &node->func.closure,
						deps, num_deps);

				body_slot = ast_node_bind_slots(
						ctx, mod, env, node->func.body,
						body_slot, body_deps, num_body_deps);
			}
		}
		break;

	case AST_NODE_CONS:
	case AST_NODE_CALL:
		{
			// Evaluate func type
			ast_slot_id func_slot;
			func_slot = ast_node_bind_slots(
					ctx, mod, env, node->call.func,
					AST_BIND_NEW, deps, num_deps);

			// Determine if func is cons or func
			//  - if func_slot type is func-like or cons that returns func, this is a call
			//  - else if slot is cons or type, this is cons
			//  - else error

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

			if (stg_type_is_func(ctx->vm, func_type)) {
				node->kind = AST_NODE_CALL;
			} else if (func_type == ctx->types.cons) {
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

					func_cons->call.cons = ast_bind_slot_cons(
							ctx, env, AST_BIND_NEW, NULL, cons);

					node->call.func = func_cons;
					node->kind = AST_NODE_CALL;
				} else {
					node->kind = AST_NODE_CONS;
					node->call.cons = ast_bind_slot_cons(
							ctx, env, AST_BIND_NEW, NULL, cons);
				}
			} else if (func_type == ctx->types.type) {
				node->kind = AST_NODE_CONS;

				type_id tid;
				err = ast_slot_pack_type(ctx, mod, env,
						func_slot, &tid);

				struct type *type;
				type = vm_get_type(ctx->vm, tid);

				if (!type->obj_def) {
					// TODO: Add type name to error message.
					stg_error(ctx->err, node->call.func->loc,
							"This type can not be used as a constructor.");
					return AST_BIND_FAILED;
				}

				node->call.cons = ast_bind_slot_cons(
						ctx, env, AST_BIND_NEW, NULL, type->obj_def);
			} else {
				// TODO: Add type name to error message.
				stg_error(ctx->err, node->call.func->loc,
						"This object is not callable.");
				return AST_BIND_FAILED;
			}

			// bind all param types to func type
			if (node->kind == AST_NODE_CALL) {
				node->call.ret_type = ast_unpack_arg_named(
						ctx, env, func_type_slot, AST_BIND_NEW,
						ctx->atoms.func_cons_arg_ret);

				ast_slot_id param_types_slot = ast_unpack_arg_named(
						ctx, env, func_type_slot, AST_BIND_NEW,
						ctx->atoms.func_cons_arg_params);

				target = ast_bind_slot_wildcard(
						ctx, env, target, NULL, node->call.ret_type);

				struct ast_env_slot param_types;
				param_types = ast_env_slot(ctx, env, param_types_slot);
				assert(param_types.kind == AST_SLOT_CONS_ARRAY);

				if (param_types.cons_array.num_members != node->call.num_args) {
					stg_error(ctx->err, node->loc,
							"Expected %zu arguments, got %zu.",
							param_types.cons_array.num_members,
							node->call.num_args);
					return AST_BIND_FAILED;
				}

				for (size_t i = 0; i < node->call.num_args; i++) {
					ast_slot_id arg_type_slot;
					arg_type_slot = param_types.cons_array.members[i];

					ast_slot_id arg_slot;
					arg_slot =
						ast_bind_slot_wildcard(ctx, env,
								AST_BIND_NEW, NULL, arg_type_slot);
					ast_node_bind_slots(
							ctx, mod, env, node->call.args[i].value,
							arg_slot, deps, num_deps);

				}
			} else if (node->kind == AST_NODE_CONS) {
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
				slot = ast_env_slot(ctx, env, node->call.cons);
				assert(slot.kind == AST_SLOT_CONS);

				if (node->call.num_args > slot.cons.def->num_params) {
					stg_error(ctx->err, node->loc,
							"Expected at most %zu arguments, got %zu.",
							slot.cons.def->num_params,
							node->call.num_args);
					return AST_BIND_FAILED;
				}

				for (size_t i = 0; i < node->call.num_args; i++) {
					struct atom *name;
					if (is_named) {
						name = node->call.args[i].name;
					} else {
						name = slot.cons.def->params[i].name;
					}

					ast_slot_id arg_slot;
					arg_slot = ast_unpack_arg_named(
							ctx, env, node->call.cons,
							AST_BIND_NEW, node->call.args[i].name);
					ast_node_bind_slots(
							ctx, mod, env, node->call.args[i].value,
							arg_slot, deps, num_deps);
				}
			} else {
				panic("Invalid node type after call func evaluation.");
				return AST_BIND_FAILED;
			}
		}
		break;

	case AST_NODE_ACCESS:
		{
			ast_slot_id slot;
			slot = ast_node_bind_slots(
					ctx, mod, env, node->access.target,
					AST_BIND_NEW, deps, num_deps);

			target = ast_unpack_arg_named(ctx, env,
					slot, target, node->access.name);
			node->slot = target;
		}
		break;

	case AST_NODE_TEMPL:
		// TODO: Implement templates.
		break;

	case AST_NODE_SLOT:
		target = ast_union_slot(ctx, env, target, node->slot);
		node->slot = target;
		break;

	case AST_NODE_LIT:
		target = ast_bind_slot_const(ctx, env, target,
				NULL, node->lit.obj);
		node->lit.slot = target;
		break;

	case AST_NODE_LOOKUP:
		{
			struct ast_typecheck_dep *res;
			res = ast_find_dep(deps, num_deps, node->lookup.ref);
			assert(res->value == AST_SLOT_TYPE || res->value >= 0);
			target = ast_union_slot(ctx, env, target, res->value);
			node->lookup.slot = target;
		}
		break;

	case AST_NODE_COMPOSITE:
		{
			if (node->composite.type == TYPE_UNSET) {
				struct ast_closure_target *closure;
				closure = &node->composite.closure;

				struct ast_typecheck_closure closure_values[closure->num_members];

				for (size_t i = 0; i < closure->num_members; i++) {
					struct ast_typecheck_dep *dep;
					dep = ast_find_dep(deps, num_deps,
							closure->members[i].ref);
					assert(dep);

					if (closure->members[i].require_const || (
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

				node->composite.type =
					ast_dt_finalize_composite(
							ctx, mod, env, node,
							closure_values, closure->num_members);
			}

			target = ast_bind_slot_const_type(
					ctx, env, target, NULL, node->composite.type);
			node->composite.ret_value = target;
		}
		break;

	case AST_NODE_VARIANT:
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
	AST_NODE_VISIT(node, false, true);
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
		struct ast_typecheck_dep *deps, size_t num_deps)
{
	int err;
	ast_node_bind_slots(ctx, mod, env, node, AST_BIND_NEW,
			deps, num_deps);

	err = ast_node_resolve_types(ctx, mod, env, node);

	if (err) {
		return -1;
	}

	return 0;
}