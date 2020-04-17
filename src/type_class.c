#include "type_class.h"
#include "vm.h"
#include "ast.h"
#include "arena.h"
#include "dlist.h"
#include <string.h>

static struct object_cons_base stg_tc_cons_base = {
	.name = STR("class"),
};

static struct stg_type_class_impl *
stg_type_class_find_impl(
		struct vm *vm, struct stg_type_class *tc,
		struct object *args)
{
	for (size_t impl_i = 0; impl_i < tc->num_impls; impl_i++) {
		struct stg_type_class_impl *impl;
		impl = &tc->impls[impl_i];
		bool match = true;
		for (size_t arg_i = 0; arg_i < tc->num_params; arg_i++) {
			if (impl->args[arg_i].constant) {
				if (!obj_equals(vm, args[arg_i], impl->args[arg_i].constant_value)) {
					match = false;
					break;
				}
			} else {
				panic("TODO: Parametric impls");
				match = false;
				break;
			}
		}

		if (match) {
			return impl;
		}
	}

	return NULL;
}

static void
stg_tc_report_missing_impl(struct ast_context *ctx,
		struct stg_type_class *tc, struct object *params)
{
	struct arena *str_mem = &ctx->vm->transient;
	struct string param_values_desc = {0};
	for (size_t i = 0; i < tc->num_params; i++) {
		arena_string_append_sprintf(
				str_mem, &param_values_desc, "\n%.*s = ",
				ALIT(tc->params[i].name));
		arena_string_append_obj_repr(
				&param_values_desc, ctx->vm, str_mem, &params[i]);
	}

	// TODO: Location
	stg_error(ctx->err, STG_NO_LOC,
			"No implementation of this type class for:%.*s",
			LIT(param_values_desc));
}

static int
stg_tc_pack(struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, void *data, void *out,
		void **params, size_t num_params)
{
	struct stg_type_class *tc = data;
	assert(num_params == tc->num_params);

	struct object param_vals[tc->num_params];
	for (size_t i = 0; i < tc->num_params; i++) {
		param_vals[i].data = params[i];
		param_vals[i].type = tc->params[i].type;
	}

	struct stg_type_class_impl *impl;

	impl = stg_type_class_find_impl(
			mod->vm, tc, param_vals);
	if (!impl) {
		stg_tc_report_missing_impl(ctx, tc, param_vals);
		return -1;
	}

	if (impl->num_params == 0) {
		struct type *impl_type;
		impl_type = vm_get_type(ctx->vm, impl->constant_inst.type);

		memcpy(out, impl->constant_inst.data, impl_type->size);
	} else {
		panic("TODO: Parametric impls.");
	}

	return 0;
}

static type_id
stg_tc_pack_type(struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	struct stg_type_class *tc = data;
	assert(num_params == tc->num_params);

	struct object param_vals[tc->num_params];
	for (size_t i = 0; i < tc->num_params; i++) {
		param_vals[i].data = params[i];
		param_vals[i].type = tc->params[i].type;
	}

	struct stg_type_class_impl *impl;

	impl = stg_type_class_find_impl(
			mod->vm, tc, param_vals);
	if (!impl) {
		stg_tc_report_missing_impl(ctx, tc, param_vals);
		return TYPE_UNSET;
	}

	if (impl->num_params == 0) {
		return impl->constant_inst.type;
	} else {
		panic("TODO: Parametric impls");
		return TYPE_UNSET;
	}
}

static int
stg_tc_unpack(struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, void *data, void *out,
		struct object obj, int param_id)
{
	struct stg_type_class *tc = data;
	assert(param_id < tc->num_params);

	for (size_t impl_i = 0; impl_i < tc->num_impls; impl_i++) {
		struct stg_type_class_impl *impl;
		impl = &tc->impls[impl_i];

		if (impl->num_params > 0) {
			continue;
		}

		if (obj_equals(ctx->vm, impl->constant_inst, obj)) {
			if (impl->args[param_id].constant) {
				struct type *param_type;
				param_type = vm_get_type(ctx->vm, impl->args[param_id].constant_value.type);
				memcpy(out, impl->args[param_id].constant_value.data, param_type->size);
			} else {
				panic("TODO: Parametric impls");
			}
			return 0;
		}
	}

	return -1;
}

/*
static bool
stg_tc_can_unpack(struct vm *vm, void *data, void *obj)
{
	struct stg_type_class *tc = data;

	for (size_t impl_i = 0; impl_i < tc->num_impls; impl_i++) {
		struct stg_type_class_impl *impl;
		impl = &tc->impls[impl_i];

		if (obj_equals(ctx->vm, impl->inst, obj)) {
		}
	}
}
*/

struct stg_type_class *
stg_type_class_create(struct stg_module *mod,
		struct stg_type_class_param *params, size_t num_params,
		struct stg_type_class_member *members, size_t num_members)
{
	struct stg_type_class *tc;
	tc = arena_alloc(&mod->mem, sizeof(struct stg_type_class));

	tc->num_params = num_params;
	tc->params = arena_allocn(&mod->mem,
			num_params, sizeof(struct stg_type_class_param));
	memcpy(tc->params, params,
			num_params * sizeof(struct stg_type_class_param));

	tc->num_members = num_members;
	tc->members = arena_allocn(&mod->mem,
			num_members, sizeof(struct stg_type_class_member));
	memcpy(tc->members, members,
			num_members * sizeof(struct stg_type_class_member));

	struct object_cons *cons;
	cons = arena_alloc(&mod->mem, sizeof(struct object_cons));
	cons->num_params = num_params;
	cons->params = arena_allocn(&mod->mem,
			num_params, sizeof(struct object_cons_param));

	for (size_t i = 0; i < tc->num_params; i++) {
		cons->params[i].name = tc->params[i].name;
		cons->params[i].type = tc->params[i].type;
		cons->params[i].def_loc = tc->params[i].decl_loc;
	}

	cons->base = &stg_tc_cons_base;
	cons->data = tc;

	cons->ct_pack = stg_tc_pack;
	cons->ct_pack_type = stg_tc_pack_type;
	cons->ct_unpack = stg_tc_unpack;
	// cons->impose_constraints = ;
	// cons->can_unpack = ;

	tc->cons = cons;

	return tc;
}

struct stg_type_class *
stg_type_class_from_ast_node(struct ast_context *ctx,
		struct stg_module *mod, struct ast_node *node,
		struct ast_typecheck_dep *deps, size_t num_deps)
{
	assert(node->kind == AST_NODE_TYPE_CLASS);
	if (node->type_class.cons) {
		assert(node->type_class.cons->base == &stg_tc_cons_base);
		return (struct stg_type_class *)node->type_class.cons->data;
	}

	struct ast_env env = {0};
	env.store = &mod->store;

#if AST_DEBUG_UNINITIALIZED_SLOT_ID
	// Add a slot 0 to debug invalid references.
	ast_slot_alloc(&env);
#endif

	size_t num_params = node->type_class.pattern.num_params;
	size_t num_closure_members = node->type_class.closure.num_members;
	size_t num_body_deps = num_closure_members + num_params;
	struct ast_typecheck_dep *body_deps;
	body_deps = arena_allocn(&mod->mem,
			num_body_deps, sizeof(struct ast_typecheck_dep));
	memset(body_deps, 0, sizeof(struct ast_typecheck_dep) * num_body_deps);
	ast_constr_fill_closure_deps(ctx, NULL,
			body_deps, &node->type_class.closure,
			deps, num_deps);
	ast_typecheck_deps_slots(&env,
			body_deps, num_deps);

	struct ast_typecheck_dep param_deps[num_deps];
	memcpy(param_deps, deps, num_deps * sizeof(struct ast_typecheck_dep));
	ast_typecheck_deps_slots(&env,
			param_deps, num_deps);

	struct stg_type_class_param params[num_params];
	ast_slot_id param_slots[num_params];

	for (size_t i = 0; i < num_params; i++) {
		struct ast_pattern_param *param;
		param = &node->type_class.pattern.params[i];
		params[i].name = param->name;
		params[i].type = TYPE_UNSET;
		params[i].decl_loc = param->loc;

		param_slots[i] = ast_slot_alloc(&env);

		struct ast_typecheck_dep *dep;
		dep = &body_deps[num_closure_members+i];

		dep->req = AST_NAME_DEP_REQUIRE_VALUE;
		dep->ref.kind = AST_NAME_REF_TEMPL;
		dep->ref.templ = i;
		dep->determined = false;
		dep->value = param_slots[i];

		if (param->type) {
			ast_slot_id type_slot;
			type_slot = ast_node_constraints(
					ctx, mod, &env, param_deps, num_deps,
					param->type);

			ast_slot_require_type(
					&env, param->type->loc,
					AST_CONSTR_SRC_TEMPL_PARAM_DECL,
					param_slots[i], type_slot);
		}
	}

	size_t num_members = node->type_class.num_members;
	struct stg_type_class_member members[num_members];
	memset(members, 0, num_members * sizeof(struct stg_type_class_member));

	size_t num_member_params = 0;
	for (size_t i = 0; i < num_members; i++) {
		struct ast_type_class_member *mbr;
		mbr = &node->type_class.members[i];

		num_member_params += mbr->type.num_params;
	}

	ast_slot_id member_param_slots[num_member_params];

	size_t mbr_param_i = 0;
	for (size_t i = 0; i < num_members; i++) {
		struct ast_type_class_member *mbr;
		mbr = &node->type_class.members[i];
		members[i].name = mbr->name;
		members[i].type = ast_node_deep_copy(
				&mod->mem, mbr->type.node);

		ast_node_constraints(
				ctx, mod, &env,
				body_deps, num_body_deps,
				members[i].type);

		members[i].num_params = mbr->type.num_params;
		members[i].params = arena_allocn(&mod->mem,
				members[i].num_params, sizeof(struct stg_type_class_member_param));

		for (size_t param_i = 0; param_i < members[i].num_params; param_i++) {
			members[i].params[param_i].name = mbr->type.params[param_i].name;
			members[i].params[param_i].loc = mbr->type.params[param_i].loc;

			if (mbr->type.params[param_i].type) {
				member_param_slots[mbr_param_i] =
					ast_node_constraints(
							ctx, mod, &env,
							body_deps, num_body_deps,
							mbr->type.params[param_i].type);
			} else {
				member_param_slots[mbr_param_i] =
					ast_slot_alloc(&env);
			}
			mbr_param_i += 1;
		}
	}
	assert(mbr_param_i == num_member_params);

#if AST_DEBUG_SLOT_SOLVE
	printf("Preliminary type solve for type class\n");
	printf("params:\n");
	for (size_t i = 0; i < num_params; i++) {
		printf(" - %.*s: %i\n",
				ALIT(params[i].name),
				body_deps[num_closure_members+i].value);
		if (node->type_class.pattern.params[i].type) {
			printf("     ");
			ast_print_node(ctx,
					node->type_class.pattern.params[i].type, true);
			printf("\n");
		}
	}

	printf("members:\n");
	for (size_t i = 0; i < num_members; i++) {
		printf(" - %.*s: ", ALIT(node->type_class.members[i].name));
		ast_print_node(ctx,
				node->type_class.members[i].type.node, true);
		printf("\n");
	}
	printf("\n");
#endif

	struct ast_slot_result result[env.num_alloced_slots];

	int err;
	err = ast_slot_try_solve(
			ctx, mod, &env, result);

	bool all_param_types_ok = true;

	for (size_t i = 0; i < num_params; i++) {
		struct ast_slot_result *res;
		res = &result[param_slots[i]];
		if (ast_slot_value_result(res->result) < AST_SLOT_RES_TYPE_FOUND) {
			stg_error(ctx->err, node->type_class.pattern.params[i].loc,
					"Failed to resolve the type of the parameter '%.*s'. <%zu:%i>",
					ALIT(node->type_class.pattern.params[i].name),
					env.invoc_id, param_slots[i]);
			all_param_types_ok = false;
			continue;
		}
		params[i].type = res->type;
	}

	mbr_param_i = 0;
	for (size_t i = 0; i < num_members; i++) {
		struct ast_type_class_member *mbr;
		mbr = &node->type_class.members[i];

		for (size_t param_i = 0; param_i < members[i].num_params; param_i++) {
			struct ast_slot_result *res;
			res = &result[member_param_slots[mbr_param_i]];
			mbr_param_i += 1;

			if (ast_slot_value_result(res->result) < AST_SLOT_RES_TYPE_FOUND) {
				// stg_error(ctx->err, members[i].params[param_i].loc,
				// 		"Failed to resolve the type of the parameter '%.*s' of member '%.*s'.",
				// 		ALIT(members[i].params[param_i].name),
				// 		ALIT(members[i].name));
				// all_param_types_ok = false;
				//
				// TODO: Handle cons of unknown type.
				members[i].params[param_i].type =
					ctx->vm->default_types.type;
				continue;
			}

			members[i].params[param_i].type = res->type;
		}
	}
	assert(mbr_param_i == num_member_params);

	if (!all_param_types_ok) {
		return NULL;
	}

	struct stg_type_class *tc;
	tc = stg_type_class_create(
			mod, params, num_params,
			members, num_members);

	if (!tc) {
		node->type_class.failed = true;
		return NULL;
	}

	node->type_class.cons = tc->cons;

	return tc;
}

int
stg_type_class_impl(struct stg_module *mod, struct stg_type_class *tc,
		struct object *args, size_t num_args,
		struct object value)
{
	if (num_args != tc->num_params) {
		return -1;
	}

	struct stg_type_class_impl *impl;
	impl = stg_type_class_find_impl(
			mod->vm, tc, args);
	if (impl) {
		if (impl->num_params > 0) {
			// TODO: Allow parametric impls to be overridden by more specific
			// impls.
			return -3;
		} else if (!obj_equals(mod->vm, impl->constant_inst, value)) {
			// TODO: Report the conflicting implementations.
			return -2;
		} else {
			return 1;
		}
	}

	struct stg_type_class_impl new_impl = {0};
	new_impl.args = arena_allocn(&mod->mem,
			num_args, sizeof(struct stg_type_class_impl_arg));
	for (size_t i = 0; i < num_args; i++) {
		new_impl.args[i].constant = true;
		new_impl.args[i].constant_value =
			stg_register_object(mod, args[i]);
	}

	new_impl.constant_inst = value;

	dlist_append(
			tc->impls,
			tc->num_impls,
			&new_impl);

	return 0;
}

int
stg_type_class_templ_impl(struct stg_module *mod, struct stg_type_class *tc,
		struct stg_type_class_impl_arg *args, size_t num_args,
		struct stg_type_class_impl_param *impl_params, size_t num_impl_params,
		struct ast_node *expr)
{
	panic("TODO: Parametric impls");
	return -1;
}

struct stg_type_class *
stg_cons_to_type_class(struct object_cons *cons)
{
	if (cons->base == &stg_tc_cons_base) {
		return (struct stg_type_class *)cons->data;
	} else {
		return NULL;
	}
}

void
stg_type_class_print(struct vm *vm, struct stg_type_class *tc)
{
	printf("type class:\n  parameters:\n");
	for (size_t i = 0; i < tc->num_params; i++) {
		printf("  - %.*s: ", ALIT(tc->params[i].name));
		print_type_id_repr(vm, tc->params[i].type);
		printf("\n");
	}

	struct ast_context ctx = {0};
	ast_init_context(&ctx, NULL, vm);

	printf("  members:\n");
	for (size_t i = 0; i < tc->num_members; i++) {
		struct stg_type_class_member *mbr;
		mbr = &tc->members[i];
		printf("  - %.*s: ", ALIT(mbr->name));
		ast_print_node(&ctx, mbr->type, false);
		printf("\n");

		if (mbr->num_params > 0) {
			printf("    parameters:\n");
			for (size_t param_i = 0; param_i < mbr->num_params; param_i++) {
				printf("    - %.*s: ", ALIT(mbr->params[param_i].name));
				print_type_id_repr(vm, mbr->params[param_i].type);
				printf("\n");
			}
		}
	}

	printf("  impls:\n");
	for (size_t impl_i = 0; impl_i < tc->num_impls; impl_i++) {
		struct stg_type_class_impl *impl;
		impl = &tc->impls[impl_i];

		printf("  - impl %zu:\n", impl_i);
		if (tc->num_params > 0) {
			printf("    parameters:\n");
			for (size_t param_i = 0; param_i < impl->num_params; param_i++) {
				printf("    - %.*s: ", ALIT(impl->params[param_i].name));
				print_type_id_repr(vm, impl->params[param_i].type);
				printf("\n");
			}
		}
		printf("    arguments:\n");
		for (size_t arg_i = 0; arg_i < tc->num_params; arg_i++) {
			printf("      %.*s = ", ALIT(tc->params[arg_i].name));
			if (impl->args[arg_i].constant) {
				print_obj_repr(vm, impl->args[arg_i].constant_value);
			} else {
				ast_print_node(&ctx, impl->args[arg_i].node, false);
			}
			printf("\n");
		}
	}
	if (tc->num_impls == 0) {
		printf("    (none)\n");
	}

	ast_destroy_context(&ctx);
}
