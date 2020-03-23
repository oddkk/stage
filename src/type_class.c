#include "type_class.h"
#include "vm.h"
#include "ast.h"
#include "arena.h"
#include <string.h>

static struct object_cons_base stg_tc_cons_base = {
	.name = STR("class"),
};

static struct stg_type_class_impl *
stg_type_class_find_impl(
		struct vm *vm, struct stg_type_class *tc,
		struct object *params)
{
	for (size_t impl_i = 0; impl_i < tc->num_impls; impl_i++) {
		struct stg_type_class_impl *impl;
		impl = &tc->impls[impl_i];
		bool match = true;
		for (size_t param_i = 0; param_i < tc->num_params; param_i++) {
			if (!obj_equals(vm, params[param_i], impl->params[param_i])) {
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

static int
stg_tc_pack(struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, void **params, size_t num_params)
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
		struct arena *str_mem = &ctx->vm->transient;
		struct string param_values_desc = {0};
		for (size_t i = 0; i < tc->num_params; i++) {
			arena_string_append_sprintf(
					str_mem, &param_values_desc, "\n%.*s = ",
					ALIT(tc->params[i].name));
			arena_string_append_obj_repr(
					&param_values_desc, ctx->vm, str_mem, &param_vals[i]);
		}

		// TODO: Location
		stg_error(ctx->err, STG_NO_LOC,
				"No implementation of this type class for:%.*s",
				LIT(param_values_desc));
		return -1;
	}

	struct type *impl_type;
	impl_type = vm_get_type(ctx->vm, impl->inst.type);

	memcpy(out, impl->inst.data, impl_type->size);

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
		return TYPE_UNSET;
	}

	return impl->inst.type;
}

static int
stg_tc_unpack(struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, struct object obj, int param_id)
{
	struct stg_type_class *tc = data;
	assert(param_id < tc->num_params);

	for (size_t impl_i = 0; impl_i < tc->num_impls; impl_i++) {
		struct stg_type_class_impl *impl;
		impl = &tc->impls[impl_i];

		if (obj_equals(ctx->vm, impl->inst, obj)) {
			struct type *param_type;
			param_type = vm_get_type(ctx->vm, tc->params[param_id].type);
			memcpy(out, impl->params[param_id].data, param_type->size);
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
					ctx, mod, &env, deps, num_deps,
					param->type);
		}
	}

	size_t num_members = node->type_class.num_members;
	struct stg_type_class_member members[num_members];
	memset(members, 0, num_members * sizeof(struct stg_type_class_member));
	for (size_t i = 0; i < num_members; i++) {
		struct ast_type_class_member *mbr;
		mbr = &node->type_class.members[i];
		members[i].name = mbr->name;
		members[i].type = ast_node_deep_copy(
				&mod->mem, mbr->type);

		ast_node_constraints(
				ctx, mod, &env,
				body_deps, num_body_deps,
				mbr->type);
	}

#if AST_DEBUG_SLOT_SOLVE
	printf("Preliminary type solve for type class\n");
	for (size_t i = 0; i < num_params; i++) {
		printf(" - param %.*s: %i:%i\n",
				ALIT(info->templ_node->templ.pattern.params[i].name),
				body_deps[info->num_deps+i].value, param_type_slots[i]);
		if (info->templ_node->templ.pattern.params[i].type) {
			printf("     ");
			ast_print_node(ctx,
					info->templ_node->templ.pattern.params[i].type, true);
		}
	}
	ast_print_node(ctx,
			info->templ_node->templ.pattern.node, true);
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
			stg_error(ctx->err, node->type_class.pattern.params[i].type->loc,
					"Failed to resolve the type of this parameter.");
			all_param_types_ok = false;
			continue;
		}
		params[i].type = res->type;
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
		struct object *params, size_t num_params,
		struct object value)
{
	if (num_params != tc->num_params) {
		return -1;
	}

	struct stg_type_class_impl *impl;
	impl = stg_type_class_find_impl(
			mod->vm, tc, params);
	if (impl) {
		if (!obj_equals(mod->vm, impl->inst, value)) {
			// TODO: Report the conflicting implementations.
			return -2;
		} else {
			return 1;
		}
	}

	struct stg_type_class_impl new_impl = {0};
	new_impl.params = arena_allocn(&mod->mem,
			num_params, sizeof(struct object));
	memcpy(new_impl.params, params,
			num_params * sizeof(struct object));

	new_impl.inst = value;

	return 0;
}
