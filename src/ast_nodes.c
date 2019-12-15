#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "module.h"
#include "bytecode.h"
#include "native_bytecode.h"
#include "base/mod.h"
#include "dlist.h"

char *
ast_node_name(enum ast_node_kind kind)
{
	switch (kind) {
		case AST_NODE_FUNC:			return "FUNC";
		case AST_NODE_FUNC_NATIVE:	return "FUNC_NATIVE";
		case AST_NODE_CALL:			return "CALL";
		case AST_NODE_CONS:			return "CONS";
		case AST_NODE_FUNC_TYPE:	return "FUNC_TYPE";
		case AST_NODE_ACCESS:		return "ACCESS";
		case AST_NODE_TEMPL:		return "TEMPL";
		case AST_NODE_SLOT:			return "SLOT";
		case AST_NODE_LIT:			return "LIT";
		case AST_NODE_LOOKUP:		return "LOOKUP";

		case AST_NODE_COMPOSITE:	return "COMPOSITE";
		case AST_NODE_VARIANT:		return "VARIANT";
	}

	return "(invalid)";
}

ast_slot_id
ast_node_resolve_slot(struct ast_env *env, ast_slot_id *slot)
{
	assert(*slot < (ast_slot_id)env->num_slots);
	while (*slot >= 0 && env->slots[*slot].kind == AST_SLOT_SUBST) {
		assert(*slot < (ast_slot_id)env->num_slots);
		*slot = env->slots[*slot].subst;
	}

	return *slot;
}

struct ast_node *
ast_init_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct atom **param_names, struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct ast_node *body)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_FUNC;
	node->loc = loc;

	node->func.params = calloc(sizeof(struct ast_func_param), num_params);
	node->func.num_params = num_params;
	node->func.body = body;
	node->func.return_type = return_type;

	for (size_t i = 0; i < num_params; i++) {
		node->func.params[i].name = param_names[i];
		node->func.params[i].slot = AST_BIND_NEW;
		node->func.params[i].type = param_types[i];
	}

	node->func.type = AST_BIND_NEW;
	node->func.return_type_slot = AST_BIND_NEW;

	return node;
}

struct ast_node *
ast_init_node_func_native(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct atom **param_names, struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct string native_func_name)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_FUNC_NATIVE;
	node->loc = loc;

	node->func.params = calloc(sizeof(struct ast_func_param), num_params);
	node->func.num_params = num_params;
	node->func.native.name = native_func_name;
	node->func.return_type = return_type;

	for (size_t i = 0; i < num_params; i++) {
		node->func.params[i].name = param_names[i];
		node->func.params[i].slot = AST_BIND_NEW;
		node->func.params[i].type = param_types[i];
	}

	node->func.type = AST_BIND_NEW;
	node->func.return_type_slot = AST_BIND_NEW;

	return node;
}

struct ast_node *
ast_init_node_templ(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct ast_node *body)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_TEMPL;
	node->loc = loc;

	node->templ.body = body;

	return node;
}

struct ast_node *
ast_init_node_call(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(
		node &&
		(args || num_args == 0)
	);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_CALL;
	node->loc = loc;

	node->call.func = func;
	node->call.args = calloc(sizeof(struct ast_func_arg), num_args);
	memcpy(node->call.args, args, sizeof(struct ast_func_arg) * num_args);
	node->call.num_args = num_args;
	node->call.cons = AST_SLOT_NOT_FOUND;

	node->call.ret_type = AST_BIND_NEW;

	return node;
}

struct ast_node *
ast_init_node_cons(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(
		node &&
		(args || num_args == 0)
	);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_CONS;
	node->loc = loc;

	node->call.func = func;
	node->call.args = calloc(sizeof(struct ast_func_arg), num_args);
	memcpy(node->call.args, args, sizeof(struct ast_func_arg) * num_args);
	node->call.num_args = num_args;
	node->call.cons = AST_SLOT_NOT_FOUND;

	node->call.ret_type = AST_BIND_NEW;

	return node;
}

struct ast_node *
ast_init_node_func_type(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct ast_node **param_types, size_t num_params,
		struct ast_node *ret_type)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(
		node && ret_type &&
		(param_types || num_params == 0)
	);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_FUNC_TYPE;
	node->loc = loc;

	node->func_type.param_types = calloc(sizeof(struct ast_node *), num_params);
	memcpy(node->func_type.param_types, param_types, sizeof(struct ast_node *) * num_params);
	node->func_type.num_params = num_params;

	node->func_type.ret_type = ret_type;

	node->func_type.slot = AST_BIND_NEW;

	return node;
}

struct ast_node *
ast_init_node_slot(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		ast_slot_id slot)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_SLOT;
	node->loc = loc;

	node->slot = slot;

	return node;
}

struct ast_node *
ast_init_node_access(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct ast_node *target, struct atom *name)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_ACCESS;
	node->loc = loc;

	node->access.target = target;
	node->access.name = name;
	node->access.slot = AST_BIND_NEW;

	return node;
}


struct ast_node *
ast_init_node_lit(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct object lit)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_LIT;
	node->loc = loc;

	node->lit.obj = lit;
	node->lit.slot = AST_BIND_NEW;

	return node;
}

struct ast_node *
ast_init_node_lookup(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct atom *name)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_LOOKUP;
	node->loc = loc;

	node->lookup.name = name;
	node->lookup.slot = AST_BIND_NEW;
	node->lookup.ref.kind = AST_NAME_REF_NOT_FOUND;

	return node;
}

void
ast_node_templ_register_param(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *templ, struct atom *name,
		struct ast_node *type, struct stg_location loc)
{
	if (!templ) {
		stg_error(ctx->err, loc,
				"Template parameters can only be declared inside functions.");
		return;
	}
	assert(templ->kind == AST_NODE_TEMPL);
	for (size_t i = 0; i < templ->templ.num_params; i++) {
		if (templ->templ.params[i].name == name) {
			stg_error(ctx->err, loc,
					"Template parameter '%.*s' has already been declared.",
					ALIT(name));
			stg_appendage(ctx->err, templ->templ.params[i].loc, "Here.");
			return;
		}
	}

	struct ast_template_param tmpl_param = {0};

	tmpl_param.name = name;
	tmpl_param.loc = loc;
	tmpl_param.slot = AST_BIND_NEW;
	tmpl_param.type = type;

	dlist_append(
			templ->templ.params,
			templ->templ.num_params,
			&tmpl_param);
}

struct ast_node *
ast_init_node_composite(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location loc)
{
	if (target == AST_NODE_NEW) {
		target = calloc(sizeof(struct ast_node), 1);
	}

	memset(target, 0, sizeof(struct ast_node));
	target->kind = AST_NODE_COMPOSITE;
	target->loc = loc;

	target->composite.cons = AST_BIND_NEW;

	target->composite.ret_value = AST_BIND_NEW;

	return target;
}

int
ast_node_composite_add_member(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct atom *name,
		struct ast_node *type, int type_giving_bind)
{
	assert(target && name);
	assert(target->kind == AST_NODE_COMPOSITE);
	assert(type_giving_bind < (ssize_t)target->composite.num_binds);

	for (size_t i = 0; i < target->composite.num_members; i++) {
		if (target->composite.members[i].name == name) {
			return -1;
		}
	}

	struct ast_datatype_member new_member = {0};
	new_member.name = name;
	new_member.loc = target->loc;

	if (type) {
		new_member.type = type;
		new_member.type_giving_bind = AST_NO_TYPE_GIVING_BIND;
	} else {
		new_member.type_giving_bind = type_giving_bind;
	}

	assert(new_member.type || new_member.type_giving_bind >= 0);

	new_member.slot = AST_BIND_NEW;

	dlist_append(
			target->composite.members,
			target->composite.num_members,
			&new_member);

	return 0;
}

ast_slot_id
ast_node_composite_get_member(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct atom *name)
{
	return ast_unpack_arg_named(ctx, env,
			target->composite.cons, AST_BIND_NEW, name);
}

int
ast_node_composite_bind(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *composite, struct ast_node *target,
		struct ast_node *value, bool overridable)
{
	assert(composite && target && value);
	assert(composite->kind == AST_NODE_COMPOSITE);

	struct ast_datatype_bind new_bind = {0};

	new_bind.target = target;
	new_bind.value = value;
	new_bind.overridable = overridable;

	int bind_id;
	bind_id = dlist_append(
			composite->composite.binds,
			composite->composite.num_binds,
			&new_bind);

	return bind_id;
}

void
ast_node_composite_tag_bind_erroneous(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *composite, int bind_id)
{
	assert(bind_id < composite->composite.num_binds);
	composite->composite.binds[bind_id].erroneous = true;
}

void
ast_node_composite_add_free_expr(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct ast_node *expr)
{
	assert(target && expr);

	dlist_append(
			target->composite.free_exprs,
			target->composite.num_free_exprs,
			&expr);
}

struct ast_node *
ast_init_node_variant(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location loc)
{
	if (target == AST_NODE_NEW) {
		target = calloc(sizeof(struct ast_node), 1);
	}

	memset(target, 0, sizeof(struct ast_node));
	target->kind = AST_NODE_VARIANT;
	target->loc = loc;

	target->variant.ret_value = AST_BIND_NEW;
	target->variant.type = TYPE_UNSET;

	return target;
}

void
ast_node_variant_add_option(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *target, struct stg_location loc,
		struct atom *name, struct ast_node *data_type)
{
	assert(target->kind == AST_NODE_VARIANT);

	struct ast_datatype_variant option = {0};

	option.name = name;
	option.data_type = data_type;
	option.loc  = loc;

	dlist_append(
			target->variant.options,
			target->variant.num_options,
			&option);
}


ast_slot_id
ast_node_type(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_FUNC_NATIVE:
		case AST_NODE_FUNC:
			return ast_node_resolve_slot(env, &node->func.type);

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			return node->call.ret_type;

		case AST_NODE_FUNC_TYPE:
			return AST_SLOT_TYPE;

		case AST_NODE_TEMPL:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->templ.slot)).type;

		case AST_NODE_ACCESS:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->access.slot)).type;

		case AST_NODE_SLOT:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->slot)).type;

		case AST_NODE_LIT:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->lit.slot)).type;

		case AST_NODE_LOOKUP:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->lookup.slot)).type;

		case AST_NODE_COMPOSITE:
			return AST_SLOT_TYPE;

		case AST_NODE_VARIANT:
			return AST_SLOT_TYPE;
	}

	panic("Invalid ast node.");
	return AST_BIND_FAILED;
}

ast_slot_id
ast_node_value(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_SLOT:
			return ast_node_resolve_slot(env, &node->slot);

		case AST_NODE_LIT:
			return ast_node_resolve_slot(env, &node->lit.slot);

		case AST_NODE_LOOKUP:
			return ast_node_resolve_slot(env, &node->lookup.slot);

		case AST_NODE_ACCESS:
			return ast_node_resolve_slot(env, &node->access.slot);

		case AST_NODE_FUNC_TYPE:
			return ast_node_resolve_slot(env, &node->func_type.slot);

		case AST_NODE_FUNC_NATIVE:
		case AST_NODE_FUNC:
		case AST_NODE_CALL:
			panic("TODO: eval");
			break;

		case AST_NODE_CONS:
			return ast_node_resolve_slot(env, &node->call.cons);

		case AST_NODE_TEMPL:
			return ast_node_resolve_slot(env, &node->templ.slot);

		case AST_NODE_COMPOSITE:
			return ast_node_resolve_slot(env, &node->composite.ret_value);

		case AST_NODE_VARIANT:
			return ast_node_resolve_slot(env, &node->variant.ret_value);
	}

	panic("Invalid ast node.");
	return AST_BIND_FAILED;
}

// Appends the reference to the provided list if the reference is found. If the
// reference is not found the function returns 1, otherwise it returns 0.
static int
ast_node_find_named_dependencies_add(
		struct ast_name_ref ref, enum ast_name_dep_requirement req,
		struct ast_name_dep **out_refs, size_t *out_num_refs)
{
	// We do not want the template parameter dependencies to be passed
	// to the underlying datastructure, so we prune them here.
	if (ref.kind != AST_NAME_REF_NOT_FOUND &&
			ref.kind != AST_NAME_REF_TEMPL) {
		struct ast_name_dep dep = {0};

		dep.ref = ref;
		dep.req = req;

		if (out_refs) {

			// Check if the reference was already found.
			for (size_t i = 0; i < *out_num_refs; i++) {
				if (ast_name_ref_equals((*out_refs)->ref, ref)) {

					// Make sure to keep the strictest requirement, in this
					// case value.
					if (req == AST_NAME_DEP_REQUIRE_VALUE) {
						(*out_refs)->req = req;
					}
					return 0;
				}
			}

			dlist_append(
					*(out_refs),
					*(out_num_refs),
					&dep);
		}

		return 0;
	} else {
		return 1;
	}
}

// Attempts to append all references from a closure to the given list. Returns
// the number of references that are not found.
static int
ast_node_closure_find_named_dependencies(
		enum ast_name_dep_requirement req, struct ast_closure_target *closure,
		struct ast_name_dep **out_refs, size_t *out_num_refs)
{
	int err = 0;

	for (size_t i = 0; i < closure->num_members; i++) {
		enum ast_name_dep_requirement mbr_req = req;
		if (closure->members[i].require_const) {
			mbr_req = AST_NAME_DEP_REQUIRE_VALUE;
		}
		err += ast_node_find_named_dependencies_add(
				closure->members[i].ref, mbr_req, out_refs, out_num_refs);
	}

	return err;
}

// Attempts to append all references from a node and its descendants to the
// given list. Returns the number of references that are not found.
int
ast_node_find_named_dependencies(
		struct ast_node *node, enum ast_name_dep_requirement req,
		struct ast_name_dep **out_refs, size_t *out_num_refs)
{
	int err = 0;

	switch (node->kind) {
		case AST_NODE_LOOKUP:
			err += ast_node_find_named_dependencies_add(
					node->lookup.ref, req, out_refs, out_num_refs);
			break;

		case AST_NODE_FUNC:
			err += ast_node_closure_find_named_dependencies(
					req, &node->func.closure,
					out_refs, out_num_refs);

			// fallthrough

		case AST_NODE_FUNC_NATIVE:
			// We require the value in order to get all the type information
			// from the function's parameters and return type. The function
			// body will not be visited.
			req = AST_NAME_DEP_REQUIRE_VALUE;
			break;

		case AST_NODE_COMPOSITE:
			err += ast_node_closure_find_named_dependencies(
					req, &node->composite.closure,
					out_refs, out_num_refs);

			// We will not visit the members, binds or free expressions from
			// the composite.
			break;

		case AST_NODE_VARIANT:
			err += ast_node_closure_find_named_dependencies(
					req, &node->variant.closure,
					out_refs, out_num_refs);

			// We will not visit the options of the variant.
			break;

		case AST_NODE_TEMPL:
			err += ast_node_closure_find_named_dependencies(
					req, &node->templ.closure,
					out_refs, out_num_refs);

			// We will not visit the body of the template.
			break;

		default:
			break;
	}

#define VISIT_NODE(node) \
	err += ast_node_find_named_dependencies(\
			(node), req, out_refs, out_num_refs);
	AST_NODE_VISIT(node, false, false, false, false);
#undef VISIT_NODE

	return err;
}

struct ast_templ_cons_inst {
	struct object *params;
	struct object result;
};

struct ast_templ_cons_info {
	struct ast_node *templ_node;

	struct ast_typecheck_dep *deps;
	size_t num_deps;

	struct ast_templ_cons_inst *insts;
	size_t num_insts;
};

struct object
ast_templ_pack(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_object_def *def, ast_slot_id obj_slot)
{
	struct ast_templ_cons_info *info;
	info = def->data;

	size_t num_body_deps = info->num_deps + def->num_params;
	struct ast_typecheck_dep body_deps[num_body_deps];
	memcpy(body_deps, info->deps, info->num_deps * sizeof(struct ast_typecheck_dep));

	for (size_t i = 0; i < info->num_deps; i++) {
		body_deps[i].value = AST_BIND_FAILED;
		assert(!body_deps[i].lookup_failed);

		if (body_deps[i].determined) {
			switch (body_deps[i].req) {
				case AST_NAME_DEP_REQUIRE_TYPE:
					body_deps[i].value =
						ast_bind_require_ok(
								ast_try_bind_slot_const_type(
									ctx, env, AST_BIND_NEW,
									body_deps[i].type));
					break;

				case AST_NAME_DEP_REQUIRE_VALUE:
					body_deps[i].value =
						ast_bind_require_ok(
								ast_try_bind_slot_const(
									ctx, env, AST_BIND_NEW,
									body_deps[i].val));
					break;
			}
		} else {
			body_deps[i].value =
				ast_bind_slot_wildcard(
						ctx, env, AST_BIND_NEW,
						ast_bind_slot_wildcard(
							ctx, env, AST_BIND_NEW,
							AST_SLOT_TYPE));
		}
	}

	struct object *param_values;
	param_values = calloc(def->num_params, sizeof(struct object));

	bool pack_failed = false;
	for (size_t i = 0; i < def->num_params; i++) {
		ast_slot_id arg_slot;
		arg_slot = ast_unpack_arg_named(
				ctx, env, obj_slot, AST_BIND_NEW,
				def->params[i].name);

		struct ast_typecheck_dep *dep;
		dep = &body_deps[info->num_deps+i];

		int err;
		err = ast_slot_pack(
				ctx, mod, env,
				arg_slot, &param_values[def->params[i].param_id]);
		if (err) {
			printf("Failed to pack templ arg.\n");
			pack_failed = true;
			continue;
		}

		ast_bind_slot_const(
				ctx, env, arg_slot,
				param_values[def->params[i].param_id]);

		dep->req = AST_NAME_DEP_REQUIRE_VALUE;
		dep->ref.kind = AST_NAME_REF_TEMPL;
		dep->ref.templ = def->params[i].param_id;
		dep->determined = true;
		dep->lookup_failed = false;
		dep->value = arg_slot;
		dep->val = param_values[def->params[i].param_id];
	}

	if (pack_failed) {
		struct object res = {0};
		return res;
	}

	for (size_t inst_i = 0; inst_i < info->num_insts; inst_i++) {
		bool match = true;
		for (size_t i = 0; i < def->num_params; i++) {
			if (!obj_equals(ctx->vm, param_values[i],
						info->insts[inst_i].params[i])) {
				match = false;
				break;
			}
		}

		if (match) {
			free(param_values);
			return info->insts[inst_i].result;
		}
	}

	struct ast_node *templ_node;
	templ_node = ast_node_deep_copy(ctx, env,
			&def->env, info->templ_node);

	struct ast_node *body;
	body = templ_node->templ.body;

	size_t num_errors_pre = ctx->err->num_errors;
	int err;
	err = ast_node_typecheck(ctx, mod, env,
			body, body_deps, num_body_deps, TYPE_UNSET);
	if (err) {
		struct object res = {0};
		return res;
	}
	assert(num_errors_pre == ctx->err->num_errors);

	struct ast_gen_info gen_info = {0};

	gen_info.templ_objs = param_values;
	gen_info.num_templ_objs = def->num_params;

	struct ast_closure_target *closure;
	closure = &templ_node->templ.closure;

	struct ast_typecheck_closure closure_values[closure->num_members];
	memset(closure_values, 0,
			sizeof(struct ast_typecheck_closure) * closure->num_members);

	for (size_t i = 0; i < closure->num_members; i++) {
		struct ast_name_ref closure_ref = {0};
		closure_ref.kind = AST_NAME_REF_CLOSURE;
		closure_ref.closure = i;

		struct ast_typecheck_dep *dep;
		dep = ast_find_dep(body_deps, num_body_deps,
				closure_ref);
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

	gen_info.closures = closure_values;
	gen_info.num_closures = closure->num_members;

	gen_info.templ_values = param_values;
	gen_info.num_templ_values = def->num_params;

	struct bc_env bc_env = {0};
	bc_env.vm = ctx->vm;
	bc_env.store = ctx->vm->instr_store;

	struct ast_gen_bc_result bc;
	bc = ast_node_gen_bytecode(
			ctx, mod, env, &gen_info,
			&bc_env, body);
	if (bc.err) {
		struct object res = {0};
		return res;
	}

	bc.last->next = bc_gen_ret(&bc_env, bc.out_var);
	bc.last = bc.last->next;

	bc_env.entry_point = bc.first;
	struct nbc_func nbc_func = {0};
	nbc_compile_from_bc(&nbc_func, &bc_env);

	struct type *res_type;
	res_type = vm_get_type(ctx->vm, body->type);

	uint8_t buffer[res_type->size];

	struct object res = {0};
	res.type = body->type;
	res.data = buffer;

	struct stg_exec exec_ctx = {0};
	exec_ctx.heap = arena_push(&ctx->vm->memory);

	nbc_exec(ctx->vm, &exec_ctx, &nbc_func,
			NULL, 0, NULL, buffer);

	arena_pop(&ctx->vm->memory, exec_ctx.heap);

	res = register_object(ctx->vm, env->store, res);

	struct ast_templ_cons_inst inst = {0};

	inst.params = param_values;
	inst.result = res;

	dlist_append(
			info->insts,
			info->num_insts,
			&inst);

	// TODO: Free.

	return res;
}

struct object
ast_templ_unpack(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, int param_id, struct object obj)
{
	struct ast_templ_cons_info *info = def->data;

	struct ast_templ_cons_inst *inst = NULL;
	for (size_t i = 0; i < info->num_insts; i++) {
		if (obj_equals(ctx->vm, obj, info->insts[i].result)) {
			inst = &info->insts[i];
			break;
		}
	}
	assert(inst);

	assert(param_id < def->num_params);
	return inst->params[param_id];
}

static bool
ast_templ_can_unpack(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, struct object obj)
{
	struct ast_templ_cons_info *info = def->data;
	for (size_t i = 0; i < info->num_insts; i++) {
		if (obj_equals(ctx->vm, obj, info->insts[i].result)) {
			return true;
		}
	}
	return false;
}

struct ast_object_def *
ast_node_create_templ(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *templ_node,
		struct ast_typecheck_dep *deps, size_t num_deps)
{
	assert(templ_node->templ.body->kind == AST_NODE_FUNC ||
			templ_node->templ.body->kind == AST_NODE_FUNC_NATIVE ||
			templ_node->templ.body->kind == AST_NODE_COMPOSITE ||
			templ_node->templ.body->kind == AST_NODE_VARIANT);

	struct ast_object_def *def;
	def = ast_object_def_register(env->store);

	def->num_params = templ_node->templ.num_params;
	def->params = calloc(def->num_params,
			sizeof(struct ast_object_def_param));

	struct ast_templ_cons_info *info;
	info = calloc(1, sizeof(struct ast_templ_cons_info));

	info->num_deps = num_deps;
	info->deps = calloc(info->num_deps, sizeof(struct ast_typecheck_dep));
	memcpy(info->deps, deps, sizeof(struct ast_typecheck_dep) * info->num_deps);

	info->templ_node = ast_node_deep_copy(
			ctx, &def->env, env, templ_node);

	for (size_t i = 0; i < def->num_params; i++) {
		def->params[i].param_id = i;
		def->params[i].name = info->templ_node->templ.params[i].name;
		def->params[i].slot = info->templ_node->templ.params[i].slot;

		ast_slot_id param_type_slot;
		param_type_slot =
			ast_env_slot(ctx, &def->env, def->params[i].slot).type;

		int err;
		err = ast_slot_pack_type(
				ctx, mod, &def->env,
				param_type_slot,
				&def->params[i].type);
		if (err) {
			printf("Failed to pack templ param type.\n");
		}
	}

	def->ret_type =
		ast_node_type(ctx, &def->env,
				info->templ_node->templ.body);

	def->data   = info;
	def->pack   = ast_templ_pack;
	def->unpack = ast_templ_unpack;
	def->can_unpack = ast_templ_can_unpack;

	return def;
}
