#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "module.h"
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
	node->func.param_types_slot = AST_BIND_NEW;

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
	node->func.native.func = NULL;
	node->func.return_type = return_type;

	for (size_t i = 0; i < num_params; i++) {
		node->func.params[i].name = param_names[i];
		node->func.params[i].slot = AST_BIND_NEW;
		node->func.params[i].type = param_types[i];
	}

	node->func.type = AST_BIND_NEW;
	node->func.return_type_slot = AST_BIND_NEW;
	node->func.param_types_slot = AST_BIND_NEW;

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
		struct stg_location loc)
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

enum ast_node_dependencies_state
ast_node_dependencies_fulfilled(struct ast_context *ctx,
		struct ast_env *env, struct ast_node *node)
{
	enum ast_node_dependencies_state result = AST_NODE_DEPS_OK;
	switch (node->kind) {
		case AST_NODE_FUNC:
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->func.body);
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->func.return_type);
			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->func.params[i].type);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->func.return_type);
			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->func.params[i].type);
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->call.args[i].value);
			}
			break;

		case AST_NODE_TEMPL:
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->templ.body);
			break;

		case AST_NODE_SLOT:
			break;

		case AST_NODE_ACCESS:
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->access.target);
			break;

		case AST_NODE_LIT:
			break;


		case AST_NODE_LOOKUP:
			if (node->lookup.ref.kind == AST_NAME_REF_NOT_FOUND) {
				result = AST_NODE_DEPS_NOT_OK;
				/*
			} else {
				struct ast_env_slot slot =
					ast_env_slot(ctx, env,
							ast_node_resolve_slot(env, &node->lookup.value));

				if (slot.kind == AST_SLOT_CONST ||
						slot.kind == AST_SLOT_CONST_TYPE ||
						slot.kind == AST_SLOT_PARAM ||
						slot.kind == AST_SLOT_TEMPL ||
						slot.kind == AST_SLOT_MEMBER) {
					node->kind = AST_NODE_SLOT;
					node->slot =
						ast_union_slot(ctx, env,
								node->lookup.value, node->lookup.slot);
				} else {
					result = AST_NODE_DEPS_NOT_READY;
				}
				*/
			}
			break;

		case AST_NODE_COMPOSITE:
			for (size_t i = 0; i < node->composite.num_members; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->composite.members[i].type);
			}

			for (size_t i = 0; i < node->composite.num_binds; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->composite.binds[i].target);
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->composite.binds[i].value);
			}

			for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->composite.free_exprs[i]);
			}
			break;

		case AST_NODE_VARIANT:
			for (size_t i = 0; i < node->composite.num_members; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->variant.variants[i].type);
			}
			break;
	}

	return result;
}

static bool
ast_slot_is_resolved(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id slot_id)
{
	struct ast_env_slot slot;
	slot = ast_env_slot(ctx, env, slot_id);

	bool result = true;

	switch (slot.kind) {
	case AST_SLOT_ERROR:
		return false;

	case AST_SLOT_WILDCARD:
		return false;

	case AST_SLOT_CONST_TYPE:
		assert(slot.type == AST_SLOT_TYPE);
		return true;

	case AST_SLOT_CONST:
		assert(ast_slot_is_resolved(ctx, env, slot.type));
		return true;

	case AST_SLOT_PARAM:
		return ast_slot_is_resolved(ctx, env, slot.type);

	case AST_SLOT_TEMPL:
		return ast_slot_is_resolved(ctx, env, slot.type);

	case AST_SLOT_MEMBER:
		return ast_slot_is_resolved(ctx, env, slot.type);

	case AST_SLOT_CLOSURE:
		return ast_slot_is_resolved(ctx, env, slot.type);

	case AST_SLOT_CONS:
		result &= !!slot.cons.def;
		result &= ast_slot_is_resolved(ctx, env, slot.type);

		for (size_t i = 0; i < slot.cons.num_present_args; i++) {
			result &= ast_slot_is_resolved(ctx, env, slot.cons.args[i].slot);
		}

		return result;

	case AST_SLOT_CONS_ARRAY:
		result &= ast_slot_is_resolved(ctx, env, slot.cons_array.member_type);
		result &= ast_slot_is_resolved(ctx, env, slot.cons_array.member_count);
		result &= ast_slot_is_resolved(ctx, env, slot.type);

		for (size_t i = 0; i < slot.cons_array.num_members; i++) {
			result &= ast_slot_is_resolved(ctx, env, slot.cons_array.members[i]);
		}

		return result;

	case AST_SLOT_SUBST:
		return ast_slot_is_resolved(ctx, env, slot.subst);
	}

	panic("Invalid slot in ast_slot_is_resolved");
	return false;
}

bool
ast_node_is_typed(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node)
{
	bool result = true;

	switch (node->kind) {
		case AST_NODE_FUNC:
			result &= ast_node_is_typed(ctx, env,
					node->func.body);
			result &= ast_node_is_typed(ctx, env,
					node->func.return_type);
			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->func.params[i].type);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			result &= ast_node_is_typed(ctx, env,
					node->func.return_type);
			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->func.params[i].type);
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			result &= ast_node_is_typed(ctx, env,
					node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->call.args[i].value);
			}

			result &= ast_slot_is_resolved(ctx, env, node->call.ret_type);

			if (node->kind == AST_NODE_CONS) {
				if (!ast_slot_is_resolved(ctx, env, node->call.cons)) {
					stg_error(ctx->err, node->loc,
							"Failed to resolve constructor.");
					result = false;
				}
			}
			break;

		case AST_NODE_TEMPL:
			result &= ast_node_is_typed(ctx, env,
					node->templ.body);

			for (size_t i = 0; i < node->templ.num_params; i++) {
				bool res;
				res = ast_slot_is_resolved(ctx, env,
						node->templ.params[i].slot);
				if (!result) {
					stg_error(ctx->err, node->templ.params[i].loc,
							"Could not resolve the type of template parameter '%.*s'.",
							ALIT(node->templ.params[i].name));
				}

				result &= res;
			}
			break;

		case AST_NODE_ACCESS:
			result &= ast_node_is_typed(ctx, env,
					node->access.target);
			{
				ast_slot_id target_value;
				target_value =
					ast_node_value(ctx, env, node->access.target);

				struct ast_env_slot slot = ast_env_slot(ctx, env, target_value);

				if (slot.kind != AST_SLOT_CONS) {
					stg_error(ctx->err, node->loc,
							"Object does not have any members.");
				} else if (!slot.cons.def) {
					printf("Access target is missing a def.\n");
					result = false;
				} else {
					ast_slot_id val;
					val = ast_unpack_arg_named(ctx, env,
							target_value, AST_BIND_NEW, node->access.name);

					if (val == AST_BIND_FAILED) {
						result = false;
					} else {
						result &= ast_slot_is_resolved(ctx, env, val);
					}
				}
			}
			break;

		case AST_NODE_SLOT:
			result &= ast_slot_is_resolved(ctx, env, node->slot);
			if (!result) {
				stg_error(ctx->err, node->loc,
						"Failed to resolve expression.");
			}
			break;

		case AST_NODE_LIT:
			break;

		case AST_NODE_COMPOSITE:
			for (size_t i = 0; i < node->composite.num_members; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->composite.members[i].type);
			}

			for (size_t i = 0; i < node->composite.num_binds; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->composite.binds[i].target);
				result &= ast_node_is_typed(ctx, env,
						node->composite.binds[i].value);
			}

			for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->composite.free_exprs[i]);
			}
			break;

		case AST_NODE_VARIANT:
			for (size_t i = 0; i < node->composite.num_members; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->variant.variants[i].type);
			}
			break;

		case AST_NODE_LOOKUP:
			result = false;
			printf("Lookup node still found during is typed check.\n");
			break;
	}

	return result;
}

// Appends the reference to the provided list if the reference is found. If the
// reference is not found the function returns 1, otherwise it returns 0.
static int
ast_node_find_named_dependencies_add(
		struct ast_name_ref ref, enum ast_name_dep_requirement req,
		struct ast_name_dep **out_refs, size_t *out_num_refs)
{
	if (ref.kind != AST_NAME_REF_NOT_FOUND) {
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

		default:
			break;
	}

#define VISIT_NODE(node) \
	err += ast_node_find_named_dependencies(\
			(node), req, out_refs, out_num_refs);
	AST_NODE_VISIT(node, false, false);
#undef VISIT_NODE

	return err;
}
