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
		struct ast_node *type)
{
	assert(target && name);
	assert(target->kind == AST_NODE_COMPOSITE);

	for (size_t i = 0; i < target->composite.num_members; i++) {
		if (target->composite.members[i].name == name) {
			return -1;
		}
	}

	struct ast_datatype_member new_member = {0};
	new_member.name = name;
	new_member.type = type;
	new_member.loc = target->loc;

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

void
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

	dlist_append(
			composite->composite.binds,
			composite->composite.num_binds,
			&new_bind);
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

/*
static bool
is_slot_func_type(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id slot_id)
{
	struct ast_env_slot slot = ast_env_slot(ctx, env, slot_id);
int
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
*/

struct ast_templ_node_data {
	struct ast_object_def def;
	struct ast_node *node;
};

/*
bool
ast_node_resolve_slots(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node)
{
	bool result = true;

	switch (node->kind) {
		case AST_NODE_FUNC_NATIVE:
		case AST_NODE_FUNC:
			node->func.type =
				ast_bind_slot_cons(ctx, env,
						ast_node_resolve_slot(env, &node->func.type),
						NULL, ctx->cons.func);

			node->func.return_type_slot =
				ast_union_slot(ctx, env,
						ast_unpack_arg_named(ctx, env,
							node->func.type,
							AST_BIND_NEW,
							ctx->atoms.func_cons_arg_ret),
						node->func.return_type_slot);

			node->func.param_types_slot =
				ast_union_slot(ctx, env,
						ast_unpack_arg_named(ctx, env,
							node->func.type,
							AST_BIND_NEW,
							ctx->atoms.func_cons_arg_params),
						node->func.param_types_slot);

			result &= ast_node_resolve_slots(ctx, mod, env, node->func.return_type);

			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_resolve_slots(ctx, mod, env,
						node->func.params[i].type);
			}

			if (node->kind == AST_NODE_FUNC) {
				result &= ast_node_resolve_slots(ctx, mod, env, node->func.body);
			}
			break;

		case AST_NODE_CALL:
			result &= ast_node_resolve_slots(ctx, mod, env, node->call.func);

			for (size_t i = 0; i < node->call.num_args; i++) {
				result &= ast_node_resolve_slots(ctx, mod, env,
						node->call.args[i].value);
			}

			{
#define FUNC_TYPE_KIND_UNKN 0
#define FUNC_TYPE_KIND_FUNC 1
#define FUNC_TYPE_KIND_CONS 2
				int func_type_kind = FUNC_TYPE_KIND_UNKN;

				switch (node->call.func->kind) {
					case AST_NODE_FUNC:
					case AST_NODE_FUNC_NATIVE:
						func_type_kind = FUNC_TYPE_KIND_FUNC;
						break;

					case AST_NODE_TEMPL:
						func_type_kind = FUNC_TYPE_KIND_CONS;
						break;

					case AST_NODE_CALL:
					case AST_NODE_CONS:
					case AST_NODE_SLOT:
						{
							struct ast_env_slot func_type_slot;
							func_type_slot = ast_env_slot(ctx, env,
									ast_node_type(ctx, env, node->call.func));

							switch (func_type_slot.kind) {
								case AST_SLOT_CONS:
									if (func_type_slot.cons.def == ctx->cons.func) {
										func_type_kind = FUNC_TYPE_KIND_FUNC;
									}
									break;

								case AST_SLOT_CONST_TYPE:
									if (stg_type_is_func(ctx->vm, func_type_slot.const_type)) {
										func_type_kind = FUNC_TYPE_KIND_FUNC;
									} else if (func_type_slot.const_type == ctx->types.cons) {
										func_type_kind = FUNC_TYPE_KIND_CONS;
									}
									break;

								default:
									break;
							}
						}
						break;

					case AST_NODE_LOOKUP:
						panic("Got lookup as target for call.");
						break;

					default:
						break;
				}

				if (func_type_kind == FUNC_TYPE_KIND_CONS) {
					struct object func_obj;
					int err;

					err = ast_node_eval(ctx, mod, env,
							node->call.func, &func_obj);
					assert(!err);
					assert_type_equals(ctx->vm, ctx->types.cons, func_obj.type);

					struct ast_object_def *cons;
					cons = *(struct ast_object_def **)func_obj.data;

					if (is_slot_func_type(ctx, &cons->env, cons->ret_type)) {
						struct ast_node *cons_node;

						cons_node = calloc(1, sizeof(struct ast_node));
						cons_node->kind = AST_NODE_CONS;
						cons_node->loc = node->loc;

						cons_node->call.func = node->call.func;
						node->call.func = cons_node;

						cons_node->call.cons = AST_BIND_NEW;

						cons_node->call.ret_type =
							ast_bind_slot_wildcard(ctx, env, AST_BIND_NEW,
									NULL, AST_SLOT_TYPE);

						func_type_kind = FUNC_TYPE_KIND_FUNC;
					}
				}

				result &= ast_node_resolve_slots(ctx, mod, env, node->call.func);

				for (size_t i = 0; i < node->call.num_args; i++) {
					result &= ast_node_resolve_slots(ctx, mod, env,
							node->call.args[i].value);
				}

				if (func_type_kind != FUNC_TYPE_KIND_CONS) {
					// If the type is not a cons, we expect it to be a
					// function. Bind the func to the arguments appropriatly.
					ast_slot_id arg_type_ids[node->call.num_args];


					for (size_t i = 0; i < node->call.num_args; i++) {
						arg_type_ids[i] = ast_node_type(
								ctx, env, node->call.args[i].value);
					}

					ast_slot_id func_type_slot;
					func_type_slot = ast_node_type(
							ctx, env, node->call.func);

					func_type_slot = ast_bind_slot_cons(
							ctx, env, func_type_slot,
							NULL, ctx->cons.func);

					ast_slot_id ret_type;
					ret_type = ast_unpack_arg_named(
							ctx, env, func_type_slot,
							AST_BIND_NEW,
							ctx->atoms.func_cons_arg_ret);

					ret_type = ast_bind_slot_wildcard(ctx, env,
							ret_type, NULL, AST_SLOT_TYPE);

					node->call.ret_type =
						ast_union_slot(ctx, env,
								ast_node_resolve_slot(env, &node->call.ret_type),
								ret_type);


					ast_slot_id param_types = ast_unpack_arg_named(ctx, env,
							func_type_slot, AST_BIND_NEW,
							ctx->atoms.func_cons_arg_params);

					param_types = ast_bind_slot_cons_array(
							ctx, env, param_types, NULL,
							arg_type_ids, node->call.num_args,
							AST_SLOT_TYPE);

					result &= ast_node_resolve_slots(ctx, mod, env, node->call.func);

					for (size_t i = 0; i < node->call.num_args; i++) {
						result &= ast_node_resolve_slots(ctx, mod, env,
								node->call.args[i].value);
					}

					break;
				} else {
					node->kind = AST_NODE_CONS;
					node->call.cons = AST_BIND_NEW;
				}

#undef FUNC_TYPE_KIND_UNKN
#undef FUNC_TYPE_KIND_FUNC
#undef FUNC_TYPE_KIND_CONS
			}
			// fallthrough

		case AST_NODE_CONS:
			{
				assert(node->kind == AST_NODE_CONS &&
						node->call.cons != AST_SLOT_NOT_FOUND);

				struct ast_object_def *cons;

				struct object func_obj;
				int err;

				err = ast_node_eval(ctx, mod, env,
						node->call.func, &func_obj);
				assert_type_equals(ctx->vm, ctx->types.cons, func_obj.type);

				cons = *(struct ast_object_def **)func_obj.data;

				node->kind = AST_NODE_CONS;

				node->call.cons = ast_bind_slot_cons(ctx, env,
						node->call.cons, NULL, cons);

				node->call.ret_type = ast_union_slot(ctx, env,
						ast_env_slot(ctx, env, node->call.cons).type,
						node->call.ret_type);

				struct ast_env_slot cons_slot;
				cons_slot = ast_env_slot(ctx, env, node->call.cons);

				assert(cons_slot.kind == AST_SLOT_CONS);
				assert(cons_slot.cons.num_present_args == cons->num_params);

				if (node->call.num_args == 0) {
					node->call.num_args = cons->num_params;
					node->call.args = calloc(cons->num_params,
							sizeof(struct ast_func_arg));

					for (size_t i = 0; i < cons->num_params; i++) {
						struct atom *param_name;
						param_name = cons->params[i].name;

						ast_slot_id cons_arg_slot;
						cons_arg_slot = ast_unpack_arg_named(ctx, env,
								node->call.cons, AST_BIND_NEW, param_name);

						node->call.args[i].name = param_name;
						node->call.args[i].value =
							ast_init_node_slot(ctx, env, AST_NODE_NEW,
									STG_NO_LOC, cons_arg_slot);
					}
				} else {
					if (node->call.num_args != cons->num_params) {
						stg_error(ctx->err, node->loc,
								"Expected %zu argument%s to constructor, got %zu.",
								cons->num_params, (cons->num_params == 1) ? "" : "s",
								node->call.num_args);
						return false;
					}

					for (size_t i = 0; i < node->call.num_args; i++) {
						struct atom *param_name;
						param_name = cons->params[i].name;

						ast_slot_id cons_arg_slot;
						cons_arg_slot = ast_unpack_arg_named(ctx, env,
								node->call.cons, AST_BIND_NEW, param_name);

						ast_slot_id node_arg_slot;
						node_arg_slot = ast_node_value(ctx, env,
								node->call.args[i].value);

						ast_union_slot(ctx, env, cons_arg_slot, node_arg_slot);
					}
				}

			}

			result &= ast_node_resolve_slots(ctx, mod, env, node->call.func);

			for (size_t i = 0; i < node->call.num_args; i++) {
				result &= ast_node_resolve_slots(ctx, mod, env,
						node->call.args[i].value);
			}

			break;

		case AST_NODE_TEMPL:
			result &= ast_node_resolve_slots(ctx, mod, env,
					node->templ.body);
			break;

		case AST_NODE_ACCESS:
			{
				result &= ast_node_resolve_slots(ctx, mod, env,
						node->access.target);

				// ast_slot_id target_value;
				// target_value =
				// 	ast_node_value(ctx, env, node->access.target);

				// ast_slot_id val;
				// val = ast_unpack_arg_named(ctx, env,
				// 		target_value, AST_BIND_NEW, node->access.name);

				// ast_union_slot(ctx, env, val, node->access.slot);

				ast_slot_id target_value;
				target_value =
					ast_node_value(ctx, env, node->access.target);

				struct ast_env_slot slot = ast_env_slot(ctx, env, target_value);
				if (slot.kind == AST_SLOT_CONS && slot.cons.def) {
					ast_slot_id val;
					val = ast_unpack_arg_named(ctx, env,
							target_value, AST_BIND_NEW, node->access.name);
					assert(val != AST_BIND_FAILED);

					ast_union_slot(ctx, env, val, node->access.slot);
				}
			}
			break;

		case AST_NODE_SLOT:
			break;

		case AST_NODE_LIT:
			node->kind = AST_NODE_SLOT;
			node->slot = node->lit.slot;
			break;

		case AST_NODE_LOOKUP:
			if (node->lookup.value == AST_SLOT_NOT_FOUND) {
				printf("Name not found.\n");
				result = false;
			} else {
				struct ast_env_slot slot =
					ast_env_slot(ctx, env,
							ast_node_resolve_slot(env, &node->lookup.value));

				if (slot.kind == AST_SLOT_CONST ||
						slot.kind == AST_SLOT_CONST_TYPE ||
						slot.kind == AST_SLOT_MEMBER ||
						slot.kind == AST_SLOT_CLOSURE) {
					node->kind = AST_NODE_SLOT;
					node->slot =
						ast_union_slot(ctx, env,
								node->lookup.value, node->lookup.slot);
				} else {
					printf("Lookup not of valid kind.\n");
					result = false;
				}
			}
			break;

		case AST_NODE_COMPOSITE:
			for (size_t i = 0; i < node->composite.num_members; i++) {
				if (node->composite.members[i].type) {
					result &= ast_node_resolve_slots(ctx, mod, env,
							node->composite.members[i].type);
				}
			}

			for (size_t i = 0; i < node->composite.num_binds; i++) {
				result &= ast_node_resolve_slots(ctx, mod, env,
						node->composite.binds[i].target);
				result &= ast_node_resolve_slots(ctx, mod, env,
						node->composite.binds[i].value);
			}

			for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
				result &= ast_node_resolve_slots(ctx, mod, env,
						node->composite.free_exprs[i]);
			}
			break;


		case AST_NODE_VARIANT:
			for (size_t i = 0; i < node->variant.num_variants; i++) {
				result &= ast_node_resolve_slots(ctx, mod, env,
						node->variant.variants[i].type);
			}

			// TODO: Make the new type.
			break;
	}

	return result;
}
*/

int
ast_node_eval_type(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node, type_id *out)
{
	struct object type_obj;
	int err;
	err = ast_node_eval(ctx, mod, env, node, &type_obj);
	if (err) {
		return err;
	}

	assert_type_equals(ctx->vm, type_obj.type, ctx->types.type);

	*out = *(type_id *)type_obj.data;
	return 0;
}

int
ast_node_eval_type_of(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node, type_id *out)
{
	struct object type_obj;

	ast_slot_id type_slot;
	type_slot = ast_node_type(ctx, env, node);
	int err;
	err = ast_slot_pack(ctx, mod, env, type_slot, &type_obj);
	if (err) {
		return err;
	}

	assert_type_equals(ctx->vm, type_obj.type, ctx->types.type);

	*out = *(type_id *)type_obj.data;
	return 0;
}


static struct object
ast_templ_node_unpack(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, int param_id, struct object obj)
{
	panic("Attemped to unpack tmpl node.");
	return OBJ_NONE;
}

static struct object
ast_templ_node_pack(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_object_def *def, ast_slot_id obj)
{
	struct ast_env_slot slot;
	slot = ast_env_slot(ctx, env, obj);

	assert(slot.kind == AST_SLOT_CONS);

	struct ast_templ_node_data *data;
	data = (struct ast_templ_node_data *)def->data;

	struct ast_node *tmpl_node = data->node;
	assert(tmpl_node->kind == AST_NODE_TEMPL);

	struct ast_node *new_node;
	new_node = ast_node_deep_copy(ctx, env,
			&data->def.env, tmpl_node);

	for (size_t i = 0; i < new_node->templ.num_params; i++) {
		ast_slot_id arg_slot = new_node->templ.params[i].slot;
		ast_slot_id param_value_slot =
			ast_unpack_arg_named(ctx, env, obj,
					AST_BIND_NEW, new_node->templ.params[i].name);

		arg_slot = ast_copy_slot(ctx,
				env, arg_slot,
				env, param_value_slot);
	}

	struct ast_node *result;
	result = new_node->templ.body;

	free(new_node->templ.params);
	free(new_node);

	// ast_node_resolve_slots(ctx, mod, env, result);
	if (!ast_node_is_typed(ctx, env, result)) {
		printf("Failed to type expression. (templated)\n");
		return OBJ_NONE;
	}

	int err;
	struct object res;
	err = ast_node_eval(ctx, mod, env, result, &res);
	if (err) {
		printf("Failed to generate object. (templated)\n");
		return OBJ_NONE;
	}

	// TODO: Free the ast or keep it somewhere.

	return res;
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

int
ast_node_eval(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node, struct object *out)
{
	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
			{
				if (node->func.instance == FUNC_UNSET) {
					struct func func = {0};

					type_id ret_type;
					type_id param_types[node->func.num_params];
					int err;

					err = ast_node_eval_type(ctx, mod, env,
							node->func.return_type, &ret_type);
					if (err) {
						printf("Failed to resolve function return type.\n");
						return -1;
					}

					for (size_t i = 0; i < node->func.num_params; i++) {
						err = ast_node_eval_type(ctx, mod, env,
								node->func.return_type, &param_types[i]);
						if (err) {
							printf("Failed to resolve function parameter.\n");
							return -1;
						}
					}

					func.type = stg_register_func_type(mod->stg_mod, ret_type,
							param_types, node->func.num_params);

					if (node->kind == AST_NODE_FUNC) {
						struct bc_env *bc_env;

						bc_env = ast_func_gen_bytecode(
								ctx, mod, env, node);
						assert(bc_env);

						func.kind = FUNC_BYTECODE;
						func.bytecode = bc_env;
					} else if (node->kind == AST_NODE_FUNC_NATIVE) {
						func.kind = FUNC_NATIVE;
						func.name = vm_atom(ctx->vm, node->func.native.name);
						func.native = node->func.native.func;
					}

					node->func.instance =
						stg_register_func(mod->stg_mod, func);
				}

				struct object func_type_obj;
				int err;
				err = ast_slot_pack(ctx, mod, env, node->func.type, &func_type_obj);
				if (err) {
					printf("Falied to pack func type.\n");
					return -1;
				}

				assert_type_equals(ctx->vm, func_type_obj.type, ctx->types.type);
				type_id func_type = *(type_id *)func_type_obj.data;

				struct stg_func_object func_obj_data = {0};

				func_obj_data.func = node->func.instance;

				assert(vm_get_type(ctx->vm, func_type)->size == sizeof(struct stg_func_object));

				struct object func_obj = {0};
				func_obj.type = func_type;
				func_obj.data = &func_obj_data;

				*out = register_object(ctx->vm, env->store, func_obj);
				return 0;
			}
			break;

		case AST_NODE_CALL:
			{
				struct object args[node->call.num_args];
				struct object func = {0};

				// TODO: Prealloc the space for the argument values to avoid
				// pushing to the arena.

				memset(args, 0, sizeof(struct object) * node->call.num_args);

				int err;

				err = ast_node_eval(ctx, mod, env, node->call.func, &func);
				if (err) {
					return -1;
				}

				for (size_t i = 0; i < node->call.num_args; i++) {
					err = ast_node_eval(ctx, mod, env, node->call.args[i].value, &args[i]);
					if (err) {
						return -1;
					}
				}

				// struct type *type = vm_get_type(ctx->vm, func.type);
				// TODO: assert(type->base == func_type_base)

				struct stg_func_object *func_obj = (struct stg_func_object  *)func.data;
				struct func *func_inst = vm_get_func(ctx->vm, func_obj->func);
				struct type *func_type = vm_get_type(ctx->vm, func_inst->type);
				struct stg_func_type *func_info = (struct stg_func_type *)func_type->data;;
				struct type *ret_type  = vm_get_type(ctx->vm, func_info->return_type);

				struct object res = {0};
				uint8_t buffer[ret_type->size];
				res.type = func_info->return_type;
				res.data = buffer;

				err = vm_call_func(ctx->vm, func_obj->func,
						args, node->call.num_args, &res);
				if (err) {
					return err;
				}

				*out = register_object(ctx->vm, env->store, res);
				return err;
			}
			break;

		case AST_NODE_CONS:
			{
				struct object cons_obj;
				int err;
				err = ast_node_eval(ctx, mod, env, node->call.func, &cons_obj);
				if (err) {
					return -1;
				}

				assert_type_equals(ctx->vm, ctx->types.cons, cons_obj.type);

				struct ast_object_def *cons;
				cons = *(struct ast_object_def **)cons_obj.data;
				assert(cons);
				assert(cons->pack);

				*out = cons->pack(ctx, mod, env,
						cons, node->call.cons);

				return 0;
			}
			break;

		case AST_NODE_TEMPL:
			if (!node->templ.def) {
				struct ast_templ_node_data *data;
				data = calloc(1, sizeof(struct ast_templ_node_data));
				data->def.env.store = env->store;

				if (!ast_object_def_from_cons(ctx, env, &data->def, node->templ.cons)) {
					free(data);
					return -1;
				}

				data->node = ast_node_deep_copy(
						ctx, &data->def.env, env, node);

				/*
				ast_print(ctx, &data->def.env, data->node);
				ast_env_print(ctx->vm, &data->def.env);
				*/

				data->def.pack   = ast_templ_node_pack;
				data->def.unpack = ast_templ_node_unpack;
				data->def.data   = data;

				node->templ.def = &data->def;
			}

			{
				struct object res = {0};
				res.type = ctx->types.cons;
				res.data = &node->templ.def;

				*out = register_object(ctx->vm, env->store, res);
			}
			return 0;

		case AST_NODE_ACCESS:
			{
				struct ast_env_slot target_slot;
				target_slot = ast_env_slot(ctx, env,
						ast_node_value(ctx, env, node->access.target));
				assert(target_slot.kind == AST_SLOT_CONS);
				assert(target_slot.cons.def);

				struct object target_obj;
				int err;

				err = ast_node_eval(ctx, mod, env, node->access.target, &target_obj);
				if (err) {
					printf("Failed to eval access target.\n");
					return -1;
				}

				if (false /* TODO: target_slot.cons.def->unpack_func */) {
				} else {
					struct ast_object_def *def;
					def = target_slot.cons.def;

					for (size_t i = 0; i < def->num_params; i++) {
						if (def->params[i].name == node->access.name) {
							*out = def->unpack(ctx, env, def,
									def->params[i].param_id, target_obj);
							return 0;
						}
					}

					return -1;
				}
			}
			break;

		case AST_NODE_SLOT:
			return ast_slot_pack(ctx, mod, env, node->slot, out);

		case AST_NODE_LIT:
			*out = node->lit.obj;
			return 0;

		case AST_NODE_LOOKUP:
			if (node->lookup.ref.kind == AST_NAME_REF_NOT_FOUND) {
				printf("Lookup was not resolved.\n");
				return -1;
			}
			// return ast_slot_pack(ctx, mod, env, node->lookup.value, out);
			printf("TODO: Eval of lookup nodes.\n");
			return -1;

		case AST_NODE_COMPOSITE:
			{
				type_id type;
				type = ast_dt_finalize_composite(ctx, mod, env, node, NULL, 0);

				if (type == TYPE_UNSET) {
					printf("Failed to initialize composite type.");
					return -1;
				}

				struct object res = {0};
				res.type = ctx->types.type;
				res.data = &type;

				*out = register_object(ctx->vm, env->store, res);
			}
			return 0;

		case AST_NODE_VARIANT:
			printf("Variant not implemented.\n");
			return -1;
	}

	panic("Invalid node in ast_node_eval");
	return -1;
}
