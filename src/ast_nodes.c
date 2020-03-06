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
		case AST_NODE_INST:			return "INST";
		case AST_NODE_FUNC_TYPE:	return "FUNC_TYPE";
		case AST_NODE_ACCESS:		return "ACCESS";
		case AST_NODE_TEMPL:		return "TEMPL";
		case AST_NODE_LIT:			return "LIT";
		case AST_NODE_LIT_NATIVE:	return "LIT_NATIVE";
		case AST_NODE_LOOKUP:		return "LOOKUP";
		case AST_NODE_MOD:			return "MOD";

		case AST_NODE_COMPOSITE:	return "COMPOSITE";
		case AST_NODE_VARIANT:		return "VARIANT";
	}

	return "(invalid)";
}

struct ast_node *
ast_init_node_func(struct ast_context *ctx,
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
		node->func.params[i].type = param_types[i];
	}

	return node;
}

struct ast_node *
ast_init_node_func_native(struct ast_context *ctx,
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
		node->func.params[i].type = param_types[i];
	}

	return node;
}

struct ast_node *
ast_init_node_templ(
		struct ast_context *ctx,
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
		struct ast_context *ctx,
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
	node->call.cons = NULL;

	return node;
}

struct ast_node *
ast_init_node_cons(
		struct ast_context *ctx,
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
	node->call.cons = NULL;

	return node;
}

struct ast_node *
ast_init_node_inst(
		struct ast_context *ctx,
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
	node->kind = AST_NODE_INST;
	node->loc = loc;

	node->call.func = func;
	node->call.args = calloc(sizeof(struct ast_func_arg), num_args);
	memcpy(node->call.args, args, sizeof(struct ast_func_arg) * num_args);
	node->call.num_args = num_args;
	node->call.cons = NULL;

	return node;
}

struct ast_node *
ast_init_node_func_type(
		struct ast_context *ctx,
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

	return node;
}

struct ast_node *
ast_init_node_access(
		struct ast_context *ctx,
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

	return node;
}


struct ast_node *
ast_init_node_lit(
		struct ast_context *ctx,
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

	return node;
}

struct ast_node *
ast_init_node_lit_native(
		struct ast_context *ctx,
		struct ast_node *node, struct stg_location loc,
		struct atom *name)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_LIT_NATIVE;
	node->loc = loc;

	node->lit_native.name = name;

	return node;
}

struct ast_node *
ast_init_node_lookup(
		struct ast_context *ctx,
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
	node->lookup.ref.kind = AST_NAME_REF_NOT_FOUND;

	return node;
}

struct ast_node *
ast_init_node_mod(
		struct ast_context *ctx,
		struct ast_node *node, struct stg_location loc,
		struct atom *name)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_MOD;
	node->loc = loc;

	node->mod.name = name;

	return node;
}

void
ast_node_templ_register_param(
		struct ast_context *ctx,
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
	tmpl_param.type = type;

	dlist_append(
			templ->templ.params,
			templ->templ.num_params,
			&tmpl_param);
}

struct ast_node *
ast_init_node_composite(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location loc)
{
	if (target == AST_NODE_NEW) {
		target = calloc(sizeof(struct ast_node), 1);
	}

	memset(target, 0, sizeof(struct ast_node));
	target->kind = AST_NODE_COMPOSITE;
	target->loc = loc;

	return target;
}

int
ast_node_composite_add_member(
		struct ast_context *ctx,
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

	dlist_append(
			target->composite.members,
			target->composite.num_members,
			&new_member);

	return 0;
}

int
ast_node_composite_bind(
		struct ast_context *ctx,
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
		struct ast_context *ctx,
		struct ast_node *composite, int bind_id)
{
	assert(bind_id < composite->composite.num_binds);
	composite->composite.binds[bind_id].erroneous = true;
}

void
ast_node_composite_add_free_expr(
		struct ast_context *ctx,
		struct ast_node *target, struct ast_node *expr)
{
	assert(target && expr);

	dlist_append(
			target->composite.free_exprs,
			target->composite.num_free_exprs,
			&expr);
}

void
ast_node_composite_add_use(
		struct ast_context *ctx, struct stg_location loc,
		struct ast_node *target, struct ast_node *expr,
		struct atom *as_name)
{
	assert(target && expr);

	struct ast_datatype_use use;
	use.target = expr;
	use.loc = loc;
	use.as_name = as_name;

	dlist_append(
			target->composite.uses,
			target->composite.num_uses,
			&use);
}

struct ast_node *
ast_init_node_variant(
		struct ast_context *ctx,
		struct ast_node *target, struct stg_location loc)
{
	if (target == AST_NODE_NEW) {
		target = calloc(sizeof(struct ast_node), 1);
	}

	memset(target, 0, sizeof(struct ast_node));
	target->kind = AST_NODE_VARIANT;
	target->loc = loc;

	target->variant.type = TYPE_UNSET;

	return target;
}

void
ast_node_variant_add_option(
		struct ast_context *ctx,
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

		case AST_NODE_CONS:
			req = AST_NAME_DEP_REQUIRE_VALUE;
			break;

		case AST_NODE_INST:
			// We must know the value of the instantiation type before we can
			// resolve the names.
			err += ast_node_find_named_dependencies(
					node->call.func, AST_NAME_DEP_REQUIRE_VALUE,
					out_refs, out_num_refs);

			for (size_t i = 0; i < (node)->call.num_args; i++) {
				err += ast_node_find_named_dependencies(
						node->call.args[i].value, req, out_refs, out_num_refs);
			}
			return err;

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

struct ast_node *
ast_node_deep_copy(struct ast_node *src)
{
	struct ast_node *result;
	result = calloc(1, sizeof(struct ast_node));

#define DCP_NODE(name)                               \
	do {                                             \
	result->name =                                   \
		ast_node_deep_copy(src->name);               \
	} while (0);

#define DCP_LIT(name)                                \
	do { result->name = src->name; } while (0);

#define DCP_DLIST(array_name, count_name)            \
	do {                                             \
		DCP_LIT(count_name);                         \
		if ((result->count_name) > 0) {              \
			result->array_name = calloc(             \
					result->count_name,              \
					sizeof(*result->array_name));    \
		} else {                                     \
			result->array_name = NULL;               \
		}                                            \
	} while (0);

	DCP_LIT(kind);
	DCP_LIT(type);
	DCP_LIT(loc);

	switch (src->kind) {
	case AST_NODE_FUNC_NATIVE:
	case AST_NODE_FUNC:
		if (src->kind == AST_NODE_FUNC_NATIVE) {
			DCP_LIT(func.native);
		} else {
			DCP_NODE(func.body);
			DCP_LIT(func.closure);
		}
		DCP_DLIST(func.params, func.num_params);
		for (size_t i = 0; i < result->func.num_params; i++) {
			DCP_LIT(func.params[i].name);
			DCP_NODE(func.params[i].type);
		}

		DCP_NODE(func.return_type);
		DCP_LIT(func.instance);
		DCP_LIT(func.closure);
		break;

		break;

	case AST_NODE_CONS:
	case AST_NODE_INST:
		DCP_LIT(call.cons);
		// fallthrough

	case AST_NODE_CALL:
		DCP_DLIST(call.args, call.num_args);
		for (size_t i = 0; i < result->call.num_args; i++) {
			DCP_LIT(call.args[i].name);
			DCP_NODE(call.args[i].value);
		}

		DCP_NODE(call.func);
		break;

	case AST_NODE_FUNC_TYPE:
		DCP_DLIST(func_type.param_types, func_type.num_params);
		for (size_t i = 0; i < result->func_type.num_params; i++) {
			DCP_NODE(func_type.param_types[i]);
		}
		DCP_NODE(func_type.ret_type);
		break;

	case AST_NODE_TEMPL:
		DCP_NODE(templ.body);

		DCP_DLIST(templ.params, templ.num_params);
		for (size_t i = 0; i < result->templ.num_params; i++) {
			DCP_LIT(templ.params[i].name);
			if (src->templ.params[i].type) {
				DCP_NODE(templ.params[i].type);
			} else {
				result->templ.params[i].type = NULL;
			}
			DCP_LIT(templ.params[i].loc);
		}

		DCP_LIT(templ.cons);

		DCP_LIT(templ.closure);
		break;

	case AST_NODE_ACCESS:
		DCP_LIT(access.name);
		DCP_NODE(access.target);
		break;

	case AST_NODE_LIT:
		DCP_LIT(lit);
		break;

	case AST_NODE_LIT_NATIVE:
		DCP_LIT(lit_native.name);
		break;

	case AST_NODE_MOD:
		DCP_LIT(mod.name);
		break;

	case AST_NODE_LOOKUP:
		DCP_LIT(lookup.name);
		DCP_LIT(lookup.ref);
		break;

	case AST_NODE_COMPOSITE:
		DCP_DLIST(composite.members, composite.num_members);
		for (size_t i = 0; i < result->composite.num_members; i++) {
			DCP_LIT(composite.members[i].name);
			DCP_NODE(composite.members[i].type);
		}

		DCP_DLIST(composite.binds, composite.num_binds);
		for (size_t i = 0; i < result->composite.num_binds; i++) {
			DCP_NODE(composite.binds[i].target);
			DCP_NODE(composite.binds[i].value);
			DCP_LIT(composite.binds[i].overridable);
		}

		DCP_DLIST(composite.free_exprs, composite.num_free_exprs);
		for (size_t i = 0; i < result->composite.num_free_exprs; i++) {
			DCP_NODE(composite.free_exprs[i]);
		}

		DCP_LIT(composite.closure);
		break;

	case AST_NODE_VARIANT:
		DCP_DLIST(variant.options, variant.num_options);
		for (size_t i = 0; i < result->variant.num_options; i++) {
			DCP_LIT(variant.options[i].name);
			if (src->variant.options[i].data_type) {
				DCP_NODE(variant.options[i].data_type);
			}
		}

		DCP_LIT(variant.closure);
		break;
	}

#undef DCP_NODE
#undef DCP_LIT
#undef DCP_DLIST

	return result;
}

struct ast_templ_cons_inst {
	struct object *params;
	struct object result;
};

struct ast_templ_cons_param {
	type_id type;
};

struct ast_templ_cons_info {
	struct stg_module *mod;
	struct ast_node *templ_node;

	struct ast_templ_cons_param *params;
	size_t num_params;

	struct ast_typecheck_dep *deps;
	size_t num_deps;

	struct ast_templ_cons_inst *insts;
	size_t num_insts;
};

static int
ast_templ_instantiate(struct ast_context *ctx, struct stg_module *mod,
		struct ast_templ_cons_info *info, void **params, size_t num_params,
		struct object *out_obj)
{
	assert(num_params == info->num_params);

	struct object param_values[info->num_params];

	for (size_t i = 0; i < info->num_params; i++) {
		param_values[i].type = info->params[i].type;
		param_values[i].data = params[i];

		param_values[i] = register_object(
				ctx->vm, &mod->store, param_values[i]);
	}

	// Check if we already have instantiated this template with the given
	// parameters.
	for (size_t inst_i = 0; inst_i < info->num_insts; inst_i++) {
		bool match = true;
		for (size_t i = 0; i < info->num_params; i++) {
			if (!obj_equals(ctx->vm, param_values[i],
						info->insts[inst_i].params[i])) {
				match = false;
				break;
			}
		}

		if (match) {
			*out_obj = info->insts[inst_i].result;
			return 0;
		}
	}

	// TODO: Do we have to copy the node? We would have to reset the type of
	// the nodes if not.
	struct ast_node *templ_node;
	templ_node =
		ast_node_deep_copy(info->templ_node);

	size_t num_body_deps = info->num_deps + info->num_params;
	struct ast_typecheck_dep body_deps[num_body_deps];
	memcpy(body_deps, info->deps,
			info->num_deps * sizeof(struct ast_typecheck_dep));

	for (size_t i = 0; i < info->num_params; i++) {
		struct ast_typecheck_dep *dep;
		dep = &body_deps[info->num_deps+i];

		dep->req = AST_NAME_DEP_REQUIRE_VALUE;
		dep->ref.kind = AST_NAME_REF_TEMPL;
		dep->ref.templ = i;
		dep->determined = true;
		dep->lookup_failed = false;
		dep->val = param_values[i];

		// This will be set by ast_node_typecheck.
		dep->value = -1;
	}

	struct ast_node *body;
	body = templ_node->templ.body;

	size_t num_errors_pre = ctx->err->num_errors;
	int err;
	err = ast_node_typecheck(ctx, info->mod,
			body, body_deps, num_body_deps, TYPE_UNSET);
	if (err) {
		return -1;
	}
	assert(num_errors_pre == ctx->err->num_errors);

	struct ast_gen_info gen_info = {0};

	gen_info.num_templ_values = info->num_params;
	gen_info.templ_values = param_values;

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

	struct bc_env bc_env = {0};
	bc_env.vm = ctx->vm;
	bc_env.store = ctx->vm->instr_store;

	struct ast_gen_bc_result bc;
	bc = ast_node_gen_bytecode(
			ctx, info->mod, &gen_info,
			&bc_env, body);
	if (bc.err) {
		return -1;
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
	mod_arena(mod, &exec_ctx.heap);

	nbc_exec(ctx->vm, &exec_ctx, &nbc_func,
			NULL, 0, NULL, buffer);

	arena_destroy(&exec_ctx.heap);

	res = register_object(ctx->vm, &mod->store, res);

	struct ast_templ_cons_inst inst = {0};

	inst.params = calloc(
			info->num_params,
			sizeof(struct object));

	for (size_t i = 0; i < info->num_params; i++) {
		inst.params[i] =
			register_object(ctx->vm,
					&mod->store, param_values[i]);
	}

	inst.result = res;

	dlist_append(
			info->insts,
			info->num_insts,
			&inst);

	*out_obj = res;

	return 0;
}

static int
ast_templ_pack(struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, void **params, size_t num_params)
{
	struct ast_templ_cons_info *info = data;

	struct object res;
	int err;

	err = ast_templ_instantiate(
			ctx, mod, info, params, num_params, &res);
	if (err) {
		return err;
	}

	struct type *type;
	type = vm_get_type(ctx->vm, res.type);
	memcpy(out, res.data, type->size);

	return 0;
}

type_id
ast_templ_pack_type(struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	struct ast_templ_cons_info *info = data;

	struct object res;
	int err;

	err = ast_templ_instantiate(
			ctx, mod, info, params, num_params, &res);
	if (err) {
		return TYPE_UNSET;
	}

	return res.type;
}

int
ast_templ_unpack(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, struct object obj, int param_id)
{
	struct ast_templ_cons_info *info = data;

	struct ast_templ_cons_inst *inst = NULL;
	for (size_t i = 0; i < info->num_insts; i++) {
		if (obj_equals(ctx->vm, obj, info->insts[i].result)) {
			inst = &info->insts[i];
			break;
		}
	}

	if (!inst) {
		return -1;
	}

	assert(param_id < info->num_params);

	struct type *type;
	type = vm_get_type(ctx->vm, inst->params[param_id].type);

	memcpy(out, inst->params[param_id].data, type->size);

	return 0;
}

void ast_templ_impose_constraints(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, struct ast_env *env,
		ast_slot_id ret_type_slot, ast_slot_id *param_slots)
{
	struct ast_templ_cons_info *info = data;

	size_t num_body_deps = info->num_deps + info->num_params;
	struct ast_typecheck_dep body_deps[num_body_deps];
	memcpy(body_deps, info->deps,
			info->num_deps * sizeof(struct ast_typecheck_dep));

	ast_typecheck_deps_slots(env,
			body_deps, info->num_deps);

	for (size_t i = 0; i < info->num_params; i++) {
		struct ast_typecheck_dep *dep;
		dep = &body_deps[info->num_deps+i];

		dep->req = AST_NAME_DEP_REQUIRE_VALUE;
		dep->ref.kind = AST_NAME_REF_TEMPL;
		dep->ref.templ = i;
		dep->determined = false;
		dep->lookup_failed = false;
		dep->value = param_slots[i];
	}

	ast_slot_id result_slot;
	result_slot = ast_node_constraints(
			ctx, info->mod, env, body_deps, num_body_deps,
			info->templ_node->templ.body);

	ast_slot_require_type(
			env, info->templ_node->loc,
			AST_CONSTR_SRC_TEMPL_PARAM_DECL,
			result_slot, ret_type_slot);
}

/*
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
*/

struct object_cons *
ast_node_create_templ(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *templ_node,
		struct ast_typecheck_dep *outer_deps, size_t num_outer_deps,
		struct ast_typecheck_dep *inner_deps, size_t num_inner_deps)
{
	assert(templ_node->templ.body->kind == AST_NODE_FUNC ||
			templ_node->templ.body->kind == AST_NODE_FUNC_NATIVE ||
			templ_node->templ.body->kind == AST_NODE_COMPOSITE ||
			templ_node->templ.body->kind == AST_NODE_VARIANT);

	struct object_cons *def;
	def = calloc(1, sizeof(struct object_cons));

	def->num_params = templ_node->templ.num_params;
	def->params = calloc(def->num_params,
			sizeof(struct object_cons_param));

	struct ast_templ_cons_info *info;
	info = calloc(1, sizeof(struct ast_templ_cons_info));

	info->mod = mod;

	info->num_params = templ_node->templ.num_params;
	info->params = calloc(def->num_params,
			sizeof(struct ast_templ_cons_param));

	info->num_deps = num_inner_deps;
	info->deps = calloc(info->num_deps, sizeof(struct ast_typecheck_dep));
	memcpy(info->deps, inner_deps, sizeof(struct ast_typecheck_dep) * info->num_deps);

	info->templ_node =
		ast_node_deep_copy(templ_node);

	// Partially typecheck to determine the type of the template's parameters.
	struct ast_env inner_env = {0};
	inner_env.store = &mod->store;

	size_t num_param_deps = num_outer_deps;
	struct ast_typecheck_dep param_deps[num_param_deps];
	memcpy(param_deps, outer_deps,
			num_param_deps * sizeof(struct ast_typecheck_dep));
	ast_typecheck_deps_slots(&inner_env,
			param_deps, num_param_deps);

	size_t num_body_deps = info->num_deps + info->num_params;
	struct ast_typecheck_dep body_deps[num_body_deps];
	memcpy(body_deps, info->deps,
			info->num_deps * sizeof(struct ast_typecheck_dep));
	ast_typecheck_deps_slots(&inner_env,
			body_deps, info->num_deps);

	ast_slot_id param_type_slots[info->num_params];

	for (size_t i = 0; i < info->num_params; i++) {
		struct ast_typecheck_dep *dep;
		dep = &body_deps[info->num_deps+i];

		dep->req = AST_NAME_DEP_REQUIRE_VALUE;
		dep->ref.kind = AST_NAME_REF_TEMPL;
		dep->ref.templ = i;
		dep->determined = false;
		dep->lookup_failed = false;
		dep->value = ast_slot_alloc(&inner_env);

		struct ast_node *param_type;
		param_type = info->templ_node->templ.params[i].type;

		ast_slot_id type_slot;

		if (param_type) {
			// We use the outer_deps for the parameters as they are not
			// supposed to be able to see the other parameters.
			type_slot = ast_node_constraints(
					ctx, mod, &inner_env,
					param_deps, num_param_deps,
					param_type);
		} else {
			type_slot = ast_slot_alloc(&inner_env);
		}

		param_type_slots[i] = type_slot;

		// TODO: Location?
		ast_slot_require_type(
				&inner_env, STG_NO_LOC,
				AST_CONSTR_SRC_TEMPL_PARAM_DECL,
				dep->value, type_slot);
	}

	ast_node_constraints(
			ctx, mod, &inner_env,
			body_deps, num_body_deps,
			info->templ_node->templ.body);
 
	struct ast_slot_result result[inner_env.num_alloced_slots];

#if AST_DEBUG_SLOT_SOLVE
	printf("Preliminary type solve for template\n");
	for (size_t i = 0; i < info->num_params; i++) {
		printf(" - param %.*s: %i:%i\n",
				ALIT(info->templ_node->templ.params[i].name),
				body_deps[info->num_deps+i].value, param_type_slots[i]);
		if (info->templ_node->templ.params[i].type) {
			printf("     ");
			ast_print_node(ctx,
					info->templ_node->templ.params[i].type, true);
		}
	}
	ast_print_node(ctx,
			info->templ_node->templ.body, true);
	printf("\n");
#endif

	int err;
	err = ast_slot_try_solve(
			ctx, mod, &inner_env, result);

	// We can accept not all values having being determined (err >= 0).
	if (err >= 0) {
		for (size_t i = 0; i < def->num_params; i++) {
			def->params[i].name = info->templ_node->templ.params[i].name;

			struct ast_slot_result *res;
			res = &result[param_type_slots[i]];

			if (ast_slot_value_result(res->result) ==
					AST_SLOT_RES_VALUE_FOUND_TYPE) {
				def->params[i].type = res->value.type;
				info->params[i].type = res->value.type;
			} else if (ast_slot_value_result(res->result) ==
					AST_SLOT_RES_VALUE_UNKNOWN) {
				def->params[i].type = TYPE_UNSET;
				info->params[i].type = TYPE_UNSET;
				stg_error(ctx->err, info->templ_node->loc,
						"Could not determine the type of the parameter '%.*s'.",
						ALIT(def->params[i].name));
				err = -1;
			} else {
				def->params[i].type = TYPE_UNSET;
				info->params[i].type = TYPE_UNSET;
				panic("Template parameter type was not a type.");
				err = -1;
			}
		}
	}

#if AST_DEBUG_SLOT_SOLVE
	for (size_t i = 0; i < info->num_params; i++) {
		printf(" - param %.*s: ",
				ALIT(info->templ_node->templ.params[i].name));
		if (def->params[i].type != TYPE_UNSET) {
			print_type_repr(ctx->vm, vm_get_type(ctx->vm, def->params[i].type));
			printf("\n");
		} else {
			printf("type not resolved.\n");
		}
	}
	printf("\n");
#endif

	if (err < 0) {
		free(def->params);
		free(def);
		free(info);
		free(info->deps);

		// TODO: Free info->templ_nodes

		return NULL;
	}

	def->data         = info;
	def->ct_pack      = ast_templ_pack;
	def->ct_unpack    = ast_templ_unpack;
	def->ct_pack_type = ast_templ_pack_type;
	def->impose_constraints =
		ast_templ_impose_constraints;
	// def->can_unpack = ast_templ_can_unpack;

	return def;
}
