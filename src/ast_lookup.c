#include "vm.h"
#include "ast.h"
#include "native.h"
#include "dlist.h"
#include <stdlib.h>
#include <string.h>

struct ast_resolve_info {
	struct stg_native_module *native_mod;

	size_t num_ambiguous_refs;
};

void
ast_scope_push_composite(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_kind = AST_SCOPE_PARENT_CLOSURE;
}

void
ast_scope_push_templ(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_kind = AST_SCOPE_PARENT_CLOSURE;
}

void
ast_scope_push_pattern(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_kind = AST_SCOPE_PARENT_LOCAL;
}

void
ast_scope_push_expr(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_kind = AST_SCOPE_PARENT_LOCAL;
}

void
ast_scope_push_func(struct ast_scope *target, struct ast_scope *parent)
{
	memset(target, 0, sizeof(struct ast_scope));

	target->parent = parent;
	target->parent_kind = AST_SCOPE_PARENT_CLOSURE;
}

static struct ast_closure_target *
ast_node_get_closure_target(struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_FUNC:
			return &node->func.closure;

		case AST_NODE_COMPOSITE:
			return &node->composite.closure;

		case AST_NODE_TEMPL:
			return &node->templ.closure;

		case AST_NODE_TYPE_CLASS:
			return &node->type_class.closure;

		default:
			return NULL;
	}
}

static struct ast_name_ref
ast_try_lookup_in_scope(struct ast_context *ctx,
		struct ast_scope *scope, struct atom *name)
{
	struct ast_name_ref res = {0};
	res.kind = AST_NAME_REF_NOT_FOUND;

	for (size_t i = 0; i < scope->num_names; i++) {
		if (scope->names[i].name == name) {
			if (res.kind == AST_NAME_REF_NOT_FOUND ||
					res.kind == AST_NAME_REF_CLOSURE) {
				res = scope->names[i].ref;
			} else {
				if (scope->names[i].ref.kind != AST_NAME_REF_NOT_FOUND &&
						scope->names[i].ref.kind != AST_NAME_REF_CLOSURE) {
					panic("Conflicting scope name '%.*s'\n", ALIT(name));
				}
			}
		}
	}

	return res;
}

enum ast_traverse_role {
	AST_TRAV_ROOT = 0,

	AST_TRAV_FUNC_PARAM_TYPE,
	AST_TRAV_FUNC_RET_TYPE,
	AST_TRAV_FUNC_BODY,

	AST_TRAV_CALL_TARGET,
	AST_TRAV_CALL_ARG,

	AST_TRAV_INST_TARGET,
	AST_TRAV_INST_ARG,

	AST_TRAV_CONS_TARGET,
	AST_TRAV_CONS_ARG,

	AST_TRAV_FUNC_TYPE_PARAM_TYPE,
	AST_TRAV_FUNC_TYPE_RET_TYPE,

	AST_TRAV_TEMPL_PARAM_TYPE,
	AST_TRAV_TEMPL_BODY,

	AST_TRAV_ACCESS_TARGET,

	AST_TRAV_MATCH_VALUE,
	AST_TRAV_MATCH_PATTERN_TYPE,
	AST_TRAV_MATCH_PATTERN_EXPR,
	AST_TRAV_MATCH_PATTERN,

	AST_TRAV_COMPOSITE_MEMBER_TYPE,
	AST_TRAV_COMPOSITE_BIND_TARGET,
	AST_TRAV_COMPOSITE_BIND_VALUE,
	AST_TRAV_COMPOSITE_FREE_EXPR,
	AST_TRAV_COMPOSITE_INIT_EXPR,
	AST_TRAV_COMPOSITE_IMPL_TARGET,
	AST_TRAV_COMPOSITE_IMPL_ARG,
	AST_TRAV_COMPOSITE_IMPL_VALUE,

	AST_TRAV_TC_PATTERN_PARAM_TYPE,
	AST_TRAV_TC_MBR_PARAM_TYPE,
	AST_TRAV_TC_MBR_TYPE,
	AST_TRAV_VARIANT_OPTION_TYPE,
};

typedef int (*ast_traverse_scope_t)(
		struct ast_context *, struct ast_scope *,
		struct ast_node *, enum ast_traverse_role,
		void *user_data);

static inline int
ast_node_traverse_scope(ast_traverse_scope_t visit,
		void *user_data, struct ast_context *ctx,
		struct ast_scope *scope, struct ast_node *node)
{
	int err = 0;

	switch (node->kind) {
		case AST_NODE_FUNC:
			{
				struct ast_scope params_scope = {0};

				ast_scope_push_func(&params_scope, scope);

				params_scope.closure_target = node;

				params_scope.num_names = node->func.num_params;
				struct ast_scope_name params_scope_names[params_scope.num_names];
				params_scope.names = params_scope_names;

				for (size_t i = 0; i < params_scope.num_names; i++) {
					params_scope.names[i].name = node->func.params[i].name;

					params_scope.names[i].ref.kind = AST_NAME_REF_PARAM;
					params_scope.names[i].ref.param = i;
				}

				for (size_t i = 0; i < params_scope.num_names; i++) {
					if (node->func.params[i].type) {
						err += visit(ctx, scope, node->func.params[i].type,
								AST_TRAV_FUNC_PARAM_TYPE, user_data);
					}
				}

				if (node->func.return_type) {
					err += visit(ctx, scope, node->func.return_type,
							AST_TRAV_FUNC_RET_TYPE, user_data);
				}

				err += visit(ctx, &params_scope, node->func.body,
						AST_TRAV_FUNC_BODY, user_data);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			{
				for (size_t i = 0; i < node->func.num_params; i++) {
					err += visit(ctx, scope, node->func.params[i].type,
							AST_TRAV_FUNC_PARAM_TYPE, user_data);
				}

				err += visit(ctx, scope, node->func.return_type,
						AST_TRAV_FUNC_RET_TYPE, user_data);
			}
			break;

		case AST_NODE_CALL:
			err += visit(ctx, scope, node->call.func,
					AST_TRAV_CALL_TARGET, user_data);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += visit(ctx, scope, node->call.args[i].value,
						AST_TRAV_CALL_ARG, user_data);
			}
			break;

		case AST_NODE_INST:
			err += visit(ctx, scope, node->call.func,
					AST_TRAV_INST_TARGET, user_data);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += visit(ctx, scope, node->call.args[i].value,
						AST_TRAV_INST_ARG, user_data);
			}
			break;

		case AST_NODE_CONS:
			err += visit(ctx, scope, node->call.func,
					AST_TRAV_CONS_TARGET, user_data);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += visit(ctx, scope, node->call.args[i].value,
						AST_TRAV_CONS_ARG, user_data);
			}
			break;

		case AST_NODE_FUNC_TYPE:
			err += visit(ctx, scope, node->func_type.ret_type,
					AST_TRAV_FUNC_TYPE_RET_TYPE, user_data);
			for (size_t i = 0; i < node->func_type.num_params; i++) {
				err += visit(ctx, scope, node->func_type.param_types[i],
						AST_TRAV_FUNC_TYPE_PARAM_TYPE, user_data);
			}
			break;

		case AST_NODE_TEMPL:
			{
				struct ast_scope templates_scope = {0};

				ast_scope_push_templ(&templates_scope, scope);
				templates_scope.closure_target = node;

				templates_scope.num_names = node->templ.pattern.num_params;
				struct ast_scope_name template_scope_names[templates_scope.num_names];
				templates_scope.names = template_scope_names;

				for (size_t i = 0; i < templates_scope.num_names; i++) {
					templates_scope.names[i].name =
						node->templ.pattern.params[i].name;
					templates_scope.names[i].ref.kind = AST_NAME_REF_TEMPL;
					templates_scope.names[i].ref.templ = i;

					if (node->templ.pattern.params[i].type) {
						err += visit(ctx, scope, node->templ.pattern.params[i].type,
								AST_TRAV_TEMPL_PARAM_TYPE, user_data);
					}
				}

				err += visit(ctx, &templates_scope, node->templ.pattern.node,
						AST_TRAV_TEMPL_BODY, user_data);
			}
			break;

		case AST_NODE_ACCESS:
			err += visit(ctx, scope, node->access.target,
					AST_TRAV_ACCESS_TARGET, user_data);
			break;

		case AST_NODE_MATCH:
			{
				err += visit(ctx, scope, node->match.value,
						AST_TRAV_MATCH_VALUE, user_data);

				for (size_t case_i = 0; case_i < node->match.num_cases; case_i++) {
					struct ast_match_case *match_case;
					match_case = &node->match.cases[case_i];

					struct ast_scope pattern_scope = {0};
					ast_scope_push_pattern(&pattern_scope, scope);
					pattern_scope.num_names = match_case->pattern.num_params;
					struct ast_scope_name pattern_scope_names[pattern_scope.num_names];
					pattern_scope.names = pattern_scope_names;

					for (size_t param_i = 0; param_i < pattern_scope.num_names; param_i++) {
						struct ast_scope_name *name;
						name = &pattern_scope.names[param_i];

						name->name = match_case->pattern.params[param_i].name;
						name->ref.kind = AST_NAME_REF_TEMPL;
						name->ref.templ = param_i;

						if (match_case->pattern.params[param_i].type) {
							err += visit(ctx, scope, match_case->pattern.params[param_i].type,
									AST_TRAV_MATCH_PATTERN_TYPE, user_data);
						}
					}

					// TODO: Should we allow non-const cases?
					err += visit(ctx, &pattern_scope, match_case->pattern.node,
							AST_TRAV_MATCH_PATTERN, user_data);
					err += visit(ctx, &pattern_scope, match_case->expr,
							AST_TRAV_MATCH_PATTERN_EXPR, user_data);
				}
			}
			break;

		case AST_NODE_COMPOSITE:
			{
				struct ast_scope member_scope = {0};

				ast_scope_push_composite(&member_scope, scope);

				member_scope.closure_target = node;

				struct ast_scope_name names[node->composite.num_members];
				for (size_t i = 0; i < node->composite.num_members; i++) {
					names[i].name = node->composite.members[i].name;
					names[i].ref.kind = AST_NAME_REF_MEMBER;
					names[i].ref.member.id = -1;
					names[i].ref.member.unpack_id = -1;
				}

				member_scope.names = names;
				member_scope.num_names = node->composite.num_members;

				for (size_t i = 0; i < node->composite.num_members; i++) {
					if (node->composite.members[i].type) {
						err += visit(ctx, &member_scope, node->composite.members[i].type,
								AST_TRAV_COMPOSITE_MEMBER_TYPE, user_data);
					}
				}

				for (size_t i = 0; i < node->composite.num_binds; i++) {
					struct ast_node *target;
					target = node->composite.binds[i].target;

					err += visit(ctx, &member_scope, target,
							AST_TRAV_COMPOSITE_BIND_TARGET, user_data);

					struct ast_scope value_scope = {0};
					struct ast_scope_name self_name = {0};

					struct ast_scope *effective_value_scope = &member_scope;

					struct atom *trivial_name = NULL;

					// TODO: Support nameing of more complex targets.
					if (target->kind == AST_NODE_LOOKUP) {
						trivial_name = target->lookup.name;
					}

					if (trivial_name) {
						ast_scope_push_expr(&value_scope, &member_scope);

						self_name.name = trivial_name;
						self_name.ref.kind = AST_NAME_REF_SELF;
						self_name.ref.self_offset = 0;

						value_scope.names = &self_name;
						value_scope.num_names = 1;
					}

					err += visit(ctx, effective_value_scope,
							node->composite.binds[i].value,
							AST_TRAV_COMPOSITE_BIND_VALUE, user_data);
				}

				for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
					err += visit(ctx, &member_scope,
							node->composite.free_exprs[i],
							AST_TRAV_COMPOSITE_FREE_EXPR, user_data);
				}

				for (size_t i = 0; i < node->composite.num_init_exprs; i++) {
					err += visit(ctx, &member_scope,
							node->composite.init_exprs[i],
							AST_TRAV_COMPOSITE_INIT_EXPR, user_data);
				}

				for (size_t impl_i = 0; impl_i < node->composite.num_impls; impl_i++) {
					struct ast_datatype_impl *impl;
					impl = &node->composite.impls[impl_i];

					err += visit(ctx, &member_scope, impl->target,
							AST_TRAV_COMPOSITE_IMPL_TARGET, user_data);

					for (size_t arg_i = 0; arg_i < impl->num_args; arg_i++) {
						err += visit(ctx, &member_scope,
								impl->args[arg_i].value,
								AST_TRAV_COMPOSITE_IMPL_ARG, user_data);
					}

					err += visit(ctx, &member_scope, impl->value,
							AST_TRAV_COMPOSITE_IMPL_VALUE, user_data);
				}
			}
			break;

		case AST_NODE_TYPE_CLASS:
			{
				struct ast_scope body_scope = {0};

				ast_scope_push_templ(&body_scope, scope);
				body_scope.closure_target = node;

				body_scope.num_names = node->type_class.pattern.num_params;
				struct ast_scope_name template_scope_names[body_scope.num_names];
				body_scope.names = template_scope_names;

				for (size_t i = 0; i < body_scope.num_names; i++) {
					body_scope.names[i].name =
						node->type_class.pattern.params[i].name;
					body_scope.names[i].ref.kind = AST_NAME_REF_TEMPL;
					body_scope.names[i].ref.templ = i;

					if (node->type_class.pattern.params[i].type) {
						err += visit(ctx, scope,
								node->type_class.pattern.params[i].type,
								AST_TRAV_TC_PATTERN_PARAM_TYPE, user_data);
					}
				}

				for (size_t i = 0; i < node->type_class.num_members; i++) {
					struct ast_type_class_member *mbr;
					mbr = &node->type_class.members[i];
					struct ast_scope mbr_scope = {0};
					ast_scope_push_pattern(&mbr_scope, &body_scope);

					mbr_scope.num_names = mbr->type.num_params;
					struct ast_scope_name mbr_scope_names[mbr_scope.num_names];
					mbr_scope.names = mbr_scope_names;

					for (size_t p_i = 0; p_i < mbr->type.num_params; p_i++) {
						mbr_scope.names[p_i].name = mbr->type.params[p_i].name;
						mbr_scope.names[p_i].ref.kind = AST_NAME_REF_TEMPL;
						mbr_scope.names[p_i].ref.templ = p_i;

						if (mbr->type.params[p_i].type) {
							err += visit(ctx, scope,
									mbr->type.params[p_i].type,
									AST_TRAV_TC_MBR_PARAM_TYPE, user_data);
						}
					}

					err += visit(ctx, &mbr_scope,
							node->type_class.members[i].type.node,
							AST_TRAV_TC_MBR_TYPE, user_data);
				}
			}
			break;

		case AST_NODE_VARIANT:
			{
				for (size_t i = 0; i < node->variant.num_options; i++) {
					if (node->variant.options[i].data_type) {
						err += visit(ctx, scope,
								node->variant.options[i].data_type,
								AST_TRAV_VARIANT_OPTION_TYPE, user_data);
					}
				}
			}
			break;

		case AST_NODE_LIT:
			break;

		case AST_NODE_LIT_NATIVE:
			break;

		case AST_NODE_MOD:
			break;

		case AST_NODE_WILDCARD:
			break;

		case AST_NODE_INIT_EXPR:
			break;

		case AST_NODE_LOOKUP:
			break;

	}

	return err;
}

struct ast_node_ambigous_refs {
	size_t num_ambiguous_refs;
};

static struct ast_name_ref
ast_scope_lookup_name(struct ast_context *ctx,
		struct ast_scope *root_scope, struct atom *name)
{
	for (struct ast_scope *scope = root_scope;
			scope != NULL;
			scope = scope->parent) {
		struct ast_name_ref ref =
			ast_try_lookup_in_scope(
				ctx, scope, name);
		if (ref.kind != AST_NAME_REF_NOT_FOUND) {
			return ref;
		}

		if (scope->parent_kind == AST_SCOPE_PARENT_CLOSURE) {
			assert(scope->closure_target);
			struct ast_closure_target *closure;
			closure = ast_node_get_closure_target(scope->closure_target);

			for (ast_closure_id i = 0; i < closure->num_members; i++) {
				if (closure->members[i].name == name) {
					struct ast_name_ref ref = {0};
					ref.kind = AST_NAME_REF_CLOSURE;
					ref.closure = i;
					return ref;
				}
			}

			break;
		}
	}

	return (struct ast_name_ref){AST_NAME_REF_NOT_FOUND};
}

static struct ast_closure_target *
ast_scope_find_closure_target(struct ast_context *ctx,
		struct ast_scope *root_scope)
{
	for (struct ast_scope *scope = root_scope;
			scope != NULL;
			scope = scope->parent) {
		if (scope->parent_kind == AST_SCOPE_PARENT_CLOSURE) {
			assert(scope->closure_target);
			return ast_node_get_closure_target(scope->closure_target);
		}
	}
	return NULL;
}

static struct ast_name_ref
ast_scope_lookup_or_propagate_name(
		struct ast_context *ctx, struct ast_scope *root_scope,
		struct atom *name)
{
	{
		struct ast_name_ref ref;
		ref = ast_scope_lookup_name(ctx, root_scope, name);
		if (ref.kind != AST_NAME_REF_NOT_FOUND) {
			return ref;
		}
	}

	struct ast_closure_target *closure;
	closure = ast_scope_find_closure_target(
			ctx, root_scope);
	if (!closure) {
		return (struct ast_name_ref){AST_NAME_REF_NOT_FOUND};
	}

	for (ast_closure_id i = 0; i < closure->num_members; i++) {
		if (closure->members[i].name == name) {
			// TODO
			// closure->members[i].require_const |= require_const;

			struct ast_name_ref ref = {0};
			ref.kind = AST_NAME_REF_CLOSURE;
			ref.closure = i;
			return ref;
		}
	}

	struct ast_closure_member mbr = {0};
	mbr.name = name;
	mbr.ref.kind = AST_NAME_REF_NOT_FOUND;
	// TODO
	mbr.require_const = true;
	// mbr.require_const = require_const;

	ast_closure_id closure_id;
	closure_id = dlist_append(
			closure->members,
			closure->num_members,
			&mbr);

	struct ast_name_ref ref = {0};
	ref.kind = AST_NAME_REF_CLOSURE;
	ref.closure = closure_id;

	return ref;
}

static int
ast_node_discover_potential_closures_internal(
		struct ast_context *ctx, struct ast_scope *scope,
		struct ast_node *node, enum ast_traverse_role role,
		void *user_data)
{

	int err = 0;

	switch (node->kind) {
		case AST_NODE_LOOKUP:
			{
				ast_scope_lookup_or_propagate_name(
						ctx, scope, node->lookup.name);
			}
			break;

		default:
			break;
	}


	err += ast_node_traverse_scope(
			ast_node_discover_potential_closures_internal, user_data,
			ctx, scope, node);

	struct ast_closure_target *closure;
	closure = ast_node_get_closure_target(node);
	if (closure) {
		for (size_t i = 0; i < closure->num_members; i++) {
			ast_scope_lookup_or_propagate_name(
					ctx, scope, closure->members[i].name);
		}
	}

	return err;
}

static int
ast_node_has_ambiguous_refs_internal(
		struct ast_context *ctx, struct ast_scope *scope,
		struct ast_node *node, enum ast_traverse_role role,
		void *user_data)
{
	struct ast_node_ambigous_refs *info = user_data;
	int err = 0;

	switch (node->kind) {
		case AST_NODE_LOOKUP:
			{
				struct ast_name_ref ref;
				ref = ast_scope_lookup_name(
						ctx, scope, node->lookup.name);
				if (ref.kind == AST_NAME_REF_NOT_FOUND ||
						ref.kind == AST_NAME_REF_CLOSURE) {
					info->num_ambiguous_refs += 1;
				}
			}
			break;

		default:
			break;
	}

	err += ast_node_traverse_scope(
			ast_node_has_ambiguous_refs_internal, user_data,
			ctx, scope, node);

	struct ast_closure_target *closure;
	closure = ast_node_get_closure_target(node);
	if (closure) {
		for (size_t i = 0; i < closure->num_members; i++) {
			struct ast_name_ref ref;
			ref = ast_scope_lookup_name(
					ctx, scope, closure->members[i].name);
			if (ref.kind == AST_NAME_REF_NOT_FOUND ||
					ref.kind == AST_NAME_REF_CLOSURE) {
				info->num_ambiguous_refs += 1;
			}
		}
	}

	return err;
}

static int
ast_node_resolve_names_internal2(struct ast_context *ctx,
		struct ast_scope *scope, struct ast_node *node,
		enum ast_traverse_role role, void *user_data)
{
	int err = 0;
	bool traverse_children = true;

	switch (node->kind) {
		case AST_NODE_LOOKUP:
			{
				struct ast_name_ref ref;
				ref = ast_scope_lookup_or_propagate_name(
						ctx, scope, node->lookup.name);
				node->lookup.ref = ref;
			}
			break;

		case AST_NODE_COMPOSITE:
			traverse_children = false;

		default:
			break;
	}

	if (traverse_children) {
		err += ast_node_traverse_scope(
				ast_node_resolve_names_internal2, NULL,
				ctx, scope, node);
	}

	struct ast_closure_target *closure;
	closure = ast_node_get_closure_target(node);
	if (closure) {
		for (size_t i = 0; i < closure->num_members; i++) {
			struct ast_name_ref ref;
			ref = ast_scope_lookup_or_propagate_name(
					ctx, scope, closure->members[i].name);
			closure->members[i].ref = ref;
		}
	}

	return err;
}

int
ast_node_resolve_names(struct ast_context *ctx,
		struct stg_native_module *native_mod, struct ast_scope *scope,
		bool require_const, struct ast_node *node)
{
	return ast_node_resolve_names_internal2(
			ctx, scope, node, AST_TRAV_ROOT, NULL);
}

static inline size_t
ast_composite_scope_num_names(struct ast_context *ctx, struct ast_node *comp)
{
	assert(comp->kind == AST_NODE_COMPOSITE);

	size_t num_use_fields = 0;
	for (size_t i = 0; i < comp->composite.num_uses; i++) {
		type_id target_type = comp->composite.uses[i].target->type;
		if (target_type == TYPE_UNSET) {
			continue;
		}

		if (!comp->composite.uses[i].as_name) {
			struct type *type = vm_get_type(ctx->vm, target_type);

			if (!type->obj_inst) {
				printf("Use target does not have an object inst.\n");
				continue;
			}

			struct object_cons *cons;
			cons = type->obj_inst->cons;

			num_use_fields += cons->num_params;
		} else {
			num_use_fields += 1;
		}
	}

	return comp->composite.num_members + num_use_fields;
}

static inline void
ast_composite_setup_scope(struct ast_context *ctx, struct ast_scope *target_scope,
		struct ast_scope *scope, struct ast_node *comp,
		ast_member_id *local_members, struct ast_scope_name *names_buffer,
		struct atom *self_name)
{
	assert(comp->kind == AST_NODE_COMPOSITE);

	ast_scope_push_composite(target_scope, scope);
	target_scope->closure_target = comp;

	size_t num_names = ast_composite_scope_num_names(ctx, comp);
	size_t names_offset = 0;

	for (size_t i = 0; i < comp->composite.num_members; i++) {
		names_buffer[names_offset+i].name = comp->composite.members[i].name;
		if (names_buffer[names_offset+i].name == self_name) {
			names_buffer[names_offset+i].ref.kind = AST_NAME_REF_SELF;
			names_buffer[names_offset+i].ref.self_offset = 0;
		} else {
			names_buffer[names_offset+i].ref.kind = AST_NAME_REF_MEMBER;
			names_buffer[names_offset+i].ref.member.id = local_members[i];
			names_buffer[names_offset+i].ref.member.unpack_id = 0;
		}
	}

	names_offset += comp->composite.num_members;

	for (size_t use_i = 0; use_i < comp->composite.num_uses; use_i++) {
		type_id target_type = comp->composite.uses[use_i].target->type;

		if (target_type == TYPE_UNSET) {
			continue;
		}

		if (!comp->composite.uses[use_i].as_name) {
			struct type *type = vm_get_type(ctx->vm, target_type);

			if (!type->obj_inst) {
				continue;
			}

			struct object_cons *cons;
			cons = type->obj_inst->cons;

			int local_descendent_ids[cons->num_params];
			object_cons_local_descendent_ids(
					ctx->vm, cons, local_descendent_ids);

			for (size_t param_i = 0; param_i < cons->num_params; param_i++) {
				struct ast_scope_name *name;
				name = &names_buffer[names_offset+param_i];

				name->name = cons->params[param_i].name;
				name->ref.kind = AST_NAME_REF_USE;
				name->ref.use.id = use_i;
				name->ref.use.param = local_descendent_ids[param_i];
			}
			names_offset += cons->num_params;
		} else {
			struct ast_scope_name *name;
			name = &names_buffer[names_offset];

			name->name = comp->composite.uses[use_i].as_name;
			name->ref.kind = AST_NAME_REF_USE;
			name->ref.use.id = use_i;
			name->ref.use.param = 0;

			names_offset += 1;
		}
	}

	target_scope->names = names_buffer;
	target_scope->num_names = num_names;
}

int
ast_composite_node_resolve_names(struct ast_context *ctx,
		struct stg_native_module *native_mod, struct ast_scope *scope,
		bool require_const, struct ast_node *comp, struct ast_node *node,
		ast_member_id *local_members, struct atom *self_name)
{
	struct ast_scope member_scope = {0};
	size_t num_scope_names = ast_composite_scope_num_names(ctx, comp);
	struct ast_scope_name member_scope_names[num_scope_names];

	ast_composite_setup_scope(ctx, &member_scope,
			scope, comp, local_members, member_scope_names,
			self_name);

	return ast_node_resolve_names_internal2(
			ctx, &member_scope, node, AST_TRAV_ROOT, NULL);
}

int
ast_node_discover_potential_closures(struct ast_context *ctx,
		struct ast_scope *scope, bool require_const, struct ast_node *node)
{
	return ast_node_discover_potential_closures_internal(
			ctx, scope, node, AST_TRAV_ROOT, NULL);
}

int
ast_node_has_ambiguous_refs(struct ast_context *ctx,
		struct ast_scope *scope, struct ast_node *node)
{
	struct ast_node_ambigous_refs info = {0};
	int err;

	err = ast_node_has_ambiguous_refs_internal(
			ctx, scope, node, AST_TRAV_ROOT, &info);

	return info.num_ambiguous_refs;
}

int
ast_composite_node_has_ambiguous_refs(
		struct ast_context *ctx, struct ast_scope *scope,
		struct ast_node *comp, struct ast_node *node,
		ast_member_id *local_members, struct atom *self_name)
{
	struct ast_scope member_scope = {0};
	size_t num_scope_names = ast_composite_scope_num_names(ctx, comp);
	struct ast_scope_name member_scope_names[num_scope_names];

	ast_composite_setup_scope(ctx, &member_scope,
			scope, comp, local_members, member_scope_names,
			self_name);

	return ast_node_has_ambiguous_refs(ctx,
			&member_scope, node);
}
