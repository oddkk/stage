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

		case AST_NODE_VARIANT:
			return &node->variant.closure;

		case AST_NODE_TEMPL:
			return &node->templ.closure;

		default:
			panic("Attempted to get closure target off a node that does not allow closures (%s).");
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
				assert(scope->names[i].ref.kind == AST_NAME_REF_NOT_FOUND ||
						scope->names[i].ref.kind == AST_NAME_REF_CLOSURE);
			}
		}
	}

	return res;
}

static struct ast_name_ref
ast_resolve_lookup(struct ast_context *ctx, struct ast_resolve_info *info,
		struct ast_scope *root_scope, enum ast_node_resolve_names_flags flags,
		struct atom *name)
{
	bool require_const = !!(flags & AST_NODE_RESOLVE_REQUIRE_CONST);
	bool allow_add_closure = !!(flags & AST_NODE_RESOLVE_ALLOW_ADD_CLOSURE);
	bool preliminary = !!(flags & AST_NODE_RESOLVE_PRELIMINARY);

	for (struct ast_scope *scope = root_scope;
			scope != NULL;
			scope = scope->parent) {
		struct ast_name_ref ref =
			ast_try_lookup_in_scope(
				ctx, scope, name);

		if (ref.kind != AST_NAME_REF_NOT_FOUND) {
			if (ref.kind == AST_NAME_REF_CLOSURE && info) {
				info->num_ambiguous_refs += 1;
			}
			return ref;
		}

		// TODO: We should check for use statements that could also provide names.

		if (scope->parent_kind == AST_SCOPE_PARENT_CLOSURE) {
			assert(scope->closure_target);
			struct ast_closure_target *closure;
			closure = ast_node_get_closure_target(scope->closure_target);

			for (ast_closure_id i = 0; i < closure->num_members; i++) {
				if (closure->members[i].name == name) {
					if (allow_add_closure) {
						closure->members[i].require_const |= require_const;
					}

					struct ast_name_ref ref = {0};
					ref.kind = AST_NAME_REF_CLOSURE;
					ref.closure = i;

					if (info) {
						info->num_ambiguous_refs += 1;
					}

					return ref;
				}
			}

			if (allow_add_closure) {
				struct ast_closure_member mbr = {0};
				mbr.name = name;
				mbr.ref.kind = AST_NAME_REF_NOT_FOUND;
				mbr.require_const = require_const;

				ast_closure_id closure_id;
				closure_id = dlist_append(
						closure->members,
						closure->num_members,
						&mbr);

				struct ast_name_ref ref = {0};
				ref.kind = AST_NAME_REF_CLOSURE;
				ref.closure = closure_id;

				return ref;
			} else if (!preliminary) {
				panic("Tried to lookup target that was not already added as a closure.");
			}

			break;
		}
	}

	if (info) {
		info->num_ambiguous_refs += 1;
	}

	return (struct ast_name_ref){AST_NAME_REF_NOT_FOUND};
}

static int
ast_closure_resolve_names(struct ast_context *ctx, struct ast_resolve_info *info,
		struct ast_scope *scope, enum ast_node_resolve_names_flags flags,
		struct ast_closure_target *closure)
{
	for (size_t i = 0; i < closure->num_members; i++) {
		struct ast_name_ref ref;
		ref = ast_resolve_lookup(ctx, info, scope, flags,
					closure->members[i].name);
		if ((flags & AST_NODE_RESOLVE_PRELIMINARY) == 0) {
			closure->members[i].ref = ref;
		}
	}

	return 0;
}

static inline enum ast_node_resolve_names_flags
flag_req_const(enum ast_node_resolve_names_flags in)
{
	return in | AST_NODE_RESOLVE_REQUIRE_CONST;
}

static inline enum ast_node_resolve_names_flags
flag_no_req_const(enum ast_node_resolve_names_flags in)
{
	return in & (~AST_NODE_RESOLVE_REQUIRE_CONST);
}

static int
ast_node_resolve_names_internal(struct ast_context *ctx,
		struct ast_resolve_info *info, struct ast_scope *scope,
		enum ast_node_resolve_names_flags flags, struct ast_node *node)
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
						err += ast_node_resolve_names_internal(ctx, info, scope,
								flag_req_const(flags), node->func.params[i].type);
					}
				}

				if (node->func.return_type) {
					err += ast_node_resolve_names_internal(ctx, info, scope,
							flag_req_const(flags), node->func.return_type);
				}

				err += ast_node_resolve_names_internal(ctx, info,
						&params_scope, flag_no_req_const(flags),
						node->func.body);

				err += ast_closure_resolve_names(ctx, info,
						scope, flags,
						&node->func.closure);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			{
				for (size_t i = 0; i < node->func.num_params; i++) {
					err += ast_node_resolve_names_internal(ctx, info, scope,
							flag_req_const(flags), node->func.params[i].type);
				}

				err += ast_node_resolve_names_internal(ctx, info, scope,
						flag_req_const(flags), node->func.return_type);

				if ((flags & AST_NODE_RESOLVE_RESOLVE_NATIVE) != 0) {
					assert(info);
					node->func.native.native_mod = info->native_mod;

					if (!info->native_mod) {
						stg_error(ctx->err, node->loc,
								"This module does not have a native module.");
							err += 1;
					} else {
						bool found = false;
						for (size_t i = 0; i < info->native_mod->num_funcs; i++) {
							if (string_equal(
										info->native_mod->funcs[i].name,
										node->func.native.name)) {
								found = true;
								break;
							}
						}

						if (!found) {
							stg_error(ctx->err, node->loc,
									"This module does not have a native function named '%.*s'.",
									LIT(node->func.native.name));
							err += 1;
						}
					}
				}

			}
			break;

		case AST_NODE_CALL:
			err += ast_node_resolve_names_internal(ctx, info,
					scope, flags, node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += ast_node_resolve_names_internal(ctx, info,
						scope, flags, node->call.args[i].value);
			}
			break;

		case AST_NODE_INST:
			err += ast_node_resolve_names_internal(ctx, info,
					scope, flag_req_const(flags), node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += ast_node_resolve_names_internal(ctx, info,
						scope, flags, node->call.args[i].value);
			}
			break;

		case AST_NODE_CONS:
			err += ast_node_resolve_names_internal(ctx, info,
					scope, flag_req_const(flags), node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += ast_node_resolve_names_internal(ctx, info,
						scope, flag_req_const(flags), node->call.args[i].value);
			}
			break;

		case AST_NODE_FUNC_TYPE:
			err += ast_node_resolve_names_internal(
					ctx, info, scope,
					flag_req_const(flags), node->func_type.ret_type);
			for (size_t i = 0; i < node->func_type.num_params; i++) {
				err += ast_node_resolve_names_internal(
						ctx, info, scope,
						flag_req_const(flags), node->func_type.param_types[i]);
			}
			break;

		case AST_NODE_TEMPL:
			{
				struct ast_scope templates_scope = {0};

				ast_scope_push_templ(&templates_scope, scope);
				templates_scope.closure_target = node;

				templates_scope.num_names = node->templ.num_params;
				struct ast_scope_name template_scope_names[templates_scope.num_names];
				templates_scope.names = template_scope_names;

				for (size_t i = 0; i < templates_scope.num_names; i++) {
					templates_scope.names[i].name =
						node->templ.params[i].name;
					templates_scope.names[i].ref.kind = AST_NAME_REF_TEMPL;
					templates_scope.names[i].ref.templ = i;

					if (node->templ.params[i].type) {
						err += ast_node_resolve_names_internal(
								ctx, info, scope, flag_req_const(flags),
								node->templ.params[i].type);
					}
				}

				err += ast_node_resolve_names_internal(ctx, info,
						&templates_scope, flags, node->templ.body);

				err += ast_closure_resolve_names(ctx, info,
						scope, flags,
						&node->templ.closure);
			}
			break;

		case AST_NODE_ACCESS:
			err += ast_node_resolve_names_internal(ctx, info,
					scope, flags, node->access.target);
			break;

		case AST_NODE_LIT:
			break;

		case AST_NODE_LOOKUP:
			{
				struct atom *name = node->lookup.name;
				struct ast_name_ref res;
				res = ast_resolve_lookup(ctx, info, scope,
						flags, name);

				if ((flags & AST_NODE_RESOLVE_PRELIMINARY) == 0) {
					node->lookup.ref = res;

					if (res.kind == AST_NAME_REF_NOT_FOUND) {
						stg_error(ctx->err, node->loc, "'%.*s' was not found.",
								ALIT(name));
						err += 1;
					}
				}
			}
			break;

		case AST_NODE_COMPOSITE:
			{
				if ((flags & AST_NODE_RESOLVE_VISIT_MEMBERS) != 0) {
					struct ast_scope member_scope = {0};

					ast_scope_push_composite(&member_scope, scope);

					member_scope.closure_target = node;

					struct ast_scope_name names[node->composite.num_members];
					for (size_t i = 0; i < node->composite.num_members; i++) {
						names[i].name = node->composite.members[i].name;
						names[i].ref.kind = AST_NAME_REF_MEMBER;
						names[i].ref.member = -1;
					}

					member_scope.names = names;
					member_scope.num_names = node->composite.num_members;

					for (size_t i = 0; i < node->composite.num_members; i++) {
						if (node->composite.members[i].type) {
							err += ast_node_resolve_names_internal(
									ctx, info, &member_scope, flag_req_const(flags),
									node->composite.members[i].type);
						}
					}

					for (size_t i = 0; i < node->composite.num_binds; i++) {
						err += ast_node_resolve_names_internal(
								ctx, info, &member_scope, flag_no_req_const(flags),
								node->composite.binds[i].target);
						err += ast_node_resolve_names_internal(
								ctx, info, &member_scope, flag_no_req_const(flags),
								node->composite.binds[i].value);
					}

					for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
						err += ast_node_discover_potential_closures(ctx,
								&member_scope, flag_no_req_const(flags),
								node->composite.free_exprs[i]);
					}

					// TODO: Visit use targets.
				}

				err += ast_closure_resolve_names(ctx, info,
						scope, flags, &node->composite.closure);
			}
			break;

		case AST_NODE_VARIANT:
			{
				struct ast_scope member_scope = {0};
				ast_scope_push_composite(&member_scope, scope);
				member_scope.closure_target = node;

				err += ast_closure_resolve_names(ctx, info,
						scope, flags, &node->variant.closure);

				for (size_t i = 0; i < node->variant.num_options; i++) {
					if (node->variant.options[i].data_type) {
						err += ast_node_resolve_names_internal(
								ctx, info, &member_scope,
								flag_req_const(flags),
								node->variant.options[i].data_type);
					}
				}
			}
			break;
	}

	return err;
}

int
ast_node_resolve_names(struct ast_context *ctx,
		struct stg_native_module *native_mod, struct ast_scope *scope,
		bool require_const, struct ast_node *node)
{
	enum ast_node_resolve_names_flags flags = 0;

	flags |= AST_NODE_RESOLVE_RESOLVE_NATIVE;

	if (require_const) {
		flags |= AST_NODE_RESOLVE_REQUIRE_CONST;
	}

	struct ast_resolve_info info = {0};
	info.native_mod = native_mod;

	return ast_node_resolve_names_internal(
			ctx, &info, scope, flags, node);
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

		struct type *type = vm_get_type(ctx->vm, target_type);

		if (!type->obj_inst) {
			printf("Use target does not have an object inst.\n");
			continue;
		}

		struct object_cons *cons;
		cons = type->obj_inst->cons;

		num_use_fields += cons->num_params;
	}

	return comp->composite.num_members + num_use_fields;
}

static inline void
ast_composite_setup_scope(struct ast_context *ctx, struct ast_scope *target_scope,
		struct ast_scope *scope, struct ast_node *comp,
		ast_member_id *local_members, struct ast_scope_name *names_buffer)
{
	assert(comp->kind == AST_NODE_COMPOSITE);

	ast_scope_push_composite(target_scope, scope);
	target_scope->closure_target = comp;

	size_t num_names = ast_composite_scope_num_names(ctx, comp);
	size_t names_offset = 0;

	for (size_t i = 0; i < comp->composite.num_members; i++) {
		names_buffer[names_offset+i].name = comp->composite.members[i].name;
		names_buffer[names_offset+i].ref.kind = AST_NAME_REF_MEMBER;
		names_buffer[names_offset+i].ref.member = local_members[i];
	}

	names_offset += comp->composite.num_members;

	for (size_t use_i = 0; use_i < comp->composite.num_uses; use_i++) {
		type_id target_type = comp->composite.uses[use_i].target->type;

		if (target_type == TYPE_UNSET) {
			continue;
		}

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
	}

	target_scope->names = names_buffer;
	target_scope->num_names = num_names;
}

static int
ast_composite_resolve_internal(struct ast_context *ctx,
		struct ast_resolve_info *info, struct ast_scope *scope,
		enum ast_node_resolve_names_flags flags,
		struct ast_node *comp, struct ast_node *node)
{
	if (node == comp) {
		int err = 0;

		for (size_t i = 0; i < node->composite.num_members; i++) {
			if (node->composite.members[i].type) {
				err += ast_node_resolve_names_internal(
						ctx, info, scope, flags,
						node->composite.members[i].type);
			}
		}

		for (size_t i = 0; i < node->composite.num_binds; i++) {
			err += ast_node_resolve_names_internal(
					ctx, info, scope, flags,
					node->composite.binds[i].target);
			err += ast_node_resolve_names_internal(
					ctx, info, scope, flags,
					node->composite.binds[i].value);
		}

		for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
			err += ast_node_resolve_names_internal(
					ctx, info, scope, flags,
					node->composite.free_exprs[i]);
		}

		return err;
	} else {
		return ast_node_resolve_names_internal(
				ctx, info, scope,
				flags, node);
	}
}

int
ast_composite_node_resolve_names(struct ast_context *ctx,
		struct stg_native_module *native_mod, struct ast_scope *scope,
		bool require_const, struct ast_node *comp, struct ast_node *node,
		ast_member_id *local_members)
{
	struct ast_scope member_scope = {0};
	size_t num_scope_names = ast_composite_scope_num_names(ctx, comp);
	struct ast_scope_name member_scope_names[num_scope_names];

	ast_composite_setup_scope(ctx, &member_scope,
			scope, comp, local_members, member_scope_names);

	struct ast_resolve_info info = {0};
	info.native_mod = native_mod;

	enum ast_node_resolve_names_flags flags = 0;

	flags |= AST_NODE_RESOLVE_RESOLVE_NATIVE;

	if (require_const) {
		flags |= AST_NODE_RESOLVE_REQUIRE_CONST;
	}

	return ast_composite_resolve_internal(ctx,
			&info, &member_scope, flags, comp, node);
}

int
ast_node_discover_potential_closures(struct ast_context *ctx,
		struct ast_scope *scope, bool require_const, struct ast_node *node)
{
	enum ast_node_resolve_names_flags flags = 0;

	flags |= AST_NODE_RESOLVE_ALLOW_ADD_CLOSURE;
	flags |= AST_NODE_RESOLVE_PRELIMINARY;
	flags |= AST_NODE_RESOLVE_VISIT_MEMBERS;

	if (require_const) {
		flags |= AST_NODE_RESOLVE_REQUIRE_CONST;
	}

	return ast_node_resolve_names_internal(
			ctx, NULL, scope, flags, node);
}

int
ast_node_has_ambiguous_refs(struct ast_context *ctx,
		struct ast_scope *scope, struct ast_node *node)
{
	enum ast_node_resolve_names_flags flags = 0;
	struct ast_resolve_info info = {0};
	int err;

	flags |= AST_NODE_RESOLVE_PRELIMINARY;
	flags |= AST_NODE_RESOLVE_VISIT_MEMBERS;

	err = ast_node_resolve_names_internal(
			ctx, &info, scope, flags, node);
	if (err) {
		return -1;
	}

	return info.num_ambiguous_refs;
}

int
ast_composite_node_has_ambiguous_refs(
		struct ast_context *ctx, struct ast_scope *scope,
		struct ast_node *comp, struct ast_node *node,
		ast_member_id *local_members)
{
	struct ast_scope member_scope = {0};
	size_t num_scope_names = ast_composite_scope_num_names(ctx, comp);
	struct ast_scope_name member_scope_names[num_scope_names];

	ast_composite_setup_scope(ctx, &member_scope,
			scope, comp, local_members, member_scope_names);

	enum ast_node_resolve_names_flags flags = 0;
	struct ast_resolve_info info = {0};
	int err;

	flags |= AST_NODE_RESOLVE_PRELIMINARY;
	flags |= AST_NODE_RESOLVE_VISIT_MEMBERS;

	err = ast_composite_resolve_internal(ctx,
			&info, &member_scope, flags, comp, node);
	if (err) {
		return -1;
	}

	return info.num_ambiguous_refs;
}
