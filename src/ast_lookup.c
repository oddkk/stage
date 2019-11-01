#include "ast.h"
#include "native.h"
#include "dlist.h"
#include <stdlib.h>
#include <string.h>

void
ast_scope_push_composite(struct ast_scope *target, struct ast_scope *parent)
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

		default:
			panic("Attempted to get closure target off a node that does not allow closures (%s).");
			return NULL;
	}
}

static struct ast_name_ref
ast_try_lookup_in_scope(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *scope, struct atom *name)
{
	for (size_t i = 0; i < scope->num_names; i++) {
		if (scope->names[i].name == name) {
			return scope->names[i].ref;
		}
	}

	return (struct ast_name_ref){AST_NAME_REF_NOT_FOUND};
}

static struct ast_name_ref
ast_resolve_lookup(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *root_scope, bool require_const, struct atom *name,
		bool allow_add_closure)
{
	for (struct ast_scope *scope = root_scope;
			scope != NULL;
			scope = scope->parent) {
		struct ast_name_ref ref =
			ast_try_lookup_in_scope(
				ctx, env, scope, name);

		if (ref.kind != AST_NAME_REF_NOT_FOUND) {
			return ref;
		}

		// TODO: We should check for use statements that could also provide names.

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

			if (allow_add_closure) {
				// We do not already have a closure for this name. Make one.

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
			} else {
				panic("Tried to lookup target that was not already added as a closure.");
			}
		}
	}

	return (struct ast_name_ref){AST_NAME_REF_NOT_FOUND};
}

static int
ast_closure_resolve_names(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *scope, bool require_const,
		struct ast_closure_target *closure)
{
	for (size_t i = 0; i < closure->num_members; i++) {
		struct ast_name_ref ref;
		ref = ast_resolve_lookup(ctx, env, scope, require_const,
					closure->members[i].name, false);
	}

	return 0;
}

int
ast_node_resolve_names(struct ast_context *ctx, struct ast_env *env,
		struct stg_native_module *native_mod, struct ast_scope *scope,
		bool require_const, struct ast_node *node)
{
	int err = 0;

	switch (node->kind) {
		case AST_NODE_FUNC_UNINIT:
			panic("Encountered uninitialized func node while resolving names.");
			break;

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
					err += ast_node_resolve_names(ctx, env, native_mod,
							scope, true, node->func.params[i].type);
				}

				err += ast_node_resolve_names(ctx, env, native_mod,
						scope, true, node->func.return_type);

				err += ast_node_resolve_names(ctx, env, native_mod,
						&params_scope, false, node->func.body);

				err += ast_closure_resolve_names(ctx, env,
						scope, require_const,
						&node->func.closure);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			{
				for (size_t i = 0; i < node->func.num_params; i++) {
					err += ast_node_resolve_names(ctx, env, native_mod,
							scope, true, node->func.params[i].type);
				}

				err += ast_node_resolve_names(ctx, env, native_mod,
						scope, true, node->func.return_type);

				if (!native_mod) {
					stg_error(ctx->err, node->loc,
							"This module does not have a native module.");
						err += 1;
				} else {
					for (size_t i = 0; i < native_mod->num_funcs; i++) {
						if (string_equal(
									native_mod->funcs[i].name,
									node->func.native.name)) {
							node->func.native.func = native_mod->funcs[i].func;
						}
					}

					if (!node->func.native.func) {
						stg_error(ctx->err, node->loc,
								"This module does not have a native function named '%.*s'.",
								LIT(node->func.native.name));
						err += 1;
					}
				}

			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			err += ast_node_resolve_names(ctx, env, native_mod,
					scope, require_const, node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += ast_node_resolve_names(ctx, env, native_mod,
						scope, require_const, node->call.args[i].value);
			}
			break;

		case AST_NODE_TEMPL:
			{
				struct ast_scope templates_scope = {0};

				ast_scope_push_expr(&templates_scope, scope);

				templates_scope.num_names = node->templ.num_params;
				struct ast_scope_name template_scope_names[templates_scope.num_names];
				templates_scope.names = template_scope_names;

				panic("TODO: Implement lookup for templ nodes.");

				for (size_t i = 0; i < templates_scope.num_names; i++) {
					templates_scope.names[i].name =
						node->templ.params[i].name;
					// templates_scope.names[i].slot =
					// 	node->templ.params[i].slot;
				}

				err += ast_node_resolve_names(ctx, env, native_mod,
						&templates_scope, require_const, node->templ.body);
			}
			break;

		case AST_NODE_ACCESS:
			err += ast_node_resolve_names(ctx, env, native_mod,
					scope, true, node->access.target);
			// At this point we do not have type information, and thus can not
			// resolve accesses.
			break;

		case AST_NODE_SLOT:
			break;

		case AST_NODE_LIT:
			break;

		case AST_NODE_LOOKUP:
			{
				struct atom *name = node->lookup.name;
				struct ast_name_ref res;
				res = ast_resolve_lookup(ctx, env, scope,
						require_const, name, false);

				node->lookup.ref = res;

				if (res.kind == AST_NAME_REF_NOT_FOUND) {
					stg_error(ctx->err, node->loc, "'%.*s' was not found.",
							ALIT(name));
					err += 1;
				}
			}
			break;

		case AST_NODE_COMPOSITE:
			err += ast_closure_resolve_names(ctx, env,
					scope, require_const,
					&node->composite.closure);
			break;

		case AST_NODE_VARIANT:
			// TODO: Populate scope.
			// TODO: Do lookup.
			break;
	}

	return err;
}

static void
ast_closure_discover_potential_closures(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *scope, bool require_const,
		struct ast_closure_target *closure)
{
	for (size_t i = 0; i < closure->num_members; i++) {
		ast_resolve_lookup(ctx, env, scope, require_const,
					closure->members[i].name, true);
	}
}

int
ast_node_discover_potential_closures(struct ast_context *ctx, struct ast_env *env,
		struct ast_scope *scope, bool require_const, struct ast_node *node)
{
	int err = 0;

	switch (node->kind) {
		case AST_NODE_FUNC_UNINIT:
			panic("Encountered uninitialized func node while resolving names.");
			break;

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
					err += ast_node_discover_potential_closures(
							ctx, env, scope, true,
							node->func.params[i].type);
				}

				err += ast_node_discover_potential_closures(
						ctx, env, scope, true,
						node->func.return_type);

				err += ast_node_discover_potential_closures(
						ctx, env, &params_scope, false,
						node->func.body);

				ast_closure_discover_potential_closures(ctx, env,
						scope, require_const,
						&node->func.closure);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			{
				for (size_t i = 0; i < node->func.num_params; i++) {
					err += ast_node_discover_potential_closures(
							ctx, env, scope, true,
							node->func.params[i].type);
				}

				err += ast_node_discover_potential_closures(
						ctx, env, scope, true,
						node->func.return_type);
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			err += ast_node_discover_potential_closures(
					ctx, env, scope, require_const,
					node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				err += ast_node_discover_potential_closures(
						ctx, env, scope, require_const,
						node->call.args[i].value);
			}
			break;

		case AST_NODE_TEMPL:
			{
				struct ast_scope templates_scope = {0};

				ast_scope_push_expr(&templates_scope, scope);

				templates_scope.num_names = node->templ.num_params;
				struct ast_scope_name template_scope_names[templates_scope.num_names];
				templates_scope.names = template_scope_names;

				panic("TODO: Implement lookup for templ nodes.");

				for (size_t i = 0; i < templates_scope.num_names; i++) {
					templates_scope.names[i].name =
						node->templ.params[i].name;
					// TODO: Template params should probably have their own ref kind
					templates_scope.names[i].ref.kind = AST_NAME_REF_PARAM;
					templates_scope.names[i].ref.param = -1;
				}

				err += ast_node_discover_potential_closures(ctx, env,
						&templates_scope, require_const, node->templ.body);
			}
			break;

		case AST_NODE_ACCESS:
			err += ast_node_discover_potential_closures(ctx, env,
					scope, true, node->access.target);
			break;

		case AST_NODE_SLOT:
			break;

		case AST_NODE_LIT:
			break;

		case AST_NODE_LOOKUP:
			{
				struct atom *name = node->lookup.name;
				ast_resolve_lookup(ctx, env, scope,
						require_const, name, true);
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
					names[i].ref.member = -1;
				}

				member_scope.names = names;
				member_scope.num_names = node->composite.num_members;

				for (size_t i = 0; i < node->composite.num_members; i++) {
					if (node->composite.members[i].type) {
						err += ast_node_discover_potential_closures(
								ctx, env, &member_scope, true,
								node->composite.members[i].type);
					}
				}

				for (size_t i = 0; i < node->composite.num_binds; i++) {
					err += ast_node_discover_potential_closures(
							ctx, env, &member_scope, false,
							node->composite.binds[i].target);
					err += ast_node_discover_potential_closures(
							ctx, env, &member_scope, false,
							node->composite.binds[i].value);
				}

				for (size_t i = 0; i < node->composite.num_free_exprs; i++) {
					err += ast_node_discover_potential_closures(ctx, env,
							&member_scope, false, node->composite.free_exprs[i]);
				}

				ast_closure_discover_potential_closures(ctx, env,
						scope, require_const,
						&node->composite.closure);
			}
			break;

		case AST_NODE_VARIANT:
			// TODO: Populate scope.
			// TODO: Do lookup.
			break;
	}

	return err;
}
