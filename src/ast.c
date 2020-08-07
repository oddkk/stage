#include "ast.h"
#include "compile.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "dlist.h"
#include "module.h"
#include "native.h"
#include "base/mod.h"

void
ast_init_context(
		struct ast_context *ctx,
		struct stg_error_context *err,
		struct vm *vm)
{
	memset(ctx, 0, sizeof(struct ast_context));
	ctx->err = err;
	ctx->vm = vm;
	ctx->mem = &ctx->_mem;
	arena_init(ctx->mem, &vm->mem);
}

void
ast_destroy_context(struct ast_context *ctx)
{
	arena_destroy(ctx->mem);
	memset(ctx, 0, sizeof(struct ast_context));
}

void
ast_module_init(struct vm *vm, stg_mod_id mod_id, struct ast_module *mod)
{
	mod->id = mod_id;

	paged_list_init(
			&mod->data_types,
			&vm->mem,
			sizeof(struct ast_node));
}

struct ast_node *
ast_module_get_data_type(struct ast_module *mod, ast_data_type_id dtid)
{
	return paged_list_get(&mod->data_types, dtid);
}

struct ast_node *
ast_module_node_get_data_type(struct vm *vm, struct ast_node *node)
{
	assert(node->kind == AST_NODE_DATA_TYPE);
	struct stg_module *dt_mod;
	dt_mod = vm_get_module_by_id(
			vm, node->data_type.mod);

	return ast_module_get_data_type(dt_mod->ast_mod, node->data_type.id);
}

static ast_data_type_id
ast_module_alloc_data_type(struct ast_module *mod)
{
	return paged_list_push(&mod->data_types);
}

ast_data_type_id
ast_module_add_composite(struct ast_context *ctx,
		struct ast_module *mod, struct stg_location loc, enum ast_composite_kind kind)
{
	ast_data_type_id id;
	id = ast_module_alloc_data_type(mod);

	struct ast_node *node;
	node = ast_module_get_data_type(mod, id);

	node = ast_init_node_composite(
			ctx, node, loc, kind);

	return id;
}

ast_data_type_id
ast_module_add_variant(struct ast_context *ctx, struct ast_module *mod,
		struct stg_location loc)
{
	ast_data_type_id id;
	id = ast_module_alloc_data_type(mod);

	struct ast_node *node;
	node = ast_module_get_data_type(mod, id);

	node = ast_init_node_variant(
			ctx, node, loc);

	return id;
}

ast_data_type_id
ast_module_add_type_class(struct ast_context *ctx, struct ast_module *mod,
		struct stg_location loc, struct ast_type_class_member *members,
		size_t num_members)
{
	ast_data_type_id id;
	id = ast_module_alloc_data_type(mod);

	struct ast_node *node;
	node = ast_module_get_data_type(mod, id);

	node = ast_init_node_type_class(
			ctx, node, loc, members, num_members);

	return id;
}

struct ast_node *
ast_namespace_add_ns(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_node *ns, struct atom *name)
{
	assert(ns->kind == AST_NODE_COMPOSITE);

	ast_data_type_id dt;

	dt = ast_module_add_composite(
			ctx, mod, STG_NO_LOC,
			AST_COMPOSITE_NAMESPACE);

	struct ast_node *ns_type;
	// TODO: Add a location to make error messages more helpful.
	ns_type = ast_init_node_data_type(
			ctx, AST_NODE_NEW, STG_NO_LOC, mod->id, dt);

	int err;
	err = ast_node_composite_add_namespace(
			ctx, mod, ns, name, ns_type);
	if (err) {
		stg_error(ctx->err, ns->loc,
				"This namespace already has a member named '%.*s'.",
				ALIT(name));
	}

	return ast_module_get_data_type(mod, dt);
}

int
ast_node_check_native_funcs(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *node)
{
	int err = 0;

	if (node->kind == AST_NODE_FUNC_NATIVE) {
		node->func.native.native_mod = mod->native_mod;

		if (!mod->native_mod) {
			stg_error(ctx->err, node->loc,
					"This module does not have a native module.");
			err += 1;
		} else {
			bool found = false;
			for (size_t i = 0; i < mod->native_mod->num_funcs; i++) {
				if (string_equal(
							mod->native_mod->funcs[i].name,
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

#define VISIT_NODE(n) \
	err += ast_node_check_native_funcs(ctx, mod, (n));
	AST_NODE_VISIT(node, true, true, true);
#undef VISIT_NODE

	switch (node->kind) {

		case AST_NODE_DATA_TYPE:
			{
				struct ast_node *dt_node;
				dt_node = ast_module_node_get_data_type(
						ctx->vm, node);

				ast_node_check_native_funcs(ctx, mod, dt_node);
			}

		default:
			break;
	}

	return err;
}

int
ast_module_finalize(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *root)
{
	int err;
	err = ast_node_discover_potential_closures(
			ctx, mod, NULL, true, root);

	ast_node_check_native_funcs(
			ctx, mod, root);

	struct ast_node *root_dt;
	root_dt = ast_module_node_get_data_type(
			mod->vm, root);

	// type_id type;
	// type = ast_dt_finalize_composite(ctx, mod,
	// 		root_dt, NULL, 0);

	ast_dt_process(ctx, mod);

	type_id type;
	type = root_dt->composite.type;

	if (type == TYPE_UNSET) {
		return -1;
	}

	err = stg_instantiate_static_object(
			ctx, mod, type, &mod->init_monad);
	if (err) {
		return -1;
	}

	return vm_mod_init(mod);
}
