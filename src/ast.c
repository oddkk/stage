#include "ast.h"
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

struct ast_node *
ast_namespace_add_ns(struct ast_context *ctx,
		struct ast_node *ns, struct atom *name)
{
	assert(ns->kind == AST_NODE_COMPOSITE);

	struct ast_node *ns_type;
	// TODO: Add a location to make error messages more helpful.
	ns_type = ast_init_node_composite(
			ctx, AST_NODE_NEW, STG_NO_LOC,
			AST_COMPOSITE_NAMESPACE);

	int err;
	err = ast_node_composite_add_namespace(
			ctx, ns, name, ns_type);
	if (err) {
		stg_error(ctx->err, ns->loc,
				"This namespace already has a member named '%.*s'.",
				ALIT(name));
	}

	return ns_type;
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

	return err;
}

int
ast_module_finalize(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *root)
{
	int err;
	err = ast_node_discover_potential_closures(
			ctx, NULL, true, root);

	ast_node_check_native_funcs(
			ctx, mod, root);

	type_id type;
	type = ast_dt_finalize_composite(ctx, mod,
			root, NULL, 0);

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
