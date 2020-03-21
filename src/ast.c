#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "dlist.h"
#include "module.h"
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
			ctx, AST_NODE_NEW, STG_NO_LOC);

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
ast_module_finalize(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *root)
{
	int err;
	err = ast_node_discover_potential_closures(
			ctx, NULL, true, root);

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
