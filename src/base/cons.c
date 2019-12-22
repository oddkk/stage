#include "mod.h"
#include "../module.h"
#include "../ast.h"
#include <ffi.h>

struct type_base base_cons_base = {
	.name = STR("cons"),
};

void
base_bootstrap_register_cons(struct stg_module *mod)
{
	struct type t;
	t = init_plain_type(
			&base_cons_base,
			mod_atoms(mod, "cons"),
			struct ast_object_def *);
	// This type should not be used in runtime expressions.
	// t.ffi_type = &ffi_type_pointer;

	type_id tid;
	tid = stg_register_type(mod, t);

	mod->vm->default_types.cons = tid;
}

void
base_init_register_cons(struct ast_context *ctx, struct stg_module *mod)
{
	struct object array_type_cons_obj;
	array_type_cons_obj.type = mod->vm->default_types.cons;
	array_type_cons_obj.data = &ctx->cons.array;
	array_type_cons_obj = register_object(
			mod->vm, mod->mod.env.store, array_type_cons_obj);

	struct ast_node *array_type_cons;
	array_type_cons = ast_init_node_lit(ctx,
			AST_NODE_NEW, STG_NO_LOC, array_type_cons_obj);
	ast_namespace_add_decl(ctx, &mod->mod,
			mod->mod.root, mod_atoms(mod, "Array"), array_type_cons);
}
