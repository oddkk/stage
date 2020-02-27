#include "mod.h"
#include "../vm.h"
#include "../module.h"
#include "../native.h"
#include <stdlib.h>
#include <ffi.h>

static int64_t
print_int(int64_t val)
{
	printf("= %li\n", val);
	return val;
}

static void
func_unset_call()
{
	panic("Called an unset function.");
}

static int
stg_base_bootstrap_pre_compile(struct stg_module *mod) {
	assert(mod->id == 0);

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		base->name = STR("unset");

		struct type unset = {0};
		unset.name = atom_create(mod->atom_table, STR("unset"));
		unset.base = base;
		unset.size = 0;

		type_id unset_id = store_register_type(&mod->store, unset);

		assert(unset_id == TYPE_UNSET);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		base->name = STR("none");

		struct type none = {0};
		none.name = atom_create(mod->atom_table, STR("none"));
		none.base = base;
		none.size = 0;

		type_id none_id = store_register_type(&mod->store, none);
		assert(none_id == TYPE_NONE);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		base->name = STR("Unit");

		struct type unit = {0};
		unit.name = atom_create(mod->atom_table, STR("unit"));
		unit.base = base;
		unit.size = 0;
		unit.ffi_type = &ffi_type_void;

		mod->vm->default_types.unit = store_register_type(&mod->store, unit);
	}

	{
		struct func func_unset = {0};

		func_unset.native = (void *)func_unset_call;
		func_unset.type = stg_register_func_type(mod,
				mod->vm->default_types.unit, NULL, 0);

		func_id fid;
		fid = store_register_func(&mod->store, func_unset);
		assert(fid == 0);
	}

	base_bootstrap_register_type(mod);
	base_bootstrap_register_cons(mod);
	base_bootstrap_register_integer(mod);
	base_bootstrap_register_string(mod);

	return 0;
}

static int
stg_base_pre_compile(struct ast_context *ctx, struct stg_module *mod)
{
	struct stg_base_mod_info *info;
	info = calloc(1, sizeof(struct stg_base_mod_info));
	mod->data = info;

	struct ast_node *int_type_expr;
	struct object int_obj = {0};
	int_obj.type = ctx->types.type;
	int_obj.data = &ctx->types.integer;
	int_obj = register_object(ctx->vm, &mod->store, int_obj);

	int_type_expr = ast_init_node_lit(
			ctx, AST_NODE_NEW, STG_NO_LOC, int_obj);
	ast_namespace_add_decl(ctx, &mod->mod,
			mod->mod.root, mod_atoms(mod, "int"), int_type_expr);

	struct ast_node *type_type_expr;
	struct object type_obj = {0};
	type_obj.type = ctx->types.type;
	type_obj.data = &ctx->types.type;
	type_obj = register_object(ctx->vm, &mod->store, type_obj);

	type_type_expr = ast_init_node_lit(
			ctx, AST_NODE_NEW, STG_NO_LOC, type_obj);
	ast_namespace_add_decl(ctx, &mod->mod,
			mod->mod.root, mod_atoms(mod, "Type"), type_type_expr);

	struct ast_node *unit_type_expr;
	struct object unit_obj = {0};
	unit_obj.type = ctx->types.type;
	unit_obj.data = &ctx->types.unit;
	unit_obj = register_object(ctx->vm, &mod->store, unit_obj);

	unit_type_expr = ast_init_node_lit(
			ctx, AST_NODE_NEW, STG_NO_LOC, unit_obj);
	ast_namespace_add_decl(ctx, &mod->mod,
			mod->mod.root, mod_atoms(mod, "Unit"), unit_type_expr);

	struct ast_node *unit_inst_expr;
	struct object unit_inst = {0};
	unit_inst.type = ctx->types.unit;
	unit_inst.data = NULL;

	unit_inst_expr = ast_init_node_lit(
			ctx, AST_NODE_NEW, STG_NO_LOC, unit_inst);
	ast_namespace_add_decl(ctx, &mod->mod,
			mod->mod.root, mod_atoms(mod, "unit"), unit_inst_expr);


	struct ast_node *string_type_expr;
	struct object string_obj = {0};
	string_obj.type = ctx->types.type;
	string_obj.data = &ctx->types.string;
	string_obj = register_object(ctx->vm, &mod->store, string_obj);

	string_type_expr = ast_init_node_lit(
			ctx, AST_NODE_NEW, STG_NO_LOC, string_obj);
	ast_namespace_add_decl(ctx, &mod->mod,
			mod->mod.root, mod_atoms(mod, "String"), string_type_expr);


	base_init_register_cons(ctx, mod);
	base_init_register_init(ctx, mod);
	base_init_register_io(ctx, mod);

	return 0;
}

void
stg_base_load(struct vm *vm)
{
	struct stg_module *base_mod;
	base_mod = vm_request_module(vm,
			VM_REQUEST_PINNED,
			vm_atoms(vm, "base"),
			VM_REQUEST_MOD_NO_LOC);

	// We manually register the base_bootstrap module this early because it is
	// required for the compiler to function.
	stg_base_bootstrap_pre_compile(base_mod);

	struct stg_native_module *mod;
	mod = vm_add_precompiled_native_module(vm, STR("base"));

	base_integer_register_native(mod);
	base_init_register_native(mod);
	base_io_register_native(mod);
	stg_native_register_funcs(mod, print_int, STG_NATIVE_FUNC_IMPURE);

	mod->hook_pre_compile = stg_base_pre_compile;
}
