#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "dlist.h"
#include "module.h"
#include "base/mod.h"

struct ast_context
ast_init_context(struct stg_error_context *err, struct atom_table *atom_table, struct vm *vm)
{
	struct ast_context ctx;

	ctx.err = err;

	ctx.atoms.type                 = atom_create(atom_table, STR("Type"));

	ctx.atoms.func_cons_arg_ret    = atom_create(atom_table, STR("ret"));
	ctx.atoms.func_cons_arg_params = atom_create(atom_table, STR("params"));

	ctx.atoms.array_cons_arg_type  = atom_create(atom_table, STR("T"));
	ctx.atoms.array_cons_arg_count = atom_create(atom_table, STR("N"));

	ctx.types.type = vm->default_types.type;
	ctx.types.cons = vm->default_types.cons;
	ctx.types.integer = vm->default_types.integer;

	ctx.cons.func = vm->default_cons.func;
	ctx.cons.array = vm->default_cons.array;

	ctx.vm = vm;

	return ctx;
}

struct ast_object_def *
ast_object_def_register(struct objstore *store)
{
	struct ast_object_def *obj;

	obj = calloc(1, sizeof(struct ast_object_def));
	obj->env.store = store;

	return obj;
}

void
ast_object_def_finalize(struct ast_object_def *obj,
		struct ast_object_def_param *params, size_t num_params,
		ast_slot_id ret_type)
{
	assert(obj != NULL);

	obj->num_params = num_params;
	obj->params = calloc(sizeof(struct ast_object_def_param), num_params);
	memcpy(obj->params, params, sizeof(struct ast_object_def_param) * num_params);

	obj->ret_type = ret_type;
}

size_t
ast_object_def_num_descendant_members(
		struct ast_context *ctx, struct ast_module *mod,
		struct ast_object_def *def)
{
	size_t count = 0;
	count += def->num_params;

	for (size_t i = 0; i < def->num_params; i++) {
		int err;
		type_id mbr_type;

		ast_slot_id type_slot;
		type_slot = ast_env_slot(ctx, &def->env,
				def->params[i].slot).type;

		err = ast_slot_pack_type(ctx, mod,
				&def->env, type_slot, &mbr_type);
		if (err) {
			printf("Failed to pack cons param type.\n");
			continue;
		}

		struct type *member_type;
		member_type = vm_get_type(ctx->vm, mbr_type);

		if (member_type->obj_def) {
			count += ast_object_def_num_descendant_members(
					ctx, mod, member_type->obj_def);
		}
	}

	return count;
}

int
ast_slot_pack(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, ast_slot_id obj, struct object *out)
{
	struct ast_env_slot slot = ast_env_slot(ctx, env, obj);

	switch (slot.kind) {
		case AST_SLOT_ERROR:
		case AST_SLOT_PARAM:
		case AST_SLOT_TEMPL:
		case AST_SLOT_MEMBER:
		case AST_SLOT_CLOSURE:
		case AST_SLOT_WILDCARD:
			printf("Tried to pack slot of kind %s.\n",
					ast_slot_name(slot.kind));
			return -1;

		case AST_SLOT_CONST_TYPE:
			{
				struct object tmp_obj;
				tmp_obj.type = ctx->types.type;
				tmp_obj.data = &slot.const_type;

				*out = register_object(ctx->vm, env->store, tmp_obj);
				return 0;
			}

		case AST_SLOT_CONST:
			*out = slot.const_object;
			return 0;


		case AST_SLOT_CONS:
			if (!slot.cons.def) {
				// TODO: Try to resolve def from type.
				printf("Cons is missing a def.");
				return -1;
			}

			*out = slot.cons.def->pack(ctx, mod, env,
					slot.cons.def, obj);
			return 0;

		case AST_SLOT_CONS_ARRAY:
			{
				struct object array_type_obj;

				int err;
				err = ast_slot_pack(ctx, mod, env, slot.type, &array_type_obj);
				if (err) {
					printf("Failed to pack array type.");
					return -1;
				}

				type_id array_type_id;
				array_type_id = *(type_id *)array_type_obj.data;

				struct type *array_type;
				array_type = vm_get_type(ctx->vm, array_type_id);

				if (!array_type->base->array_def) {
					// TODO: Try to resolve def from type.
					printf("Cons array is missing a def.");
					return -1;
				}

				*out = array_type->base->array_def->pack(ctx, mod, env,
						array_type->base->array_def, obj);
			}
			return 0;

		case AST_SLOT_SUBST:
			return ast_slot_pack(ctx, mod, env, slot.subst, out);
	}

	panic("Got invalid slot kind in ast_slot_pack.");
	return -1;
}

int
ast_slot_pack_type(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, ast_slot_id obj, type_id *out)
{
	int err;
	struct object type_obj;
	err = ast_slot_pack(ctx, mod, env, obj, &type_obj);
	if (err) {
		return err;
	}

	assert_type_equals(ctx->vm, type_obj.type, ctx->types.type);

	*out = *(type_id *)type_obj.data;

	return 0;
}

int
ast_namespace_add_decl(struct ast_context *ctx, struct ast_module *mod,
		struct ast_node *ns, struct atom *name, struct ast_node *expr)
{
	int err;
	err = ast_node_composite_add_member(ctx, &mod->env,
			ns, name, NULL);
	if (err) {
		return err;
	}

	struct ast_node *target;
	target = ast_init_node_lookup(ctx, &mod->env,
			AST_NODE_NEW, STG_NO_LOC, name);

	ast_node_composite_bind(ctx, &mod->env, ns,
			target, expr, false);

	return 0;
}

void
ast_namespace_add_free_expr(struct ast_context *ctx, struct ast_module *mod,
		struct ast_node *ns, struct ast_node *expr)
{
	ast_node_composite_add_free_expr(ctx, &mod->env, ns, expr);
}


struct ast_node *
ast_namespace_add_ns(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *ns, struct atom *name)
{
	assert(ns->kind == AST_NODE_COMPOSITE);

	for (size_t i = 0; i < ns->composite.num_members; i++) {
		if (ns->composite.members[i].name == name) {
			return ns->composite.members[i].type;
		}
	}

	struct ast_node *ns_type;
	// TODO: Add a location to make error messages more helpful.
	ns_type = ast_init_node_composite(ctx, env,
			AST_NODE_NEW, STG_NO_LOC);

	int err;
	err = ast_node_composite_add_member(ctx, env,
			ns, name, ns_type);
	assert(!err);

	return ns_type;
}

void
ast_namespace_add_import(struct ast_context *ctx, struct ast_module *mod,
		struct ast_node *ns, struct atom *name)
{
	ast_module_add_dependency(ctx, mod, ns, name);
}


void
ast_namespace_use(struct ast_context *ctx,
		struct ast_module *mod, struct ast_node *ns,
		ast_slot_id object)
{
	panic("TODO: Implement use\n");
	/*
	struct ast_env_slot slot = ast_env_slot(ctx, &mod->env, object);

	assert(slot.kind == AST_SLOT_CONS);

	dlist_append(ns->used_objects, ns->num_used_objects, &object);
	*/
}

void
ast_module_add_dependency(struct ast_context *ctx,
		struct ast_module *mod, struct ast_node *container, struct atom *name)
{
	for (size_t i = 0; i < mod->num_dependencies; i++) {
		if (name == mod->dependencies[i].name) {
			panic("Module %.*s imported multiple times.",
					ALIT(name));
			return;
		}
	}

	struct ast_module_dependency new_dep = {0};

	new_dep.name = name;
	new_dep.container = container;

	size_t dep_id;
	dep_id = dlist_append(
			mod->dependencies,
			mod->num_dependencies,
			&new_dep);
}

void
ast_module_resolve_dependencies(struct ast_context *ctx,
		struct ast_module *mod)
{
	for (size_t i = 0; i < mod->num_dependencies; i++) {
		struct ast_module_dependency *dep;
		dep = &mod->dependencies[i];

		struct ast_node *type;
		type = ast_init_node_slot(ctx, &mod->env,
				AST_NODE_NEW, STG_NO_LOC,
				ast_bind_slot_const_type(ctx, &mod->env,
					AST_BIND_NEW, NULL, dep->mod->type));

		assert(dep->mod->type != TYPE_UNSET);

		int err;
		err = ast_node_composite_add_member(ctx, &mod->env,
				dep->container, dep->name, type);
		assert(!err);
	}
}

int
ast_module_finalize(struct ast_context *ctx, struct ast_module *mod)
{
	int err;
	err = ast_node_discover_potential_closures(ctx, &mod->env,
			NULL, true, mod->root);

	ast_print(ctx, &mod->env,
			mod->root);

	type_id type;
	type = ast_dt_finalize_composite(ctx, mod,
			&mod->env, mod->root);

	mod->type = type;

	/*
	printf("Final type for %.*s: ", LIT(mod->stg_mod->info.name));
	print_type_repr(ctx->vm, vm_get_type(ctx->vm, type));
	printf("\n");
	*/

	return 0;
}
