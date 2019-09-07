#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "modules/base/mod.h"

struct ast_context
ast_init_context(struct stg_error_context *err, struct atom_table *atom_table, struct vm *vm, type_id type, type_id integer)
{
	struct ast_context ctx;

	ctx.err = err;

	ctx.atoms.type                 = atom_create(atom_table, STR("Type"));

	ctx.atoms.func_cons_arg_ret    = atom_create(atom_table, STR("ret"));
	ctx.atoms.func_cons_arg_params = atom_create(atom_table, STR("params"));

	ctx.atoms.array_cons_arg_type  = atom_create(atom_table, STR("T"));
	ctx.atoms.array_cons_arg_count = atom_create(atom_table, STR("N"));

	ctx.types.type = type;
	ctx.types.integer = integer;

	ctx.vm = vm;

	{
		struct ast_object_def *array_type_def =
			ast_object_def_register(&vm->modules[0]->store);

		struct ast_object_def_param array_type_params[] = {
			{ctx.atoms.array_cons_arg_type, AST_SLOT_TYPE},
			{ctx.atoms.array_cons_arg_count,
				ast_bind_slot_const_type(
						&ctx, &array_type_def->env, AST_BIND_NEW,
						NULL, integer)},
		};

		ast_object_def_finalize(array_type_def,
				array_type_params, ARRAY_LENGTH(array_type_params),
				AST_SLOT_TYPE);

		ctx.cons.array = array_type_def;
	}

	{
		struct ast_object_def *func_type_def =
			ast_object_def_register(&vm->modules[0]->store);

		ast_slot_id func_params_type = ast_bind_slot_cons(
				&ctx, &func_type_def->env, AST_BIND_NEW,
				NULL, ctx.cons.array);

		ast_slot_id func_params_T =
			ast_unpack_arg_named(&ctx, &func_type_def->env,
					func_params_type, ctx.atoms.array_cons_arg_type);
		func_params_T = ast_bind_slot_templ(
				&ctx, &func_type_def->env, func_params_T,
				ctx.atoms.array_cons_arg_type, AST_SLOT_TYPE);

		ast_slot_id func_params_N =
			ast_unpack_arg_named(&ctx, &func_type_def->env,
					func_params_type, ctx.atoms.array_cons_arg_count);
		func_params_N = ast_bind_slot_templ(
				&ctx, &func_type_def->env, func_params_N, ctx.atoms.array_cons_arg_count,
				ast_bind_slot_const_type(&ctx, &func_type_def->env,
					ast_env_slot(&ctx, &func_type_def->env, func_params_N).type,
						NULL, integer));

		struct ast_object_def_param func_type_params[] = {
			{ctx.atoms.func_cons_arg_ret,    AST_SLOT_TYPE},
			{ctx.atoms.func_cons_arg_params, func_params_type},
		};

		ast_object_def_finalize(func_type_def,
				func_type_params, ARRAY_LENGTH(func_type_params),
				AST_SLOT_TYPE);

		ctx.cons.func = func_type_def;
	}

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

static int
ast_namespace_insert(struct ast_namespace *ns,
		struct ast_module_name value)
{
	int new_id = (int)ns->num_names;

	size_t new_num_names = ns->num_names + 1;
	struct ast_module_name *new_names;

	new_names = realloc(ns->names,
			new_num_names * sizeof(struct ast_module_name));

	if (!new_names) {
		return -1;
	}

	ns->names = new_names;
	ns->num_names = new_num_names;

	ns->names[new_id] = value;

	return new_id;
}

int
ast_namespace_add_decl(struct ast_context *ctx, struct ast_module *mod,
		struct ast_namespace *ns,
		struct atom *name, struct ast_node *expr)
{
	struct ast_module_name value = {0};

	value.kind = AST_MODULE_NAME_DECL;
	value.name = name;
	value.decl.expr = expr;

	value.decl.value = ast_bind_slot_wildcard(
			ctx, &mod->env, AST_BIND_NEW, NULL,
			ast_node_type(ctx, &mod->env, expr));

	int err;
	err = ast_namespace_insert(ns, value);
	return err >= 0 ? 0 : -1;
}

struct ast_namespace *
ast_namespace_add_ns(struct ast_context *ctx, struct ast_env *env,
		struct ast_namespace *ns, struct atom *name)
{
	struct ast_module_name value = {0};

	value.kind = AST_MODULE_NAME_NAMESPACE;
	value.name = name;
	value.ns = calloc(1, sizeof(struct ast_namespace));
	value.ns->name = name;
	value.ns->parent = ns;
	value.ns->instance = ast_bind_slot_cons(ctx, env,
			AST_BIND_NEW, NULL, NULL);

	int err;
	err = ast_namespace_insert(ns, value);

	if (err < 0) {
		return NULL;
	}

	return value.ns;
}

static struct type_base namespace_type_base = {
	.name = STR("namespace"),
	// TODO: Make repr functions.
	// TODO: Make unpack function.
};

static ast_slot_id
ast_namespace_finalize(struct ast_context *ctx,
		struct ast_module *mod, struct ast_namespace *ns)
{
	for (size_t i = 0; i < ns->num_names; i++) {
		if (ns->names[i].kind == AST_MODULE_NAME_NAMESPACE) {
			ast_namespace_finalize(ctx, mod,
					ns->names[i].ns);
		}
	}

	ns->def.env.store = mod->env.store;

	ns->def.num_params = ns->num_names;
	ns->def.params = calloc(ns->def.num_params,
			sizeof(struct ast_object_def_param));

	struct type ns_type_def = {0};

	ns_type_def.name = ns->name;
	ns_type_def.base = &namespace_type_base;
	// TODO: Do we have to store anything on the namespace?
	ns_type_def.size = 0;
	ns_type_def.data = &ns->def;

	type_id ns_type;
	ns_type = register_type(mod->env.store, ns_type_def);

	ns->def.ret_type = ast_bind_slot_const_type(
			ctx, &ns->def.env, AST_BIND_NEW, NULL,
			ns_type);

	// TODO: Ensure all types are resolved.

	for (size_t i = 0; i < ns->def.num_params; i++) {
		ns->def.params[i].name = ns->names[i].name;
		ns->def.params[i].type =
			ast_copy_slot(ctx,
					&ns->def.env, AST_BIND_NEW,
					&mod->env, ast_env_slot(ctx, &mod->env,
						ast_node_resolve_slot(&mod->env, &ns->names[i].decl.value)).type);
	}

	ast_slot_id instance;

	instance = ast_bind_slot_cons(
			ctx, &mod->env, ns->instance, NULL,
			&ns->def);

	for (size_t i = 0; i < ns->def.num_params; i++) {
		ast_slot_id arg;

		arg = ast_unpack_arg_named(ctx, &mod->env,
				instance, ns->names[i].name);

		// TODO: Ensure the value is evaluated.
		// TODO: Avoid having to alloc duplicate slots and join them.
		ast_union_slot(ctx, &mod->env, ns->names[i].decl.value, arg);
	}

	ns->instance = instance;

	return instance;
}

ast_slot_id
ast_module_add_dependency(struct ast_context *ctx,
		struct ast_module *mod, struct atom *name)
{
	for (size_t i = 0; i < mod->num_dependencies; i++) {
		if (name == mod->dependencies[i].name) {
			return mod->dependencies[i].slot;
		}
	}

	size_t dep_id = mod->num_dependencies;

	size_t tmp_num_deps;
	struct ast_module_dependency *tmp_deps;
	tmp_num_deps = mod->num_dependencies + 1;
	tmp_deps = realloc(mod->dependencies,
			sizeof(struct ast_module_dependency) * tmp_num_deps);
	if (!tmp_deps) {
		mod->num_dependencies -= 1;
		panic("Failed to allocate space for module dependencies.");
		return AST_BIND_FAILED;
	}

	mod->dependencies = tmp_deps;
	mod->num_dependencies = tmp_num_deps;

	memset(&mod->dependencies[dep_id], 0, sizeof(struct ast_module_dependency));
	mod->dependencies[dep_id].name = name;
	mod->dependencies[dep_id].slot =
		ast_bind_slot_cons(ctx, &mod->env, AST_BIND_NEW, name, NULL);

	return mod->dependencies[dep_id].slot;
}

ast_slot_id
ast_module_finalize(struct ast_context *ctx, struct ast_module *mod)
{
	return ast_namespace_finalize(ctx, mod, &mod->root);
}
