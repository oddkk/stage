#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "module.h"
#include "base/mod.h"
#include "dlist.h"

ast_slot_id
ast_node_resolve_slot(struct ast_env *env, ast_slot_id *slot)
{
	assert(*slot < (ast_slot_id)env->num_slots);
	while (*slot >= 0 && env->slots[*slot].kind == AST_SLOT_SUBST) {
		assert(*slot < (ast_slot_id)env->num_slots);
		*slot = env->slots[*slot].subst;
	}

	return *slot;
}

struct ast_node *
ast_init_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct atom **param_names, size_t num_params)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_FUNC_UNINIT;
	node->loc = loc;

	node->func.params = calloc(sizeof(struct ast_func_param), num_params);
	// memcpy(node->func.params, params, sizeof(struct ast_func_param) * num_params);
	node->func.num_params = num_params;

	ast_slot_id param_types[num_params];

	for (size_t i = 0; i < num_params; i++) {
		node->func.params[i].name = param_names[i];
		node->func.params[i].slot = ast_bind_slot_param(
				ctx, env, AST_BIND_NEW,
				param_names[i], i,
				ast_bind_slot_wildcard(
					ctx, env, AST_BIND_NEW, NULL, AST_SLOT_TYPE));
		param_types[i] = ast_env_slot(
				ctx, env, node->func.params[i].slot).type;
	}

	node->func.type = ast_bind_slot_cons(ctx, env, AST_BIND_NEW,
			NULL, ctx->cons.func);

	node->func.return_type_slot =
		ast_unpack_arg_named(ctx, env,
			node->func.type,
			ctx->atoms.func_cons_arg_ret);

	node->func.return_type_slot =
		ast_bind_slot_wildcard(ctx, env,
				node->func.return_type_slot,
				NULL, AST_SLOT_TYPE);

	ast_slot_id param_types_slot =
		ast_unpack_arg_named(ctx, env,
			node->func.type,
			ctx->atoms.func_cons_arg_params);

	param_types_slot = ast_bind_slot_cons_array(
			ctx, env, param_types_slot, NULL,
			param_types, num_params,
			AST_SLOT_TYPE);

	return node;
}

struct ast_node *
ast_finalize_node_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node,
		struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct ast_node *body)
{
	assert(node != NULL && node != AST_NODE_NEW && node->kind == AST_NODE_FUNC_UNINIT);

	assert(
		node &&
		(param_types || num_params == 0) &&
		node->func.num_params == num_params &&
		body
	);

	node->kind = AST_NODE_FUNC;
	node->func.body = body;
	node->func.return_type = return_type;

	if (node->func.return_type) {
		ast_union_slot(ctx, env,
				ast_node_type(ctx, env, node->func.body),
				ast_node_value(ctx, env, node->func.return_type));
	} else {
		node->func.return_type = ast_init_node_slot(ctx, env,
				calloc(sizeof(struct ast_node), 1),
				node->func.body->loc,
				ast_node_type(ctx, env, node->func.body));

	}

	ast_union_slot(ctx, env,
			ast_node_value(ctx, env, node->func.return_type),
			ast_node_resolve_slot(env, &node->func.return_type_slot));

	for (size_t i = 0; i < num_params; i++) {
		node->func.params[i].type = param_types[i];
		ast_union_slot(ctx, env,
				ast_env_slot(ctx, env, node->func.params[i].slot).type,
				ast_node_value(ctx, env, node->func.params[i].type));
	}

	return node;
}

struct ast_node *
ast_finalize_node_func_native(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct ast_node **param_types, size_t num_params,
		struct ast_node *return_type, struct string native_func_name)
{
	assert(node != NULL && node != AST_NODE_NEW && node->kind == AST_NODE_FUNC_UNINIT);

	assert(
		node &&
		(param_types || num_params == 0) &&
		node->func.num_params == num_params &&
		return_type
	);

	node->kind = AST_NODE_FUNC_NATIVE;
	node->func.native.name = native_func_name;
	node->func.native.func = NULL;
	node->func.return_type = return_type;

	ast_union_slot(ctx, env,
			ast_node_value(ctx, env, node->func.return_type),
			ast_node_resolve_slot(env, &node->func.return_type_slot));

	for (size_t i = 0; i < num_params; i++) {
		node->func.params[i].type = param_types[i];
		ast_union_slot(ctx, env,
				ast_env_slot(ctx, env, node->func.params[i].slot).type,
				ast_node_value(ctx, env, node->func.params[i].type));
	}

	return node;
}

struct ast_node *
ast_init_node_call(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct ast_node *func,
		struct ast_func_arg *args, size_t num_args)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(
		node &&
		(args || num_args == 0)
	);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_CALL;
	node->loc = loc;

	node->call.func = func;
	node->call.args = calloc(sizeof(struct ast_func_arg), num_args);
	memcpy(node->call.args, args, sizeof(struct ast_func_arg) * num_args);
	node->call.num_args = num_args;
	node->call.cons = AST_SLOT_NOT_FOUND;

	node->call.ret_type =
		ast_bind_slot_wildcard(ctx, env, AST_BIND_NEW,
				NULL, AST_SLOT_TYPE);

	return node;
}

struct ast_node *
ast_init_node_slot(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		ast_slot_id slot)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_SLOT;
	node->loc = loc;

	node->slot = slot;

	return node;
}

struct ast_node *
ast_init_node_lookup(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node, struct stg_location loc,
		struct atom *name, ast_slot_id slot)
{
	if (node == AST_NODE_NEW) {
		node = calloc(sizeof(struct ast_node), 1);
	}

	assert(node);

	memset(node, 0, sizeof(struct ast_node));
	node->kind = AST_NODE_LOOKUP;
	node->loc = loc;

	node->lookup.name = name;
	node->lookup.slot = ast_bind_slot_wildcard(
			ctx, env, slot, NULL, AST_BIND_NEW);
	node->lookup.value = AST_SLOT_NOT_FOUND;

	return node;
}

ast_slot_id
ast_node_func_register_templ_param(
		struct ast_context *ctx, struct ast_env *env,
		struct ast_node *func, struct atom *name,
		struct stg_location loc, ast_slot_id type_slot)
{
	if (!func) {
		stg_error(ctx->err, loc,
				"Template parameters can only be declared inside functions.");
		return AST_BIND_FAILED;
	}
	assert(func->kind == AST_NODE_FUNC ||
			func->kind == AST_NODE_FUNC_UNINIT);
	for (size_t i = 0; i < func->func.num_template_params; i++) {
		if (func->func.template_params[i].name == name) {
			stg_error(ctx->err, loc,
					"Template parameter '%.*s' has already been declared.",
					ALIT(name));
			stg_appendage(ctx->err, func->func.template_params[i].loc, "Here.");
			return func->func.template_params[i].slot;
		}
	}

	struct ast_func_template_param tmpl_param = {0};

	tmpl_param.name = name;
	tmpl_param.loc = loc;
	tmpl_param.slot =
		ast_bind_slot_templ(ctx, env, AST_BIND_NEW, name,
				ast_bind_slot_wildcard(
					ctx, env, type_slot,
					NULL, AST_SLOT_TYPE));

	dlist_append(
			func->func.template_params,
			func->func.num_template_params,
			&tmpl_param);

	return tmpl_param.slot;
}

ast_slot_id
ast_node_type(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_FUNC_UNINIT:
		case AST_NODE_FUNC_NATIVE:
		case AST_NODE_FUNC:
			return ast_node_resolve_slot(env, &node->func.type);

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			return node->call.ret_type;

		case AST_NODE_SLOT:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->slot)).type;

		case AST_NODE_LOOKUP:
			return ast_env_slot(ctx, env,
					ast_node_resolve_slot(env, &node->lookup.slot)).type;
	}

	panic("Invalid ast node.");
	return AST_BIND_FAILED;
}

ast_slot_id
ast_node_value(struct ast_context *ctx, struct ast_env *env, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_SLOT:
			return ast_node_resolve_slot(env, &node->slot);

		case AST_NODE_LOOKUP:
			return ast_node_resolve_slot(env, &node->lookup.slot);

		case AST_NODE_FUNC_NATIVE:
		case AST_NODE_FUNC:
		case AST_NODE_CALL:
			panic("TODO: eval");
			break;

		case AST_NODE_CONS:
			return ast_node_resolve_slot(env, &node->call.cons);

		case AST_NODE_FUNC_UNINIT:
			panic("Attempted to resolve value of uninitialized func.");
			break;
	}

	panic("Invalid ast node.");
	return AST_BIND_FAILED;
}

enum ast_node_dependencies_state
ast_node_dependencies_fulfilled(struct ast_context *ctx,
		struct ast_env *env, struct ast_node *node)
{
	enum ast_node_dependencies_state result = AST_NODE_DEPS_OK;
	switch (node->kind) {
		case AST_NODE_FUNC:
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->func.body);
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->func.return_type);
			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->func.params[i].type);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->func.return_type);
			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->func.params[i].type);
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			result &= ast_node_dependencies_fulfilled(
					ctx, env, node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				result &= ast_node_dependencies_fulfilled(
						ctx, env, node->call.args[i].value);
			}
			break;

		case AST_NODE_SLOT:
			break;

		case AST_NODE_LOOKUP:
			if (node->lookup.value == AST_SLOT_NOT_FOUND) {
				result = AST_NODE_DEPS_NOT_OK;
			} else {
				struct ast_env_slot slot =
					ast_env_slot(ctx, env,
							ast_node_resolve_slot(env, &node->lookup.value));

				if (slot.kind == AST_SLOT_CONST ||
						slot.kind == AST_SLOT_CONST_TYPE ||
						slot.kind == AST_SLOT_PARAM ||
						slot.kind == AST_SLOT_TEMPL) {
					node->kind = AST_NODE_SLOT;
					node->slot =
						ast_union_slot(ctx, env,
								node->lookup.value, node->lookup.slot);
				} else {
					result = AST_NODE_DEPS_NOT_READY;
				}
			}
			break;

		case AST_NODE_FUNC_UNINIT:
			panic("Encountered uninitialized func in dependency check.");
			return AST_NODE_DEPS_NOT_OK;
	}

	return result;
}

static bool
ast_slot_is_resolved(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id slot_id)
{
	struct ast_env_slot slot;
	slot = ast_env_slot(ctx, env, slot_id);

	bool result = true;

	switch (slot.kind) {
	case AST_SLOT_ERROR:
		return false;

	case AST_SLOT_WILDCARD:
		return false;

	case AST_SLOT_CONST_TYPE:
		assert(slot.type == AST_SLOT_TYPE);
		return true;

	case AST_SLOT_CONST:
		assert(ast_slot_is_resolved(ctx, env, slot.type));
		return true;

	case AST_SLOT_PARAM:
		return ast_slot_is_resolved(ctx, env, slot.type);

	case AST_SLOT_TEMPL:
		return ast_slot_is_resolved(ctx, env, slot.type);

	case AST_SLOT_CONS:
		result &= !!slot.cons.def;
		result &= ast_slot_is_resolved(ctx, env, slot.type);

		for (size_t i = 0; i < slot.cons.num_present_args; i++) {
			result &= ast_slot_is_resolved(ctx, env, slot.cons.args[i].slot);
		}

		return result;

	case AST_SLOT_CONS_ARRAY:
		result &= ast_slot_is_resolved(ctx, env, slot.cons_array.member_type);
		result &= ast_slot_is_resolved(ctx, env, slot.cons_array.member_count);
		result &= ast_slot_is_resolved(ctx, env, slot.type);

		for (size_t i = 0; i < slot.cons_array.num_members; i++) {
			result &= ast_slot_is_resolved(ctx, env, slot.cons_array.members[i]);
		}

		return result;

	case AST_SLOT_SUBST:
		return ast_slot_is_resolved(ctx, env, slot.subst);
	}

	panic("Invalid slot in ast_slot_is_resolved");
	return false;
}

bool
ast_node_is_typed(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *node)
{
	bool result = true;

	switch (node->kind) {
		case AST_NODE_FUNC:
			result &= ast_node_is_typed(ctx, env,
					node->func.body);
			result &= ast_node_is_typed(ctx, env,
					node->func.return_type);
			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->func.params[i].type);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			result &= ast_node_is_typed(ctx, env,
					node->func.return_type);
			for (size_t i = 0; i < node->func.num_params; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->func.params[i].type);
			}
			break;

		case AST_NODE_CALL:
		case AST_NODE_CONS:
			result &= ast_node_is_typed(ctx, env,
					node->call.func);
			for (size_t i = 0; i < node->call.num_args; i++) {
				result &= ast_node_is_typed(ctx, env,
						node->call.args[i].value);
			}

			result &= ast_slot_is_resolved(ctx, env, node->call.ret_type);

			if (node->kind == AST_NODE_CONS) {
				if (!ast_slot_is_resolved(ctx, env, node->call.cons)) {
					stg_error(ctx->err, node->loc,
							"Failed to resolve constructor.");
					result = false;
				}
			}
			break;

		case AST_NODE_SLOT:
			result &= ast_slot_is_resolved(ctx, env, node->slot);
			if (!result) {
				stg_error(ctx->err, node->loc,
						"Failed to resolve expression.");
			}
			break;

		case AST_NODE_LOOKUP:
			result = false;
			printf("Lookup node still found during is typed check.\n");
			break;

		case AST_NODE_FUNC_UNINIT:
			panic("Encountered uninitialized func in dependency check.");
			return false;
	}

	return result;
}

bool
ast_node_resolve_slots(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node)
{
	bool result = true;

	switch (node->kind) {
		case AST_NODE_FUNC:
			if (node->func.num_template_params == 0) {
				ast_node_resolve_slots(ctx, mod, env, node->func.return_type);

				for (size_t i = 0; i < node->func.num_params; i++) {
					ast_node_resolve_slots(ctx, mod, env,
							node->func.params[i].type);
				}

				ast_node_resolve_slots(ctx, mod, env, node->func.body);
			}
			break;

		case AST_NODE_FUNC_NATIVE:
			assert(node->func.num_template_params == 0);
			ast_node_resolve_slots(ctx, mod, env, node->func.return_type);

			for (size_t i = 0; i < node->func.num_params; i++) {
				ast_node_resolve_slots(ctx, mod, env,
						node->func.params[i].type);
			}
			break;

		case AST_NODE_CALL:
			ast_node_resolve_slots(ctx, mod, env, node->call.func);

			for (size_t i = 0; i < node->call.num_args; i++) {
				ast_node_resolve_slots(ctx, mod, env,
						node->call.args[i].value);
			}

			{
				ast_slot_id func_type_slot;
				func_type_slot = ast_node_type(
						ctx, env, node->call.func);

				struct object func_type_obj;
				int err;
				err = ast_slot_pack(ctx, mod, env,
						ast_node_type(ctx, env, node->call.func),
						&func_type_obj);

				assert_type_equals(ctx->vm, ctx->types.type, func_type_obj.type);
				type_id func_type = *(type_id *)func_type_obj.data;

				if (func_type != ctx->types.cons) {
					// If the type is not a cons, we expect it to be a
					// function. Bind the func to the arguments appropriatly.
					ast_slot_id arg_type_ids[node->call.num_args];

					for (size_t i = 0; i < node->call.num_args; i++) {
						arg_type_ids[i] = ast_node_type(
								ctx, env, node->call.args[i].value);
					}

					func_type_slot = ast_bind_slot_cons(
							ctx, env, func_type_slot,
							NULL, ctx->cons.func);

					ast_slot_id ret_type;
					ret_type = ast_unpack_arg_named(
							ctx, env, func_type_slot,
							ctx->atoms.func_cons_arg_ret);

					ret_type = ast_bind_slot_wildcard(ctx, env,
							ret_type, NULL, AST_SLOT_TYPE);

					node->call.ret_type =
						ast_union_slot(ctx, env, node->call.ret_type, ret_type);


					ast_slot_id param_types = ast_unpack_arg_named(ctx, env,
							func_type_slot, ctx->atoms.func_cons_arg_params);

					param_types = ast_bind_slot_cons_array(
							ctx, env, param_types, NULL,
							arg_type_ids, node->call.num_args,
							AST_SLOT_TYPE);

					break;
				} else {
					node->kind = AST_NODE_CONS;
					node->call.cons = AST_BIND_NEW;
				}
			}
			// fallthrough

		case AST_NODE_CONS:
			{
				assert(node->kind == AST_NODE_CONS &&
						node->call.cons != AST_SLOT_NOT_FOUND);

				struct ast_object_def *cons;

				struct object func_obj;
				int err;

				err = ast_node_eval(ctx, mod, env,
						node->call.func, &func_obj);
				assert_type_equals(ctx->vm, ctx->types.cons, func_obj.type);

				cons = *(struct ast_object_def **)func_obj.data;

				node->kind = AST_NODE_CONS;

				node->call.cons = ast_bind_slot_cons(ctx, env,
						node->call.cons, NULL, cons);

				node->call.ret_type = ast_union_slot(ctx, env,
						ast_env_slot(ctx, env, node->call.cons).type,
						node->call.ret_type);

				struct ast_env_slot cons_slot;
				cons_slot = ast_env_slot(ctx, env, node->call.cons);

				assert(cons_slot.kind == AST_SLOT_CONS);
				assert(cons_slot.cons.num_present_args == cons->num_params);

				if (node->call.num_args != cons->num_params) {
					stg_error(ctx->err, node->loc,
							"Expected %zu arguments to constructor, got %zu.",
							cons->num_params, node->call.num_args);
					return false;
				}

				for (size_t i = 0; i < node->call.num_args; i++) {
					struct atom *param_name;
					param_name = cons->params[i].name;

					ast_slot_id cons_arg_slot;
					cons_arg_slot = ast_unpack_arg_named(ctx, env,
							node->call.cons, param_name);

					ast_slot_id node_arg_slot;
					node_arg_slot = ast_node_value(ctx, env,
							node->call.args[i].value);

					ast_union_slot(ctx, env, cons_arg_slot, node_arg_slot);
				}

			}
			break;

		case AST_NODE_SLOT:
			break;

		case AST_NODE_LOOKUP:
			if (node->lookup.value == AST_SLOT_NOT_FOUND) {
				result = false;
			} else {
				struct ast_env_slot slot =
					ast_env_slot(ctx, env, node->lookup.value);

				if (slot.kind == AST_SLOT_CONST ||
						slot.kind == AST_SLOT_CONST_TYPE) {
					node->kind = AST_NODE_SLOT;
					node->slot =
						ast_union_slot(ctx, env,
								node->lookup.value, node->lookup.slot);
				} else {
					result = false;
				}
			}
			break;

		case AST_NODE_FUNC_UNINIT:
			panic("Encountered uninitialized func in dependency check.");
			return false;
	}

	return result;
}

static int
ast_node_eval_type(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node, type_id *out)
{
	struct object type_obj;
	int err;
	err = ast_node_eval(ctx, mod, env, node, &type_obj);
	if (err) {
		return err;
	}

	assert_type_equals(ctx->vm, type_obj.type, ctx->types.type);

	*out = *(type_id *)type_obj.data;
	return 0;
}

int
ast_node_eval(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node, struct object *out)
{
	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
			{
				if (node->func.instance == FUNC_UNSET) {
					struct func func = {0};

					type_id ret_type;
					type_id param_types[node->func.num_params];
					int err;

					err = ast_node_eval_type(ctx, mod, env,
							node->func.return_type, &ret_type);
					if (err) {
						printf("Failed to resolve function return type.\n");
						return -1;
					}

					for (size_t i = 0; i < node->func.num_params; i++) {
						err = ast_node_eval_type(ctx, mod, env,
								node->func.return_type, &param_types[i]);
						if (err) {
							printf("Failed to resolve function parameter.\n");
							return -1;
						}
					}

					func.type = stg_register_func_type(mod->stg_mod, ret_type,
							param_types, node->func.num_params);

					if (node->kind == AST_NODE_FUNC) {
						struct bc_env *bc_env;

						bc_env = ast_func_gen_bytecode(
								ctx, mod, env, node);
						assert(bc_env);

						func.kind = FUNC_BYTECODE;
						func.bytecode = bc_env;
					} else if (node->kind == AST_NODE_FUNC_NATIVE) {
						func.kind = FUNC_NATIVE;
						func.name = vm_atom(ctx->vm, node->func.native.name);
						func.native = node->func.native.func;
					}

					node->func.instance =
						stg_register_func(mod->stg_mod, func);
				}

				struct object func_type_obj;
				int err;
				err = ast_slot_pack(ctx, mod, env, node->func.type, &func_type_obj);
				if (err) {
					printf("Falied to pack func type.\n");
					return -1;
				}

				assert_type_equals(ctx->vm, func_type_obj.type, ctx->types.type);
				type_id func_type = *(type_id *)func_type_obj.data;

				struct stg_func_object func_obj_data = {0};

				func_obj_data.func = node->func.instance;

				assert(vm_get_type(ctx->vm, func_type)->size == sizeof(struct stg_func_object));

				struct object func_obj = {0};
				func_obj.type = func_type;
				func_obj.data = &func_obj_data;

				*out = register_object(ctx->vm, env->store, func_obj);
				return 0;
			}
			break;

		case AST_NODE_CALL:
			{
				struct object args[node->call.num_args];
				struct object func = {0};

				// TODO: Prealloc the space for the argument values to avoid
				// pushing to the arena.

				memset(args, 0, sizeof(struct object) * node->call.num_args);

				int err;

				err = ast_node_eval(ctx, mod, env, node->call.func, &func);
				if (err) {
					return -1;
				}

				for (size_t i = 0; i < node->call.num_args; i++) {
					err = ast_node_eval(ctx, mod, env, node->call.args[i].value, &args[i]);
					if (err) {
						return -1;
					}
				}

				// struct type *type = vm_get_type(ctx->vm, func.type);
				// TODO: assert(type->base == func_type_base)

				struct stg_func_object *func_obj = (struct stg_func_object  *)func.data;
				struct func *func_inst = vm_get_func(ctx->vm, func_obj->func);
				struct type *func_type = vm_get_type(ctx->vm, func_inst->type);
				struct stg_func_type *func_info = (struct stg_func_type *)func_type->data;;
				struct type *ret_type  = vm_get_type(ctx->vm, func_info->return_type);

				struct object res = {0};
				uint8_t buffer[ret_type->size];
				res.type = func_info->return_type;
				res.data = buffer;

				err = vm_call_func(ctx->vm, func_obj->func,
						args, node->call.num_args, &res);
				if (err) {
					return err;
				}

				*out = register_object(ctx->vm, env->store, res);
				return err;
			}
			break;

		case AST_NODE_CONS:
			{
				struct object cons_obj;
				int err;
				err = ast_node_eval(ctx, mod, env, node->call.func, &cons_obj);
				if (err) {
					return -1;
				}

				assert_type_equals(ctx->vm, ctx->types.cons, cons_obj.type);

				struct ast_object_def *cons;
				cons = *(struct ast_object_def **)cons_obj.data;
				assert(cons);

				*out = cons->pack(ctx, mod, env,
						cons, node->call.cons);

				return 0;
			}
			break;

		case AST_NODE_SLOT:
			return ast_slot_pack(ctx, mod, env, node->slot, out);

		case AST_NODE_LOOKUP:
			if (node->lookup.value == AST_SLOT_NOT_FOUND) {
				printf("Lookup was not resolved.\n");
				return -1;
			}
			return ast_slot_pack(ctx, mod, env, node->lookup.value, out);


		case AST_NODE_FUNC_UNINIT:
			break;

	}

	panic("Invalid node in ast_node_eval");
	return -1;
}
