#include "compile.h"
#include "syntax_tree.h"
#include "utils.h"
#include "dlist.h"
#include "ast.h"
#include "objstore.h"
#include "errors.h"
#include "term_color.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>

struct string complie_job_names[] = {
#define COMPILE_JOB(name, data) STR(#name),
#include "compile_job_defs.h"
#undef COMPILE_JOB
};

#define COMPILE_JOB(name, data) typedef data job_##name##_t;
#include "compile_job_defs.h"
#undef COMPILE_JOB

enum job_status_code {
	JOB_STATUS_OK = 0,
	JOB_STATUS_ERROR = -1,
	JOB_STATUS_YIELD = 1,
	JOB_STATUS_YIELD_FOR_PHASE = 2,
	JOB_STATUS_IDLE = 3,
};

struct complie_job {
	size_t id;
	size_t last_dispatch_time;
	enum complie_job_type type;
	enum job_status_code status;

	struct complie_job *next_job;
	struct complie_job *first_dependant_node;

	struct stg_module *mod;

#define COMPILE_JOB(name, data) job_##name##_t name;
	union {
#include "compile_job_defs.h"
	};
#undef COMPILE_JOB
};

enum compile_phase_name {
	COMPILE_PHASE_DISCOVER = 0,
	COMPILE_PHASE_RESOLVE,

	COMPILE_NUM_PHASES
};

struct job_status {
	enum job_status_code status;
	struct complie_job *yield_for;
	enum compile_phase_name yield_for_phase;
};

#define JOB_OK  ((struct job_status){.status=JOB_STATUS_OK})
#define JOB_ERROR ((struct job_status){.status=JOB_STATUS_ERROR})
#define JOB_YIELD ((struct job_status){.status=JOB_STATUS_YIELD, .yield_for=NULL})
#define JOB_YIELD_FOR(job) ((struct job_status){.status=JOB_STATUS_YIELD, .yield_for=(job)})
#define JOB_YIELD_FOR_PHASE(phase) ((struct job_status){.status=JOB_STATUS_YIELD_FOR_PHASE, .yield_for_phase=(phase)})

struct compile_phase {
	struct complie_job *job_head;
	struct complie_job *job_end;
};

#define COMPILE_JOB_MAX_CONSECUTIVE_YIELDS (3)

struct compile_ctx {
	struct vm *vm;
	struct arena *mem;

	struct ast_context *ast_ctx;

	size_t next_job_id;
	// A measure of how many times jobs have yielded.
	size_t time;
	size_t last_completed_job_time;

	enum compile_phase_name current_phase;
	struct compile_phase phases[COMPILE_NUM_PHASES];

	// struct string *file_names;
	// size_t num_files;

	// size_t num_errors;
	size_t num_jobs_failed;

	struct stg_error_context err;
};

static void
append_job(struct compile_ctx *ctx, enum compile_phase_name ph, struct complie_job *job)
{
	assert(ph < COMPILE_NUM_PHASES);
	assert(ph >= ctx->current_phase);

	struct compile_phase *phase = &ctx->phases[ph];

	if (phase->job_end) {
		assert(phase->job_end->next_job == NULL);
		assert(job->next_job == NULL);
		assert(phase->job_end != job);

		phase->job_end->next_job = job;
		phase->job_end = job;

		if (!phase->job_head) {
			phase->job_head = job;
		}
	} else {
		assert(!phase->job_head);
		phase->job_end = job;
		phase->job_head = job;
	}
}

static struct complie_job *
dispatch_job(struct compile_ctx *ctx, enum compile_phase_name phase, struct complie_job job)
{
	struct complie_job *new_job = calloc(1, sizeof(struct complie_job));
	*new_job = job;
	new_job->status = JOB_STATUS_IDLE;
	new_job->next_job = NULL;
	new_job->id = ctx->next_job_id;
	ctx->next_job_id += 1;

	append_job(ctx, phase, new_job);

	return new_job;
}

#define DISPATCH_JOB(ctx, _mod, name, phase, ...)				\
	dispatch_job((ctx), (phase),								\
				 (struct complie_job){								\
					 .type=COMPILE_JOB_##name,						\
					 .mod = _mod,								\
					 .name = (job_##name##_t){__VA_ARGS__}		\
				 })

/*
static struct scope *
instantiate_scope_by_access_pattern(struct compile_ctx *ctx,
									struct stg_module *mod,
									struct scope *parent,
									struct st_node *node)
{
	switch (node->type) {
	case ST_NODE_ACCESS: {
		struct scope *scope;

		scope = instantiate_scope_by_access_pattern(ctx, mod,parent, node->ACCESS.lhs);
		if (!scope) {
			return NULL;
		}

		return instantiate_scope_by_access_pattern(ctx, mod,scope, node->ACCESS.rhs);
	} break;

	case ST_NODE_IDENT: {
		struct scope *scope;
		struct scope_entry entry;

		if (scope_local_lookup(parent, node->IDENT, &entry) == 0) {
			if (entry.anchor != SCOPE_ANCHOR_NONE || !entry.scope) {
				stg_error(&ctx->err, node->loc,
					"'%.*s' already exists, and is not a namespace.",
					ALIT(node->IDENT)
				);
				return NULL;
			}
		}

		scope = scope_push(parent);
		scope_insert(parent, node->IDENT, SCOPE_ANCHOR_NONE,
					 OBJ_NONE, scope);
		return scope;
	} break;

	default:
		panic("Invalid node in access pattern.");
		break;
	}

	return NULL;
}
*/

static void dispatch_stmt(struct compile_ctx *ctx, struct stg_module *mod,
						  struct ast_namespace *ns, struct st_node *stmt)
{
	assert(stmt->type == ST_NODE_STMT);

	// TODO: Handle attributes

	struct st_node *node = stmt->STMT.stmt;

	if (!node) {
		return;
	}

	switch (node->type) {

		/*
	case ST_NODE_DECL_STMT:
		DISPATCH_JOB(ctx, mod, visit_decl_stmt, COMPILE_PHASE_DISCOVER,
					 .scope = scope,
					 .stmt = node);
		break;
		*/

	case ST_NODE_USE_STMT:
		/*
		DISPATCH_JOB(ctx, mod, use_stmt, COMPILE_PHASE_DISCOVER,
					 .ns = ns,
					 .node = node);
		*/
		break;

	case ST_NODE_ASSERT_STMT:
		/*
		DISPATCH_JOB(ctx, mod, assert_stmt, COMPILE_PHASE_DISCOVER,
					 .ns = ns,
					 .node = node);
		*/
		break;

	case ST_NODE_ASSIGN_STMT:
		DISPATCH_JOB(ctx, mod, assign_stmt, COMPILE_PHASE_DISCOVER,
					 .ns = ns,
					 .node = node);
		break;

	default:
		panic("Invalid node '%.*s' as statement.",
				LIT(st_node_names[node->type]));
		break;
	}
}

struct st_tuple_members {
	struct atom **names;
	struct st_node **types;
	size_t num_members;
};

static void
st_node_unpack_tuple_nodes(struct st_node *tuple_node,
		struct st_tuple_members *out_members)
{
	assert(tuple_node->type == ST_NODE_TUPLE_DECL);

	struct st_tuple_members members = {0};

	for (struct st_node *param = tuple_node->TUPLE_DECL.items;
			param; param = param->next_sibling) {
		members.num_members += 1;
	}

	members.names = calloc(members.num_members, sizeof(struct atom *));
	members.types = calloc(members.num_members, sizeof(struct st_node *));

	size_t i = 0;

	for (struct st_node *param = tuple_node->TUPLE_DECL.items;
			param; param = param->next_sibling) {
		members.names[i] = param->TUPLE_DECL_ITEM.name;
		members.types[i] = param->TUPLE_DECL_ITEM.type;

		i += 1;
	}

	*out_members = members;
}

static void
st_node_unpack_func_proto(struct st_node *proto_node,
		struct st_tuple_members *out_params,
		struct st_node **out_ret_type)
{
	if (proto_node) {
		switch (proto_node->type) {
			case ST_NODE_FUNC_PROTO:
				st_node_unpack_tuple_nodes(
						proto_node->FUNC_PROTO.params, out_params);
				*out_ret_type = proto_node->FUNC_PROTO.ret;
				break;

			case ST_NODE_TUPLE_DECL:
				st_node_unpack_tuple_nodes(
						proto_node->FUNC_PROTO.params, out_params);
				*out_ret_type = NULL;
				break;

			case ST_NODE_IDENT:
				out_params->num_members = 1;
				out_params->names = calloc(1, sizeof(struct atom *));
				out_params->types = calloc(1, sizeof(struct st_node*));
				out_params->names[0] = proto_node->IDENT;
				out_params->types[0] = NULL;
				*out_ret_type = NULL;
				break;

			default:
				panic("Invalid node '%.*s' as function prototype.",
						LIT(st_node_names[proto_node->type]));
				break;
		}
	} else {
		out_params->names = NULL;
		out_params->types = NULL;
		out_params->num_members = 0;
		*out_ret_type = NULL;
	}
}

static struct ast_node *
st_node_visit_expr(struct compile_ctx *ctx, struct stg_module *mod,
					struct ast_env *env, struct ast_scope *scope,
					struct st_node *node)
{
	switch (node->type) {

	case ST_NODE_ACCESS: {
		/*
		struct expr_node *lhs, *rhs;

		lhs = st_node_visit_expr(ctx, mod, expr, scope, lookup_scope,
								  func_scope, node->ACCESS.lhs);
		rhs = st_node_visit_expr(ctx, mod, expr, scope, lhs,
								  func_scope, node->ACCESS.rhs);

		return rhs;
		*/
	} break;

	case ST_NODE_BIN_OP: {
		struct ast_func_arg func_args[] = {
			{vm_atoms(mod->vm, "lhs"),
				st_node_visit_expr(ctx, mod, env, scope,
						node->BIN_OP.lhs)},
			{vm_atoms(mod->vm, "rhs"),
				st_node_visit_expr(ctx, mod, env, scope,
						node->BIN_OP.rhs)},
		};

		struct atom *op_name;
		op_name =
			binop_atom(mod->atom_table,
					   node->BIN_OP.op);

		struct ast_node *func;

		// TODO: Lookup.

		func = ast_init_node_slot(ctx->ast_ctx, env,
				AST_NODE_NEW, node->loc,
				ast_env_lookup_or_alloc_free(ctx->ast_ctx, env, op_name,
					ast_bind_slot_wildcard(ctx->ast_ctx, env, AST_BIND_NEW, NULL,
						AST_SLOT_TYPE)));

		struct ast_node *call;
		call = ast_init_node_call(ctx->ast_ctx, env,
				AST_NODE_NEW, node->loc, func,
				func_args, ARRAY_LENGTH(func_args));

		return call;
	 }

	case ST_NODE_BIND: {
		struct ast_func_arg func_args[] = {
			{vm_atoms(mod->vm, "src"),
				st_node_visit_expr(ctx, mod, env, scope,
						node->BIN_OP.lhs)},
			{vm_atoms(mod->vm, "drain"),
				st_node_visit_expr(ctx, mod, env, scope,
						node->BIN_OP.rhs)},
		};

		struct ast_node *func;

		// TODO: Lookup.

		struct atom *op_name;
		op_name = vm_atoms(mod->vm, "op->");

		func = ast_init_node_slot(ctx->ast_ctx, env,
				AST_NODE_NEW, node->loc,
				ast_env_lookup_or_alloc_free(ctx->ast_ctx, env, op_name,
					ast_bind_slot_wildcard(ctx->ast_ctx, env, AST_BIND_NEW, NULL,
						AST_SLOT_TYPE)));

		struct ast_node *call;
		call = ast_init_node_call(ctx->ast_ctx, env,
				AST_NODE_NEW, node->loc, func,
				func_args, ARRAY_LENGTH(func_args));


		return call;
	} break;

	case ST_NODE_LAMBDA: {
		struct st_node *proto_node;
		proto_node = node->LAMBDA.proto;

		struct st_tuple_members params_decl;
		struct st_node *ret_type_decl;

		st_node_unpack_func_proto(proto_node,
				&params_decl, &ret_type_decl);

		struct ast_node *func;

		func = ast_init_node_func(ctx->ast_ctx, env,
				AST_NODE_NEW, node->loc,
				params_decl.names, params_decl.num_members);

		struct ast_node *params[params_decl.num_members];
		struct ast_node *ret_type = NULL, *body = NULL;

		for (size_t i = 0; i < params_decl.num_members; i++) {
			params[i] = st_node_visit_expr(ctx, mod, env, scope,
					params_decl.types[i]);
		}

		if (ret_type_decl) {
			ret_type = st_node_visit_expr(ctx, mod, env, scope,
					ret_type_decl);
		} else {
			ret_type = ast_init_node_slot(
					ctx->ast_ctx, env,
					AST_NODE_NEW, node->loc,
					ast_bind_slot_wildcard(
						ctx->ast_ctx, env, AST_BIND_NEW, NULL,
						AST_SLOT_TYPE));
		}

		struct ast_scope func_scope = {0};
		func_scope.num_names = params_decl.num_members;
		struct ast_scope_name tmp_scope_names[func_scope.num_names];
		func_scope.names = tmp_scope_names;
		func_scope.env = env;

		for (size_t i = 0; i < func_scope.num_names; i++) {
			func_scope.names[i].name = params_decl.names[i];
			func_scope.names[i].slot =
				ast_node_resolve_slot(env, &func->func.params[i].slot);
		}

		struct st_node *body_decl;
		body_decl = node->LAMBDA.body;

		body = st_node_visit_expr(ctx, mod, env, &func_scope,
				body_decl);

		ast_finalize_node_func(ctx->ast_ctx, env, func,
				params, params_decl.num_members,
				ret_type, body);

		free(params_decl.names);
		free(params_decl.types);

		return func;
	} break;

	case ST_NODE_FUNC_CALL: {

		struct ast_node *func;

		func = st_node_visit_expr(ctx, mod, env, scope,
				node->FUNC_CALL.ident);

		size_t num_args = 0;

		assert(node->FUNC_CALL.params);
		assert(node->FUNC_CALL.params->type == ST_NODE_TUPLE_LIT);

		struct st_node *args = node->FUNC_CALL.params->TUPLE_LIT.items;

		for (struct st_node *arg = args;
				arg != NULL;
				arg = arg->next_sibling) {
			num_args += 1;
		}

		struct ast_func_arg func_args[num_args];

		{
			struct st_node *arg = args;
			for (size_t i = 0; i < num_args; i++) {
				assert(arg != NULL);
				assert(arg->type == ST_NODE_TUPLE_LIT_ITEM);
				func_args[i].name = arg->TUPLE_LIT_ITEM.name;
				func_args[i].value =
					st_node_visit_expr(ctx, mod, env, scope,
							arg->TUPLE_LIT_ITEM.value);
				arg = arg->next_sibling;
			}
		}

		return ast_init_node_call(
				ctx->ast_ctx, env, AST_NODE_NEW, node->loc,
				func, func_args, num_args);
	} break;

	case ST_NODE_TUPLE_DECL: {
		/*
		struct expr_node *first_arg = NULL, *last_arg = NULL;

		struct st_node *args_tuple;
		args_tuple = node;
		assert(args_tuple->type == ST_NODE_TUPLE_DECL);
		// @TODO: Named tuples
		assert(!args_tuple->TUPLE_DECL.named);

		struct st_node *arg;
		arg = args_tuple->TUPLE_DECL.items;


		while (arg) {
			struct expr_node *n;

			n = st_node_visit_expr(ctx, mod, expr,
									scope, NULL, func_scope,
									arg->TUPLE_DECL_ITEM.type);

			if (!first_arg) {
				assert(!last_arg);
				first_arg = n;
				last_arg = n;
			} else {
				last_arg->next_arg = n;
			}

			arg = arg->next_sibling;
		}

		struct expr_node *local_scope, *func;
		struct atom *tuple_func_name;
		tuple_func_name =
			atom_create(mod->atom_table,
						STR("tuple"));
		local_scope =
			expr_scope(mod, expr, node->loc, scope);
		func =
			expr_lookup(mod, expr, node->loc,
						tuple_func_name, local_scope,
						EXPR_LOOKUP_GLOBAL);

		struct expr_node *call;
		call = expr_call(mod, expr, node->loc, func, first_arg);

		return call;
		*/
	} break;

	case ST_NODE_TUPLE_LIT:
		break;

	case ST_NODE_ARRAY_LIT:
		break;

	case ST_NODE_NUM_LIT: {
		struct object obj;

		obj.data = &node->NUM_LIT;
		obj.type = ctx->ast_ctx->types.integer;

		return ast_init_node_slot(
				ctx->ast_ctx, env, AST_NODE_NEW, node->loc,
				ast_bind_slot_const(
					ctx->ast_ctx, env, AST_BIND_NEW, NULL,
					register_object(ctx->vm, env->store, obj)));
	} break;

	case ST_NODE_STR_LIT:
		// return expr_lit_str(mod, expr, node->loc, node->STR_LIT);
		break;

	case ST_NODE_IDENT:
		return ast_init_node_slot(ctx->ast_ctx, env,
				AST_NODE_NEW, node->loc,
				ast_env_lookup_or_alloc_free(ctx->ast_ctx, env, node->IDENT,
					ast_bind_slot_wildcard(ctx->ast_ctx, env, AST_BIND_NEW, NULL,
						AST_SLOT_TYPE)));

	case ST_NODE_TEMPLATE_VAR:
		return ast_init_node_slot(
				ctx->ast_ctx, env, AST_NODE_NEW, node->loc,
				ast_bind_slot_templ(ctx->ast_ctx, env, AST_BIND_NEW,
					node->TEMPLATE_VAR.name,
					ast_bind_slot_wildcard(
						ctx->ast_ctx, env, AST_BIND_NEW,
						NULL, AST_SLOT_TYPE)));

	default:
		panic("Invalid node '%.*s' in expr.",
			  LIT(st_node_names[node->type]));
		break;
	}

	panic("Unhandled node '%.*s' in expr.",
		  LIT(st_node_names[node->type]));
	return NULL;
}


static struct job_status
job_use_stmt(struct compile_ctx *ctx, struct stg_module *mod,
			 job_use_stmt_t *data)
{
	/*
	struct st_node *node = data->node;
	assert(node->type == ST_NODE_USE_STMT);
	assert(node->USE_STMT.ident->type == ST_NODE_IDENT);

	struct atom *name = node->USE_STMT.ident->IDENT;
	for (size_t i = 0; i < mod->vm->num_modules; i++) {
		struct atom *modname =
			atom_create(mod->atom_table,
						mod->vm->modules[i]->info.name);
		if (modname == name) {
			scope_use(&mod->root_scope, &mod->vm->modules[i]->root_scope);
			return JOB_OK;
		}
	}

	stg_error(&ctx->err, data->node->loc, "Could not find module '%.*s'.", ALIT(name));
	*/
	return JOB_ERROR;
}

static struct job_status
job_visit_decl_stmt(struct compile_ctx *ctx, struct stg_module *mod,
					job_visit_decl_stmt_t *data)
{
	/*
	struct st_node *node = data->stmt;
	assert(node->type == ST_NODE_DECL_STMT);
	assert(node->DECL_STMT.decl != NULL);

	if (!data->initialized) {
		data->initialized = true;

		struct atom *name;

		assert(node->DECL_STMT.name->type == ST_NODE_IDENT);
		name = node->DECL_STMT.name->IDENT;
		data->scope_entry_id =
			scope_insert_overloadable(data->scope, name, SCOPE_ANCHOR_ABSOLUTE,
									  OBJ_UNSET);

		size_t num_params = 0;
		struct expr_func_decl_param *params = NULL;

		struct expr_node *decl_node = NULL;
		struct expr_func_scope *func_scope = NULL;
		struct expr *func_expr = &data->expr;

		if (node->DECL_STMT.args) {
			decl_node = calloc(1, sizeof(struct expr_node));
			func_scope = &decl_node->func_decl.scope;
			func_expr = &decl_node->func_decl.expr;
			params =
				st_node_tuple_decl_to_params(ctx, mod, func_expr,
											  data->scope, node->DECL_STMT.args,
											  func_scope, &num_params);

		}

		// We need to prealloc decl_node before we visit its body
		// because we need a reference to its inner scope
		// (func_scope). Note that the scope is not yet initialized,
		// but this should be fine.

		struct st_node *body_node;
		body_node = node->DECL_STMT.decl;

		struct expr_node *body;
		body = st_node_visit_expr(ctx, mod, func_expr, data->scope,
								   NULL, func_scope, body_node);

		if (params) {
			data->expr.body =
				expr_init_func_decl(mod, func_expr, node->loc, decl_node,
									params, num_params, NULL, body);

			expr_finalize(mod, func_expr);
			expr_bind_type(mod, func_expr,
						   data->expr.body->rule.abs.ret,
						   ctx->vm->default_types.type);

			expr_finalize(mod, &data->expr);
		} else {
			data->expr.body = body;
			expr_finalize(mod, func_expr);
			expr_bind_type(mod, func_expr,
						   data->expr.body->rule.out,
						   ctx->vm->default_types.type);
		}

		struct complie_job *decl_job;
		decl_job = DISPATCH_JOB(ctx, mod, typecheck_expr, COMPILE_PHASE_RESOLVE,
								.expr = &data->expr);

		return JOB_YIELD_FOR(decl_job);
	}

	struct expr *expr;
	expr = calloc(1, sizeof(struct expr));
	*expr = data->expr;

	struct object obj;

	expr_eval_simple(mod->vm, mod, expr, expr->body, &obj);

	// NOTE: The object has to be registered right after the eval,
	// otherwise the object might get overwritten on the arena.
	struct object new_obj =
		register_object(mod->vm, &mod->store, obj);

	struct scope_entry *entry;

	entry = &data->scope->entries[data->scope_entry_id];
	entry->object = new_obj;

	*/
	return JOB_OK;
}

static struct job_status
job_assign_stmt(struct compile_ctx *ctx, struct stg_module *mod, job_assign_stmt_t *data)
{
	assert(data->node->type == ST_NODE_ASSIGN_STMT);
	assert(data->node->ASSIGN_STMT.ident->type == ST_NODE_IDENT);

	if (!data->initialized) {
		data->initialized = true;

		struct atom *name;

		name = data->node->ASSIGN_STMT.ident->IDENT;

		struct st_node *body_node;
		body_node = data->node->ASSIGN_STMT.body;

		struct ast_scope expr_scope = {0};

		expr_scope.ns = data->ns;

		struct ast_node *expr;
		expr = st_node_visit_expr(ctx, mod, &mod->mod.env, &expr_scope, body_node);

		if (data->node->ASSIGN_STMT.type) {
			panic("TODO: assign stmt type.");
		}

		ast_namespace_add_decl(
				ctx->ast_ctx, &mod->mod,
				data->ns, name, expr);


		// TODO: Should the expression be typechecked here or during finalize?

		return JOB_OK;
	}

	// TODO: Eval
	/*
	struct object obj;
	int err;

	err = expr_eval_simple(ctx->vm, mod, &data->expr, data->expr.body, &obj);
	if (err) {
		return JOB_ERROR;
	}

	// NOTE: The object has to be registered right after the eval,
	// otherwise the object might get overwritten on the arena.
	struct object new_obj =
		register_object(mod->vm, &mod->store, obj);

	struct scope_entry *entry;

	entry = &data->scope->entries[data->scope_entry_id];
	entry->object = new_obj;
	*/

	return JOB_OK;
}

static struct job_status
job_assert_stmt(struct compile_ctx *ctx, struct stg_module *mod, job_assert_stmt_t *data)
{
	/*
	assert(data->node->type == ST_NODE_ASSERT_STMT);

	if (!data->initialized) {
		data->initialized = true;

		data->expr.outer_scope = data->scope;

		struct st_node *body_node;
		body_node = data->node->ASSERT_STMT.expr;

		data->expr.body =
			st_node_visit_expr(ctx, mod, &data->expr, data->scope,
								NULL, NULL, body_node);

		expr_finalize(mod, &data->expr);
		expr_bind_type(mod, &data->expr,
					   data->expr.body->rule.out,
					   ctx->vm->default_types.boolean);

		struct complie_job *job;
		job = DISPATCH_JOB(ctx, mod, typecheck_expr, COMPILE_PHASE_RESOLVE,
						   .expr = &data->expr);

		return JOB_YIELD_FOR(job);
	}

	struct object obj;
	int err;

	err = expr_eval_simple(ctx->vm, mod, &data->expr, data->expr.body, &obj);
	if (err) {
		return JOB_ERROR;
	}

	assert(obj.type == ctx->vm->default_types.boolean);

	int64_t value = *(int64_t *)obj.data;

	if (value == 0) {
		stg_error(&ctx->err, data->node->loc, "Assertion failed.");
		return JOB_ERROR;
	} else {
		return JOB_OK;
	}
	*/

	return JOB_ERROR;
}

static struct job_status
job_typecheck_expr(struct compile_ctx *ctx, struct stg_module *mod, job_typecheck_expr_t *data)
{
	/*
	int err;

	data->expr->mod = mod;
	err = expr_typecheck(mod, data->expr);

	if (err < 0) {
		return JOB_ERROR;
	} else if (err > 0) {
		return JOB_YIELD;
	}

	return JOB_OK;
	*/

	return JOB_ERROR;
}

int parse_config_file(struct string filename,
					  struct atom_table *table,
					  struct arena *memory,
					  unsigned int file_id,
					  struct stg_error_context *err,
					  struct st_node **out_node);

static struct job_status
job_parse_file(struct compile_ctx *ctx, struct stg_module *mod,
			   job_parse_file_t *data)
{
	int err;
	struct st_node *node;

	file_id_t file_id;
	file_id = stg_err_add_file(&ctx->err, data->file_name);

	err = parse_config_file(data->file_name, mod->atom_table,
							&ctx->vm->memory, file_id, &ctx->err, &node);
	if (err) {
		return JOB_ERROR;
	}

	assert(node);
	assert(node->type == ST_NODE_MODULE);

	DISPATCH_JOB(ctx, mod, visit_stmt_list, COMPILE_PHASE_DISCOVER,
				 .ns = data->ns,
				 .first_stmt = node->MODULE.body);

	return JOB_OK;
}

static struct job_status
job_visit_stmt_list(struct compile_ctx *ctx, struct stg_module *mod,
					job_visit_stmt_list_t *data)
{
	struct st_node *stmt = data->first_stmt;
	while (stmt) {
		assert(stmt->type == ST_NODE_STMT);

		dispatch_stmt(ctx, mod, data->ns, stmt);
		stmt = stmt->next_sibling;
	}

	return JOB_OK;
}

static bool
has_extension(struct string str, struct string ext)
{
	if (str.length < ext.length + 1) {
		return false;
	}

	struct string file_ext;
	file_ext.length = ext.length;
	file_ext.text = (str.text + str.length) - ext.length;

	return string_equal(file_ext, ext);
}

static void
discover_module_files(struct compile_ctx *ctx, struct stg_module *mod,
					  struct string src_dir)
{
	/* // TODO: Ensure zero-terminated */
	char *paths[] = {src_dir.text, NULL};

	FTS *ftsp;
	ftsp = fts_open(paths, FTS_PHYSICAL, NULL);

	struct ast_namespace *dir_ns = &mod->mod.root;

	FTSENT *f;
	while ((f = fts_read(ftsp)) != NULL) {
		switch (f->fts_info) {
		case FTS_F: {
			struct string path;
			path.text = f->fts_path;
			path.length = f->fts_pathlen;

			if (has_extension(path, STR(".stg")) &&
				f->fts_name[0] != '.') {
				struct string name;
				name.text = f->fts_name;
				// Remove the ".stg" suffix
				name.length = f->fts_namelen - 4;

				struct string file_name = {0};
				string_duplicate(&ctx->vm->memory, &file_name, path);

				struct atom *atom = vm_atom(ctx->vm, name);
				struct ast_namespace *file_ns;

				if (atom == vm_atoms(ctx->vm, "mod")) {
					file_ns = dir_ns;
				} else {
					file_ns = ast_namespace_add_ns(dir_ns, atom);
				}

				DISPATCH_JOB(ctx, mod, parse_file, COMPILE_PHASE_DISCOVER,
							 .ns = file_ns,
							 .file_name = file_name);
			}
		} break;

		case FTS_D:
			if (f->fts_namelen) {
				struct string name;
				name.text = f->fts_name;
				name.length = f->fts_namelen;

				struct atom *atom = vm_atom(ctx->vm, name);

				dir_ns = ast_namespace_add_ns(dir_ns, atom);
			}
			break;

		case FTS_DP:
			if (f->fts_namelen) {
				dir_ns = dir_ns->parent;
			}
			break;

		default:
			break;
		}
	}

	if (errno != 0) {
		perror("fts");
	}

	fts_close(ftsp);
}

static struct job_status
job_discover_module(struct compile_ctx *ctx, struct stg_module *mod,
			   job_discover_module_t *data)
{
	discover_module_files(ctx, mod, data->module_src_dir);

	return JOB_OK;
}

#define COMPILE_DEBUG_JOBS 0

static void compile_exec_job(struct compile_ctx *ctx, struct complie_job *job)
{
	assert(job->next_job != job);

	if (job->status == JOB_STATUS_ERROR) {
		return;
	}

	assert(job->status == JOB_STATUS_IDLE || job->status == JOB_STATUS_YIELD);

	if (job->last_dispatch_time == ctx->time) {
		ctx->time += 1;
	}
	job->last_dispatch_time = ctx->time;

#if COMPILE_DEBUG_JOBS
	printf("[%zu] Dispatching 0x%03zx %.*s... ",
		   job->last_dispatch_time,
		   job->id,
		   LIT(complie_job_names[job->type]));
#endif

	struct job_status res;

	switch (job->type) {
#define COMPILE_JOB(name, data) case COMPILE_JOB_##name: res = job_##name(ctx, job->mod, &job->name); break;
#include "compile_job_defs.h"
#undef COMPILE_JOB

	case COMPILE_JOBS_LEN:
		assert(false);
		break;
	}

	job->status = res.status;

	switch (res.status) {
	case JOB_STATUS_OK: {
		struct complie_job *dep = job->first_dependant_node;
		while (dep) {
			struct complie_job *next = dep->next_job;

			dep->next_job = NULL;
			append_job(ctx, ctx->current_phase, dep);

			dep = next;
		}

		job->first_dependant_node = NULL;
#if COMPILE_DEBUG_JOBS
		printf(TERM_COLOR_GREEN("ok") "\n");
#endif
	} break;

	case JOB_STATUS_ERROR: {
		ctx->num_jobs_failed += 1;

		size_t num_canceled = 0;

		for (struct complie_job *dep = job->first_dependant_node;
			 dep != NULL;
			 dep = dep->next_job) {
			dep->status = JOB_STATUS_ERROR;
			num_canceled += 1;
		}

#if COMPILE_DEBUG_JOBS
		if (num_canceled > 0) {
			printf(TERM_COLOR_RED("=== error (%zu dependenc%s canceled) ===") "\n", num_canceled,
				   (num_canceled == 1 ? "y" : "ies"));
		} else {
			printf(TERM_COLOR_RED("=== error ===") "\n");
		}
#endif
	} break;

	case JOB_STATUS_YIELD:
		if (res.yield_for) {
#if COMPILE_DEBUG_JOBS
			printf(TERM_COLOR_YELLOW("=== yield for 0x%03zx %.*s ===") "\n",
				   res.yield_for->id, LIT(complie_job_names[res.yield_for->type]));
#endif

			if (res.yield_for->status == JOB_STATUS_ERROR) {
				job->status = JOB_STATUS_ERROR;
				break;

			} else if (res.yield_for->status == JOB_STATUS_OK) {
				job->next_job = NULL;
				append_job(ctx, ctx->current_phase, job);

			} else {
				// TODO: Thread-safe
				job->next_job = res.yield_for->first_dependant_node;
				res.yield_for->first_dependant_node = job;
			}
		} else {
#if COMPILE_DEBUG_JOBS
			printf(TERM_COLOR_YELLOW("=== yield ===") "\n");
#endif

			job->next_job = NULL;
			append_job(ctx, ctx->current_phase, job);
		}
		break;

	case JOB_STATUS_YIELD_FOR_PHASE:
		job->status = JOB_STATUS_YIELD;
		job->next_job = NULL;
		append_job(ctx, res.yield_for_phase, job);
#if COMPILE_DEBUG_JOBS
		printf(TERM_COLOR_YELLOW("=== yield for phase %i ===") "\n",
			   res.yield_for_phase);
#endif
		break;

	case JOB_STATUS_IDLE:
		panic("Job returned idle as result.");
		break;
	}

	if (job->status == JOB_STATUS_YIELD) {
		if (ctx->time - ctx->last_completed_job_time > COMPILE_JOB_MAX_CONSECUTIVE_YIELDS) {
#if COMPILE_DEBUG_JOBS
			printf(TERM_COLOR_RED("=== erroring due to no progress ===") "\n");
#endif
			job->status = JOB_STATUS_ERROR;
			ctx->num_jobs_failed += 1;
		}
	}

	if (job->status  != JOB_STATUS_YIELD) {
		ctx->last_completed_job_time = ctx->time;
	}
}

int
stg_compile(struct vm *vm, struct ast_context *ast_ctx, struct string initial_module_src_dir)
{
	struct compile_ctx ctx = {0};

	ctx.vm = vm;
	ctx.mem = &vm->memory;
	ctx.err.string_arena = &vm->memory;
	ctx.ast_ctx = ast_ctx;

	assert(!vm->err);
	vm->err = &ctx.err;

	struct stg_module_info modinfo;
	memset(&modinfo, 0, sizeof(struct stg_module_info));

	// TODO: Have an actual name.
	modinfo.name = STR("native");
	modinfo.version.major = 1;
	modinfo.version.minor = 0;

	struct stg_module *mod;
	// TODO: We should somehow initialize through the init method
	// instead of outside of register.
	mod = vm_register_module(vm, &modinfo);

	scope_use(&mod->root_scope, &vm->modules[0]->root_scope);

	DISPATCH_JOB(&ctx, mod, discover_module, COMPILE_PHASE_DISCOVER,
			.module_src_dir = initial_module_src_dir);

	for (; ctx.current_phase < COMPILE_NUM_PHASES; ctx.current_phase += 1) {
		struct compile_phase *phase = &ctx.phases[ctx.current_phase];
#if COMPILE_DEBUG_JOBS
		printf("\n" TERM_COLOR_BLUE("=== Phase %i ===") "\n",
			   ctx.current_phase);
#endif
		while (phase->job_head) {
			struct complie_job *job = phase->job_head;
			phase->job_head = job->next_job;
			if (!phase->job_head) {
				phase->job_end = NULL;
			}

			compile_exec_job(&ctx, job);
		}
	}

	print_errors(&ctx.err);

	if (ctx.num_jobs_failed > 0 || ctx.err.num_errors > 0) {
#if COMPILE_DEBUG_JOBS
		printf("\n");
#endif
		printf(TERM_COLOR_RED("Compilation failed! "
					"(%zu jobs failed, %zu errors)") "\n",
			   ctx.num_jobs_failed, ctx.err.num_errors);
	}

	// scope_print(vm, &vm->root_scope);
	printf("\n");
	ast_print_module(ctx.ast_ctx, &mod->mod);

	printf("\n");
	ast_env_print(vm, &mod->mod.env);

	vm->err = NULL;
	return 0;
}
