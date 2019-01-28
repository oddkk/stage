#include "config.h"
#include "utils.h"
#include "dlist.h"
#include "expr.h"
#include "objstore.h"
#include "scope.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>

#define ENABLE_TERM_COLORS 1

#if ENABLE_TERM_COLORS
#define TERM_COLOR_ESCAPE_LIT "\x1b"
#define TERM_COLOR_RED_LIT TERM_COLOR_ESCAPE_LIT "[1;31m"
#define TERM_COLOR_GREEN_LIT TERM_COLOR_ESCAPE_LIT "[1;32m"
#define TERM_COLOR_YELLOW_LIT TERM_COLOR_ESCAPE_LIT "[1;33m"
#define TERM_COLOR_BLUE_LIT TERM_COLOR_ESCAPE_LIT "[1;34m"
#define TERM_COLOR_CLEAR_LIT TERM_COLOR_ESCAPE_LIT "[0m"
#else
#define TERM_COLOR_RED_LIT ""
#define TERM_COLOR_GREEN_LIT ""
#define TERM_COLOR_YELLOW_LIT ""
#define TERM_COLOR_BLUE_LIT ""
#define TERM_COLOR_CLEAR_LIT ""
#endif

#define TERM_COLOR_RED(text) TERM_COLOR_RED_LIT text TERM_COLOR_CLEAR_LIT
#define TERM_COLOR_GREEN(text) TERM_COLOR_GREEN_LIT text TERM_COLOR_CLEAR_LIT
#define TERM_COLOR_YELLOW(text) TERM_COLOR_YELLOW_LIT text TERM_COLOR_CLEAR_LIT
#define TERM_COLOR_BLUE(text) TERM_COLOR_BLUE_LIT text TERM_COLOR_CLEAR_LIT

struct string cfg_bin_op_sym[] = {
#define OP(name, sym) STR(sym),
	CFG_BIN_OPS
#undef OP
};

struct atom *binop_atom(struct atom_table *atom_table,
						enum cfg_bin_op op)
{
	assert(op < CFG_OP_LEN);

	char buffer[2 + CFG_BIN_OPS_MAX_LEN] = {0};

	buffer[0] = 'o';
	buffer[1] = 'p';

	struct string sym = cfg_bin_op_sym[op];

	assert(sym.length <= CFG_BIN_OPS_MAX_LEN);

	memcpy(&buffer[2], sym.text, sym.length);

	struct string name;
	name.text = buffer;
	name.length = 2 + sym.length;

	return atom_create(atom_table, name);
}

struct string cfg_node_names[] = {
#define CFG_NODE(name, data) STR(#name),
#include "config_nodes.h"
#undef CFG_NODE
};

struct string cfg_job_names[] = {
#define CFG_JOB(name, data) STR(#name),
#include "config_jobs.h"
#undef CFG_JOB
};

#define CFG_JOB(name, data) typedef data job_##name##_t;
#include "config_jobs.h"
#undef CFG_JOB

enum job_status_code {
	JOB_STATUS_OK = 0,
	JOB_STATUS_ERROR = -1,
	JOB_STATUS_YIELD = 1,
	JOB_STATUS_YIELD_FOR_PHASE = 2,
	JOB_STATUS_IDLE = 3,
};

struct cfg_job {
	size_t id;
	size_t last_dispatch_time;
	enum cfg_job_type type;
	enum job_status_code status;

	struct cfg_job *next_job;
	struct cfg_job *first_dependant_node;

	struct stg_module *mod;

#define CFG_JOB(name, data) job_##name##_t name;
	union {
#include "config_jobs.h"
	};
#undef CFG_JOB
};

enum cfg_compile_phase {
	CFG_PHASE_DISCOVER = 0,
	CFG_PHASE_RESOLVE,

	CFG_NUM_PHASES
};

struct job_status {
	enum job_status_code status;
	struct cfg_job *yield_for;
	enum cfg_compile_phase yield_for_phase;
};

#define JOB_OK  ((struct job_status){.status=JOB_STATUS_OK})
#define JOB_ERROR ((struct job_status){.status=JOB_STATUS_ERROR})
#define JOB_YIELD ((struct job_status){.status=JOB_STATUS_YIELD, .yield_for=NULL})
#define JOB_YIELD_FOR(job) ((struct job_status){.status=JOB_STATUS_YIELD, .yield_for=(job)})
#define JOB_YIELD_FOR_PHASE(phase) ((struct job_status){.status=JOB_STATUS_YIELD_FOR_PHASE, .yield_for_phase=(phase)})

struct cfg_phase {
	struct cfg_job *job_head;
	struct cfg_job *job_end;
};

#define CFG_JOB_MAX_CONSECUTIVE_YIELDS (3)

struct cfg_ctx {
	struct vm *vm;
	struct arena *mem;

	size_t next_job_id;
	// A measure of how many times jobs have yielded.
	size_t time;
	size_t last_completed_job_time;

	enum cfg_compile_phase current_phase;
	struct cfg_phase phases[CFG_NUM_PHASES];

	struct string *file_names;
	size_t num_files;

	size_t num_errors;
	size_t num_jobs_failed;
};

static void
cfg_error(struct cfg_ctx *ctx, struct cfg_node *node,
		  const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);

	struct string file_name = {0};
	if (node->file_id < ctx->num_files) {
		file_name = ctx->file_names[node->file_id];
	} else {
		file_name = STR("(unknown)");
	}

	fprintf(stderr, "%.*s %zu:%zu: ",
			LIT(file_name),
			node->from.line, node->from.column);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	ctx->num_errors += 1;
}

static void
append_job(struct cfg_ctx *ctx, enum cfg_compile_phase ph, struct cfg_job *job)
{
	assert(ph < CFG_NUM_PHASES);
	assert(ph >= ctx->current_phase);

	struct cfg_phase *phase = &ctx->phases[ph];

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

static struct cfg_job *
dispatch_job(struct cfg_ctx *ctx, enum cfg_compile_phase phase, struct cfg_job job)
{
	struct cfg_job *new_job = calloc(1, sizeof(struct cfg_job));
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
				 (struct cfg_job){								\
					 .type=CFG_JOB_##name,						\
					 .mod = _mod,								\
					 .name = (job_##name##_t){__VA_ARGS__}		\
				 })

static struct scope *
instantiate_scope_by_access_pattern(struct cfg_ctx *ctx,
									struct stg_module *mod,
									struct scope *parent,
									struct cfg_node *node)
{
	switch (node->type) {
	case CFG_NODE_ACCESS: {
		struct scope *scope;

		scope = instantiate_scope_by_access_pattern(ctx, mod,parent, node->ACCESS.lhs);
		if (!scope) {
			return NULL;
		}

		return instantiate_scope_by_access_pattern(ctx, mod,scope, node->ACCESS.rhs);
	} break;

	case CFG_NODE_IDENT: {
		struct scope *scope;
		struct scope_entry entry;

		if (scope_local_lookup(parent, node->IDENT, &entry) == 0) {
			if (entry.anchor != SCOPE_ANCHOR_NONE || !entry.scope) {
				cfg_error(ctx, node, "'%.*s' already exists, and is not a namespace.");
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

static void dispatch_stmt(struct cfg_ctx *ctx, struct stg_module *mod,
						  struct scope *parent_scope, struct cfg_node *stmt)
{
	assert(stmt->type == CFG_NODE_STMT);

	// TODO: Handle attributes

	struct cfg_node *node = stmt->STMT.stmt;

	if (!node) {
		return;
	}

	switch (node->type) {

	case CFG_NODE_DECL_STMT: {
		DISPATCH_JOB(ctx, mod, visit_decl_stmt, CFG_PHASE_DISCOVER,
					 .scope = parent_scope,
					 .stmt = node);
	} break;

	case CFG_NODE_USE_STMT:
		break;

	case CFG_NODE_ASSERT_STMT:
		DISPATCH_JOB(ctx, mod, assert_stmt, CFG_PHASE_DISCOVER,
					 .scope = parent_scope,
					 .node = node);
		break;

	case CFG_NODE_FUNC_STMT:
		DISPATCH_JOB(ctx, mod, func_decl, CFG_PHASE_DISCOVER,
					 .scope = parent_scope,
					 .node = node);
		break;

	case CFG_NODE_ASSIGN_STMT:
		DISPATCH_JOB(ctx, mod, assign_stmt, CFG_PHASE_DISCOVER,
					 .scope = parent_scope,
					 .node = node);
		break;

	case CFG_NODE_BIND:
		DISPATCH_JOB(ctx, mod, expr_stmt, CFG_PHASE_DISCOVER,
					 .scope = parent_scope,
					 .node = node);
		break;

	case CFG_NODE_NAMESPACE: {
		struct scope *ns_scope;
		ns_scope = instantiate_scope_by_access_pattern(ctx, mod, parent_scope,
													   node->NAMESPACE.name);

		DISPATCH_JOB(ctx, mod, visit_stmt_list, CFG_PHASE_DISCOVER,
					 .scope = ns_scope,
					 .first_stmt = node->NAMESPACE.body);
	} break;

	default:
		panic("Invalid node '%.*s' as statement.",
				LIT(cfg_node_names[node->type]));
		break;
	}
}

static struct expr_node *
cfg_node_visit_expr(struct cfg_ctx *ctx, struct stg_module *mod,
					struct expr *expr, struct scope *scope,
					struct expr_node *lookup_scope,
					struct expr_func_scope *func_scope,
					struct cfg_node *node)
{
	switch (node->type) {

	case CFG_NODE_ACCESS: {
		struct expr_node *lhs, *rhs;

		lhs = cfg_node_visit_expr(ctx, mod, expr, scope, lookup_scope,
								  func_scope, node->ACCESS.lhs);
		rhs = cfg_node_visit_expr(ctx, mod, expr, scope, lhs,
								  func_scope, node->ACCESS.rhs);

		return rhs;
	} break;

	case CFG_NODE_BIN_OP: {
		struct expr_node
			*lhs, *rhs, *func,
			*func_lookup, *local_scope;

		lhs = cfg_node_visit_expr(ctx, mod, expr, scope, NULL,
								  func_scope, node->BIN_OP.lhs);
		rhs = cfg_node_visit_expr(ctx, mod, expr, scope, NULL,
								  func_scope, node->BIN_OP.rhs);

		lhs->next_arg = rhs;

		struct atom *op_name;
		op_name =
			binop_atom(mod->atom_table,
					   node->BIN_OP.op);
		local_scope =
			expr_scope(mod, expr, scope);
		func_lookup =
			expr_lookup(mod, expr,
						op_name, local_scope,
						EXPR_LOOKUP_GLOBAL);

		func = expr_call(mod, expr,
						 func_lookup, lhs);

		return func;
	} break;

	case CFG_NODE_BIND: {
		struct expr_node
			*src, *drain, *func,
			*func_lookup, *local_scope;

		src = cfg_node_visit_expr(ctx, mod, expr, scope, NULL,
								  func_scope, node->BIND.src);
		drain = cfg_node_visit_expr(ctx, mod, expr, scope, NULL,
									func_scope, node->BIND.drain);

		src->next_arg = drain;

		struct atom *op_name;
		op_name =
			binop_atom(mod->atom_table,
					   CFG_OP_BIND);
		local_scope =
			expr_scope(mod, expr, scope);
		func_lookup =
			expr_lookup(mod, expr,
						op_name, local_scope,
						EXPR_LOOKUP_GLOBAL);

		func = expr_call(mod, expr,
						 func_lookup, src);

		return func;
	} break;

	case CFG_NODE_LAMBDA:
		panic("TODO: Lambda");
		break;

	case CFG_NODE_FUNC_CALL: {
		struct expr_node *first_arg = NULL, *last_arg = NULL;

		struct cfg_node *args_tuple;
		args_tuple = node->FUNC_CALL.params;
		if (args_tuple) {
			assert(args_tuple->type == CFG_NODE_TUPLE_LIT);
			assert(!args_tuple->TUPLE_LIT.named);

			struct cfg_node *arg;
			arg = args_tuple->TUPLE_LIT.items;


			while (arg) {
				struct expr_node *n;

				n = cfg_node_visit_expr(ctx, mod, expr,
										scope, NULL, func_scope,
										arg->TUPLE_LIT_ITEM.value);

				if (!first_arg) {
					assert(!last_arg);
					first_arg = n;
					last_arg = n;
				} else {
					last_arg->next_arg = n;
				}

				arg = arg->next_sibling;
			}
		}

		struct expr_node *func;
		func = cfg_node_visit_expr(ctx, mod, expr,
								   scope, NULL, func_scope,
								   node->FUNC_CALL.ident);

		struct expr_node *call;
		call = expr_call(mod, expr, func, first_arg);

		return call;
	} break;

	case CFG_NODE_TUPLE_DECL: {
		struct expr_node *first_arg = NULL, *last_arg = NULL;

		struct cfg_node *args_tuple;
		args_tuple = node;
		assert(args_tuple->type == CFG_NODE_TUPLE_DECL);
		// @TODO: Named tuples
		assert(!args_tuple->TUPLE_DECL.named);

		struct cfg_node *arg;
		arg = args_tuple->TUPLE_DECL.items;


		while (arg) {
			struct expr_node *n;

			n = cfg_node_visit_expr(ctx, mod, expr,
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
			expr_scope(mod, expr, scope);
		func =
			expr_lookup(mod, expr,
						tuple_func_name, local_scope,
						EXPR_LOOKUP_GLOBAL);

		struct expr_node *call;
		call = expr_call(mod, expr, func, first_arg);

		return call;
	} break;

	case CFG_NODE_TUPLE_LIT:
		panic("TODO: Tuple lit");
		break;

	case CFG_NODE_ARRAY_LIT:
		panic("TODO: Array lit");
		break;

	case CFG_NODE_NUM_LIT:
		return expr_lit_int(mod, expr, node->NUM_LIT);

	case CFG_NODE_STR_LIT:
		return expr_lit_str(mod, expr, node->STR_LIT);

	case CFG_NODE_IDENT:
		if (lookup_scope) {
			return expr_lookup(mod, expr,
							   node->IDENT,
							   lookup_scope,
							   EXPR_LOOKUP_LOCAL);
		} else {
			struct expr_node *l_scope;
			if (func_scope) {
				return expr_lookup_func_scope(mod, expr,
											  node->IDENT,
											  func_scope);
			} else {
				l_scope = expr_scope(mod, expr, scope);

				return expr_lookup(mod, expr,
								   node->IDENT, l_scope,
								   EXPR_LOOKUP_GLOBAL);
			}
		}
		break;

	default:
		panic("Invalid node '%.*s' in expr.",
			  LIT(cfg_node_names[node->type]));
		break;
	}

	panic("Unhandled node '%.*s' in expr.",
		  LIT(cfg_node_names[node->type]));
	return NULL;
}


static struct expr_func_decl_param *
cfg_node_tuple_decl_to_params(struct cfg_ctx *ctx, struct stg_module *mod,
							  struct expr *expr, struct scope *scope,
							  struct cfg_node *param_tuple,
							  struct expr_func_scope *func_scope,
							  size_t *out_num_params)
{
	assert(param_tuple->type == CFG_NODE_TUPLE_DECL);

	size_t num_params = 0;
	struct cfg_node *param;
	param = param_tuple->TUPLE_DECL.items;

	while (param) {
		num_params += 1;
		param = param->next_sibling;
	}

	struct expr_func_decl_param *params;
	params = calloc(num_params, sizeof(struct expr_func_decl_param));

	size_t i = 0;
	param = param_tuple->TUPLE_DECL.items;

	while (param) {
		params[i].name = param->TUPLE_DECL_ITEM.name;
		params[i].type =
			cfg_node_visit_expr(ctx, mod, expr, scope, NULL, func_scope,
								param->TUPLE_DECL_ITEM.type);

		i += 1;
		param = param->next_sibling;
	}

	*out_num_params = num_params;
	return params;
}

static struct job_status
job_visit_decl_stmt(struct cfg_ctx *ctx, struct stg_module *mod,
					job_visit_decl_stmt_t *data)
{
	struct cfg_node *node = data->stmt;
	assert(node->type == CFG_NODE_DECL_STMT);
	assert(node->DECL_STMT.decl != NULL);

	if (!data->initialized) {
		data->initialized = true;

		struct atom *name;

		assert(node->DECL_STMT.name->type == CFG_NODE_IDENT);
		name = node->DECL_STMT.name->IDENT;
		data->scope_entry_id =
			scope_insert_overloadable(data->scope, name, SCOPE_ANCHOR_ABSOLUTE,
									  OBJ_UNSET);

		size_t num_params = 0;
		struct expr_func_decl_param *params = NULL;

		struct expr_node *decl_node = NULL;
		struct expr_func_scope *func_scope = NULL;

		if (node->DECL_STMT.args) {
			params =
				cfg_node_tuple_decl_to_params(ctx, mod, &data->expr,
											  data->scope, node->DECL_STMT.args,
											  NULL, &num_params);

			decl_node = calloc(1, sizeof(struct expr_node));
			func_scope = &decl_node->func_decl.scope;
		}

		// We need to prealloc decl_node before we visit its body
		// because we need a reference to its inner scope
		// (func_scope). Note that the scope is not yet initialized,
		// but this should be fine.

		struct cfg_node *body_node;
		body_node = node->DECL_STMT.decl;

		struct expr_node *body;
		body = cfg_node_visit_expr(ctx, mod, &data->expr, data->scope,
								   NULL, func_scope, body_node);

		if (params) {
			data->expr.body =
				expr_init_func_decl(mod, &data->expr, decl_node,
									params, num_params, NULL, body);

			expr_finalize(mod, &data->expr);
			expr_bind_type(mod, &data->expr,
						   data->expr.body->rule.abs.ret,
						   ctx->vm->default_types.type);
		} else {
			data->expr.body = body;
			expr_finalize(mod, &data->expr);
			expr_bind_type(mod, &data->expr,
						   data->expr.body->rule.out,
						   ctx->vm->default_types.type);
		}

		struct cfg_job *decl_job;
		decl_job = DISPATCH_JOB(ctx, mod, typecheck_expr, CFG_PHASE_RESOLVE,
								.expr = &data->expr);

		return JOB_YIELD_FOR(decl_job);
	}

	struct expr *expr;
	expr = calloc(1, sizeof(struct expr));
	*expr = data->expr;

	struct object obj;

	expr_eval_simple(mod->vm, expr, expr->body, &obj);

	// NOTE: The object has to be registered right after the eval,
	// otherwise the object might get overwritten on the arena.
	struct object new_obj =
		register_object(&mod->store, obj);

	struct scope_entry *entry;

	entry = &data->scope->entries[data->scope_entry_id];
	entry->object = new_obj;

	return JOB_OK;
}


static struct job_status
job_func_decl(struct cfg_ctx *ctx, struct stg_module *mod, job_func_decl_t *data)
{
	assert(data->node->type == CFG_NODE_FUNC_STMT);
	assert(data->node->FUNC_STMT.ident->type == CFG_NODE_IDENT);

	if (!data->initialized) {
		data->initialized = true;

		struct atom *name;

		name = data->node->FUNC_STMT.ident->IDENT;
		data->scope_entry_id =
			scope_insert_overloadable(data->scope, name, SCOPE_ANCHOR_ABSOLUTE,
									  OBJ_UNSET);

		data->expr.outer_scope = data->scope;

		struct cfg_node *proto_node = data->node->FUNC_STMT.proto;
		struct cfg_node *body_node = data->node->FUNC_STMT.body;

		size_t num_params = 0;
		struct expr_func_decl_param *params = NULL;
		struct expr_node *ret = NULL;

		if (proto_node) {
			switch (proto_node->type) {
			case CFG_NODE_FUNC_PROTO: {
				struct cfg_node *ret_node;
				ret_node = proto_node->FUNC_PROTO.ret;

				ret = cfg_node_visit_expr(ctx, mod, &data->expr,
										  data->scope, NULL, NULL, ret_node);

				struct cfg_node *param_tuple;
				param_tuple = proto_node->FUNC_PROTO.params;

				params =
					cfg_node_tuple_decl_to_params(ctx, mod, &data->expr,
												  data->scope, param_tuple,
												  NULL, &num_params);
			} break;

			case CFG_NODE_TUPLE_DECL: {
				params =
					cfg_node_tuple_decl_to_params(ctx, mod, &data->expr,
												  data->scope, proto_node,
												  NULL, &num_params);
			} break;

			case CFG_NODE_IDENT:
				num_params = 1;
				params = calloc(1, sizeof(struct expr_func_decl_param));
				params[0].name = proto_node->IDENT;
				params[0].type = NULL;
				break;

			default:
				panic("Invalid node '%.*s' as function prototype.",
					  LIT(cfg_node_names[proto_node->type]));
				break;
			}
		}

		struct expr_node *decl_node;
		decl_node = calloc(1, sizeof(struct expr_node));

		// We need to prealloc decl_node before we visit its body
		// because we need a reference to its inner scope
		// (func_scope). Note that the scope is not yet initialized,
		// but this should be fine.

		struct expr_node *body;
		body = cfg_node_visit_expr(ctx, mod, &data->expr, data->scope,
								   NULL, &decl_node->func_decl.scope, body_node);

		data->expr.body =
			expr_init_func_decl(mod, &data->expr, decl_node,
								params, num_params, ret, body);

		struct cfg_job *func_job;
		func_job = DISPATCH_JOB(ctx, mod, typecheck_expr, CFG_PHASE_RESOLVE,
								.expr = &data->expr);

		return JOB_YIELD_FOR(func_job);
	}

	struct expr *expr;
	expr = calloc(1, sizeof(struct expr));
	*expr = data->expr;

	struct object func_obj;

	expr_eval_simple(mod->vm, expr, expr->body, &func_obj);

	// NOTE: The object has to be registered right after the eval,
	// otherwise the object might get overwritten on the arena.
	struct object new_func_obj =
		register_object(&mod->store, func_obj);

	struct scope_entry *entry;

	entry = &data->scope->entries[data->scope_entry_id];
	entry->object = new_func_obj;

	return JOB_OK;
}

static struct job_status
job_assign_stmt(struct cfg_ctx *ctx, struct stg_module *mod, job_assign_stmt_t *data)
{
	assert(data->node->type == CFG_NODE_ASSIGN_STMT);
	assert(data->node->ASSIGN_STMT.ident->type == CFG_NODE_IDENT);

	if (!data->initialized) {
		data->initialized = true;

		struct atom *name;

		name = data->node->ASSIGN_STMT.ident->IDENT;
		data->scope_entry_id =
			scope_insert_overloadable(data->scope, name, SCOPE_ANCHOR_ABSOLUTE,
									  OBJ_UNSET);

		data->expr.outer_scope = data->scope;

		if (data->node->ASSIGN_STMT.type) {
			panic("TODO: assign stmt type.");
		}

		struct cfg_node *body_node;
		body_node = data->node->ASSIGN_STMT.body;

		data->expr.body =
			cfg_node_visit_expr(ctx, mod, &data->expr, data->scope,
								NULL, NULL, body_node);

		struct cfg_job *job;
		job = DISPATCH_JOB(ctx, mod, typecheck_expr, CFG_PHASE_RESOLVE,
						   .expr = &data->expr);

		return JOB_YIELD_FOR(job);
	}

	struct object obj;
	int err;

	err = expr_eval_simple(ctx->vm, &data->expr, data->expr.body, &obj);
	if (err) {
		return JOB_ERROR;
	}

	// NOTE: The object has to be registered right after the eval,
	// otherwise the object might get overwritten on the arena.
	struct object new_obj =
		register_object(&mod->store, obj);

	struct scope_entry *entry;

	entry = &data->scope->entries[data->scope_entry_id];
	entry->object = new_obj;

	return JOB_OK;
}

static struct job_status
job_assert_stmt(struct cfg_ctx *ctx, struct stg_module *mod, job_assert_stmt_t *data)
{
	assert(data->node->type == CFG_NODE_ASSERT_STMT);

	if (!data->initialized) {
		data->initialized = true;

		data->expr.outer_scope = data->scope;

		struct cfg_node *body_node;
		body_node = data->node->ASSERT_STMT.expr;

		data->expr.body =
			cfg_node_visit_expr(ctx, mod, &data->expr, data->scope,
								NULL, NULL, body_node);

		expr_finalize(mod, &data->expr);
		expr_bind_type(mod, &data->expr,
					   data->expr.body->rule.out,
					   ctx->vm->default_types.boolean);

		struct cfg_job *job;
		job = DISPATCH_JOB(ctx, mod, typecheck_expr, CFG_PHASE_RESOLVE,
						   .expr = &data->expr);

		return JOB_YIELD_FOR(job);
	}

	struct object obj;
	int err;

	err = expr_eval_simple(ctx->vm, &data->expr, data->expr.body, &obj);
	if (err) {
		return JOB_ERROR;
	}

	assert(obj.type == ctx->vm->default_types.boolean);

	int64_t value = *(int64_t *)obj.data;

	if (value == 0) {
		cfg_error(ctx, data->node, "Assertion failed.");
		return JOB_ERROR;
	} else {
		return JOB_OK;
	}
}

static struct job_status
job_expr_stmt(struct cfg_ctx *ctx, struct stg_module *mod, job_expr_stmt_t *data)
{
	struct cfg_node *expr_node = NULL;

	switch (data->node->type) {
	case CFG_NODE_BIND:
		expr_node = data->node;
		break;

	default:
		panic("Invalid node '%.*s' as expr stmt.",
			  LIT(cfg_node_names[data->node->type]));
		break;
	}

	assert(expr_node);

	if (!data->initialized) {
		data->initialized = true;

		data->expr.outer_scope = data->scope;

		data->expr.body =
			cfg_node_visit_expr(ctx, mod, &data->expr, data->scope,
								NULL, NULL, expr_node);

		struct cfg_job *job;
		job = DISPATCH_JOB(ctx, mod, typecheck_expr, CFG_PHASE_RESOLVE,
						   .expr = &data->expr);

		return JOB_YIELD_FOR(job);
	}

	struct object obj;
	int err;

	err = expr_eval_simple(ctx->vm, &data->expr, data->expr.body, &obj);
	if (err) {
		return JOB_ERROR;
	}

	return JOB_OK;
}


static struct job_status
job_typecheck_expr(struct cfg_ctx *ctx, struct stg_module *mod, job_typecheck_expr_t *data)
{
	int err;

	err = expr_typecheck(mod, data->expr);

	if (err < 0) {
		return JOB_ERROR;
	} else if (err > 0) {
		return JOB_YIELD;
	}

	return JOB_OK;
}

int parse_config_file(struct string filename,
					  struct atom_table *table,
					  struct arena *memory,
					  unsigned int file_id,
					  struct cfg_node **out_node);

static struct job_status
job_parse_file(struct cfg_ctx *ctx, struct stg_module *mod,
			   job_parse_file_t *data)
{
	int err;
	struct cfg_node *node;

	unsigned int file_id = ctx->num_files;
	ctx->num_files += 1;

	struct string *new_file_names;
	new_file_names = realloc(ctx->file_names, ctx->num_files * sizeof(struct string));

	if (!new_file_names) {
		panic("Could not allocate file name array.");
		return JOB_ERROR;
	}

	ctx->file_names = new_file_names;
	ctx->file_names[file_id] = data->file_name;

	err = parse_config_file(data->file_name, mod->atom_table,
							&ctx->vm->memory, file_id, &node);
	if (err) {
		return JOB_ERROR;
	}

	assert(node);
	assert(node->type == CFG_NODE_MODULE);

	DISPATCH_JOB(ctx, mod, visit_stmt_list, CFG_PHASE_DISCOVER,
				 .scope = data->mod_scope,
				 .first_stmt = node->MODULE.body);

	return JOB_OK;
}

static struct job_status
job_visit_stmt_list(struct cfg_ctx *ctx, struct stg_module *mod,
					job_visit_stmt_list_t *data)
{
	struct cfg_node *stmt = data->first_stmt;
	while (stmt) {
		assert(stmt->type == CFG_NODE_STMT);

		dispatch_stmt(ctx, mod, data->scope, stmt);
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
discover_config_files(struct cfg_ctx *ctx, struct stg_module *mod,
					  struct string cfg_dir)
{
	/* // TODO: Ensure zero-terminated */
	char *paths[] = {cfg_dir.text, NULL};

	FTS *ftsp;
	ftsp = fts_open(paths, FTS_PHYSICAL, NULL);

	struct scope *scope = &mod->root_scope;

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

				struct atom *atom = atom_create(mod->atom_table, name);
				struct string file_name = {0};
				struct scope *mod_scope;

				string_duplicate(&ctx->vm->memory, &file_name, path);

				mod_scope = scope_push(scope);
				scope_insert(scope, atom, SCOPE_ANCHOR_NONE,
							 OBJ_NONE, mod_scope);

				DISPATCH_JOB(ctx, mod, parse_file, CFG_PHASE_DISCOVER,
							 .mod_scope = mod_scope,
							 .file_name = file_name);
			}
		} break;

		case FTS_D:
			if (f->fts_namelen) {
				scope = scope_push(scope);

				struct string name;
				name.text = f->fts_name;
				name.length = f->fts_namelen;

				struct atom *atom = atom_create(mod->atom_table, name);

				scope_insert(scope->parent, atom, SCOPE_ANCHOR_NONE,
							 OBJ_NONE, scope);
			}
			break;

		case FTS_DP:
			if (f->fts_namelen) {
				scope = scope->parent;
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

#define CFG_DEBUG_JOBS 0

static void cfg_exec_job(struct cfg_ctx *ctx, struct cfg_job *job)
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

#if CFG_DEBUG_JOBS
	printf("[%zu] Dispatching 0x%03zx %.*s... ",
		   job->last_dispatch_time,
		   job->id,
		   LIT(cfg_job_names[job->type]));
#endif

	struct job_status res;

	switch (job->type) {
#define CFG_JOB(name, data) case CFG_JOB_##name: res = job_##name(ctx, job->mod, &job->name); break;
#include "config_jobs.h"
#undef CFG_JOB

	case CFG_JOBS_LEN:
		assert(false);
		break;
	}

	job->status = res.status;

	switch (res.status) {
	case JOB_STATUS_OK: {
		struct cfg_job *dep = job->first_dependant_node;
		while (dep) {
			struct cfg_job *next = dep->next_job;

			dep->next_job = NULL;
			append_job(ctx, ctx->current_phase, dep);

			dep = next;
		}

		job->first_dependant_node = NULL;
#if CFG_DEBUG_JOBS
		printf(TERM_COLOR_GREEN("ok") "\n");
#endif
	} break;

	case JOB_STATUS_ERROR: {
		ctx->num_jobs_failed += 1;

		size_t num_canceled = 0;

		for (struct cfg_job *dep = job->first_dependant_node;
			 dep != NULL;
			 dep = dep->next_job) {
			dep->status = JOB_STATUS_ERROR;
			num_canceled += 1;
		}

#if CFG_DEBUG_JOBS
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
#if CFG_DEBUG_JOBS
			printf(TERM_COLOR_YELLOW("=== yield for 0x%03zx %.*s ===") "\n",
				   res.yield_for->id, LIT(cfg_job_names[res.yield_for->type]));
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
#if CFG_DEBUG_JOBS
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
#if CFG_DEBUG_JOBS
		printf(TERM_COLOR_YELLOW("=== yield for phase %i ===") "\n",
			   res.yield_for_phase);
#endif
		break;

	case JOB_STATUS_IDLE:
		panic("Job returned idle as result.");
		break;
	}

	if (job->status == JOB_STATUS_YIELD) {
		if (ctx->time - ctx->last_completed_job_time > CFG_JOB_MAX_CONSECUTIVE_YIELDS) {
#if CFG_DEBUG_JOBS
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
cfg_compile(struct vm *vm, struct string cfg_dir)
{
	struct cfg_ctx ctx = {0};

	ctx.vm = vm;
	ctx.mem = &vm->memory;

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

	discover_config_files(&ctx, mod, cfg_dir);

	for (; ctx.current_phase < CFG_NUM_PHASES; ctx.current_phase += 1) {
		struct cfg_phase *phase = &ctx.phases[ctx.current_phase];
#if CFG_DEBUG_JOBS
		printf("\n" TERM_COLOR_BLUE("=== Phase %i ===") "\n",
			   ctx.current_phase);
#endif
		while (phase->job_head) {
			struct cfg_job *job = phase->job_head;
			phase->job_head = job->next_job;
			if (!phase->job_head) {
				phase->job_end = NULL;
			}

			cfg_exec_job(&ctx, job);
		}
	}

	if (ctx.num_jobs_failed > 0 || ctx.num_errors > 0) {
		printf("\n" TERM_COLOR_RED("Compilation failed! "
								   "(%zu jobs failed, %zu errors)") "\n",
			   ctx.num_jobs_failed, ctx.num_errors);
	}

	scope_print(vm, &vm->root_scope);

	return 0;
}
