#include "config.h"
#include "utils.h"
#include "dlist.h"
#include "config_func.h"
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
	enum cfg_job_type type;
	enum job_status_code status;

	struct cfg_job *next_job;
	struct cfg_job *first_dependant_node;

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

struct cfg_ctx {
	struct vm *vm;
	struct arena *mem;

	size_t next_job_id;

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
_expr_to_string_internal(struct arena *mem, struct string *str,
						 struct cfg_node *node)
{
	switch (node->type) {
	case CFG_NODE_ACCESS:
		_expr_to_string_internal(mem, str, node->ACCESS.lhs);
		arena_string_append(mem, str, STR("."));
		_expr_to_string_internal(mem, str, node->ACCESS.rhs);
		break;

	case CFG_NODE_SUBSCRIPT:
		_expr_to_string_internal(mem, str, node->SUBSCRIPT.lhs);
		arena_string_append(mem, str, STR("["));
		_expr_to_string_internal(mem, str, node->SUBSCRIPT.index);
		arena_string_append(mem, str, STR("]"));
		break;

	case CFG_NODE_IDENT:
		arena_string_append(mem, str, node->IDENT->name);
		break;

	default:
		panic("Invalid node '%.*s' as expression.",
				LIT(cfg_node_names[node->type]));
		break;
	}
}

static struct string
expr_to_string(struct arena *mem, struct cfg_node *node)
{
	struct string str = {0};

	/* str.text = (uint8_t *)(mem->data + mem->head); */
	str.text = arena_alloc(mem, 0);
	str.length = 0;

	_expr_to_string_internal(mem, &str, node);

	return str;
}

static void
append_job(struct cfg_ctx *ctx, enum cfg_compile_phase ph, struct cfg_job *job)
{
	assert(ph < CFG_NUM_PHASES);
	assert(ph >= ctx->current_phase);

	struct cfg_phase *phase = &ctx->phases[ph];

	if (phase->job_end) {
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

#define DISPATCH_JOB(ctx, name, phase, ...)						\
	dispatch_job((ctx), (phase),								\
				 (struct cfg_job){								\
					 .type=CFG_JOB_##name,						\
						 .name = (job_##name##_t){__VA_ARGS__}	\
				 })

static struct scope *
instantiate_scope_by_access_pattern(struct cfg_ctx *ctx,
									struct scope *parent,
									struct cfg_node *node)
{
	switch (node->type) {
	case CFG_NODE_ACCESS: {
		struct scope *scope;

		scope = instantiate_scope_by_access_pattern(ctx, parent, node->ACCESS.lhs);
		if (!scope) {
			return NULL;
		}

		return instantiate_scope_by_access_pattern(ctx, scope, node->ACCESS.rhs);
	} break;

	case CFG_NODE_IDENT: {
		struct scope *scope;
		struct scope_entry entry;

		if (scope_local_lookup(parent, node->IDENT, &entry) == 0) {
			if (entry.id != 0 || !entry.scope) {
				cfg_error(ctx, node, "'%.*s' already exists, and is not a namespace.");
				return NULL;
			}
		}

		scope = scope_push(parent);
		scope_insert(parent, node->IDENT, OBJ_NONE, scope);
		return scope;
	} break;

	default:
		panic("Invalid node in access pattern.");
		break;
	}

	return NULL;
}

static void dispatch_stmt(struct cfg_ctx *ctx, struct scope *parent_scope, struct cfg_node *stmt)
{
	assert(stmt->type == CFG_NODE_STMT);

	// TODO: Handle attributes

	struct cfg_node *node = stmt->STMT.stmt;

	if (!node) {
		return;
	}

	switch (node->type) {
	case CFG_NODE_DECL_STMT: {
		DISPATCH_JOB(ctx, visit_decl_stmt, CFG_PHASE_DISCOVER,
					 .scope = parent_scope,
					 .stmt = node);
	} break;

	case CFG_NODE_USE_STMT:
		break;

	case CFG_NODE_ASSERT_STMT:
		break;

	case CFG_NODE_FUNC_STMT:
		DISPATCH_JOB(ctx, func_decl, CFG_PHASE_DISCOVER,
					 .scope = parent_scope,
					 .node = node);
		break;

	case CFG_NODE_ASSIGN_STMT:
		break;

	case CFG_NODE_BIND:
		break;

	case CFG_NODE_NAMESPACE: {
		struct scope *ns_scope;
		ns_scope = instantiate_scope_by_access_pattern(ctx, parent_scope, node->NAMESPACE.name);

		DISPATCH_JOB(ctx, visit_stmt_list, CFG_PHASE_DISCOVER,
					 .scope = ns_scope,
					 .first_stmt = node->NAMESPACE.body);
	} break;

	default:
		panic("Invalid node '%.*s' as statement.",
				LIT(cfg_node_names[node->type]));
		break;
	}
}

static struct cfg_job *
resolve_type(struct cfg_ctx *ctx, struct cfg_node *node,
			 struct cfg_node *args, struct scope *scope,
			 type_id *out_type, struct scope **out_child_scope)
{
	struct cfg_job *job = NULL;
	switch (node->type) {
	case CFG_NODE_ENUM_DECL:
		job = DISPATCH_JOB(ctx, enum_decl, CFG_PHASE_DISCOVER,
						   .scope = scope,
						   .node  = node,
						   .args  = args,
						   .out_type = out_type,
						   .out_child_scope = out_child_scope);
		break;

	case CFG_NODE_OBJ_DECL:
		job = DISPATCH_JOB(ctx, obj_decl, CFG_PHASE_RESOLVE,
						   .scope = scope,
						   .node  = node,
						   .args  = args,
						   .out_type = out_type);
		break;

	case CFG_NODE_TUPLE_DECL:
		job = DISPATCH_JOB(ctx, tuple_decl, CFG_PHASE_RESOLVE,
						   .scope = scope,
						   .node  = node,
						   .args  = args,
						   .out_type = out_type);
		break;

	case CFG_NODE_FUNC_PROTO:
		job = DISPATCH_JOB(ctx, func_proto_decl, CFG_PHASE_RESOLVE,
						   .scope = scope,
						   .node  = node,
						   /* .args  = node->DECL_STMT.args, */
						   .out_type = out_type);
		break;

	case CFG_NODE_IDENT:
	case CFG_NODE_ACCESS:
	case CFG_NODE_SUBSCRIPT:
		job = DISPATCH_JOB(ctx, resolve_type_l_expr, CFG_PHASE_RESOLVE,
						   .scope = scope,
						   .node = node,
						   .out_type = out_type);
		break;

	default:
		panic("Invalid node '%.*s' as type expression.",
			  LIT(cfg_node_names[node->type]));
		break;

	}

	assert(job != NULL);

	return job;
}

static struct job_status
job_visit_decl_stmt(struct cfg_ctx *ctx, job_visit_decl_stmt_t *data)
{
	struct cfg_node *node = data->stmt;
	assert(node->DECL_STMT.decl != NULL);

	if (!data->initialized) {
		data->initialized = true;
		struct cfg_job *job;

		job = resolve_type(ctx, node->DECL_STMT.decl, node->DECL_STMT.args,
						   data->scope, &data->type, &data->child_scope);

		assert(job != NULL);

		return JOB_YIELD_FOR(job);
	}

	assert(data->type != TYPE_UNSET);

	obj_id object;

	object = obj_register_type(ctx->vm, data->type);

	struct atom *name;
	assert(node->DECL_STMT.name->type == CFG_NODE_IDENT);
	name = node->DECL_STMT.name->IDENT;

	scope_insert(data->scope, name, object, data->child_scope);

	return JOB_OK;
}

static struct job_status
job_func_proto_decl(struct cfg_ctx *ctx, job_func_proto_decl_t *data)
{
	assert(data->out_type != NULL);

	switch (data->state) {
	case CFG_FUNC_PROTO_DECL_RESOLVE_PARAMS: {
		switch (data->node->type) {
		case CFG_NODE_FUNC_PROTO: {
			struct cfg_job *job;
			job = resolve_type(ctx, data->node->FUNC_PROTO.params,
							   NULL, data->scope, &data->params, NULL);

			data->state = CFG_FUNC_PROTO_DECL_RESOLVE_RET;

			return JOB_YIELD_FOR(job);
		} break;

		case CFG_NODE_TUPLE_DECL: {
			struct cfg_job *job;
			job = resolve_type(ctx, data->node, NULL, data->scope, &data->params, NULL);

			data->state = CFG_FUNC_PROTO_DECL_FINALIZE;

			data->ret = ctx->vm->default_types.template_param;

			return JOB_YIELD_FOR(job);
		} break;

		case CFG_NODE_IDENT: {
			struct type_tuple_item item = {0};

			item.name = data->node->IDENT;
			item.type = TYPE_TEMPLATE_PARAM;

			data->params = type_register_tuple(ctx->vm, &item, 1);
			data->ret = TYPE_TEMPLATE_PARAM;

			data->state = CFG_FUNC_PROTO_DECL_FINALIZE;

			return JOB_YIELD;
		} break;

		default:
			panic("Invalid node '%.*s' in function prototype.",
				LIT(cfg_node_names[data->node->type]));
		}
	} break;

	case CFG_FUNC_PROTO_DECL_RESOLVE_RET: {
		switch (data->node->type) {
		case CFG_NODE_FUNC_PROTO: {
			struct cfg_job *job;
			job = resolve_type(ctx, data->node->FUNC_PROTO.ret,
							   NULL, data->scope, &data->params, NULL);

			data->state = CFG_FUNC_PROTO_DECL_FINALIZE;

			return JOB_YIELD_FOR(job);
		} break;

		default:
			panic("Invalid node '%.*s' in function prototype resolve ret.",
				LIT(cfg_node_names[data->node->type]));
		}
	} break;

	case CFG_FUNC_PROTO_DECL_FINALIZE: {
		*data->out_type = type_register_function(ctx->vm, data->params, data->ret);

		return JOB_OK;
	} break;

	}

	return JOB_ERROR;
}

static struct job_status
job_func_decl(struct cfg_ctx *ctx, job_func_decl_t *data)
{
	assert(data->node->type == CFG_NODE_FUNC_STMT);
	assert(data->node->FUNC_STMT.ident->type == CFG_NODE_IDENT);

	if (!data->initialized) {
		data->initialized = true;

		struct cfg_job *func_job;
		func_job = DISPATCH_JOB(ctx, compile_func, CFG_PHASE_RESOLVE,
								.scope = data->scope,
								.proto_node = data->node->FUNC_STMT.proto,
								.body_node  = data->node->FUNC_STMT.body,
								.out_func_obj = &data->func_object);

		return JOB_YIELD_FOR(func_job);
	}

	// The object should have been defined before we are called
	// again.
	if (data->func_object == OBJ_NONE) {
		return JOB_ERROR;
	}

	struct atom *name = data->node->FUNC_STMT.ident->IDENT;

	scope_insert(data->scope, name, OBJ_NONE, NULL);

	return JOB_OK;
}

static struct job_status
job_compile_func(struct cfg_ctx *ctx, job_compile_func_t *data)
{
	switch (data->state) {
	case CFG_COMPILE_FUNC_IDLE: {
		data->state = CFG_COMPILE_FUNC_RESOLVE_SIGNATURE;

		struct cfg_job *job;

		job = DISPATCH_JOB(ctx, func_proto_decl,
						   .scope = data->scope,
						   .node  = data->proto_node,
						   .out_type = &data->proto);

		return JOB_YIELD_FOR(job);
	} break;

	case CFG_COMPILE_FUNC_RESOLVE_SIGNATURE: {
		data->state = CFG_COMPILE_FUNC_VISIT_BODY;

		data->func_ctx.outer_scope = data->scope;
		data->func_ctx.inner_scope = scope_push(data->scope);

		// @TODO: Insert params into inner scope

		struct cfg_job *job;

		job = DISPATCH_JOB(ctx, visit_expr, CFG_PHASE_RESOLVE,
						   .func_ctx = &data->func_ctx,
						   .node     = data->body_node,
						   .out_func = &data->func);

		return JOB_YIELD_FOR(job);
	} break;

	case CFG_COMPILE_FUNC_VISIT_BODY: {
		return JOB_OK;
	} break;


	/* case CFG_COMPILE_FUNC_EVAL_TYPES: { */
	/* } break; */

	/* case CFG_COMPILE_FUNC_EVAL_BODY_TYPE_CONSTRAINTS: { */
	/* } break; */

	/* case CFG_COMPILE_FUNC_RESOLVE_TYPES: { */
	/* } break; */

	/* case CFG_COMPILE_FUNC_OPTIMIZE: { */
	/* } break; */

	/* case CFG_COMPILE_FUNC_GEN_INSTRUCTIONS: { */
	/* } break; */

	}

	return JOB_ERROR;
}

static struct job_status
job_visit_expr(struct cfg_ctx *ctx, job_visit_expr_t *data)
{
	struct cfg_job *job;

	switch (data->node->type) {

	case CFG_NODE_ACCESS:
		break;

	case CFG_NODE_SUBSCRIPT:
		break;

	case CFG_NODE_BIN_OP: {
		struct cfg_node *node_to_process = NULL;

		/* State machine
		 * 0: Initialize func and visit lhs
		 * 1: Add the result of lhs, and visit rhs
		 * 2: Add the result of rhs and finalize
		 */
		assert(data->iter <= 2);

		if (data->iter != 0) {
			assert(data->tmp_func != NULL);
			cfg_func_call_add_arg(*data->out_func,
								  data->tmp_func);
		}

		switch (data->iter) {
		case 0:
			*data->out_func =
				cfg_func_call(ctx->vm, data->func_ctx,
							  binop_atom(&ctx->vm->atom_table,
										 data->node->BIN_OP.op));

			node_to_process = data->node->BIN_OP.lhs;
		break;

		case 1:
			node_to_process = data->node->BIN_OP.rhs;
			break;

		case 2:
			return JOB_OK;
		}

		assert(node_to_process);

		job = DISPATCH_JOB(ctx, visit_expr, CFG_PHASE_RESOLVE,
						   .func_ctx = data->func_ctx,
						   .node     = node_to_process,
						   .out_func = &data->tmp_func);
		data->iter += 1;

		return JOB_YIELD_FOR(job);
	} break;

	case CFG_NODE_LAMBDA:
		panic("TODO: Lambda");
		break;

	case CFG_NODE_FUNC_CALL:
		break;

	case CFG_NODE_TUPLE_LIT:
		break;

	case CFG_NODE_ARRAY_LIT:
		break;

	case CFG_NODE_NUM_LIT:
		*data->out_func =
			cfg_func_lit_int(ctx->vm, data->node->NUM_LIT);
		return JOB_OK;

	case CFG_NODE_STR_LIT:
		*data->out_func =
			cfg_func_lit_str(ctx->vm, data->node->STR_LIT);
		return JOB_OK;

	case CFG_NODE_IDENT:
		break;

	default:
		panic("Invalid node '%.*s' in expr.",
			  LIT(cfg_node_names[data->node->type]));
		break;
	}

	return JOB_ERROR;
}

static struct job_status
job_resolve_type_l_expr(struct cfg_ctx *ctx, job_resolve_type_l_expr_t *data)
{
	if (data->dispatched) {
		assert(data->entry.id != OBJ_NONE);
		struct object obj = get_object(&ctx->vm->store, data->entry.id);

		if (obj.type != ctx->vm->default_types.type) {
			arena_point p = arena_push(&ctx->vm->memory);

			struct type *type = &ctx->vm->store.types[obj.type];

			struct string obj_repr;
			obj_repr = type->base->repr(ctx->vm, &ctx->vm->memory, &obj);
			cfg_error(ctx, data->node, "Expected a type but got '%.*s'.", LIT(obj_repr));

			arena_pop(&ctx->vm->memory, p);

			return JOB_ERROR;
		}

		*data->out_type = type_obj_get(ctx->vm, obj);

		return JOB_OK;
	}

	struct cfg_job *job;

	job = DISPATCH_JOB(ctx, resolve_l_expr, CFG_PHASE_RESOLVE,
					   .scope = data->scope,
					   .node = data->node,
					   .out_entry = &data->entry);

	data->dispatched = true;

	return JOB_YIELD_FOR(job);
}

static struct job_status
job_resolve_l_expr(struct cfg_ctx *ctx, job_resolve_l_expr_t *data)
{
	if (!data->l_expr_top_node) {
		data->l_expr_top_node = data->node;
	}
	switch (data->node->type) {
	case CFG_NODE_ACCESS: {
		switch (data->state) {
		case CFG_BINOP_LHS: {
			struct cfg_job *lhs_job;
			lhs_job = DISPATCH_JOB(ctx, resolve_l_expr, CFG_PHASE_RESOLVE,
								   .scope = data->scope,
								   .node  = data->node->ACCESS.lhs,
								   .l_expr_top_node  = data->l_expr_top_node,
								   .local = data->local,
								   .out_entry = &data->entry);

			return JOB_YIELD_FOR(lhs_job);
		} break;

		case CFG_BINOP_RHS: {
			struct cfg_job *rhs_job;

			if (!data->entry.scope) {
				cfg_error(ctx, data->node->ACCESS.lhs, "This object does not contain any elements.");

				return JOB_ERROR;
			}

			rhs_job = DISPATCH_JOB(ctx, resolve_l_expr, CFG_PHASE_RESOLVE,
								   .scope = data->scope,
								   .node  = data->node->ACCESS.rhs,
								   .l_expr_top_node  = data->l_expr_top_node,
								   .local = true,
								   .out_entry = data->out_entry);

			return JOB_YIELD_FOR(rhs_job);
		} break;

		case CFG_BINOP_FINALIZE:
			return JOB_OK;
		}

	} break;

	case CFG_NODE_SUBSCRIPT:
		printf("TODO: Subscript.\n");
		break;

	case CFG_NODE_IDENT: {
		int err;

		if (data->local) {
			err = scope_local_lookup(data->scope,
									 data->node->IDENT,
									 data->out_entry);
		} else {
			err = scope_lookup(data->scope,
							   data->node->IDENT,
							   data->out_entry);
		}

		if (err) {
			struct arena *mem = &ctx->vm->memory;
			arena_point p = arena_push(mem);

			struct string expr;

			expr = expr_to_string(mem, data->l_expr_top_node);

			cfg_error(ctx, data->node, "Could not find '%.*s'.", LIT(expr));

			arena_pop(mem, p);
			return JOB_ERROR;
		}

		return JOB_OK;
	} break;

	default:
		panic("Invalid node '%.*s' in l-expr.",
				LIT(cfg_node_names[data->node->type]));
		break;
	}

	return JOB_ERROR;
}

int parse_config_file(struct string filename,
					  struct atom_table *table,
					  struct arena *memory,
					  unsigned int file_id,
					  struct cfg_node **out_node);

static struct job_status
job_parse_file(struct cfg_ctx *ctx, job_parse_file_t *data)
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

	err = parse_config_file(data->file_name, &ctx->vm->atom_table, &ctx->vm->memory, file_id, &node);
	if (err) {
		return JOB_ERROR;
	}

	assert(node);
	assert(node->type == CFG_NODE_MODULE);

	DISPATCH_JOB(ctx, visit_stmt_list, CFG_PHASE_DISCOVER,
				 .scope = data->mod_scope,
				 .first_stmt = node->MODULE.body);

	return JOB_OK;
}

static struct job_status
job_visit_stmt_list(struct cfg_ctx *ctx, job_visit_stmt_list_t *data)
{
	struct cfg_node *stmt = data->first_stmt;
	while (stmt) {
		assert(stmt->type == CFG_NODE_STMT);

		dispatch_stmt(ctx, data->scope, stmt);
		stmt = stmt->next_sibling;
	}

	return JOB_OK;
}

static void
enum_item_constructor(struct vm *vm, struct exec_stack *stack,
					  void *data)
{
	struct type_enum_item *item = (struct type_enum_item *)data;
	int64_t enum_value = item->value;
	assert(item->owner->size == 0); // TODO: Implement
	stack_push(stack, &enum_value, sizeof(enum_value));
}

static struct job_status
job_enum_decl(struct cfg_ctx *ctx, job_enum_decl_t *data)
{
	// @TODO: Enum should initially be dispatched in the discovery
	// phase, to discover the value constructors, but should yield
	// until the resolve phase for being instantiatiated, especially
	// when it has any items with data, or parameters.

	/* struct cfg_node *decl = data->node; */
	/* assert(decl->type == CFG_NODE_DECL_STMT); */

	struct cfg_node *node = data->node; // decl->DECL_STMT.decl;
	assert(node->type == CFG_NODE_ENUM_DECL);

	/* struct atom *name; */
	/* assert(decl->DECL_STMT.name->type == CFG_NODE_IDENT); */
	/* name = decl->DECL_STMT.name->IDENT; */

	/* if (decl->DECL_STMT.args) { */
	/* 	printf("Decl args not implemented.\n"); */
	/* 	return JOB_ERROR; */
	/* } */

	size_t length = 0;
	struct cfg_node *entry = node->ENUM_DECL.items;
	while (entry) {
		length += 1;
		entry = entry->next_sibling;
	}

	// TODO: This should be allocated temporarly and copied inside
	// type_register_enum.
	struct type_enum *result;
	struct type_enum_item *items;
	result = arena_alloc(ctx->mem, sizeof(struct type_enum));
	items  = arena_alloc(ctx->mem, sizeof(struct type_enum_item) * length);

	// TODO: Types. If any option has an associated type, the enum
	// decl has to be yielded until resolve phase.

	{
		size_t i = 0;
		entry = node->ENUM_DECL.items;
		while (entry) {
			items[i].name = entry->ENUM_ITEM.name;
			// TODO: Types
			items[i].type = TYPE_NONE;

			i += 1;
			entry = entry->next_sibling;
		}
		assert(i == length);
	}

	result->num_items = length;
	result->items = items;

	type_id type;
	type = type_register_enum(ctx->vm, result);

	*data->out_type = type;

	if (data->out_child_scope != NULL) {
		struct scope *enum_scope;
		enum_scope = scope_push(data->scope);

		for (size_t i = 0; i < result->num_items; i++) {
			struct type_enum_item *item = &result->items[i];

			// TODO: This shuold be automated inside type_register_enum.
			obj_id item_constructor;
			item_constructor =
				obj_register_builtin_func(ctx->vm, TYPE_NONE, type,
										enum_item_constructor, (void *)item);

			scope_insert(enum_scope, item->name, item_constructor, NULL);
		}

		*data->out_child_scope = enum_scope;
	}

	return JOB_OK;
}

static struct job_status
job_obj_decl(struct cfg_ctx *ctx, job_obj_decl_t *data)
{
	struct cfg_node *node = data->node;
	assert(node->type == CFG_NODE_OBJ_DECL);

	return JOB_ERROR;
}

static struct job_status
job_tuple_decl(struct cfg_ctx *ctx, job_tuple_decl_t *data)
{
	struct cfg_node *node = data->node;
	assert(node->type == CFG_NODE_TUPLE_DECL);

	if (data->state == CFG_TUPLE_DECL_GET_LENGTH) {
		size_t len = 0;
		struct cfg_node *n = node->TUPLE_DECL.items;

		while (n != NULL) {
			assert(n->type == CFG_NODE_TUPLE_DECL_ITEM);
			len += 1;
			n = n->next_sibling;
		}

		data->num_items = len;
		data->items = calloc(data->num_items,
							 sizeof(struct type_tuple_item));

		data->next_node_to_resolve = node->TUPLE_DECL.items;
		data->num_nodes_resolved = 0;

		data->state = CFG_TUPLE_DECL_RESOLVE_TYPES;
	}

	if (data->state == CFG_TUPLE_DECL_RESOLVE_TYPES) {
		if (data->num_nodes_resolved < data->num_items) {
			struct cfg_node *n = data->next_node_to_resolve;
			size_t i = data->num_nodes_resolved;

			data->items[i].name = n->TUPLE_DECL_ITEM.name;

			struct cfg_job *item_job;

			item_job = resolve_type(ctx, n->TUPLE_DECL_ITEM.type, NULL,
									data->scope, &data->items[i].type, NULL);

			data->num_nodes_resolved += 1;
			data->next_node_to_resolve = n->next_sibling;

			return JOB_YIELD_FOR(item_job);
		} else {
			assert(data->next_node_to_resolve == NULL);

			data->state = CFG_TUPLE_DECL_FINALIZE;
		}
	}

	if (data->state == CFG_TUPLE_DECL_FINALIZE) {
		*data->out_type = type_register_tuple(ctx->vm, data->items, data->num_items);
		free(data->items);

		return JOB_OK;
	}

	return JOB_ERROR;
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
discover_config_files(struct cfg_ctx *ctx, struct string cfg_dir)
{
	/* // TODO: Ensure zero-terminated */
	char *paths[] = {cfg_dir.text, NULL};

	FTS *ftsp;
	ftsp = fts_open(paths, FTS_PHYSICAL, NULL);

	struct scope *scope = &ctx->vm->root_scope;

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

				struct atom *atom = atom_create(&ctx->vm->atom_table, name);
				struct string file_name = {0};
				struct scope *mod_scope;

				string_duplicate(&ctx->vm->memory, &file_name, path);

				mod_scope = scope_push(scope);
				scope_insert(scope, atom, OBJ_NONE, mod_scope);

				DISPATCH_JOB(ctx, parse_file, CFG_PHASE_DISCOVER,
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

				struct atom *atom = atom_create(&ctx->vm->atom_table, name);

				scope_insert(scope->parent, atom, OBJ_NONE, scope);
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

static void cfg_exec_job(struct cfg_ctx *ctx, struct cfg_job *job)
{
	assert(job->status == JOB_STATUS_IDLE || job->status == JOB_STATUS_YIELD);

	printf("Dispatching 0x%03zx %.*s... ", job->id, LIT(cfg_job_names[job->type]));

	struct job_status res;

	switch (job->type) {
#define CFG_JOB(name, data) case CFG_JOB_##name: res = job_##name(ctx, &job->name); break;
#include "config_jobs.h"
#undef CFG_JOB

	case CFG_JOBS_LEN:
		assert(false);
		break;
	}

	job->status = res.status;

	switch (res.status) {
	case JOB_STATUS_OK:
		if (job->first_dependant_node) {
			append_job(ctx, ctx->current_phase, job->first_dependant_node);
			job->first_dependant_node = NULL;
		}
		printf("ok\n");
		break;

	case JOB_STATUS_ERROR: {
		ctx->num_jobs_failed += 1;

		size_t num_canceled = 0;

		for (struct cfg_job *dep = job->first_dependant_node;
			 dep != NULL;
			 dep = dep->next_job) {
			dep->status = JOB_STATUS_ERROR;
			num_canceled += 1;
		}

		if (num_canceled > 0) {
			printf("=== error (%zu dependenc%s canceled) ===\n", num_canceled,
				   (num_canceled == 1 ? "y" : "ies"));
		} else {
			printf("=== error ===\n");
		}
	} break;

	case JOB_STATUS_YIELD:
		if (res.yield_for) {
			printf("=== yield for 0x%03zx %.*s ===\n",
				   res.yield_for->id, LIT(cfg_job_names[res.yield_for->type]));

			if (res.yield_for->status == JOB_STATUS_ERROR) {
				job->status = JOB_STATUS_ERROR;
				break;

			} else if (res.yield_for->status == JOB_STATUS_OK) {
				job->next_job = NULL;
				append_job(ctx, ctx->current_phase, job);
				break;
			}

			// TODO: Thread-safe
			job->next_job = res.yield_for->first_dependant_node;
			res.yield_for->first_dependant_node = job;
		} else {
			printf("=== yield ===\n");

			job->next_job = NULL;
			append_job(ctx, ctx->current_phase, job);
		}
		break;

	case JOB_STATUS_YIELD_FOR_PHASE:
		job->status = JOB_STATUS_YIELD;
		append_job(ctx, res.yield_for_phase, job);
		printf("=== yield for phase %i ===\n", res.yield_for_phase);
		break;

	case JOB_STATUS_IDLE:
		panic("Job returned idle as result.");
		break;
	}
}

int
cfg_compile(struct vm *vm, struct string cfg_dir)
{
	struct cfg_ctx ctx = {0};

	ctx.vm = vm;
	ctx.mem = &vm->memory;

	discover_config_files(&ctx, cfg_dir);

	for (; ctx.current_phase < CFG_NUM_PHASES; ctx.current_phase += 1) {
		struct cfg_phase *phase = &ctx.phases[ctx.current_phase];
		printf("\n=== Phase %i ===\n", ctx.current_phase);
		while (phase->job_head) {
			struct cfg_job *job = phase->job_head;
			phase->job_head = job->next_job;

			cfg_exec_job(&ctx, job);
		}
	}

	if (ctx.num_jobs_failed > 0 || ctx.num_errors > 0) {
		printf("Compilation failed! (%zu jobs failed, %zu errors)\n",
			   ctx.num_jobs_failed, ctx.num_errors);
	}

	printf("\n");

	scope_print(vm, &vm->root_scope);

	return 0;
}
