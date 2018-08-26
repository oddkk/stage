#include "config.h"
#include "stage.h"
#include "device_type.h"
#include "device.h"
#include "utils.h"
#include "dlist.h"
#include "scope_lookup.h"
#include "access_pattern.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define APPLY_DEBUG 0

enum apply_node_type {
	APPLY_NODE_DEVICE_TYPE,
	APPLY_NODE_DEVICE_TYPE_INPUT,
	APPLY_NODE_DEVICE_TYPE_OUTPUT,

	APPLY_NODE_DEVICE,
	/* APPLY_NODE_DEVICE_ATTR, */

	APPLY_NODE_TYPE_DECL,

	APPLY_NODE_TYPE_L_EXPR,
	APPLY_NODE_TYPE_TUPLE,
	APPLY_NODE_TYPE_ARRAY,
	APPLY_NODE_TYPE_SUBRANGE,
	APPLY_NODE_TYPE_TEMPLATE_FIELD,

	APPLY_NODE_BINARY_OP,
	APPLY_NODE_ACCESS,
	APPLY_NODE_ACCESS_INDEX,
	APPLY_NODE_ACCESS_INDEX_RANGE,
	APPLY_NODE_BIND,
	APPLY_NODE_IDENT,
	APPLY_NODE_NUMLIT,
	APPLY_NODE_TUPLE_LIT,
	APPLY_NODE_ARRAY_LIT,

	APPLY_NODE_L_VALUE,
};

const char *apply_node_name(enum apply_node_type type) {
#define APPLY_NODE_NAME(name) case APPLY_NODE_##name: return #name;
	switch (type) {
		APPLY_NODE_NAME(DEVICE_TYPE);
		APPLY_NODE_NAME(DEVICE_TYPE_INPUT);
		APPLY_NODE_NAME(DEVICE_TYPE_OUTPUT);
		APPLY_NODE_NAME(DEVICE);
		/* APPLY_NODE_NAME(DEVICE_ATTR); */

		APPLY_NODE_NAME(TYPE_DECL);

		APPLY_NODE_NAME(TYPE_L_EXPR);
		APPLY_NODE_NAME(TYPE_TUPLE);
		APPLY_NODE_NAME(TYPE_ARRAY);
		APPLY_NODE_NAME(TYPE_SUBRANGE);
		APPLY_NODE_NAME(TYPE_TEMPLATE_FIELD);

		APPLY_NODE_NAME(BINARY_OP);
		APPLY_NODE_NAME(ACCESS);
		APPLY_NODE_NAME(ACCESS_INDEX);
		APPLY_NODE_NAME(ACCESS_INDEX_RANGE);
		APPLY_NODE_NAME(BIND);
		APPLY_NODE_NAME(IDENT);
		APPLY_NODE_NAME(NUMLIT);
		APPLY_NODE_NAME(TUPLE_LIT);
		APPLY_NODE_NAME(ARRAY_LIT);

		APPLY_NODE_NAME(L_VALUE);
	}
#undef APPLY_NODE_NAME
	return "(unknown apply node)";
}

enum apply_dispatch_result {
	DISPATCH_DONE  = 0,
	DISPATCH_YIELD = 1,
	DISPATCH_ERROR = -1,
};

struct apply_node;

struct apply_tuple_member {
	struct atom *name;
	struct apply_node *type;
	type_id type_id;
};

struct apply_tuple_lit_member {
	struct atom *name;
	struct apply_node *expr;
	type_id type_id;
};

struct apply_node {
	struct apply_node *next;

	enum apply_node_type type;
	struct config_node *cnode;

	size_t generation;

	union {
		struct {
			struct scoped_hash *scope;
			struct device_type *type;
			struct atom *name;
			struct apply_node *params;
			struct type_template_context params_type;
			bool visited;
			bool complete;

			size_t missing_inputs;
			size_t missing_outputs;
		} device_type;

		struct {
			struct atom *name;
			struct apply_node *owner;
			struct apply_node *type;

			struct scope_lookup type_lookup;
		} device_type_input;

		struct {
			struct atom *name;
			struct apply_node *owner;
			struct apply_node *type;
			struct scope_lookup type_lookup;
		} device_type_output;

		struct {
			bool visited;

			struct device *device;
			struct atom *name;
			struct scoped_hash *scope;
			struct apply_node *type;
			struct scope_lookup type_lookup;
			struct apply_node *args;
		} device;

		struct {
			struct apply_node *name;
			struct apply_node *value;

			struct apply_node *device;

			struct scope_lookup lookup;
		} device_attr;

		struct {
			struct atom *name;
			struct apply_node *type;
			struct scoped_hash *scope;
		} type_decl;

		struct {
			struct apply_node *expr;
			struct scope_lookup lookup;

			type_id type;
		} type_l_expr;

		struct {
			struct atom *name;
			bool named;
			struct apply_tuple_member *members;
			size_t num_members;

			type_id type;
			struct type_template_context *template;
		} type_tuple;

		struct {
			struct atom *name;
			struct apply_node *type_node;
			struct apply_node *length_node;
			struct access_pattern length_template;
			bool template_length;

			type_id type;
			struct type_template_context *template;
		} type_array;

		struct {
			struct atom *name;
			struct apply_node *lhs;
			struct apply_node *low;
			struct apply_node *high;

			scalar_value value_low;
			scalar_value value_high;

			type_id type;
			// @TODO: Make subrange type templatable.
			struct type_template_context *template;
		} type_subrange;

		struct {
			struct apply_node *expr;

			type_id type;
			struct access_pattern pattern;
			struct type_template_context *template;
		} type_template_field;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;
			struct scoped_hash *scope;

			struct scope_lookup *lookup;
			struct access_pattern *pattern;
		} access;

		struct {
			struct apply_node *lhs;
			struct apply_node *index;
			struct scoped_hash *scope;

			scalar_value value_index;

			struct scope_lookup *lookup;
			struct access_pattern *pattern;
		} access_index;

		struct {
			struct apply_node *lhs;
			struct apply_node *low_index;
			struct apply_node *high_index;
			struct scoped_hash *scope;

			scalar_value value_low_index;
			scalar_value value_high_index;

			struct scope_lookup *lookup;
			struct access_pattern *pattern;
		} access_index_range;

		struct {
			struct scoped_hash *scope;
			struct atom *name;
			bool local_lookup;

			struct scope_lookup *lookup;
			struct access_pattern *pattern;
		} ident;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;
			enum config_binary_op op;
			type_id type;
		} binary_op;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;

			struct scope_lookup lookup_lhs;
			struct scope_lookup lookup_rhs;
		} bind;

		struct {
			scalar_value numlit;
		} literal;

		struct {
			struct apply_tuple_lit_member *members;
			size_t num_members;
			bool named;
			type_id type;
		} tuple_lit;

		struct {
			struct apply_node **items;
			size_t length;
			type_id type;
		} array_lit;

		struct {
			struct apply_node *expr;
			struct scope_lookup lookup;
		} l_value;
	};

	enum apply_dispatch_result state;
};

struct apply_context {
	struct apply_node *queue;
	struct apply_node *queue_tail;

	struct stage *stage;
};

static inline enum apply_dispatch_result apply_node_state(struct apply_node *node)
{
	return node->state;
}

#define WAIT_FOR(node) if (apply_node_state(node) != DISPATCH_DONE) { return apply_node_state(node); }


#if APPLY_DEBUG
static void apply_debug(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vfprintf(stdout, fmt, ap);
	fprintf(stdout, "\n");
	va_end(ap);
}
#else
#define apply_debug(...)
#endif

static void apply_begin_error(struct config_node *node)
{
	fprintf(stderr, "%zu:%zu: ", node->from.line, node->from.column);
}

static void apply_end_error(struct config_node *node)
{
	fprintf(stderr, "\n");
}

static void apply_error(struct config_node *node, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	apply_begin_error(node);
	vfprintf(stderr, fmt, ap);
	apply_end_error(node);
	va_end(ap);
}

static void print_l_expr(struct config_node *expr)
{
	switch (expr->type) {
	case CONFIG_NODE_IDENT:
		if (expr->ident) {
			fprintf(stderr, "%.*s", ALIT(expr->ident));
		} else {
			fprintf(stderr, "_");
		}
		break;

	case CONFIG_NODE_BINARY_OP:
		print_l_expr(expr->binary_op.lhs);

		if (expr->binary_op.op == CONFIG_OP_SUBSCRIPT) {
			fprintf(stderr, "[");
			fprintf(stderr, "(expr)");
			fprintf(stderr, "]");
		} else if (expr->binary_op.op == CONFIG_OP_ACCESS) {
			fprintf(stderr, ".");
			print_l_expr(expr->binary_op.rhs);
		} else {
			fprintf(stderr, "(non l-expr)");
		}
		break;

	case CONFIG_NODE_SUBSCRIPT_RANGE:
		print_l_expr(expr->subscript_range.lhs);

		fprintf(stderr, "[");
		fprintf(stderr, "(expr)..(expr)");
		fprintf(stderr, "]");

		break;

	default:
		fprintf(stderr, "(non l-expr)");
		break;
	}
}


static bool apply_expect_lookup_result(enum scope_entry_kind expected,
									   enum scope_entry_kind found,
									   struct apply_node *node)
{
	if (expected != found) {
		apply_begin_error(node->cnode);
		fprintf(stderr, "'");
		print_l_expr(node->cnode);
		fprintf(stderr, "' is a %s, but a %s was expected.",
				humanreadable_scope_entry(found),
				humanreadable_scope_entry(expected));
		apply_end_error(node->cnode);

		return false;
	}

	return true;
}

static void apply_l_expr_not_found(char *expected,
								   struct apply_node *node)
{
	apply_begin_error(node->cnode);
	fprintf(stderr, "Could not find the %s '", expected);
	print_l_expr(node->cnode);
	fprintf(stderr, "'.");
	apply_end_error(node->cnode);
}

static struct apply_node *alloc_apply_node(struct apply_context *ctx, enum apply_node_type type, struct config_node *cnode)
{
	struct apply_node *node;
	node = calloc(1, sizeof(struct apply_node));
	node->type = type;
	node->cnode = cnode;
	node->state = DISPATCH_YIELD;
	return node;
}

static void push_apply_node(struct apply_context *ctx, struct apply_node *node)
{
	// Only append the node if it is not already queued.
	if (node->next == NULL) {
		// @TODO: Make this concurrent-safe
		if (ctx->queue_tail) {
			assert(ctx->queue);
			ctx->queue_tail->next = node;
		} else {
			assert(!ctx->queue);
			ctx->queue = node;
		}
		ctx->queue_tail = node;
	}
}

static struct apply_node *pop_apply_node(struct apply_context *ctx)
{
	struct apply_node *result;

	result = ctx->queue;
	// @TODO: Make this concurrent-safe
	ctx->queue = result->next;
	result->next = NULL;
	if (ctx->queue_tail == result) {
		ctx->queue_tail = NULL;
	}

	return result;
}

static struct apply_node *apply_discover_expr(struct apply_context *ctx,
											  struct scoped_hash *scope,
											  struct config_node *expr);

static struct apply_node *apply_discover_l_expr(struct apply_context *ctx,
												struct scoped_hash *scope,
												struct config_node *expr,
												struct scope_lookup *lookup,
												struct access_pattern *pattern)
{
	switch (expr->type) {
	case CONFIG_NODE_IDENT: {
		struct apply_node *node;

		node = alloc_apply_node(ctx, APPLY_NODE_IDENT, expr);
		node->ident.name = expr->ident;
		node->ident.scope = scope;
		node->ident.lookup = lookup;
		node->ident.pattern = pattern;

		push_apply_node(ctx, node);

		return node;
	} break;

	case CONFIG_NODE_BINARY_OP: {
		if (expr->binary_op.op == CONFIG_OP_ACCESS) {
			struct apply_node *node;
			node = alloc_apply_node(ctx, APPLY_NODE_ACCESS, expr);
			node->access.lookup = lookup;
			node->access.pattern = pattern;
			node->access.scope = scope;
			node->access.lhs = apply_discover_l_expr(ctx, scope, expr->binary_op.lhs, lookup, pattern);

			push_apply_node(ctx, node);

			return node;
		}
		else if (expr->binary_op.op == CONFIG_OP_SUBSCRIPT) {
			struct apply_node *node;
			node = alloc_apply_node(ctx, APPLY_NODE_ACCESS_INDEX, expr);
			node->access_index.lookup = lookup;
			node->access_index.pattern = pattern;
			node->access_index.scope = scope;
			node->access_index.lhs
				= apply_discover_l_expr(ctx, scope, expr->binary_op.lhs, lookup, pattern);

			node->access_index.index
				= apply_discover_expr(ctx, scope, expr->binary_op.rhs);

			push_apply_node(ctx, node);

			return node;
		}
		else {
			apply_error(expr, "Invalid node in l-expression.");
		}
	} break;

	case CONFIG_NODE_SUBSCRIPT_RANGE: {
		struct apply_node *node;
		node = alloc_apply_node(ctx, APPLY_NODE_ACCESS_INDEX_RANGE, expr);
		node->access_index_range.lookup = lookup;
		node->access_index_range.pattern = pattern;
		node->access_index_range.lhs
			= apply_discover_l_expr(ctx, scope, expr->subscript_range.lhs, lookup, pattern);
		node->access_index_range.scope = scope;

		if (expr->subscript_range.low) {
			node->access_index_range.low_index
					= apply_discover_expr(ctx, scope, expr->subscript_range.low);
		}

		if (expr->subscript_range.high) {
			node->access_index_range.high_index
				= apply_discover_expr(ctx, scope, expr->subscript_range.high);
		}

		push_apply_node(ctx, node);

		return node;
	} break;

	default:
		apply_error(expr, "Invalid node '%s' in l-expression.");
		break;
	}
	return NULL;
}

static struct apply_node *apply_discover_tuple_lit(struct apply_context *ctx,
												   struct scoped_hash *scope,
												   struct config_node *expr)
{
	assert(expr->type == CONFIG_NODE_TUPLE_LIT);

	struct apply_node *node;

	node = alloc_apply_node(ctx, APPLY_NODE_TUPLE_LIT, expr);
	node->tuple_lit.named = expr->tuple_lit.named;

	for (struct config_node *item = expr->tuple_lit.first_child;
		 item;
		 item = item->next_sibling) {
		assert(item->type == CONFIG_NODE_TUPLE_LIT_ITEM);

		struct apply_tuple_lit_member *member;
		int id;
		id = dlist_alloc(node->tuple_lit.members, node->tuple_lit.num_members);
		member = &node->tuple_lit.members[id];

		if (node->tuple_lit.named) {
			member->name = item->tuple_lit_item.name;
		}
		member->expr = apply_discover_expr(ctx, scope, item->tuple_lit_item.expr);
	}

	push_apply_node(ctx, node);

	return node;
}

static struct apply_node *apply_discover_expr(struct apply_context *ctx,
											  struct scoped_hash *scope,
											  struct config_node *expr)
{
	switch (expr->type) {
	case CONFIG_NODE_BINARY_OP:
		if (expr->binary_op.op == CONFIG_OP_ACCESS) {
			struct apply_node *node;
			node = alloc_apply_node(ctx, APPLY_NODE_L_VALUE, expr);
			node->l_value.lookup = scope_lookup_init(ctx->stage, scope);
			node->l_value.expr = apply_discover_l_expr(ctx, scope, expr, &node->l_value.lookup, NULL);

			push_apply_node(ctx, node);

			return node;
		} else {
			printf("@TODO: Binary operators.");
			/* struct apply_node *node; */
			/* node = alloc_apply_node(ctx, APPLY_NODE_BINARY_OP, expr); */
			/* node->l_value.dest = dest; */

			/* push_apply_node(ctx, node); */
		}
		break;

	case CONFIG_NODE_IDENT: {
		struct apply_node *node;
		node = alloc_apply_node(ctx, APPLY_NODE_L_VALUE, expr);
		node->l_value.lookup = scope_lookup_init(ctx->stage, scope);
		node->l_value.expr = apply_discover_l_expr(ctx, scope, expr, &node->l_value.lookup, NULL);

		push_apply_node(ctx, node);

		return node;
	} break;

	case CONFIG_NODE_NUMLIT: {
		struct apply_node *numlit;

		numlit = alloc_apply_node(ctx, APPLY_NODE_NUMLIT, expr);
		numlit->literal.numlit = expr->numlit;

		push_apply_node(ctx, numlit);

		return numlit;
	}

	case CONFIG_NODE_ARRAY_LIT: {
		struct apply_node *arraylit;

		arraylit = alloc_apply_node(ctx, APPLY_NODE_ARRAY_LIT, expr);

		for (struct config_node *item = expr->array_lit.first_child;
			 item;
			 item = item->next_sibling) {
			struct apply_node *item_node;
			item_node = apply_discover_expr(ctx, scope, item);

			dlist_append(arraylit->array_lit.items, arraylit->array_lit.length, &item_node);

			push_apply_node(ctx, item_node);
		}

		push_apply_node(ctx, arraylit);

		return arraylit;
	}

	case CONFIG_NODE_TUPLE_LIT:
		return apply_discover_tuple_lit(ctx, scope, expr);

	default:
		apply_error(expr, "Invalid node in expression.");
	}

	printf("@TODO: Expr\n");
	return NULL;
}

static struct apply_node *apply_discover_type_tuple(struct apply_context *ctx,
													struct scoped_hash *scope,
													struct config_node *type_expr,
													struct atom *name,
													struct type_template_context *template);

static void apply_discover_device_type(struct apply_context *ctx,
									   struct scoped_hash *scope,
									   struct config_node *device_type_node)
{
	assert(device_type_node->type == CONFIG_NODE_DEVICE_TYPE);

	struct apply_node *device_type;

	device_type = alloc_apply_node(ctx, APPLY_NODE_DEVICE_TYPE, device_type_node);
	device_type->device_type.name = device_type_node->device_type.name;
	device_type->device_type.scope = scope;
	if (device_type_node->device_type.params) {
		device_type->device_type.params
			= apply_discover_type_tuple(ctx, scope, device_type_node->device_type.params, NULL,
										&device_type->device_type.params_type);
	}

	push_apply_node(ctx, device_type);
}

static void apply_discover_device_type_members(struct apply_context *ctx,
											   struct scoped_hash *scope,
											   struct config_node *node,
											   struct apply_node *device_type)
{
	for (; node; node = node->next_sibling) {
		switch (node->type) {
		case CONFIG_NODE_INPUT: {
			struct apply_node *input;

			input = alloc_apply_node(ctx, APPLY_NODE_DEVICE_TYPE_INPUT, node);
			input->device_type_input.owner = device_type;
			input->device_type_input.name = node->input.name;
			input->device_type_input.type_lookup
				= scope_lookup_init(ctx->stage, scope);
			device_type->device_type.missing_inputs += 1;

			push_apply_node(ctx, input);
		} break;

		case CONFIG_NODE_OUTPUT: {
			struct apply_node *output;

			output = alloc_apply_node(ctx, APPLY_NODE_DEVICE_TYPE_OUTPUT, node);
			output->device_type_output.owner = device_type;
			output->device_type_output.name = node->output.name;
			output->device_type_output.type_lookup
				= scope_lookup_init(ctx->stage, scope);
			device_type->device_type.missing_outputs += 1;

			push_apply_node(ctx, output);
		} break;

		default:
			break;
		}
	}
}

static void apply_discover_bind(struct apply_context *ctx,
								struct scoped_hash *scope,
								struct config_node *bind_node)
{
	assert(bind_node->type == CONFIG_NODE_BINARY_OP);
	assert(bind_node->binary_op.op == CONFIG_OP_BIND);

	struct apply_node *node;

	node = alloc_apply_node(ctx, APPLY_NODE_BIND, bind_node);
	node->bind.lookup_lhs
		= scope_lookup_init(ctx->stage, scope);
	node->bind.lhs
		= apply_discover_l_expr(ctx, scope,
								bind_node->binary_op.lhs,
								&node->bind.lookup_lhs,
								NULL);
	node->bind.lookup_rhs
		= scope_lookup_init(ctx->stage, scope);
	node->bind.rhs
		= apply_discover_l_expr(ctx, scope,
								bind_node->binary_op.rhs,
								&node->bind.lookup_rhs,
								NULL);

	push_apply_node(ctx, node);

}

static void apply_discover_device(struct apply_context *ctx,
								  struct scoped_hash *scope,
								  struct config_node *device_node);

static void apply_discover_device_members(struct apply_context *ctx,
										struct apply_node *device,
										struct config_node *first_member,
										bool from_device_type)
{
	for (struct config_node *member = first_member;
		 member;
		 member = member->next_sibling) {

		if (member->type == CONFIG_NODE_BINARY_OP &&
				   member->binary_op.op == CONFIG_OP_BIND) {
			apply_discover_bind(ctx, device->device.device->scope, member);
		}
		else if (member->type == CONFIG_NODE_DEVICE) {
			apply_discover_device(ctx, device->device.device->scope, member);
		}
	}
}

static void apply_discover_device(struct apply_context *ctx,
								  struct scoped_hash *scope,
								  struct config_node *device_node)
{
	assert(device_node->type == CONFIG_NODE_DEVICE);

	struct apply_node *device;

	device = alloc_apply_node(ctx, APPLY_NODE_DEVICE, device_node);
	device->device.name = device_node->device.name;
	device->device.scope = scope;
	device->device.type_lookup
		= scope_lookup_init(ctx->stage, scope);
	device->device.type
		= apply_discover_l_expr(ctx, scope, device_node->device.type, &device->device.type_lookup, NULL);
	if (device_node->device.args) {
		device->device.args
			= apply_discover_tuple_lit(ctx, scope, device_node->device.args);
	}

	push_apply_node(ctx, device);
}

static struct apply_node *apply_discover_type(struct apply_context *ctx,
											  struct scoped_hash *scope,
											  struct config_node *type_node,
											  struct atom *name,
											  struct type_template_context *template);

static struct apply_node *apply_discover_type_tuple(struct apply_context *ctx,
													struct scoped_hash *scope,
													struct config_node *type_expr,
													struct atom *name,
													struct type_template_context *template)
{
	struct apply_node *type = NULL;
	bool named;

	assert(type_expr->type == CONFIG_NODE_TUPLE);

	type = alloc_apply_node(ctx, APPLY_NODE_TYPE_TUPLE, type_expr);
	type->type_tuple.name = name;

	named = type_expr->tuple.named;
	type->type_tuple.named = named;
	type->type_tuple.template = template;

	for (struct config_node *member = type_expr->tuple.first_child;
		 member;
		 member = member->next_sibling) {
		assert(member->type == CONFIG_NODE_TUPLE_ITEM);

		struct apply_tuple_member new_member = {0};

		if (named) {
			new_member.name = member->tuple_item.name;
		}
		new_member.type = apply_discover_type(ctx, scope, member->tuple_item.type, NULL, template);

		dlist_append(type->type_tuple.members, type->type_tuple.num_members, &new_member);
	}

	push_apply_node(ctx, type);
	return type;
}

static struct apply_node *apply_discover_type(struct apply_context *ctx,
											  struct scoped_hash *scope,
											  struct config_node *type_node,
											  struct atom *name,
											  struct type_template_context *template)
{
	if (type_node->type == CONFIG_NODE_TYPE) {
		struct config_node *type_expr;
		struct apply_node *type = NULL;
		type_expr = type_node->type_def.first_child;

		switch (type_expr->type) {
		case CONFIG_NODE_TUPLE: {
			type = apply_discover_type_tuple(ctx, scope, type_expr, name, template);
		} break;

		case CONFIG_NODE_SUBRANGE: {
			type = alloc_apply_node(ctx, APPLY_NODE_TYPE_SUBRANGE, type_node);
			type->type_subrange.name = name;
			type->type_subrange.template = template;

			type->type_subrange.lhs
				= apply_discover_type(ctx, scope, type_expr->subrange.lhs, NULL, template);

			type->type_subrange.low
				= apply_discover_expr(ctx, scope, type_expr->subrange.low);

			type->type_subrange.high
				= apply_discover_expr(ctx, scope, type_expr->subrange.high);

			push_apply_node(ctx, type);
		} break;

		case CONFIG_NODE_ARRAY_TYPE: {
			type = alloc_apply_node(ctx, APPLY_NODE_TYPE_ARRAY, type_node);
			type->type_array.name = name;
			type->type_array.template = template;

			type->type_array.type_node
				= apply_discover_type(ctx, scope, type_expr->array_type.lhs, NULL, template);

			type->type_array.template_length = type_expr->array_type.template_length;
			if (type_expr->array_type.template_length) {
				type->type_array.length_node
					= apply_discover_l_expr(ctx, scope, type_expr->array_type.length, NULL,
											&type->type_array.length_template);
			} else {
				type->type_array.length_node
					= apply_discover_expr(ctx, scope, type_expr->array_type.length);
			}

			push_apply_node(ctx, type);
			break;
		}

		default:
			assert(!"Not a valid type.");
		}

		return type;

	} else if (type_node->type == CONFIG_NODE_TYPE_TEMPLATE_PARAM) {
		struct apply_node *node;
		node = alloc_apply_node(ctx, APPLY_NODE_TYPE_TEMPLATE_FIELD, type_node);
		node->type_template_field.template = template;
		node->type_template_field.expr
			= apply_discover_l_expr(ctx, scope, type_node->type_template_param.expr, NULL,
									&node->type_template_field.pattern);

		push_apply_node(ctx, node);

		return node;

	} else {
		struct apply_node *node;
		node = alloc_apply_node(ctx, APPLY_NODE_TYPE_L_EXPR, type_node);
		node->type_l_expr.lookup
			= scope_lookup_init(ctx->stage, scope);
		node->type_l_expr.expr
			= apply_discover_l_expr(ctx, scope, type_node, &node->type_l_expr.lookup, NULL);

		push_apply_node(ctx, node);

		return node;
	}
}

static void apply_discover_type_decl(struct apply_context *ctx,
									 struct scoped_hash *scope,
									 struct config_node *type_decl_node)
{
	assert(type_decl_node->type == CONFIG_NODE_TYPE_DECL);

	struct apply_node *type_decl;

	type_decl = alloc_apply_node(ctx, APPLY_NODE_TYPE_DECL, type_decl_node);
	type_decl->type_decl.name = type_decl_node->type_decl.name;
	type_decl->type_decl.scope = scope;
	type_decl->type_decl.type =
		apply_discover_type(ctx, scope, type_decl_node->type_decl.type,
							type_decl->type_decl.name, NULL);

	push_apply_node(ctx, type_decl);
}

static void apply_discover(struct apply_context *ctx,
						   struct scoped_hash *scope,
						   struct config_node *node)
{
	for (; node; node = node->next_sibling) {
		switch (node->type) {
		case CONFIG_NODE_MODULE:
			apply_discover(ctx, scope, node->module.first_child);
			break;

		case CONFIG_NODE_DEVICE_TYPE:
			apply_discover_device_type(ctx, scope, node);
			break;

		case CONFIG_NODE_DEVICE:
			apply_discover_device(ctx, scope, node);
			break;

		case CONFIG_NODE_TYPE_DECL:
			apply_discover_type_decl(ctx, scope, node);
			break;

		case CONFIG_NODE_NAMESPACE: {
			struct scoped_hash *child_scope;

			child_scope
				= scoped_hash_namespace(scope, node->namespace.name);

			apply_discover(ctx, child_scope, node->namespace.first_child);
		} break;

		case CONFIG_NODE_BINARY_OP:
			if (node->binary_op.op == CONFIG_OP_BIND) {
				apply_discover_bind(ctx, scope, node);
				break;
			}
			// fallthrough

		default:
			apply_error(node, "Unexpected node.");
			break;
		}
	}
}

static type_id apply_resolve_type_id(struct apply_node *node)
{
	switch (node->type) {
	case APPLY_NODE_TYPE_L_EXPR:
		return node->type_l_expr.type;

	case APPLY_NODE_TYPE_TUPLE:
		return node->type_tuple.type;

	case APPLY_NODE_TYPE_ARRAY:
		return node->type_array.type;

	case APPLY_NODE_TYPE_SUBRANGE:
		return node->type_subrange.type;

	case APPLY_NODE_TYPE_TEMPLATE_FIELD:
		return node->type_template_field.type;

	default:
		assert(!"Invalid node in type.");
		return -1;
	}
}

static type_id apply_resolve_expr_type_id(struct apply_context *ctx, struct apply_node *node)
{
	switch (node->type) {
	case APPLY_NODE_NUMLIT:
		return ctx->stage->standard_types.integer;

	case APPLY_NODE_L_VALUE:
		return node->l_value.lookup.type->id;

	case APPLY_NODE_BINARY_OP:
		return node->binary_op.type;

	case APPLY_NODE_TUPLE_LIT:
		return node->tuple_lit.type;

	case APPLY_NODE_ARRAY_LIT:
		return node->array_lit.type;

	default:
		assert(!"Invalid node in expr.");
		return -1;
	}
}

int apply_eval_expr(struct apply_context *ctx,
					struct apply_node *expr,
					struct value_ref *out)
{
	struct type *out_type;
	out_type = get_type(ctx->stage, out->type);

	assert(out_type && out_type->kind != TYPE_KIND_NONE);

	switch (expr->type) {
	case APPLY_NODE_NUMLIT:
		if (out_type->kind == TYPE_KIND_SCALAR) {
			out->data[0] = expr->literal.numlit;

		} else {
			apply_error(expr->cnode, "Cannot put a scalar into %s",
						humanreadable_type_kind(out_type->kind));
			return -1;
		}
		break;

	case APPLY_NODE_TUPLE_LIT:
		if (out_type->kind == TYPE_KIND_TUPLE) {
			if (expr->tuple_lit.num_members != out_type->tuple.length) {
				return -1;
				apply_error(expr->cnode, "Tuple lengths does not match.");
			}

			size_t num_scalars = 0;
			for (size_t i = 0; i < out_type->tuple.length; i++) {
				struct apply_tuple_lit_member *member;
				struct type *out_member_type;

				member = &expr->tuple_lit.members[i];
				out_member_type = get_type(ctx->stage, out_type->tuple.types[i]);

				struct value_ref val;
				val.data = &out->data[num_scalars];
				val.type = out_member_type->id;

				int err;
				err = apply_eval_expr(ctx, member->expr, &val);

				if (err) {
					return err;
				}

				num_scalars += out_member_type->num_scalars;
			}

		} else if (expr->tuple_lit.named && out_type->kind == TYPE_KIND_NAMED_TUPLE) {
			if (expr->tuple_lit.num_members != out_type->named_tuple.length) {
				apply_error(expr->cnode, "Tuple lengths does not match.");
				return -1;
			}

			size_t num_scalars = 0;
			for (size_t i = 0; i < out_type->tuple.length; i++) {
				struct named_tuple_member *out_member;
				struct type *out_member_type;
				out_member = &out_type->named_tuple.members[i];
				out_member_type = get_type(ctx->stage, out_member->type);

				struct apply_tuple_lit_member *member = NULL;

				for (size_t j = 0; j < expr->tuple_lit.num_members; j++) {
					if (out_member->name == expr->tuple_lit.members[j].name) {
						member = &expr->tuple_lit.members[j];
						break;
					}
				}

				if (!member) {
					apply_begin_error(expr->cnode);
					fprintf(stderr, "Missing '%.*s' for type '",
							ALIT(out_member->name));
					print_type(stderr, ctx->stage, out_member_type);
					fprintf(stderr, "'.");
					apply_end_error(expr->cnode);
				}

				struct value_ref val;
				val.data = &out->data[num_scalars];
				val.type = out_member_type->id;

				int err;
				err = apply_eval_expr(ctx, member->expr, &val);

				if (err) {
					return err;
				}

				num_scalars += out_member_type->num_scalars;
			}

		} else {
			apply_error(expr->cnode, "Cannot put a %stuple into %s",
						expr->tuple_lit.named ? "named" : "",
						humanreadable_type_kind(out_type->kind));
			return -1;
		}
		break;

	case APPLY_NODE_ARRAY_LIT:
		if (out_type->kind == TYPE_KIND_ARRAY) {
			if (expr->array_lit.length != out_type->array.length) {
				return -1;
				apply_error(expr->cnode, "Array lengths does not match.");
			}

			struct type *out_member_type;
			out_member_type = get_type(ctx->stage, out_type->array.type);

			for (size_t i = 0; i < out_type->array.length; i++) {
				struct value_ref val;
				val.data = &out->data[out_member_type->num_scalars * i];
				val.type = out_member_type->id;

				int err;
				err = apply_eval_expr(ctx, expr->array_lit.items[i], &val);

				if (err) {
					return err;
				}
			}

		} else {
			apply_error(expr->cnode, "Cannot put an array into %s",
						humanreadable_type_kind(out_type->kind));
			return -1;
		}
		break;


	case APPLY_NODE_L_VALUE: {
		int err;
		struct scope_lookup_range res;
		err = scope_lookup_result_single(expr->l_value.lookup, &res);

		if (err) {
			apply_error(expr->cnode, "Failed to look up.");
			return DISPATCH_ERROR;
		}

		// @TODO: Support multiple results (repetitions).
		err = eval_lookup_result(ctx->stage, res, out);

		if (err) {
			apply_error(expr->cnode, "Failed to lookup.");
			return DISPATCH_ERROR;
		}

	} break;

	case APPLY_NODE_BINARY_OP:
		printf("@TODO: Binary op in eval expr\n");
		break;

	default:
		assert(!"Invalid node in expression.");
	}

	return 0;
}

struct config_device_type_data {
	struct apply_node *device_type_node;
};

struct config_device_context {
	struct apply_context *ctx;
	struct apply_node *device;
};

static int apply_message_pump(struct apply_context *ctx);

struct tmp_config_device_context {
	struct apply_context tmp_apply_context;

	struct apply_context *ctx;
	struct apply_node *device_node;
	struct apply_node *device_type_node;

	struct config_node *device_type_first_child;
};

static void config_device_initialize_context(struct tmp_config_device_context *ctx,
											 struct stage *stage,
											 struct device_type *dev_type,
											 struct device *dev,
											 void *context)
{
	if (context) {
		struct config_device_context *config_ctx;
		config_ctx = (struct config_device_context *)context;

		ctx->ctx = config_ctx->ctx;
		ctx->device_node = config_ctx->device;
	} else{
		ctx->ctx = &ctx->tmp_apply_context;
		ctx->ctx->stage = stage;

		ctx->device_node = alloc_apply_node(ctx->ctx, APPLY_NODE_DEVICE, NULL);
		ctx->device_node->device.device = dev;
		ctx->device_node->device.name = dev->name;
		ctx->device_node->device.visited = false;
	}

	struct config_device_type_data *device_type_data;
	device_type_data = (struct config_device_type_data *)dev_type->user_data;

	ctx->device_type_node
		= device_type_data->device_type_node;

	ctx->device_type_first_child
		= ctx->device_type_node->cnode->device_type.first_child;
}

static int config_device_init(struct stage *stage,
							  struct device_type *dev_type,
							  struct device *dev,
							  void *context)
{
	struct tmp_config_device_context ctx;
	zero_struct(ctx);

	config_device_initialize_context(&ctx, stage, dev_type, dev, context);

	apply_discover_device_members(ctx.ctx, ctx.device_node,
								  ctx.device_type_first_child, true);

	if (!context) {
		return apply_message_pump(ctx.ctx);
	}

	return 0;
}

static enum apply_dispatch_result
apply_dispatch(struct apply_context *ctx,
			   struct apply_node *node)
{
	switch (node->type) {
	case APPLY_NODE_DEVICE_TYPE:
		WAIT_FOR(node->device_type.params);

		if (!node->device_type.type) {
			node->device_type.params_type.type
				= apply_resolve_type_id(node->device_type.params);

			node->device_type.type
				= register_device_type_two_phase(ctx->stage,
												 node->device_type.name->name,
												 node->device_type.params_type,
												 node->device_type.scope);
			if (!node->device_type.type) {
				return DISPATCH_ERROR;
			}

			struct config_device_type_data *data;
			data = calloc(1, sizeof(struct config_device_type_data));
			data->device_type_node = node;

			node->device_type.type->user_data = data;
			node->device_type.type->device_context_init = config_device_init;
			node->device_type.type->takes_context = true;
		}

		if (!node->device_type.visited) {
			apply_discover_device_type_members(ctx,
											   node->device_type.type->scope,
											   node->cnode->device_type.first_child,
											   node);

			node->device_type.visited = true;
		}

		if (node->device_type.missing_inputs     > 0 ||
			node->device_type.missing_outputs    > 0) {
			return DISPATCH_YIELD;
		}
		node->device_type.complete = true;
		finalize_device_type_two_phase(node->device_type.type);

		return DISPATCH_DONE;

	case APPLY_NODE_DEVICE_TYPE_OUTPUT: // fallthrough
	case APPLY_NODE_DEVICE_TYPE_INPUT: {
		struct apply_node *dev_type_node;

		dev_type_node = node->device_type_input.owner;
		assert(dev_type_node->type == APPLY_NODE_DEVICE_TYPE);

		// @TODO: This should use input or output.
		if (!node->device_type_input.type) {
			node->device_type_input.type
				= apply_discover_type(ctx, dev_type_node->device_type.scope,
									  node->cnode->input.type, NULL, NULL);
		}

		struct apply_node *type_expr;
		type_expr = node->device_type_input.type;

		WAIT_FOR(type_expr);

		type_id type = apply_resolve_type_id(type_expr);

		struct device_type *dev_type;
		struct atom *name;

		dev_type = dev_type_node->device_type.type;

		if (node->type == APPLY_NODE_DEVICE_TYPE_OUTPUT) {
			name = node->device_type_output.name;
			device_type_add_output(ctx->stage, dev_type, name->name, type);

			assert(dev_type_node->device_type.missing_outputs > 0);
			dev_type_node->device_type.missing_outputs -= 1;
		} else {
			name = node->device_type_input.name;
			device_type_add_input(ctx->stage, dev_type, name->name, type);

			assert(dev_type_node->device_type.missing_inputs > 0);
			dev_type_node->device_type.missing_inputs -= 1;
		}

		return DISPATCH_DONE;
	}

	case APPLY_NODE_DEVICE: {
		WAIT_FOR(node->device.type);
		if (node->device.args) {
			WAIT_FOR(node->device.args);
		}

		if (!apply_expect_lookup_result(SCOPE_ENTRY_DEVICE_TYPE,
										node->device.type_lookup.kind,
										node->device.type)) {
			return DISPATCH_ERROR;
		}

		struct device_type *dev_type;
		struct scope_lookup_range dev_type_id;
		int err;
		err = scope_lookup_result_single(node->device.type_lookup, &dev_type_id);

		if (err || dev_type_id.length != 1) {
			apply_error(node->device.type->cnode,
						"Not a single device type.");
			return DISPATCH_ERROR;
		}

		dev_type = get_device_type(ctx->stage, dev_type_id.begin);
		assert(dev_type != NULL);

		if (!dev_type->finalized) {
			apply_debug("Waiting for device type finalization (%.*s).",
						ALIT(dev_type->name));
			return DISPATCH_YIELD;
		}

		struct config_device_context device_context = {0};

		device_context.device = node;
		device_context.ctx = ctx;

		if (!node->device.device) {
			struct value_ref args = {0};

			if (node->device.args) {
				type_id args_type_id;
				args_type_id = apply_resolve_expr_type_id(ctx, node->device.args);
				args = alloc_value(ctx->stage, args_type_id);

				err = apply_eval_expr(ctx, node->device.args, &args);
				if (err) {
					apply_error(node->device.args->cnode,
								"Failed to evaluate literal.");
					return DISPATCH_ERROR;
				}
			}

			node->device.device
				= register_device_with_context(ctx->stage, dev_type->id,
											   node->device.scope,
											   node->device.name,
											   args,
											   &device_context);
			if (!node->device.device) {
				apply_debug("Failed to register device.");
				return DISPATCH_ERROR;
			}
		}

		apply_discover_device_members(ctx, node, node->cnode->device.first_child, false);


		return DISPATCH_DONE;
	}

	case APPLY_NODE_TYPE_DECL: {
		struct apply_node *type_node;
		type_node = node->type_decl.type;

		WAIT_FOR(node->type_decl.type);

		type_id type;
		type = apply_resolve_type_id(type_node);

		register_type_name(ctx->stage, type,
						   node->type_decl.scope,
						   node->type_decl.name);

		return DISPATCH_DONE;
	}


	case APPLY_NODE_TYPE_L_EXPR: {
		WAIT_FOR(node->type_l_expr.expr);

		struct scope_lookup_range range;
		int err;

		if (!apply_expect_lookup_result(SCOPE_ENTRY_TYPE,
										node->type_l_expr.lookup.kind,
										node->type_l_expr.expr)) {
			return DISPATCH_ERROR;
		}

		err = scope_lookup_result_single(node->type_l_expr.lookup, &range);
		if (err) {
			return DISPATCH_ERROR;
		}

		if (range.length != 1) {
			apply_error(node->cnode, "Expected 1 type, got %zu.", range.length);
			return DISPATCH_ERROR;
		}

		node->type_l_expr.type = range.begin;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_TYPE_TUPLE: {
		// @TODO: This should handle unnamed types
		for (size_t i = 0; i < node->type_tuple.num_members; i++) {
			struct apply_tuple_member *member;
			member = &node->type_tuple.members[i];

			WAIT_FOR(member->type);

			member->type_id = apply_resolve_type_id(member->type);
		}

		struct type new_type = {0};
		new_type.name = node->type_tuple.name;

		if (node->type_tuple.named) {
			new_type.kind = TYPE_KIND_NAMED_TUPLE;
			new_type.named_tuple.length = node->type_tuple.num_members;
			new_type.named_tuple.members
				= calloc(new_type.named_tuple.length,
						 sizeof(struct named_tuple_member));
			for (size_t i = 0; i < new_type.named_tuple.length; i++) {
				struct named_tuple_member *new_member;
				struct apply_tuple_member *member;
				new_member = &new_type.named_tuple.members[i];
				member = &node->type_tuple.members[i];

				new_member->name = member->name;
				new_member->type = member->type_id;
			}
		} else {
			new_type.kind = TYPE_KIND_TUPLE;
			new_type.tuple.length = node->type_tuple.num_members;
			new_type.tuple.types
				= calloc(new_type.tuple.length,
						 sizeof(type_id));
			for (size_t i = 0; i < new_type.tuple.length; i++) {
				struct apply_tuple_member *member;

				member = &node->type_tuple.members[i];
				new_type.tuple.types[i] = member->type_id;
			}
		}

		struct type *final_type;

		final_type = register_type(ctx->stage, new_type);

		if (!final_type) {
			return DISPATCH_ERROR;
		}

		node->type_tuple.type = final_type->id;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_TYPE_ARRAY: {
		WAIT_FOR(node->type_array.type_node);

		struct type *final_type;
		struct atom *name;
		type_id member_type;

		name        = node->type_array.name;
		member_type = apply_resolve_type_id(node->type_array.type_node);

		WAIT_FOR(node->type_array.length_node);

		if (!node->type_array.template_length) {
			scalar_value length = 0;

			struct value_ref length_value = {0};
			length_value.data = &length;
			length_value.type = ctx->stage->standard_types.integer;
			int err;
			err = apply_eval_expr(ctx, node->type_array.length_node, &length_value);

			if (err) {
				return DISPATCH_ERROR;
			}

			final_type = register_array_type(ctx->stage, name, member_type, length);

		} else {
			final_type = register_template_length_array_type(ctx->stage, name, member_type,
															 node->type_array.length_template,
															 node->type_array.template);
		}

		if (!final_type) {
			// @TODO: Print child type
			apply_error(node->cnode, "Could not register the type.");
			return DISPATCH_ERROR;
		}

		node->type_array.type = final_type->id;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_TYPE_SUBRANGE:
		WAIT_FOR(node->type_subrange.lhs);
		WAIT_FOR(node->type_subrange.low);
		WAIT_FOR(node->type_subrange.high);

		struct atom *name = node->type_subrange.name;
		scalar_value low  = node->type_subrange.value_low;
		scalar_value high = node->type_subrange.value_high;

		type_id parent_type_id;
		struct type *parent_type;

		parent_type_id = apply_resolve_type_id(node->type_subrange.lhs);
		parent_type = get_type(ctx->stage, parent_type_id);

		if (parent_type->kind != TYPE_KIND_SCALAR) {
			apply_error(node->cnode, "Cannot make a subrange-type of a %s.",
						humanreadable_type_kind(parent_type->kind));
		}

		if (low  < parent_type->scalar.min ||
			high > parent_type->scalar.max) {
			apply_begin_error(node->cnode);

			fprintf(stderr,
					"The range of %.*s, {%u..%u}, "
					"is outside than the range of the parent, ",
					LIT(name ? name->name : STR("the new type")), low, high);
			print_type(stderr, ctx->stage, parent_type);
			fprintf(stderr, ", {%u..%u}.",
					parent_type->scalar.min,
					parent_type->scalar.max);
			apply_end_error(node->cnode);
		}

		struct type *final_type;

		final_type = register_scalar_type(ctx->stage, name, low, high);

		node->type_subrange.type = final_type->id;
		return DISPATCH_DONE;

	case APPLY_NODE_TYPE_TEMPLATE_FIELD: {
		WAIT_FOR(node->type_template_field.expr);

		struct type *type;

		type = register_template_type(ctx->stage, NULL,
									  node->type_template_field.pattern,
									  node->type_template_field.template);

		node->type_template_field.type = type->id;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_BINARY_OP:
		return DISPATCH_ERROR;

	case APPLY_NODE_ACCESS: {
		assert(node->access.lhs != NULL);

		WAIT_FOR(node->access.lhs);

		if (!node->access.rhs) {
			node->access.rhs
				= apply_discover_l_expr(ctx,
										node->access.scope,
										node->cnode->binary_op.rhs,
										node->access.lookup,
										node->access.pattern);
		}

		WAIT_FOR(node->access.rhs);

		return DISPATCH_DONE;
	}

	case APPLY_NODE_ACCESS_INDEX: {
		assert(node->access_index.lhs != NULL);
		assert(node->access_index.index != NULL);

		WAIT_FOR(node->access_index.lhs);
		WAIT_FOR(node->access_index.index);

		assert((node->access_index.lookup != NULL) || (node->access_index.pattern != NULL));

		int err = 0;

		if (node->access_index.lookup) {

			err = scope_lookup_index(node->access_index.lookup,
									node->access_index.value_index);

			if (err) {
				apply_debug("Waiting for ident '%.*s'%s.",
							ALIT(node->ident.name),
							node->ident.local_lookup
							? " local lookup"
							: "");
			}
		}

		if (node->access_index.pattern) {
			access_pattern_index(node->access_index.pattern,
								 node->access_index.value_index);
		}

		return (err == 0) ? DISPATCH_DONE : DISPATCH_YIELD;
	}

	case APPLY_NODE_ACCESS_INDEX_RANGE: {
		struct apply_node *lhs_node;
		struct apply_node *low_node;
		struct apply_node *high_node;

		lhs_node = node->access_index_range.lhs;
		low_node = node->access_index_range.low_index;
		high_node = node->access_index_range.high_index;

		assert(lhs_node != NULL);

		WAIT_FOR(lhs_node);
		if (low_node)  { WAIT_FOR(low_node);  }
		if (high_node) { WAIT_FOR(high_node); }

		assert((node->access_index_range.lookup != NULL) || (node->access_index_range.pattern != NULL));


		size_t low = 0;
		size_t high = SCOPE_LOOKUP_RANGE_END;

		if (low_node) {
			low  = node->access_index_range.value_low_index;
		}
		if (high_node) {
			high = node->access_index_range.value_high_index;
		}

		int err = 0;

		if (node->access_index_range.lookup) {

			err = scope_lookup_range(node->access_index_range.lookup, low, high);

			if (err) {
				apply_debug("Waiting for ident '%.*s'%s.",
							ALIT(node->ident.name),
							node->ident.local_lookup
							? " local lookup"
							: "");
			}
		}

		if (node->access_index_range.pattern) {
			access_pattern_range(node->access_index_range.pattern,
								 low, high);
		}
		return (err == 0) ? DISPATCH_DONE : DISPATCH_YIELD;
	}

	case APPLY_NODE_BIND: {
		WAIT_FOR(node->bind.lhs);
		WAIT_FOR(node->bind.rhs);

		if (!apply_expect_lookup_result(SCOPE_ENTRY_DEVICE_CHANNEL,
										node->bind.lookup_lhs.kind,
										node->bind.lhs)) {
			return DISPATCH_ERROR;
		}

		if (!apply_expect_lookup_result(SCOPE_ENTRY_DEVICE_CHANNEL,
										node->bind.lookup_rhs.kind,
										node->bind.rhs)) {
			return DISPATCH_ERROR;
		}

		size_t lhs_i = 0, rhs_i = 0;
		int err;
		struct scope_lookup_range lhs_range;
		struct scope_lookup_range rhs_range;

		size_t lhs_instances, rhs_instances;
		size_t lhs_instance_size, rhs_instance_size;

		lhs_instances = scope_lookup_instances(node->bind.lookup_lhs);
		rhs_instances = scope_lookup_instances(node->bind.lookup_rhs);

		lhs_instance_size = scope_lookup_instance_size(node->bind.lookup_lhs);
		rhs_instance_size = scope_lookup_instance_size(node->bind.lookup_rhs);

		if (lhs_instances == rhs_instances &&
			lhs_instance_size == rhs_instance_size) {
			while ((err = scope_lookup_iterate(node->bind.lookup_lhs,
											&lhs_i, &lhs_range)) == LOOKUP_FOUND &&
				(err = scope_lookup_iterate(node->bind.lookup_rhs,
											&rhs_i, &rhs_range)) == LOOKUP_FOUND) {

				assert(lhs_range.length == rhs_range.length);

				for (size_t i = 0; i < lhs_range.length; i++) {
					channel_bind(ctx->stage,
								rhs_range.begin + i,
								lhs_range.begin + i);
				}
			}
		}
		else if (rhs_instances == 1 &&
				 lhs_instance_size == rhs_instance_size) {
			err = scope_lookup_iterate(node->bind.lookup_rhs,
									   &rhs_i, &rhs_range);

			if (err) {
				apply_error(node->cnode, "Could not iterate.");
				return DISPATCH_ERROR;
			}

			while ((err = scope_lookup_iterate(node->bind.lookup_lhs,
											   &lhs_i, &lhs_range)) == LOOKUP_FOUND) {

				assert(lhs_range.length == rhs_range.length);

				for (size_t i = 0; i < lhs_range.length; i++) {
					channel_bind(ctx->stage,
								rhs_range.begin + i,
								lhs_range.begin + i);
				}
			}
		}
		else if (rhs_instances == 1 &&
				 lhs_instance_size == 1 &&
				 lhs_instances == rhs_instance_size) {
			err = scope_lookup_iterate(node->bind.lookup_rhs,
									   &rhs_i, &rhs_range);

			if (err) {
				apply_error(node->cnode, "Could not iterate.");
				return DISPATCH_ERROR;
			}

			size_t i = 0;
			while ((err = scope_lookup_iterate(node->bind.lookup_lhs,
											   &lhs_i, &lhs_range)) == LOOKUP_FOUND) {

				channel_bind(ctx->stage,
							rhs_range.begin + i,
							lhs_range.begin);

				i += 1;
			}
		}
		else {
			apply_error(node->cnode,
						"Missmatched number of instances, "
						"%zu on the left and %zu on the right.",
						lhs_instances, rhs_instances);
			return DISPATCH_ERROR;
		}

		if (err != LOOKUP_END) {
			apply_error(node->cnode, "Could not iterate.");
			return DISPATCH_ERROR;
		}

		return DISPATCH_DONE;
	}

	case APPLY_NODE_IDENT: {
		assert((node->ident.lookup != NULL) || (node->ident.pattern != NULL));

		if (node->ident.lookup) {
			int err;

			err = scope_lookup_ident(node->ident.lookup, node->ident.name);

			if (err) {
				apply_debug("Waiting for ident '%.*s'%s.",
							ALIT(node->ident.name),
							node->ident.local_lookup
							? " local lookup"
							: "");
				return DISPATCH_YIELD;
			}
		}
		if (node->ident.pattern) {
			access_pattern_ident(node->ident.pattern, node->ident.name);
		}

		return DISPATCH_DONE;
	}

	case APPLY_NODE_L_VALUE: {
		WAIT_FOR(node->l_value.expr);
		return DISPATCH_DONE;
	}

	case APPLY_NODE_NUMLIT:
		return DISPATCH_DONE;


	case APPLY_NODE_TUPLE_LIT: {
		size_t num_scalars = 0;
		for (size_t i = 0; i < node->tuple_lit.num_members; i++) {
			struct apply_tuple_lit_member *member;
			member = &node->tuple_lit.members[i];

			WAIT_FOR(member->expr);

			if (member->type_id == 0) {
				member->type_id = apply_resolve_expr_type_id(ctx, member->expr);
			}

			struct type *member_type;

			member_type = get_type(ctx->stage, member->type_id);
			num_scalars += member_type->num_scalars;
		}

		struct type *type;

		if (node->tuple_lit.named) {
			struct named_tuple_member *type_members;
			type_members = calloc(node->tuple_lit.num_members, sizeof(struct named_tuple_member));

			for (size_t i = 0; i < node->tuple_lit.num_members; i++) {
				struct apply_tuple_lit_member *member;
				member = &node->tuple_lit.members[i];

				type_members[i].name = member->name;
				type_members[i].type = member->type_id;
			}

			type = register_named_tuple_type(ctx->stage, NULL, type_members,
											 node->tuple_lit.num_members);
		} else {
			type_id *type_members;
			type_members = calloc(node->tuple_lit.num_members, sizeof(type_id));

			for (size_t i = 0; i < node->tuple_lit.num_members; i++) {
				struct apply_tuple_lit_member *member;
				member = &node->tuple_lit.members[i];

				type_members[i] = member->type_id;
			}

			type = register_tuple_type(ctx->stage, NULL, type_members,
									   node->tuple_lit.num_members);
		}

		node->tuple_lit.type = type->id;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_ARRAY_LIT: {
		for (size_t i = 0; i < node->array_lit.length; i++) {
			struct apply_node *item;
			item = node->array_lit.items[i];
			WAIT_FOR(item);
		}

		type_id type = 0;
		for (size_t i = 0; i < node->array_lit.length; i++) {
			struct apply_node *item;
			type_id item_type;

			item = node->array_lit.items[i];
			item_type = apply_resolve_expr_type_id(ctx, item);

			if (type == 0) {
				type = item_type;
			} else {
				if (!types_compatible(ctx->stage, type, item_type)) {
					apply_begin_error(node->cnode);
					fprintf(stderr,
							"All the entries if an array has to have the same type. "
							"The array contains both '");
					print_type_id(stderr, ctx->stage, type);
					fprintf(stderr, "' and '");
					print_type_id(stderr, ctx->stage, item_type);
					fprintf(stderr, "'.");
					apply_end_error(node->cnode);
					return DISPATCH_ERROR;
				}
			}
		}

		struct type *new_type;
		new_type = register_array_type(ctx->stage, NULL, type, node->array_lit.length);
		node->array_lit.type = new_type->id;

		return DISPATCH_DONE;
	}
	}

	printf("Encountered an unexpeted node '%s'!\n",
		   apply_node_name(node->type));
	return DISPATCH_ERROR;
}

void apply_print_missing_error(struct apply_context *ctx, struct apply_node *node)
{
	switch (node->type) {
	case APPLY_NODE_DEVICE:
		if (node->device.type->state != DISPATCH_DONE) {
			apply_l_expr_not_found("device type", node->device.type);
		}
		break;

	case APPLY_NODE_TYPE_DECL:
		break;

	case APPLY_NODE_TYPE_L_EXPR:
		if (node->type_l_expr.expr->state != DISPATCH_DONE) {
			apply_l_expr_not_found("type", node->type_l_expr.expr);
		}
		break;

	case APPLY_NODE_BIND:
		if (node->bind.lhs->state != DISPATCH_DONE) {
			apply_l_expr_not_found("channel(s)", node->bind.lhs);
		}
		if (node->bind.rhs->state != DISPATCH_DONE) {
			apply_l_expr_not_found("channel(s)", node->bind.rhs);
		}
		break;

	case APPLY_NODE_L_VALUE:
		if (node->l_value.expr->state != DISPATCH_DONE) {
			apply_l_expr_not_found("value", node->l_value.expr);
		}
		break;

	default:
		break;
	}
}

static int apply_message_pump(struct apply_context *ctx)
{
	size_t generation = 0;
	size_t last_successful_generation = 0;

	while (ctx->queue) {
		struct apply_node *node;
		enum apply_dispatch_result result;

		node = pop_apply_node(ctx);

		if (node->generation + 1 > generation) {
			generation = node->generation + 1;

			// Wait for 2 generations before aborting, to allow newly
			// created nodes to be applied.
			if (generation > last_successful_generation + 2) {
				push_apply_node(ctx, node);

				 while (ctx->queue) {
					node = pop_apply_node(ctx);
					apply_print_missing_error(ctx, node);
				}

				return -1;
			}
		}

		node->generation = generation;

		#if APPLY_DEBUG
		printf("%zu:Applying %s... ", node->generation, apply_node_name(node->type));
		#endif

		result = apply_dispatch(ctx, node);

		node->state = result;

		if (result == DISPATCH_ERROR) {
			apply_debug("ERROR");
			printf("Failed to apply '%s'. Aborting.\n",
				   apply_node_name(node->type));
			return -1;
		} else if (result == DISPATCH_YIELD) {
			apply_debug("Yield");
			push_apply_node(ctx, node);
		} else {
			last_successful_generation = generation;
			apply_debug("Ok");
		}
	}

	apply_debug("Application done\n");

	return 0;
}

int apply_config(struct stage *stage, struct config_node *node)
{
	struct apply_context ctx = {0};
	ctx.stage = stage;

	apply_discover(&ctx, &ctx.stage->root_scope, node);

	return apply_message_pump(&ctx);
}
