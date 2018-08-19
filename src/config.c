#include "config.h"
#include "stage.h"
#include "device_type.h"
#include "device.h"
#include "utils.h"
#include "dlist.h"
#include "scope_lookup.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define APPLY_DEBUG 0

enum apply_node_type {
	APPLY_NODE_DEVICE_TYPE,
	APPLY_NODE_DEVICE_TYPE_ATTR,
	APPLY_NODE_DEVICE_TYPE_INPUT,
	APPLY_NODE_DEVICE_TYPE_OUTPUT,

	APPLY_NODE_DEVICE,
	APPLY_NODE_DEVICE_ATTR,

	APPLY_NODE_TYPE_DECL,

	APPLY_NODE_TYPE_L_EXPR,
	APPLY_NODE_TYPE_TUPLE,
	APPLY_NODE_TYPE_SUBRANGE,

	APPLY_NODE_BINARY_OP,
	APPLY_NODE_ACCESS,
	APPLY_NODE_ACCESS_INDEX,
	APPLY_NODE_ACCESS_INDEX_RANGE,
	APPLY_NODE_BIND,
	APPLY_NODE_IDENT,
	APPLY_NODE_NUMLIT,

	APPLY_NODE_L_VALUE,
};

const char *apply_node_name(enum apply_node_type type) {
#define APPLY_NODE_NAME(name) case APPLY_NODE_##name: return #name;
	switch (type) {
		APPLY_NODE_NAME(DEVICE_TYPE);
		APPLY_NODE_NAME(DEVICE_TYPE_ATTR);
		APPLY_NODE_NAME(DEVICE_TYPE_INPUT);
		APPLY_NODE_NAME(DEVICE_TYPE_OUTPUT);
		APPLY_NODE_NAME(DEVICE);
		APPLY_NODE_NAME(DEVICE_ATTR);

		APPLY_NODE_NAME(TYPE_DECL);

		APPLY_NODE_NAME(TYPE_L_EXPR);
		APPLY_NODE_NAME(TYPE_TUPLE);
		APPLY_NODE_NAME(TYPE_SUBRANGE);

		APPLY_NODE_NAME(BINARY_OP);
		APPLY_NODE_NAME(ACCESS);
		APPLY_NODE_NAME(ACCESS_INDEX);
		APPLY_NODE_NAME(ACCESS_INDEX_RANGE);
		APPLY_NODE_NAME(BIND);
		APPLY_NODE_NAME(IDENT);
		APPLY_NODE_NAME(NUMLIT);

		APPLY_NODE_NAME(L_VALUE);
	}
#undef APPLY_NODE_NAME
	return "(unknown apply node)";
}

enum entry_found_state {
	ENTRY_FOUND_WAITING = 0,
	ENTRY_NOT_FOUND = -1,
	ENTRY_FOUND = 1,
};

struct apply_node;

struct apply_tuple_member {
	struct atom *name;
	struct apply_node *type;
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
			bool visited;
			bool complete;

			size_t missing_inputs;
			size_t missing_outputs;
			size_t missing_attributes;
		} device_type;

		struct {
			struct atom *name;
			struct apply_node *owner;
			struct apply_node *type;
			struct apply_node *def;

			struct scope_lookup type_lookup;
		} device_type_attribute;

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

			struct apply_node **attrs;
			size_t num_attrs;
			size_t not_found_attrs;
			size_t missing_attrs;
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
		} type_tuple;

		struct {
			struct atom *name;
			struct apply_node *lhs;
			struct apply_node *low;
			struct apply_node *high;

			scalar_value value_low;
			scalar_value value_high;

			struct scope_lookup lhs_lookup;

			type_id type;
		} type_subrange;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;
			struct scoped_hash *scope;

			struct scope_lookup *lookup;
		} access;

		struct {
			struct apply_node *lhs;
			struct apply_node *index;
			struct scoped_hash *scope;

			scalar_value value_index;

			struct scope_lookup *lookup;
		} access_index;

		struct {
			struct apply_node *lhs;
			struct apply_node *low_index;
			struct apply_node *high_index;
			struct scoped_hash *scope;

			scalar_value value_low_index;
			scalar_value value_high_index;

			struct scope_lookup *lookup;
		} access_index_range;

		struct {
			struct scoped_hash *scope;
			struct atom *name;
			bool local_lookup;

			struct scope_lookup *lookup;
		} ident;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;
			enum config_binary_op op;

			struct scope_lookup *lookup;

			struct value_ref dest;
		} binary_op;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;

			struct scope_lookup lookup_lhs;
			struct scope_lookup lookup_rhs;
		} bind;

		struct {
			scalar_value numlit;

			struct value_ref dest;
		} literal;

		struct {
			struct apply_node *expr;
			struct scope_lookup lookup;

			struct value_ref dest;
		} l_value;
	};

	enum entry_found_state entry_found;
};

struct apply_context {
	struct apply_node *queue;
	struct apply_node *queue_tail;

	struct stage *stage;
};

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

static void apply_error(struct config_node *node, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	fprintf(stderr, "%zu:%zu: ", node->from.line, node->from.column);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

static struct apply_node *alloc_apply_node(struct apply_context *ctx, enum apply_node_type type, struct config_node *cnode)
{
	struct apply_node *node;
	node = calloc(1, sizeof(struct apply_node));
	node->type = type;
	node->cnode = cnode;
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

/* static void print_l_expr(struct config_node *expr) */
/* { */
/* 	switch (expr->type) { */
/* 	case CONFIG_NODE_IDENT: */
/* 		if (expr->ident) { */
/* 			printf("%.*s", ALIT(expr->ident)); */
/* 		} else { */
/* 			printf("_"); */
/* 		} */
/* 		break; */

/* 	case CONFIG_NODE_BINARY_OP: */
/* 		print_l_expr(expr->binary_op.lhs); */

/* 		if (expr->binary_op.op == CONFIG_OP_SUBSCRIPT) { */
/* 			printf("["); */
/* 			printf("(expr)"); */
/* 			printf("]"); */
/* 		} else if (expr->binary_op.op == CONFIG_OP_ACCESS) { */
/* 			printf("."); */
/* 			print_l_expr(expr->binary_op.rhs); */
/* 		} else { */
/* 			printf("(non l-expr)"); */
/* 		} */
/* 		break; */

/* 	case CONFIG_NODE_SUBSCRIPT_RANGE: */
/* 		print_l_expr(expr->subscript_range.lhs); */

/* 		printf("["); */
/* 		printf("(expr)..(expr)"); */
/* 		printf("]"); */

/* 		break; */

/* 	default: */
/* 		printf("(non l-expr)"); */
/* 		break; */
/* 	} */
/* } */

static struct apply_node *apply_discover_expr(struct apply_context *ctx,
											  struct scoped_hash *scope,
											  struct config_node *expr,
											  struct value_ref dest);

static struct apply_node *apply_discover_l_expr(struct apply_context *ctx,
												struct scoped_hash *scope,
												struct config_node *expr,
												struct scope_lookup *lookup)
{
	switch (expr->type) {
	case CONFIG_NODE_IDENT: {
		struct apply_node *node;

		node = alloc_apply_node(ctx, APPLY_NODE_IDENT, expr);
		node->ident.name = expr->ident;
		node->ident.scope = scope;
		node->ident.lookup = lookup;

		push_apply_node(ctx, node);

		return node;
	} break;

	case CONFIG_NODE_BINARY_OP: {
		if (expr->binary_op.op == CONFIG_OP_ACCESS) {
			struct apply_node *node;
			node = alloc_apply_node(ctx, APPLY_NODE_ACCESS, expr);
			node->access.lookup = lookup;
			node->access.scope = scope;
			node->access.lhs = apply_discover_l_expr(ctx, scope, expr->binary_op.lhs, lookup);

			push_apply_node(ctx, node);

			return node;
		}
		else if (expr->binary_op.op == CONFIG_OP_SUBSCRIPT) {
			struct apply_node *node;
			node = alloc_apply_node(ctx, APPLY_NODE_ACCESS_INDEX, expr);
			node->access_index.lookup = lookup;
			node->access_index.scope = scope;
			node->access_index.lhs
				= apply_discover_l_expr(ctx, scope, expr->binary_op.lhs, lookup);

			struct value_ref index_ref = {0};
			index_ref.data = &node->access_index.value_index;
			index_ref.type = ctx->stage->standard_types.integer;

			node->access_index.index = apply_discover_expr(ctx, scope, expr->binary_op.rhs, index_ref);

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
		node->access_index_range.lhs
				= apply_discover_l_expr(ctx, scope, expr->subscript_range.lhs, lookup);
		node->access_index_range.scope = scope;

		if (expr->subscript_range.low) {
			struct value_ref low_index_ref = {0};
			low_index_ref.type = ctx->stage->standard_types.integer;
			low_index_ref.data = &node->access_index_range.value_low_index;

			node->access_index_range.low_index
					= apply_discover_expr(ctx, scope, expr->subscript_range.low, low_index_ref);
		}

		if (expr->subscript_range.high) {
			struct value_ref high_index_ref = {0};
			high_index_ref.type = ctx->stage->standard_types.integer;
			high_index_ref.data = &node->access_index_range.value_high_index;

			node->access_index_range.high_index
				= apply_discover_expr(ctx, scope, expr->subscript_range.high, high_index_ref);
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

static struct apply_node *apply_discover_expr(struct apply_context *ctx,
											  struct scoped_hash *scope,
											  struct config_node *expr,
											  struct value_ref dest)
{
	switch (expr->type) {
	case CONFIG_NODE_BINARY_OP:
		if (expr->binary_op.op == CONFIG_OP_ACCESS) {
			struct apply_node *node;
			node = alloc_apply_node(ctx, APPLY_NODE_L_VALUE, expr);
			node->l_value.expr = apply_discover_l_expr(ctx, scope, expr, &node->l_value.lookup);
			node->l_value.dest = dest;

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
		node->l_value.expr = apply_discover_l_expr(ctx, scope, expr, &node->l_value.lookup);
		node->l_value.dest = dest;

		push_apply_node(ctx, node);

		return node;
	} break;

	case CONFIG_NODE_NUMLIT: {
		struct apply_node *numlit;

		numlit = alloc_apply_node(ctx, APPLY_NODE_NUMLIT, expr);
		// @TODO: Support more complex literals.
		numlit->literal.numlit = expr->numlit;
		numlit->literal.dest = dest;

		push_apply_node(ctx, numlit);

		return numlit;
	}

	default:
		apply_error(expr, "Invalid node in expression.");
	}

	printf("@TODO: Expr\n");
	return NULL;
}

static void apply_discover_device_type(struct apply_context *ctx,
									   struct scoped_hash *scope,
									   struct config_node *device_type_node)
{
	assert(device_type_node->type == CONFIG_NODE_DEVICE_TYPE);

	struct apply_node *device_type;

	device_type = alloc_apply_node(ctx, APPLY_NODE_DEVICE_TYPE, device_type_node);
	device_type->device_type.name = device_type_node->device_type.name;
	device_type->device_type.scope = scope;

	push_apply_node(ctx, device_type);
}

static void apply_discover_device_type_members(struct apply_context *ctx,
											   struct scoped_hash *scope,
											   struct config_node *node,
											   struct apply_node *device_type)
{
	for (; node; node = node->next_sibling) {
		switch (node->type) {
		case CONFIG_NODE_ATTR: {
			struct apply_node *attr;

			attr = alloc_apply_node(ctx, APPLY_NODE_DEVICE_TYPE_ATTR, node);
			attr->device_type_attribute.owner = device_type;
			attr->device_type_attribute.name = node->attr.name;
			attr->device_type_attribute.type_lookup
				= scope_lookup_init(ctx->stage, scope);
			device_type->device_type.missing_attributes += 1;

			push_apply_node(ctx, attr);
		} break;

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
			apply_error(node, "Unexpected node in device type.");
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
								&node->bind.lookup_lhs);
	node->bind.lookup_rhs
		= scope_lookup_init(ctx->stage, scope);
	node->bind.rhs
		= apply_discover_l_expr(ctx, scope,
								bind_node->binary_op.rhs,
								&node->bind.lookup_rhs);

	push_apply_node(ctx, node);

}

static void apply_discover_device_attrs(struct apply_context *ctx,
										struct apply_node *device,
										struct config_node *device_node)
{
	for (struct config_node *member = device_node->device.first_child;
		 member;
		 member = member->next_sibling) {

		if (member->type == CONFIG_NODE_BINARY_OP &&
			member->binary_op.op == CONFIG_OP_ASSIGN) {
			struct config_node *lhs;
			lhs = member->binary_op.lhs;

			struct apply_node *attr;
			attr = alloc_apply_node(ctx, APPLY_NODE_DEVICE_ATTR, member);
			attr->device_attr.device = device;

			attr->device_attr.lookup
				= scope_lookup_init(ctx->stage, device->device.device->scope);
			attr->device_attr.name
				= apply_discover_l_expr(ctx, device->device.device->scope, lhs, &attr->device_attr.lookup);

			dlist_append(device->device.attrs, device->device.num_attrs, &attr);
			device->device.missing_attrs += 1;

			push_apply_node(ctx, attr);
		} else if (member->type == CONFIG_NODE_BINARY_OP &&
				   member->binary_op.op == CONFIG_OP_BIND) {
				apply_discover_bind(ctx, device->device.device->scope, member);
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
		= apply_discover_l_expr(ctx, scope, device_node->device.type, &device->device.type_lookup);

	push_apply_node(ctx, device);
}

static struct apply_node *apply_discover_type(struct apply_context *ctx,
											  struct scoped_hash *scope,
											  struct config_node *type_node,
											  struct atom *name)
{
	if (type_node->type == CONFIG_NODE_TYPE) {
		struct config_node *type_expr;
		struct apply_node *type = NULL;
		type_expr = type_node->type_def.first_child;

		switch (type_expr->type) {
		case CONFIG_NODE_TUPLE: {
			bool named;
			type = alloc_apply_node(ctx, APPLY_NODE_TYPE_TUPLE, type_node);
			type->type_tuple.name = name;

			named = type_expr->tuple.named;
			type->type_tuple.named = named;

			for (struct config_node *member = type_expr->tuple.first_child;
				 member;
				 member = member->next_sibling) {
				assert(member->type == CONFIG_NODE_TUPLE_ITEM);

				struct apply_tuple_member new_member = {0};

				if (named) {
					new_member.name = member->tuple_item.name;
				}
				new_member.type = apply_discover_type(ctx, scope, member->tuple_item.type, new_member.name);

				dlist_append(type->type_tuple.members, type->type_tuple.num_members, &new_member);
			}
		} break;

		case CONFIG_NODE_SUBRANGE: {
			type = alloc_apply_node(ctx, APPLY_NODE_TYPE_SUBRANGE, type_node);
			type->type_subrange.name = name;

			type->type_subrange.lhs_lookup
				= scope_lookup_init(ctx->stage, scope);
			type->type_subrange.lhs
				= apply_discover_l_expr(ctx, scope, type_expr->subrange.lhs, &type->type_subrange.lhs_lookup);

			struct value_ref value_low;

			value_low.type  = ctx->stage->standard_types.integer;
			value_low.data  = &type->type_subrange.value_low;

			type->type_subrange.low
				= apply_discover_expr(ctx, scope, type_expr->subrange.low,
									  value_low);

			struct value_ref value_high;

			value_high.type = ctx->stage->standard_types.integer;
			value_high.data = &type->type_subrange.value_high;

			type->type_subrange.high
				= apply_discover_expr(ctx, scope, type_expr->subrange.high,
									  value_high);
		} break;

		default:
			apply_error(type_expr, "Not a valid type.");
		}

		if (type) {
			push_apply_node(ctx, type);
		}

		return type;

	} else {
		struct apply_node *node;
		node = alloc_apply_node(ctx, APPLY_NODE_TYPE_L_EXPR, type_node);
		node->type_l_expr.lookup
			= scope_lookup_init(ctx->stage, scope);
		node->type_l_expr.expr
			= apply_discover_l_expr(ctx, scope, type_node, &node->type_l_expr.lookup);

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
							type_decl->type_decl.name);

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

static int apply_resolve_type_id(struct apply_node *node, type_id *type)
{
	switch (node->type) {
	case APPLY_NODE_TYPE_L_EXPR:
		*type = node->type_l_expr.type;
		return 0;

	case APPLY_NODE_TYPE_TUPLE:
		*type = node->type_tuple.type;
		return 0;

	case APPLY_NODE_TYPE_SUBRANGE:
		*type = node->type_subrange.type;
		return 0;

	default:
		apply_error(node->cnode, "Not a type.");
		return -1;
	}
}

enum apply_dispatch_result {
	DISPATCH_DONE  = 0,
	DISPATCH_YIELD = 1,
	DISPATCH_ERROR = -1,
};

static int config_device_init(struct stage *stage,
							  struct device_type *dev_type,
							  struct device *dev)
{
	printf("Init %.*s (%.*s)\n", ALIT(dev->name), ALIT(dev_type->name));

	return 0;
}

static enum apply_dispatch_result
apply_dispatch(struct apply_context *ctx,
			   struct apply_node *node)
{
	switch (node->type) {
	case APPLY_NODE_DEVICE_TYPE:
		if (!node->device_type.type) {
			node->device_type.type
				= register_device_type_scoped(ctx->stage,
											  node->device_type.name->name,
											  node->device_type.scope);
			node->device_type.type->user_data = node;
			node->device_type.type->device_init = config_device_init;
			if (!node->device_type.type) {
				return DISPATCH_ERROR;
			}
		}

		if (!node->device_type.visited) {
			apply_discover_device_type_members(ctx,
											   node->device_type.type->scope,
											   node->cnode->device_type.first_child,
											   node);

			node->device_type.visited = true;
		}

		if (node->device_type.missing_inputs     > 0 ||
			node->device_type.missing_outputs    > 0 ||
			node->device_type.missing_attributes > 0) {
			return DISPATCH_YIELD;
		}
		node->device_type.complete = true;
		finalize_device_type(node->device_type.type);

		node->entry_found = ENTRY_FOUND;

		return DISPATCH_DONE;

	case APPLY_NODE_DEVICE_TYPE_ATTR: {
		struct apply_node *dev_type_node;

		dev_type_node = node->device_type_attribute.owner;
		assert(dev_type_node->type == APPLY_NODE_DEVICE_TYPE);

		if (!node->device_type_attribute.type) {
			node->device_type_attribute.type_lookup
				= scope_lookup_init(ctx->stage, dev_type_node->device_type.scope);
			node->device_type_attribute.type
				= apply_discover_l_expr(ctx, dev_type_node->device_type.scope,
										node->cnode->attr.type,
										&node->device_type_attribute.type_lookup);
		}

		struct apply_node *type_expr;
		type_expr = node->device_type_attribute.type;

		if (type_expr->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}
		else if (type_expr->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		}

		if (node->device_type_attribute.type_lookup.kind != SCOPE_ENTRY_TYPE) {
			apply_error(type_expr->cnode, "Expression is not a type.");
			return DISPATCH_ERROR;
		}

		int err;
		struct scope_lookup_range type_res;
		err = scope_lookup_result_single(node->device_type_attribute.type_lookup, &type_res);
		assert(type_res.length == 1);

		type_id type = type_res.begin;
		struct device_type *dev_type;
		struct atom *name;

		dev_type = dev_type_node->device_type.type;

		name = node->device_type_attribute.name;
		struct device_attribute_def *res;
		res = device_type_add_attribute(ctx->stage, dev_type, name->name, 0, type);
		if (!res) {
			return DISPATCH_ERROR;
		}

		assert(dev_type_node->device_type.missing_attributes > 0);
		dev_type_node->device_type.missing_attributes -= 1;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_DEVICE_TYPE_OUTPUT: // fallthrough
	case APPLY_NODE_DEVICE_TYPE_INPUT: {
		struct apply_node *dev_type_node;

		dev_type_node = node->device_type_input.owner;
		assert(dev_type_node->type == APPLY_NODE_DEVICE_TYPE);

		// @TODO: This should use input or output.
		if (!node->device_type_input.type) {
			node->device_type_input.type
				= apply_discover_l_expr(ctx, dev_type_node->device_type.scope,
										node->cnode->input.type,
										&node->device_type_input.type_lookup);
		}

		struct apply_node *type_expr;
		type_expr = node->device_type_input.type;

		if (type_expr->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}
		else if (type_expr->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		}

		if (node->device_type_input.type_lookup.kind != SCOPE_ENTRY_TYPE) {
			apply_error(type_expr->cnode, "Expression is not a type.");
			return DISPATCH_ERROR;
		}

		int err;
		struct scope_lookup_range type_res;
		err = scope_lookup_result_single(node->device_type_input.type_lookup, &type_res);
		assert(type_res.length == 1);

		type_id type = type_res.begin;
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

	case APPLY_NODE_DEVICE_ATTR: {
		struct apply_node *name;
		name = node->device_attr.name;

		if (name->entry_found == ENTRY_FOUND_WAITING) {
			apply_debug("Waiting for attribute name.");
			return DISPATCH_YIELD;
		}
		else if (name->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		}

		/* if (node->device_type_input.type_lookup.kind != SCOPE_ENTRY_TYPE) { */
		/* 	apply_error(node->device_type_input.type->cnode, */
		/* 				"Expression is not an attribute."); */
		/* 	return DISPATCH_ERROR; */
		/* } */

		struct apply_node *dev;
		dev = node->device_attr.device;

		if (!node->device_attr.value) {
			if (!dev->device.device) {
				apply_debug("Wait for device.");
				return DISPATCH_YIELD;
			}

			struct scope_lookup_range range;
			struct value_ref dest;

			// @TODO: Check the type.

			int err =
				scope_lookup_result_single(node->device_attr.lookup, &range);
			if (err) {
				apply_error(node->device_attr.name->cnode, "Not found.");
				return DISPATCH_ERROR;
			}

			if (!node->device_attr.lookup.type) {
				apply_error(node->device_attr.name->cnode,
							"Not a typed value.");
			}
			dest.type = node->device_attr.lookup.type->id;
			dest.data = &dev->device.device->attribute_values[range.begin];

			if (!dest.data) {
				return DISPATCH_ERROR;
			}

			node->device_attr.value
				= apply_discover_expr(ctx, dev->device.device->scope,
									  node->cnode->binary_op.rhs,
									  dest);
		}

		if (node->device_attr.value->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}
		else if (node->device_attr.value->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		}

		assert(dev->device.missing_attrs > 0);
		dev->device.missing_attrs -= 1;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_DEVICE: {
		if (node->device.type->entry_found == ENTRY_NOT_FOUND) {
			apply_error(node->device.type->cnode, "Device type not found.");
			return DISPATCH_ERROR;
		} else if (node->device.type->entry_found == ENTRY_FOUND_WAITING) {
			apply_debug("Waiting for device type.");
			return DISPATCH_YIELD;
		}

		if (node->device.type_lookup.kind != SCOPE_ENTRY_DEVICE_TYPE) {
			apply_error(node->device.type->cnode, "Not a device type.");
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

		if (!node->device.device) {
			node->device.device
				= register_device_pre_attrs(ctx->stage, dev_type->id,
											node->device.scope, node->device.name);
			if (!node->device.device) {
				apply_debug("Failed to register device.");
				return DISPATCH_ERROR;
			}
		}

		if (!node->device.visited) {
			apply_discover_device_attrs(ctx, node, node->cnode);
			node->device.visited = true;
		}

		if (node->device.not_found_attrs > 0) {
			apply_error(node->cnode, "Missing %zu attributes\n", node->device.not_found_attrs);
			return DISPATCH_ERROR;
		}

		if (node->device.missing_attrs > 0) {
			apply_debug("Waiting for %zu missing attributes.",
						node->device.missing_attrs);
			return DISPATCH_YIELD;
		}

		for (size_t i = 0; i < node->device.num_attrs; i++) {
			struct apply_node *attr;
			attr = node->device.attrs[i];
		}

		err = finalize_device(ctx->stage, node->device.device);

		if (err) {
			apply_error(node->cnode, "Failed to finalized device.");
			return DISPATCH_ERROR;
		}

		return DISPATCH_DONE;
	}

	case APPLY_NODE_TYPE_DECL: {
		struct apply_node *type_node;
		type_node = node->type_decl.type;

		if (node->type_decl.type->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->type_decl.type->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}

		type_id type;
		int err;

		err = apply_resolve_type_id(type_node, &type);
		if (err) {
			return DISPATCH_ERROR;
		}

		register_type_name(ctx->stage, type,
						   node->type_decl.scope,
						   node->type_decl.name);

		node->entry_found = ENTRY_FOUND;

		return DISPATCH_DONE;
	}


	case APPLY_NODE_TYPE_L_EXPR: {
		if (node->type_l_expr.expr->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->type_l_expr.expr->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}

		struct scope_lookup_range range;
		int err;

		if (node->type_l_expr.lookup.kind != SCOPE_ENTRY_TYPE) {
			apply_error(node->cnode, "Not a type.");
			return DISPATCH_ERROR;
		}

		err = scope_lookup_result_single(node->type_l_expr.lookup, &range);
		if (err) {
			return DISPATCH_ERROR;
		}

		node->entry_found = ENTRY_FOUND;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_TYPE_TUPLE: {
		for (size_t i = 0; i < node->type_tuple.num_members; i++) {
			struct apply_tuple_member *member;
			member = &node->type_tuple.members[i];
			if (member->type->entry_found == ENTRY_NOT_FOUND) {
				return DISPATCH_ERROR;
			} else if (member->type->entry_found == ENTRY_FOUND_WAITING) {
				apply_debug("Waiting for member %zu (%.*s) type.",
							i, ALIT(member->name));
				return DISPATCH_YIELD;
			}

			type_id type;
			int err;
			err = apply_resolve_type_id(member->type, &type);
			if (err) {
				return DISPATCH_ERROR;
			}
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

		node->entry_found = ENTRY_FOUND;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_TYPE_SUBRANGE:
		return DISPATCH_ERROR;

	case APPLY_NODE_BINARY_OP:
		return DISPATCH_ERROR;

	case APPLY_NODE_ACCESS: {
		assert(node->access.lhs != NULL);

		if (node->access.lhs->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->access.lhs->entry_found == ENTRY_FOUND_WAITING) {
			apply_debug("Waiting for lhs.");
			return DISPATCH_YIELD;
		}

		if (!node->access.rhs) {
			node->access.rhs
				= apply_discover_l_expr(ctx,
										node->access.scope,
										node->cnode->binary_op.rhs,
										node->access.lookup);
		}

		if (node->access.rhs->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->access.rhs->entry_found == ENTRY_FOUND_WAITING) {
			apply_debug("Waiting for rhs.");
			return DISPATCH_YIELD;
		}

		node->entry_found = node->access.rhs->entry_found;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_ACCESS_INDEX: {
		assert(node->access_index.lhs != NULL);
		assert(node->access_index.index != NULL);

		if (node->access_index.lhs->entry_found   == ENTRY_NOT_FOUND ||
			node->access_index.index->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->access_index.lhs->entry_found   == ENTRY_FOUND_WAITING ||
				   node->access_index.index->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}

		int err;

		err = scope_lookup_index(node->access_index.lookup,
								 node->access_index.value_index);

		if (!err) {
			node->entry_found = ENTRY_FOUND;
		} else {
			apply_debug("Waiting for ident '%.*s'%s.",
						ALIT(node->ident.name),
						node->ident.local_lookup
						? " local lookup"
						: "");
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

		if (lhs_node->entry_found  == ENTRY_NOT_FOUND ||
			(low_node  && low_node->entry_found  == ENTRY_NOT_FOUND) ||
			(high_node && high_node->entry_found == ENTRY_NOT_FOUND)) {
			return DISPATCH_ERROR;
		} else if (lhs_node->entry_found  == ENTRY_FOUND_WAITING ||
				   (low_node  && low_node->entry_found  == ENTRY_FOUND_WAITING) ||
				   (high_node && high_node->entry_found == ENTRY_FOUND_WAITING)) {
			return DISPATCH_YIELD;
		}

		int err;

		size_t low = 0;
		size_t high = SCOPE_LOOKUP_RANGE_END;

		if (node->access_index_range.low_index) {
			low  = node->access_index_range.value_low_index;
		}
		if (node->access_index_range.high_index) {
			high = node->access_index_range.value_high_index;
		}

		err = scope_lookup_range(node->access_index_range.lookup, low, high);

		if (!err) {
			node->entry_found = ENTRY_FOUND;
		} else {
			apply_debug("Waiting for ident '%.*s'%s.",
						ALIT(node->ident.name),
						node->ident.local_lookup
						? " local lookup"
						: "");
		}
		return (err == 0) ? DISPATCH_DONE : DISPATCH_YIELD;
	}

	case APPLY_NODE_BIND: {
		if (node->bind.lhs->entry_found == ENTRY_NOT_FOUND ||
			node->bind.rhs->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->bind.lhs->entry_found == ENTRY_FOUND_WAITING ||
				   node->bind.rhs->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}

		if (node->bind.lookup_lhs.kind != SCOPE_ENTRY_DEVICE_CHANNEL) {
			apply_error(node->bind.lhs->cnode, "Not a channel.");
			return DISPATCH_ERROR;
		}

		if (node->bind.lookup_rhs.kind != SCOPE_ENTRY_DEVICE_CHANNEL) {
			apply_error(node->bind.rhs->cnode, "Not a channel.");
			return DISPATCH_ERROR;
		}

		/* print_steps(node->bind.lookup_lhs); */

		size_t lhs_i = 0, rhs_i = 0;
		int err;
		struct scope_lookup_range lhs_range;
		struct scope_lookup_range rhs_range;

		size_t lhs_instances, rhs_instances;

		lhs_instances = scope_lookup_instances(node->bind.lookup_lhs);
		rhs_instances = scope_lookup_instances(node->bind.lookup_rhs);

		if (lhs_instances == rhs_instances) {
			while ((err = scope_lookup_iterate(node->bind.lookup_lhs,
											&lhs_i, &lhs_range)) == LOOKUP_FOUND &&
				(err = scope_lookup_iterate(node->bind.lookup_rhs,
											&rhs_i, &rhs_range)) == LOOKUP_FOUND) {
				if (lhs_range.length != rhs_range.length) {
					apply_error(node->cnode, "Left and right side does not match.");
					return DISPATCH_ERROR;
				}

				for (size_t i = 0; i < lhs_range.length; i++) {
					channel_bind(ctx->stage,
								rhs_range.begin + i,
								lhs_range.begin + i);
				}
			}
		}
		else if (rhs_instances == 1) {
			err = scope_lookup_iterate(node->bind.lookup_rhs,
									   &rhs_i, &rhs_range);

			if (err) {
				apply_error(node->cnode, "Could not iterate.");
				return DISPATCH_ERROR;
			}

			while ((err = scope_lookup_iterate(node->bind.lookup_lhs,
											   &lhs_i, &lhs_range)) == LOOKUP_FOUND) {
				if (lhs_range.length != rhs_range.length) {
					apply_error(node->cnode, "Left and right side does not match.");
					return DISPATCH_ERROR;
				}

				for (size_t i = 0; i < lhs_range.length; i++) {
					channel_bind(ctx->stage,
								rhs_range.begin + i,
								lhs_range.begin + i);
				}
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
		int err;
		err = scope_lookup_ident(node->ident.lookup, node->ident.name);

		if (!err) {
			node->entry_found = ENTRY_FOUND;
		} else {
			apply_debug("Waiting for ident '%.*s'%s.",
						ALIT(node->ident.name),
						node->ident.local_lookup
						? " local lookup"
						: "");
		}
		return (err == 0) ? DISPATCH_DONE : DISPATCH_YIELD;
	}

	case APPLY_NODE_L_VALUE: {
		struct apply_node *expr;

		expr = node->l_value.expr;
		if (expr->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (expr->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}

		// @TODO: Fix
		return DISPATCH_ERROR;

		/* switch (expr->entry.kind) { */
		/* case SCOPE_ENTRY_TYPE: */
		/* 	assert(node->l_value.dest.type == ctx->stage->standard_types.type); */
		/* 	node->l_value.dest.data[0] = expr->entry.id; */
		/* 	break; */

		/* case SCOPE_ENTRY_DEVICE_ATTRIBUTE: */
		/* 	printf("@TODO: Implement evaluation of device attribute value into expr.\n"); */
		/* 	break; */

		/* default: */
		/* 	// @TODO: Improve error message. Tell what the found */
		/* 	// variable is. */
		/* 	apply_error(node->cnode, "Cannot read this entry."); */
		/* 	return DISPATCH_ERROR; */
		/* } */

		node->entry_found = ENTRY_FOUND;
		return DISPATCH_DONE;
	}

	case APPLY_NODE_NUMLIT:
		if (node->literal.dest.type == ctx->stage->standard_types.integer) {
			node->literal.dest.data[0] = node->literal.numlit;
			node->entry_found = ENTRY_FOUND;
		} else {
			printf("@TODO: Implement literal unification.");
			node->entry_found = ENTRY_NOT_FOUND;
		}

		return DISPATCH_DONE;
	}

	printf("Encountered an unexpeted node '%s'!\n",
		   apply_node_name(node->type));
	return DISPATCH_ERROR;
}

int apply_config(struct stage *stage, struct config_node *node)
{
	struct apply_context ctx = {0};
	ctx.stage = stage;

	apply_discover(&ctx, &ctx.stage->root_scope, node);

	size_t generation = 0;
	size_t last_successful_generation = 0;

	while (ctx.queue) {
		struct apply_node *node;
		enum apply_dispatch_result result;

		node = pop_apply_node(&ctx);

		if (node->generation + 1 > generation) {
			generation = node->generation + 1;

			// Wait for 1 generations before aborting, to allow newly
			// created nodes to be applied.
			if (generation > last_successful_generation + 1) {
				// @TODO: Print what was not applied.
				printf("Failed to apply config. Some attributes where not applied.\n");
				return -1;
			}
		}

		node->generation = generation;

		#if APPLY_DEBUG
		printf("%zu:Applying %s... ", node->generation, apply_node_name(node->type));
		#endif

		result = apply_dispatch(&ctx, node);

		if (result == DISPATCH_ERROR) {
			node->entry_found = ENTRY_NOT_FOUND;
			apply_debug("ERROR");
			printf("Failed to apply '%s'. Aborting.\n",
				   apply_node_name(node->type));
			return -1;
		} else if (result == DISPATCH_YIELD) {
			apply_debug("Yield");
			push_apply_node(&ctx, node);
		} else {
			last_successful_generation = generation;
			apply_debug("Ok");
		}
	}

	apply_debug("Application done\n");

	return 0;
}
