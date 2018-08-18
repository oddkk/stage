#include "config.h"
#include "stage.h"
#include "device_type.h"
#include "device.h"
#include "utils.h"
#include "dlist.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <stdatomic.h>

#define APPLY_DEBUG 0

enum apply_node_type {
	APPLY_NODE_DEVICE_TYPE,
	APPLY_NODE_DEVICE_TYPE_ATTR,
	APPLY_NODE_DEVICE_TYPE_INPUT,
	APPLY_NODE_DEVICE_TYPE_OUTPUT,

	APPLY_NODE_DEVICE,
	APPLY_NODE_DEVICE_ATTR,

	APPLY_NODE_TYPE_DECL,

	APPLY_NODE_TYPE_TUPLE,
	APPLY_NODE_TYPE_SUBRANGE,

	APPLY_NODE_BINARY_OP,
	APPLY_NODE_ACCESS,
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

		APPLY_NODE_NAME(TYPE_TUPLE);
		APPLY_NODE_NAME(TYPE_SUBRANGE);

		APPLY_NODE_NAME(BINARY_OP);
		APPLY_NODE_NAME(ACCESS);
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
		} device_type_attribute;

		struct {
			struct atom *name;
			struct apply_node *owner;
			struct apply_node *type;
		} device_type_input;

		struct {
			struct atom *name;
			struct apply_node *owner;
			struct apply_node *type;
		} device_type_output;

		struct {
			bool visited;

			struct device *device;
			struct atom *name;
			struct scoped_hash *scope;
			struct apply_node *type;

			struct apply_node **attrs;
			size_t num_attrs;
			size_t not_found_attrs;
			size_t missing_attrs;
		} device;

		struct {
			struct apply_node *name;
			struct apply_node *value;

			struct apply_node *device;
		} device_attr;

		struct {
			struct atom *name;
			struct apply_node *type;
			struct scoped_hash *scope;
		} type_decl;

		struct {
			struct atom *name;
			bool named;
			struct apply_tuple_member *members;
			size_t num_members;
		} type_tuple;

		struct {
			struct atom *name;
			struct apply_node *lhs;
			struct apply_node *low;
			struct apply_node *high;

			scalar_value value_low;
			scalar_value value_high;

			scalar_value buffer[2];
		} type_subrange;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;
			bool local_lookup;

			scalar_value *dest;
		} access;

		struct {
			struct scoped_hash *scope;
			struct atom *name;
			bool local_lookup;
		} ident;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;
			enum config_binary_op op;

			struct value_ref dest;
		} binary_op;

		struct {
			struct apply_node *lhs;
			struct apply_node *rhs;
		} bind;

		struct {
			scalar_value numlit;

			struct value_ref dest;
		} literal;

		struct {
			struct apply_node *expr;

			struct value_ref dest;
		} l_value;
	};

	enum entry_found_state entry_found;
	struct scope_entry entry;
};

struct apply_queue_entry {
	int _dc;
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

static struct apply_node *apply_discover_l_expr(struct apply_context *ctx,
												struct scoped_hash *scope,
												struct config_node *expr,
												bool local_lookup)
{
	switch (expr->type) {
	case CONFIG_NODE_IDENT: {
		struct apply_node *node;

		node = alloc_apply_node(ctx, APPLY_NODE_IDENT, expr);
		node->ident.name = expr->ident;
		node->ident.scope = scope;
		node->ident.local_lookup = local_lookup;

		push_apply_node(ctx, node);

		return node;
	} break;

	case CONFIG_NODE_BINARY_OP: {
		if (expr->binary_op.op == CONFIG_OP_ACCESS) {
			struct apply_node *node;
			node = alloc_apply_node(ctx, APPLY_NODE_ACCESS, expr);
			node->access.local_lookup = local_lookup;
			node->access.lhs = apply_discover_l_expr(ctx, scope, expr->binary_op.lhs, local_lookup);

			push_apply_node(ctx, node);

			return node;
		} else {
			apply_error(expr, "Invalid node in l-expression.");
		}
	} break;

	case CONFIG_NODE_SUBSCRIPT_RANGE:
		printf("@TODO: Subscript range\n");
		break;

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
			node->l_value.expr = apply_discover_l_expr(ctx, scope, expr, false);
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
		node->l_value.expr = apply_discover_l_expr(ctx, scope, expr, false);
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
			device_type->device_type.missing_attributes += 1;

			push_apply_node(ctx, attr);
		} break;

		case CONFIG_NODE_INPUT: {
			struct apply_node *input;

			input = alloc_apply_node(ctx, APPLY_NODE_DEVICE_TYPE_INPUT, node);
			input->device_type_input.owner = device_type;
			input->device_type_attribute.name = node->input.name;
			device_type->device_type.missing_inputs += 1;

			push_apply_node(ctx, input);
		} break;

		case CONFIG_NODE_OUTPUT: {
			struct apply_node *output;

			output = alloc_apply_node(ctx, APPLY_NODE_DEVICE_TYPE_OUTPUT, node);
			output->device_type_output.owner = device_type;
			output->device_type_attribute.name = node->output.name;
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
	node->bind.lhs = apply_discover_l_expr(ctx, scope, bind_node->binary_op.lhs, false);
	node->bind.rhs = apply_discover_l_expr(ctx, scope, bind_node->binary_op.rhs, false);

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

			attr->device_attr.name
				= apply_discover_l_expr(ctx, device->device.device->scope, lhs, true);

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
	device->device.type
		= apply_discover_l_expr(ctx, scope, device_node->device.type, false);

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

			type->type_subrange.lhs
				= apply_discover_l_expr(ctx, scope, type_expr->subrange.lhs, false);

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
		return apply_discover_l_expr(ctx, scope, type_node, false);
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

			child_scope = scoped_hash_insert_namespace(scope,
													   node->namespace.name,
													   node);
			/* child_scope = scoped_hash_push(scope, SCOPE_ENTRY_NAMESPACE, 0); */
			/* scoped_hash_insert(scope, node->namespace.name, SCOPE_ENTRY_NAMESPACE, */
			/* 				   0, node, child_scope); */

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

		node->entry.kind = SCOPE_ENTRY_DEVICE_TYPE;
		node->entry.id = node->device_type.type->id;
		node->entry.scope = node->device_type.type->scope;
		node->entry_found = ENTRY_FOUND;

		return DISPATCH_DONE;

	case APPLY_NODE_DEVICE_TYPE_ATTR: {
		struct apply_node *dev_type_node;

		dev_type_node = node->device_type_attribute.owner;
		assert(dev_type_node->type == APPLY_NODE_DEVICE_TYPE);

		if (!node->device_type_attribute.type) {
			node->device_type_attribute.type
				= apply_discover_l_expr(ctx, dev_type_node->device_type.scope,
										node->cnode->attr.type, false);
		}

		struct apply_node *type_expr;
		type_expr = node->device_type_attribute.type;

		if (type_expr->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}
		else if (type_expr->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		}

		if (type_expr->entry.kind != SCOPE_ENTRY_TYPE) {
			apply_error(type_expr->cnode, "Expression is not a type.");
			return DISPATCH_ERROR;
		}

		type_id type;
		struct device_type *dev_type;
		struct atom *name;

		type = type_expr->entry.id;
		dev_type = dev_type_node->device_type.type;

		name = node->device_type_attribute.name;
		//device_type_add_output(ctx->stage, dev_type, name->name, type);
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
										node->cnode->input.type, false);
		}

		struct apply_node *type_expr;
		type_expr = node->device_type_input.type;

		if (type_expr->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}
		else if (type_expr->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		}

		if (type_expr->entry.kind != SCOPE_ENTRY_TYPE) {
			apply_error(type_expr->cnode, "Expression is not a type.");
			return DISPATCH_ERROR;
		}

		type_id type;
		struct device_type *dev_type;
		struct atom *name;

		type = type_expr->entry.id;
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

		if (name->entry.kind != SCOPE_ENTRY_DEVICE_ATTRIBUTE) {
			apply_error(name->cnode, "Not an attribute");
			return DISPATCH_ERROR;
		}

		struct apply_node *dev;
		dev = node->device_attr.device;

		if (!node->device_attr.value) {
			if (!dev->device.device) {
				apply_debug("Wait for device.");
				return DISPATCH_YIELD;
			}

			struct value_ref dest;
			dest = device_get_attr_from_entry(ctx->stage,
											  dev->device.device,
											  name->entry);

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

		if (node->device.type->entry.kind != SCOPE_ENTRY_DEVICE_TYPE) {
			apply_error(node->device.type->cnode, "Not a device type.");
			return DISPATCH_ERROR;
		}

		struct device_type *dev_type;
		dev_type = get_device_type(ctx->stage, node->device.type->entry.id);

		if (!dev_type->finalized) {
			apply_debug("Waiting for device type finalization (%.*s).",
						ALIT(dev_type->name));
			return DISPATCH_YIELD;
		}

		if (!node->device.device) {
			node->device.device
				= register_device_pre_attrs(ctx->stage, node->device.type->entry.id,
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

		int err;
		err = finalize_device(ctx->stage, node->device.device);

		if (err) {
			apply_error(node->cnode, "Failed to finalized device.");
			return DISPATCH_ERROR;
		}

		return DISPATCH_DONE;
	}

	case APPLY_NODE_TYPE_DECL:
		if (node->type_decl.type->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->type_decl.type->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}

		if (node->type_decl.type->entry.kind != SCOPE_ENTRY_TYPE) {
			apply_error(node->type_decl.type->cnode,
						"Not a type.");
			return DISPATCH_ERROR;
		}

		register_type_name(ctx->stage,
						   node->type_decl.type->entry.id,
						   node->type_decl.scope,
						   node->type_decl.name);
		return DISPATCH_DONE;

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

			if (member->type->entry.kind != SCOPE_ENTRY_TYPE) {
				// @TODO: Better error message.
				apply_error(member->type->cnode, "Not a type.");
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
				new_member->type = member->type->entry.id;
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
				new_type.tuple.types[i] = member->type->entry.id;
			}
		}

		struct type *final_type;

		final_type = register_type(ctx->stage, new_type);

		if (!final_type) {
			return DISPATCH_ERROR;
		}

		node->entry.kind = SCOPE_ENTRY_TYPE;
		node->entry.id = final_type->id;
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
			return DISPATCH_YIELD;
		}

		if (!node->access.rhs) {
			if (!node->access.lhs->entry.scope) {
				// @TODO: Add what expresseion does not have rhs to
				// the error message, and what rhs is.
				apply_error(node->access.lhs->cnode,
							"Does not have the member.");
				return DISPATCH_ERROR;
			}
			node->access.rhs
				= apply_discover_l_expr(ctx,
										node->access.lhs->entry.scope,
										node->cnode->binary_op.rhs,
										true);
		}

		if (node->access.rhs->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->access.rhs->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}

		node->entry_found = node->access.rhs->entry_found;
		node->entry = node->access.rhs->entry;

		return DISPATCH_DONE;
	}

	case APPLY_NODE_BIND: {
		if (node->bind.lhs->entry_found == ENTRY_NOT_FOUND ||
			node->bind.rhs->entry_found == ENTRY_NOT_FOUND) {
			return DISPATCH_ERROR;
		} else if (node->bind.lhs->entry_found == ENTRY_FOUND_WAITING ||
				   node->bind.rhs->entry_found == ENTRY_FOUND_WAITING) {
			return DISPATCH_YIELD;
		}

		channel_bind(ctx->stage,
					 node->bind.rhs->entry.id,
					 node->bind.lhs->entry.id);

		return DISPATCH_DONE;
	}

	case APPLY_NODE_IDENT: {
		int err;
		if (node->ident.local_lookup) {
			err = scoped_hash_local_lookup(node->ident.scope,
										   node->ident.name,
										   &node->entry);
		} else {
			err = scoped_hash_lookup(node->ident.scope,
									 node->ident.name,
									 &node->entry);
		}
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

		switch (expr->entry.kind) {
		case SCOPE_ENTRY_TYPE:
			assert(node->l_value.dest.type == ctx->stage->standard_types.type);
			node->l_value.dest.data[0] = expr->entry.id;
			break;

		case SCOPE_ENTRY_DEVICE_ATTRIBUTE:
			printf("@TODO: Implement evaluation of device attribute value into expr.\n");
			break;

		default:
			// @TODO: Improve error message. Tell what the found
			// variable is.
			apply_error(node->cnode, "Cannot read this entry.");
			return DISPATCH_ERROR;
		}

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
