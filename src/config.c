#include "config.h"
#include "stage.h"
#include "device_type.h"
#include "device.h"
#include "utils.h"
#include "dlist.h"
#include <stdio.h>
#include <stdlib.h>

void config_apply_devices(struct stage *stage, struct config_node *node, struct scoped_hash *scope);

static void _print_indent(int depth) {
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

static char *_binary_op_name(enum config_binary_op op) {
	switch (op) {
	case CONFIG_OP_ASSIGN:    return "=";
	case CONFIG_OP_BIND:      return "<-";
	case CONFIG_OP_ADD:       return "+";
	case CONFIG_OP_SUB:       return "-";
	case CONFIG_OP_MUL:       return "*";
	case CONFIG_OP_DIV:       return "/";
	case CONFIG_OP_ACCESS:    return ".";
	case CONFIG_OP_SUBSCRIPT: return "[]";
	}
}

static void _config_print_tree(struct config_node *node, int depth) {
	if (!node) {
		return;
	}

	_print_indent(depth);

	switch (node->type) {
	case CONFIG_NODE_MODULE:
		printf("module\n");
		_print_indent(depth + 1);
		printf("version: %i.%i.%i\n",
			   node->module.version.major,
			   node->module.version.minor,
			   node->module.version.patch);
		_config_print_tree(node->module.first_child, depth + 1);
		break;
	case CONFIG_NODE_DEVICE_TYPE:
		printf("device_type\n");
		_print_indent(depth + 1);
		printf("name: %.*s\n", LIT(node->device_type.name->name));
		_config_print_tree(node->device_type.first_child, depth + 1);
		break;
	case CONFIG_NODE_DEVICE:
		printf("device\n");

		_print_indent(depth + 1);
		if (node->device.name) {
			printf("name: %.*s\n", LIT(node->device.name->name));
		} else {
			printf("name: (unnamed)\n");
		}

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->device.type, depth + 2);

		_config_print_tree(node->device.first_child, depth + 1);
		break;

	case CONFIG_NODE_TYPE_DECL:
		printf("type_decl\n");

		_print_indent(depth + 1);
		printf("name: %.*s\n", LIT(node->type_decl.name->name));

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->type_decl.type, depth + 2);
		break;

	case CONFIG_NODE_ATTR:
		printf("attr\n");

		_print_indent(depth + 1);
		printf("name: %.*s\n", LIT(node->attr.name->name));

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->attr.type, depth + 2);

		_print_indent(depth + 1);
		printf("def_value:\n");
		_config_print_tree(node->attr.def_value, depth + 2);
		break;

	case CONFIG_NODE_INPUT:
		printf("input\n");

		_print_indent(depth + 1);
		if (node->input.name) {
			printf("name: %.*s\n", LIT(node->input.name->name));
		} else {
			printf("name: _\n");
		}

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->input.type, depth + 2);
		break;

	case CONFIG_NODE_OUTPUT:
		printf("output\n");

		_print_indent(depth + 1);
		if (node->output.name) {
			printf("name: %.*s\n", LIT(node->output.name->name));
		} else {
			printf("name: _\n");
		}

		_print_indent(depth + 1);
		printf("type:\n");
		_config_print_tree(node->output.type, depth + 2);
		break;

	case CONFIG_NODE_BINARY_OP:
		printf("binary %s\n", _binary_op_name(node->binary_op.op));

		_print_indent(depth + 1);
		printf("lhs:\n");
		_config_print_tree(node->binary_op.lhs, depth + 2);

		_print_indent(depth + 1);
		printf("rhs:\n");
		_config_print_tree(node->binary_op.rhs, depth + 2);

		break;

	case CONFIG_NODE_SUBSCRIPT_RANGE:
		printf("subscript range\n");

		_print_indent(depth + 1);
		printf("lhs:\n");
		_config_print_tree(node->subscript_range.lhs, depth + 2);

		_print_indent(depth + 1);
		printf("low:\n");
		_config_print_tree(node->subscript_range.low, depth + 2);

		_print_indent(depth + 1);
		printf("high:\n");
		_config_print_tree(node->subscript_range.high, depth + 2);
		break;

	case CONFIG_NODE_IDENT:
		if (node->ident) {
			printf("ident %.*s\n", LIT(node->ident->name));
		} else {
			printf("ident (self)\n");
		}
		break;

	case CONFIG_NODE_NUMLIT:
		printf("numlit %i\n", node->numlit);
		break;
	}

	if (node->next_sibling) {
		_config_print_tree(node->next_sibling, depth);
	}
}

void config_print_tree(struct config_node *node) {
	_config_print_tree(node, 0);
}

int config_eval_l_expr_internal(struct scoped_hash *scope, struct config_node *expr, bool restrict_local, struct scope_entry *result, struct scoped_hash **owner)
{
	switch (expr->type) {
	case CONFIG_NODE_IDENT:
		if (restrict_local) {
			if (!expr->ident) {
				printf("Cannot have a self node not in this device.");
				break;
			}

			if (owner) {
				*owner = scope;
			}
			return scoped_hash_local_lookup(scope, expr->ident, result);
		} else {
			if (!expr->ident) {
				if (!scope->parent) {
					printf("This scope can not be referenced as self.\n");
					return false;
				}

				assert(scope->parent->num_entries < scope->id);
				*result = scope->parent->entries[scope->id];
				if (owner) {
					*owner = scope->parent;
				}

				return true;
			}

			return scoped_hash_lookup_owner(scope, expr->ident, result, owner);
		}
		break;

	case CONFIG_NODE_BINARY_OP:
		switch (expr->binary_op.op) {
		case CONFIG_OP_ACCESS: {
			struct scope_entry lhs;
			int err;

			err = config_eval_l_expr_internal(scope, expr->binary_op.lhs, restrict_local, &lhs, NULL);

			if (err) {
				return err;
			}

			if (!lhs.scope) {
				// @TODO: Improve this error message.
				printf("%.*s has no members.\n", ALIT(lhs.name));
				return -1;
			}

			return config_eval_l_expr_internal(lhs.scope, expr->binary_op.rhs, true, result, owner);
		} break;

		default:
			printf("Got unexpected operator '%s' in l-expression.",
				   _binary_op_name(expr->binary_op.op));
		}

	default:
		printf("@TODO: Implement evaluating l_expressions!\n");
		return -1;
	}

	return 0;
}

int config_eval_l_expr(struct scoped_hash *scope, struct config_node *expr, struct scope_entry *result)
{
	return config_eval_l_expr_internal(scope, expr, false, result, NULL);
}

int config_eval_l_expr_owner(struct scoped_hash *scope, struct config_node *expr, struct scope_entry *result, struct scoped_hash **owner)
{
	return config_eval_l_expr_internal(scope, expr, false, result, owner);
}

void print_l_expr(struct config_node *expr) {
	switch (expr->type) {
	case CONFIG_NODE_IDENT:
		if (expr->ident) {
			printf("%.*s", ALIT(expr->ident));
		} else {
			printf("_");
		}
		break;

	case CONFIG_NODE_BINARY_OP:
		print_l_expr(expr->binary_op.lhs);

		if (expr->binary_op.op == CONFIG_OP_SUBSCRIPT) {
			printf("[");
			printf("(expr)");
			printf("]");
		} else if (expr->binary_op.op == CONFIG_OP_ACCESS) {
			printf(".");
			print_l_expr(expr->binary_op.rhs);
		} else {
			printf("(non l-expr)");
		}
		break;

	default:
		printf("(non l-expr)");
		break;
	}
}

int config_eval_scalar_expr(struct scoped_hash *scope, struct config_node *expr, scalar_value *result)
{
	switch (expr->type) {
	case CONFIG_NODE_NUMLIT:
		*result = expr->numlit;
		return 0;

	default:
		printf("@TODO: Implement eval expr\n");
		break;
	}

	return -1;
}


void device_apply_config_node(struct stage *stage, struct device_type *dev_type, struct device *dev, struct config_node *node)
{
	while (node) {
		switch (node->type) {
		case CONFIG_NODE_DEVICE:
			break;

		case CONFIG_NODE_BINARY_OP:
			switch (node->binary_op.op) {
			case CONFIG_OP_BIND: {
				struct scoped_hash *owner;
				struct scope_entry lhs_entry, rhs_entry;
				channel_id input, output;
				int err;

				if (node->binary_op.lhs->type != CONFIG_NODE_IDENT || node->binary_op.lhs->ident) {
					err = config_eval_l_expr_owner(dev->scope, node->binary_op.lhs, &lhs_entry, &owner);
					if (err) {
						printf("Could not find input '");
						print_l_expr(node->binary_op.lhs);
						printf("' for '%.*s'.\n", ALIT(dev_type->name));
						break;
					}

					if (lhs_entry.kind == SCOPE_ENTRY_DEVICE_INPUT ||
						(lhs_entry.kind == SCOPE_ENTRY_DEVICE_OUTPUT &&
						 owner->id == dev->id)) {
						struct device *lhs_dev;
						assert(owner->kind == SCOPE_ENTRY_DEVICE);
						lhs_dev = get_device(stage, owner->id);

						input = lhs_dev->input_begin + lhs_entry.id;
					} else if (lhs_entry.kind == SCOPE_ENTRY_DEVICE) {
						struct device *lhs_dev;
						struct device_type *lhs_dev_type;

						lhs_dev = get_device(stage, lhs_entry.id);
						lhs_dev_type = get_device_type(stage, lhs_dev->type);

						input = lhs_dev->input_begin + lhs_dev_type->self_input;

						if (input < 0) {
							printf("'");
							print_l_expr(node->binary_op.lhs);
							printf("' does not have a self input. (%.*s)\n", ALIT(dev_type->name));
							break;
						}
					} else {
						printf("'");
						print_l_expr(node->binary_op.lhs);
						printf("' is not an input for %.*s.\n", ALIT(dev_type->name));
						break;
					}
				} else {
					if (dev_type->self_input < 0) {
						printf("Device '%.*s' of type '%.*s' has no self input.\n",
							   ALIT(dev->name), ALIT(dev_type->name));
						break;
					}
					input = dev->input_begin + dev_type->self_input;
				}

				if (node->binary_op.rhs->type != CONFIG_NODE_IDENT || node->binary_op.rhs->ident) {
					err = config_eval_l_expr_owner(dev->scope, node->binary_op.rhs, &rhs_entry, &owner);
					if (err) {
						printf("Could not find output '");
						print_l_expr(node->binary_op.rhs);
						printf("' for '%.*s'.\n", ALIT(dev_type->name));
						break;
					}

					if (rhs_entry.kind == SCOPE_ENTRY_DEVICE_OUTPUT ||
						(rhs_entry.kind == SCOPE_ENTRY_DEVICE_INPUT &&
						 owner->id == dev->id)) {
						struct device *rhs_dev;
						assert(owner->kind == SCOPE_ENTRY_DEVICE);
						rhs_dev = get_device(stage, owner->id);

						output = rhs_dev->output_begin + rhs_entry.id;
					} else if (rhs_entry.kind == SCOPE_ENTRY_DEVICE) {
						struct device *rhs_dev;
						struct device_type *rhs_dev_type;

						rhs_dev = get_device(stage, rhs_entry.id);
						rhs_dev_type = get_device_type(stage, rhs_dev->type);

						output = rhs_dev->output_begin + rhs_dev_type->self_output;

						if (output < 0) {
							printf("'");
							print_l_expr(node->binary_op.rhs);
							printf("' does not have a self output. (%.*s)\n", ALIT(dev_type->name));
							break;
						}
					} else {
						printf(" not found\n");

						printf("'");
						print_l_expr(node->binary_op.rhs);
						printf("' is not an output for %.*s.\n", ALIT(dev_type->name));
						break;
					}
				} else {
					if (dev_type->self_output < 0) {
						printf("Device '%.*s' of type '%.*s' has no self output.\n",
							   ALIT(dev->name), ALIT(dev_type->name));
						break;
					}
					output = dev->output_begin + dev_type->self_output;
				}

				channel_bind(stage, output, input);

			} break;

			case CONFIG_OP_ASSIGN:
				break;

			default:
				// @TODO: Is this an error?
				printf("(%.*s: %.*s)The %s operator has no effect in this scope\n", ALIT(dev->name), ALIT(dev_type->name), _binary_op_name(node->binary_op.op));
				break;
			}
			break;

		default:
			break;
		}

		node = node->next_sibling;
	}
}

int device_type_config_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct config_node *dev_node = (struct config_node *)dev->data;
	struct config_node *dev_type_node = (struct config_node *)type->user_data;

	assert(dev_node && dev_node->type == CONFIG_NODE_DEVICE);
	assert(dev_type_node && dev_type_node->type == CONFIG_NODE_DEVICE_TYPE);

	config_apply_devices(stage, dev_type_node->device_type.first_child, type->scope);
	config_apply_devices(stage, dev_node->device.first_child, dev->scope);

	device_apply_config_node(stage, type, dev, dev_type_node->device_type.first_child);
	device_apply_config_node(stage, type, dev, dev_node->device.first_child);

	return 0;
}

void config_apply_device_type(struct stage *stage, struct config_node *node, struct device_type *dev_type)
{
	struct device_channel_def *channel;
	while (node) {
		switch (node->type) {
		case CONFIG_NODE_ATTR: {
			scalar_value default_value = 0;

			if (node->attr.def_value) {
				config_eval_scalar_expr(dev_type->scope, node->attr.def_value, &default_value);
			}

			// @TODO: Fix evaluating type and default value.
			device_type_add_attribute(stage, dev_type, node->attr.name->name, default_value);
		} break;

		case CONFIG_NODE_INPUT:
			// @TODO: Fix evaluating type.
			channel = device_type_add_input(stage, dev_type, node->input.name->name,
											stage->standard_types.integer);

			if (node->input.def) {
				if (dev_type->self_input >= 0) {
					// @TODO: Improve this error message with the name
					// of the current default channel.
					printf("The device type '%.*s' already has a default input channel.",
						   ALIT(dev_type->name));
					break;
				}
				dev_type->self_input = channel->id;
			}
			break;

		case CONFIG_NODE_OUTPUT:
			// @TODO: Fix evaluating type.
			channel = device_type_add_output(stage, dev_type, node->output.name->name,
											 stage->standard_types.integer);

			if (node->output.def) {
				if (dev_type->self_output >= 0) {
					// @TODO: Improve this error message with the name
					// of the current default channel.
					printf("The device type '%.*s' already has a default output channel.",
						   ALIT(dev_type->name));
					break;
				}
				dev_type->self_output = channel->id;
			}
			break;

		default:
			break;

		/* case CONFIG_NODE_DEVICE: break; */
		/* case CONFIG_NODE_TYPE_DECL: break; */
		/* case CONFIG_NODE_ATTR: break; */
		/* case CONFIG_NODE_INPUT: break; */
		/* case CONFIG_NODE_OUTPUT: break; */
		/* case CONFIG_NODE_BINARY_OP: break; */
		/* case CONFIG_NODE_SUBSCRIPT_RANGE: break; */
		/* case CONFIG_NODE_IDENT: break; */
		/* case CONFIG_NODE_NUMLIT: break; */
		}

		node = node->next_sibling;
	}
}

void config_apply_device_types(struct stage *stage, struct config_node *node, struct scoped_hash *scope)
{
	while (node) {
		switch (node->type) {
		case CONFIG_NODE_MODULE:
			config_apply_device_types(stage, node->module.first_child, scope);
			break;

		case CONFIG_NODE_DEVICE_TYPE: {
			struct device_type *dev_type;
			dev_type = register_device_type_scoped(stage, node->device_type.name->name, scope);

			if (!dev_type) {
				print_error("apply device types", "Could not register device type %.*s", ALIT(node->device_type.name));
				continue;
			}

			dev_type->user_data = node;
			dev_type->device_init = device_type_config_init;
			config_apply_device_type(stage, node->device_type.first_child, dev_type);
		} break;

		default:
			break;
		}

		node = node->next_sibling;
	}
}


static size_t count_device_attributes(struct config_node *node) {
	struct config_node *current;
	size_t result = 0;

	current = node;

	if (!current) {
		return result;
	}

	while (current) {
		switch (node->type) {
		case CONFIG_NODE_BINARY_OP:
			if (node->binary_op.op == CONFIG_OP_ASSIGN) {
				result += 1;
			}
			break;

		default: break;
		}

		current = current->next_sibling;
	}

	return result;
}

static void eval_device_attributes(struct stage *stage, struct config_node *node,
								   struct scoped_hash *scope,
								   struct device_attribute *attributes, size_t capacity) {
	struct config_node *current;

	current = node;

	if (!current) {
		return;
	}

	while (current) {
		switch (node->type) {
		case CONFIG_NODE_BINARY_OP:
			if (node->binary_op.op == CONFIG_OP_ASSIGN) {
				struct device_attribute *attr;

				assert(capacity > 0);

				attr = attributes++;
				capacity -= 1;

				assert(node->binary_op.lhs->type == CONFIG_NODE_IDENT);

				attr->name = node->binary_op.lhs->ident;
				config_eval_scalar_expr(scope, node->binary_op.rhs, &attr->value);
			}
			break;

		default: break;
		}

		current = current->next_sibling;
	}

	// Make sure we have used all assigned attributes.
	assert(capacity == 0);
}

void config_apply_devices(struct stage *stage, struct config_node *node, struct scoped_hash *scope)
{
	while (node) {
		switch (node->type) {
		case CONFIG_NODE_MODULE:
			config_apply_devices(stage, node->module.first_child, scope);
			break;

		case CONFIG_NODE_DEVICE: {
			struct device *dev;
			struct device_type *dev_type;
			struct scope_entry dev_type_entry;
			struct device_attribute *attributes;
			size_t num_attributes;
			int err;

			err = config_eval_l_expr(scope, node->device.type, &dev_type_entry);

			if (err) {
				// @TODO: Improve this error message. It should state
				// the attempted identifier.
				print_error("apply device", "Could not find device type for %.*s",
							ALIT(node->device.name));
				return;
			}

			if (dev_type_entry.kind != SCOPE_ENTRY_DEVICE_TYPE) {
				// @TODO: Improve this error message. It should state
				// the attempted identifier.
				print_error("apply device", "This is not a device type for device %.*s!",
							ALIT(node->device.name));
				return;
			}

			dev_type = get_device_type(stage, dev_type_entry.id);

			num_attributes = count_device_attributes(node->device.first_child);
			// @TODO: Use another allocation scheme?
			attributes = calloc(num_attributes, sizeof(struct device_attribute));

			// @TODO: Scope should be the scope of this device. We
			// should also allow reference to other attributes of this
			// device.
			eval_device_attributes(stage, node->device.first_child, scope, attributes, num_attributes);

			dev = register_device_scoped(stage, dev_type->id, node->device.name, scope, attributes, num_attributes, node);

			free(attributes);

			node->device.id = dev->id;

			config_apply_devices(stage, node->module.first_child, dev->scope);
		} break;

		default:
			break;

		/* case CONFIG_NODE_DEVICE: break; */
		/* case CONFIG_NODE_TYPE_DECL: break; */
		/* case CONFIG_NODE_ATTR: break; */
		/* case CONFIG_NODE_INPUT: break; */
		/* case CONFIG_NODE_OUTPUT: break; */
		/* case CONFIG_NODE_BINARY_OP: break; */
		/* case CONFIG_NODE_SUBSCRIPT_RANGE: break; */
		/* case CONFIG_NODE_IDENT: break; */
		/* case CONFIG_NODE_NUMLIT: break; */
		}

		node = node->next_sibling;
	}
}

void config_apply_device_configs(struct stage *stage, struct config_node *node, struct scoped_hash *scope)
{
	while (node) {
		switch (node->type) {
		case CONFIG_NODE_MODULE:
			config_apply_device_configs(stage, node->module.first_child, scope);
			break;

		case CONFIG_NODE_DEVICE: {
			struct device_type *dev_type;
			struct device *dev;

			dev = get_device(stage, node->device.id);
			dev_type = get_device_type(stage, dev->type);

			device_apply_config_node(stage, dev_type, dev, node->device.first_child);
			//device_apply_config_node(stage, node->device.first_child, dev->scope);
			config_apply_device_configs(stage, node->device.first_child, dev->scope);
		} break;

		default:
			break;
		}

		node = node->next_sibling;
	}
}

enum apply_node_type {
	APPLY_NODE_DEVICE_TYPE_PRE,
	APPLY_NODE_DEVICE_TYPE,
	APPLY_NODE_DEVICE_TYPE_BUILTIN,
	APPLY_NODE_DEVICE_TYPE_INPUT,
	APPLY_NODE_DEVICE_TYPE_OUTPUT,
	APPLY_NODE_DEVICE_TYPE_ATTR,
	APPLY_NODE_DEVICE,
	APPLY_NODE_DEVICE_INPUT,
	APPLY_NODE_DEVICE_OUTPUT,
	APPLY_NODE_DEVICE_ATTR,
	APPLY_NODE_DEVICE_BIND,
	APPLY_NODE_DEVICE_ASSIGN,
};

struct apply_node;

struct apply_edge {
	struct apply_node *from, *to;
	bool visited;
};

struct apply_node {
	struct apply_node *next;
	size_t id;
	struct apply_node *owner;
	enum apply_node_type type;
	struct atom *name;
	struct scoped_hash *scope;
	struct config_node *cnode;

	size_t num_incoming_edges;

	struct apply_edge *outgoing_edges;
	size_t num_outgoing_edges;

	int final_id;

	union {
		struct device_type *dev_type;
		struct device_type *dev;
		int channel;
		int attribute;
	} final;

	// For channels
	bool is_default;
};

struct apply_context {
	struct apply_node **nodes;
	size_t num_nodes;

	struct scoped_hash scope;

	// A linked list of all nodes with no incoming edges.
	struct apply_node *terminal_nodes;
	size_t unvisited_edges;
};

static struct apply_node *create_apply_node_with_owner(struct apply_context *ctx,
													   struct scoped_hash *scope,
													   enum apply_node_type type,
													   struct atom *name,
													   struct config_node *cnode,
													   struct apply_node *owner)
{
	enum scope_entry_kind scope_entry_kind;
	struct apply_node *new_node;
	bool create_scope = false;
	size_t id;

	switch (type) {
		case APPLY_NODE_DEVICE_TYPE_PRE:
			scope_entry_kind = SCOPE_ENTRY_NONE;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE_TYPE:
			scope_entry_kind = SCOPE_ENTRY_DEVICE_TYPE;
			create_scope = true;
			assert(!owner);
			break;

		case APPLY_NODE_DEVICE_TYPE_BUILTIN:
			scope_entry_kind = SCOPE_ENTRY_DEVICE_TYPE;
			create_scope = true;
			assert(!owner);
			break;

		case APPLY_NODE_DEVICE_TYPE_INPUT:
			scope_entry_kind = SCOPE_ENTRY_DEVICE_INPUT;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE_TYPE_OUTPUT:
			scope_entry_kind = SCOPE_ENTRY_DEVICE_OUTPUT;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE_TYPE_ATTR:
			scope_entry_kind = SCOPE_ENTRY_DEVICE_ATTRIBUTE;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE_INPUT:
			scope_entry_kind = SCOPE_ENTRY_DEVICE_INPUT;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE_OUTPUT:
			scope_entry_kind = SCOPE_ENTRY_DEVICE_OUTPUT;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE_ATTR:
			scope_entry_kind = SCOPE_ENTRY_DEVICE_ATTRIBUTE;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE:
			scope_entry_kind = SCOPE_ENTRY_DEVICE;
			create_scope = true;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE_BIND:
			scope_entry_kind = SCOPE_ENTRY_NONE;
			assert(owner);
			break;

		case APPLY_NODE_DEVICE_ASSIGN:
			scope_entry_kind = SCOPE_ENTRY_NONE;
			assert(owner);
			break;
	}

	new_node = calloc(1, sizeof(struct apply_node));
	id = dlist_append(ctx->nodes, ctx->num_nodes, &new_node);

	new_node->id    = id;
	new_node->type  = type;
	new_node->name  = name;
	new_node->cnode = cnode;
	new_node->owner = owner;
	new_node->final_id = -1;

	new_node->next = ctx->terminal_nodes;
	ctx->terminal_nodes = new_node;

	if (scope_entry_kind != SCOPE_ENTRY_NONE) {
		if (create_scope) {
			new_node->scope = scoped_hash_push(scope, scope_entry_kind, id);
		}

		scoped_hash_insert(scope,
						   new_node->name,
						   scope_entry_kind,
						   id,
						   cnode,
						   new_node->scope);
	}

	return new_node;
}

static struct apply_node *create_apply_node(struct apply_context *ctx,
											struct scoped_hash *scope,
											enum apply_node_type type,
											struct atom *name,
											struct config_node *cnode)

{
	struct apply_node *result;

	result = create_apply_node_with_owner(ctx, scope, type, name, cnode, NULL);

	return result;
}

static void remove_terminal_apply_node(struct apply_context *ctx,
									   struct apply_node *node)
{
	struct apply_node **n;
	n = &ctx->terminal_nodes;
	while (*n) {
		if (*n == node) {
			*n = node->next;
			node->next = NULL;
			break;
		}
		n = &(*n)->next;
	}
}

// Makes depends_on require node. In other words, node must be
// initialized before depends_on.
static void apply_node_depends(struct apply_context *ctx, struct apply_node *node, struct apply_node *depends_on)
{
	struct apply_edge new_edge = {0};

	new_edge.from = node;
	new_edge.to = depends_on;

	dlist_append(node->outgoing_edges, node->num_outgoing_edges, &new_edge);

	ctx->unvisited_edges += 1;
	depends_on->num_incoming_edges += 1;
	if (depends_on->num_incoming_edges == 1) {
		remove_terminal_apply_node(ctx, depends_on);
	}
}

static void create_dependency_for_l_expr(struct apply_context *ctx,
										 struct scoped_hash *scope,
										 struct apply_node *node,
										 struct config_node *expr)
{
	struct scope_entry entry;
	struct apply_node *attr;
	int err;

	err = config_eval_l_expr(scope, expr, &entry);

	if (err) {
		printf("No such variable '");
		print_l_expr(expr);
		printf("'.\n");
		return;
	}

	attr = ctx->nodes[entry.id];

	if (attr->type != APPLY_NODE_DEVICE_ATTR) {
		printf("'");
		print_l_expr(expr);
		printf("' is not an attribute.\n");
		return;
	}

	// The result depends on the attribute (value).
	apply_node_depends(ctx, attr, node);
}

static void create_dependencies_for_expr(struct apply_context *ctx,
										 struct scoped_hash *scope,
										 struct apply_node *node,
										 struct config_node *expr)
{
	switch (expr->type) {
	case CONFIG_NODE_IDENT:
		create_dependency_for_l_expr(ctx, scope, node, expr);
		break;

	case CONFIG_NODE_BINARY_OP:
		if (expr->binary_op.op == CONFIG_OP_ACCESS ||
			expr->binary_op.op == CONFIG_OP_SUBSCRIPT) {
			// This is a l-expr
			create_dependency_for_l_expr(ctx, scope, node, expr);
		} else {
			create_dependencies_for_expr(ctx, scope, node, expr->binary_op.lhs);
			create_dependencies_for_expr(ctx, scope, node, expr->binary_op.rhs);
		}
		break;

	case CONFIG_NODE_NUMLIT:
		break;

	default:
		print_error("config", "Invalid node in expr!");
		break;
	}
}


static void discover_device_types(struct apply_context *ctx,
								  struct scoped_hash *scope,
								  struct config_node *node)
{
	struct config_node *current = node;

	while (current) {
		switch (current->type) {
		case CONFIG_NODE_MODULE:
			discover_device_types(ctx, scope, current->module.first_child);
			break;

		case CONFIG_NODE_DEVICE_TYPE: {
			struct config_node *stmt;
			struct apply_node *dev_type, *dev_type_pre;
			dev_type = create_apply_node(ctx, scope,
										 APPLY_NODE_DEVICE_TYPE,
										 current->device_type.name,
										 current);
			dev_type_pre = create_apply_node_with_owner(ctx, scope,
														APPLY_NODE_DEVICE_TYPE_PRE,
														current->device_type.name,
														current, dev_type);

			apply_node_depends(ctx, dev_type_pre, dev_type);


			stmt = current->device_type.first_child;

			while (stmt) {
				struct apply_node *new_node;
				switch (stmt->type) {
				case CONFIG_NODE_INPUT:
					new_node = create_apply_node_with_owner(ctx, dev_type->scope,
															APPLY_NODE_DEVICE_TYPE_INPUT,
															stmt->input.name, stmt,
															dev_type);
					new_node->is_default = stmt->input.def;
					apply_node_depends(ctx, dev_type_pre, new_node);
					apply_node_depends(ctx, new_node, dev_type);
					break;

				case CONFIG_NODE_OUTPUT:
					new_node = create_apply_node_with_owner(ctx, dev_type->scope,
															APPLY_NODE_DEVICE_TYPE_OUTPUT,
															stmt->output.name, stmt,
															dev_type);
					new_node->is_default = stmt->output.def;
					apply_node_depends(ctx, dev_type_pre, new_node);
					apply_node_depends(ctx, new_node, dev_type);
					break;

				case CONFIG_NODE_ATTR:
					new_node = create_apply_node_with_owner(ctx, dev_type->scope,
															APPLY_NODE_DEVICE_TYPE_ATTR,
															stmt->attr.name, stmt,
															dev_type);
					apply_node_depends(ctx, dev_type_pre, new_node);
					apply_node_depends(ctx, new_node, dev_type);
					break;

				default:
					break;
				}

				stmt = stmt->next_sibling;
			}
		} break;

		default:
			break;
		}

		current = current->next_sibling;
	}
}

static void discover_devices(struct apply_context *ctx,
							 struct scoped_hash *scope,
							 struct config_node *node)
{
	struct config_node *current = node;

	while (current) {
		switch (current->type) {
		case CONFIG_NODE_MODULE:
			discover_devices(ctx, scope, current->module.first_child);
			break;

		case CONFIG_NODE_DEVICE_TYPE:
			break;

		case CONFIG_NODE_DEVICE: {
			struct scope_entry dev_type_entry;
			struct apply_node *dev_type;
			struct apply_node *dev;
			int err;

			err = config_eval_l_expr(scope,
									 current->device.type,
									 &dev_type_entry);

			if (err) {
				printf("No such device type '");
				print_l_expr(current->device.type);
				printf("'.\n");
				break;
			}

			dev_type = ctx->nodes[dev_type_entry.id];

			if (dev_type->type != APPLY_NODE_DEVICE_TYPE &&
				dev_type->type != APPLY_NODE_DEVICE_TYPE_BUILTIN) {
				printf("'");
				print_l_expr(current->device.type);
				printf("' is not a device type (for the device %.*s).\n",
					   ALIT(current->device.name));
				break;
			}

			dev = create_apply_node_with_owner(ctx, scope,
											   APPLY_NODE_DEVICE,
											   current->device.name,
											   current, dev_type);

			apply_node_depends(ctx, dev_type, dev);

			struct scoped_hash *scope;

			scope = dev_type->scope;
			for (size_t i = 0; i < scope->num_entries; i++) {
				struct scope_entry *entry;
				struct apply_node *cnl_node;
				struct apply_node *new_node;

				entry = &scope->entries[i];
				cnl_node = ctx->nodes[entry->id];

				switch (entry->kind) {
				case SCOPE_ENTRY_DEVICE_INPUT:
					assert(cnl_node->type == APPLY_NODE_DEVICE_TYPE_INPUT);

					new_node = create_apply_node_with_owner(ctx, dev->scope,
															APPLY_NODE_DEVICE_INPUT,
															cnl_node->name,
															cnl_node->cnode,
															dev);
					new_node->is_default = cnl_node->is_default;
					apply_node_depends(ctx, cnl_node, new_node);
					apply_node_depends(ctx, dev, new_node);
					break;

				case SCOPE_ENTRY_DEVICE_OUTPUT:
					assert(cnl_node->type == APPLY_NODE_DEVICE_TYPE_OUTPUT);

					new_node = create_apply_node_with_owner(ctx, dev->scope,
															APPLY_NODE_DEVICE_OUTPUT,
															cnl_node->name,
															cnl_node->cnode,
															dev);
					new_node->is_default = cnl_node->is_default;
					apply_node_depends(ctx, cnl_node, new_node);
					apply_node_depends(ctx, dev, new_node);
					break;

				case SCOPE_ENTRY_DEVICE_ATTRIBUTE:
					assert(cnl_node->type == APPLY_NODE_DEVICE_TYPE_ATTR);

					new_node = create_apply_node_with_owner(ctx, dev->scope,
															APPLY_NODE_DEVICE_ATTR,
															cnl_node->name,
															cnl_node->cnode,
															dev);
					// NOTE: Because attributes must be known when the
					// device is created, the device depends on the attribute.
					apply_node_depends(ctx, cnl_node, new_node);
					apply_node_depends(ctx, new_node, dev);
					break;

				default:
					break;
				}
			}

			// TODO: Fix scope not staying with original definition of
			// device type.
			if (dev_type->cnode) {
				discover_devices(ctx,
								 dev->scope,
								 dev_type->cnode->device_type.first_child);
			}
		} break;

		default:
			break;
		}

		current = current->next_sibling;
	}
}

static struct apply_node *find_default_channel_for_dev(struct apply_context *ctx,
													   struct apply_node *dev,
													   enum apply_node_type type)
{
	assert(type == APPLY_NODE_DEVICE_INPUT ||
		   type == APPLY_NODE_DEVICE_OUTPUT);

	for (size_t i = 0; i < dev->scope->num_entries; i++) {
		struct apply_node *child;
		child = ctx->nodes[dev->scope->entries[i].id];

		if (child->type == type && child->is_default) {
			return child;
		}
	}

	return NULL;
}

static void discover_entries_device(struct apply_context *ctx,
									struct scoped_hash *scope,
									struct apply_node *dev,
									struct config_node *node)
{
	struct config_node *current = node;

	while (current) {
		switch (current->type) {
		case CONFIG_NODE_BINARY_OP:
			if (current->binary_op.op == CONFIG_OP_ASSIGN) {
				struct apply_node *op;

				op = create_apply_node_with_owner(ctx, scope,
												  APPLY_NODE_DEVICE_ASSIGN,
												  NULL, current, dev);

				create_dependency_for_l_expr(ctx, scope, op, current->binary_op.lhs);
				create_dependencies_for_expr(ctx, scope, op, current->binary_op.rhs);
			} else if (current->binary_op.op == CONFIG_OP_BIND) {
				struct apply_node *op;

				op = create_apply_node_with_owner(ctx, scope,
												  APPLY_NODE_DEVICE_BIND,
												  NULL, current, dev);


				struct scope_entry lhs_entry, rhs_entry;
				struct apply_node *lhs_node, *rhs_node;
				bool has_errors = false;
				int err;

				err = config_eval_l_expr(scope, current->binary_op.lhs, &lhs_entry);
				if (err) {
					printf("No such variable '");
					print_l_expr(current->binary_op.lhs);
					printf("' for %.*s.\n", ALIT(dev->name));
					has_errors = true;
				}

				err = config_eval_l_expr(scope, current->binary_op.rhs, &rhs_entry);
				if (err) {
					printf("No such variable '");
					print_l_expr(current->binary_op.rhs);
					printf("' for %.*s.\n", ALIT(dev->name));
					has_errors = true;
				}

				if (has_errors) {
					break;
				}

				lhs_node = ctx->nodes[lhs_entry.id];
				rhs_node = ctx->nodes[rhs_entry.id];

				if (lhs_node->type == APPLY_NODE_DEVICE) {
					enum apply_node_type channel_type;

					channel_type =
						(lhs_node == dev)
						? APPLY_NODE_DEVICE_OUTPUT
						: APPLY_NODE_DEVICE_INPUT;

					lhs_node = find_default_channel_for_dev(ctx, lhs_node, channel_type);

					if (!lhs_node) {
						printf("'");
						print_l_expr(current->binary_op.lhs);
						printf("' has no default %s.\n",
							   (channel_type == APPLY_NODE_DEVICE_OUTPUT)
							   ? "output"
							   : "input");
						has_errors = true;
					}
				}

				if (rhs_node->type == APPLY_NODE_DEVICE) {
					enum apply_node_type channel_type;

					channel_type =
						(rhs_node == dev)
						? APPLY_NODE_DEVICE_INPUT
						: APPLY_NODE_DEVICE_OUTPUT;

					rhs_node = find_default_channel_for_dev(ctx, rhs_node, channel_type);

					if (!rhs_node) {
						printf("'");
						print_l_expr(current->binary_op.rhs);
						printf("' has no default %s.\n",
							   (channel_type == APPLY_NODE_DEVICE_OUTPUT)
							   ? "output"
							   : "input");
						has_errors = true;
					}
				}

				if (has_errors) {
					break;
				}

				if (!(lhs_node->type == APPLY_NODE_DEVICE_INPUT ||
					  (lhs_node->type == APPLY_NODE_DEVICE_OUTPUT &&
					   lhs_node->owner == dev))) {
					printf("Expected '");
					print_l_expr(current->binary_op.rhs);
					printf("' to be an input.\n");
					has_errors = true;
				}

				if (!(rhs_node->type == APPLY_NODE_DEVICE_OUTPUT ||
					  (rhs_node->type == APPLY_NODE_DEVICE_INPUT &&
					   rhs_node->owner == dev))) {
					printf("Expected '");
					print_l_expr(current->binary_op.rhs);
					printf("' to be an output.\n");
					has_errors = true;
				}

				if (has_errors) {
					break;
				}

				// The bind depends on the operands.
				apply_node_depends(ctx, lhs_node, op);
				apply_node_depends(ctx, rhs_node, op);
			}
			break;

		default:
			break;
		}
		current = current->next_sibling;
	}
}

static void discover_entries(struct apply_context *ctx,
							 struct scoped_hash *scope,
							 struct config_node *node)
{
	for (size_t i = 0; i < ctx->num_nodes; i++) {
		struct apply_node *node;

		node = ctx->nodes[i];

		switch (node->type) {
		/* case APPLY_NODE_DEVICE_TYPE_INPUT: */
		/* 	break; */

		/* case APPLY_NODE_DEVICE_TYPE_OUTPUT: */
		/* 	break; */

		case APPLY_NODE_DEVICE_TYPE_ATTR:
			if (node->cnode && node->cnode->attr.def_value) {
				struct apply_node *dev_type;

				assert(node->owner);

				dev_type = node->owner;

				assert(node->cnode->type == CONFIG_NODE_ATTR);
				create_dependencies_for_expr(ctx, dev_type->scope,
											 node, node->cnode->attr.def_value);
			}
			break;

		case APPLY_NODE_DEVICE: {
			struct apply_node *dev_type;

			dev_type = node->owner;

			// Apply dev_type
			discover_entries_device(ctx, dev_type->scope, node, dev_type->cnode);
			discover_entries_device(ctx, node->scope, node,
									node->cnode->device.first_child);

		} break;

		default:
			break;
		}
	}
}

static void discover_built_ins(struct apply_context *ctx, struct stage *stage)
{
	for (size_t i = 0; i < stage->num_device_types; i++) {
		struct device_type *dev_type = stage->device_types[i];
		struct apply_node *node;

		node = create_apply_node(ctx, &ctx->scope,
								 APPLY_NODE_DEVICE_TYPE_BUILTIN,
								 dev_type->name, NULL);
		node->final.dev_type = dev_type;

		for (size_t cnl = 0; cnl < dev_type->num_inputs; cnl++) {
			struct apply_node *cnl_node;
			cnl_node = create_apply_node_with_owner(ctx, node->scope,
													APPLY_NODE_DEVICE_TYPE_INPUT,
													dev_type->inputs[cnl].name, NULL,
													node);
			if (cnl == dev_type->self_input) {
				cnl_node->is_default = true;
			}
			apply_node_depends(ctx, node, cnl_node);
		}

		for (size_t cnl = 0; cnl < dev_type->num_outputs; cnl++) {
			struct apply_node *cnl_node;
			cnl_node = create_apply_node_with_owner(ctx, node->scope,
													APPLY_NODE_DEVICE_TYPE_OUTPUT,
													dev_type->outputs[cnl].name, NULL,
													node);
			if (cnl == dev_type->self_output) {
				cnl_node->is_default = true;
			}
			apply_node_depends(ctx, node, cnl_node);
		}

		for (size_t attr = 0; attr < dev_type->num_attributes; attr++) {
			struct apply_node *attr_node;
			attr_node = create_apply_node_with_owner(ctx, node->scope,
													 APPLY_NODE_DEVICE_TYPE_ATTR,
													 dev_type->attributes[attr].name, NULL,
													 node);
			apply_node_depends(ctx, node, attr_node);
		}
	}
}

static bool apply_topological_sort(struct apply_context *ctx, struct apply_node **out)
{
	struct apply_node *result = NULL;
	struct apply_node *result_tail = NULL;

	while (ctx->terminal_nodes) {
		struct apply_node *node;
		node = ctx->terminal_nodes;
		ctx->terminal_nodes = node->next;
		node->next = NULL;

		if (!result) {
			result = node;
			result_tail = node;
		} else {
			result_tail->next = node;
			result_tail = node;
		}

		for (size_t i = 0; i < node->num_outgoing_edges; i++) {
			struct apply_edge *edge;

			edge = &node->outgoing_edges[i];
			if (!edge->visited) {
				edge->visited = true;

				assert(edge->to->num_incoming_edges > 0);

				edge->to->num_incoming_edges -= 1;
				ctx->unvisited_edges -= 1;
				if (edge->to->num_incoming_edges == 0) {
					edge->to->next = ctx->terminal_nodes;
					ctx->terminal_nodes = edge->to;
				}
			}
		}
	}

	if (ctx->unvisited_edges != 0) {
		// TODO: Improve error message!
		printf("Detected one or more circular dependencies in the config file.\n");
		return false;
	}

	*out = result;
	return true;
}

static void print_apply_node_name(struct apply_node *node)
{
	switch (node->type) {
	case APPLY_NODE_DEVICE_TYPE_PRE:
		printf("dev_t_pre");
		break;
	case APPLY_NODE_DEVICE_TYPE:
		printf("dev_t    ");
		break;
	case APPLY_NODE_DEVICE_TYPE_BUILTIN:
		printf("dev_t_bi ");
		break;
	case APPLY_NODE_DEVICE_TYPE_INPUT:
		printf("dev_t_in ");
		break;
	case APPLY_NODE_DEVICE_TYPE_OUTPUT:
		printf("dev_t_out");
		break;
	case APPLY_NODE_DEVICE_TYPE_ATTR:
		printf("dev_t_atr");
		break;
	case APPLY_NODE_DEVICE:
		printf("dev      ");
		break;
	case APPLY_NODE_DEVICE_INPUT:
		printf("dev_in   ");
		break;
	case APPLY_NODE_DEVICE_OUTPUT:
		printf("dev_out  ");
		break;
	case APPLY_NODE_DEVICE_ATTR:
		printf("dev_atr  ");
		break;
	case APPLY_NODE_DEVICE_BIND:
		printf("dev_bnd  ");
		break;
	case APPLY_NODE_DEVICE_ASSIGN:
		printf("dev_asn  ");
		break;
	}
	printf(" %.*s", ALIT(node->name));
	if (node->owner) {
		printf(" (");
		print_apply_node_name(node->owner);
		printf(")");
	}
}

static struct scoped_hash *get_equivalent_scope(struct apply_context *ctx,
												struct scoped_hash *target,
												struct scoped_hash *root)
{
	assert(root->parent == NULL);
	assert(target != NULL);

	struct scoped_hash *eq_parent;
	struct scope_entry entry;
	struct apply_node *node;
	int err;

	if (target->parent != NULL) {
		eq_parent = get_equivalent_scope(ctx, target->parent, root);
	} else {
		eq_parent = root;
	}

	node = ctx->nodes[eq_parent->id];

	assert(node->name);

	err = scoped_hash_local_lookup(eq_parent, node->name, &entry);
	assert(!err);

	return entry.scope;
}

static scalar_value apply_eval_value(struct apply_context *ctx,
									 struct stage *stage,
									 struct apply_node *expr)
{
	// TODO: Implement
	return 0;
}

static bool do_apply_config(struct apply_context *ctx,
							struct stage *stage,
							struct apply_node *action_list)
{
	while (action_list) {
		struct apply_node *node;
		node = action_list;
		action_list = node->next;
		node->next = NULL;

		switch (node->type) {
		case APPLY_NODE_DEVICE_TYPE_BUILTIN:
			break;

		case APPLY_NODE_DEVICE_TYPE_PRE: {
			struct device_type *dev_type;
			struct scoped_hash *scope;

			scope = get_equivalent_scope(ctx, node->owner->scope->parent, &stage->root_scope);
			dev_type = register_device_type_scoped(stage, node->owner->name->name, scope);

			node->owner->final.dev_type = dev_type;
		} break;

		case APPLY_NODE_DEVICE_TYPE:
			assert(node->final.dev_type);
			break;

		case APPLY_NODE_DEVICE_TYPE_INPUT: {
			struct device_channel_def *cnl;

			if (node->owner->type == APPLY_NODE_DEVICE_TYPE_BUILTIN) {
				continue;
			}

			cnl = device_type_add_input(stage, node->owner->final.dev_type,
										node->name->name, stage->standard_types.integer);

			node->final.channel = cnl->id;
		} break;

		case APPLY_NODE_DEVICE_TYPE_OUTPUT: {
			struct device_channel_def *cnl;

			if (node->owner->type == APPLY_NODE_DEVICE_TYPE_BUILTIN) {
				continue;
			}

			cnl = device_type_add_output(stage, node->owner->final.dev_type,
										node->name->name, stage->standard_types.integer);

			node->final.channel = cnl->id;
		} break;

		case APPLY_NODE_DEVICE_TYPE_ATTR: {
			struct device_attribute_def *attr;
			scalar_value default_value;

			if (node->owner->type == APPLY_NODE_DEVICE_TYPE_BUILTIN) {
				continue;
			}

			// TODO: Figure out what node should be.
			// How should we store what expressions eval to?
			default_value = apply_eval_value(ctx, stage, node);

			attr = device_type_add_attribute(stage, node->owner->final.dev_type,
											 node->name->name, stage->standard_types.integer);

			node->final.attribute = attr->id;
		} break;


		default:
			//assert(false);
			break;
		}
	}

	return true;
}

int apply_config(struct stage *stage, struct config_node *node)
{
	struct apply_context ctx = {0};

	ctx.scope.parent = 0;
	ctx.scope.lookup.page_arena = &stage->memory;
	ctx.scope.lookup.string_arena = &stage->memory;

	discover_built_ins(&ctx, stage);
	discover_device_types(&ctx, &ctx.scope, node);
	discover_devices(&ctx, &ctx.scope, node);
	discover_entries(&ctx, &ctx.scope, node);

	printf("Nodes:\n");
	for (size_t i = 0; i < ctx.num_nodes; i++) {
		struct apply_node *node;

		node = ctx.nodes[i];

		print_apply_node_name(node);
		if (node->is_default) {
			printf(" (default)");
		}
		printf("\n");

		for (size_t i = 0; i < node->num_outgoing_edges; i++) {
			struct apply_node *dep;

			dep = node->outgoing_edges[i].to;
			printf("  ");
			print_apply_node_name(dep);
			printf("\n");
		}
	}

	printf("\nSorted nodes:\n");

	struct apply_node *tn;

	if (!apply_topological_sort(&ctx, &tn)) {
		return -1;
	}

	do_apply_config(&ctx, stage, tn);

	/* while (tn) { */
	/* 	print_apply_node_name(tn); */
	/* 	printf("\n"); */
	/* 	tn = tn->next; */
	/* } */

	/* config_apply_device_types(stage, node, &stage->root_scope); */
	/* config_apply_devices(stage, node, &stage->root_scope); */
	/* config_apply_device_configs(stage, node, &stage->root_scope); */
	return 0;
}
