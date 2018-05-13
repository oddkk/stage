#include "config.h"
#include "stage.h"
#include "device_type.h"
#include "device.h"
#include "utils.h"
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

int apply_config(struct stage *stage, struct config_node *node)
{
	config_apply_device_types(stage, node, &stage->root_scope);
	config_apply_devices(stage, node, &stage->root_scope);
	config_apply_device_configs(stage, node, &stage->root_scope);
	return 0;
}
