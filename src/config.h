#ifndef STAGE_CONFIG_H
#define STAGE_CONFIG_H

#include "type.h"
#include "atom.h"

enum config_binary_op {
	CONFIG_OP_ASSIGN,
	CONFIG_OP_BIND,
	CONFIG_OP_ADD,
	CONFIG_OP_SUB,
	CONFIG_OP_MUL,
	CONFIG_OP_DIV,
	CONFIG_OP_ACCESS,
	CONFIG_OP_SUBSCRIPT,
};

enum config_node_type {
	CONFIG_NODE_MODULE,
	CONFIG_NODE_NAMESPACE,
	CONFIG_NODE_DEVICE_TYPE,
	CONFIG_NODE_DEVICE,
	CONFIG_NODE_TYPE_DECL,
	CONFIG_NODE_TYPE,
	CONFIG_NODE_TUPLE,
	CONFIG_NODE_TUPLE_ITEM,
	CONFIG_NODE_ATTR,
	CONFIG_NODE_INPUT,
	CONFIG_NODE_OUTPUT,
	/* CONFIG_NODE_ASSIGN, */
	/* CONFIG_NODE_BIND, */
	CONFIG_NODE_BINARY_OP,
	CONFIG_NODE_SUBSCRIPT_RANGE,
	CONFIG_NODE_SUBRANGE,
	CONFIG_NODE_IDENT,
	CONFIG_NODE_NUMLIT,

	CONFIG_NODE_INTERNAL_LIST,
};

struct version {
	int major;
	int minor;
	int patch;
};

struct scoped_hash;

struct config_location {
	size_t line;
	size_t column;
};

struct config_node {
	enum config_node_type type;
	struct config_node *next_sibling;

	struct config_location from;
	struct config_location to;

	union {
		struct {
			struct string filename;
			struct config_node *first_child;
			struct version version;
		} module;
		struct {
			struct atom *name;
			struct config_node *first_child;
		} namespace;
		struct {
			struct atom *name;
			struct config_node *first_child;
			int id;
		} device_type;
		struct {
			struct config_node *type;
			struct atom *name;
			struct config_node *first_child;
			int id;
		} device;
		struct {
			struct atom *name;
			struct config_node *type;
		} type_decl;
		struct {
			struct config_node *first_child;
		} type_def;
		struct {
			bool named;
			struct config_node *first_child;
		} tuple;
		struct {
			struct atom *name;
			struct config_node *type;
		} tuple_item;
		struct {
			struct atom *name;
			struct config_node *type;
			struct config_node *def_value;
		} attr;
		struct {
			struct atom *name;
			struct config_node *type;
			bool def;
		} input;
		struct {
			struct atom *name;
			struct config_node *type;
			bool def;
		} output;
		struct {
			enum config_binary_op op;
			struct config_node *lhs;
			struct config_node *rhs;
		} binary_op;
		struct {
			struct config_node *lhs;
			struct config_node *low;
			struct config_node *high;
		} subscript_range;
		struct {
			struct config_node *lhs;
			struct config_node *low;
			struct config_node *high;
		} subrange;
		struct {
			struct config_node *head;
			struct config_node *tail;
		} internal_list;
		struct atom *ident;
		scalar_value numlit;
	};
};

int parse_config_file(struct string filename, struct atom_table *table,
		      struct arena *memory, struct config_node **out_node);

int apply_config(struct stage *, struct config_node *);

void config_print_tree(struct config_node *node);
#endif
