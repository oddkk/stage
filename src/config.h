#ifndef STAGE_CONFIG_H
#define STAGE_CONFIG_H

#include "type.h"
#include "atom.h"

enum cfg_bin_op {
	CFG_OP_ADD,
	CFG_OP_SUB,
	CFG_OP_MUL,
	CFG_OP_DIV,
	CFG_OP_RANGE,
	CFG_OP_EQ,
	CFG_OP_NEQ,
	CFG_OP_GTE,
	CFG_OP_LTE,
	CFG_OP_GT,
	CFG_OP_LT,
};

#define CFG_NODES \
	CFG_NODE(INTERNAL_LIST, struct {			\
			struct cfg_node *head;				\
			struct cfg_node *tail;				\
	})											\
	CFG_NODE(MODULE, struct {					\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(STMT, struct {						\
			struct cfg_node *attrs;				\
			struct cfg_node *stmt;				\
	})											\
	CFG_NODE(DECL_STMT, struct {				\
			struct cfg_node *name;				\
			struct cfg_node *args;				\
			struct cfg_node *decl;				\
	})											\
	CFG_NODE(OBJ_DECL, struct {					\
			struct cfg_node *ident;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(ENUM_DECL, struct {				\
			struct cfg_node *items;				\
	})											\
	CFG_NODE(ENUM_ITEM, struct {				\
			struct atom *name;					\
			struct cfg_node *data;				\
	})											\
	CFG_NODE(USE_STMT, struct {					\
			struct cfg_node *ident;				\
	})											\
	CFG_NODE(USE_ALL, struct {					\
			int _dc;							\
	})											\
	CFG_NODE(FUNC_STMT, struct {				\
			struct cfg_node *ident;				\
			struct cfg_node *proto;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(FUNC_PROTO, struct {				\
			struct cfg_node *params;			\
			struct cfg_node *ret;				\
	})											\
	CFG_NODE(ASSIGN_STMT, struct {				\
			struct cfg_node *ident;				\
			struct cfg_node *type;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(BIND, struct {						\
			struct cfg_node *src;				\
			struct cfg_node *drain;				\
	})											\
	CFG_NODE(NAMESPACE, struct {				\
			struct cfg_node *name;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(TEMPLATE_VAR, struct {				\
			struct atom *name;					\
	})											\
	CFG_NODE(ACCESS, struct {					\
			struct cfg_node *lhs;				\
			struct cfg_node *rhs;				\
	})											\
	CFG_NODE(SUBSCRIPT, struct {				\
			struct cfg_node *lhs;				\
			struct cfg_node *index;				\
	})											\
	CFG_NODE(BIN_OP, struct {					\
			enum cfg_bin_op op;					\
			struct cfg_node *lhs;				\
			struct cfg_node *rhs;				\
	})											\
	CFG_NODE(LAMBDA, struct {					\
			struct cfg_node *proto;				\
			struct cfg_node *body;				\
	})											\
	CFG_NODE(FUNC_CALL, struct {				\
			struct cfg_node *ident;				\
			struct cfg_node *params;			\
	})											\
	CFG_NODE(TUPLE_DECL, struct {				\
			struct cfg_node *items;				\
			bool named;							\
	})											\
	CFG_NODE(TUPLE_DECL_ITEM, struct {			\
			struct atom *name;					\
			struct cfg_node *type;				\
	})											\
	CFG_NODE(TUPLE_LIT, struct {				\
			struct cfg_node *items;				\
			bool named;							\
	})											\
	CFG_NODE(TUPLE_LIT_ITEM, struct {			\
			struct atom *name;					\
			struct cfg_node *value;				\
	})											\
	CFG_NODE(ARRAY_LIT, struct {				\
			struct cfg_node *items;				\
	})											\
	CFG_NODE(NUM_LIT, scalar_value)				\
	CFG_NODE(IDENT, struct atom *)				\

enum cfg_node_type {
#define CFG_NODE(name, data) CFG_NODE_##name,
	CFG_NODES
#undef CFG_NODE
	CFG_NODES_LEN
};

struct cfg_location {
	size_t line;
	size_t column;
};

#define CFG_NODE(name, data) typedef data name##_t;
CFG_NODES
#undef CFG_NODE

struct cfg_node {
	enum cfg_node_type type;

	struct cfg_location from;
	struct cfg_location to;

	struct cfg_node *next_sibling;

#define CFG_NODE(name, data) name##_t name;
	union {
		CFG_NODES
	};
#undef CFG_NODE
};

extern struct string cfg_node_names[CFG_NODES_LEN];

/* #define CFG_JOBS \ */
/* 	CFG_JOB(DISCOVER) \ */









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
	CONFIG_NODE_TYPE_TEMPLATE_PARAM,
	CONFIG_NODE_TUPLE,
	CONFIG_NODE_TUPLE_ITEM,
	CONFIG_NODE_ARRAY_TYPE,
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
	CONFIG_NODE_TUPLE_LIT,
	CONFIG_NODE_TUPLE_LIT_ITEM,
	CONFIG_NODE_ARRAY_LIT,

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
			struct config_node *params;
			int id;
		} device_type;
		struct {
			struct config_node *type;
			struct atom *name;
			struct config_node *first_child;
			struct config_node *args;
			int id;
		} device;
		struct {
			struct atom *name;
			struct config_node *type;
		} type_decl;
		struct {
			struct config_node *expr;
		} type_template_param;
		struct {
			struct config_node *lhs;
			struct config_node *length;
			bool template_length;
		} array_type;
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
			struct config_node *first_child;
			bool named;
		} tuple_lit;
		struct {
			struct atom *name;
			struct config_node *expr;
		} tuple_lit_item;
		struct {
			struct config_node *first_child;
		} array_lit;
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

void config_tree_print(struct config_node *node);
void cfg_tree_print(struct cfg_node *node);
#endif
