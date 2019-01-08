#ifndef STAGE_CONFIG_FUNC_H
#define STAGE_CONFIG_FUNC_H

#include "vm.h"

enum cfg_func_node_type {
	CFG_FUNC_NODE_FUNC_CALL,
	CFG_FUNC_NODE_GLOBAL,
	CFG_FUNC_NODE_LIT_INT,
	CFG_FUNC_NODE_LIT_STR,
};

struct cfg_func_context;

struct cfg_func_node {
	enum cfg_func_node_type type;
	struct cfg_func_node *next_arg;

	type_id arg_type;
	type_id return_type;

	union {
		struct {
			struct atom *name;
			struct cfg_func_context *ctx;
			struct cfg_func_node *args;
		} func_call;

		obj_id global;

		int64_t lit_int;
		struct string lit_str;
	};
};

struct cfg_func_context {
	struct scope *outer_scope;
	struct scope *inner_scope;
};

struct cfg_func_node *
cfg_func_call(struct vm *, struct cfg_func_context *,
			  struct atom *);

void
cfg_func_call_add_arg(struct cfg_func_node *func,
					  struct cfg_func_node *arg);

struct cfg_func_node *
cfg_func_lit_int(struct vm *, int64_t);

struct cfg_func_node *
cfg_func_lit_str(struct vm *, struct string);

void
cfg_func_print(struct cfg_func_node *node);

#endif
