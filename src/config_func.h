#ifndef STAGE_CONFIG_FUNC_H
#define STAGE_CONFIG_FUNC_H

#include "vm.h"

enum cfg_func_node_type {
	CFG_FUNC_NODE_FUNC_CALL,
	CFG_FUNC_NODE_LOOKUP_GLOBAL,
	CFG_FUNC_NODE_LOOKUP_LOCAL,
	CFG_FUNC_NODE_GLOBAL,
	CFG_FUNC_NODE_STACK,
	CFG_FUNC_NODE_SCOPE,
	CFG_FUNC_NODE_LIT_INT,
	CFG_FUNC_NODE_LIT_STR,
};

struct cfg_func_context;

struct cfg_func_node {
	enum cfg_func_node_type type;
	struct cfg_func_node *next_arg;

	type_id return_type;

	union {
		struct {
			struct atom *name;
			struct cfg_func_node *scope;
		} lookup;

		struct {
			struct cfg_func_node *func;
			struct cfg_func_node *args;
		} func_call;

		struct object obj;

		struct scope *scope;

		int64_t lit_int;
		struct string lit_str;
	};
};

struct cfg_func_context {
	struct scope *outer_scope;
	struct scope *inner_scope;
};

struct cfg_func {
	struct cfg_func_context *ctx;
	struct cfg_func_node *body;
};

struct cfg_func_node *
cfg_func_call(struct vm *, struct cfg_func_node *func);

void
cfg_func_call_add_arg(struct cfg_func_node *func,
					  struct cfg_func_node *arg);

enum cfg_func_lookup_mode {
	CFG_FUNC_LOOKUP_GLOBAL,
	CFG_FUNC_LOOKUP_LOCAL,
};

struct cfg_func_node *
cfg_func_lookup(struct vm *vm, struct atom *name,
				struct cfg_func_node *scope,
				enum cfg_func_lookup_mode lookup_mode);

struct cfg_func_node *
cfg_func_scope(struct vm *, struct scope *);

struct cfg_func_node *
cfg_func_global(struct vm *, struct object);

struct cfg_func_node *
cfg_func_lit_int(struct vm *, int64_t);

struct cfg_func_node *
cfg_func_lit_str(struct vm *, struct string);

int
cfg_func_eval_simple(struct vm *vm, struct cfg_func_node *,
					 struct object *out);

int
cfg_func_eval(struct vm *vm, struct exec_stack *stack,
			  struct cfg_func_node *, struct object *out);

void
cfg_func_simplify(struct vm *vm, struct cfg_func_node *node);

void
cfg_func_print(struct vm *vm, struct cfg_func_node *node);

void
cfg_func_destroy(struct cfg_func_node *node);
void
cfg_func_free(struct cfg_func_node *node);

#endif
