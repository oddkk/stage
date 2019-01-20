#ifndef STAGE_CONFIG_FUNC_H
#define STAGE_CONFIG_FUNC_H

#include "vm.h"

enum expr_node_type {
	/* EXPR_NODE_FUNC_DECL,     // [ABS] */
	EXPR_NODE_FUNC_CALL,     // [APP]
	EXPR_NODE_LOOKUP_GLOBAL, // [VAR]
	EXPR_NODE_LOOKUP_LOCAL,  // [VAR]
	EXPR_NODE_GLOBAL,        // [VAR]
	EXPR_NODE_STACK,         // [VAR]
	EXPR_NODE_SCOPE,         // [VAR]
	EXPR_NODE_LIT_INT,       // [VAR]
	EXPR_NODE_LIT_STR,       // [VAR]
};

struct expr_context;

typedef unsigned int func_type_id;

struct expr_type_rule {
	union {
		struct {
			int _dc;
		} var;

		struct {
			func_type_id func;
			func_type_id params_begin;
			func_type_id num_params;
			func_type_id ret;
		} app;

		struct {
			func_type_id params_begin;
			func_type_id num_params;
			func_type_id ret;
		} abs;
	};

	size_t out;
};

struct expr_node {
	enum expr_node_type type;
	struct expr_node *next_arg;

	struct expr_type_rule rule;

	union {
		struct {
			struct atom *name;
			struct expr_node *scope;
		} lookup;

		struct {
			struct expr_node *func;
			struct expr_node *args;
		} func_call;

		struct object obj;

		struct scope *scope;

		int64_t lit_int;
		struct string lit_str;
	};
};

struct expr_context {
	struct scope *outer_scope;
	struct scope *inner_scope;

	size_t num_types;
};

struct expr {
	struct expr_context *ctx;
	struct expr_node *body;
};

struct expr_node *
expr_call(struct vm *, struct expr_node *func);

void
expr_call_add_arg(struct expr_node *func,
					  struct expr_node *arg);

enum expr_lookup_mode {
	EXPR_LOOKUP_GLOBAL,
	EXPR_LOOKUP_LOCAL,
};

struct expr_node *
expr_lookup(struct vm *vm, struct atom *name,
				struct expr_node *scope,
				enum expr_lookup_mode lookup_mode);

struct expr_node *
expr_scope(struct vm *, struct scope *);

struct expr_node *
expr_global(struct vm *, struct object);

struct expr_node *
expr_lit_int(struct vm *, int64_t);

struct expr_node *
expr_lit_str(struct vm *, struct string);

int
expr_eval_simple(struct vm *vm, struct expr_node *,
					 struct object *out);

int
expr_eval(struct vm *vm, struct exec_stack *stack,
			  struct expr_node *, struct object *out);

void
expr_simplify(struct vm *vm, struct expr_node *node);

void
expr_print(struct vm *vm, struct expr_node *node);

void
expr_destroy(struct expr_node *node);
void
expr_free(struct expr_node *node);

#endif
