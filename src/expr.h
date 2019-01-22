#ifndef STAGE_CONFIG_FUNC_H
#define STAGE_CONFIG_FUNC_H

#include "vm.h"

enum expr_node_type {
	EXPR_NODE_FUNC_DECL,     // [ABS]
	EXPR_NODE_FUNC_CALL,     // [APP]
	EXPR_NODE_LOOKUP_GLOBAL, // [VAR]
	EXPR_NODE_LOOKUP_LOCAL,  // [VAR]
	EXPR_NODE_UNKNOWN,       // [VAR]
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
			func_type_id *args;
			func_type_id num_args;
		} app;

		struct {
			func_type_id *params;
			func_type_id num_params;
			func_type_id ret;
		} abs;
	};

	func_type_id out;
};

struct expr_node;

struct expr_func_decl_param {
	struct atom *name;
	struct expr_node *type;
};

enum expr_node_flags {
	// Set when an expression has been completly typed.
	EXPR_TYPED = 0x1,
	EXPR_CONST = 0x2,
};

struct expr_node {
	enum expr_node_type type;
	enum expr_node_flags flags;
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

		struct {
			struct expr_func_decl_param *params;
			size_t num_params;
			struct expr_node *ret_type;
			struct expr_node *body;
		} func_decl;

		struct object obj;

		struct scope *scope;

		int64_t lit_int;
		struct string lit_str;
	};
};

struct expr {
	struct expr_node *body;
	struct scope *outer_scope;

	size_t num_type_slots;
	type_id *slots;
	size_t num_type_errors;
};

struct expr_node *
expr_func_decl(struct vm *, struct expr *,
			   struct expr_func_decl_param *params,
			   size_t num_params,
			   struct expr_node *ret,
			   struct expr_node *body);

struct expr_node *
expr_call(struct vm *, struct expr *,
		  struct expr_node *func,
		  struct expr_node *first_arg);

enum expr_lookup_mode {
	EXPR_LOOKUP_GLOBAL,
	EXPR_LOOKUP_LOCAL,
};

struct expr_node *
expr_lookup(struct vm *, struct expr *,
			struct atom *name,
			struct expr_node *scope,
			enum expr_lookup_mode lookup_mode);

struct expr_node *
expr_unknown(struct vm *, struct expr *);

struct expr_node *
expr_scope(struct vm *, struct expr *, struct scope *);

struct expr_node *
expr_global(struct vm *, struct expr *, struct object);

struct expr_node *
expr_lit_int(struct vm *, struct expr *, int64_t);

struct expr_node *
expr_lit_str(struct vm *, struct expr *, struct string);



void
expr_finalize(struct vm *, struct expr *);

int
expr_bind_type(struct vm *, struct expr *,
			   func_type_id, type_id);

int
expr_typecheck(struct vm *, struct expr *);

int
expr_eval_simple(struct vm *vm, struct expr_node *,
				 struct object *out);

int
expr_eval(struct vm *vm, struct exec_stack *stack,
		  struct expr_node *, struct object *out);

void
expr_simplify(struct vm *vm, struct expr_node *node);

void
expr_print(struct vm *vm, struct expr *);

void
expr_destroy(struct expr_node *node);
void
expr_free(struct expr_node *node);

#endif
