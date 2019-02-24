#ifndef STAGE_CONFIG_FUNC_H
#define STAGE_CONFIG_FUNC_H

#include "vm.h"
#include "module.h"

enum expr_node_type {
	EXPR_NODE_FUNC_DECL,     // [ABS]
	EXPR_NODE_FUNC_CALL,     // [APP]
	EXPR_NODE_LOOKUP_FUNC,   // [VAR]
	EXPR_NODE_LOOKUP_GLOBAL, // [VAR]
	EXPR_NODE_LOOKUP_LOCAL,  // [VAR]
	EXPR_NODE_GLOBAL,        // [VAR]
	EXPR_NODE_STACK,         // [VAR]
	EXPR_NODE_SCOPE,         // [VAR]
	EXPR_NODE_LIT_INT,       // [VAR]
	EXPR_NODE_LIT_STR,       // [VAR]
	EXPR_NODE_TYPE_EXPR,
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

		func_type_id type;
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

struct expr_func_scope {
	struct atom **entry_names;
	func_type_id *entry_types;
	size_t num_entries;

	struct scope *outer_scope;
};

enum expr_typecheck_state {
	EXPR_TYPECHECK_IDLE = 0,
	EXPR_TYPECHECK_INFER_TYPES,
	EXPR_TYPECHECK_DONE,
	EXPR_TYPECHECK_ERROR,
};

struct expr {
	struct expr_node *body;
	struct scope *outer_scope;

	size_t num_type_slots;
	struct expr_type_slot *slots;
	size_t num_type_errors;

	enum expr_typecheck_state state;
	size_t num_infer;

	// TODO: It seems wrong that an expr should have a reference to
	// its module, as it is called with either mod or vm for typecheck
	// and eval. We have it here because we need to create new types
	// for function overloads type unification. The function
	// overloads, and all lookups, should be moved to the typecheck
	// stage.
	struct stg_module *mod;
};

struct expr_node {
	enum expr_node_type type;
	enum expr_node_flags flags;
	struct expr_node *next_arg;

	struct expr_type_rule rule;

	union {
		struct {
			struct atom *name;

			union {
				struct expr_node *scope;
				struct expr_func_scope *func_scope;
			};
		} lookup;

		struct {
			struct expr_node *func;
			struct expr_node *args;
		} func_call;

		struct {
			struct expr_func_decl_param *params;
			size_t num_params;
			struct expr_node *ret_type;

			struct expr expr;
			struct expr_func_scope scope;
		} func_decl;

		struct expr_node *type_expr;

		struct object obj;

		struct scope *scope;

		int64_t lit_int;
		struct string lit_str;
	};
};

enum expr_type_slot_state {
	SLOT_UNBOUND   = 0,
	SLOT_BOUND     = 1,
	SLOT_BOUND_REF = 2,

	SLOT_TEMPLATE = 8,
};

#define SLOT_STATE(slot) ((uint8_t)(slot) & (~(uint8_t)SLOT_TEMPLATE))
#define SLOT_IS_TEMPLATE(slot) (!!((uint8_t)(slot) & ((uint8_t)SLOT_TEMPLATE)))

struct expr_type_slot {
	enum expr_type_slot_state state;
	union {
		type_id type;
		func_type_id ref;
	};
};

struct expr_node *
expr_func_decl(struct stg_module *, struct expr *,
			   struct expr_func_decl_param *params,
			   size_t num_params,
			   struct expr_node *ret,
			   struct expr_node *body);

// Initializes node as a func decl.
struct expr_node *
expr_init_func_decl(struct stg_module *, struct expr *expr,
					struct expr_node *node,
					struct expr_func_decl_param *params,
					size_t num_params,
					struct expr_node *ret,
					struct expr_node *body);

struct expr_node *
expr_call(struct stg_module *, struct expr *,
		  struct expr_node *func,
		  struct expr_node *first_arg);

enum expr_lookup_mode {
	EXPR_LOOKUP_GLOBAL,
	EXPR_LOOKUP_LOCAL,
};

struct expr_node *
expr_lookup_func_scope(struct stg_module *, struct expr *,
					   struct atom *name,
					   struct expr_func_scope *scope);

struct expr_node *
expr_lookup(struct stg_module *, struct expr *,
			struct atom *name,
			struct expr_node *scope,
			enum expr_lookup_mode lookup_mode);

struct expr_node *
expr_scope(struct stg_module *, struct expr *, struct scope *);

struct expr_node *
expr_global(struct stg_module *, struct expr *, struct object);

struct expr_node *
expr_lit_int(struct stg_module *, struct expr *, int64_t);

struct expr_node *
expr_lit_str(struct stg_module *, struct expr *, struct string);



void
expr_finalize(struct stg_module *, struct expr *);

int
expr_bind_type(struct stg_module *, struct expr *,
			   func_type_id, type_id);

void
expr_slot_mark_template(struct stg_module *, struct expr *,
						func_type_id);

int
expr_bind_ref_slot(struct stg_module *, struct expr *,
				   func_type_id, func_type_id other);


func_type_id
expr_get_actual_slot(struct expr *expr, func_type_id slot);


type_id
expr_get_slot_type(struct expr *expr, func_type_id slot);

type_id
expr_slot_is_template(struct expr *expr, func_type_id slot);

// Returns 0 if done, -1 if error and 1 if yield.
int
expr_typecheck(struct stg_module *, struct expr *);

int
expr_eval_simple(struct vm *, struct stg_module *mod, struct expr *,
				 struct expr_node *, struct object *out);

enum expr_eval_error {
	EXPR_EVAL_OK = 0,
	EXPR_EVAL_ERROR = -1,
	EXPR_EVAL_YIELD = 1,
};

int
expr_eval(struct vm *, struct expr *,
		  struct exec_stack *, struct expr_node *,
		  struct object *out);

void
expr_simplify(struct stg_module *, struct expr *,
			  struct expr_node *node);

void
expr_print(struct vm *, struct expr *);

void
expr_destroy(struct expr_node *node);
void
expr_free(struct expr_node *node);

#endif
