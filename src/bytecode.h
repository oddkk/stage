#ifndef STAGE_BYTECODE_H
#define STAGE_BYTECODE_H

#include "objstore.h"
#include <limits.h>

typedef int bc_var;
typedef unsigned int bc_const;
typedef unsigned int bc_closure;

#define BC_VAR_NEW ((bc_var)INT_MIN)

enum bc_op {
	// Do nothing.
	BC_NOP,

	// Load global into var.
	BC_LOAD,

	// Copy a var to another.
	BC_COPY,

	// Copy closure into var.
	BC_COPY_CLOSURE,

	// Append a var to the list of args for the next CALL.
	BC_PUSH_ARG,

	// Call literal function.
	BC_LCALL,

	// Call literal function with the closure as its first argument.
	BC_CLCALL,

	// Call var function.
	BC_VCALL,

	// Writes True to target if the two arguments at the argument stack are
	// equal, or False otherwise.
	BC_TESTEQ,

	// Writes the negated value of the boolean at the top of the stack to target.
	BC_LNOT,

	// Moves the instruction pointer.
	BC_JMP,

	// Moves the instruction pointer if the argument on the stack is True.
	BC_JMPIF,

	// Call the pack func on the given arguments.
	BC_PACK,

	// Unpack a member from the given member.
	BC_UNPACK,

	// Writes true to target if the given constructor can unpack the argument
	// at the top of the stack, and false otherwise.
	BC_TEST_UNPACK,

	// Passes a var and returns execution to the caller.
	BC_RET,
};

struct bc_instr {
	enum bc_op op;
	union {
		struct {
			bc_const obj;
			bc_var target;
		} load;

		struct {
			bc_var src;
			bc_var target;
		} copy;

		struct {
			bc_closure closure;
			bc_var target;
		} copy_closure;

		struct {
			bc_var var;
		} push_arg;

		struct {
			func_id func;
			bc_var target;
		} lcall;

		struct {
			func_id func;
			void *closure;
			bc_var target;
		} clcall;

		struct {
			bc_var func;
			bc_var target;
		} vcall;

		struct {
			bc_var target;
			bc_var lhs, rhs;
		} testeq;

		struct {
			bc_var target;
		} lnot;

		struct {
			// TODO: Reduce the size of this instruction.
			object_pack_func func;
			void *data;
			bc_var target;
		} pack;

		struct {
			object_unpack_func func;
			void *data;
			int param_id;
			bc_var target;
		} unpack;

		struct {
			object_can_unpack_func func;
			void *data;
			bc_var target;
		} test_unpack;

		struct {
			bc_var var;
		} ret;
	};

	ssize_t label;
	struct bc_instr *next, *jmp;
};

struct bc_instr_store {
	size_t page_size;
	size_t instr_per_page;

	struct bc_instr **pages;
	size_t last_page_num_used;
	size_t num_pages;

	// TODO: Freelist?
};

struct vm;
struct nbc_func;

struct bc_env {
	struct vm *vm;
	struct bc_instr_store *store;

	struct bc_instr *entry_point;
	struct object *consts;
	size_t num_consts;

	type_id *var_types;
	size_t num_vars;

	type_id *param_types;
	size_t num_params;

	type_id *closure_types;
	size_t num_closures;

	bool labels_tagged;
	size_t num_labels;

	struct nbc_func *nbc;
};

bc_var
bc_alloc_var(struct bc_env *, type_id);

type_id
bc_get_var_type(struct bc_env *, bc_var);

type_id
bc_get_closure_type(struct bc_env *, bc_closure);

bc_var
bc_alloc_param(struct bc_env *, unsigned int param_id, type_id);

struct bc_instr *
bc_gen_nop(struct bc_env *);

struct bc_instr *
bc_gen_load(struct bc_env *, bc_var target, struct object obj);

struct bc_instr *
bc_gen_copy(struct bc_env *, bc_var target, bc_var src);

struct bc_instr *
bc_gen_copy_closure(struct bc_env *, bc_var target, bc_closure closure);

struct bc_instr *
bc_gen_push_arg(struct bc_env *, bc_var var);

struct bc_instr *
bc_gen_lcall(struct bc_env *, bc_var target, func_id);

struct bc_instr *
bc_gen_clcall(struct bc_env *, bc_var target, func_id, void *closure);

struct bc_instr *
bc_gen_vcall(struct bc_env *, bc_var target, bc_var func);

struct bc_instr *
bc_gen_testeq(struct bc_env *, bc_var target, bc_var lhs, bc_var rhs);

struct bc_instr *
bc_gen_lnot(struct bc_env *, bc_var target);

struct bc_instr *
bc_gen_jmp(struct bc_env *, struct bc_instr *dest);

struct bc_instr *
bc_gen_jmpif(struct bc_env *, struct bc_instr *dest);

struct bc_instr *
bc_gen_pack(struct bc_env *, bc_var target,
		object_pack_func, void *data, type_id ret_type);

struct bc_instr *
bc_gen_unpack(struct bc_env *, bc_var target,
		object_unpack_func, void *data, int param_id, type_id ret_type);

struct bc_instr *
bc_gen_test_unpack(struct bc_env *, bc_var target,
		object_can_unpack_func, void *data);

struct bc_instr *
bc_gen_ret(struct bc_env *, bc_var var);

void
bc_tag_labels(struct bc_env *env);

void
bc_print(struct bc_env *, struct bc_instr *);

struct bc_result {
	struct bc_instr *first;
	struct bc_instr *last;
	bc_var out_var;
	int err;
};

static inline void
append_bc_instr(struct bc_result *res, struct bc_instr *instr)
{
	if (res->last) {
		assert(res->first);
		res->last->next = instr;
	} else {
		res->first = instr;
	}
	res->last = instr;
}

static inline void
append_bc_instrs(struct bc_result *res, struct bc_result instrs)
{
	if (!instrs.first) {
		return;
	}
	assert(instrs.first && instrs.last);
	append_bc_instr(res, instrs.first);
	res->last = instrs.last;
}

#endif
