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

	// Call the pack func on the given arguments.
	BC_PACK,

	// Unpack a member from the given member.
	BC_UNPACK,

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
			bc_var var;
		} ret;
	};

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
bc_gen_pack(struct bc_env *, bc_var target,
		object_pack_func, void *data, type_id ret_type);

struct bc_instr *
bc_gen_unpack(struct bc_env *, bc_var target,
		object_unpack_func, void *data, int param_id, type_id ret_type);

struct bc_instr *
bc_gen_ret(struct bc_env *, bc_var var);

void
bc_print(struct bc_env *, struct bc_instr *);

#endif
