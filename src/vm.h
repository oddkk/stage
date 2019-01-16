#ifndef STAGE_VM_H
#define STAGE_VM_H

#include "objstore.h"
#include "scope.h"
#include "atom.h"
#include "arena.h"

enum vm_instruction {
	// Does nothing.
	VM_INST_NOOP = 0x00,

	// Loads a value from the global object store, and pushes it to
	// the top of the stack. Takes 2 parameter, a 64-bit pointer to
	// the beginning of the data, and a 8-bit unsigned int of the size.
	VM_INST_PUSH_GLOBAL = 0x01,

	// Loads a value from the stack, relative to the base pointer
	// (bp), and pushes it to the top of the stack. Takes 2 parameter,
	// a 64-bit signed offset from bp, and a 8-bit size.
	VM_INST_PUSH_LOCAL = 0x02,

	// Pops a value from the stack, and discards the result. Takes 1
	// parameter, a 8-bit size.
	VM_INST_POP = 0x03,

	// Pops the top of the stack, and calls this object.
	VM_INST_CALL = 0x04,

	// Calls a built in (C-style) function. Takes 2 parameters: a
	// 64-bit pointer to the function to be called, and a 64-bit
	// pointer to some user data. This function should have the
	// signature
	// void fun(struct vm *, struct stack *stack, void *data);
	VM_INST_CALL_BUILTIN = 0x05,

	// Terminates execution of the current function and returns
	// execution to the caller.
	VM_INST_RETURN = 0x06,
};

// Calling convention:
// The vm consists of an instruction list, a stack, and a global
// store. When a function is called, its instruction list is iterated
// through, and each instruction is evaluated.

// The stack grows positivly. It contains two special registers: sp
// and bp. Sp is the stack pointer, and is pointing to the next
// address to insert an object. Bp is the base pointer, and is
// pointing to the beginning of the current function's stack
// frame. When called, a function expects to have its n arguments at
// (bp - 1), (bp - 2), ..., (bp - n).

#define INT16_TO_2INT8(i)						\
	(((i) >> 0)  & 0xff),						\
		(((i) >> 8)  & 0xff)

#define INT32_TO_4INT8(i)						\
	(((i) >> 0)  & 0xff),						\
		(((i) >> 8)  & 0xff),					\
		(((i) >> 16) & 0xff),					\
		(((i) >> 24) & 0xff)

#define INT64_TO_8INT8(i)						\
	(((i) >> 0)  & 0xff),						\
		(((i) >> 8)  & 0xff),					\
		(((i) >> 16) & 0xff),					\
		(((i) >> 24) & 0xff),					\
		(((i) >> 32) & 0xff),					\
		(((i) >> 40) & 0xff),					\
		(((i) >> 48) & 0xff),					\
		(((i) >> 56) & 0xff)

#define LIT_VM_INST_NOOP() ((uint8_t)VM_INST_NOOP)
#define LIT_VM_INST_PUSH_GLOBAL(addr, size)		\
	((uint8_t)VM_INST_PUSH_GLOBAL),				\
		INT64_TO_8INT8(addr),					\
		((uint8_t)size)
#define LIT_VM_INST_PUSH_LOCAL(offset, size)			\
	((uint8_t)VM_INST_PUSH_LOCAL),						\
		INT64_TO_4INT8(offset),							\
		((uint8_t)size)
#define LIT_VM_INST_POP(size) ((uint8_t)VM_INST_POP), ((uint8_t)size)
#define LIT_VM_INST_CALL() ((uint8_t)VM_INST_CALL)
#define LIT_VM_INST_CALL_BUILTIN(func, data)	\
	((uint8_t)VM_INST_CALL_BUILTIN),			\
		INT64_TO_8INT8((uint64_t)func),			\
		INT64_TO_8INT8((uint64_t)data)
#define LIT_VM_INST_RETURN() ((uint8_t)VM_INST_RETURN)

#define TYPE_UNSET ((type_id)0)
#define TYPE_NONE ((type_id)1)
#define TYPE_SCOPE ((type_id)2)
#define TYPE_TEMPLATE_PARAM ((type_id)3)

#define TYPE_VALID(tid) ((tid) > TYPE_NONE)

#define OBJ_UNSET ((obj_id)0)
#define OBJ_NONE ((obj_id)1)

#define OBJ_VALID(oid) ((oid) > OBJ_NONE)

struct vm {
	struct objstore store;
	struct scope root_scope;
	struct arena memory;
	struct atom_table atom_table;

	struct {
		type_id unset;
		type_id none;
		type_id scope;
		type_id template_param;
		type_id type;
		type_id integer;
		type_id string;

		type_id func_template_return;

		struct type_base func_base;
		struct type_base enum_base;
		struct type_base tuple_base;
	} default_types;

};

struct exec_stack {
	uint8_t *memory;
	uint8_t *bp;
	uint8_t *sp;
	size_t cap;
};

typedef void (*vm_builtin_func)(struct vm *, struct exec_stack *, void *);

obj_id obj_register_string(struct vm *vm, struct string value);
obj_id obj_register_integer(struct vm *vm, int64_t value);
obj_id obj_register_type(struct vm *vm, type_id value);
obj_id obj_register_builtin_func_from_tuple(struct vm *vm, type_id params, type_id ret_type,
											vm_builtin_func value, void *data);
obj_id obj_register_builtin_func(struct vm *vm, struct atom **param_names,
								 type_id *params, size_t num_params,
								 type_id ret_type, vm_builtin_func value,
								 void *data);

type_id type_obj_get(struct vm *vm, struct object obj);

struct type_enum_item {
	struct atom *name;
	type_id type;

	int64_t value;
	struct type_enum *owner;
};

struct type_enum {
	size_t num_items;
	struct type_enum_item *items;

	// Number of bytes, excluding the size of the enum value.
	size_t size;
};

struct type_tuple_item {
	struct atom *name;
	type_id type;
};

struct type_tuple {
	struct atom **names;
	type_id *types;
	size_t num_items;
	bool named;

	// Number of bytes
	size_t size;
};

struct type_func {
	struct atom **param_names;
	type_id *param_types;
	size_t num_params;

	type_id ret;
};

struct type_class {
	int _dc;
};

struct obj_builtin_func_data {
	vm_builtin_func func;
	void *data;
};

type_id type_register_enum(struct vm *vm, struct type_enum *t);
type_id type_register_named_tuple(struct vm *vm, struct type_tuple_item *items, size_t num_items);
type_id type_register_unnamed_tuple(struct vm *vm, type_id *items, size_t num_items);
type_id type_register_function(struct vm *vm, struct atom **param_names,
							   type_id *param_types, size_t num_params,
							   type_id ret);

void vm_exec(struct vm *vm, struct exec_stack *stack, void *instructions, size_t length);

/* static inline struct object vm_arg(struct exec_stack *stack, size_t arg) */
/* { */
/* 	assert(stack->bp > arg); */
/* 	return stack->memory[stack->bp - arg - 1]; */
/* } */

int arena_alloc_stack(struct exec_stack *stack, struct arena *mem, size_t stack_size);

void stack_push(struct exec_stack *stack, void *src, size_t size);
void stack_pop_void(struct exec_stack *stack, size_t size);
void stack_pop(struct exec_stack *stack, void *dest, size_t size);

int vm_init(struct vm *);

#endif
