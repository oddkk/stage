#ifndef STAGE_VM_H
#define STAGE_VM_H

#include "objstore.h"
#include "atom.h"
#include "arena.h"
#include "errors.h"

#define TYPE_UNSET ((type_id)0)
#define TYPE_NONE ((type_id)1)

#define TYPE_VALID(tid) ((tid) > TYPE_NONE)

#define OBJ_UNSET ((struct object){.type=TYPE_UNSET})
#define OBJ_NONE ((struct object){.type=TYPE_NONE})

struct stg_module;
struct stg_module_info;

struct stg_native_module;
struct bc_instr_store;

struct vm {
	/* struct objstore store; */
	struct arena memory;
	struct atom_table atom_table;

	struct stg_module **modules;
	size_t num_modules;

	struct stg_native_module **precompiled_native_modules;
	size_t num_precompiled_native_modules;

	struct bc_instr_store *instr_store;

	struct {
		type_id unit;
		type_id type;
		type_id cons;
		type_id integer;
		type_id string;
		type_id boolean;
	} default_types;

	func_id init_func;
	type_id program_object_type;
};

struct ast_context;
struct ast_module;

struct stg_module *
vm_register_module(struct vm *vm, struct ast_context *,
		struct ast_module *, struct stg_module_info *);

struct stg_module *
vm_get_module(struct vm *vm, struct string name);

struct stg_module *
vm_get_module_by_native(struct vm *vm, struct stg_native_module *mod);

struct type *
vm_get_type(struct vm *, type_id);

struct type *
vm_find_type(struct vm *, struct string mod, struct string name);

type_id
vm_find_type_id(struct vm *, struct string mod, struct string name);

struct func *
vm_get_func(struct vm *, func_id);

struct stg_exec
vm_init_exec_context(struct vm *);

void
vm_release_exec_context(struct vm *, struct stg_exec *);

// Note that this function expects ret to have its type set to the expected
// return, and to have data point to a memory location where the result will be
// written.
int
vm_call_func_obj(
		struct vm *, struct stg_exec *ctx,
		struct stg_func_object, struct object *args,
		size_t num_args, struct object *ret);

int
vm_call_func(
		struct vm *, struct stg_exec *ctx,
		func_id, struct object *args,
		size_t num_args, struct object *ret);

struct atom *
vm_atom(struct vm *, struct string name);

// Create an atom from a cstr.
#define vm_atoms(vm, str) vm_atom(vm, STR(str))

struct stg_native_module *
vm_add_precompiled_native_module(struct vm *, struct string name);

int vm_init(struct vm *);
void vm_destroy(struct vm *);

int vm_start(struct vm *);
int vm_post_init(struct vm *);

#endif
