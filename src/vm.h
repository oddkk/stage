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

struct stg_native_module;
struct bc_instr_store;

struct stg_compile_options {
	struct string *module_locations;
	size_t num_module_locations;
};

struct vm {
	/* struct objstore store; */
	struct stg_memory mem;

	struct arena memory;
	struct arena transient;

	struct atom_table atom_table;

	struct stg_compile_options compile_options;

	struct stg_module **modules;
	size_t num_modules;

	struct stg_native_module **precompiled_native_modules;
	size_t num_precompiled_native_modules;

	struct bc_instr_store *instr_store;

	struct {
		type_id unit;
		type_id type;
		type_id cons;
		type_id inst;
		type_id integer;
		type_id string;
		type_id boolean;
	} default_types;
};

struct ast_context;
struct ast_module;

#define VM_REQUEST_MOD_NO_LOC ((struct string){.text=NULL, .length=0})
#define VM_REQUEST_PINNED ((stg_mod_id)UINT32_MAX)

struct stg_module *
vm_request_module(struct vm *vm,
		stg_mod_id requestor,
		struct atom *module,
		struct string location);

struct stg_module *
vm_get_module(struct vm *vm, struct atom *name);

struct stg_module *
vm_get_module_by_id(struct vm *vm, stg_mod_id);

struct stg_module *
vm_get_module_by_native(struct vm *vm, struct stg_native_module *mod);

struct type *
vm_get_type(struct vm *, type_id);

struct func *
vm_get_func(struct vm *, func_id);

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

int vm_mod_init(struct stg_module *);

int vm_init(struct vm *);
void vm_destroy(struct vm *);

int vm_start(struct vm *);

#endif
