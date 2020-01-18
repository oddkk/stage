#include "vm.h"
#include "utils.h"
#include "module.h"
#include "native.h"
#include "dlist.h"
#include "base/mod.h"
#include "bytecode.h"
#include "native_bytecode.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

int vm_init(struct vm *vm)
{
	int err;

	zero_memory(vm, sizeof(struct vm));
	err = arena_init(&vm->memory, MEGABYTE(10));

	if (err) {
		return -1;
	}

	vm->atom_table.string_arena = &vm->memory;
	atom_table_rehash(&vm->atom_table, 64);

	vm->instr_store = calloc(1, sizeof(struct bc_instr_store));

	return 0;
}

void vm_destroy(struct vm *vm)
{
	for (size_t i = 0; i < vm->num_modules; i++) {
		struct stg_module *mod = vm->modules[i];

		if (mod->info.free) {
			mod->info.free(mod);
		}

		free(mod);
	}

	free(vm->modules);
	free(vm->memory.data);
	// TODO: Free atom table
}

int vm_start(struct vm *vm)
{
	for (size_t i = 0; i < vm->num_modules; i++) {
		struct stg_module *mod = vm->modules[i];
		if (mod->info.start) {
			mod->info.start(mod);
		}
	}

	return 0;
}

int vm_post_init(struct vm *vm)
{
	for (size_t i = 0; i < vm->num_modules; i++) {
		struct stg_module *mod = vm->modules[i];
		if (mod->info.post_init) {
			mod->info.post_init(mod);
		}
	}

	return 0;
}

struct stg_module *
vm_register_module(struct vm *vm, struct ast_context *ctx,
		struct ast_module *old_module, struct stg_module_info *info)
{

	struct stg_module *mod;
	mod = calloc(1, sizeof(struct stg_module));
	// TODO: Should each module have its own arena, and use that for
	// its root scope?
	mod->vm = vm;
	mod->atom_table = &vm->atom_table;

	mod->info = *info;

	mod->id = dlist_append(vm->modules,
						   vm->num_modules, &mod);
	mod->store.mod_id = mod->id;
	if (old_module) {
		mod->mod = *old_module;
	} else {
		mod->mod.env.store = &mod->store;
		// TODO: Initialize the module.
		// mod->mod.root.instance = ast_bind_slot_cons(ctx, &mod->mod.env,
		// 		AST_BIND_NEW, NULL, NULL);
	}

	mod->mod.stg_mod = mod;

	if (mod->info.init) {
		mod->info.init(ctx, mod);
	}

	return mod;
}

struct stg_module *
vm_get_module(struct vm *vm, struct string mod_name)
{
	struct stg_module *mod = NULL;

	for (uint32_t mid = 0; mid < vm->num_modules; mid++) {
		if (string_equal(vm->modules[mid]->info.name, mod_name)) {
			mod = vm->modules[mid];
		}
	}

	return mod;
}

struct stg_module *
vm_get_module_by_native(struct vm *vm, struct stg_native_module *nmod)
{
	struct stg_module *mod = NULL;

	for (uint32_t mid = 0; mid < vm->num_modules; mid++) {
		if (vm->modules[mid]->native_mod == nmod) {
			mod = vm->modules[mid];
			break;
		}
	}

	return mod;
}

struct type *
vm_get_type(struct vm *vm, type_id tid)
{
	uint32_t mid = TYPE_ID_MOD(tid);
	modtype_id mtid = TYPE_ID_TYPE(tid);

	assert(mid < vm->num_modules);
	return store_get_type(&vm->modules[mid]->store, mtid);
}

type_id
vm_find_type_id(struct vm *vm, struct string mod_name, struct string name)
{
	struct stg_module *mod = NULL;

	mod = vm_get_module(vm, mod_name);

	// for (uint32_t mid = 0; mid < vm->num_modules; mid++) {
	// 	if (string_equal(vm->modules[mid]->info.name, mod_name)) {
	// 		mod = vm->modules[mid];
	// 	}
	// }

	assert(mod != NULL);

	/*
	struct string tail = name;
	struct string part;

	// TODO: Looup type name.
	while (string_split(tail, &part, &tail, '.')) {
	}
	*/

	type_id result = TYPE_UNSET;
	//result = type_obj_get(vm, entry.object);

	return result;
}

struct type *
vm_find_type(struct vm *vm, struct string mod, struct string name)
{
	type_id tid;
	tid = vm_find_type_id(vm, mod, name);
	return vm_get_type(vm, tid);
}

struct func *
vm_get_func(struct vm *vm, func_id fid)
{
	uint32_t mid = FUNC_ID_MOD(fid);
	modtype_id mfid = FUNC_ID_LOCAL(fid);

	assert(mid < vm->num_modules);
	return store_get_func(&vm->modules[mid]->store, mfid);
}

struct stg_exec
vm_init_exec_context(struct vm *vm)
{
	struct stg_exec res = {0};
	res.heap = arena_push(&vm->memory);
	return res;
}

void
vm_release_exec_context(struct vm *vm, struct stg_exec *ctx)
{
	arena_pop(&vm->memory, ctx->heap);
	memset(ctx, 0, sizeof(struct stg_exec));
}

int
vm_call_func_obj(
		struct vm *vm, struct stg_exec *ctx,
		struct stg_func_object func_obj, struct object *args,
		size_t num_args, struct object *ret)
{

	struct func *func = vm_get_func(vm, func_obj.func);
	struct type *type = vm_get_type(vm, func->type);

	struct stg_func_type *func_type;
	func_type = (struct stg_func_type *)type->data;

	if (func_type->num_params != num_args) {
		printf("Attempted to call function '%.*s' with %zu parameters, expected %zu.\n",
				ALIT(func->name), num_args, func_type->num_params);
		return -1;
	}

	struct type *ret_type = vm_get_type(vm, func_type->return_type);;

	assert_type_equals(vm, ret->type, func_type->return_type);
	assert(ret->data != NULL || ret_type->size == 0);

	bool tmp_exec_ctx = !ctx;
	struct stg_exec _tmp_exec_ctx;
	if (tmp_exec_ctx) {
		_tmp_exec_ctx = vm_init_exec_context(vm);
		ctx = &_tmp_exec_ctx;
	}

	switch (func->kind) {
		case FUNC_NATIVE:
			{
				// We reserver arg_values[0] and arg_values[1] for the heap and
				// closure value if present.
				void *arg_values[num_args+2];
				size_t prefix = 0;

				if ((func->flags & FUNC_HEAP) != 0) {
					arg_values[prefix] = &ctx;
					prefix += 1;
				}

				if ((func->flags & FUNC_CLOSURE) != 0) {
					arg_values[prefix] = &func_obj.closure;
					prefix += 1;
				}

				for (size_t i = 0; i < num_args; i++) {
					arg_values[prefix+i] = args[i].data;
				}

				ffi_cif *cif = stg_func_ffi_cif(vm, func->type,
						func->flags);

				if ((func->flags & FUNC_REFS) != 0) {
					native_ref_func fp;
					fp = (native_ref_func)func->native;
					fp(arg_values, num_args+prefix, ret->data);
				} else {
					ffi_call(cif, FFI_FN(func->native), ret->data, arg_values);
				}
			}
			break;

		case FUNC_BYTECODE:
			{
				void *call_args[num_args];
				for (size_t i = 0; i < num_args; i++) {
					call_args[i] = args[i].data;
				}
				nbc_exec(vm, ctx, func->bytecode->nbc,
						call_args, num_args, func_obj.closure, ret->data);
			}
			break;

		default:
			panic("Invalid func kind");
			return -1;
	}

	if (tmp_exec_ctx) {
		vm_release_exec_context(vm, ctx);
	}

	return 0;
}

int
vm_call_func(
		struct vm *vm, struct stg_exec *ctx,
		func_id fid, struct object *args,
		size_t num_args, struct object *ret)
{
	struct func *func = vm_get_func(vm, fid);
	assert((func->flags & FUNC_CLOSURE) == 0);

	struct stg_func_object func_obj = {0};
	func_obj.func = fid;

	return vm_call_func_obj(vm, ctx, func_obj, args, num_args, ret);
}

struct atom *
vm_atom(struct vm *vm, struct string name)
{
	return atom_create(&vm->atom_table, name);
}

struct stg_native_module *
vm_add_precompiled_native_module(struct vm *vm, struct string name)
{
	struct stg_native_module *mod;

	mod = calloc(1, sizeof(struct stg_native_module));

	mod->name = vm_atom(vm, name);

	size_t id = dlist_append(vm->precompiled_native_modules,
			vm->num_precompiled_native_modules, &mod);

	return vm->precompiled_native_modules[id];
}
