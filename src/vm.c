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
	zero_memory(vm, sizeof(struct vm));

	int err;

	err = stg_memory_init(&vm->mem);
	if (err) {
		return -1;
	}

	arena_init(&vm->memory, &vm->mem);
	vm->memory.flags |= ARENA_NO_CHECKPOINT;

	arena_init(&vm->transient, &vm->mem);

	vm->atom_table.string_arena = &vm->memory;
	atom_table_rehash(&vm->atom_table, 64);

	vm->instr_store = calloc(1, sizeof(struct bc_instr_store));

	return 0;
}

void vm_destroy(struct vm *vm)
{
	for (size_t i = 0; i < vm->num_modules; i++) {
		struct stg_module *mod = vm->modules[i];

		if (mod->state == STG_MOD_LIFE_DESTROYED) {
			continue;
		}

		assert(mod->state == STG_MOD_LIFE_IDLE ||
				mod->state == STG_MOD_LIFE_FAILED);

		stg_mod_invoke_destroy(mod);
		stg_module_destroy(mod);
		mod->state = STG_MOD_LIFE_DESTROYED;
	}

	for (size_t i = 0; i < vm->num_modules; i++) {
		struct stg_module *mod = vm->modules[i];
		free(mod);
	}

	free(vm->modules);

	for (size_t i = 0; i < vm->num_precompiled_native_modules; i++) {
		stg_native_module_destroy(vm->precompiled_native_modules[i]);
		free(vm->precompiled_native_modules[i]);
	}
	free(vm->precompiled_native_modules);

	free(vm->instr_store);
	atom_table_destroy(&vm->atom_table);
	stg_memory_destroy(&vm->mem);

	memset(vm, 0, sizeof(struct vm));
}

int
vm_mod_init(struct stg_module *mod)
{
	int err;
	err = stg_mod_invoke_pre_init(mod);
	if (err < 0) {
		printf("Intialize module '%.*s' failed. (pre)\n", ALIT(mod->name));
		mod->state = STG_MOD_LIFE_FAILED;
		return -1;
	}

	if (mod->name == mod_atoms(mod, "base")) {
		mod->instance = mod->init_monad;
	} else {
		struct object main_obj = mod->init_monad;

		assert(stg_type_is_init(mod->vm, main_obj.type));

		struct type *main_obj_type;
		main_obj_type = vm_get_type(mod->vm, main_obj.type);

		type_id inner_type_id;
		inner_type_id = stg_init_get_return_type(mod->vm, main_obj.type);

		struct type *inner_type;
		inner_type = vm_get_type(mod->vm, inner_type_id);

		struct object result = {0};
		result.type = inner_type_id;

		uint8_t result_buffer[inner_type->size];
		memset(result_buffer, 0, inner_type->size);
		result.data = result_buffer;

		struct stg_exec exec_ctx = {0};

		exec_ctx.vm = mod->vm;
		exec_ctx.heap = &mod->vm->transient;
		arena_mark cp = arena_checkpoint(exec_ctx.heap);

		struct stg_init_context init_ctx = {0};
		init_ctx.mod = mod;
		init_ctx.vm = mod->vm;

		// TODO: Let dependecies initialize the init context.

		stg_unsafe_call_init(&init_ctx, &exec_ctx, main_obj, &result);

		result = register_object(
				mod->vm, &mod->store, result);

		arena_reset(exec_ctx.heap, cp);

		mod->instance = result;
	}

	err = stg_mod_invoke_post_init(mod);
	if (err < 0) {
		printf("Intialize module '%.*s' failed. (post)\n", ALIT(mod->name));
		mod->state = STG_MOD_LIFE_FAILED;
		return -1;
	}

	return 0;
}

int vm_start(struct vm *vm)
{
	for (size_t i = 0; i < vm->num_modules; i++) {
		if (vm->modules[i]) {
			stg_mod_invoke_start(vm->modules[i]);
		}
	}

	return 0;
}

int vm_stop(struct vm *vm)
{
	for (size_t i = 0; i < vm->num_modules; i++) {
		if (vm->modules[i]) {
			stg_mod_invoke_stop(vm->modules[i]);
		}
	}

	return 0;
}


static void
vm_add_request(struct vm *vm,
		struct stg_module *req_mod,
		struct stg_module *mod)
{
	if (req_mod) {
		bool dep_exist = false;
		for (size_t i = 0; i < req_mod->num_dependencies; i++) {
			if (req_mod->dependencies[i] == mod->id) {
				dep_exist = true;
				break;
			}
		}

		if (!dep_exist) {
			dlist_append(
					req_mod->dependencies,
					req_mod->num_dependencies,
					&mod->id);
		}
	} else {
		mod->pin = true;
	}
}

struct stg_module *
vm_request_module(struct vm *vm,
		stg_mod_id requestor,
		struct atom *target,
		struct string src_dir)
{
	struct stg_module *req_mod = NULL;
	if (requestor != VM_REQUEST_PINNED) {
		req_mod = vm_get_module_by_id(vm, requestor);
		assert(req_mod);
	}

	struct stg_module *mod;
	mod = vm_get_module(vm, target);

	if (mod) {
		if (src_dir.text && src_dir.length > 0) {
			if (!string_equal(mod->src_dir, src_dir)) {
				printf("Module conflict: Attempted to load module '%.*s' from:\n"
					   " - %.*s\n - %.*s\n",
					   ALIT(target), LIT(mod->src_dir), LIT(src_dir));
				return NULL;
			}
		}

		vm_add_request(vm, req_mod, mod);

		return mod;
	}

	mod = calloc(1, sizeof(struct stg_module));

	mod->vm = vm;
	mod_arena(mod, &mod->mem);

	mod->name = target;
	mod->src_dir = src_dir;

	mod->id = dlist_append(
			vm->modules, vm->num_modules, &mod);

	objstore_init(&mod->store, mod->id, &vm->mem);

	struct atom *base_atom = vm_atoms(vm, "base");
	if (mod->name != base_atom) {
		vm_request_module(vm, mod->id,
				base_atom,
				VM_REQUEST_MOD_NO_LOC);
	}

	vm_add_request(vm, req_mod, mod);

	return mod;
}

struct stg_module *
vm_get_module(struct vm *vm, struct atom *mod_name)
{
	struct stg_module *mod = NULL;

	for (uint32_t mid = 0; mid < vm->num_modules; mid++) {
		if (vm->modules[mid] && vm->modules[mid]->name == mod_name) {
			mod = vm->modules[mid];
		}
	}

	return mod;
}

struct stg_module *
vm_get_module_by_id(struct vm *vm, stg_mod_id id)
{
	assert(id < vm->num_modules);
	return vm->modules[id];
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

struct func *
vm_get_func(struct vm *vm, func_id fid)
{
	uint32_t mid = FUNC_ID_MOD(fid);
	modtype_id mfid = FUNC_ID_LOCAL(fid);

	assert(mid < vm->num_modules);
	return store_get_func(&vm->modules[mid]->store, mfid);
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
	struct stg_exec _tmp_exec_ctx = {0};
	arena_mark cp = {0};

	if (tmp_exec_ctx) {
		_tmp_exec_ctx.heap = &vm->transient;
		cp = arena_checkpoint(_tmp_exec_ctx.heap);
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

				if ((func->flags & FUNC_REFS) != 0) {
					native_ref_func fp;
					fp = (native_ref_func)func->native;
					fp(arg_values, num_args+prefix, ret->data);
				} else {
					ffi_cif *cif = stg_func_ffi_cif(vm, func->type,
							func->flags);

					ffi_call(cif, FFI_FN(func->native), ret->data, arg_values);
				}
			}
			break;

		case FUNC_CONS:
			{
				void *args_data[num_args];
				for (size_t i = 0; i < num_args; i++) {
					args_data[i] = args[i].data;
				}
				assert(func->cons->pack);
				func->cons->pack(vm, ctx,
						func->cons->data, ret->data,
						args_data, num_args);
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
		arena_reset(_tmp_exec_ctx.heap, cp);
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
