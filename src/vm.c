#include "vm.h"
#include "utils.h"
#include "module.h"
#include "native.h"
#include "dlist.h"
#include "base/mod.h"
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
		mod->mod.root.instance = ast_bind_slot_cons(ctx, &mod->mod.env,
				AST_BIND_NEW, NULL, NULL);
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
	modtype_id mfid = FUNC_ID_TYPE(fid);

	assert(mid < vm->num_modules);
	return store_get_func(&vm->modules[mid]->store, mfid);
}

int
vm_call_func(struct vm *vm, func_id fid, struct object *args,
		size_t num_args, struct object *ret)
{
	struct func *func = vm_get_func(vm, fid);
	struct type *type = vm_get_type(vm, func->type);

	struct stg_func_type *func_type;
	func_type = (struct stg_func_type *)type->data;

	if (func_type->num_params != num_args) {
		printf("Attempted to call function '%.*s' with %zu parameters, expected %zu.\n",
				ALIT(func->name), num_args, func_type->num_params);
		return -1;
	}

	assert_type_equals(vm, ret->type, func_type->return_type);
	assert(ret->data != NULL);

	switch (func->kind) {
		case FUNC_NATIVE:
			{
				ffi_cif cif;
				ffi_type *arg_types[num_args];
				void *arg_values[num_args];

				struct type *return_type;
				return_type = vm_get_type(vm, func_type->return_type);
				if (!return_type->ffi_type) {
					printf("Type '");
					print_type_repr(vm, return_type);
					printf("' is missing a ffi_type.\n");
					abort();
					return -1;
				}
				assert(return_type->ffi_type);

				for (size_t i = 0; i < num_args; i++) {
					struct type *arg_type = vm_get_type(vm, func_type->params[i]);
					assert_type_equals(vm, args[i].type, func_type->params[i]);

					if (!arg_type->ffi_type) {
						printf("Type '");
						print_type_repr(vm, arg_type);
						printf("' is missing a ffi_type.\n");
						abort();
						return -1;
					}
					assert(arg_type->ffi_type);
					arg_types[i] = arg_type->ffi_type;
					arg_values[i] = args[i].data;
				}
				int err;
				err = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, num_args,
						return_type->ffi_type, arg_types);
				if (err != FFI_OK) {
					printf("Failed to prepare call interface (%i).\n", err);
					return -1;
				}

				ffi_call(&cif, (void (*)(void))func->native,
						ret->data, arg_values);
			}
			return 0;
	}

	panic("Invalid func kind");
	return -1;
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

int arena_alloc_stack(struct exec_stack *stack, struct arena *mem, size_t stack_size)
{
	stack->memory = arena_alloc(mem, sizeof(struct object) * stack_size);
	stack->cap = stack_size;
	stack->sp = stack->memory;
	stack->bp = stack->memory;

	if (!stack->memory) {
		return -1;
	}

	return 0;
}

void *stack_push_void(struct exec_stack *stack, size_t size)
{
	void *result;

	assert((stack->sp + size) < (stack->memory + stack->cap));
	memset(stack->sp, 0, size);
	result = stack->sp;
	stack->sp += size;

	return result;
}

void stack_push(struct exec_stack *stack, void *src, size_t size)
{
	assert((stack->sp + size) < (stack->memory + stack->cap));
	memmove(stack->sp, src, size);
	stack->sp += size;
}

void stack_pop_void(struct exec_stack *stack, size_t size)
{
	assert((stack->sp - stack->memory) >= size);
	stack->sp -= size;
}

void stack_pop(struct exec_stack *stack, void *dest, size_t size)
{
	assert((stack->sp - stack->memory) >= size);
	stack->sp -= size;
	memcpy(dest, stack->sp, size);
}

void stack_peek(struct exec_stack *stack, void *dest, size_t size)
{
	assert((stack->sp - stack->memory) >= size);
	memcpy(dest, stack->sp - size, size);
}

inline static void *instr_read_pointer(void **data)
{
	uint64_t result;
	result = **(uint64_t **)data;
	*((uint64_t **)data) += 1;
	return (void *)result;
}

inline static uint8_t instr_read_uint8(void **data)
{
	uint8_t result;
	result = **(uint8_t **)data;
	*((uint8_t **)data) += 1;
	return result;
}

/* inline static uint32_t instr_read_uint32(void **data) */
/* { */
/* 	uint32_t result; */
/* 	result = **(uint32_t **)data; */
/* 	*((uint32_t **)data) += 1; */
/* 	return result; */
/* } */

/* inline static int32_t instr_read_int32(void **data) */
/* { */
/* 	int32_t result; */
/* 	result = **(int32_t **)data; */
/* 	*((int32_t **)data) += 1; */
/* 	return result; */
/* } */

inline static int64_t instr_read_int64(void **data)
{
	int64_t result;
	result = **(int64_t **)data;
	*((int64_t **)data) += 1;
	return result;
}

void vm_exec(struct vm *vm, struct exec_stack *stack, void *instructions, size_t length)
{
	uint8_t *old_bp = stack->bp;
	stack->bp = stack->sp;

	void *ip = instructions;
	void *end = (void *)((uint8_t *)instructions + length);

	while (ip < end) {
		uint8_t op = instr_read_uint8(&ip);

		printf("instr: %x\n", op);

		switch ((enum vm_instruction)op) {
		case VM_INST_NOOP:
			break;

		case VM_INST_PUSH_GLOBAL: {
			uint8_t *addr = (uint8_t *)instr_read_pointer(&ip);
			uint8_t size = instr_read_uint8(&ip);
			stack_push(stack, addr, size);
		} break;

		case VM_INST_PUSH_LOCAL: {
			int64_t offset = instr_read_int64(&ip);
			uint32_t size  = instr_read_uint8(&ip);
			stack_push(stack, stack->bp + offset, size);
		} break;

		case VM_INST_POP: {
			uint8_t size  = instr_read_uint8(&ip);
			stack_pop_void(stack, size);
		} break;

		case VM_INST_CALL: {
							   /*
			struct obj_builtin_func_data data;
			stack_pop(stack, &data, sizeof(data));

			data.func(vm, stack, data.data);
			*/
		} break;

		case VM_INST_CALL_BUILTIN: {
			void *ptr = instr_read_pointer(&ip);
			void *data = instr_read_pointer(&ip);
			vm_builtin_func func = (vm_builtin_func)ptr;

			func(vm, stack, data);
		} break;

		case VM_INST_RETURN:
			ip = end;
			break;
		}
	}

	stack->bp = old_bp;
}
