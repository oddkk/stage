#include "vm.h"
#include "utils.h"
#include "expr.h"
#include "module.h"
#include "dlist.h"
#include "modules/base/mod.h"
#include <stdlib.h>
#include <string.h>

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

	vm->root_scope.parent = 0;
	vm->root_scope.lookup.page_arena = &vm->memory;
	vm->root_scope.lookup.string_arena = &vm->memory;

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
	// TODO: Free scopes
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
vm_register_module(struct vm *vm, struct stg_module_info *info)
{

	struct stg_module *mod;
	mod = calloc(1, sizeof(struct stg_module));
	// TODO: Should each module have its own arena, and use that for
	// its root scope?
	mod->vm = vm;
	mod->root_scope.parent = &vm->root_scope;
	mod->root_scope.lookup.page_arena = &vm->memory;
	mod->root_scope.lookup.string_arena = &vm->memory;
	mod->atom_table = &vm->atom_table;

	mod->info = *info;

	mod->id = dlist_append(vm->modules,
						   vm->num_modules, &mod);
	mod->store.mod_id = mod->id;

	if (mod->info.init) {
		mod->info.init(mod);
	}

	scope_insert(&vm->root_scope, atom_create(&vm->atom_table, info->name),
				 SCOPE_ANCHOR_NONE, OBJ_NONE, &mod->root_scope);

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

	for (uint32_t mid = 0; mid < vm->num_modules; mid++) {
		if (string_equal(vm->modules[mid]->info.name, mod_name)) {
			mod = vm->modules[mid];
		}
	}

	assert(mod != NULL);

	struct string tail = name;
	struct string part;

	struct scope_entry entry;
	entry.scope = &mod->root_scope;

	while (string_split(tail, &part, &tail, '.')) {
		int err;
		struct atom *part_atom;
		part_atom =
			atom_create(mod->atom_table, part);

		assert(entry.scope != NULL);

		err = scope_local_lookup(entry.scope, part_atom, &entry);
		assert(!err);
	}

	assert(entry.object.type == vm->default_types.type);
	type_id result;
	result = type_obj_get(vm, entry.object);

	return result;
}

struct type *
vm_find_type(struct vm *vm, struct string mod, struct string name)
{
	type_id tid;
	tid = vm_find_type_id(vm, mod, name);
	return vm_get_type(vm, tid);
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
			struct obj_builtin_func_data data;
			stack_pop(stack, &data, sizeof(data));

			data.func(vm, stack, data.data);
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
