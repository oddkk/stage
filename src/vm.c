#include "vm.h"
#include "utils.h"

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
