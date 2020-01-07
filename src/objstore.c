#include "objstore.h"
#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>
#include "dlist.h"
#include "utils.h"
#include "vm.h"
#include "base/mod.h"

static struct string default_type_repr(struct vm *vm, struct arena *mem,
									   struct type *type)
{
	return type->base->name;
}

static struct string default_obj_repr(struct vm *vm, struct arena *mem,
									  struct object *object)
{
	struct type *type = vm_get_type(vm, object->type);
	return type->base->name;
}

struct type
_init_plain_type(struct type_base *base, struct atom *name, size_t size)
{
	struct type t = {0};

	t.name = name;
	t.base = base;
	t.size = size;

	return t;
}

bool
type_equals(struct vm *vm, type_id lhs_id, type_id rhs_id)
{
	if (lhs_id == rhs_id) {
		return true;
	}

	struct type *lhs = vm_get_type(vm, lhs_id);
	struct type *rhs = vm_get_type(vm, rhs_id);

	return (
		lhs->base == rhs->base &&
		lhs->base->equals &&
		lhs->base->equals(vm, lhs, rhs)
	);
}

void
_assert_type_equals_failed(struct vm *vm, type_id lhs, type_id rhs,
		const char *file, int line, const char *func)
{
	printf("stage: %s:%i: %s: Assertion type_equals(<",
			file, line, func);
	print_type_repr(vm, vm_get_type(vm, lhs));
	printf(">, <");
	print_type_repr(vm, vm_get_type(vm, rhs));
	printf(">) failed.\n");
	abort();
}

bool
obj_equals(struct vm *vm, struct object lhs, struct object rhs)
{
	if (!type_equals(vm, lhs.type, rhs.type)) {
		return false;
	}

	struct type *lhs_type = vm_get_type(vm, lhs.type);
	struct type *rhs_type = vm_get_type(vm, rhs.type);
	assert(lhs_type->size == rhs_type->size);
	// TODO: Use user defined comparator.
	return memcmp(lhs.data, rhs.data, lhs_type->size) == 0;
}

struct object
register_object(struct vm *vm, struct objstore *store, struct object obj) {
	if (store->page_size == 0) {
		store->page_size = sysconf(_SC_PAGESIZE);
	}

	struct type *type = vm_get_type(vm, obj.type);

	if (store->data_pages == NULL ||
		store->last_data_page_used + type->size >= store->page_size) {
		size_t new_size = (store->num_data_pages + 1) * sizeof(uint8_t *);
		uint8_t **new_pages = realloc(store->data_pages, new_size);

		if (!new_pages) {
			perror("realloc");
			return OBJ_NONE;
		}

		store->data_pages = new_pages;

		new_pages[store->num_data_pages] = mmap(NULL, store->page_size,
										   PROT_READ|PROT_WRITE,
										   MAP_PRIVATE|MAP_ANONYMOUS,
										   -1, 0);

		if (new_pages[store->num_data_pages] == MAP_FAILED) {
			perror("mmap");
			return OBJ_NONE;
		}

		store->num_data_pages = store->num_data_pages + 1;
		store->last_data_page_used = 0;
	}


	struct object o = {0};
	o.type = obj.type;

	if (type->size > 0) {
		size_t addr = ((store->num_data_pages - 1) * store->page_size
					   + store->last_data_page_used);
		store->last_data_page_used += type->size;

		o.data = &store->data_pages[addr / store->page_size][addr % store->page_size];

		memcpy((void *)o.data, (void *)obj.data, type->size);
	}

	return o;
}

void free_objstore(struct objstore *store) {
	for (size_t i = 0; i < store->num_data_pages; i++) {
		int err;
		err = munmap(store->data_pages[i], store->page_size);
		if (err != 0) {
			perror("munmap");
		}
	}

	if (store->data_pages) {
		free(store->data_pages);
	}

	if (store->types) {
		free(store->types);
		store->types = NULL;
	}

	if (store->funcs) {
		free(store->funcs);
		store->funcs = NULL;
	}
}

void *
stg_alloc(struct stg_exec *ctx, size_t nmemb, size_t size)
{
	size_t res;
	// TODO: Make this cross platform and cross compiler compliant.
	if (!__builtin_mul_overflow(nmemb, size, &res)) {
		panic("Attempted to allocate memory with a size that exeedes 64-bit integers.");
		return NULL;
	}

	return arena_alloc(&ctx->heap, res);
}

modtype_id
store_register_type(struct objstore *store, struct type type)
{
	modtype_id mtid;
	mtid = dlist_append(store->types, store->num_types, &type);
	return TYPE_ID(store->mod_id, mtid);
}

modfunc_id
store_register_func(struct objstore *store, struct func func)
{
	modfunc_id mfid;
	mfid = dlist_append(store->funcs, store->num_funcs, &func);
	return FUNC_ID(store->mod_id, mfid);
}

type_id
func_return_type(struct vm *vm, type_id func_type_id)
{
	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	return func_info->return_type;
}

size_t
func_num_params(struct vm *vm, type_id func_type_id)
{
	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	return func_info->num_params;
}

type_id
func_param_type(struct vm *vm, type_id func_type_id, size_t param_i)
{
	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	assert(param_i < func_info->num_params);
	return func_info->params[param_i];
}

void print_type_repr(struct vm *vm, struct type *type)
{
	struct arena mem = arena_push(&vm->memory);
	/* struct type *type = &vm->store.types[obj.type]; */
	struct string res;

	if (type->base->repr) {
		res = type->base->repr(vm, &mem, type);
	} else {
		res = default_type_repr(vm, &mem, type);
	}
	printf("%.*s", LIT(res));

	arena_pop(&vm->memory, mem);
}

struct string
type_repr_to_alloced_string(struct vm *vm, struct type *type)
{
	struct arena mem = arena_push(&vm->memory);
	struct string res;

	if (type->base->repr) {
		res = type->base->repr(vm, &mem, type);
	} else {
		res = default_type_repr(vm, &mem, type);
	}

	struct string alloced;
	alloced.length = res.length;
	alloced.text = calloc(1, alloced.length);

	memcpy(alloced.text, res.text, alloced.length);

	arena_pop(&vm->memory, mem);

	return alloced;
}

void print_obj_repr(struct vm *vm, struct object obj)
{
	struct arena mem = arena_push(&vm->memory);
	struct type *type = vm_get_type(vm, obj.type);
	struct string res;

	if (type->base->obj_repr) {
		res = type->base->obj_repr(vm, &mem, &obj);
	} else {
		res = default_obj_repr(vm, &mem, &obj);
	}
	printf("%.*s", LIT(res));

	arena_pop(&vm->memory, mem);
}

struct string
obj_repr_to_alloced_string(struct vm *vm, struct object obj)
{
	struct arena mem = arena_push(&vm->memory);
	struct type *type = vm_get_type(vm, obj.type);
	struct string res;

	if (type->base->obj_repr) {
		res = type->base->obj_repr(vm, &mem, &obj);
	} else {
		res = default_obj_repr(vm, &mem, &obj);
	}

	struct string alloced;
	alloced.length = res.length;
	alloced.text = calloc(1, alloced.length);

	memcpy(alloced.text, res.text, alloced.length);

	arena_pop(&vm->memory, mem);

	return alloced;
}

void
arena_string_append_type_repr(struct string *str, struct vm *vm,
							  struct arena *mem, struct type *type)
{
	struct string type_repr;

	struct arena tmp_mem = arena_push(mem);
	if (type->base->repr) {
		type_repr = type->base->repr(vm, &tmp_mem, type);
	} else {
		type_repr = default_type_repr(vm, &tmp_mem, type);
	}
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, str, type_repr);
}

void
arena_string_append_obj_repr(struct string *str, struct vm *vm,
							 struct arena *mem, struct object *object)
{
	struct string repr;
	struct type *type;
	type = vm_get_type(vm, object->type);

	struct arena tmp_mem = arena_push(mem);
	if (type->base->obj_repr) {
		repr = type->base->obj_repr(vm, &tmp_mem, object);
	} else {
		repr = default_obj_repr(vm, &tmp_mem, object);
	}
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, str, repr);
}

ssize_t
object_cons_find_param(
		struct object_cons *cons,
		struct atom *name)
{
	for (size_t i = 0; i < cons->num_params; i++) {
		if (cons->params[i].name == name) {
			return i;
		}
	}

	return -1;
}

size_t
object_cons_num_descendants(
		struct vm *vm, struct object_cons *cons)
{
	size_t count = 0;
	count += cons->num_params;

	for (size_t i = 0; i < cons->num_params; i++) {
		struct type *member_type;
		member_type = vm_get_type(
				vm, cons->params[i].type);

		if (member_type->obj_def) {
			count += object_cons_num_descendants(
					vm, member_type->obj_def);
		}
	}

	return count;
}

void
object_cons_local_descendent_ids(
		struct vm *vm, struct object_cons *cons,
		int *out_local_descendent_ids)
{
	size_t count = 0;
	for (size_t i = 0; i < cons->num_params; i++) {
		out_local_descendent_ids[i] = count;

		struct type *member_type;
		member_type = vm_get_type(
				vm, cons->params[i].type);

		if (member_type->obj_def) {
			count += object_cons_num_descendants(
					vm, member_type->obj_def);
		}
	}
}

int
object_unpack(
		struct vm *vm, struct object obj,
		size_t unpack_id, struct object *out)
{
	if (unpack_id == 0) {
		*out = obj;
		return 0;
	}

	struct type *type;
	type = vm_get_type(vm, obj.type);

	struct object_cons *def;
	def = type->obj_def;
	// If unpack_id is not 0 it is implied that it must be a child of this
	// member. If this member does not have a obj_def it can not have children.
	assert(def);

	size_t offset = 1;
	for (size_t i = 0; i < def->num_params; i++) {
		struct type *mbr_type;
		mbr_type = vm_get_type(
				vm, def->params[i].type);

		size_t num_desc;
		if (mbr_type->obj_def) {
			num_desc = object_cons_num_descendants(
					vm, mbr_type->obj_def);
		} else {
			num_desc = 1;
		}

		assert(unpack_id >= offset);
		if (unpack_id >= offset + num_desc) {
			offset += num_desc;
			continue;
		}

		uint8_t buffer[mbr_type->size];
		assert(def->unpack);
		def->unpack(vm, def->data, buffer, obj.data, i);

		struct object mbr = {0};
		mbr.data = buffer;
		mbr.type = def->params[i].type;

		return object_unpack(vm,
				mbr, unpack_id - offset, out);
	}

	return -1;
}

int
object_cons_descendant_type(
		struct vm *vm, type_id tid,
		size_t unpack_id, type_id *out)
{
	if (unpack_id == 0) {
		*out = tid;
		return 0;
	}

	struct type *type;
	type = vm_get_type(vm, tid);

	struct object_cons *def;
	def = type->obj_def;
	// If unpack_id is not 0 it is implied that it must be a child of this
	// member. If this member does not have a obj_def it can not have children.
	assert(def);

	size_t offset = 1;
	for (size_t i = 0; i < def->num_params; i++) {
		struct type *mbr_type;
		mbr_type = vm_get_type(
				vm, def->params[i].type);

		size_t num_desc;
		if (mbr_type->obj_def) {
			num_desc = object_cons_num_descendants(
					vm, mbr_type->obj_def);
		} else {
			num_desc = 1;
		}

		assert(unpack_id >= offset);
		if (unpack_id >= offset + num_desc) {
			offset += num_desc;
			continue;
		}

		return object_cons_descendant_type(
				vm, def->params[i].type,
				unpack_id - offset, out);
	}

	return -1;
}

int
object_inst_order(
		struct vm *vm, struct object_inst *inst,
		struct object_inst_extra_expr *extra_exprs, size_t num_extra_exprs,
		struct object_inst_bind       *extra_binds, size_t num_extra_binds,
		struct object_inst_action **out_actions, size_t *out_num_actions)
{
	panic("TODO: object_inst_order");

	return -1;
}
