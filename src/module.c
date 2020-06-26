#include "module.h"
#include "str.h"
#include "ast.h"
#include "base/mod.h"
#include "native.h"
#include "dlist.h"
#include <stdlib.h>
#include <string.h>

struct atom *
mod_atom(struct stg_module *mod, struct string name)
{
	return atom_create(&mod->vm->atom_table, name);
}

void
mod_arena(struct stg_module *mod, struct arena *out)
{
	arena_init(out, &mod->vm->mem);
}

struct object
stg_register_object(struct stg_module *mod, struct object obj)
{
	return register_object(mod->vm, &mod->store, obj);
}

type_id
stg_register_type(struct stg_module *mod, struct type t)
{
	modtype_id local_tid;
	local_tid = store_register_type(&mod->store, t);

	type_id tid;
	tid = TYPE_ID(mod->id, local_tid);

	return tid;
}

func_id
stg_register_func(struct stg_module *mod, struct func f)
{
	modfunc_id local_fid;
	local_fid = store_register_func(&mod->store, f);

	func_id fid;
	fid = TYPE_ID(mod->id, local_fid);

	return fid;
}

struct stg_module *
stg_mod_find_module(struct stg_module *mod, struct atom *name)
{
	for (size_t i = 0; i < mod->num_dependencies; i++) {
		struct stg_module *dep;
		dep = vm_get_module_by_id(mod->vm, mod->dependencies[i]);
		if (dep->name == name) {
			return dep;
		}
	}

	return NULL;
}

void
stg_mod_register_native_object(struct stg_module *mod,
		struct atom *name, struct object obj)
{
	for (size_t i = 0; i < mod->num_native_objs; i++) {
		if (mod->native_objs[i].name == name) {
			printf("Module '%.*s' has conflicting native objects named '%.*s'.\n",
					ALIT(mod->name), ALIT(name));
			return;
		}
	}

	struct stg_module_native_object nobj = {0};
	nobj.name = name;
	nobj.obj = register_object(mod->vm, &mod->store, obj);

	dlist_append(
			mod->native_objs,
			mod->num_native_objs,
			&nobj);
}

void
stg_mod_register_native_type(struct stg_module *mod,
		struct atom *name, type_id tid)
{
	struct object obj = {0};
	obj.type = mod->vm->default_types.type;
	obj.data = &tid;
	stg_mod_register_native_object(mod, name, obj);
}

void
stg_mod_register_native_cons(struct stg_module *mod,
		struct atom *name, struct object_cons *cons)
{
	struct object obj = {0};
	obj.type = mod->vm->default_types.cons;
	obj.data = &cons;
	stg_mod_register_native_object(mod, name, obj);
}

int
stg_mod_lookup_native_object(
		struct stg_module *mod, struct atom *name, struct object *out)
{
	for (size_t i = 0; i < mod->num_native_objs; i++) {
		if (mod->native_objs[i].name == name) {
			*out = mod->native_objs[i].obj;
			return 0;
		}
	}

	return -1;
}

int
stg_mod_lookup_member(
		struct stg_module *mod, struct string lookup, struct object *out)
{
	if (mod->instance.type == TYPE_UNSET) {
		return -1;
	}

	ssize_t unpack_id;
	unpack_id = object_cons_simple_lookup(
			mod->vm, mod->instance.type, lookup);
	if (unpack_id < 0) {
		return -2;
	}

	struct object obj = {0};

	int err;
	err = object_cons_descendant_type(
			mod->vm, mod->instance.type,
			unpack_id, &obj.type);
	if (err) {
		return -3;
	}

	struct type *out_type;
	out_type = vm_get_type(mod->vm, obj.type);
	uint8_t buffer[out_type->size];
	memset(buffer, 0, out_type->size);
	obj.data = buffer;

	struct stg_exec heap = {0};
	heap.vm = mod->vm;
	heap.heap = &mod->vm->transient;
	arena_mark cp = arena_checkpoint(heap.heap);

	err = object_unpack(
			mod->vm, &heap, mod->instance,
			unpack_id, &obj);
	if (err) {
		arena_reset(heap.heap, cp);
		return -3;
	}

	*out = register_object(mod->vm, &mod->store, obj);
	arena_reset(heap.heap, cp);

	return 0;
}

int
stg_mod_invoke_register(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_register) {
		return mod->native_mod->hook_register(mod);
	}

	return 0;
}

int
stg_mod_invoke_pre_compile(struct ast_context *ctx,
		struct stg_module *mod, struct ast_node *mod_root)
{
	if (mod->native_mod && mod->native_mod->hook_pre_compile) {
		return mod->native_mod->hook_pre_compile(ctx, mod, mod_root);
	}

	return 0;
}

int
stg_mod_invoke_pre_init(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_pre_init) {
		return mod->native_mod->hook_pre_init(mod);
	}

	return 0;
}

int
stg_mod_invoke_post_init(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_post_init) {
		return mod->native_mod->hook_post_init(mod);
	}

	return 0;
}

int
stg_mod_invoke_start(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_start) {
		return mod->native_mod->hook_start(mod);
	}

	return 0;
}

void
stg_mod_invoke_destroy(struct stg_module *mod)
{
	if (mod->native_mod && mod->native_mod->hook_destroy) {
		mod->native_mod->hook_destroy(mod);
	}
}

void
stg_module_destroy(struct stg_module *mod)
{
	if (mod->native_mod) {
		bool native_module_is_precompiled = false;
		for (size_t i = 0; i < mod->vm->num_precompiled_native_modules; i++) {
			if (mod->vm->precompiled_native_modules[i] == mod->native_mod) {
				native_module_is_precompiled = true;
				break;
			}
		}
		if (!native_module_is_precompiled) {
			stg_native_module_destroy(mod->native_mod);
		}
	}

	free(mod->dependencies);
	free_objstore(&mod->store);
}
