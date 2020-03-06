#ifndef STAGE_MODULE_H
#define STAGE_MODULE_H

#include "vm.h"
#include "atom.h"
#include "objstore.h"

struct stg_module;
struct ast_node;

enum stg_module_lifetime {
	STG_MOD_LIFE_PRE_COMPILE = 0,
	STG_MOD_LIFE_COMPILING,
	STG_MOD_LIFE_INIT,
	STG_MOD_LIFE_IDLE,
	STG_MOD_LIFE_RUNNING,
	STG_MOD_LIFE_FAILED,
	STG_MOD_LIFE_DESTROYED,
};

static inline bool
stg_mod_state_ok(enum stg_module_lifetime state) {
	return
		state == STG_MOD_LIFE_IDLE ||
		state == STG_MOD_LIFE_RUNNING;
}

struct stg_module_native_object {
	struct atom *name;
	struct object obj;
};

struct stg_module {
	stg_mod_id id;
	enum stg_module_lifetime state;

	// If a module is pinned it will not be unloaded even if no other modules
	// depend on it.
	bool pin;

	struct atom *name;
	struct string src_dir;

	stg_mod_id *dependencies;
	size_t num_dependencies;

	struct objstore store;
	struct atom_table *atom_table;
	struct vm *vm;

	struct object instance;

	struct stg_native_module *native_mod;

	bool has_native_module_ext;
	struct string native_module_ext;

	struct stg_module_native_object *native_objs;
	size_t num_native_objs;

	void *data;
};

struct object_decons_member {
	struct atom *name;
	type_id type;
	bool ref;
	size_t offset;
};

struct object_decons {
	struct object_decons_member *members;
	size_t num_members;

	type_id target_type;
};


struct atom *
mod_atom(struct stg_module *, struct string name);

// Create an atom from a cstr.
#define mod_atoms(mod, str) mod_atom(mod, STR(str))

void
mod_arena(struct stg_module *mod, struct arena *out);

type_id
stg_register_type(struct stg_module *, struct type);

func_id
stg_register_func(struct stg_module *, struct func);

// The object will be copied to the module's object storage.
void
stg_mod_register_native_object(struct stg_module *,
		struct atom *name, struct object);

int
stg_mod_lookup_native_object(
		struct stg_module *, struct atom *name, struct object *out);

void
stg_mod_register_native_type(struct stg_module *,
		struct atom *name, type_id);

void
stg_mod_register_native_cons(struct stg_module *,
		struct atom *name, struct object_cons *);

struct stg_module *
stg_mod_find_module(struct stg_module *, struct atom *name);

int
stg_mod_invoke_register(struct stg_module *mod);

int
stg_mod_invoke_pre_compile(struct ast_context *ctx,
		struct stg_module *mod, struct ast_node *mod_root);

int
stg_mod_invoke_pre_init(struct stg_module *mod);

int
stg_mod_invoke_post_init(struct stg_module *mod);

int
stg_mod_invoke_start(struct stg_module *mod);

void
stg_mod_invoke_destroy(struct stg_module *mod);

void
stg_module_destroy(struct stg_module *mod);

#endif
