#ifndef STAGE_MODULE_H
#define STAGE_MODULE_H

#include "vm.h"
#include "ast.h"
#include "atom.h"
#include "objstore.h"

struct stg_module;

struct stg_module_info {
	struct string name;
	struct {
		unsigned int major;
		unsigned int minor;
	} version;

	void *data;

	int  (*init)(struct ast_context *ctx, struct stg_module *);
	void (*free)(struct stg_module *);

	int  (*start)(struct stg_module *);
};

enum stg_module_lifetime {
	STG_MOD_LIFE_INIT = 0,
	STG_MOD_LIFE_IDLE,
	STG_MOD_LIFE_RUNNING,
	STG_MOD_LIFE_DESTROYED,
};

static inline bool
stg_mod_state_ok(enum stg_module_lifetime state) {
	return
		state == STG_MOD_LIFE_IDLE ||
		state == STG_MOD_LIFE_RUNNING;
}

struct stg_module {
	unsigned int id;
	enum stg_module_lifetime state;

	struct stg_module_info info;

	struct objstore store;
	struct atom_table *atom_table;
	struct vm *vm;

	struct ast_module mod;

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

type_id
stg_register_type(struct stg_module *, struct type);

func_id
stg_register_func(struct stg_module *, struct func);

struct ast_object_def *
stg_create_simple_object_def(struct ast_context *ctx,
		struct ast_module *mod, struct ast_object_def *,
		struct object_decons);

#endif
