#ifndef STAGE_NATIVE_H
#define STAGE_NATIVE_H

#include "string.h"
#include "objstore.h"

enum stg_native_func_flags {
	STG_NATIVE_FUNC_IMPURE = 0x1,

	// If true, the function expects to receive a `struct stg_exec *` ast its
	// first argument.
	STG_NATIVE_FUNC_HEAP = 0x2,

	// If true, the function expects to receive a `struct stg_module *` ast its
	// first argument (second if STG_NATIVE_FUNC_HEAP also is set).
	STG_NATIVE_FUNC_MODULE_CLOSURE = 0x4,

	// If true, this function will be called according to native_ref_func
	// prototype in objstore.h
	STG_NATIVE_FUNC_REFS = 0x8,
};

struct stg_native_func {
	struct string name;
	void *func;

	enum stg_native_func_flags flags;
};

struct stg_native_type {
	struct string name;
	struct type type;
};

struct vm;
struct atom;
struct stg_module;
struct ast_node;
struct ast_context;

struct stg_native_module {
	struct atom *name;
	void *dl_handle;

	struct stg_native_func *funcs;
	size_t num_funcs;

	struct stg_native_type *types;
	size_t num_types;

	// Called before the module is compiled to allow registering native
	// objects.
	int  (*hook_register   )(struct stg_module *);

	// Called before the AST is compiled to allow the module to make changes to
	// the tree.
	int  (*hook_pre_compile)(struct ast_context *,
			struct stg_module *, struct ast_node *mod_root);

	// Called when the module is about to be started, right before and after
	// this module's init monad is called.
	int  (*hook_pre_init   )(struct stg_module *);
	int  (*hook_post_init  )(struct stg_module *);

	// Indicates that the module should start any background processes.
	int  (*hook_start      )(struct stg_module *);
	int  (*hook_stop       )(struct stg_module *);
	void (*hook_destroy    )(struct stg_module *);
};

void
stg_native_register_func(struct stg_native_module *,
		struct string name, void *func,
		enum stg_native_func_flags flags);
#define stg_native_register_funcs(mod, func, flags) \
	stg_native_register_func((mod), STR(#func), (void *)(func), (flags))

void
stg_native_register_type(struct stg_native_module *,
		struct string name, struct type);

struct stg_module_magic {
	size_t magic_size;
	struct string name;
	int(*load)(struct stg_native_module *);
};

#define STAGE_MODULE_MAGIC_FUNC_NAME stage_module_magic
#define STAGE_MODULE_MAGIC_FUNC_NAME_STR "stage_module_magic"

typedef struct stg_module_magic *(*stg_magic_func)(void);

#define STAGE_MODULE(modname, load_func) \
	const struct stg_module_magic *\
	STAGE_MODULE_MAGIC_FUNC_NAME() { \
		static const struct stg_module_magic magic = { \
			.magic_size = sizeof(struct stg_module_magic), \
			.name = STR(#modname), \
			.load = load_func, \
		}; \
		return &magic; \
	} \
	extern int _dc

struct stg_native_module *
stg_native_load_module_ext(struct vm *, struct string name);

void
stg_native_module_destroy(struct stg_native_module *);

#endif
