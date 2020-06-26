#ifndef STAGE_OBJSTORE_H
#define STAGE_OBJSTORE_H

#include "intdef.h"
#include "atom.h"
#include "arena.h"
#include "utils.h"
#include "errors.h"

typedef unsigned int obj_id;
typedef uint64_t type_id;
typedef uint32_t modtype_id;
typedef uint64_t func_id;
typedef uint32_t modfunc_id;
typedef uint32_t stg_mod_id;
typedef size_t template_id;

#define TYPE_ID(modid, typeid) (((type_id)modid << 32) | ((type_id)typeid & 0xffffffff))
#define TYPE_ID_MOD(typeid) (stg_mod_id)((typeid >> 32) & 0xffffffff)
#define TYPE_ID_TYPE(typeid) (modtype_id)(typeid & 0xffffffff)

#define FUNC_ID(modid, funcid) (((func_id)modid << 32) | ((func_id)funcid & 0xffffffff))
#define FUNC_ID_MOD(funcid) (stg_mod_id)((funcid >> 32) & 0xffffffff)
#define FUNC_ID_LOCAL(funcid) (modfunc_id)(funcid & 0xffffffff)

#define FUNC_UNSET ((func_id)0)

struct vm;
struct type;
struct scope;
struct object;
struct objstore;
struct stg_exec;
struct stg_module;
struct object_cons;

// TYPE_UNSET
#define TYPE_SUBTYPES_END ((type_id)0)

typedef struct string (*type_repr)(struct vm *vm, struct arena *mem, struct type *);
typedef struct string (*obj_repr)(struct vm *vm, struct arena *mem, struct object *);

// obj_data is a pointer to the new object's data. This function is expected to
// modify obj_data such that the old object and all related heap memory can be
// freed without corrupting this object.
typedef void (*obj_copy)(struct stg_exec *, void *type_data, void *obj_data);
typedef bool (*obj_equals_func)(struct vm *, void *type_data,
		void *lhs_obj_data, void *rhs_obj_data);
typedef bool (*type_equals_func)(struct vm *vm, struct type *lhs, struct type *rhs);
typedef void (*type_free)(struct vm *vm, struct type *type);

struct object {
	type_id type;
	void *data;
};

struct type_base {
	struct string name;
	type_repr repr;
	obj_repr obj_repr;
	obj_copy obj_copy;
	type_free free;
	type_equals_func equals;
	obj_equals_func obj_equals;

	struct ast_array_def *array_def;
};

typedef struct _ffi_type ffi_type;

struct type {
	struct atom *name;
	struct type_base *base;
	struct object_inst *obj_inst;
	struct object_cons *type_def;
	struct object static_object;
	ffi_type *ffi_type;
	void *data;

	size_t size;
};

struct type
_init_plain_type(struct type_base *, struct atom *name, size_t size);

#define init_plain_type(base, name, datatype) \
	_init_plain_type(base, name, sizeof(datatype))

bool
type_equals(struct vm *, type_id lhs, type_id rhs);

void
_assert_type_equals_failed(struct vm *, type_id lhs, type_id rhs,
		const char *file, int line, const char *func);

#define assert_type_equals(vm, lhs, rhs) \
	do { if (!type_equals((vm), (lhs), (rhs))) { \
		_assert_type_equals_failed((vm), (lhs), (rhs), __FILE__, __LINE__, __func__); \
	} } while (0)

bool
obj_equals(struct vm *, struct object lhs, struct object rhs);

enum func_kind {
	FUNC_NATIVE,
	FUNC_CONS,
	FUNC_BYTECODE,
};

enum func_flags {
	FUNC_IMPURE  = 0x1,
	FUNC_HEAP    = 0x2,
	FUNC_CLOSURE = 0x4,
	FUNC_REFS    = 0x8,
};

typedef void (*native_ref_func)(void **args, size_t num_args, void *ret);

struct bc_env;

struct func {
	struct atom *name;
	enum func_kind kind;
	enum func_flags flags;
	type_id type;

	union {
		void *native;
		struct object_cons *cons;
		struct bc_env *bytecode;
	};
};

struct stg_func_object {
	func_id func;
	void *closure;
};

struct objstore {
	size_t page_size;
	stg_mod_id mod_id;

	struct arena data;
	struct paged_list types;
	struct paged_list funcs;

	/*
	// TODO: Better data structure?
	struct type *types;
	size_t num_types;
	*/

	/*
	// TODO: Better data structure?
	struct func *funcs;
	size_t num_funcs;
	*/
};

void
objstore_init(struct objstore *store, stg_mod_id mod_id, struct stg_memory *mem);

modtype_id
store_register_type(struct objstore *store, struct type type);

modfunc_id
store_register_func(struct objstore *store, struct func func);

type_id
func_return_type(struct vm *, type_id func_type);

type_id
func_inst_return_type(struct vm *, func_id func);

size_t
func_num_params(struct vm *, type_id func_type);

type_id
func_param_type(struct vm *, type_id func_type, size_t param_i);

void print_type_repr(struct vm *vm, struct type *);
void print_type_id_repr(struct vm *vm, type_id);
void print_obj_repr(struct vm *vm, struct object);

struct string
type_repr_to_string(struct vm *vm, struct arena *, struct type *type);

struct string
obj_repr_to_string(struct vm *vm, struct arena *, struct object obj);

struct string
type_repr_to_alloced_string(struct vm *vm, struct type *type);

struct string
obj_repr_to_alloced_string(struct vm *vm, struct object obj);

struct object
register_object(struct vm *, struct objstore *store, struct object obj);

static inline struct type *store_get_type(struct objstore *store, type_id id) {
	return paged_list_get(&store->types, id);
}

static inline struct func *store_get_func(struct objstore *store, func_id id) {
	return paged_list_get(&store->funcs, id);
}

struct ast_context;

// TODO: Compile time hooks should take vm and stg_error_context instead of
// ast_context.
typedef int (*object_ct_pack_func)(
		struct ast_context *, struct stg_module *mod, struct stg_exec *,
		void *data, void *out, void **params, size_t num_params);

typedef type_id (*object_ct_pack_type_func)(
		struct ast_context *, struct stg_module *mod,
		void *data, void **params, size_t num_params);

typedef int (*object_ct_unpack_func)(
		struct ast_context *, struct stg_module *mod, struct stg_exec *,
		void *data, void *out, struct object obj, int param_id);

typedef void (*object_pack_func)(
		struct vm *, struct stg_exec *, void *data, void *out,
		void **params, size_t num_params);

typedef type_id (*object_pack_type_func)(
		struct vm *, void *data, void **params, size_t num_params);

typedef void (*object_unpack_func)(
		struct vm *, struct stg_exec *, void *data, void *out, void *obj, int param_id);

typedef bool (*object_can_unpack_func)(
		struct vm *, void *data, void *obj);

typedef int32_t ast_slot_id;
struct ast_env;

typedef void (*object_impose_constraints)(
		struct ast_context *, struct stg_module *,
		void *data, struct ast_env *,
		ast_slot_id ret_type_slot, ast_slot_id *param_slots);

struct object_cons_param {
	struct atom *name;
	type_id type;
	struct stg_location def_loc;
};

struct object_cons_base {
	struct string name;
};

struct object_cons {
	struct object_cons_param *params;
	size_t num_params;

	// Runtime or compile time
	object_pack_func pack;
	object_pack_type_func pack_type;
	object_unpack_func unpack;
	object_can_unpack_func can_unpack;

	// Compile time
	object_ct_pack_func ct_pack;
	object_ct_unpack_func ct_unpack;
	object_ct_pack_type_func ct_pack_type;
	object_impose_constraints impose_constraints;

	struct object_cons_base *base;
	void *data;
};

struct object_inst_bind {
	// Pre-order index of the cons' descendant that is the target of this bind.
	size_t target_id;

	// Pre-order index of the expression's member that is the value of this
	// bind.
	size_t unpack_id;

	// The object inst's expression that is the value of this bind.
	size_t expr_id;

	bool overridable;

	struct stg_location loc;
};

enum object_inst_dep_kind {
	OBJECT_INST_DEP_MEMBER,
	OBJECT_INST_DEP_INIT_EXPR,
};

struct object_inst_dep {
	enum object_inst_dep_kind kind;
	union {
		size_t member;
		size_t init_expr;
	};
};

struct object_inst_expr {
	bool constant;
	union {
		func_id func;
		struct object const_value;
	};

	// A list of members that must be evaluated before this expression.
	struct object_inst_dep *deps;
	size_t num_deps;

	bool is_init_expr;
	size_t init_id;

	struct stg_location loc;
};

struct object_inst {
	struct object_cons *cons;
	type_id type;

	bool init_monad;

	struct object_inst_bind *binds;
	size_t num_binds;

	struct object_inst_expr *exprs;
	size_t num_exprs;
};

void
object_cons_print(struct vm *, struct object_cons *);

void
object_inst_print(struct vm *, struct object_inst *);

ssize_t
object_cons_find_param(
		struct object_cons *cons,
		struct atom *name);

ssize_t
object_cons_find_param_unpack_id(
		struct vm *,
		struct object_cons *cons,
		struct atom *name);

// Returns a unpack id for the member pointed to by lookup, or -1 if the lookup
// failed.
ssize_t
object_cons_simple_lookup(
		struct vm *,
		type_id type,
		struct string lookup);

size_t
object_cons_num_descendants(
		struct vm *, struct object_cons *);

// out_local_descendent_ids is expected to be an array of length
// cons->num_params. Note that the parent member has ID 0, so the first
// parameter will have ID 1.
void
object_cons_local_descendent_ids(
		struct vm *, struct object_cons *cons,
		int *out_local_descendent_ids);

// Out descs is expected to be an array capable of holding all the type's
// inst's descendents. The top object itself will be at out_descs[0], but will
// onl have its type filled.
ssize_t
object_cons_all_descendants(
		struct vm *, type_id top_type,
		struct object_cons_param *out_descs, size_t out_descs_size);

// out->type is expected to point to the final type of the requested object,
// and out->data should point to a buffer with sufficient space to store such
// an object.
int
object_unpack(
		struct vm *, struct stg_exec *,
		struct object obj, size_t unpack_id,
		struct object *out);

int
object_ct_pack(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, struct object_cons *cons,
		void *args, size_t num_args, struct object *out);

int
object_ct_pack_type(
		struct ast_context *ctx, struct stg_module *mod,
		struct object_cons *cons, void *args, size_t num_args,
		type_id *out);

int
object_ct_unpack_param(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, struct object_cons *cons,
		struct object obj, size_t param_id, struct object *out);

int
object_cons_descendant_type(
		struct vm *, type_id type,
		size_t unpack_id, type_id *out);

enum object_inst_action_op {
	OBJ_INST_EXPR,
	OBJ_INST_INIT_EXPR,
	OBJ_INST_BIND,
	OBJ_INST_PACK,
};

struct object_inst_action {
	enum object_inst_action_op op;
	union {
		struct {
			int id;

			struct object_inst_dep *deps;
			size_t num_deps;
		} expr;

		struct {
			int id;

			struct object_inst_dep *deps;
			size_t num_deps;
		} init_expr;

		struct {
			int expr_id;
			int member_id;
			int unpack_id;
		} bind;

		struct {
			int member_id;
		} pack;
	};
};

struct object_inst_extra_expr {
	type_id type;

	struct object_inst_dep *deps;
	size_t num_deps;

	struct stg_location loc;
};

// TODO: More descriptive name.
// The expressions in extra_expressions will have ids from inst->num_exprs to
// inst->num_exprs+num_extra_exprs.
int
object_inst_order(
		struct vm *vm, struct stg_error_context *, struct object_inst *inst,
		struct object_inst_extra_expr *extra_exprs, size_t num_extra_exprs,
		struct object_inst_bind       *extra_binds, size_t num_extra_binds,
		struct object_inst_action **out_actions, size_t *out_num_actions,
		struct stg_location inst_loc);

int
stg_instantiate_static_object(
		struct ast_context *ctx, struct stg_module *mod,
		type_id type, struct object *out);

struct stg_exec {
	struct vm *vm;

	struct objstore *store;
	struct arena *heap;
};

void *
stg_alloc(struct stg_exec *, size_t nmemb, size_t size);

struct string
stg_exec_copy_string(struct stg_exec *, struct string);

void free_objstore(struct objstore *store);

void
arena_string_append_type_repr(struct string *str, struct vm *vm,
							  struct arena *mem, struct type *type);

void
arena_string_append_obj_repr(struct string *str, struct vm *vm,
							 struct arena *mem, struct object *object);

#endif
