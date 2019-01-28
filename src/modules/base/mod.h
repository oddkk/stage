#ifndef STAGE_MOD_BASE_H
#define STAGE_MOD_BASE_H

#include "../../module.h"

#define MOD_BASE "base"

#define STG_TYPE DEF_TYPE(MOD_BASE, "type", type_id)
struct object
obj_register_type(struct vm *, struct objstore *,
						 type_id value);
type_id type_obj_get(struct vm *, struct object obj);


#define STG_INT DEF_TYPE(MOD_BASE, "int", int64_t)
struct object
obj_register_integer(struct vm *, struct objstore *,
							int64_t value);


#define STG_STR DEF_TYPE(MOD_BASE, "str", struct string)
struct object
obj_register_string(struct vm *, struct objstore *,
						   struct string value);


#define STG_BOOL DEF_TYPE(MOD_BASE, "bool", int64_t)



enum type_function_kind {
	TYPE_FUNCTION_GENERIC,
	TYPE_FUNCTION_BUILTIN,
	TYPE_FUNCTION_NATIVE,
};

type_id type_register_function(struct vm *, struct objstore *,
							   struct atom **param_names,
							   type_id *param_types, size_t num_params,
							   type_id ret, enum type_function_kind kind);

struct type_func {
	struct atom **param_names;
	type_id *param_types;
	size_t num_params;

	type_id ret;
};


/* obj_id obj_register_builtin_func_from_tuple(struct vm *, struct objstore *, */
/* 											type_id params, type_id ret_type, */
/* 											vm_builtin_func value, void *data); */

struct object
obj_register_builtin_func(struct vm *, struct objstore *,
								 struct atom **param_names,
								 type_id *params, size_t num_params,
								 type_id ret_type, vm_builtin_func value,
								 void *data);

struct obj_builtin_func_data {
	vm_builtin_func func;
	void *data;
};

struct expr;
struct expr_node;

struct object
obj_register_native_func(struct vm *, struct objstore *,
						 struct expr *expr,
						 struct expr_node *node, type_id);

enum type_native_function_storage {
	NATIVE_FUNC_STORAGE_INSTR,
	NATIVE_FUNC_STORAGE_NODES,
};

struct obj_native_func_data {
	enum type_native_function_storage storage;
	union {
		struct {
			void *data;
			size_t length;
		} instr;
		struct {
			struct expr *expr;
			struct expr_node *node;
		} node;
	};
};


struct type_enum_item {
	struct atom *name;
	type_id type;

	int64_t value;
	struct type_enum *owner;
};

struct type_enum {
	size_t num_items;
	struct type_enum_item *items;

	// Number of bytes, excluding the size of the enum value.
	size_t size;
};


struct type_tuple_item {
	struct atom *name;
	type_id type;
};

struct type_tuple {
	struct atom **names;
	type_id *types;
	size_t num_items;
	bool named;

	// Number of bytes
	size_t size;
};

type_id type_register_enum(struct vm *, struct objstore *,
						   struct type_enum *t);
type_id type_register_named_tuple(struct vm *, struct objstore *,
								  struct type_tuple_item *items, size_t num_items);
type_id type_register_unnamed_tuple(struct vm *, struct objstore *,
									type_id *items, size_t num_items);

#endif
