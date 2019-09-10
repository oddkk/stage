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

struct stg_module {
	unsigned int id;
	struct stg_module_info info;

	struct objstore store;
	struct atom_table *atom_table;
	struct vm *vm;

	struct ast_module mod;

	void *data;
};

struct atom *
mod_atom(struct stg_module *, struct string name);

// Create an atom from a cstr.
#define mod_atoms(mod, str) mod_atom(mod, STR(str))

type_id
stg_register_type(struct stg_module *, struct type);

enum builtin_func_kind {
	STG_BUILTIN_FUNC_PURE,
	STG_BUILTIN_FUNC_IMPURE,
};

struct stg_builtin_type {
	struct string mod;
	struct string name;
	struct string ctype_name;
	size_t size;
};

struct stg_builtin_func_param {
	struct string name;
	struct stg_builtin_type type;
};

typedef void (*vm_builtin_func)(struct vm *, struct exec_stack *, void *);

struct stg_builtin_func {
	enum builtin_func_kind kind;
	struct string name;
	struct string cname;
	vm_builtin_func func;
	struct stg_builtin_func_param *params;
	size_t num_params;
	struct stg_builtin_type ret_type;
};


#include "map.h"

#define _STG_TYPE_CTYPE(ctype, mod, name) ctype
#define STG_TYPE_CTYPE(type) DEFER1(_STG_TYPE_CTYPE) type

#define _STG_TYPE_MOD(ctype, mod, name) mod
#define STG_TYPE_MOD(type) DEFER1(_STG_TYPE_MOD) type

#define _STG_TYPE_NAME(ctype, mod, name) name
#define STG_TYPE_NAME(type) DEFER1(_STG_TYPE_NAME) type

#define _STG_TYPE_DATA(ctype, type_mod, type_name) \
	(struct stg_builtin_type) {					   \
		.mod = STR(type_mod),			     	   \
		.name = STR(type_name),			     	   \
		.ctype_name = STR(#ctype),		     	   \
		.size = sizeof(ctype),			     	   \
	}
#define STG_TYPE_DATA1(type) DEFER1(_STG_TYPE_DATA) type
#define STG_TYPE_DATA(type) EVAL(DEFER1(_STG_TYPE_DATA) type)

#define DEF_TYPE(mod, name, ctype) (ctype, mod, name)




#define EMPTY()
#define DEFER1(m) m EMPTY()

#define _STG_BUILTIN_READ_ARG1(type, arg) \
	STG_TYPE_CTYPE(type) arg; \
	stack_pop(stack, &arg, sizeof(STG_TYPE_CTYPE(type)));

#define _STG_BUILTIN_READ_ARG(arg) DEFER1(_STG_BUILTIN_READ_ARG1) arg

#define _STG_BUILTIN_PARAM1(type, arg) \
	STG_TYPE_CTYPE(type) arg
#define _STG_BUILTIN_PARAM(arg) DEFER1(_STG_BUILTIN_PARAM1) arg

#define _STG_BUILTIN_ARG1(type, arg) arg
#define _STG_BUILTIN_ARG(arg) DEFER1(_STG_BUILTIN_ARG1) arg


#define _STG_BUILTIN_PARAM_INFO1(_type, arg)		\
	{												\
		.name = STR(#arg),							\
		.type = STG_TYPE_DATA1(_type),				\
	}

#define _STG_BUILTIN_PARAM_INFO(arg) DEFER1(_STG_BUILTIN_PARAM_INFO1) arg

#define STG_BUILTIN(_name, _cname, func_kind, ret, ...)					\
	static EVAL(STG_TYPE_CTYPE(ret))									\
		_##_cname(struct vm *vm, struct stg_module *mod, void *data, 	\
				 MAP_LIST(_STG_BUILTIN_PARAM, __VA_ARGS__));			\
	static void _call_##_cname(struct vm *vm,							\
							  struct exec_stack *stack,					\
							  void *data)								\
	{																	\
		MAP(_STG_BUILTIN_READ_ARG, __VA_ARGS__);						\
		EVAL(STG_TYPE_CTYPE(ret)) result =								\
			_##_cname(vm, stack->mod, data,								\
					 MAP_LIST(_STG_BUILTIN_ARG, __VA_ARGS__));			\
		stack_push(stack, &result, sizeof(EVAL(STG_TYPE_CTYPE(ret))));	\
	}																	\
	struct stg_builtin_func_param _##_cname##_params[] = {				\
		MAP_LIST(_STG_BUILTIN_PARAM_INFO, __VA_ARGS__)					\
	};																	\
	struct stg_builtin_func _cname = {									\
		.name = STR(#_name),											\
		.cname = STR(#_cname),											\
		.func = _call_##_cname,											\
		.params = _##_cname##_params,									\
		.num_params = ARRAY_LENGTH(_##_cname##_params),					\
		.ret_type = STG_TYPE_DATA(ret),									\
		.kind = func_kind,												\
	};																	\
	static EVAL(STG_TYPE_CTYPE(ret))									\
		_##_cname(struct vm *vm, struct stg_module *mod, void *data,	\
				 MAP_LIST(_STG_BUILTIN_PARAM, __VA_ARGS__))

#define BUILTIN_PURE(name, cname, ret, ...) \
	STG_BUILTIN(name, cname, STG_BUILTIN_FUNC_PURE, ret, __VA_ARGS__)
#define BUILTIN_IMPURE(name, cname, ret, ...) \
	STG_BUILTIN(name, cname, STG_BUILTIN_FUNC_IMPURE, ret, __VA_ARGS__)

struct object
stg_register_builtin_func_obj(struct stg_module *mod,
							  struct stg_builtin_func func,
							  void *data);

void
stg_register_builtin_func(struct stg_module *mod,
						  struct stg_builtin_func func,
						  void *data);

type_id
stg_register_builtin_type(struct stg_module *mod,
						  struct type_base *base,
						  struct stg_builtin_type type);

type_id
stg_resolve_type(struct vm *vm, struct stg_builtin_type type);

#endif
