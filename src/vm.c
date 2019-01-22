#include "vm.h"
#include "utils.h"
#include "expr.h"
#include <stdlib.h>
#include <string.h>

static bool
type_unset_unify(struct vm *vm,
				 type_id lhs, type_id rhs,
				 type_id *out_type)
{
	if (lhs != TYPE_UNSET) {
		*out_type = lhs;
	} else {
		*out_type = rhs;
	}

	return true;
}

static void arena_string_append_type_repr(struct string *str, struct vm *vm,
										  struct arena *mem, struct type *type)
{
	struct string type_repr;

	struct arena tmp_mem = arena_push(mem);
	type_repr = type->base->repr(vm, &tmp_mem, type);
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, str, type_repr);
}

static void arena_string_append_obj_repr(struct string *str, struct vm *vm,
										 struct arena *mem, struct object *object)
{
	struct string repr;
	struct type *type;
	type = &vm->store.types[object->type];

	struct arena tmp_mem = arena_push(mem);
	repr = type->base->obj_repr(vm, &tmp_mem, object);
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, str, repr);
}

static struct string type_obj_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	type_id tid = *(type_id *)obj->data;
	struct type *type = &vm->store.types[tid];
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("type("));
	arena_string_append_type_repr(&res, vm, mem, type);
	arena_string_append(mem, &res, STR(")"));

	return res;
}

static struct string obj_integer_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	int64_t value = *(int64_t *)obj->data;
	return arena_sprintf(mem, "%li", value);
}

static void obj_integer_add(struct vm *vm, struct exec_stack *stack, void *data)
{
	(void)data;
	int64_t lhs, rhs;

	stack_pop(stack, &rhs, sizeof(rhs));
	stack_pop(stack, &lhs, sizeof(lhs));

	int64_t result = lhs + rhs;

	stack_push(stack, &result, sizeof(result));
}

static void obj_integer_sub(struct vm *vm, struct exec_stack *stack, void *data)
{
	(void)data;
	int64_t lhs, rhs;

	stack_pop(stack, &rhs, sizeof(rhs));
	stack_pop(stack, &lhs, sizeof(lhs));

	int64_t result = lhs - rhs;

	stack_push(stack, &result, sizeof(result));
}

static void obj_integer_mul(struct vm *vm, struct exec_stack *stack, void *data)
{
	(void)data;
	int64_t lhs, rhs;

	stack_pop(stack, &lhs, sizeof(lhs));
	stack_pop(stack, &rhs, sizeof(rhs));

	int64_t result = lhs * rhs;

	stack_push(stack, &result, sizeof(result));
}

static void obj_integer_div(struct vm *vm, struct exec_stack *stack, void *data)
{
	(void)data;
	int64_t lhs, rhs;

	stack_pop(stack, &lhs, sizeof(lhs));
	stack_pop(stack, &rhs, sizeof(rhs));

	int64_t result = lhs / rhs;

	stack_push(stack, &result, sizeof(result));
}

static struct string obj_string_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	struct string value = *(struct string *)obj->data;
	return arena_sprintf(mem, "\"%.*s\"", LIT(value));
}

static struct string type_enum_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct type_enum *type_enum = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("Enum {"));

	for (size_t i = 0; i < type_enum->num_items; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		struct type_enum_item *item;
		item = &type_enum->items[i];

		arena_string_append(mem, &res, item->name->name);

		if (item->type != TYPE_NONE) {
			struct type *item_type;
			item_type = &vm->store.types[item->type];
			arena_string_append(mem, &res, STR("("));
			arena_string_append_type_repr(&res, vm, mem, item_type);
			arena_string_append(mem, &res, STR(")"));
		}
	}

	arena_string_append(mem, &res, STR("}"));


	return res;
}


static struct string obj_enum_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	int64_t value = *(int64_t *)obj->data;
	struct type *type = &vm->store.types[obj->type];
	struct type_enum *type_enum = type->data;

	struct string res = arena_string_init(mem);

	if (value < type_enum->num_items) {
		struct type_enum_item *item = &type_enum->items[value];
		arena_string_append(mem, &res, item->name->name);

		if (item->type != TYPE_NONE) {
			struct object item_obj = {0};
			item_obj.type = item->type;
			item_obj.data = (uint8_t *)obj->data + sizeof(int64_t);

			arena_string_append(mem, &res, STR(" "));
			arena_string_append_obj_repr(&res, vm, mem, &item_obj);
		}

	} else {
		arena_string_append(mem, &res, STR("(invalid)"));
	}

	return res;
}

static type_id type_enum_subtypes_iter(struct vm *vm, struct type *type, size_t *iter)
{
	struct type_enum *enum_data = (struct type_enum *)type->data;

	for (; *iter < enum_data->num_items; *iter += 1) {
		if (enum_data->items[*iter].type != TYPE_NONE) {
			return enum_data->items[*iter].type;
		}
	}

	return TYPE_SUBTYPES_END;
}

static struct string type_tuple_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct type_tuple *tuple = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("("));

	for (size_t i = 0; i < tuple->num_items; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		if (tuple->named) {
			arena_string_append(mem, &res, tuple->names[i]->name);
			arena_string_append(mem, &res, STR(": "));
		}

		struct type *item_type;
		item_type = get_type(&vm->store, tuple->types[i]);
		arena_string_append_type_repr(&res, vm, mem, item_type);
	}

	arena_string_append(mem, &res, STR(")"));

	return res;
}

static struct string obj_tuple_repr(struct vm *vm, struct arena *mem, struct object *object)
{
	struct type *type = get_type(&vm->store, object->type);
	struct type_tuple *tuple = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("("));

	size_t offset = 0;

	for (size_t i = 0; i < tuple->num_items; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		if (tuple->named) {
			arena_string_append(mem, &res, tuple->names[i]->name);
			arena_string_append(mem, &res, STR(" = "));
		}

		struct object item_obj = {0};
		item_obj.data = (uint8_t *)object->data + offset;
		item_obj.type = tuple->types[i];

		arena_string_append_obj_repr(&res, vm, mem, &item_obj);

		struct type *item_type;
		item_type = get_type(&vm->store, tuple->types[i]);
		offset += item_type->size;
	}

	arena_string_append(mem, &res, STR(")"));

	return res;
}

static type_id type_tuple_subtypes_iter(struct vm *vm, struct type *type, size_t *iter)
{
	struct type_tuple *tuple = (struct type_tuple *)type->data;

	if (*iter < tuple->num_items) {
		*iter += 1;
		return tuple->types[*iter];
	} else {
		return TYPE_SUBTYPES_END;
	}
}

static struct string type_func_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct type_func *func = type->data;
	struct string res = arena_string_init(mem);

	struct type *ret_type;

	ret_type = get_type(&vm->store, func->ret);

	arena_string_append(mem, &res, STR("("));

	for (size_t i = 0; i < func->num_params; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		if (func->param_names) {
			arena_string_append(mem, &res, func->param_names[i]->name);
			arena_string_append(mem, &res, STR(": "));
		}

		struct type *item_type;
		item_type = get_type(&vm->store, func->param_types[i]);
		arena_string_append_type_repr(&res, vm, mem, item_type);
	}

	arena_string_append(mem, &res, STR(") -> "));
	arena_string_append_type_repr(&res, vm, mem, ret_type);

	return res;
}

static bool type_func_unify(struct vm *vm, type_id lhs, type_id rhs, type_id *out)
{
	struct type *lhs_type = get_type(&vm->store, lhs);
	struct type_func *lhs_func = lhs_type->data;

	struct type *rhs_type = get_type(&vm->store, rhs);
	struct type_func *rhs_func = rhs_type->data;


	if (lhs_func->num_params != rhs_func->num_params) {
		return false;
	}

	size_t num_params = lhs_func->num_params;
	struct atom **param_names = NULL;

	bool lhs_specialized = (lhs_type->base != &vm->default_types.func_base);
	bool rhs_specialized = (rhs_type->base != &vm->default_types.func_base);

	if (lhs_specialized && rhs_specialized) {
		printf("Cannot unify a generic function with a specialized one.");
		return false;
	}

	if (lhs_func->param_names && rhs_func->param_names) {
		bool equal_param_names = true;
		for (size_t i = 0; i < num_params; i++) {
			if (lhs_func->param_names[i] == rhs_func->param_names[i]) {
				equal_param_names = false;
				break;
			}
		}

		if (equal_param_names) {
			param_names = lhs_func->param_names;
		} else {
			if (lhs_specialized) {
				param_names = lhs_func->param_names;
			} else {
				param_names = rhs_func->param_names;
			}
		}

	} else if (lhs_func->param_names) {
		param_names = lhs_func->param_names;
	} else if (rhs_func->param_names) {
		param_names = rhs_func->param_names;
	}

	type_id out_params[num_params];
	// TODO: Avoid creating new a new type if nothing has changed.

	for (size_t i = 0; i < num_params; i++) {
		if (!unify_types(vm,
						 lhs_func->param_types[i],
						 rhs_func->param_types[i],
						 &out_params[i])) {
			return false;
		}
	}

	type_id out_ret;

	if (!unify_types(vm, lhs_func->ret, rhs_func->ret, &out_ret)) {
		return false;
	}

	enum type_function_kind kind;
	kind = TYPE_FUNCTION_GENERIC;


	if (lhs_specialized) {
		if (lhs_type->base == &vm->default_types.builtin_func_base) {
			kind = TYPE_FUNCTION_BUILTIN;
		} else if (lhs_type->base == &vm->default_types.native_func_base) {
			kind = TYPE_FUNCTION_NATIVE;
		}
	} else if (rhs_specialized) {
		if (rhs_type->base == &vm->default_types.builtin_func_base) {
			kind = TYPE_FUNCTION_BUILTIN;
		} else if (rhs_type->base == &vm->default_types.native_func_base) {
			kind = TYPE_FUNCTION_NATIVE;
		}
	}

	*out = type_register_function(vm,param_names, out_params,
								  num_params, out_ret, kind);

	return true;
}

static struct string type_builtin_func_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("builtin "));

	struct string type_repr;

	struct arena tmp_mem = arena_push(mem);
	type_repr = type_func_repr(vm, &tmp_mem, type);
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, &res, type_repr);

	return res;
}


static void obj_eval_builtin_func(struct vm *vm, struct exec_stack *stack, void *data)
{
	struct obj_builtin_func_data func;
	stack_pop(stack, &func, sizeof(struct obj_builtin_func_data));
	func.func(vm, stack, func.data);
}

static struct string type_native_func_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("native "));

	struct string type_repr;

	struct arena tmp_mem = arena_push(mem);
	type_repr = type_func_repr(vm, &tmp_mem, type);
	arena_pop(mem, tmp_mem);

	arena_string_append(mem, &res, type_repr);

	return res;
}

static void obj_eval_native_func(struct vm *vm, struct exec_stack *stack, void *data)
{
	struct obj_native_func_data func;
	stack_pop(stack, &func, sizeof(struct obj_native_func_data));
	switch (func.storage) {

	case NATIVE_FUNC_STORAGE_INSTR:
		vm_exec(vm, stack, func.instr.data, func.instr.length);
		break;

	case NATIVE_FUNC_STORAGE_NODES:
		expr_eval(vm, stack, func.node, NULL);
		break;
	}
}

static struct string obj_native_func_repr(struct vm *vm, struct arena *mem, struct object *object)
{
	struct type *type = get_type(&vm->store, object->type);
	return type_native_func_repr(vm, mem, type);
}

static struct string obj_builtin_func_repr(struct vm *vm, struct arena *mem, struct object *object)
{
	struct type *type = get_type(&vm->store, object->type);
	return type_builtin_func_repr(vm, mem, type);
}

static type_id type_func_subtypes_iter(struct vm *vm, struct type *type, size_t *iter)
{
	struct type_func *func = (struct type_func *)type->data;

	type_id result;

	if (*iter < func->num_params) {
		result = func->param_types[*iter];
	} else if (*iter == func->num_params) {
		result = func->ret;
	} else {
		result = TYPE_SUBTYPES_END;
	}

	*iter += 1;

	return result;
}

static void debug_print_integer(struct vm *vm, struct exec_stack *stack, void *data)
{
	int64_t value;
	stack_pop(stack, &value, sizeof(int64_t));

	printf("debug print: %li\n", value);
}

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

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("unset"));

		struct type unset = {0};
		unset.name = atom_create(&vm->atom_table, STR("unset"));
		unset.base = base;
		unset.size = 0;
		vm->default_types.unset = register_type(&vm->store, unset);
		type_base_register_unifier(base, NULL, type_unset_unify);

		assert(vm->default_types.unset == TYPE_UNSET);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("none"));

		struct type none = {0};
		none.name = atom_create(&vm->atom_table, STR("none"));
		none.base = base;
		none.size = 0;
		vm->default_types.none = register_type(&vm->store, none);
		assert(vm->default_types.none == TYPE_NONE);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("scope"));

		struct type scope = {0};
		scope.name = atom_create(&vm->atom_table, STR("scope"));
		scope.base = base;
		scope.size = sizeof(struct scope *);
		vm->default_types.scope = register_type(&vm->store, scope);
		assert(vm->default_types.scope == TYPE_SCOPE);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("template_param"));

		struct type template_param = {0};
		template_param.name = atom_create(&vm->atom_table, STR("template_param"));
		template_param.base = base;
		template_param.size = 0;
		template_param.num_template_params = 1;
		vm->default_types.template_param = register_type(&vm->store, template_param);
		assert(vm->default_types.template_param == TYPE_TEMPLATE_PARAM);
	}


	{
		struct object unset = {0};
		obj_id unset_id;

		unset.type = TYPE_UNSET;
		unset_id = register_object(&vm->store, unset);
		assert(unset_id == OBJ_UNSET);
	}

	{
		struct object none = {0};
		obj_id none_id;

		none.type = TYPE_NONE;
		none_id = register_object(&vm->store, none);
		assert(none_id == OBJ_NONE);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("type"));
		base->obj_repr = type_obj_repr;

		struct type type = {0};
		type.name = atom_create(&vm->atom_table, STR("type"));
		type.base = base;
		type.size = sizeof(type_id);
		vm->default_types.type = register_type(&vm->store, type);

		obj_id obj = obj_register_type(vm, vm->default_types.type);
		scope_insert(&vm->root_scope, type.name,
					 SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, obj), NULL);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("int"));
		base->obj_repr = obj_integer_repr;

		struct type integer = {0};
		integer.name = atom_create(&vm->atom_table, STR("int"));
		integer.base = base;
		integer.size = sizeof(int64_t);
		vm->default_types.integer = register_type(&vm->store, integer);

		obj_id obj = obj_register_type(vm, vm->default_types.integer);
		struct atom *name = atom_create(&vm->atom_table, STR("int"));
		scope_insert(&vm->root_scope, name,
					 SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, obj), NULL);
	}

	{
		struct type_base *base = calloc(1, sizeof(struct type_base));
		type_base_init(base, STR("str"));
		base->obj_repr = obj_string_repr;

		struct type string = {0};
		string.name = atom_create(&vm->atom_table, STR("str"));
		string.base = base;
		string.size = sizeof(struct string);
		vm->default_types.string = register_type(&vm->store, string);

		obj_id obj = obj_register_type(vm, vm->default_types.string);
		struct atom *name = atom_create(&vm->atom_table, STR("str"));
		scope_insert(&vm->root_scope, name,
					 SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, obj), NULL);
	}

	{
		struct type_base *base = &vm->default_types.func_base;
		type_base_init(base, STR("func"));
		base->repr = type_func_repr;
		base->subtypes_iter = type_func_subtypes_iter;
		base->abstract = true;
		type_base_register_unifier(base, base, type_func_unify);
	}

	{
		struct type_base *base = &vm->default_types.builtin_func_base;
		type_base_init(base, STR("builtin_func"));
		base->repr = type_builtin_func_repr;
		base->obj_repr = obj_builtin_func_repr;
		base->subtypes_iter = type_func_subtypes_iter;
		base->eval = obj_eval_builtin_func;
		type_base_register_unifier(base, base, type_func_unify);

		type_base_register_unifier(&vm->default_types.func_base,
								  base, type_func_unify);
	}

	{
		struct type_base *base = &vm->default_types.native_func_base;
		type_base_init(base, STR("native_func"));
		base->repr = type_native_func_repr;
		base->obj_repr = obj_native_func_repr;
		base->subtypes_iter = type_func_subtypes_iter;
		base->eval = obj_eval_native_func;
		type_base_register_unifier(base, base, type_func_unify);

		type_base_register_unifier(&vm->default_types.func_base,
								  base, type_func_unify);
	}


	{
		struct type_base *base = &vm->default_types.enum_base;
		type_base_init(base, STR("enum"));
		base->repr = type_enum_repr;
		base->obj_repr = obj_enum_repr;
		base->subtypes_iter = type_enum_subtypes_iter;
	}

	{
		struct type_base *base = &vm->default_types.tuple_base;
		type_base_init(base, STR("tuple"));
		base->repr = type_tuple_repr;
		base->obj_repr = obj_tuple_repr;
		base->subtypes_iter = type_tuple_subtypes_iter;
	}

	{
		vm->default_types.func_template_return
			= type_register_function(vm, NULL, NULL, 0,
									 TYPE_TEMPLATE_PARAM,
									 TYPE_FUNCTION_GENERIC);
	}


	{
		struct atom *param_names[] = {
			atom_create(&vm->atom_table, STR("lhs")),
			atom_create(&vm->atom_table, STR("rhs")),
		};

		type_id param_types[] = {
			vm->default_types.integer,
			vm->default_types.integer,
		};

		size_t num_params = 2;

		{
			obj_id func;
			func = obj_register_builtin_func(vm, param_names, param_types, num_params,
											 vm->default_types.integer,
											 obj_integer_add, NULL);

			struct atom *name = atom_create(&vm->atom_table, STR("op+"));
			scope_insert_overloadable(&vm->root_scope, name,
									  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
		}

		{
			obj_id func;
			func = obj_register_builtin_func(vm, param_names, param_types, num_params,
											 vm->default_types.integer,
											 obj_integer_sub, NULL);

			struct atom *name = atom_create(&vm->atom_table, STR("op-"));
			scope_insert_overloadable(&vm->root_scope, name,
									  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
		}

		{
			obj_id func;
			func = obj_register_builtin_func(vm, param_names, param_types, num_params,
											 vm->default_types.integer,
											 obj_integer_mul, NULL);

			struct atom *name = atom_create(&vm->atom_table, STR("op*"));
			scope_insert_overloadable(&vm->root_scope, name,
									  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
		}

		{
			obj_id func;
			func = obj_register_builtin_func(vm, param_names, param_types, num_params,
											 vm->default_types.integer,
											 obj_integer_div, NULL);

			struct atom *name = atom_create(&vm->atom_table, STR("op/"));
			scope_insert_overloadable(&vm->root_scope, name,
									  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
		}
	}

	{
		struct atom *param_names[] = {
			atom_create(&vm->atom_table, STR("value")),
		};

		type_id param_types[] = {
			vm->default_types.integer,
		};

		size_t num_params = 1;

		obj_id func;
		func = obj_register_builtin_func(vm, param_names, param_types, num_params,
										 vm->default_types.integer,
										 debug_print_integer, NULL);

		struct atom *name = atom_create(&vm->atom_table, STR("print"));
		scope_insert_overloadable(&vm->root_scope, name,
								  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
	}

	return 0;
}

type_id type_register_enum(struct vm *vm, struct type_enum *t)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("enum"));
	type.base = &vm->default_types.enum_base;
	type.data = (void *)t;

	size_t size = 0;
	int num_template_params = 0;
	for (size_t i = 0; i < t->num_items; i++) {
		struct type_enum_item *itm = &t->items[i];

		itm->value = i;
		itm->owner = t;

		struct type *item_type = &vm->store.types[itm->type];
		num_template_params += item_type->num_template_params;

		size += item_type->size;
	}

	type.num_template_params = num_template_params;
	t->size = size;

	return register_type(&vm->store, type);
}

struct tuple_member_data {
	uint16_t member_size;
	uint16_t tuple_size;
	uint16_t offset;
};

static_assert(sizeof(struct tuple_member_data) <= sizeof(void *),
			  "For now, tuple member data should fit in a void *.");

static void tuple_access_member(struct vm *vm, struct exec_stack *stack, void *data)
{
	struct tuple_member_data tuple_member = *(struct tuple_member_data *)&data;
	uint8_t buffer[tuple_member.tuple_size];

	stack_pop(stack, buffer, tuple_member.tuple_size);

	stack_push(stack, &buffer[tuple_member.offset], tuple_member.member_size);
}

type_id type_register_named_tuple(struct vm *vm, struct type_tuple_item *items, size_t num_items)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("tuple"));
	type.base = &vm->default_types.tuple_base;

	struct type_tuple *tuple = arena_alloc(&vm->memory, sizeof(struct type_tuple));

	tuple->names = arena_alloc(&vm->memory, num_items * sizeof(struct atom *));
	tuple->types = arena_alloc(&vm->memory, num_items * sizeof(type_id));
	tuple->num_items = num_items;
	tuple->named = true;

	size_t size = 0;
	bool num_template_params = 0;

	for (size_t i = 0; i < num_items; i++) {
		tuple->names[i] = items[i].name;
		tuple->types[i] = items[i].type;

		assert(items[i].type < vm->store.num_types);
		struct type *subtype = &vm->store.types[items[i].type];

		size += subtype->size;
		num_template_params += num_template_params;

		// @TODO: Check for duplicate members.
	}

	type_id result;

	type.num_template_params = num_template_params;
	type.size = size;
	type.data = (void *)tuple;

	struct scope *obj_scope = NULL;

	if (num_template_params == 0) {
		obj_scope = scope_push(&vm->root_scope);
		type.object_scope = obj_scope;
	}

	result = register_type(&vm->store, type);

	if (obj_scope) {
		size_t offset = 0;

		for (size_t i = 0; i < num_items; i++) {
			obj_id access_func;

			struct atom *param_name;
			type_id param_type;
			param_name = atom_create(&vm->atom_table, STR("self"));
			param_type = result;

			type_id ret_type = items[i].type;

			struct type *subtype = get_type(&vm->store, items[i].type);
			struct tuple_member_data data = {0};

			data.tuple_size = type.size;
			data.member_size = subtype->size;
			data.offset = offset;

			access_func =
				obj_register_builtin_func(vm, &param_name, &param_type, 1,
										  ret_type, tuple_access_member,
										  *(void **)&data);

			int err;
			err = scope_insert(obj_scope,
							   items[i].name,
							   SCOPE_ANCHOR_ABSOLUTE,
							   get_object(&vm->store, access_func),
							   NULL);

			if (err < 0) {
				printf("Tuple has duplicate member '%.*s'.",
					   ALIT(items[i].name));
				// @TODO: Deallocate
				return TYPE_NONE;
			}

			offset += subtype->size;
		}
	}

	return result;
}

type_id type_register_unnamed_tuple(struct vm *vm, type_id *items, size_t num_items)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("tuple"));
	type.base = &vm->default_types.tuple_base;

	struct type_tuple *tuple = arena_alloc(&vm->memory, sizeof(struct type_tuple));

	tuple->types = arena_alloc(&vm->memory, num_items * sizeof(type_id));
	tuple->num_items = num_items;
	tuple->named = false;

	size_t size = 0;
	bool num_template_params = 0;

	for (size_t i = 0; i < num_items; i++) {
		tuple->types[i] = items[i];

		assert(items[i] < vm->store.num_types);
		struct type *subtype = &vm->store.types[items[i]];

		size += subtype->size;
		num_template_params += num_template_params;
	}

	type.num_template_params = num_template_params;
	type.size = size;
	type.data = (void *)tuple;

	return register_type(&vm->store, type);
}


type_id type_register_function(struct vm *vm, struct atom **param_names,
							   type_id *param_types, size_t num_params,
							   type_id ret, enum type_function_kind kind)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("function"));

	switch (kind) {
	case TYPE_FUNCTION_GENERIC:
		type.size = 0;
		type.base = &vm->default_types.func_base;
		break;

	case TYPE_FUNCTION_BUILTIN:
		type.size = sizeof(struct obj_builtin_func_data);
		type.base = &vm->default_types.builtin_func_base;
		break;

	case TYPE_FUNCTION_NATIVE:
		type.size = sizeof(struct obj_native_func_data);
		type.base = &vm->default_types.native_func_base;
		break;
	}

	struct type_func *data;
	data = calloc(1, sizeof(struct type_func));

	if (param_names) {
		data->param_names = calloc(num_params, sizeof(struct atom *));
		memcpy(data->param_names, param_names, num_params * sizeof(struct atom *));
	}

	data->param_types = calloc(num_params, sizeof(type_id));
	memcpy(data->param_types, param_types, num_params * sizeof(type_id));

	data->num_params = num_params;
	data->ret = ret;

	type.data = data;

	type.num_template_params = 0;

	for (size_t i = 0; i < data->num_params; i++) {
		struct type *param_type = get_type(&vm->store, data->param_types[i]);

		type.num_template_params += param_type->num_template_params;
	}

	struct type *ret_type = get_type(&vm->store, ret);
	type.num_template_params += ret_type->num_template_params;

	return register_type(&vm->store, type);
}

obj_id obj_register_string(struct vm *vm, struct string value)
{
	struct object result = {0};

	result.type = vm->default_types.string;
	result.data = &value;

	return register_object(&vm->store, result);
}

obj_id obj_register_integer(struct vm *vm, int64_t value)
{
	struct object result = {0};

	result.type = vm->default_types.integer;
	result.data = &value;

	return register_object(&vm->store, result);
}

obj_id obj_register_type(struct vm *vm, type_id value)
{
	struct object result = {0};

	result.type = vm->default_types.type;
	result.data = &value;

	return register_object(&vm->store, result);
}

obj_id obj_register_builtin_func(struct vm *vm, struct atom **param_names,
								 type_id *params, size_t num_params,
								 type_id ret_type, vm_builtin_func value,
								 void *data)
{
	struct object result = {0};
	struct obj_builtin_func_data obj_data = {0};

	obj_data.func = value;
	obj_data.data = data;

	type_id type = type_register_function(vm, param_names, params,
										  num_params, ret_type,
										  TYPE_FUNCTION_BUILTIN);

	result.type = type;
	result.data = &obj_data;

	return register_object(&vm->store, result);
}

obj_id obj_register_builtin_func_from_tuple(struct vm *vm, type_id params, type_id ret_type,
											vm_builtin_func value, void *data)
{
	struct type *params_type = get_type(&vm->store, params);
	assert(params_type->base == &vm->default_types.tuple_base);
	struct type_tuple *tuple = params_type->data;
	assert(tuple->named == true);

	return obj_register_builtin_func(vm,
									 tuple->names,
									 tuple->types,
									 tuple->num_items,
									 ret_type,
									 value, data);
}

obj_id obj_register_native_func(struct vm *vm, struct atom **param_names,
								type_id *params, size_t num_params,
								type_id ret_type, struct expr_node *node)
{
	struct object result = {0};
	struct obj_native_func_data obj_data = {0};

	obj_data.storage = NATIVE_FUNC_STORAGE_NODES;
	obj_data.node = node;

	type_id type;
	type = type_register_function(vm, param_names, params,
								  num_params, ret_type,
								  TYPE_FUNCTION_NATIVE);

	result.type = type;
	result.data = &obj_data;

	return register_object(&vm->store, result);
}

type_id type_obj_get(struct vm *vm, struct object obj)
{
	assert(obj.type == vm->default_types.type);
	return *(type_id *)obj.data;
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

void stack_push(struct exec_stack *stack, void *src, size_t size)
{
	assert((stack->sp + size) < (stack->memory + stack->cap));
	memcpy(stack->sp, src, size);
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
