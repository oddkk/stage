#include "vm.h"
#include "utils.h"
#include <stdlib.h>
#include <string.h>

/* static struct string type_unset_repr(struct vm *vm, struct arena *mem, struct object *obj) */
/* { */
/* 	return STR("<unset>"); */
/* } */

/* static struct string type_none_repr(struct vm *vm, struct arena *mem, struct object *obj) */
/* { */
/* 	return STR("<none>"); */
/* } */

/* static struct string type_template_param_repr(struct vm *vm, struct arena *mem, struct object *obj) */
/* { */
/* 	return STR("<template param>"); */
/* } */

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

	stack_pop(stack, &lhs, sizeof(lhs));
	stack_pop(stack, &rhs, sizeof(rhs));

	int64_t result = lhs + rhs;

	stack_push(stack, &result, sizeof(result));
}

static void obj_integer_sub(struct vm *vm, struct exec_stack *stack, void *data)
{
	(void)data;
	int64_t lhs, rhs;

	stack_pop(stack, &lhs, sizeof(lhs));
	stack_pop(stack, &rhs, sizeof(rhs));

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

	struct type *params_type, *ret_type;

	assert(func->params < vm->store.num_types);
	assert(func->ret < vm->store.num_types);

	params_type = &vm->store.types[func->params];
	ret_type = &vm->store.types[func->ret];

	arena_string_append_type_repr(&res, vm, mem, params_type);
	arena_string_append(mem, &res, STR(" -> "));
	arena_string_append_type_repr(&res, vm, mem, ret_type);

	return res;
}

static struct string obj_func_repr(struct vm *vm, struct arena *mem, struct object *object)
{
	struct type *type = get_type(&vm->store, object->type);
	return type_func_repr(vm, mem, type);
}

static type_id type_func_subtypes_iter(struct vm *vm, struct type *type, size_t *iter)
{
	struct type_func *func = (struct type_func *)type->data;

	type_id result;

	switch (*iter) {
	case 0:  result = func->params;
	case 1:  result = func->ret;
	default: result = TYPE_SUBTYPES_END;
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
		base->obj_repr = obj_func_repr;
		base->subtypes_iter = type_func_subtypes_iter;
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
			= type_register_function(vm, TYPE_NONE, TYPE_TEMPLATE_PARAM);
	}


	{
		struct type_tuple_item params[] = {
			{ atom_create(&vm->atom_table, STR("lhs")), vm->default_types.integer },
			{ atom_create(&vm->atom_table, STR("rhs")), vm->default_types.integer },
		};

		type_id param_type = type_register_named_tuple(vm, params, 2);

		{
			obj_id func;
			func = obj_register_builtin_func(vm, param_type, vm->default_types.integer,
											obj_integer_add, NULL);

			struct atom *name = atom_create(&vm->atom_table, STR("op+"));
			scope_insert_overloadable(&vm->root_scope, name,
									  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
		}

		{
			obj_id func;
			func = obj_register_builtin_func(vm, param_type, vm->default_types.integer,
											obj_integer_sub, NULL);

			struct atom *name = atom_create(&vm->atom_table, STR("op-"));
			scope_insert_overloadable(&vm->root_scope, name,
									  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
		}

		{
			obj_id func;
			func = obj_register_builtin_func(vm, param_type, vm->default_types.integer,
											obj_integer_mul, NULL);

			struct atom *name = atom_create(&vm->atom_table, STR("op*"));
			scope_insert_overloadable(&vm->root_scope, name,
									  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
		}

		{
			obj_id func;
			func = obj_register_builtin_func(vm, param_type, vm->default_types.integer,
											obj_integer_div, NULL);

			struct atom *name = atom_create(&vm->atom_table, STR("op/"));
			scope_insert_overloadable(&vm->root_scope, name,
									  SCOPE_ANCHOR_ABSOLUTE, get_object(&vm->store, func));
		}
	}

	{
		struct type_tuple_item params[] = {
			{ atom_create(&vm->atom_table, STR("value")), vm->default_types.integer },
		};

		type_id param_type = type_register_named_tuple(vm, params, 1);
		obj_id func;
		func = obj_register_builtin_func(vm, param_type, vm->default_types.integer,
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
	}

	type.num_template_params = num_template_params;
	type.size = size;
	type.data = (void *)tuple;

	return register_type(&vm->store, type);
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


type_id type_register_function(struct vm *vm, type_id params, type_id ret)
{
	struct type type = {0};

	// TODO: Better names
	type.name = atom_create(&vm->atom_table, STR("function"));
	type.base = &vm->default_types.func_base;
	type.size = sizeof(struct obj_builtin_func_data);

	struct type_func *data;
	data = calloc(1, sizeof(struct type_func));

	data->params = params;
	data->ret = ret;

	type.data = data;

	struct type *params_type = &vm->store.types[params];
	struct type *ret_type = &vm->store.types[ret];

	type.num_template_params =
		params_type->num_template_params + ret_type->num_template_params;

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

obj_id obj_register_builtin_func(struct vm *vm, type_id params, type_id ret_type,
								 vm_builtin_func value, void *data)
{
	struct object result = {0};
	struct obj_builtin_func_data obj_data = {0};

	obj_data.func = value;
	obj_data.data = data;

	type_id type = type_register_function(vm, params, ret_type);

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
