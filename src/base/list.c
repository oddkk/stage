#include "list.h"
#include "mod.h"
#include "../ast.h"
#include "../module.h"
#include "../native.h"
#include <string.h>

#include <ffi.h>

static struct object_cons *
stg_list_cons_from_vm(struct vm *vm)
{
	struct stg_module *base_mod;
	base_mod = vm_get_module(vm, vm_atoms(vm, "base"));
	assert(base_mod);

	struct stg_base_mod_info *mod_info;
	mod_info = base_mod->data;

	return mod_info->list_cons;
}

static ffi_type *stg_list_ffi_type_members[] = {
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_pointer,
	&ffi_type_uint64,
	&ffi_type_uint64,
	NULL
};

static ffi_type stg_list_ffi_type = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = stg_list_ffi_type_members,
};

static void
stg_list_obj_copy(struct stg_exec *ctx, void *type_data, void *obj_data)
{
	struct stg_list_data *list = obj_data;
	stg_list_copy(ctx, list);
}

void
stg_list_copy(struct stg_exec *heap, struct stg_list_data *list)
{
	void *new_data = NULL;
	if (list->data_size > 0) {
		new_data = stg_alloc(heap, list->data_size, 1);
		memcpy(new_data, list->data, list->data_size);
	}

	list->data = new_data;

	if (list->copy) {
		list->copy(heap, list->data);
	}
}

#define CNAME stg_list
#define REAL_NAME "List"
#define UWT_TYPE_INFO_TYPE struct stg_list_type_info
#define UWT_OBJ_COPY_FUNC stg_list_obj_copy
#define UWT_OBJ_FFI_TYPE stg_list_ffi_type
#define UWT_OBJ_DATA_TYPE struct stg_list_data
#define EXPOSE_FUNCS 1
#include "../unary_wrapping_type_template.h"
#undef UWT_TYPE_INFO_TYPE
#undef UWT_OBJ_COPY_FUNC
#undef UWT_OBJ_FFI_TYPE
#undef UWT_OBJ_DATA_TYPE

static int
stg_list_nil_cons_pack(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, void *data, void *out,
		void **params, size_t num_params)
{
	struct stg_list_data *list = out;
	memset(list, 0, sizeof(struct stg_list_data));

	return 0;
}

static type_id
stg_list_nil_cons_pack_type(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	assert(num_params == 1);
	type_id tid = *(type_id *)params[0];

	return stg_list_register_type(mod, tid);
}

static int
stg_list_nil_cons_unpack(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, void *data, void *out,
		struct object obj, int param_id)
{
	if (param_id != 0) {
		return -1;
	}

	*(type_id *)out =
		stg_list_return_type(ctx->vm, obj.type);

	return 0;
}

static void
stg_list_register_list_nil(struct stg_module *mod)
{
	struct object_cons *nil_cons;
	nil_cons = arena_alloc(&mod->mem, sizeof(struct object_cons));
	nil_cons->num_params = 1;
	nil_cons->params = arena_allocn(&mod->mem, 1, sizeof(struct object_cons_param));

	nil_cons->params[0].name = mod_atoms(mod, "T");
	nil_cons->params[0].type = mod->vm->default_types.type;

	nil_cons->ct_pack = stg_list_nil_cons_pack;
	nil_cons->ct_pack_type = stg_list_nil_cons_pack_type;
	nil_cons->ct_unpack = stg_list_nil_cons_unpack;

	stg_mod_register_native_cons(
			mod, mod_atoms(mod, "stg_list_nil"), nil_cons);
}

struct stg_list_cons_func_info {
	type_id element_type;
	size_t element_size;
	type_id list_type;
};

struct stg_list_cons_data {
	size_t element_size;
	obj_copy element_copy;
	void *element_type_data;
	type_id element_type;

	struct stg_list_data tail;
	uint8_t head[];
};

static int
stg_list_cons_head(struct stg_exec *heap,
		struct stg_list_data *list, void *out)
{
	struct stg_list_cons_data *data;
	data = list->data;
	memcpy(out, data->head, data->element_size);

	return 0;
}

static struct stg_list_data
stg_list_cons_tail(struct stg_exec *heap,
		struct stg_list_data *list)
{
	struct stg_list_cons_data *data;
	data = list->data;
	return data->tail;
}

static void
stg_list_cons_copy(struct stg_exec *heap,
		void *list_data)
{
	struct stg_list_cons_data *data;
	data = list_data;
	stg_list_copy(heap, &data->tail);

	// data->list is already copied by being a flexible array member.
	if (data->element_copy) {
		data->element_copy(
				heap, data->element_type_data, data->head);
	}
}

static void
stg_list_cons_func_pack(
		struct vm *vm, struct stg_exec *heap,
		void *data, void *out, void **params, size_t num_params)
{
	struct stg_list_cons_func_info *info = data;

	void *head = params[0];
	struct stg_list_data tail = *(struct stg_list_data *)params[1];

	struct stg_list_data *list = out;
	memset(list, 0, sizeof(struct stg_list_data));

	struct type *element_type;
	element_type = vm_get_type(vm, info->element_type);

	struct stg_list_cons_data *cons_data;
	cons_data = stg_alloc(heap, 1, sizeof(struct stg_list_cons_data) + element_type->size);
	cons_data->element_size = element_type->size;
	cons_data->element_type = info->element_type;
	cons_data->element_type_data = element_type->data;
	cons_data->element_copy = element_type->base->obj_copy;

	cons_data->tail = tail;
	stg_list_copy(heap, &cons_data->tail);
	memcpy(cons_data->head, head, info->element_size);

	// TODO: Do we have to copy during pack?
	// if (cons_data->element_copy) {
	// 	cons_data->element_copy(
	// 			heap, element_type->data, cons_data->head);
	// }

	list->head = stg_list_cons_head;
	list->tail = stg_list_cons_tail;
	list->copy = stg_list_cons_copy;
	list->data = cons_data;
	list->data_size = sizeof(struct stg_list_cons_data) + element_type->size;
	list->element_type = info->element_type;
}

static type_id
stg_list_cons_func_pack_type(
		struct vm *vm, void *data,
		void **params, size_t num_params)
{
	struct stg_list_cons_func_info *info = data;

	return info->list_type;
}

static void
stg_list_cons_func_unpack(
		struct vm *vm, struct stg_exec *heap,
		void *data, void *out,
		void *obj, int param_id)
{
	struct stg_list_data *list = obj;

	switch (param_id) {
		case 0:
			{
				assert(list->head);
				int end;
				end = list->head(heap, list, out);
				assert(!end);
			}
			break;

		case 1:
			{
				struct stg_list_data *out_tail;
				out_tail = out;
				if (list->tail) {
					*out_tail = list->tail(heap, list);
					stg_list_copy(heap, out_tail);
				} else {
					memset(out_tail, 0, sizeof(struct stg_list_data));
				}
			}
			break;

		default:
			panic("Invalid parameter for list cons.");
	}
}

static bool
stg_list_cons_func_can_unpack(
		struct vm *vm, void *data, void *obj)
{
	struct stg_list_cons_func_info *info = data;
	struct stg_list_data *list = obj;

	int end = true;
	if (list->head) {
		uint8_t buffer[info->element_size];
		struct stg_exec tmp_heap = {0};
		tmp_heap.heap = &vm->transient;
		arena_mark cp = arena_checkpoint(tmp_heap.heap);

		end = list->head(&tmp_heap, list, buffer);
		end = !!end;

		arena_reset(tmp_heap.heap, cp);
	}

	return !end;
}

static type_id
stg_list_create_list_cons_func_type(struct stg_module *mod, type_id tid)
{
	type_id list_tid;
	list_tid = stg_list_register_type(
			mod, tid);

	type_id param_types[] = { tid, list_tid };

	return stg_register_func_type(
			mod, list_tid, param_types, ARRAY_LENGTH(param_types));
}

struct object_cons_base stg_list_cons_func_base = {
	.name = STR("cons"),
};

static func_id
stg_list_create_list_cons_func(struct stg_module *mod, type_id element_type_id)
{
	struct object_cons *cons_func;
	cons_func = arena_alloc(&mod->mem, sizeof(struct object_cons));
	cons_func->num_params = 2;
	cons_func->params = arena_allocn(&mod->mem,
			cons_func->num_params, sizeof(struct object_cons_param));

	cons_func->params[0].name = mod_atoms(mod, "head");
	cons_func->params[0].type = element_type_id;

	type_id list_type_id;
	list_type_id = stg_list_register_type(mod, element_type_id);

	cons_func->params[1].name = mod_atoms(mod, "tail");
	cons_func->params[1].type = list_type_id;

	cons_func->pack = stg_list_cons_func_pack;
	cons_func->pack_type = stg_list_cons_func_pack_type;
	cons_func->unpack = stg_list_cons_func_unpack;
	cons_func->can_unpack = stg_list_cons_func_can_unpack;

	cons_func->base = &stg_list_cons_func_base;

	struct type *element_type;
	element_type = vm_get_type(mod->vm, element_type_id);

	struct stg_list_cons_func_info *info;
	info = arena_alloc(&mod->mem, sizeof(struct stg_list_cons_func_info));
	info->element_type = element_type_id;
	info->element_size = element_type->size;
	info->list_type = list_type_id;

	cons_func->data = info;

	type_id func_tid;
	func_tid = stg_list_create_list_cons_func_type(
			mod, element_type_id);

	struct func func = {0};
	func.kind = FUNC_CONS;
	func.type = func_tid;
	func.cons = cons_func;

	return stg_register_func(mod, func);
}

static int
stg_list_cons_cons_pack(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, void *data, void *out,
		void **params, size_t num_params)
{
	struct stg_func_object *func = out;
	memset(func, 0, sizeof(struct stg_list_data));

	assert(num_params == 1);
	type_id tid = *(type_id *)params[0];

	func_id fid;
	fid = stg_list_create_list_cons_func(
			mod, tid);

	func->func = fid;

	return 0;
}

static type_id
stg_list_cons_cons_pack_type(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	assert(num_params == 1);
	type_id tid = *(type_id *)params[0];

	return stg_list_create_list_cons_func_type(
			mod, tid);
}

struct object_cons_base stg_list_cons_cons_base = {
	.name = STR("cons"),
};

static int
stg_list_cons_cons_unpack(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, void *data, void *out,
		struct object obj, int param_id)
{
	if (param_id != 0) {
		return -1;
	}

	assert(stg_type_is_func(ctx->vm, obj.type));
	struct stg_func_object *func_obj = obj.data;

	struct func *func = vm_get_func(ctx->vm, func_obj->func);
	assert(func->kind == FUNC_CONS);
	assert(func->cons->base == &stg_list_cons_func_base);

	struct stg_list_cons_func_info *info;
	info = func->cons->data;

	*(type_id *)out = info->element_type;

	return 0;
}

static void
stg_list_register_list_cons(struct stg_module *mod)
{
	struct object_cons *cons_cons;
	cons_cons = arena_alloc(&mod->mem, sizeof(struct object_cons));
	cons_cons->num_params = 1;
	cons_cons->params = arena_allocn(&mod->mem, 1, sizeof(struct object_cons_param));

	cons_cons->params[0].name = mod_atoms(mod, "T");
	cons_cons->params[0].type = mod->vm->default_types.type;

	cons_cons->ct_pack = stg_list_cons_cons_pack;
	cons_cons->ct_pack_type = stg_list_cons_cons_pack_type;
	cons_cons->ct_unpack = stg_list_cons_cons_unpack;

	cons_cons->base = &stg_list_cons_cons_base;

	stg_mod_register_native_cons(
			mod, mod_atoms(mod, "stg_list_cons"), cons_cons);
}

void
stg_list_register(struct stg_module *mod)
{
	struct stg_base_mod_info *mod_info;
	mod_info = mod->data;

	mod_info->list_cons =
		stg_list_register_cons(mod);
	stg_mod_register_native_cons(
			mod, mod_atoms(mod, "stg_list"), mod_info->list_cons);

	stg_list_register_list_nil(mod);
	stg_list_register_list_cons(mod);
}

void
stg_list_register_native(struct stg_native_module *mod)
{
}

struct stg_list_array_data {
	size_t element_size;
	obj_copy element_copy;
	void *element_type_data;
	type_id element_type;

	void *elements;
	size_t num_elements;
};

static int
stg_list_array_cons_head(struct stg_exec *heap,
		struct stg_list_data *list, void *out)
{
	struct stg_list_array_data *data;
	data = list->data;
	assert(data->num_elements > 0);
	memcpy(out, data->elements, data->element_size);

	return 0;
}

static struct stg_list_data
stg_list_array_cons_tail(struct stg_exec *heap,
		struct stg_list_data *list)
{
	struct stg_list_array_data *data;
	data = list->data;

	struct stg_list_data new_list;
	new_list = *list;
	new_list.data = stg_alloc(heap, 1,
			sizeof(struct stg_list_array_data));
	memcpy(new_list.data, list->data,
			sizeof(struct stg_list_array_data));

	struct stg_list_array_data *array;
	array = new_list.data;

	array->elements = (uint8_t *)array->elements + array->element_size;
	array->num_elements -= 1;

	return new_list;
}

void
stg_list_array_cons_copy(struct stg_exec *heap,
		void *list_data)
{
	struct stg_list_array_data *data;
	data = list_data;

	void *new_array = stg_alloc(heap,
			data->num_elements, data->element_size);
	memcpy(new_array, data->elements, data->num_elements * data->element_size);

	if (data->element_copy) {
		for (size_t i = 0; i < data->num_elements; i++) {
			void *elem = (uint8_t *)data->elements + (i * data->element_size);
			data->element_copy(
					heap, data->element_type_data, elem);
		}
	}
}

struct stg_list_data
stg_list_from_carray(struct vm *vm, struct stg_exec *heap,
		type_id element_type, void *data, size_t num_elements)
{
	struct stg_list_data list = {0};
	list.head = stg_list_array_cons_head;
	list.tail = stg_list_array_cons_tail;
	list.copy = stg_list_array_cons_copy;

	struct type *type;
	type = vm_get_type(vm, element_type);

	struct stg_list_array_data *array_data;
	array_data = stg_alloc(heap, 1, sizeof(struct stg_list_array_data));
	array_data->element_type = element_type;
	array_data->element_size = type->size;
	array_data->element_copy = type->base->obj_copy;
	array_data->element_type_data = type->data;

	array_data->elements = data;
	array_data->num_elements = num_elements;

	list.data = array_data;

	return list;
}
