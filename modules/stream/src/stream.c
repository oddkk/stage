#include "mod.h"
#include "stream.h"
#include <module.h>
#include <native.h>
#include <utils.h>
#include <ast.h>
#include <ffi.h>

static struct object_cons *
stream_cons_from_vm(struct vm *vm)
{
	struct stream_mod_info *mod_info;
	mod_info = stream_mod_get_info(vm);

	return mod_info->stream_cons;
}

static bool
stream_type_equals(struct vm *vm, struct type *lhs, struct type *rhs)
{
	struct stream_type_info *lhs_info;
	struct stream_type_info *rhs_info;

	lhs_info = lhs->data;
	rhs_info = rhs->data;

	return lhs_info->freq == rhs_info->freq &&
		type_equals(vm, lhs_info->type, rhs_info->type);
}

static struct string
stream_type_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct stream_type_info *info;
	info = type->data;

	struct string res = {0};
	arena_string_append(mem, &res, STR("Stream["));

	struct type *item_type;
	item_type = vm_get_type(vm, info->type);
	arena_string_append_type_repr(
			&res, vm, mem, item_type);

	arena_string_append_sprintf(mem, &res, ", %uhz]", info->freq);

	return res;
}

void
stream_obj_copy(struct stg_exec *ctx, void *type_data, void *obj_data)
{
	struct stream_data *node;
	node = obj_data;

	*node = stream_copy_stream_data(ctx, *node);
}

static struct type_base stream_type_base = {
	.name = STR("Stream"),
	.equals = stream_type_equals,
	.repr = stream_type_repr,
	.obj_copy = stream_obj_copy,
	.obj_equals = NULL,
};

type_id
stream_register_type(struct stg_module *mod, type_id res_type, freq_t freq);

static int
stream_type_pack(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, void *data, void *out,
		void **params, size_t num_params)
{
	assert(num_params == 2);

	type_id tid  = *(type_id *)params[0];
	freq_t  freq = *(freq_t  *)params[1];

	type_id result_type;
	result_type = stream_register_type(
			mod, tid, freq);

	memcpy(out, &result_type, sizeof(type_id));

	return 0;
}

static type_id
stream_type_pack_type(struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	return ctx->vm->default_types.type;
}

static int
stream_type_unpack(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_exec *heap, void *data, void *out,
		struct object obj, int param_id)
{
	assert_type_equals(ctx->vm,
			obj.type, ctx->vm->default_types.type);

	type_id tid = *(type_id *)obj.data;

	struct type *type;
	type = vm_get_type(ctx->vm, tid);

	if (type->base != &stream_type_base) {
	// TODO: Properly report type mismatch error.
		stg_error(ctx->err, STG_NO_LOC,
				"Expected Stream type, got %.*s.",
				LIT(type->base->name));
		return -1;
	}

	struct stream_type_info *info;
	info = type->data;

	switch (param_id) {
		case 0:
			memcpy(out, &info->type, sizeof(type_id));
			break;

		case 1:
			memcpy(out, &info->freq, sizeof(freq_t));
			break;

		case 2:
			printf("Invalid param id\n");
			return -1;
	}

	return 0;
}

static ffi_type *stream_ffi_type_members[] = {
	&ffi_type_pointer,
	NULL,
};

static ffi_type stream_ffi_type = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = stream_ffi_type_members,
};

type_id
stream_register_type(struct stg_module *mod, type_id res_type, freq_t freq)
{
	struct stream_type_info *info;
	info = arena_alloc(&mod->mem, sizeof(struct stream_type_info));

	info->type = res_type;
	info->freq = freq;

	struct type type = {0};
	type.base = &stream_type_base;
	type.data = info;
	type.size = sizeof(struct stream_data);
	type.type_def = stream_cons_from_vm(mod->vm);
	type.ffi_type = &stream_ffi_type;

	return stg_register_type(mod, type);
}

static struct object_cons *
stream_register_cons(struct stg_module *mod)
{
	struct stream_mod_info *mod_info;
	mod_info = mod->data;

	struct object_cons *cons;
	cons = arena_alloc(&mod->mem,
			sizeof(struct object_cons));

	cons->num_params = 2;
	cons->params = arena_allocn(&mod->mem,
			cons->num_params, sizeof(struct object_cons_param));

	cons->params[0].name = mod_atoms(mod, "T");
	cons->params[0].type = mod->vm->default_types.type;

	cons->params[1].name = mod_atoms(mod, "Freq");
	cons->params[1].type = mod_info->freq;

	cons->ct_pack      = stream_type_pack;
	cons->ct_pack_type = stream_type_pack_type;
	cons->ct_unpack    = stream_type_unpack;

	return cons;
}

bool
stream_type_is_inst(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	return type->base == &stream_type_base;
}

type_id
stream_return_type(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	assert(type->base == &stream_type_base);

	struct stream_type_info *info;
	info = type->data;

	return info->type;
}

type_id
stream_freq(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	assert(type->base == &stream_type_base);

	struct stream_type_info *info;
	info = type->data;

	return info->freq;
}

void
stream_register_stream(struct stg_module *mod)
{
	struct stream_mod_info *mod_info;
	mod_info = mod->data;

	mod_info->stream_cons = stream_register_cons(mod);

	stg_mod_register_native_cons(mod,
			mod_atoms(mod, "Stream"), mod_info->stream_cons);
}

void
stream_mod_load_stream(struct stg_native_module *mod)
{
}

struct stream_data
stream_copy_stream_data(struct stg_exec *ctx, struct stream_data node)
{
	node.node = stream_copy_node_ref(ctx, *node.node);
	return node;
}
