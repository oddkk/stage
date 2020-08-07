#include "vm.h"
#include "ast.h"
#include "dlist.h"
#include "module.h"
#include "base/mod.h"
#include <stdlib.h>
#include <string.h>
#include <ffi.h>

struct ast_dt_variant
ast_dt_decode_variant(struct stg_type_variant_info *info, void *obj_data)
{
	uint64_t tag;
	void *data;

	switch (info->tag_size) {
		case 1: tag = *(uint8_t  *)obj_data; break;
		case 2: tag = *(uint16_t *)obj_data; break;
		case 4: tag = *(uint32_t *)obj_data; break;
		case 8: tag = *(uint64_t *)obj_data; break;
		default:
			panic("Invalid variant tag size");
			return (struct ast_dt_variant){0};
	}

	assert(tag < info->num_options);

	data = (void *)((uint8_t *)obj_data + info->tag_size);

	struct ast_dt_variant var = {0};
	var.tag = tag;
	var.data.type = info->options[tag].data_type;
	var.data.data = data;

	return var;
}

void
ast_dt_encode_variant(struct stg_type_variant_info *info,
		uint64_t tag, void *in_data, void *out_data)
{
	memset(out_data, 0, info->total_size);
	switch (info->tag_size) {
		case 1: *((uint8_t  *)out_data) = (uint8_t )tag; break;
		case 2: *((uint16_t *)out_data) = (uint16_t)tag; break;
		case 4: *((uint32_t *)out_data) = (uint32_t)tag; break;
		case 8: *((uint64_t *)out_data) = (uint64_t)tag; break;
		default:
			panic("Invalid variant tag size");
			return;
	}

	void *out_data_ptr = ((uint8_t *)out_data) + info->tag_size;
	if (info->options[tag].size) {
		memcpy(out_data_ptr, in_data, info->options[tag].size);
	}
}

static struct string
ast_dt_variant_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct stg_type_variant_info *info = type->data;
	struct string res = {0};

	arena_string_append(mem, &res, STR("variant { "));

	for (size_t i = 0; i < info->num_options; i++) {
		if (i != 0) {
			arena_string_append(mem, &res, STR(", "));
		}

		arena_string_append_sprintf(mem, &res, "%.*s%s", ALIT(info->options[i].name),
				info->options[i].data_type != TYPE_UNSET ? " " : "");

		if (info->options[i].data_type != TYPE_UNSET) {
			struct type *option_type;
			option_type = vm_get_type(vm, info->options[i].data_type);
			arena_string_append_type_repr(&res, vm, mem, option_type);
		}
	}

	arena_string_append(mem, &res, STR(" }"));

	return res;
}

static struct string
ast_dt_variant_obj_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	struct type *type = vm_get_type(vm, obj->type);
	struct stg_type_variant_info *info = type->data;
	struct string res = {0};

	struct ast_dt_variant var;
	var = ast_dt_decode_variant(info, obj->data);

	struct stg_type_variant_option *opt;
	opt = &info->options[var.tag];

	if (opt->data_type != TYPE_UNSET) {
		arena_string_append_sprintf(mem, &res, "%.*s(", ALIT(opt->name));
		arena_string_append_obj_repr(&res, vm, mem, &var.data);
		arena_string_append(mem, &res, STR(")"));
	} else {
		arena_string_append(mem, &res, opt->name->name);
	}

	return res;
}

static void
ast_dt_variant_obj_copy(struct stg_exec *heap, void *type_data, void *obj_data)
{
	struct stg_type_variant_info *info = type_data;
	struct ast_dt_variant var;
	var = ast_dt_decode_variant(info, obj_data);

	struct stg_type_variant_option *opt;
	opt = &info->options[var.tag];

	if (opt->copy) {
		opt->copy(heap,
				opt->type_data,
				var.data.data);
	}
}

struct type_base variant_type_base = {
	.name     = STR("variant"),
	.repr     = ast_dt_variant_repr,
	.obj_repr = ast_dt_variant_obj_repr,
	.obj_copy = ast_dt_variant_obj_copy,

	// TODO: Implement
	// .free = ...,
};

struct ast_dt_variant_cons_closure {
	struct stg_type_variant_info *info;
	uint64_t tag;
};

static void
ast_dt_variant_pack(struct vm *vm, struct stg_exec *heap,
		void *data, void *out, void **args, size_t num_args)
{
	struct ast_dt_variant_cons_closure *closure = data;
	void *arg = args[0];

	ast_dt_encode_variant(
			closure->info, closure->tag,
			arg, out);
}

static void
ast_dt_variant_unpack(struct vm *vm, struct stg_exec *heap,
		void *data, void *out, void *obj, int param_id)
{
	assert(param_id == 0);

	struct ast_dt_variant_cons_closure *closure = data;

	struct ast_dt_variant val;
	val = ast_dt_decode_variant(
			closure->info, obj);
	assert(val.tag == closure->tag);

	memcpy(out, val.data.data,
			closure->info->options[val.tag].size);
}

static bool
ast_dt_variant_can_unpack(struct vm *vm, void *data, void *obj)
{
	struct ast_dt_variant_cons_closure *closure = data;

	struct ast_dt_variant val;
	val = ast_dt_decode_variant(
			closure->info, obj);
	return val.tag == closure->tag;
}

static struct object
ast_dt_create_variant_type_scope(
		struct ast_context *ctx, struct stg_module *mod,
		struct stg_type_variant_info *info)
{
#if 0
	struct arena *mem = &mod->mem;

	struct ast_node *scope;

	scope = ast_init_node_composite(
			ctx, AST_NODE_NEW, info->loc,
			AST_COMPOSITE_STATIC_OBJ);

	struct type *variant_type;
	variant_type = vm_get_type(mod->vm, info->type);

	for (size_t tag = 0; tag < info->num_options; tag++) {
		struct object obj = {0};
		type_id option_type = info->options[tag].data_type;
		if (option_type != TYPE_UNSET) {
			struct func func = {0};
			func.type = stg_register_func_type(
					mod, info->type, &option_type, 1);

			struct ast_dt_variant_cons_closure *closure;
			closure = arena_alloc(mem, sizeof(struct ast_dt_variant_cons_closure));
			closure->tag = tag;
			closure->info = info;

			func.kind = FUNC_CONS;
			func.cons = arena_alloc(mem, sizeof(struct object_cons));
			func.cons->num_params = 1;
			func.cons->params = arena_alloc(mem, sizeof(struct object_cons_param));
			func.cons->params[0].name = NULL;
			func.cons->params[0].type = option_type;
			func.cons->data = closure;
			func.cons->pack = ast_dt_variant_pack;
			func.cons->unpack = ast_dt_variant_unpack;
			func.cons->can_unpack = ast_dt_variant_can_unpack;

			func_id fid;
			fid = stg_register_func(mod, func);

			obj = stg_register_func_object(
					mod->vm, &mod->store, fid, NULL);
		} else {
			uint8_t buffer[variant_type->size];
			assert(info->options[tag].size == 0);
			ast_dt_encode_variant(
					info, tag, NULL, buffer);

			obj.type = info->type;
			obj.data = buffer;
			obj = register_object(mod->vm, &mod->store, obj);
		}

		struct ast_node *value_node;
		value_node = ast_init_node_lit(
				ctx, AST_NODE_NEW,
				info->options[tag].loc, obj);

		struct ast_node *target_node;
		target_node = ast_init_node_lookup(
				ctx, AST_NODE_NEW, info->options[tag].loc,
				info->options[tag].name);

		int bind_id;
		bind_id = ast_node_composite_bind(
				ctx, scope, target_node,
				value_node, false);

		ast_node_composite_add_member(
				ctx, scope, info->options[tag].name,
				NULL, bind_id);
	}

	type_id scope_type;
	scope_type = ast_dt_finalize_composite(
			ctx, mod, scope, NULL, 0);

	struct object res = {0};

	int err;
	err = stg_instantiate_static_object(
			ctx, mod, scope_type, &res);

	if (err) {
		printf("Failed to create static object for variant.\n");
		return OBJ_UNSET;
	}

	return res;
#else
	(void)ast_dt_variant_pack;
	(void)ast_dt_variant_unpack;
	(void)ast_dt_variant_can_unpack;
	return OBJ_NONE;
#endif
}

type_id
ast_dt_finalize_variant(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_datatype_variant *options, size_t num_options,
		struct ast_typecheck_dep *deps, size_t num_deps)
{
	bool ok = true;
	size_t max_data_size = 0;

	struct stg_type_variant_option *opts;
	opts = arena_allocn(&mod->mem,
			num_options, sizeof(struct stg_type_variant_option));

	for (size_t i = 0; i < num_options; i++) {
		opts[i].name = options[i].name;
		opts[i].data_type = TYPE_UNSET;
		opts[i].loc = options[i].loc;

		if (options[i].data_type) {
			int err;

			struct object type_out = {0};

			struct ast_tc_expected out_expectation = {0};
			out_expectation.kind = AST_NODE_TC_EXP_TYPE;
			out_expectation.type = ctx->vm->default_types.type;

			err = ast_node_typecheck(
					ctx, mod,
					options[i].data_type,
					deps, num_deps,
					out_expectation,
					&type_out);
			if (err) {
				ok = false;
				continue;
			}

			assert_type_equals(ctx->vm,
					options[i].data_type->type, ctx->vm->default_types.type);

			if (!type_equals(ctx->vm,
						type_out.type, ctx->vm->default_types.type)) {
				stg_error(ctx->err, opts[i].loc,
						"Failed to evaluate the option's data type.");
				ok = false;
				continue;
			}

			opts[i].data_type = *(type_id *)type_out.data;

			struct type *type;
			type = vm_get_type(ctx->vm, opts[i].data_type);
			if (type->size > max_data_size) {
				max_data_size = type->size;
			}

			opts[i].size = type->size;
			opts[i].copy = type->base->obj_copy;
			opts[i].type_data = type->data;
		}
	}

	if (!ok) {
		// TODO: Free opts.
		return TYPE_UNSET;
	}

	struct stg_type_variant_info *info;
	info = arena_alloc(&mod->mem,
			sizeof(struct stg_type_variant_info));

	info->num_options = num_options;
	info->options = opts;
	// TODO: Variant location
	// info->loc = node->loc;
	info->loc = STG_NO_LOC;

	size_t tag_size;
	ffi_type *tag_type;

	if (num_options <= UINT8_MAX) {
		tag_size = 1;
		tag_type = &ffi_type_uint8;
	} else if (num_options <= UINT16_MAX) {
		tag_size = 2;
		tag_type = &ffi_type_uint16;
	} else if (num_options <= UINT32_MAX) {
		tag_size = 4;
		tag_type = &ffi_type_uint32;
	} else {
		tag_size = 8;
		tag_type = &ffi_type_uint64;
	}

	info->tag_size = tag_size;
	info->total_size = tag_size + max_data_size;

	struct type new_type = {0};

	new_type.name = mod_atoms(mod, "variant");
	new_type.base = &variant_type_base;
	new_type.size = info->total_size;
	new_type.data = info;

	if (max_data_size == 0) {
		new_type.ffi_type = tag_type;
	}

	type_id variant_type_id;
	variant_type_id = stg_register_type(mod, new_type);

	info->type = variant_type_id;

	struct type *variant_type;
	variant_type = vm_get_type(mod->vm, variant_type_id);

	variant_type->static_object =
		ast_dt_create_variant_type_scope(
				ctx, mod, info);

	return variant_type_id;
}

bool
ast_dt_variant_type_is_inst(
		struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	return type->base == &variant_type_base;
}

ssize_t
ast_dt_variant_tag_by_name(
		struct vm *vm, type_id tid, struct atom *name)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	assert(type->base == &variant_type_base);

	struct stg_type_variant_info *info;
	info = type->data;

	for (size_t i = 0; i < info->num_options; i++) {
		if (info->options[i].name == name) {
			return i;
		}
	}

	return -1;
}

type_id
ast_dt_variant_tag_data_type(
		struct vm *vm, type_id tid, size_t tag)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	assert(type->base == &variant_type_base);

	struct stg_type_variant_info *info;
	info = type->data;

	assert(tag < info->num_options);

	return info->options[tag].data_type;
}
