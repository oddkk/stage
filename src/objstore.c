#include "objstore.h"
#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <math.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>
#include "dlist.h"
#include "utils.h"
#include "vm.h"
#include "module.h"
#include "base/mod.h"
#include "ast.h"

static struct string default_type_repr(struct vm *vm, struct arena *mem,
									   struct type *type)
{
	return type->base->name;
}

static struct string default_obj_repr(struct vm *vm, struct arena *mem,
									  struct object *object)
{
	struct type *type = vm_get_type(vm, object->type);
	return type->base->name;
}

struct type
_init_plain_type(struct type_base *base, struct atom *name, size_t size)
{
	struct type t = {0};

	t.name = name;
	t.base = base;
	t.size = size;

	return t;
}

bool
type_equals(struct vm *vm, type_id lhs_id, type_id rhs_id)
{
	if (lhs_id == rhs_id) {
		return true;
	}

	struct type *lhs = vm_get_type(vm, lhs_id);
	struct type *rhs = vm_get_type(vm, rhs_id);

	return (
		lhs->base == rhs->base &&
		lhs->base->equals &&
		lhs->base->equals(vm, lhs, rhs)
	);
}

void
_assert_type_equals_failed(struct vm *vm, type_id lhs, type_id rhs,
		const char *file, int line, const char *func)
{
	printf("stage: %s:%i: %s: Assertion type_equals(<",
			file, line, func);
	print_type_repr(vm, vm_get_type(vm, lhs));
	printf(">, <");
	print_type_repr(vm, vm_get_type(vm, rhs));
	printf(">) failed.\n");
	abort();
}

bool
obj_equals(struct vm *vm, struct object lhs, struct object rhs)
{
	if (!type_equals(vm, lhs.type, rhs.type)) {
		return false;
	}

	struct type *lhs_type = vm_get_type(vm, lhs.type);
	struct type *rhs_type = vm_get_type(vm, rhs.type);
	assert(lhs_type->size == rhs_type->size);
	// TODO: Use user defined comparator.
	return memcmp(lhs.data, rhs.data, lhs_type->size) == 0;
}

void
objstore_init(struct objstore *store, stg_mod_id mod_id, struct stg_memory *mem)
{
	store->mod_id = mod_id;
	arena_init(&store->data, mem);
	paged_list_init(&store->types, mem, sizeof(struct type));
	paged_list_init(&store->funcs, mem, sizeof(struct func));
}

void *
objstore_alloc(struct objstore *store, size_t num_bytes)
{
	return arena_alloc(&store->data, num_bytes);
}

struct object
register_object(struct vm *vm, struct objstore *store, struct object obj) {
	struct type *type;
	type = vm_get_type(vm, obj.type);

	struct object res = {0};
	res.type = obj.type;
	res.data = objstore_alloc(store, type->size);

	if (type->size > 0) {
		if (!res.data) {
			return OBJ_NONE;
		}
		memcpy(res.data, obj.data, type->size);
	}

	if (type->base->obj_copy) {
		struct stg_exec ctx = {0};
		ctx.store = store;
		type->base->obj_copy(&ctx, type->data, res.data);
	}

	return res;
}

void free_objstore(struct objstore *store) {
	arena_destroy(&store->data);
	paged_list_destroy(&store->types);
	paged_list_destroy(&store->funcs);
}

void *
stg_alloc(struct stg_exec *ctx, size_t nmemb, size_t size)
{
	size_t res;
	// TODO: Make this cross platform and cross compiler compliant.
	if (__builtin_mul_overflow(nmemb, size, &res)) {
		panic("Attempted to allocate memory with a size that exeedes 64-bit integers.");
		return NULL;
	}

	if (ctx->store) {
		return objstore_alloc(ctx->store, res);
	} else {
		return arena_alloc(&ctx->heap, res);
	}
}

modtype_id
store_register_type(struct objstore *store, struct type type)
{
	modtype_id mtid;
	mtid = paged_list_push(&store->types);
	*(struct type *)paged_list_get(&store->types, mtid) = type;
	return TYPE_ID(store->mod_id, mtid);
}

modfunc_id
store_register_func(struct objstore *store, struct func func)
{
	modfunc_id mfid;
	mfid = paged_list_push(&store->funcs);
	*(struct func *)paged_list_get(&store->funcs, mfid) = func;
	return FUNC_ID(store->mod_id, mfid);
}

type_id
func_return_type(struct vm *vm, type_id func_type_id)
{
	if (!stg_type_is_func(vm, func_type_id)) {
		return TYPE_UNSET;
	}

	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	return func_info->return_type;
}

size_t
func_num_params(struct vm *vm, type_id func_type_id)
{
	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	return func_info->num_params;
}

type_id
func_param_type(struct vm *vm, type_id func_type_id, size_t param_i)
{
	struct type *func_type = vm_get_type(vm, func_type_id);
	struct stg_func_type *func_info =
		(struct stg_func_type *)func_type->data;

	assert(param_i < func_info->num_params);
	return func_info->params[param_i];
}

void print_type_repr(struct vm *vm, struct type *type)
{
	struct arena *mem = &vm->transient;
	struct string res;

	arena_mark cp = arena_checkpoint(mem);

	if (type->base->repr) {
		res = type->base->repr(vm, mem, type);
	} else {
		res = default_type_repr(vm, mem, type);
	}
	printf("%.*s", LIT(res));

	arena_reset(mem, cp);
}

struct string
type_repr_to_alloced_string(struct vm *vm, struct type *type)
{
	struct arena *mem = &vm->transient;
	struct string res;

	arena_mark cp = arena_checkpoint(mem);

	if (type->base->repr) {
		res = type->base->repr(vm, mem, type);
	} else {
		res = default_type_repr(vm, mem, type);
	}

	struct string alloced;
	alloced.length = res.length;
	alloced.text = calloc(1, alloced.length);

	memcpy(alloced.text, res.text, alloced.length);

	arena_reset(mem, cp);

	return alloced;
}

void print_obj_repr(struct vm *vm, struct object obj)
{
	struct arena *mem = &vm->transient;

	arena_mark mark = arena_checkpoint(mem);

	struct type *type = vm_get_type(vm, obj.type);
	struct string res;

	if (type->base->obj_repr) {
		res = type->base->obj_repr(vm, mem, &obj);
	} else {
		res = default_obj_repr(vm, mem, &obj);
	}
	printf("%.*s", LIT(res));

	arena_reset(mem, mark);
}

struct string
obj_repr_to_alloced_string(struct vm *vm, struct object obj)
{
	struct arena *mem = &vm->transient;
	struct type *type = vm_get_type(vm, obj.type);
	struct string res;

	arena_mark cp = arena_checkpoint(mem);

	if (type->base->obj_repr) {
		res = type->base->obj_repr(vm, mem, &obj);
	} else {
		res = default_obj_repr(vm, mem, &obj);
	}

	struct string alloced;
	alloced.length = res.length;
	alloced.text = calloc(1, alloced.length);

	memcpy(alloced.text, res.text, alloced.length);

	arena_reset(mem, cp);

	return alloced;
}

struct string
type_repr_to_string(struct vm *vm, struct arena *mem, struct type *type)
{
	struct string res;

	if (type->base->obj_repr) {
		res = type->base->repr(vm, mem, type);
	} else {
		res = default_type_repr(vm, mem, type);
	}

	return res;
}

struct string
obj_repr_to_string(struct vm *vm, struct arena *mem, struct object obj)
{
	struct type *type = vm_get_type(vm, obj.type);
	struct string res;

	if (type->base->obj_repr) {
		res = type->base->obj_repr(vm, mem, &obj);
	} else {
		res = default_obj_repr(vm, mem, &obj);
	}

	return res;
}


void
arena_string_append_type_repr(struct string *str, struct vm *vm,
							  struct arena *mem, struct type *type)
{
	struct string type_repr;

	arena_mark mark = arena_checkpoint(mem);
	if (type->base->repr) {
		type_repr = type->base->repr(vm, mem, type);
	} else {
		type_repr = default_type_repr(vm, mem, type);
	}
	arena_string_append(mem, str, type_repr);

	str->text = arena_reset_and_keep(mem, mark, str->text, str->length);
}

void
arena_string_append_obj_repr(struct string *str, struct vm *vm,
							 struct arena *mem, struct object *object)
{
	struct string repr;
	struct type *type;
	type = vm_get_type(vm, object->type);

	arena_mark mark = arena_checkpoint(mem);
	if (type->base->obj_repr) {
		repr = type->base->obj_repr(vm, mem, object);
	} else {
		repr = default_obj_repr(vm, mem, object);
	}
	arena_string_append(mem, str, repr);
	str->text = arena_reset_and_keep(mem, mark, str->text, str->length);
}

ssize_t
object_cons_find_param(
		struct object_cons *cons,
		struct atom *name)
{
	for (size_t i = 0; i < cons->num_params; i++) {
		if (cons->params[i].name == name) {
			return i;
		}
	}

	return -1;
}

ssize_t
object_cons_find_param_unpack_id(
		struct vm *vm,
		struct object_cons *cons,
		struct atom *name)
{
	int local_ids[cons->num_params];
	object_cons_local_descendent_ids(
			vm, cons, local_ids);

	ssize_t param_i;
	param_i = object_cons_find_param(
			cons, name);

	if (param_i < 0) {
		return -1;
	}

	assert(param_i < cons->num_params);

	return local_ids[param_i];
}

ssize_t
object_cons_simple_lookup(
		struct vm *vm,
		struct object_cons *cons,
		struct string lookup)
{
	struct object_cons *current = cons;
	size_t offset = 1;
	struct string expr = lookup;
	struct string part = {0};

	while (string_split(expr, &part, &expr, '.')) {
		struct atom *part_name;
		part_name = vm_atom(vm, part);
		bool found = false;
		for (size_t i = 0; i < current->num_params; i++) {
			if (current->params[i].name == part_name) {
				found = true;
				break;
			} else {
				struct type *mbr_type;
				mbr_type = vm_get_type(
						vm, cons->params[i].type);
				if (mbr_type->obj_inst) {
					offset +=
						object_cons_num_descendants(
								vm, mbr_type->obj_inst->cons) + 1;
				} else {
					offset += 1;
				}
			}
		}

		if (!found) {
			return -1;
		}
	}

	return offset;
}

size_t
object_cons_num_descendants(
		struct vm *vm, struct object_cons *cons)
{
	size_t count = 0;
	count += cons->num_params;

	for (size_t i = 0; i < cons->num_params; i++) {
		struct type *member_type;
		member_type = vm_get_type(
				vm, cons->params[i].type);

		if (member_type->obj_inst) {
			count += object_cons_num_descendants(
					vm, member_type->obj_inst->cons);
		}
	}

	return count;
}

void
object_cons_local_descendent_ids(
		struct vm *vm, struct object_cons *cons,
		int *out_local_descendent_ids)
{
	size_t count = 1;
	for (size_t i = 0; i < cons->num_params; i++) {
		out_local_descendent_ids[i] = count;

		struct type *member_type;
		member_type = vm_get_type(
				vm, cons->params[i].type);

		if (member_type->obj_inst) {
			count += object_cons_num_descendants(
					vm, member_type->obj_inst->cons);
		}

		count += 1;
	}
}

ssize_t
object_cons_all_descendences(struct vm *vm, type_id tid,
		struct object_cons_param *out_descs, size_t out_descs_size)
{
	assert(out_descs_size > 0);
	out_descs[0].type = tid;

	struct type *type = vm_get_type(vm, tid);

	if (!type->obj_inst) {
		return 1;
	}

	struct object_inst *inst;
	inst = type->obj_inst;

	size_t offset = 1;
	for (size_t i = 0; i < inst->cons->num_params; i++) {
		assert(offset < out_descs_size);
		out_descs[offset] = inst->cons->params[i];
		ssize_t err;
		err = object_cons_all_descendences(
				vm, out_descs[offset].type,
				&out_descs[offset], out_descs_size - offset);
		if (err < 0) {
			return -1;
		}

		offset += err;
	}

	return offset;
}

int
object_unpack(
		struct vm *vm, struct object obj,
		size_t unpack_id, struct object *out)
{
	struct type *type;
	type = vm_get_type(vm, obj.type);

	if (unpack_id == 0) {
		assert_type_equals(vm, obj.type, out->type);
		memcpy(out->data, obj.data, type->size);
		return 0;
	}

	// If unpack_id is not 0 it is implied that it must be a child of this
	// member. If this member does not have a obj_inst it can not have children.
	assert(type->obj_inst);

	struct object_cons *def;
	def = type->obj_inst->cons;

	size_t offset = 1;
	for (size_t i = 0; i < def->num_params; i++) {
		struct type *mbr_type;
		mbr_type = vm_get_type(
				vm, def->params[i].type);

		size_t num_desc;
		if (mbr_type->obj_inst) {
			num_desc = object_cons_num_descendants(
					vm, mbr_type->obj_inst->cons);
		} else {
			num_desc = 0;
		}

		assert(unpack_id >= offset);
		if (unpack_id > offset + num_desc) {
			offset += num_desc + 1;
			continue;
		}

		uint8_t buffer[mbr_type->size];
		assert(def->unpack);
		def->unpack(vm, def->data, buffer, obj.data, i);

		struct object mbr = {0};
		mbr.data = buffer;
		mbr.type = def->params[i].type;

		return object_unpack(vm,
				mbr, unpack_id - offset, out);
	}

	return -1;
}

int
object_ct_pack_type(
		struct ast_context *ctx, struct stg_module *mod,
		struct object_cons *cons, void *args, size_t num_args,
		type_id *out)
{
	assert(cons->pack_type || cons->ct_pack_type);
	assert(num_args == cons->num_params);

	type_id res;

	size_t num_errors_pre = 0;
	if (ctx->err) {
		num_errors_pre = ctx->err->num_errors;
	}

	if (cons->pack_type) {
		res = cons->pack_type(
				ctx->vm, cons->data,
				args, num_args);
	} else {
		res = cons->ct_pack_type(
				ctx, mod, cons->data,
				args, num_args);
	}

	if (res == TYPE_UNSET) {
		// If the pack failed it must emit an error.
		if (ctx->err && ctx->err->num_errors <= num_errors_pre) {
			// TODO: Constructor name and location.
			stg_error(ctx->err, STG_NO_LOC,
					"Constructor pack type failed but did not emit any errors.");
		}
		return -1;
	}

	*out = res;
	return 0;
}


int
object_ct_pack(
		struct ast_context *ctx, struct stg_module *mod,
		struct object_cons *cons, void *args, size_t num_args,
		struct object *out)
{
	assert(cons->pack || cons->ct_pack);
	assert(num_args == cons->num_params);
	if (cons->pack) {
		cons->pack(
				ctx->vm, cons->data, out->data,
				args, num_args);
	} else {
		int err;

		size_t num_errors_pre = 0;
		if (ctx->err) {
			num_errors_pre = ctx->err->num_errors;
		}

		err = cons->ct_pack(
				ctx, mod, cons->data, out->data,
				args, num_args);
		if (err) {
			// If the pack failed it must emit an error.
			if (ctx->err && ctx->err->num_errors <= num_errors_pre) {
				// TODO: Constructor name and location.
				stg_error(ctx->err, STG_NO_LOC,
						"Constructor pack failed but did not emit any errors.");
			}
			return -1;
		}
	}

	return 0;
}

int
object_ct_unpack_param(
		struct ast_context *ctx, struct stg_module *mod,
		struct object_cons *cons, struct object obj, size_t param_id,
		struct object *out)
{
	assert(cons->unpack || cons->ct_unpack);
	assert(param_id < cons->num_params);
	assert_type_equals(ctx->vm,
			out->type, cons->params[param_id].type);

	if (cons->unpack) {
		cons->unpack(
				ctx->vm, cons->data, out->data,
				obj.data, param_id);
	} else {
		int err;
		size_t num_errors_pre = 0;
		if (ctx->err) {
			num_errors_pre = ctx->err->num_errors;
		}

		err = cons->ct_unpack(
				ctx, mod, cons->data, out->data,
				obj, param_id);
		if (err) {
			// If the unpack failed it must emit an error.
			if (ctx->err && ctx->err->num_errors <= num_errors_pre) {
				// TODO: Constructor name and location.
				stg_error(ctx->err, STG_NO_LOC,
						"Constructor unpack failed but did not emit any errors.");
			}
			return -1;
		}
	}

	return 0;
}

int
object_cons_descendant_type(
		struct vm *vm, type_id tid,
		size_t unpack_id, type_id *out)
{
	if (unpack_id == 0) {
		*out = tid;
		return 0;
	}

	struct type *type;
	type = vm_get_type(vm, tid);

	// If unpack_id is not 0 it is implied that it must be a child of this
	// member. If this member does not have a obj_inst it can not have children.
	assert(type->obj_inst);

	struct object_cons *def;
	def = type->obj_inst->cons;

	size_t offset = 1;
	for (size_t i = 0; i < def->num_params; i++) {
		struct type *mbr_type;
		mbr_type = vm_get_type(
				vm, def->params[i].type);

		size_t num_desc;
		if (mbr_type->obj_inst) {
			num_desc = object_cons_num_descendants(
					vm, mbr_type->obj_inst->cons);
		} else {
			num_desc = 0;
		}

		assert(unpack_id >= offset);
		if (unpack_id > offset + num_desc) {
			offset += num_desc + 1;
			continue;
		}

		return object_cons_descendant_type(
				vm, def->params[i].type,
				unpack_id - offset, out);
	}

	return -1;
}

#define OBJ_DEBUG_ACTIONS 0

typedef int obj_member_id;
typedef int obj_expr_id;
typedef int obj_bind_id;
typedef int obj_unpack_id;

struct obj_inst_member {
	type_id type;
	struct atom *name;
	size_t num_descendants;

	obj_bind_id bind;
	obj_unpack_id unpack_id;
	obj_bind_id overridden_bind;
	obj_member_id next_expr_target;
	bool action_emitted;
	bool has_inst;
};

struct obj_inst_expr {
	type_id type;

	size_t *mbr_deps;
	size_t num_mbr_deps;

	obj_member_id first_target;

	obj_expr_id next_terminal;
	obj_expr_id *expr_deps;
	size_t num_expr_deps;

	obj_expr_id *outgoing_expr_deps;
	size_t num_outgoing_expr_deps;
	size_t exp_outgoing_expr_deps;

	ssize_t num_incoming_deps;

	struct stg_location loc;
};

struct object_inst_context {
	struct vm *vm;
	struct stg_error_context *err;

	// Note that members, like unpack id, starts with 0 being the top-level
	// object, with descendants from 1.
	struct obj_inst_member *members;
	size_t num_desc_members;

	struct obj_inst_expr *exprs;
	size_t num_exprs;

	size_t num_unvisited_deps;
	obj_expr_id first_terminal_expr;

	struct object_inst_bind *binds;
	size_t num_binds;

	struct object_inst_action *actions;
	size_t num_actions;
	size_t action_i;

	size_t num_errors;
};

static inline struct obj_inst_member *
get_member(struct object_inst_context *ctx, obj_member_id id)
{
	assert(id < ctx->num_desc_members+1);
	return &ctx->members[id];
}

static inline struct object_inst_bind *
get_bind(struct object_inst_context *ctx, obj_bind_id id)
{
	assert(id < ctx->num_binds);
	return &ctx->binds[id];
}

static inline struct obj_inst_expr *
get_expr(struct object_inst_context *ctx, obj_expr_id id)
{
	assert(id < ctx->num_exprs);
	return &ctx->exprs[id];
}

static void
obj_inst_add_expr_target(
		struct object_inst_context *ctx,
		obj_expr_id expr_id, obj_member_id mbr_id)
{
	struct obj_inst_expr *expr;
	expr = get_expr(ctx, expr_id);

	obj_member_id iter = expr->first_target;
	while (iter >= 0) {
		if (iter == mbr_id) {
			return;
		}
		iter = get_member(ctx, iter)->next_expr_target;
	}

	struct obj_inst_member *mbr;
	mbr = get_member(ctx, mbr_id);

	mbr->next_expr_target = expr->first_target;
	expr->first_target = mbr_id;
}

static int
object_inst_bind_single(struct object_inst_context *ctx,
		size_t target_id, size_t unpack_id, size_t bind_id)
{
	struct obj_inst_member *target;
	target = get_member(ctx, target_id);

	struct object_inst_bind *new_bind;
	new_bind = get_bind(ctx, bind_id);

	struct obj_inst_expr *expr;
	expr = get_expr(ctx, new_bind->expr_id);

	int err;
	type_id expr_member_type;
	err = object_cons_descendant_type(
			ctx->vm, expr->type, unpack_id,
			&expr_member_type);
	assert(!err);

	assert_type_equals(ctx->vm, target->type, expr_member_type);

	if (target->bind < 0) {
		target->bind = bind_id;
		target->unpack_id = unpack_id;
#if OBJ_DEBUG_ACTIONS
		printf("bind %zu = %zu[%zu]\n",
				target_id, bind_id, unpack_id);
#endif
	} else if (target->bind != bind_id) {
		struct object_inst_bind *prev_bind;
		prev_bind = get_bind(ctx, target->bind);

		if (prev_bind->expr_id == new_bind->expr_id &&
				target->unpack_id == unpack_id) {
			return 0;
		}

		if (!prev_bind->overridable && !new_bind->overridable) {
			// TODO: Name
			stg_error(ctx->err, new_bind->loc,
					"Member bound multiple times.");
			stg_appendage(ctx->err, prev_bind->loc,
					"Also bound here.");
			ctx->num_errors += 1;
			return -1;
		}

		if (prev_bind->overridable && new_bind->overridable) {
			// TODO: Name
			stg_error(ctx->err, new_bind->loc,
					"Member bound multiple times as overridable.");
			stg_appendage(ctx->err, prev_bind->loc,
					"Also bound here.");
			ctx->num_errors += 1;
			return -1;
		}

		if (new_bind->overridable) {
			target->overridden_bind = bind_id;
#if OBJ_DEBUG_ACTIONS
			printf("no bind %zu != %zu[%zu] (overridden by %i)\n",
					target_id, bind_id, unpack_id, target->bind);
#endif
		} else {
			target->overridden_bind = target->bind;
			target->bind = bind_id;
			target->unpack_id = unpack_id;
#if OBJ_DEBUG_ACTIONS
			printf("bind %zu = %zu[%zu] (overriding %i)\n",
					target_id, bind_id, unpack_id, target->overridden_bind);
#endif
		}
	}

	return 0;
}

static void
obj_inst_remove_terminal_expr(
		struct object_inst_context *ctx, obj_expr_id expr_id)
{
	struct obj_inst_expr *expr;
	expr = get_expr(ctx, expr_id);

	obj_expr_id *iter = &ctx->first_terminal_expr;
	while (*iter >= 0) {
		if (*iter == expr_id) {
			*iter = expr->next_terminal;
			expr->next_terminal = -1;
			return;
		}

		struct obj_inst_expr *iter_expr;
		iter_expr = get_expr(ctx, *iter);
		iter = &iter_expr->next_terminal;
	}

	panic("Attempted to remove expression that was not found in terminal expressions.");
}

static void
obj_inst_emit_action(
		struct object_inst_context *ctx,
		struct object_inst_action act)
{
	assert(ctx->actions);
	assert(ctx->action_i < ctx->num_actions);
	ctx->actions[ctx->action_i] = act;
	ctx->action_i += 1;
}

static int
obj_inst_try_emit_pack(
		struct object_inst_context *ctx,
		obj_member_id mbr_id)
{
	struct obj_inst_member *mbr;
	mbr = get_member(ctx, mbr_id);

	assert(mbr->has_inst);

	if (mbr->action_emitted) {
		return 0;
	}

	int res = 0;

	for (size_t i = 1; i < mbr->num_descendants+1; i++) {
		obj_member_id desc_id;
		desc_id = mbr_id + i;

		struct obj_inst_member *desc;
		desc = get_member(ctx, desc_id);

		if (desc->num_descendants > 0 && !desc->action_emitted) {
			int err;
			err = obj_inst_try_emit_pack(ctx, desc_id);
			if (err) {
				res = -1;
			}
		}

		if (!desc->action_emitted) {
			res = -1;
		}
	}

	if (res) {
		return res;
	}

	{
		struct object_inst_action act = {0};
		act.op = OBJ_INST_PACK;
		act.pack.member_id = mbr_id;
		obj_inst_emit_action(ctx, act);
	}

	mbr->action_emitted = true;
#if OBJ_DEBUG_ACTIONS
	printf("Emit pack %i\n", mbr_id);
#endif

	return 0;
}

static void
obj_inst_expr_emit_actions(
		struct object_inst_context *ctx, obj_expr_id expr_id)
{
	struct obj_inst_expr *expr;
	expr = get_expr(ctx, expr_id);

	// Emit pack actions for all non-terminal members required by this
	// expression, and ensure all binds for all dependencies have been emitted.
	// The packs are issued here and not together with the binds because the
	// pack might depend on expressions that have not yet been emitted.
	for (size_t i = 0; i < expr->num_mbr_deps; i++) {
		obj_member_id dep_id = expr->mbr_deps[i];
		struct obj_inst_member *dep;
		dep = get_member(ctx, dep_id);

		if (dep->num_descendants > 0 && !dep->action_emitted) {
			int err;
			err = obj_inst_try_emit_pack(ctx, dep_id);
			assert(!err);
		}

		assert(dep->action_emitted);
	}

	{
		struct object_inst_action expr_act = {0};
		expr_act.op = OBJ_INST_EXPR;
		expr_act.expr.id = expr_id;
		expr_act.expr.deps = expr->mbr_deps;
		expr_act.expr.num_deps = expr->num_mbr_deps;
		obj_inst_emit_action(ctx, expr_act);
#if OBJ_DEBUG_ACTIONS
		printf("Emit expr %i\n", expr_id);
#endif
	}

	obj_member_id target_id = expr->first_target;

	// Emit bind actions for all terminal members touched by this expression.
	while (target_id >= 0) {
		struct obj_inst_member *target;
		target = get_member(ctx, target_id);

		struct object_inst_bind *bind;
		bind = get_bind(ctx, target->bind);

		// Note that we iterate through the target->num_descendants+1 members
		// from target to include the target itself.
		for (ssize_t i = target->num_descendants; i >= 0; i--) {
			obj_member_id desc_id = target_id + i;
			struct obj_inst_member *desc;
			desc = get_member(ctx, desc_id);

			if (desc->bind != target->bind) {
				continue;
			}

			if (desc->action_emitted) {
				continue;
			}

			if (desc->num_descendants == 0) {
				struct object_inst_action act = {0};
				act.op = OBJ_INST_BIND;
				act.bind.expr_id = bind->expr_id;
				act.bind.member_id = desc_id;
				act.bind.unpack_id = desc->unpack_id;
				obj_inst_emit_action(ctx, act);

				desc->action_emitted = true;
#if OBJ_DEBUG_ACTIONS
				printf("Emit bind %i\n", desc_id);
#endif
			}
		}

		target_id = target->next_expr_target;
	}
}

int
object_inst_order(
		struct vm *vm, struct stg_error_context *err, struct object_inst *inst,
		struct object_inst_extra_expr *extra_exprs, size_t num_extra_exprs,
		struct object_inst_bind       *extra_binds, size_t num_extra_binds,
		struct object_inst_action **out_actions, size_t *out_num_actions,
		struct stg_location inst_loc)
{
#if OBJ_DEBUG_ACTIONS
	printf("\n\nbegin object_inst_order\n");
#endif
	struct object_inst_context _ctx = {0};
	struct object_inst_context *ctx = &_ctx;

	ctx->vm = vm;
	ctx->err = err;

	ctx->num_exprs = inst->num_exprs + num_extra_exprs;
	ctx->num_binds = inst->num_binds + num_extra_binds;
	ctx->num_desc_members =
		object_cons_num_descendants(vm, inst->cons);

	struct obj_inst_expr _exprs[ctx->num_exprs];
	ctx->exprs = _exprs;
	memset(ctx->exprs, 0, sizeof(struct obj_inst_expr) * ctx->num_exprs);

	struct object_inst_bind _binds[ctx->num_binds];
	ctx->binds = _binds;
	memset(ctx->binds, 0, sizeof(struct object_inst_bind) * ctx->num_binds);

	struct obj_inst_member _members[ctx->num_desc_members+1];
	ctx->members = _members;
	memset(ctx->members, 0, sizeof(struct obj_inst_member) * (ctx->num_desc_members+1));

	{
		size_t offset = 0;
		for (size_t i = 0; i < inst->num_exprs; i++) {
			struct object_inst_expr *expr;
			expr = &inst->exprs[i];
			if (expr->constant) {
				ctx->exprs[offset+i].type = expr->const_value.type;
			} else {
				struct func *func;
				func = vm_get_func(vm, expr->func);
				assert(stg_type_is_func(vm, func->type));

				struct type *func_type;
				func_type = vm_get_type(vm, func->type);

				struct stg_func_type *func_info;
				func_info = func_type->data;

				ctx->exprs[offset+i].type = func_info->return_type;
			}

			assert(ctx->exprs[offset+i].type != TYPE_UNSET);

			ctx->exprs[offset+i].mbr_deps = expr->deps;
			ctx->exprs[offset+i].num_mbr_deps = expr->num_deps;
		}

		offset += inst->num_exprs;

		for (size_t i = 0; i < num_extra_exprs; i++) {
			ctx->exprs[offset+i].type = extra_exprs[i].type;
			ctx->exprs[offset+i].mbr_deps = extra_exprs[i].deps;
			ctx->exprs[offset+i].num_mbr_deps = extra_exprs[i].num_deps;
			ctx->exprs[offset+i].loc = extra_exprs[i].loc;
		}
	}

	{
		size_t offset = 0;
		for (size_t i = 0; i < inst->num_binds; i++) {
			ctx->binds[offset+i] = inst->binds[i];
		}

		offset += inst->num_binds;

		for (size_t i = 0; i < num_extra_binds; i++) {
			ctx->binds[offset+i] = extra_binds[i];
		}
	}

	{
		size_t num_descs = ctx->num_desc_members+1;
		struct object_cons_param descs[num_descs];
		memset(descs, 0, sizeof(struct object_cons_param) * num_descs);
		object_cons_all_descendences(
				vm, inst->type, descs, num_descs);

		// Determine what bind is assigned to each member.
		for (size_t i = 0; i < ctx->num_desc_members+1; i++) {
			ctx->members[i].bind = -1;
			ctx->members[i].overridden_bind = -1;

			ctx->members[i].name = descs[i].name;
			ctx->members[i].type = descs[i].type;

			struct type *type;
			type = vm_get_type(vm, ctx->members[i].type);

			if (type->obj_inst) {
				ctx->members[i].num_descendants =
					object_cons_num_descendants(
							vm, type->obj_inst->cons);
				ctx->members[i].has_inst = true;
			} else {
				ctx->members[i].num_descendants = 0;
			}
		}
	}

	for (size_t expr_i = 0; expr_i < ctx->num_exprs; expr_i++) {
		struct obj_inst_expr *expr;
		expr = get_expr(ctx, expr_i);

		expr->first_target = -1;
	}

	for (size_t bind_i = 0; bind_i < ctx->num_binds; bind_i++) {
		struct object_inst_bind *bind;
		bind = get_bind(ctx, bind_i);

		struct obj_inst_expr *expr;
		expr = get_expr(ctx, bind->expr_id);

		struct obj_inst_member *target;
		target = get_member(ctx, bind->target_id);

		for (size_t i = 0; i < target->num_descendants+1; i++) {
			obj_member_id desc_id = bind->target_id+i;
			struct obj_inst_member *desc;
			desc = get_member(ctx, desc_id);

			object_inst_bind_single(ctx,
					desc_id, bind->unpack_id+i, bind_i);

			obj_inst_add_expr_target(
					ctx, bind->expr_id, desc_id);
		}
	}

	for (size_t mbr_i = 0; mbr_i < ctx->num_desc_members+1; mbr_i++) {
		struct obj_inst_member *mbr;
		mbr = get_member(ctx, mbr_i);

		if (mbr->bind < 0 && !mbr->has_inst) {
			// TODO: Location and context about instantiation.
			stg_error(ctx->err, inst_loc,
					"The member '%.*s' was not bound.",
					ALIT(mbr->name));
			ctx->num_errors += 1;
		}
	}

	if (ctx->num_errors > 0) {
		return -1;
	}

	size_t num_total_dependencies = 0;
	for (size_t expr_i = 0; expr_i < ctx->num_exprs; expr_i++) {
		struct obj_inst_expr *expr;
		expr = get_expr(ctx, expr_i);

		for (size_t dep_i = 0; dep_i < expr->num_mbr_deps; dep_i++) {
			ast_member_id dep_id = expr->mbr_deps[dep_i];
			struct obj_inst_member *dep;
			dep = get_member(ctx, dep_id);

			num_total_dependencies += dep->num_descendants+1;
		}
	}

	// Set up for topological sort by finding dependencies between expressions.
	obj_expr_id _expr_deps[num_total_dependencies];

	for (size_t expr_i = 0; expr_i < ctx->num_exprs; expr_i++) {
		struct obj_inst_expr *expr;
		expr = get_expr(ctx, expr_i);

		if (expr_i+1 == ctx->num_exprs) {
			expr->next_terminal = -1;
		} else {
			expr->next_terminal = expr_i + 1;
		}
	}
	if (ctx->num_exprs > 0) {
		ctx->first_terminal_expr = 0;
	} else {
		ctx->first_terminal_expr = -1;
	}

	size_t total_outgoing_dependencies = 0;
	{
		size_t offset = 0;
		for (size_t expr_id = 0; expr_id < ctx->num_exprs; expr_id++) {
			struct obj_inst_expr *expr;
			expr = get_expr(ctx, expr_id);

			obj_expr_id *deps;
			deps = &_expr_deps[offset];
			size_t num_deps = 0;

			for (size_t dep_i = 0; dep_i < expr->num_mbr_deps; dep_i++) {
				ast_member_id dep_id = expr->mbr_deps[dep_i];
				struct obj_inst_member *dep;
				dep = get_member(ctx, dep_id);

				for (size_t desc_i = 0; desc_i < dep->num_descendants+1; desc_i++) {
					ast_member_id desc_id = dep_id + desc_i;
					struct obj_inst_member *desc;
					desc = get_member(ctx, desc_id);

					if (desc->num_descendants > 0) {
						continue;
					}

					struct object_inst_bind *bind;
					bind = get_bind(ctx, desc->bind);

					obj_expr_id dep_expr_id;
					dep_expr_id = bind->expr_id;

					bool found = false;
					for (size_t i = 0; i < num_deps; i++) {
						if (deps[i] == dep_expr_id) {
							found = true;
							break;
						}
					}

					if (!found) {
						assert(offset + num_deps + 1 <= num_total_dependencies);
						deps[num_deps] = dep_expr_id;
						num_deps += 1;

						struct obj_inst_expr *dep_expr;
						dep_expr = get_expr(ctx, dep_expr_id);

						dep_expr->exp_outgoing_expr_deps += 1;
						total_outgoing_dependencies += 1;
					}
				}
			}

			expr->num_expr_deps = num_deps;
			expr->expr_deps = deps;

			offset += num_deps;
		}
	}

	obj_expr_id _outgoing_expr_deps[total_outgoing_dependencies];

	{
		size_t offset = 0;
		for (size_t expr_i = 0; expr_i < ctx->num_exprs; expr_i++) {
			struct obj_inst_expr *expr;
			expr = get_expr(ctx, expr_i);

			assert(offset + expr->exp_outgoing_expr_deps <= total_outgoing_dependencies);
			expr->outgoing_expr_deps = &_outgoing_expr_deps[offset];
			offset += expr->exp_outgoing_expr_deps;
		}
	}

	for (size_t expr_id = 0; expr_id < ctx->num_exprs; expr_id++) {
		struct obj_inst_expr *expr;
		expr = get_expr(ctx, expr_id);

		for (size_t dep_i = 0; dep_i < expr->num_expr_deps; dep_i++) {
			obj_expr_id dep_id = expr->expr_deps[dep_i];
			struct obj_inst_expr *dep_expr;
			dep_expr = get_expr(ctx, dep_id);

			bool found = false;
			for (size_t i = 0; i < dep_expr->num_outgoing_expr_deps; i++) {
				if (dep_expr->outgoing_expr_deps[i] == expr_id) {
					found = true;
					break;
				}
			}

			if (!found) {
				assert(dep_expr->num_outgoing_expr_deps+1 <=
						dep_expr->exp_outgoing_expr_deps);

				dep_expr->outgoing_expr_deps[dep_expr->num_outgoing_expr_deps] =
					expr_id;
				dep_expr->num_outgoing_expr_deps += 1;

				if (expr->num_incoming_deps == 0) {
					obj_inst_remove_terminal_expr(
							ctx, expr_id);
				}

				expr->num_incoming_deps += 1;
				ctx->num_unvisited_deps += 1;
			}
		}
	}

	// There is one action for each descendant member to pack or bind it, plus
	// one action per expression to evaluate it.
	ctx->num_actions =
		ctx->num_desc_members+1 +
		ctx->num_exprs;

	ctx->actions = calloc(ctx->num_actions, sizeof(struct object_inst_action));

	ctx->action_i = 0;

	// Sort the expressions by dependencies.
	while (ctx->first_terminal_expr >= 0) {
		obj_expr_id expr_id = ctx->first_terminal_expr;
		struct obj_inst_expr *expr;
		expr = get_expr(ctx, expr_id);

		ctx->first_terminal_expr = expr->next_terminal;

		obj_inst_expr_emit_actions(
				ctx, expr_id);

		// Make sure each expression is visited at most once.
		assert(expr->num_incoming_deps == 0);
		expr->num_incoming_deps = -1;

		for (size_t dep_i = 0; dep_i < expr->num_outgoing_expr_deps; dep_i++) {
			obj_expr_id dep_id = expr->outgoing_expr_deps[dep_i];
			struct obj_inst_expr *dep;
			dep = get_expr(ctx, dep_id);

			assert(dep->num_incoming_deps > 0);
			dep->num_incoming_deps -= 1;
			ctx->num_unvisited_deps -= 1;

			if (dep->num_incoming_deps == 0) {
				assert(dep->next_terminal < 0);
				dep->next_terminal = ctx->first_terminal_expr;
				ctx->first_terminal_expr = dep_id;
			}
		}
	}

	if (ctx->num_unvisited_deps > 0) {
		printf("One or more loops detected when resolving expression order.\n");

		for (size_t expr_i = 0; expr_i < ctx->num_exprs; expr_i++) {
			struct obj_inst_expr *expr;
			expr = get_expr(ctx, expr_i);

			if (expr->num_incoming_deps <= 0) {
				continue;
			}

			printf("  - Expr %zu (%zi unvisited deps, targets ",
					expr_i, expr->num_incoming_deps);

			{
				obj_member_id target = expr->first_target;
				bool first = true;
				while (target >= 0) {
					struct obj_inst_member *mbr;
					mbr = get_member(ctx, target);

					printf("%s%i", first ? "" : ",", target);

					first = false;
					target = mbr->next_expr_target;
				}
			}

			printf(") ");
			print_type_repr(ctx->vm, vm_get_type(ctx->vm, expr->type));
			printf("\n");

			for (size_t dep_i = 0; dep_i < expr->num_expr_deps; dep_i++) {
				obj_expr_id dep_id = expr->expr_deps[dep_i];
				struct obj_inst_expr *dep;
				dep = get_expr(ctx, dep_id);

				printf("      dep %i %s\n", dep_id,
						(dep->num_incoming_deps == -1)
						? "visited" : "not visited");
			}
		}

		free(ctx->actions);
		return -1;
	}

	// Note that we iterate through the ctx->num_desc_members+1 members to
	// include the final object.
	for (ssize_t mbr_id = ctx->num_desc_members; mbr_id >= 0; mbr_id--) {
		struct obj_inst_member *mbr;
		mbr = get_member(ctx, mbr_id);
		if (mbr->has_inst && !mbr->action_emitted) {
			obj_inst_try_emit_pack(ctx, mbr_id);
		}
		assert(mbr->action_emitted);
	}

	*out_actions = ctx->actions;
	*out_num_actions = ctx->action_i;
	return 0;
}

static void
print_indent(int depth)
{
	for (int i = 0; i < depth; ++i) {
		printf("  ");
	}
}

size_t
object_cons_print_internal(
		struct vm *vm, struct object_cons *cons,
		size_t indent, int num_desc_digits, size_t id_offset)
{
	size_t count = 0;

	for (size_t i = 0; i < cons->num_params; i++) {
		struct type *member_type;
		member_type = vm_get_type(
				vm, cons->params[i].type);

		printf("%*zu ", num_desc_digits, id_offset+count);
		print_indent(indent);
		printf("%.*s: ", ALIT(cons->params[i].name));
		print_type_repr(vm, member_type);
		printf("\n");

		count += 1;

		if (member_type->obj_inst) {
			count += object_cons_print_internal(
					vm, member_type->obj_inst->cons,
					indent+1, num_desc_digits, id_offset+count);
		}
	}

	return count;
}

void
object_cons_print(struct vm *vm, struct object_cons *cons)
{
	printf("object cons:\n");

	size_t num_desc;
	num_desc = object_cons_num_descendants(
			vm, cons);

	int num_desc_digits;
	num_desc_digits =
		(int)ceil(log10((double)num_desc));
	if (num_desc_digits <= 0) {
		num_desc_digits = 1;
	}

	object_cons_print_internal(
			vm, cons, 0, num_desc_digits, 1);
	printf("\n");
}

int
stg_instantiate_static_object(
		struct ast_context *ctx, struct stg_module *mod,
		type_id tid, struct object *out)
{
	struct ast_node *init_func;
	struct ast_node *inst;
	struct ast_node *cons_obj_lit;
	struct ast_node *ret;

	struct type *type;
	type = vm_get_type(ctx->vm, tid);

	if (!type->obj_inst) {
		printf("Attempted to instantiate a static object without an object"
				"instantiator.\n");
		return -1;
	}

	struct object cons_obj = {0};
	cons_obj.type = ctx->types.type;
	cons_obj.data = &tid;

	struct object type_obj = {0};
	type_obj.type = ctx->types.type;
	type_obj.data = &tid;

	ret = ast_init_node_lit(ctx,
			AST_NODE_NEW, STG_NO_LOC, type_obj);

	cons_obj_lit = ast_init_node_lit(ctx,
			AST_NODE_NEW, STG_NO_LOC, cons_obj);

	inst = ast_init_node_inst(ctx,
			AST_NODE_NEW, STG_NO_LOC, cons_obj_lit, NULL, 0);

	init_func = ast_init_node_func(ctx,
			AST_NODE_NEW, STG_NO_LOC,
			NULL, NULL, 0,
			ret, inst);

	int err;
	err = ast_node_typecheck(ctx, mod,
			init_func, NULL, 0, TYPE_UNSET);
	if (err) {
		printf("Failed typechecking initialize function for module '%.*s'.\n",
				ALIT(mod->name));
		return -1;
	}

	struct bc_env *bc_env;
	bc_env = ast_func_gen_bytecode(ctx, mod,
			NULL, NULL, 0, init_func);
	if (!bc_env) {
		printf("Failed codegen for module '%.*s'.\n",
				ALIT(mod->name));
		return -1;
	}

	struct func func = {0};

	func.type = stg_register_func_type(
			mod, tid, NULL, 0);

	func.kind = FUNC_BYTECODE;
	func.bytecode = bc_env;


	func_id init_func_id;
	init_func_id = stg_register_func(mod, func);

	uint8_t obj_buffer[type->size];
	struct object obj = {0};

	obj.data = obj_buffer;
	obj.type = tid;

	struct stg_exec exec_ctx = {0};
	mod_arena(mod, &exec_ctx.heap);
	vm_call_func(ctx->vm, &exec_ctx, init_func_id, NULL, 0, &obj);

	*out =
		register_object(ctx->vm, &mod->store, obj);

	arena_destroy(&exec_ctx.heap);

	return 0;
}
