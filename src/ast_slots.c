#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "base/mod.h"
#include "dlist.h"

#include <unistd.h>
#include <sys/mman.h>

#define AST_DEBUG_BINDS 0
#define AST_DEBUG_UNION 0
#define AST_DEBUG_SUBST 0

#define AST_BIND_ERROR_DEBUG_PRINT 0

// If 1, slot 0 will be set to error to debug uninitialized slots.
#define AST_DEBUG_OFFSET_SLOTS 0

#define AST_DEBUG_SLOT_SOLVE 0

const char *
ast_slot_name(enum ast_env_slot_kind kind) {
	switch (kind) {
		case AST_SLOT_ERROR:      return "ERROR";

		case AST_SLOT_WILDCARD:   return "WILDCARD";
		case AST_SLOT_CONST_TYPE: return "CONST_TYPE";
		case AST_SLOT_CONST:      return "CONST";
		case AST_SLOT_PARAM:      return "PARAM";
		case AST_SLOT_TEMPL:      return "TEMPL";
		case AST_SLOT_CLOSURE:    return "CLOSURE";
		case AST_SLOT_CONS:       return "CONS";
		case AST_SLOT_CONS_ARRAY: return "CONS_ARRAY";

		case AST_SLOT_SUBST:      return "SUBST";
	}

	return "(invalid)";
}

static struct object
ast_register_integer(struct ast_context *ctx, struct ast_env *env, int64_t value)
{
	struct object result = {0};

	result.type = ctx->types.integer;
	result.data = &value;

	return register_object(ctx->vm, env->store, result);
}

ssize_t
ast_object_lookup_arg(struct ast_object *obj, struct atom *arg_name)
{
	for (size_t i = 0; i < obj->num_present_args; i++) {
		if (obj->args[i].name == arg_name) {
			return i;
		}
	}

	return AST_SLOT_NOT_FOUND;
}

static struct ast_object_def *
ast_try_determine_object_def(
		struct ast_context *ctx, struct ast_env *env,
		struct object obj, struct ast_object_def *obj_def,
		struct ast_object_def *target_def)
{
	if (!obj_def && !target_def) {
		return NULL;
	}

	if (target_def && obj_def != target_def) {
		if (target_def->can_unpack &&
				target_def->can_unpack(ctx, env, target_def, obj)) {
			return target_def;
		} else {
			return NULL;
		}
	}

	return obj_def;
}

ast_slot_id
ast_slot_alloc(struct ast_env *env)
{
	ast_slot_id new_slot;

	new_slot = env->num_alloced_slots;
	env->num_alloced_slots += 1;

	return new_slot;
}

static struct ast_slot_constraint *
ast_alloc_constraint(
		struct ast_env *env, enum ast_constraint_kind kind,
		enum ast_constraint_source source,
		struct stg_location loc, ast_slot_id target)
{
	if (env->page_size == 0) {
		env->page_size = sysconf(_SC_PAGESIZE);
		env->constraints_per_page = env->page_size / sizeof(struct ast_slot_constraint);
	}

	if (env->last_page_num_used == 0 ||
			env->last_page_num_used >= env->constraints_per_page) {
		size_t new_num_pages = env->num_pages + 1;
		struct ast_slot_constraint **new_pages = realloc(
				env->constraint_pages, new_num_pages * sizeof(struct ast_slot_constraint *));
		if (!new_pages) {
			perror("realloc");
			return NULL;
		}

		env->constraint_pages = new_pages;

		env->constraint_pages[env->num_pages] = mmap(
				NULL, env->page_size,
				PROT_READ|PROT_WRITE,
				MAP_PRIVATE|MAP_ANONYMOUS,
				-1, 0);

		if (env->constraint_pages[env->num_pages] == MAP_FAILED) {
			perror("mmap");
			return NULL;
		}

		env->num_pages = new_num_pages;
		env->last_page_num_used = 0;
	}

	struct ast_slot_constraint *res;

	res = &env->constraint_pages[env->num_pages - 1][env->last_page_num_used];
	env->last_page_num_used += 1;

	memset(res, 0, sizeof(struct ast_slot_constraint));

	res->kind = kind;
	res->source = source;
	res->target = target;
	res->reason.loc = loc;

	return res;
}

void
ast_slot_value_error(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_ERROR, source, loc, target);
}

void
ast_slot_require_is_obj(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, struct object val)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_IS_OBJ, source, loc, target);

	constr->is.obj = val;
}

void
ast_slot_require_is_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, type_id val)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_IS_TYPE, source, loc, target);

	constr->is.type = val;
}

void
ast_slot_require_is_func_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id ret_type,
		ast_slot_id *param_types, size_t num_params)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_IS_FUNC_TYPE, source, loc, target);

	ast_slot_require_member_index(
			env, loc, source, target, 0, ret_type);
	ast_slot_require_type(
			env, loc, source, ret_type, AST_SLOT_TYPE);

	for (size_t i = 0; i < num_params; i++) {
		ast_slot_require_member_index(
				env, loc, source, target, i+1, param_types[i]);
		ast_slot_require_type(
				env, loc, source, param_types[i], AST_SLOT_TYPE);
	}
}

void
ast_slot_require_equals(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id slot)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_EQUALS, source, loc, target);

	constr->equals = slot;
}
void
ast_slot_require_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id type)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_TYPE, source, loc, target);

	constr->type = type;
}

void
ast_slot_require_member_named(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, struct atom *name, ast_slot_id member)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_MEMBER_NAMED, source, loc, target);

	constr->member.slot = member;
	constr->member.name = name;
}

void
ast_slot_require_member_index(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, size_t index, ast_slot_id member)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_MEMBER_INDEXED, source, loc, target);

	constr->member.slot = member;
	constr->member.index = index;
}

void
ast_slot_require_is_cons(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, struct object_cons *def)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_IS_CONS, source, loc, target);

	constr->is_cons = def;
}

void
ast_slot_require_cons(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id cons)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_CONS, source, loc, target);

	constr->cons = cons;
}

void
ast_env_copy(struct ast_env *dest, struct ast_env *src)
{
	*dest = *src;

	dest->constraint_pages
		= calloc(dest->num_pages, sizeof(struct ast_slot_constraint));
	if (dest->num_pages > 0) {
		// Already imposed constraints are immutable, so we use src's full
		// pages and just copy the last one.
		for (size_t i = 0; i < dest->num_pages - 1; i++) {
			dest->constraint_pages[i] = src->constraint_pages[i];
		}
		dest->num_borrowed_pages = dest->num_pages - 1;

		struct ast_slot_constraint *last_page;
		last_page = mmap(
				NULL, dest->page_size,
				PROT_READ|PROT_WRITE,
				MAP_PRIVATE|MAP_ANONYMOUS,
				-1, 0);

		memcpy(last_page, src->constraint_pages[src->num_pages - 1],
				dest->page_size);

		dest->constraint_pages[dest->num_pages - 1] = last_page;
	}
}

enum ast_slot_resolve_kind {
	AST_SLOT_RES_UNKNOWN,
	AST_SLOT_RES_ERROR,
	AST_SLOT_RES_CONST,
	AST_SLOT_RES_CONST_TYPE,
	AST_SLOT_RES_CONS,
	AST_SLOT_RES_CONS_ARRAY,
	AST_SLOT_RES_SUBST,
};

enum ast_slot_state_flags {
	AST_SLOT_HAS_ERROR      = (1<<0),
	AST_SLOT_HAS_SUBST      = (1<<1),
	AST_SLOT_HAS_CONS       = (1<<2),
	AST_SLOT_IS_FUNC_TYPE   = (1<<3),
	AST_SLOT_HAS_TYPE       = (1<<4),
	AST_SLOT_HAS_VALUE      = (1<<5),
	AST_SLOT_VALUE_IS_TYPE  = (1<<6),
};

typedef int32_t ast_constraint_id;

struct ast_slot_member_ref {
	bool named;
	union {
		struct atom *name;
		size_t index;
	};
};

struct ast_slot_resolve_member {
	ast_slot_id slot;
	ast_constraint_id authority;
	struct ast_slot_member_ref ref;
};

struct ast_slot_resolve {
	enum ast_slot_state_flags flags;

	ast_slot_id subst;
	ast_slot_id type;

	union {
		type_id type;
		struct object obj;
	} value;

	struct object_cons *cons;

	struct ast_slot_resolve_member *members;
	size_t num_members;

	// Designates the constraints that has the highest priority for the
	// parameter.
	struct {
		ast_constraint_id type;
		ast_constraint_id value;
		ast_constraint_id cons;
	} authority;
};

struct solve_context {
	type_id type;
	struct vm *vm;
	struct stg_error_context *err;
	struct ast_module *mod;
	struct ast_env *env;
	struct ast_slot_resolve *slots;
	size_t num_slots;
};

#if AST_DEBUG_SLOT_SOLVE
static const char *
ast_constraint_source_name(enum ast_constraint_source source)
{
	switch (source) {
		case AST_CONSTR_SRC_FUNC_DECL:
			return "func decl";
		case AST_CONSTR_SRC_TEMPL_PARAM_DECL:
			return "param decl";
	}
	return "(unknown)";
}
#endif

static inline size_t
ast_env_num_constraints(struct solve_context *ctx)
{
	if (ctx->env->num_pages == 0) {
		return 0;
	}

	return
		(ctx->env->num_pages - 1) * ctx->env->constraints_per_page
		+ ctx->env->last_page_num_used;
}

static inline struct ast_slot_constraint *
ast_get_constraint(struct solve_context *ctx, ast_constraint_id constr)
{
	size_t num_constraints = ast_env_num_constraints(ctx);
	assert(constr < num_constraints && constr >= 0);

	return &ctx->env->constraint_pages
		[constr / ctx->env->constraints_per_page]
		[constr % ctx->env->constraints_per_page];
}

static inline struct ast_slot_resolve *
ast_get_slot(struct solve_context *ctx, ast_slot_id slot)
{
	assert(slot < ctx->num_slots && slot >= 0);
	return &ctx->slots[slot];
}

static inline ast_slot_id
ast_slot_resolve_subst(struct solve_context *ctx, ast_slot_id slot)
{
	while (slot >= 0 && (ctx->slots[slot].flags & AST_SLOT_HAS_SUBST) != 0) {
		slot = ctx->slots[slot].subst;
	}

	return slot;
}

static inline struct ast_slot_resolve *
ast_get_real_slot(struct solve_context *ctx, ast_slot_id slot)
{
	return ast_get_slot(ctx, ast_slot_resolve_subst(ctx, slot));
}

static inline bool
is_more_authorative(struct ast_slot_constraint *lhs, struct ast_slot_constraint *rhs)
{
	return lhs->source < rhs->source;
}

static ast_slot_id
ast_slot_join(
		struct solve_context *ctx,
		ast_slot_id lhs, ast_slot_id rhs);

enum ast_solve_slot_param {
	AST_SLOT_PARAM_TYPE,
	AST_SLOT_PARAM_VALUE,
	AST_SLOT_PARAM_CONS,
};

static inline bool
ast_solve_should_apply_internal(
		struct solve_context *ctx, ast_constraint_id constr_id,
		ast_constraint_id *authority)
{
	assert(authority);

	struct ast_slot_constraint *prev_authority = NULL;

	prev_authority = ast_get_constraint(
			ctx, *authority);

	struct ast_slot_constraint *constr;
	constr = ast_get_constraint(ctx, constr_id);
	if (!prev_authority || is_more_authorative(constr, prev_authority)) {
		*authority = constr_id;
#if AST_DEBUG_SLOT_SOLVE
		if (prev_authority) {
			printf(" yes (more authorative '%s' > '%s')\n",
					ast_constraint_source_name(constr->source),
					ast_constraint_source_name(prev_authority->source));
		} else {
			printf(" yes (new)\n");
		}
#endif
		return true;
	}

#if AST_DEBUG_SLOT_SOLVE
	printf(" no (less authorative '%s' > '%s')\n",
			ast_constraint_source_name(constr->source),
			ast_constraint_source_name(prev_authority->source));
#endif
	return false;
}

static inline bool
ast_solve_should_apply(
		struct solve_context *ctx, ast_constraint_id constr_id,
		ast_slot_id slot, enum ast_solve_slot_param param)
{
	struct ast_slot_resolve *res;
	res = ast_get_slot(ctx, slot);

	enum ast_slot_state_flags flag = 0;
	ast_constraint_id *authority = NULL;
	switch (param) {
		case AST_SLOT_PARAM_TYPE:
			flag = AST_SLOT_HAS_TYPE;
			authority = &res->authority.type;
			break;

		case AST_SLOT_PARAM_VALUE:
			flag = AST_SLOT_HAS_VALUE;
			authority = &res->authority.value;
			break;

		case AST_SLOT_PARAM_CONS:
			flag = AST_SLOT_HAS_CONS;
			authority = &res->authority.cons;
			break;
	}

	assert(authority && flag != 0);

	if ((res->flags & flag) != 0) {
		return ast_solve_should_apply_internal(
				ctx, constr_id, authority);
	} else {
		*authority = constr_id;
		res->flags |= flag;
#if AST_DEBUG_SLOT_SOLVE
		printf(" yes (new)\n");
#endif
		return true;
	}
}

static bool
ast_solve_apply_value_type(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot, type_id type)
{
#if AST_DEBUG_SLOT_SOLVE
	printf("%3i apply value type %i = ",
			constr_id, slot);
	print_type_repr(ctx->vm, vm_get_type(ctx->vm, type));
#endif
	if (ast_solve_should_apply(
				ctx, constr_id, slot,
				AST_SLOT_PARAM_VALUE)) {
		struct ast_slot_resolve *res = ast_get_slot(ctx, slot);
		res->value.type = type;
		res->flags |= AST_SLOT_VALUE_IS_TYPE;
		return true;
	}
	return false;
}

static bool
ast_solve_apply_value_obj(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot, struct object obj)
{
	if (obj.type == ctx->type) {
		type_id tid = *(type_id *)obj.type;
		return ast_solve_apply_value_type(
				ctx, constr_id, slot, tid);
	}

#if AST_DEBUG_SLOT_SOLVE
	printf("%3i apply value obj %i = ",
			constr_id, slot);
	print_obj_repr(ctx->vm, obj);
#endif
	if (ast_solve_should_apply(
				ctx, constr_id, slot,
				AST_SLOT_PARAM_VALUE)) {
		struct ast_slot_resolve *res = ast_get_slot(ctx, slot);
		res->value.obj = obj;
		res->flags &= ~AST_SLOT_VALUE_IS_TYPE;
		return true;
	}
	return false;
}

static bool
ast_solve_apply_cons(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot, struct object_cons *cons)
{
#if AST_DEBUG_SLOT_SOLVE
	printf("%3i apply cons %i = %p",
			constr_id, slot, (void *)cons);
#endif
	if (ast_solve_should_apply(
				ctx, constr_id, slot,
				AST_SLOT_PARAM_CONS)) {
		struct ast_slot_resolve *res = ast_get_slot(ctx, slot);
		res->cons = cons;
		res->flags &= ~AST_SLOT_IS_FUNC_TYPE;
		return true;
	}
	return false;
}

static bool
ast_solve_apply_func_type(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot)
{
#if AST_DEBUG_SLOT_SOLVE
	printf("%i apply cons func %i",
			constr_id, slot);
#endif
	if (ast_solve_should_apply(
				ctx, constr_id, slot,
				AST_SLOT_PARAM_CONS)) {
		struct ast_slot_resolve *res = ast_get_slot(ctx, slot);
		res->cons = NULL;
		res->flags |= AST_SLOT_IS_FUNC_TYPE;
		return true;
	}
	return false;
}

static bool
ast_solve_apply_type(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot,
		ast_slot_id type_id)
{
#if AST_DEBUG_SLOT_SOLVE
	printf("%3i apply type %i.type = %i",
			constr_id, slot, type_id);
#endif
	struct ast_slot_resolve *res;
	res = ast_get_real_slot(ctx, slot);

	ast_slot_id old_type;

	if ((res->flags & AST_SLOT_HAS_TYPE) != 0) {
		old_type = res->type;
		res->type = ast_slot_join(
				ctx, res->type, type_id);
	} else {
		old_type = AST_SLOT_NOT_FOUND;
		res->type = type_id;
	}

	ast_solve_should_apply(
			ctx, constr_id, slot,
			AST_SLOT_PARAM_TYPE);

	return old_type != res->type;
}

static ssize_t
ast_slot_find_member(
		struct ast_slot_resolve *slot,
		struct ast_slot_member_ref ref)
{
	for (size_t i = 0; slot->num_members; i++) {
		if (slot->members[i].ref.named == ref.named) {
			if (( slot->members[i].ref.named && slot->members[i].ref.name  == ref.name) ||
				(!slot->members[i].ref.named && slot->members[i].ref.index == ref.index)) {
				return i;
			}
		}
	}

	return -1;
}

static bool
ast_solve_apply_member(struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot_id,
		struct ast_slot_member_ref ref, ast_slot_id mbr_slot)
{
#if AST_DEBUG_SLOT_SOLVE
	if (ref.named) {
		printf("%3i apply member %i.%.*s = %i",
				constr_id, slot_id, ALIT(ref.name), mbr_slot);
	} else {
		printf("%3i apply member %i[%zu] = %i",
				constr_id, slot_id, ref.index, mbr_slot);
	}
#endif
	struct ast_slot_resolve *slot;
	slot = ast_get_real_slot(ctx, slot_id);

	ssize_t mbr_i;
	mbr_i = ast_slot_find_member(slot, ref);

	if (mbr_i >= 0) {
		ast_slot_id new_slot;
		new_slot = ast_slot_join(ctx,
				mbr_slot, slot->members[mbr_i].slot);

		if (ast_solve_should_apply_internal(
					ctx, constr_id, &slot->members[mbr_i].authority)) {
			slot->members[mbr_i].slot = new_slot;
			slot->members[mbr_i].authority = constr_id;
			return true;
		}
		return false;
	}

#if AST_DEBUG_SLOT_SOLVE
	printf(" yes (new)\n");
#endif

	struct ast_slot_resolve_member new_mbr = {0};

	new_mbr.ref = ref;
	new_mbr.authority = constr_id;
	new_mbr.slot = ast_slot_resolve_subst(
			ctx, mbr_slot);

	dlist_append(
			slot->members,
			slot->num_members,
			&new_mbr);
	return true;
}

static inline bool
ast_slot_update_type(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot)
{
	struct ast_slot_resolve *res;
	res = ast_get_slot(ctx, slot);

	if ((res->flags & AST_SLOT_HAS_VALUE) != 0) {
		type_id type;

		if ((res->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
			type = ctx->type;
		} else {
			type = res->value.obj.type;
		}

		return
			ast_solve_apply_value_type(
					ctx, constr_id, res->type, type);
	}
	return false;
}

static ast_slot_id
ast_slot_join(
		struct solve_context *ctx,
		ast_slot_id lhs, ast_slot_id rhs)
{
#if AST_DEBUG_SLOT_SOLVE
	printf("join %i U %i\n", lhs, rhs);
#endif
	ast_slot_id to, from;

	lhs = ast_slot_resolve_subst(ctx, lhs);
	rhs = ast_slot_resolve_subst(ctx, rhs);

	// Substitute the later slot with the earlier one.
	if (lhs > rhs) {
		to = rhs;
		from = lhs;
	} else if (lhs < rhs) {
		to = lhs;
		from = rhs;
	} else {
		return lhs;
	}

	struct ast_slot_resolve *to_slot, *from_slot;
	assert(to >= 0 && from >= 0);

	to_slot   = ast_get_slot(ctx, to);
	from_slot = ast_get_slot(ctx, from);

	assert((to_slot->flags & AST_SLOT_HAS_SUBST) == 0 &&
			(from_slot->flags & AST_SLOT_HAS_SUBST) == 0);

	if ((from_slot->flags & AST_SLOT_HAS_ERROR) != 0) {
		to_slot->flags |= AST_SLOT_HAS_ERROR;
	}

	if ((from_slot->flags & AST_SLOT_HAS_VALUE) != 0) {
		if ((from_slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
			ast_solve_apply_value_type(
					ctx, from_slot->authority.value,
					to, from_slot->value.type);
		} else {
			ast_solve_apply_value_obj(
					ctx, from_slot->authority.value,
					to, from_slot->value.obj);
		}
	}

	if ((from_slot->flags & AST_SLOT_HAS_CONS) != 0) {
		if ((from_slot->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
			ast_solve_apply_func_type(
					ctx, from_slot->authority.cons, to);
		} else {
			ast_solve_apply_cons(
					ctx, from_slot->authority.cons,
					to, from_slot->cons);
		}
	}

	if ((from_slot->flags & AST_SLOT_HAS_TYPE) != 0) {
		ast_solve_apply_type(
				ctx, from_slot->authority.type,
				to, from_slot->type);
	}

	for (size_t i = 0; i < from_slot->num_members; i++) {
		ast_solve_apply_member(ctx,
				from_slot->members[i].authority, to,
				from_slot->members[i].ref,
				from_slot->members[i].slot);
	}

	from_slot->flags = AST_SLOT_HAS_SUBST;
	from_slot->subst = to;

#if AST_DEBUG_SLOT_SOLVE
	printf("join finished\n");
#endif

	return to;
}

// ast_slot_try_get_{value,value_type,type} has a return value less than 0 if a
// typing error occured, a return value of greater than 0 if not enough
// information was available to determine the type, and a return value of 0 if
// the type was determined.  *out_type is only modified if the return value is
// 0.

enum ast_slot_get_error {
	AST_SLOT_GET_OK = 0,
	AST_SLOT_GET_NOT_ENOUGH_INFO = 1,
	AST_SLOT_GET_NO_TYPE_SLOT = 2,
	AST_SLOT_GET_TYPE_ERROR = -1,
};

static enum ast_slot_get_error
ast_slot_try_get_value(
		struct solve_context *ctx, ast_slot_id slot_id,
		type_id require_type, struct object *out_obj);

static enum ast_slot_get_error
ast_slot_try_get_value_type(
		struct solve_context *ctx, ast_slot_id slot_id,
		type_id *out_type);

static enum ast_slot_get_error
ast_slot_try_get_type(
		struct solve_context *ctx, ast_slot_id slot_id,
		type_id *out_type)
{
	struct ast_slot_resolve *slot;
	slot = ast_get_slot(ctx, slot_id);

	if ((slot->flags & AST_SLOT_HAS_TYPE) != 0) {
		return ast_slot_try_get_value_type(
				ctx, slot->type, out_type);
	} else {
		return AST_SLOT_GET_NO_TYPE_SLOT;
	}
}

static enum ast_slot_get_error
ast_slot_try_get_value(
		struct solve_context *ctx, ast_slot_id slot_id,
		type_id require_type, struct object *out_obj)
{
	struct ast_slot_resolve *slot;
	slot = ast_get_slot(ctx, slot_id);

	type_id slot_type = TYPE_UNSET;

	int err;
	err = ast_slot_try_get_type(
			ctx, slot_id, &slot_type);
	if (err != AST_SLOT_GET_OK &&
			err != AST_SLOT_GET_NO_TYPE_SLOT) {
		return err;
	}

	if ((slot->flags & AST_SLOT_HAS_VALUE) == 0) {
		return AST_SLOT_GET_NOT_ENOUGH_INFO;
	}

	struct object value = {0};

	if ((slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
		value.type = ctx->type;
		value.data = &slot->value.type;
	} else {
		value = slot->value.obj;
	}

	if (slot_type != TYPE_UNSET &&
			!type_equals(ctx->vm, value.type, slot_type)) {
		return AST_SLOT_GET_TYPE_ERROR;
	}

	if (require_type != TYPE_UNSET &&
			!type_equals(ctx->vm, value.type, require_type)) {
		return AST_SLOT_GET_TYPE_ERROR;
	}

	*out_obj = value;
	return AST_SLOT_GET_OK;
}

static enum ast_slot_get_error
ast_slot_try_get_value_type(
		struct solve_context *ctx, ast_slot_id slot_id,
		type_id *out_type)
{
	int err;
	struct object obj;
	err = ast_slot_try_get_value(ctx, slot_id, ctx->type, &obj);
	if (err) {
		return err;
	}

	assert_type_equals(ctx->vm, obj.type, ctx->type);
	*out_type = *(type_id *)obj.data;
	return AST_SLOT_GET_OK;
}

static void
ast_slot_solve_impose_constraint(
		struct solve_context *ctx, ast_constraint_id constr_id)
{
	struct ast_slot_constraint *constr;
	constr = ast_get_constraint(ctx, constr_id);

	switch (constr->kind) {
		case AST_SLOT_REQ_ERROR:
			{
				struct ast_slot_resolve *target;
				target = ast_get_real_slot(ctx, constr->target);
				target->flags |= AST_SLOT_HAS_ERROR;
			}
			break;

		case AST_SLOT_REQ_EQUALS:
			ast_slot_join(
					ctx, constr->target, constr->equals);
			break;

		case AST_SLOT_REQ_IS_OBJ:
			ast_solve_apply_value_obj(
					ctx, constr_id,
					constr->target, constr->is.obj);
			break;

		case AST_SLOT_REQ_IS_TYPE:
			ast_solve_apply_value_type(
					ctx, constr_id,
					constr->target, constr->is.type);
			break;

		case AST_SLOT_REQ_IS_CONS:
			ast_solve_apply_cons(
					ctx, constr_id,
					constr->target, constr->is_cons);
			break;

		case AST_SLOT_REQ_CONS:
			panic("TODO: Cons");
			break;

		case AST_SLOT_REQ_TYPE:
			ast_solve_apply_type(
					ctx, constr_id,
					constr->target, constr->type);
			break;


		case AST_SLOT_REQ_IS_FUNC_TYPE:
			ast_solve_apply_func_type(
					ctx, constr_id, constr->target);
			break;

		case AST_SLOT_REQ_MEMBER_NAMED:
			{
				struct ast_slot_member_ref ref = {0};
				ref.named = true;
				ref.name  = constr->member.name;
				ast_solve_apply_member(
						ctx, constr_id, constr->target,
						ref, constr->member.slot);
			}
			break;

		case AST_SLOT_REQ_MEMBER_INDEXED:
			{
				struct ast_slot_member_ref ref = {0};
				ref.named = false;
				ref.index = constr->member.index;
				ast_solve_apply_member(
						ctx, constr_id, constr->target,
						ref, constr->member.slot);
			}
			break;
	}
}

static bool
ast_slot_solve_push_value(struct solve_context *ctx, ast_slot_id slot_id)
{
	struct ast_slot_resolve *slot;
	slot = ast_get_slot(ctx, slot_id);

	if ((slot->flags & AST_SLOT_HAS_SUBST) != 0) {
		return false;
	}

	bool made_change = false;

	if ((slot->flags & (AST_SLOT_HAS_VALUE|AST_SLOT_HAS_TYPE)) ==
			(AST_SLOT_HAS_VALUE|AST_SLOT_HAS_TYPE)) {
		made_change |=
			ast_slot_update_type(ctx, slot->authority.value, slot_id);
	}

	if ((slot->flags & (AST_SLOT_HAS_VALUE|AST_SLOT_HAS_TYPE|AST_SLOT_HAS_CONS)) ==
			AST_SLOT_HAS_TYPE) {
		// TODO: Try determine cons from type
	}

	if ((slot->flags & (AST_SLOT_HAS_VALUE|AST_SLOT_HAS_CONS)) ==
			(AST_SLOT_HAS_VALUE|AST_SLOT_HAS_CONS) &&
			slot->num_members > 0) {
		// Unpack value and push down to the members.
		if ((slot->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
			if ((slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
				if (stg_type_is_func(ctx->vm, slot->value.type)) {
					struct type *func_type;
					func_type = vm_get_type(ctx->vm, slot->value.type);

					struct stg_func_type *func_info;
					func_info = func_type->data;

					for (size_t i = 0; i < slot->num_members; i++) {
						if (slot->members[i].ref.named) {
							printf("Expected indexed members, got a named one (%.*s).\n",
									ALIT(slot->members[i].ref.name));
							continue;
						}

						type_id mbr_type = TYPE_UNSET;

						if (slot->members[i].ref.index == 0) {
							mbr_type = func_info->return_type;
						} else {
							if (slot->members[i].ref.index > func_info->num_params) {
								printf("Attempted to get param %zu from a "
										"function with %zu parameters.\n",
										slot->members[i].ref.index-1,
										func_info->num_params);
								continue;
							}

							mbr_type = func_info->params[slot->members[i].ref.index-1];
						}
						assert(mbr_type != TYPE_UNSET);

						made_change |= ast_solve_apply_value_type(
								ctx, slot->authority.value,
								slot->members[i].slot, mbr_type);
					}
				} else {
					printf("Expected function type, got another type.\n");
				}
			} else {
				printf("Expected function type, got object.\n");
			}

		} else {
			struct object obj = {0};
			if ((slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
				obj.type = ctx->type;
				obj.data = &slot->value.type;
			} else {
				obj = slot->value.obj;
			}

			for (size_t i = 0; i < slot->num_members; i++) {
				if (!slot->members[i].ref.named) {
					printf("Expected named members, got indexed one.\n");
					continue;
				}

				ssize_t param_i;
				param_i = object_cons_find_param(
						slot->cons, slot->members[i].ref.name);
				if (param_i < 0) {
					printf("Cons does not have member %.*s.\n",
							ALIT(slot->members[i].ref.name));
					continue;
				}
				assert(param_i < slot->cons->num_params);

				struct object res = {0};
				res.type = slot->cons->params[i].type;

				struct type *param_type;
				param_type = vm_get_type(ctx->vm, res.type);

				uint8_t buffer[param_type->size];
				memset(buffer, 0, param_type->size);
				res.data = buffer;

				assert(slot->cons->unpack);
				slot->cons->unpack(ctx->vm, slot->cons->data,
						res.data, obj.data, param_i);

				res = register_object(ctx->vm, ctx->env->store, res);

				made_change |= ast_solve_apply_value_obj(
						ctx, slot->authority.value,
						slot->members[i].slot, res);
			}
		}
	}

	if ((slot->flags & (AST_SLOT_HAS_VALUE|AST_SLOT_HAS_CONS)) ==
			AST_SLOT_HAS_CONS) {
		// Try pack this slot's value from its members.

		if ((slot->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
			ssize_t max_param_i = -1;
			for (size_t i = 0; i < slot->num_members; i++) {
				if (slot->members[i].ref.named) {
					printf("Expected indexed arguments for func type cons, got named arg.\n");
					slot->flags |= AST_SLOT_HAS_ERROR;
					continue;
				}

				if (slot->members[i].ref.index > max_param_i) {
					max_param_i = slot->members[i].ref.index;
				}
			}

			assert(max_param_i >= 0);

			type_id arg_types[max_param_i+1];
			bool arg_set[max_param_i+1];
			memset(arg_set, 0, sizeof(bool) * max_param_i+1);

			for (size_t i = 0; i < slot->num_members; i++) {
				if (slot->members[i].ref.named) {
					continue;
				}

				size_t param_i = slot->members[i].ref.index;
				assert(param_i <= max_param_i);

				int err;
				arg_types[param_i] = TYPE_UNSET;
				err = ast_slot_try_get_value_type(
						ctx, slot->members[i].slot, &arg_types[param_i]);
				if (err < 0) {
					slot->flags |= AST_SLOT_HAS_ERROR;
					continue;
				} else if (err > 0) {
					continue;
				}

				arg_set[param_i] = true;
			}

			bool all_args_set = true;
			for (size_t i = 0; i < max_param_i+1; i++) {
				all_args_set &= arg_set[i];
			}

			if (all_args_set) {
				type_id res;
				res = stg_register_func_type(
						ctx->mod->stg_mod, arg_types[0],
						&arg_types[1], max_param_i);

				made_change |= ast_solve_apply_value_type(
						ctx, slot->authority.value,
						slot_id, res);
			}

		} else {
			struct object args[slot->cons->num_params];
			void *arg_data[slot->cons->num_params];
			bool arg_set[slot->cons->num_params];
			memset(arg_set, 0, sizeof(bool) * slot->cons->num_params);

			for (size_t i = 0; i < slot->num_members; i++) {
				if (!slot->members[i].ref.named) {
					printf("Expected named arguments for cons, got indexed arg.\n");
					slot->flags |= AST_SLOT_HAS_ERROR;
					continue;
				}

				ssize_t param_i;
				param_i = object_cons_find_param(
						slot->cons, slot->members[i].ref.name);

				if (param_i < 0) {
					printf("Got unexpected parameter '%.*s' to cons.\n",
							ALIT(slot->members[i].ref.name));
					continue;
				}
				assert(param_i < slot->cons->num_params);

				int err;
				err = ast_slot_try_get_value(
						ctx, slot->members[i].slot,
						TYPE_UNSET, &args[param_i]);
				if (err < 0) {
					slot->flags |= AST_SLOT_HAS_ERROR;
					continue;
				} else if (err > 0) {
					continue;
				}

				arg_data[param_i] = args[param_i].data;
				arg_set[param_i] = true;
			}

			bool all_args_set = true;
			for (size_t i = 0; i < slot->cons->num_params; i++) {
				all_args_set &= arg_set[i];
			}

			if (all_args_set) {
				struct object res = {0};

				assert(slot->cons->pack_type);
				res.type = slot->cons->pack_type(
						ctx->vm, slot->cons->data,
						arg_data, slot->cons->num_params);

				assert(res.type != TYPE_UNSET);

				struct type *res_type;
				res_type = vm_get_type(ctx->vm, res.type);

				uint8_t buffer[res_type->size];
				memset(buffer, 0, res_type->size);
				res.data = buffer;

				assert(slot->cons->pack);
				slot->cons->pack(
						ctx->vm, slot->cons->data, buffer,
						arg_data, slot->cons->num_params);

				res = register_object(
						ctx->vm, ctx->env->store, res);

				made_change |= ast_solve_apply_value_obj(
						ctx, slot->authority.value,
						slot_id, res);
			}
		}
	}

	return made_change;
}

int
ast_slot_verify_constraint(
		struct solve_context *ctx, ast_constraint_id constr_id)
{
	struct ast_slot_constraint *constr;
	constr = ast_get_constraint(ctx, constr_id);

	struct ast_slot_resolve *target;
	target = ast_get_real_slot(ctx, constr->target);

	switch (constr->kind) {
		case AST_SLOT_REQ_ERROR:
			return -1;

		case AST_SLOT_REQ_IS_OBJ:
			// TODO: Handle the case when the type of the object is type.
			assert((target->flags & AST_SLOT_HAS_VALUE) != 0);
			if ((target->flags & AST_SLOT_VALUE_IS_TYPE) != 0 ||
					!obj_equals(ctx->vm, constr->is.obj, target->value.obj)) {
				struct string exp_str, got_str;

				struct object exp_obj = {0};

				if ((target->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
					exp_obj.type = ctx->type;
					exp_obj.data = &target->value.type;
				} else {
					exp_obj = target->value.obj;
				}

				exp_str = obj_repr_to_alloced_string(
						ctx->vm, exp_obj);
				got_str = obj_repr_to_alloced_string(
						ctx->vm, constr->is.obj);

				stg_error(ctx->err, constr->reason.loc,
						"Expected value '%.*s', got '%.*s'",
						LIT(exp_str), LIT(got_str));

				free(exp_str.text);
				free(got_str.text);

				target->flags |= AST_SLOT_HAS_ERROR;

				return -1;
			}
			break;

		case AST_SLOT_REQ_IS_TYPE:
			assert((target->flags & AST_SLOT_HAS_VALUE) != 0);
			if ((target->flags & AST_SLOT_VALUE_IS_TYPE) == 0) {
				struct string exp_str, got_str;

				struct object got_obj = {0};

				got_obj.type = ctx->type;
				got_obj.data = &constr->is.type;

				exp_str = obj_repr_to_alloced_string(
						ctx->vm, target->value.obj);
				got_str = obj_repr_to_alloced_string(
						ctx->vm, got_obj);

				stg_error(ctx->err, constr->reason.loc,
						"Expected object '%.*s', got '%.*s'",
						LIT(exp_str), LIT(got_str));

				free(exp_str.text);
				free(got_str.text);

				target->flags |= AST_SLOT_HAS_ERROR;

				return -1;
			} else if (!type_equals(ctx->vm, constr->is.type, target->value.type)) {
				struct string exp_str, got_str;

				exp_str = type_repr_to_alloced_string(
						ctx->vm, vm_get_type(ctx->vm, target->value.type));
				got_str = type_repr_to_alloced_string(
						ctx->vm, vm_get_type(ctx->vm, constr->is.type));

				stg_error(ctx->err, constr->reason.loc,
						"Expected type '%.*s', got '%.*s'",
						LIT(exp_str), LIT(got_str));

				free(exp_str.text);
				free(got_str.text);

				target->flags |= AST_SLOT_HAS_ERROR;

				return -1;
			}
			return 0;

		case AST_SLOT_REQ_EQUALS:
			// Slots that are supposed to be equal have already been joined
			// together.
			assert(ast_slot_resolve_subst(ctx, constr->target) ==
					ast_slot_resolve_subst(ctx, constr->equals));
			return 0;

		case AST_SLOT_REQ_TYPE:
			assert((target->flags & AST_SLOT_HAS_TYPE) != 0);
			if ((target->flags & AST_SLOT_HAS_VALUE) != 0) {
				type_id expected_type;
				if ((target->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
					expected_type = ctx->type;
				} else {
					expected_type = target->value.type;
				}

				struct ast_slot_resolve *type_slot;
				type_slot = ast_get_real_slot(ctx, target->type);
			}
			return 0;

		case AST_SLOT_REQ_MEMBER_NAMED:
			{
				if ((target->flags & AST_SLOT_HAS_CONS) == 0) {
					stg_error(ctx->err, constr->reason.loc,
							"Attempted to unpack a value that has no constructor.");
					return -1;
				} else if ((target->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
					stg_error(ctx->err, constr->reason.loc,
							"Got invalid parameter '%.*s' to function type constructor.",
							ALIT(constr->member.name));
					return -1;
				}

				int err;
				struct object mbr_val = {0};
				err = ast_slot_try_get_value(
						ctx, constr->member.slot, TYPE_UNSET, &mbr_val);
				if (err < 0) {
					// TODO: Better error message.
					stg_error(ctx->err, constr->reason.loc,
							"Failed to resolve the value of this member.");
					return -1;
				} else if (err > 0) {
					return 1;
				}

				if ((target->flags & AST_SLOT_HAS_VALUE) == 0) {
					return 1;
				}

				struct object target_val;
				err = ast_slot_try_get_value(
						ctx, constr->target, TYPE_UNSET, &target_val);
				if (err < 0) {
					// TODO: Better error message.
					stg_error(ctx->err, constr->reason.loc,
							"Failed to resolve the value of the target.");
					return -1;
				} else if (err > 0) {
					return 1;
				}

				ssize_t param_i;
				param_i = object_cons_find_param(
						target->cons, constr->member.name);

				struct object exp_val = {0};
				exp_val.type = target->cons->params[param_i].type;
				struct type *exp_type;
				exp_type = vm_get_type(ctx->vm, exp_val.type);

				uint8_t buffer[exp_type->size];
				memset(buffer, 0, exp_type->size);
				exp_val.data = buffer;

				assert(target->cons->unpack);
				target->cons->unpack(
						ctx->vm, target->cons->data,
						exp_val.data, target_val.data, param_i);

				if (!obj_equals(ctx->vm, mbr_val, exp_val)) {
					struct string exp_str, got_str;

					exp_str = obj_repr_to_alloced_string(
							ctx->vm, exp_val);
					got_str = obj_repr_to_alloced_string(
							ctx->vm, mbr_val);

					stg_error(ctx->err, constr->reason.loc,
							"Expected '%.*s' to be %.*s, got %.*s.",
							ALIT(target->cons->params[param_i].name),
							LIT(exp_str), LIT(got_str));

					free(exp_str.text);
					free(got_str.text);

					return -1;
				}
			}
			return 0;

		case AST_SLOT_REQ_MEMBER_INDEXED:
			{
				if ((target->flags & AST_SLOT_HAS_CONS) == 0) {
					stg_error(ctx->err, constr->reason.loc,
							"Attempted to unpack a value that has no constructor.");
					return -1;
				} else if ((target->flags & AST_SLOT_IS_FUNC_TYPE) == 0) {
					stg_error(ctx->err, constr->reason.loc,
							"Got invalid parameter %zu to function type constructor.",
							constr->member.index);
					return -1;
				}

				int err;
				type_id mbr_val;
				err = ast_slot_try_get_value_type(
						ctx, constr->member.slot, &mbr_val);
				if (err < 0) {
					// TODO: Better error message.
					stg_error(ctx->err, constr->reason.loc,
							"Failed to resolve the value of this member.");
					return -1;
				} else if (err > 0) {
					return 1;
				}

				if ((target->flags & AST_SLOT_HAS_VALUE) == 0) {
					return 1;
				}

				type_id target_val;
				err = ast_slot_try_get_value_type(
						ctx, constr->target, &target_val);
				if (err < 0) {
					// TODO: Better error message.
					stg_error(ctx->err, constr->reason.loc,
							"Failed to resolve the value of the target.");
					return -1;
				} else if (err > 0) {
					return 1;
				}

				assert(stg_type_is_func(ctx->vm, target_val));
				struct type *target_type;
				target_type = vm_get_type(ctx->vm, target_val);

				struct stg_func_type *target_type_info;
				target_type_info = target_type->data;

				type_id exp_val;
				if (constr->member.index == 0) {
					exp_val = target_type_info->return_type;
				} else if (constr->member.index <= target_type_info->num_params) {
					exp_val = target_type_info->params[constr->member.index-1];
				} else {
					stg_error(ctx->err, constr->reason.loc,
							"Attempted to access function parameter number %zu "
							"of a function that has only %zu parameters.",
							constr->member.index-1, target_type_info->num_params);
					return -1;
				}

				if (!type_equals(ctx->vm, exp_val, mbr_val)) {
					struct string exp_str, got_str;

					exp_str = type_repr_to_alloced_string(
							ctx->vm, vm_get_type(ctx->vm, exp_val));
					got_str = type_repr_to_alloced_string(
							ctx->vm, vm_get_type(ctx->vm, mbr_val));

					if (constr->member.index == 0) {
						exp_val = target_type_info->return_type;
						stg_error(ctx->err, constr->reason.loc,
								"Expected the return value to be %.*s, got %.*s.",
								LIT(exp_str), LIT(got_str));
					} else if (constr->member.index <= target_type_info->num_params) {
						exp_val = target_type_info->params[constr->member.index-1];
						stg_error(ctx->err, constr->reason.loc,
								"Expected parameter %zu to be %.*s, got %.*s.",
								constr->member.index-1, LIT(exp_str), LIT(got_str));
					}

					free(exp_str.text);
					free(got_str.text);

					return -1;
				}
			}
			return 0;

		case AST_SLOT_REQ_IS_CONS:
			assert((target->flags & AST_SLOT_HAS_CONS) != 0);
			if ((target->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
				// TODO: Better error message.
				stg_error(ctx->err, constr->reason.loc,
						"Expected constructor, got function type constructor.");
				return -1;
			} else if (target->cons != constr->is_cons) {
				// TODO: Better error message.
				stg_error(ctx->err, constr->reason.loc,
						"Missmatching constructors.");
				return -1;
			}
			return 0;

		case AST_SLOT_REQ_CONS:
			panic("TODO: Cons");
			break;


		case AST_SLOT_REQ_IS_FUNC_TYPE:
			assert((target->flags & AST_SLOT_HAS_CONS) != 0);
			if ((target->flags & AST_SLOT_IS_FUNC_TYPE) == 0) {
				// TODO: Better error message.
				stg_error(ctx->err, constr->reason.loc,
						"Expected function type, got object constructor.");
				return -1;
			}
			return 0;

	}

	return -1;
}

int
ast_slot_try_solve(
		struct ast_context *ast_ctx, struct ast_module *mod,
		struct ast_env *in_env, struct ast_slot_result *out_result)
{
	struct ast_env _env = {0};
	ast_env_copy(&_env, in_env);

	struct solve_context _ctx = {0};
	struct solve_context *ctx = &_ctx;

	ctx->vm = ast_ctx->vm;
	ctx->err = ast_ctx->err;
	ctx->mod = mod;
	ctx->type = ast_ctx->types.type;
	ctx->env = &_env;
	ctx->num_slots = ctx->env->num_alloced_slots;

	struct ast_slot_resolve _slots[ctx->num_slots];
	memset(_slots, 0, sizeof(struct ast_slot_resolve) * ctx->num_slots);
	ctx->slots = _slots;

#if AST_DEBUG_SLOT_SOLVE
	printf("====== begin solve slots ======\n");
#endif

	size_t num_applied_constraints = 0;
	bool made_progress = true;

	while (made_progress) {
		made_progress = false;

		size_t num_constraints = ast_env_num_constraints(ctx);
		for (ast_constraint_id constr_id = num_applied_constraints;
				constr_id < num_constraints; constr_id++) {
			ast_slot_solve_impose_constraint(
					ctx, constr_id);
		}

		for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
			made_progress |= ast_slot_solve_push_value(
					ctx, slot_id);
		}
	};

	// Verify the solution.
	size_t num_constraints = ast_env_num_constraints(ctx);
	for (ast_constraint_id constr_id = 0;
			constr_id < num_constraints; constr_id++) {
		ast_slot_verify_constraint(
				ctx, constr_id);
	}

#if AST_DEBUG_SLOT_SOLVE
		printf("final slots (%zu slot%s):\n", ctx->num_slots,
				(ctx->num_slots == 1) ? "" : "s");

	for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
		struct ast_slot_resolve *slot;
		slot = ast_get_slot(ctx, slot_id);

		printf("%i:", slot_id);
		if ((slot->flags & AST_SLOT_HAS_SUBST) != 0) {
			printf(" subst %i\n", slot->subst);
			continue;
		}
		if ((slot->flags & AST_SLOT_HAS_ERROR) != 0) {
			printf(" (error)");
		}

		printf("\n -value: ");
		if ((slot->flags & AST_SLOT_HAS_VALUE) != 0) {
			printf("[%i] ", slot->authority.value);
			if ((slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
				printf("type(");
				print_type_repr(ctx->vm, vm_get_type(ctx->vm, slot->value.type));
				printf(")");
			} else {
				print_obj_repr(ctx->vm, slot->value.obj);
			}
		} else {
			printf("none");
		}

		printf("\n -cons: ");
		if ((slot->flags & AST_SLOT_HAS_CONS) != 0) {
			printf("[%i] ", slot->authority.cons);
			if ((slot->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
				printf("func");
			} else {
				printf("%p", (void *)slot->cons);
			}
		} else {
			printf("none");
		}

		printf("\n -type: ");
		if ((slot->flags & AST_SLOT_HAS_TYPE) != 0) {
			printf("[%i] %i", slot->authority.type, slot->type);
		} else {
			printf("none");
		}

		printf("\n -members:\n");
		if (slot->num_members == 0) {
			printf("    (none)");
		}
		for (size_t i = 0; i < slot->num_members; i++) {
			if (slot->members[i].ref.named) {
				printf("    '%.*s': [%i] %i\n",
						ALIT(slot->members[i].ref.name),
						slot->members[i].authority,
						slot->members[i].slot);
			} else {
				printf("    %zu: [%i] %i\n",
						slot->members[i].ref.index,
						slot->members[i].authority,
						slot->members[i].slot);
			}
		}

		printf("\n");
	}
#endif

	// Produce result.
	int num_errors = 0;
	memset(out_result, 0, sizeof(struct ast_slot_result) * ctx->num_slots);
	for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
		struct ast_slot_resolve *slot;
		slot = ast_get_slot(ctx, slot_id);

		struct ast_slot_result *res;
		res = &out_result[slot_id];

		if ((slot->flags & AST_SLOT_HAS_ERROR) != 0) {
			res->result = AST_SLOT_RESULT_ERROR;
			num_errors += 1;
			continue;
		}

		if ((slot->flags & AST_SLOT_HAS_SUBST) != 0) {
			// We will produce the result of substitutions in the next step.
			continue;
		}

		if ((slot->flags & AST_SLOT_HAS_VALUE) != 0) {
			if ((slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
				res->result = AST_SLOT_RESULT_FOUND_VALUE_TYPE;
				res->value.type = slot->value.type;
				res->type = ctx->type;
			} else {
				res->result = AST_SLOT_RESULT_FOUND_VALUE_OBJ;
				res->value.obj = slot->value.obj;
				res->type = res->value.obj.type;
			}
		} else {
			int err;
			err = ast_slot_try_get_type(
					ctx, slot_id, &res->type);
			if (err) {
				res->result = AST_SLOT_RESULT_UNKNOWN;
			} else {
				res->result = AST_SLOT_RESULT_FOUND_TYPE;
			}
		}
	}

	// Produce the result of substitutions by copying from the already produced
	// subst target.
	for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
		struct ast_slot_resolve *slot;
		slot = ast_get_slot(ctx, slot_id);

		struct ast_slot_result *res;
		res = &out_result[slot_id];

		if ((slot->flags & AST_SLOT_HAS_SUBST) != 0) {
			ast_slot_id resolved_id;
			resolved_id = ast_slot_resolve_subst(
					ctx, slot->subst);

			*res = out_result[resolved_id];
		}
	}

#if AST_DEBUG_SLOT_SOLVE
	printf("======  end solve slots  ======\n");
#endif

	return num_errors;
}

ast_slot_id
ast_alloc_slot(struct ast_env *ctx,
		ast_slot_id type, enum ast_env_slot_kind kind)
{
	ast_slot_id res;

	struct ast_env_slot *new_slots;
	size_t new_num_slots;

#if AST_DEBUG_OFFSET_SLOTS
	bool is_new_env = ctx->num_slots == 0;
	if (is_new_env) {
		ctx->num_slots = 1;
	}
#endif

	res = ctx->num_slots;

	new_num_slots = ctx->num_slots + 1;
	new_slots = realloc(ctx->slots, sizeof(struct ast_env_slot) * new_num_slots);

	if (!new_slots) {
		panic("Failed to realloc slots.");
		return AST_BIND_FAILED;
	}

	ctx->num_slots = new_num_slots;
	ctx->slots = new_slots;

#if AST_DEBUG_OFFSET_SLOTS
	if (is_new_env) {
		memset(&ctx->slots[0], 0, sizeof(struct ast_env_slot));
		ctx->slots[0].kind = AST_SLOT_ERROR;
	}
#endif

	memset(&ctx->slots[res], 0, sizeof(struct ast_env_slot));

	ctx->slots[res].type = type;
	ctx->slots[res].kind = kind;

	assert(
			type >= 0 ||
			type == AST_SLOT_TYPE ||
			type == AST_BIND_FAILED);

	return res;
}

static const char *
ast_bind_result_code_name(enum ast_bind_result_code code)
{
	switch (code) {
		case AST_BIND_OK:                    return "ok";
		case AST_BIND_TYPE_MISMATCH:         return "type mismatch";
		case AST_BIND_VALUE_MISMATCH:        return "value mismatch";
		case AST_BIND_TYPE_VALUE_MISMATCH:   return "type value mismatch";
		case AST_BIND_ARRAY_LENGTH_MISMATCH: return "array length mismatch";
		case AST_BIND_OBJ_HAS_NO_MEMBERS:    return "object has no members";
		case AST_BIND_TYPE_HAS_NO_MEMBERS:   return "type has no members";
		case AST_BIND_OBJ_MISSING_MEMBER:    return "object has no such member";
		case AST_BIND_COMPILER_ERROR:        return "compiler error";
	}
	return "invalid code";
}

ast_slot_id
ast_bind_result_to_slot(struct ast_bind_result res)
{
	if (res.code != AST_BIND_OK) {
		printf("[warning] Failed to bind slot: %s.\n",
				ast_bind_result_code_name(res.code));
		return AST_BIND_FAILED;
	}
	return res.ok.result;
}

ast_slot_id
ast_bind_require_ok(struct ast_bind_result res)
{
	if (res.code != AST_BIND_OK) {
		panic("Failed to bind slot: %s.",
				ast_bind_result_code_name(res.code));
		return AST_BIND_FAILED;
	}
	return res.ok.result;
}

static struct ast_bind_result
ast_bind_res_as_type(struct ast_bind_result res)
{
	switch (res.code) {
		case AST_BIND_TYPE_VALUE_MISMATCH:
			res.code = AST_BIND_TYPE_MISMATCH;
			return res;

		default:
			return res;
	}
}

#define BIND_OK(res) (struct ast_bind_result){.code=AST_BIND_OK, .ok={.result=res}}

#define BIND_COMPILER_ERROR (struct ast_bind_result){.code=AST_BIND_COMPILER_ERROR}
#define BIND_VAL_MISMATCH(_old, _new) (struct ast_bind_result){\
	.code=AST_BIND_VALUE_MISMATCH, .value_mismatch={.old=_old, .new=_new}}
#define BIND_TYPE_MISMATCH(_old, _new) (struct ast_bind_result){\
	.code=AST_BIND_TYPE_MISMATCH, .type_mismatch={.old=_old, .new=_new}}
#define BIND_TYPE_VAL_MISMATCH(_old, _new) (struct ast_bind_result){\
	.code=AST_BIND_TYPE_VALUE_MISMATCH, .type_mismatch={.old=_old, .new=_new}}
#define BIND_ARRAY_LENGTH_MISMATCH(_old, _new) (struct ast_bind_result){\
	.code=AST_BIND_ARRAY_LENGTH_MISMATCH, .array_length_mismatch={.old=_old, .new=_new}}
#define BIND_OBJ_NO_MEMBERS(type) (struct ast_bind_result){\
	.code=AST_BIND_OBJ_HAS_NO_MEMBERS, .obj_no_members={.obj_type=type}}
#define BIND_OBJ_MISSING_MEMBER(_name) (struct ast_bind_result){\
	.code=AST_BIND_OBJ_MISSING_MEMBER, .obj_missing_member={.name=_name}}
#define BIND_TYPE_NO_MEMBERS(type) (struct ast_bind_result){\
	.code=AST_BIND_TYPE_HAS_NO_MEMBERS, .obj_no_members={.obj_type=type}}

#define BIND_EXPECT_OK(res)                           \
	do {                                              \
		struct ast_bind_result _local_result = (res); \
		if (_local_result.code != AST_BIND_OK) {      \
			return _local_result;                     \
		}                                             \
	} while (0);

#define TYPE_BIND_EXPECT_OK(res)                      \
	do {                                              \
		struct ast_bind_result _local_result = (res); \
		if (_local_result.code != AST_BIND_OK) {      \
			return ast_bind_res_as_type(_local_result);                     \
		}                                             \
	} while (0);

struct ast_bind_result
ast_try_bind_slot_error(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, AST_BIND_FAILED, AST_SLOT_ERROR);
	} else if (target >= 0) {
		assert(target < env->num_slots);
		env->slots[target].kind = AST_SLOT_ERROR;
	} else {
		target = AST_SLOT_ERROR;
	}

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_wildcard(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		if (type == AST_BIND_NEW) {
			type = ast_bind_require_ok(
					ast_try_bind_slot_wildcard(
						ctx, env, AST_BIND_NEW, AST_SLOT_TYPE));
		}
		target = ast_alloc_slot(env, type, AST_SLOT_WILDCARD);
	} else {
		TYPE_BIND_EXPECT_OK(ast_try_union_slot(
					ctx, env, ast_env_slot(ctx, env, target).type, type));
	}

#if AST_DEBUG_BINDS
	printf("bind %i=wildcard ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_const(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct object obj)
{
	if (type_equals(ctx->vm, obj.type, ctx->types.type)) {
		return ast_try_bind_slot_const_type(
				ctx, env, target,
				// TODO: We should have a procedure to unpack type object data.
				*(type_id *)obj.data);
	}

	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env,
				ast_bind_require_ok(
					ast_try_bind_slot_const_type(
					ctx, env, AST_BIND_NEW, obj.type)),
				AST_SLOT_CONST);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				{
					// TODO: Name?
					env->slots[target].kind = AST_SLOT_CONST;

					struct ast_bind_result res;
					res = ast_try_bind_slot_const_type(ctx, env,
							target_slot.type, obj.type);
					TYPE_BIND_EXPECT_OK(res);
					env->slots[target].type = res.ok.result;
				}
				break;

			case AST_SLOT_CONST:
				{
					if (target_slot.const_object.type != obj.type) {
#if AST_BIND_ERROR_DEBUG_PRINT
						printf("Warning: Attempted to bind CONST with type '");
						print_type_repr(ctx->vm, vm_get_type(ctx->vm, obj.type));
						printf("' over CONST with type '");
						print_type_repr(ctx->vm, vm_get_type(ctx->vm,
									target_slot.const_object.type));
						printf("'. (bind %i)\n", target);
#endif
						return BIND_TYPE_MISMATCH(target_slot.const_object.type, obj.type);
					}

					if (!obj_equals(ctx->vm, target_slot.const_object, obj)) {
#if AST_BIND_ERROR_DEBUG_PRINT
						printf("Warning: Attempted to bind CONST '");
						print_obj_repr(ctx->vm, obj);
						printf("' over CONST '");
						print_obj_repr(ctx->vm, target_slot.const_object);
						printf("'. (bind %i)\n", target);
#endif
						return BIND_VAL_MISMATCH(target_slot.const_object, obj);
					}
				}
				break;

			case AST_SLOT_CONS:
				{
					struct type *type = vm_get_type(ctx->vm, obj.type);

					if (!type->obj_def) {
#if AST_BIND_ERROR_DEBUG_PRINT
						printf("Warning: Attempted to bind CONS over CONST with "
								"object that does not have a constructor (bind %i).\n",
								target);
#endif
						return BIND_COMPILER_ERROR;
					}

					if (!target_slot.cons.def) {
						struct ast_bind_result res;
						res = ast_try_bind_slot_cons(
								ctx, env, target, type->obj_def);
						BIND_EXPECT_OK(res);
						target = res.ok.result;
					} else {
						if (target_slot.cons.def != type->obj_def) {
							printf("Warning: Attempted to bind CONS over CONST with "
									"object that does not match the one in CONS.\n");
							return BIND_COMPILER_ERROR;
						}
					}

					struct ast_object_def *def = type->obj_def;

					for (size_t i = 0; i < def->num_params; i++) {
						struct object member;

						if (def->unpack_func) {
							// NOTE: TYPE_NONE is used for the function cons
							// params param to allow a parametric type
							// (type[$N]). This kind of parameters have to be
							// handled by the old def->unpack function and can
							// not be compiled into bytecode.
							assert(def->params[i].type != TYPE_NONE);

							struct type *param_type;
							param_type = vm_get_type(ctx->vm, def->params[i].type);

							uint8_t buffer[param_type->size];

							def->unpack_func(ctx->vm, def->data, buffer,
									obj.data, def->params[i].param_id);

							member.type = def->params[i].type;
							member.data = buffer;
							member = register_object(ctx->vm, env->store, member);
						} else {
							assert(def->unpack);
							member = def->unpack(ctx, env, def,
									def->params[i].param_id, obj);
							member = register_object(ctx->vm, env->store, member);
						}

						struct ast_bind_result res;

						ast_slot_id member_slot;
						res = ast_try_unpack_arg_named(ctx, env, target,
								AST_BIND_NEW, def->params[i].name);
						BIND_EXPECT_OK(res);
						member_slot = res.ok.result;

						res = ast_try_bind_slot_const(
								ctx, env, member_slot, member);
						BIND_EXPECT_OK(res);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return BIND_OK(target);

			case AST_SLOT_CONS_ARRAY:
				{
					struct type *type = vm_get_type(ctx->vm, obj.type);
					struct ast_array_def *def = type->base->array_def;

					if (!def) {
#if AST_BIND_ERROR_DEBUG_PRINT
						printf("Warning: Attempted to bind CONS_ARRAY over CONST with "
								"object that does not have an array constructor.\n");
#endif
						return BIND_COMPILER_ERROR;
					}


					for (size_t i = 0; i < target_slot.cons_array.num_members; i++) {
						struct object member;

						member = def->unpack(ctx, env, def, i, obj);
						member = register_object(ctx->vm, env->store, member);

						ast_slot_id member_slot;
						member_slot = target_slot.cons_array.members[i];

						ast_slot_id new_member_slot;
						struct ast_bind_result res;
						res = ast_try_bind_slot_const(
								ctx, env, member_slot, member);
						BIND_EXPECT_OK(res);
						new_member_slot = res.ok.result;

						ast_substitute(ctx, env, new_member_slot, member_slot);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return BIND_OK(target);

			default:
#if AST_BIND_ERROR_DEBUG_PRINT
				panic("Warning: Attempted to bind CONST over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
#endif
				return BIND_COMPILER_ERROR;
		}
	}

	env->slots[target].const_object = obj;

#if AST_DEBUG_BINDS
	printf("bind %i=const ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_const_type(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		type_id type)
{
	if (target == AST_BIND_NEW) {
		if (type_equals(ctx->vm, type, ctx->types.type)) {
			target = AST_SLOT_TYPE;
		} else {
			target = ast_alloc_slot(env, AST_SLOT_TYPE, AST_SLOT_CONST_TYPE);
		}
	} else if (target >= 0 && type_equals(ctx->vm, type, ctx->types.type)) {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				TYPE_BIND_EXPECT_OK(
						ast_try_bind_slot_const_type(
							ctx, env, target_slot.type, ctx->types.type));

				ast_substitute(ctx, env, AST_SLOT_TYPE, target);
				return BIND_OK(AST_SLOT_TYPE);

			case AST_SLOT_CONST_TYPE:
				return BIND_TYPE_VAL_MISMATCH(
						target_slot.const_type, ctx->types.type);

			default:
#if AST_BIND_ERROR_DEBUG_PRINT
				printf("Warning: Attempted to bind CONST_TYPE over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
#endif
				return BIND_COMPILER_ERROR;
		}

#if AST_DEBUG_BINDS
		printf("=== bind const type of type, %i -> -1: \n", target);
#endif
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				{
					struct ast_bind_result res;
					res = ast_try_bind_slot_const_type(ctx, env,
							env->slots[target].type, ctx->types.type);
					TYPE_BIND_EXPECT_OK(res);
					env->slots[target].type = res.ok.result;

					// TODO: Name?
					env->slots[target].kind = AST_SLOT_CONST_TYPE;
				}
				break;

			case AST_SLOT_CONST_TYPE:
				if (!type_equals(ctx->vm, target_slot.const_type, type)) {
#if AST_BIND_ERROR_DEBUG_PRINT
					printf("Warning: Attempted to bind CONST_TYPE with type '");
					print_type_repr(ctx->vm, vm_get_type(ctx->vm, type));
					printf("' over CONST_TYPE with type '");
					print_type_repr(ctx->vm, vm_get_type(ctx->vm, target_slot.const_type));
					printf("'. (bind %i)\n", target);
#endif
					return BIND_TYPE_VAL_MISMATCH(target_slot.const_type, type);
				}
				break;

			case AST_SLOT_CONS:
				{
					struct type *type_inst = vm_get_type(ctx->vm, type);

					struct object type_obj = {0};
					type_obj.type = ctx->types.type;
					type_obj.data = &type;

					struct ast_object_def *type_def;
					type_def = ast_try_determine_object_def(
							ctx, env, type_obj, type_inst->type_def, target_slot.cons.def);

					if (!type_def) {
#if AST_BIND_ERROR_DEBUG_PRINT
						printf("Warning: Attempted to bind a type with no type "
								"def over a CONS with no def <");
						print_type_repr(ctx->vm, type_inst);
						printf(">.\n");
#endif
						return BIND_TYPE_NO_MEMBERS(type);
					}

					for (size_t i = 0; i < type_def->num_params; i++) {
						struct object member;

						member = type_def->unpack(ctx, env, type_def,
								type_def->params[i].param_id, type_obj);
						member = register_object(ctx->vm, env->store, member);

						struct ast_bind_result res;

						ast_slot_id member_slot;
						res = ast_try_unpack_arg_named(ctx, env, target,
								AST_BIND_NEW, type_def->params[i].name);
						BIND_EXPECT_OK(res);
						member_slot = res.ok.result;

						res = ast_try_bind_slot_const(
								ctx, env, member_slot, member);
						BIND_EXPECT_OK(res);
					}
				}
#if AST_DEBUG_BINDS
				printf("bind %i=const_type ", target);
				ast_print_slot(ctx, env, target);
				printf("\n");
#endif
				return BIND_OK(target);

			default:
#if AST_BIND_ERROR_DEBUG_PRINT
				printf("Warning: Attempted to bind CONST_TYPE over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
#endif
				return BIND_COMPILER_ERROR;
		}
	}

	if (target != AST_SLOT_TYPE) {
		env->slots[target].const_type = type;
	}

#if AST_DEBUG_BINDS
	printf("bind %i=const_type(%lu) ", target, type);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_param(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		int64_t param_index, ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, type, AST_SLOT_PARAM);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				// TODO: Union types
				// TODO: Name?
				env->slots[target].kind = AST_SLOT_PARAM;
				break;

			default:
#if AST_BIND_ERROR_DEBUG_PRINT
				printf("Warning: Attempted to bind PARAM over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
#endif
				return BIND_COMPILER_ERROR;
		}
	}

	env->slots[target].param_index = param_index;

#if AST_DEBUG_BINDS
	printf("bind %i=param(index=%zu) ", target, param_index);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_templ(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, type, AST_SLOT_TEMPL);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_TEMPL;
				TYPE_BIND_EXPECT_OK(ast_try_union_slot(
							ctx, env, env->slots[target].type, type));
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_SUBST:
#if AST_BIND_ERROR_DEBUG_PRINT
				printf("Warning: Attempted to bind TEMPL over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
#endif
				return BIND_COMPILER_ERROR;

			default:
				break;
		}
	}

#if AST_DEBUG_BINDS
	printf("bind %i=templ ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_closure(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		ast_slot_id type)
{
	if (target == AST_BIND_NEW) {
		if (type == AST_BIND_NEW) {
			struct ast_bind_result res;
			res = ast_try_bind_slot_wildcard(ctx, env,
					AST_BIND_NEW, AST_SLOT_TYPE);
			type = res.ok.result;
		}
		target = ast_alloc_slot(env, type, AST_SLOT_CLOSURE);
	} else {
		struct ast_env_slot target_slot;

		target_slot = ast_env_slot(ctx, env, target);
		switch (target_slot.kind) {
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_CLOSURE;
				TYPE_BIND_EXPECT_OK(ast_try_union_slot(
							ctx, env, env->slots[target].type, type));
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_SUBST:
#if AST_BIND_ERROR_DEBUG_PRINT
				printf("Warning: Attempted to bind CLOSURE over %s. (bind %i)\n",
						ast_slot_name(target_slot.kind), target);
#endif
				return BIND_COMPILER_ERROR;

			default:
				break;
		}
	}

#if AST_DEBUG_BINDS
	printf("bind %i=closure ", target);
	ast_print_slot(ctx, env, target);
	printf("\n");
#endif

	return BIND_OK(target);
}

struct ast_union_context {
	struct ast_context *ctx;
	ast_slot_id *slot_map;
	size_t slot_map_len;
	bool slot_map_freeable;

	bool copy_mode;
};

static struct ast_bind_result
ast_union_slot_internal(struct ast_union_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot);

struct ast_bind_result
ast_try_bind_slot_cons(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct ast_object_def *def)
{

	ast_slot_id type_slot = AST_BIND_NEW;

	if (target == AST_BIND_NEW) {
		type_slot = AST_BIND_NEW;
	} else {
		struct ast_env_slot old_slot = ast_env_slot(ctx, env, target);

		switch (old_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_CONS;
				env->slots[target].cons.args = NULL;
				env->slots[target].cons.num_present_args = 0;
				env->slots[target].cons.def = NULL;
				type_slot = old_slot.type;
				break;

			case AST_SLOT_CONS:
				if (def && old_slot.cons.def && old_slot.cons.def != def) {
#if AST_BIND_ERROR_DEBUG_PRINT
					printf("Warning: Attempted to bind CONS of %p over %p. (bind %i)\n",
							(void *)def, (void *)old_slot.cons.def, target);
#endif
					return BIND_COMPILER_ERROR;
				} else if (def == old_slot.cons.def) {
					return BIND_OK(target);
				/*
				} else if (def && env->slots[target].cons.num_present_args > def->num_params) {
#if AST_BIND_ERROR_DEBUG_PRINT
					printf("Warning: Attempted to bind CONS with %zu parameters to "
							"CONS with %zu arguments. (bind %i)\n",
							def->num_params, env->slots[target].cons.num_present_args,
							target);
#endif
					return AST_BIND_FAILED;
				*/
				}
				type_slot = old_slot.type;
				break;

			case AST_SLOT_CONST:
				{
					struct object slot_obj = old_slot.const_object;
					struct type *slot_obj_type =
						vm_get_type(ctx->vm, slot_obj.type);

					if (!slot_obj_type->obj_def) {
#if AST_BIND_ERROR_DEBUG_PRINT
						printf("Warning: Attempted to unpack a object that cannot be "
								"unpacked (missing def).\n");
#endif
						return BIND_COMPILER_ERROR;
					}

					if (def && slot_obj_type->obj_def != def) {
#if AST_BIND_ERROR_DEBUG_PRINT
						printf("Warning: Attempted to bind CONS with def %p over "
								"CONST with def %p.\n",
								(void *)def,
								(void *)slot_obj_type->obj_def);
#endif
					}

					// We first rebind the target slot to wildcard to allow us
					// to use bind_slot_cons to correctly instantiate it as a
					// cons slot. Then we apply the const object on top of the
					// cons.
					env->slots[target].kind = AST_SLOT_WILDCARD;
					env->slots[target].const_object.type = 0;
					env->slots[target].const_object.data = NULL;

					struct ast_bind_result res;
					res = ast_try_bind_slot_cons(
							ctx, env, target, slot_obj_type->obj_def);
					BIND_EXPECT_OK(res);
					res = ast_try_bind_slot_const(
							ctx, env, res.ok.result, slot_obj);
					BIND_EXPECT_OK(res);
					target = res.ok.result;
				}
				return BIND_OK(target);

			case AST_SLOT_CONST_TYPE:
				{
					type_id slot_val = old_slot.const_type;
					struct type *slot_val_type =
						vm_get_type(ctx->vm, slot_val);

					struct object obj = {0};
					obj.type = ctx->types.type;
					obj.data = &slot_val;

					struct ast_object_def *type_def;
					type_def = ast_try_determine_object_def(
							ctx, env, obj, slot_val_type->type_def, def);

					if (!type_def) {
						if (!slot_val_type->type_def && !def) {
#if AST_BIND_ERROR_DEBUG_PRINT
							printf("Warning: Attempted to unpack a type with no type "
									"def over a CONS with no def <");
							print_type_repr(ctx->vm, slot_val_type);
							printf("> (%i).\n", target);
#endif
							return BIND_TYPE_NO_MEMBERS(slot_val);
						} else {
#if AST_BIND_ERROR_DEBUG_PRINT
							printf("Warning: Attempted to bind type with type def "
									"over CONS with mismatching def. <");
							print_type_repr(ctx->vm, slot_val_type);
							printf("> (%i). ", target);
							ast_print_slot(ctx, env, target);
							printf("\n");
#endif
							// TODO: Figure out a way of getting the type of
							// the def for the error message.
							return BIND_TYPE_VAL_MISMATCH(slot_val, TYPE_UNSET);
						}
					}

					// We first rebind the target slot to wildcard to allow us
					// to use bind_slot_cons to correctly instantiate it as a
					// cons slot. Then we apply the const object on top of the
					// cons.
					env->slots[target].kind = AST_SLOT_WILDCARD;
					env->slots[target].const_type = 0;

					struct ast_bind_result res;
					res = ast_try_bind_slot_cons(
							ctx, env, target, type_def);
					BIND_EXPECT_OK(res);
					res = ast_try_bind_slot_const_type(
							ctx, env, res.ok.result, slot_val);
					BIND_EXPECT_OK(res);
					target = res.ok.result;
				}
				return BIND_OK(target);

			default:
#if AST_BIND_ERROR_DEBUG_PRINT
				printf("Warning: Attempted to bind CONS over %s. (bind %i)\n",
						ast_slot_name(old_slot.kind), target);
#endif
				return BIND_COMPILER_ERROR;
		}
	}

	size_t num_map_slots = 0;

	if (def) {
		num_map_slots = def->env.num_slots;
	}

	ast_slot_id slot_map[num_map_slots];
	struct ast_union_context cpy_ctx = {0};

	if (def) {
		for (size_t i = 0; i < def->env.num_slots; i++) {
			slot_map[i] = AST_SLOT_NOT_FOUND;
		}

		cpy_ctx.ctx = ctx;
		cpy_ctx.slot_map = slot_map;
		cpy_ctx.slot_map_len = num_map_slots;
		cpy_ctx.copy_mode = true;

		struct ast_bind_result res;

		res = ast_union_slot_internal(
				&cpy_ctx, env, type_slot, &def->env, def->ret_type);
		TYPE_BIND_EXPECT_OK(res);
		type_slot = res.ok.result;
	} else {
		struct ast_bind_result res;
		res = ast_try_bind_slot_wildcard(
				ctx, env, type_slot, AST_SLOT_TYPE);
		TYPE_BIND_EXPECT_OK(res);
		type_slot = res.ok.result;
	}

	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env, type_slot, AST_SLOT_CONS);
	} else {
		env->slots[target].type = type_slot;
	}

	env->slots[target].cons.def = def;

#define AST_OBJ_DUPLICATE_CHECK 1

	if (def) {
		bool valid_params = true;

		// Check that all arguments are valid for the given def.
		for (size_t arg_i = 0; arg_i < env->slots[target].cons.num_present_args; arg_i++) {
			bool found = false;
			struct atom *arg_name = env->slots[target].cons.args[arg_i].name;
			printf("arg '%.*s'\n", ALIT(arg_name));

			for (size_t param_i = 0; param_i < def->num_params; param_i++) {
				if (def->params[param_i].name == arg_name) {
#if AST_OBJ_DUPLICATE_CHECK
					if (found) {
						panic("Duplicate argument '%.*s' to object %p. (bind %i)",
								ALIT(arg_name), def, target);
					}
					found = true;
#else
					found = true;
					break;
#endif
				}
			}

			if (!found) {
				printf("Object of type %p got invalid argument '%.*s'. (bind %i)\n",
						(void *)def, ALIT(arg_name), target);
				valid_params = false;
			}
		}

		if (!valid_params) {
			printf("Expected ");
			for (size_t param_i = 0; param_i < def->num_params; param_i++) {
				if (param_i != 0 && param_i == def->num_params-1) {
					printf(" and ");
				} else if (param_i != 0) {
					printf(", ");
				}
				printf("%.*s", ALIT(def->params[param_i].name));
			}
			printf("\n");
			return BIND_COMPILER_ERROR;
		}

		// Bind the missing arguments.
		assert(env->slots[target].cons.num_present_args <= def->num_params);
		struct ast_object_arg *tmp_args =
			realloc(env->slots[target].cons.args,
					def->num_params * sizeof(struct ast_object_arg));

		if (!tmp_args) {
			panic("Failed to realloc object args.");
			return BIND_COMPILER_ERROR;
		}

		size_t num_filled_args = env->slots[target].cons.num_present_args;

		env->slots[target].cons.args = tmp_args;

		for (size_t param_i = 0; param_i < def->num_params; param_i++) {
			struct atom *param_name = def->params[param_i].name;

			bool found = false;
			size_t arg;
			for (size_t arg_i = 0; arg_i < num_filled_args; arg_i++) {
				if (env->slots[target].cons.args[arg_i].name == param_name) {
					found = true;
					arg = arg_i;
					break;
				}
			}

			if (!found) {
				arg = env->slots[target].cons.num_present_args;
				env->slots[target].cons.num_present_args += 1;
				assert(env->slots[target].cons.num_present_args <= def->num_params);

				env->slots[target].cons.args[arg].name = param_name;
				env->slots[target].cons.args[arg].slot = AST_BIND_NEW;
			}

			struct ast_bind_result res;
			res = ast_union_slot_internal(&cpy_ctx,
						env, env->slots[target].cons.args[arg].slot,
						&def->env, def->params[param_i].slot);
			BIND_EXPECT_OK(res);
			env->slots[target].cons.args[arg].slot = res.ok.result;
		}

		assert(env->slots[target].cons.num_present_args == def->num_params);
	}

#if AST_DEBUG_BINDS
	{
		struct ast_env_slot *slot = &env->slots[target];
		printf("bind %i=Cons[%p](", target, (void *)def);
		for (size_t j = 0; j < slot->cons.num_present_args; j++) {
			if (j != 0)
				printf(", ");
			printf("%.*s=%i",
					ALIT(slot->cons.args[j].name),
					slot->cons.args[j].slot);
		}
		printf(") ");
		ast_print_slot(ctx, env, target);
		printf("\n");
	}
#endif

	return BIND_OK(target);
}

struct ast_bind_result
ast_try_bind_slot_cons_array(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		ast_slot_id *members, size_t num_members, ast_slot_id member_type_slot)
{
	if (target == AST_BIND_NEW) {
		target = ast_alloc_slot(env,
				ast_bind_require_ok(
					ast_try_bind_slot_wildcard(
						ctx, env, AST_BIND_NEW, AST_SLOT_TYPE)),
				AST_SLOT_CONS_ARRAY);
		env->slots[target].cons_array.member_count = AST_BIND_NEW;
		env->slots[target].cons_array.member_type = AST_BIND_NEW;
		env->slots[target].cons_array.members = NULL;
	} else {
		struct ast_env_slot old_slot = ast_env_slot(ctx, env, target);

		switch (old_slot.kind) {
			case AST_SLOT_TEMPL:
			case AST_SLOT_WILDCARD:
				env->slots[target].kind = AST_SLOT_CONS_ARRAY;
				env->slots[target].cons_array.member_count = AST_BIND_NEW;
				env->slots[target].cons_array.member_type = AST_BIND_NEW;
				env->slots[target].cons_array.members = NULL;
				break;

			case AST_SLOT_CONS_ARRAY:
				if (old_slot.cons_array.num_members != num_members) {
#if AST_BIND_ERROR_DEBUG_PRINT
					printf("Warning: Attempted to bind CONS_ARRAY with length %zu "
							"over CONS_ARRAY with length %zu. (bind %i)\n",
							num_members, old_slot.cons_array.num_members, target);
#endif
					return BIND_ARRAY_LENGTH_MISMATCH(
							old_slot.cons_array.num_members, num_members);
				}
				break;

			case AST_SLOT_CONST:
				{
					struct object slot_obj = old_slot.const_object;
					struct type *slot_obj_type =
						vm_get_type(ctx->vm, slot_obj.type);
					struct stg_array_type *array_info =
						(struct stg_array_type *)slot_obj_type->data;

					if (!slot_obj_type->base->array_def) {
#if AST_BIND_ERROR_DEBUG_PRINT
						printf("Warning: Attempted to unpack an object as an "
								"array that cannot be unpacked (missing def).\n");
#endif
						return BIND_COMPILER_ERROR;
					}

					// We first rebind the target slot to wildcard to allow us
					// to use bind_slot_cons to correctly instantiate it as a
					// cons slot. Then we apply the const object on top of the
					// cons.
					env->slots[target].kind = AST_SLOT_WILDCARD;
					env->slots[target].const_object.type = 0;
					env->slots[target].const_object.data = NULL;

					struct ast_bind_result res;

					ast_slot_id member_type;
					res = ast_try_bind_slot_const_type(
							ctx, env, AST_BIND_NEW, array_info->member_type);
					BIND_EXPECT_OK(res);
					member_type = res.ok.result;

					res = ast_try_bind_slot_cons_array(
							ctx, env, target, NULL,
							array_info->length, member_type);
					BIND_EXPECT_OK(res);
					res = ast_try_bind_slot_const(
							ctx, env, res.ok.result,
							slot_obj);
					BIND_EXPECT_OK(res);
					target = res.ok.result;
				}
				return BIND_OK(target);

			default:
#if AST_BIND_ERROR_DEBUG_PRINT
				printf("Warning: Attempted to bind CONS_ARRAY over %s. (bind %i)\n",
						ast_slot_name(old_slot.kind), target);
#endif
				return BIND_COMPILER_ERROR;
		}

		env->slots[target].kind = AST_SLOT_CONS_ARRAY;
	}

	struct ast_bind_result res;
	res = ast_try_bind_slot_cons(
			ctx, env, env->slots[target].type,
			ctx->cons.array);
	TYPE_BIND_EXPECT_OK(res);
	env->slots[target].type = res.ok.result;

	ast_slot_id old_member_count = env->slots[target].cons_array.member_count;

	res = ast_try_unpack_arg_named(ctx, env,
				env->slots[target].type,
				AST_BIND_NEW,
				ctx->atoms.array_cons_arg_count);
	TYPE_BIND_EXPECT_OK(res);
	env->slots[target].cons_array.member_count = res.ok.result;

	assert(old_member_count == AST_BIND_NEW ||
			old_member_count == env->slots[target].cons_array.member_count);

	ast_slot_id old_member_type = env->slots[target].cons_array.member_type;
	res = ast_try_unpack_arg_named(ctx, env,
				env->slots[target].type,
				AST_BIND_NEW,
				ctx->atoms.array_cons_arg_type);
	TYPE_BIND_EXPECT_OK(res);

	env->slots[target].cons_array.member_type = res.ok.result;
	assert(old_member_type == AST_BIND_NEW || old_member_type == AST_SLOT_TYPE ||
			old_member_type == env->slots[target].cons_array.member_type);

	res = ast_try_bind_slot_const(
			ctx, env, env->slots[target].cons_array.member_count,
			ast_register_integer(ctx, env, num_members));
	BIND_EXPECT_OK(res);
	env->slots[target].cons_array.member_count = res.ok.result;

	res = ast_try_union_slot(ctx, env,
				env->slots[target].cons_array.member_type,
				member_type_slot);
	TYPE_BIND_EXPECT_OK(res);
	env->slots[target].cons_array.member_type = res.ok.result;

	if (!env->slots[target].cons_array.members) {
		env->slots[target].cons_array.num_members = num_members;
		env->slots[target].cons_array.members =
			calloc(num_members, sizeof(ast_slot_id));
		for (size_t i = 0; i < num_members; i++) {
			env->slots[target].cons_array.members[i] = AST_BIND_NEW;
		}
	}

	if (members) {
		for (size_t i = 0; i < num_members; i++) {
			struct ast_bind_result res;
			res = ast_try_union_slot(ctx, env,
					env->slots[target].cons_array.members[i],
					members[i]);
			BIND_EXPECT_OK(res);
			env->slots[target].cons_array.members[i] = res.ok.result;
		}
	} else {
		for (size_t i = 0; i < num_members; i++) {
			struct ast_bind_result res;
			res = ast_try_bind_slot_wildcard(ctx, env,
					env->slots[target].cons_array.members[i],
					env->slots[target].cons_array.member_type);
			BIND_EXPECT_OK(res);
			env->slots[target].cons_array.members[i] = res.ok.result;
		}
	}

#if AST_DEBUG_BINDS
	{
		struct ast_env_slot *slot = &env->slots[target];
		printf("bind %i=[", target);
		for (size_t j = 0; j < slot->cons_array.num_members; j++) {
			if (j != 0)
				printf(", ");
			printf("%i", slot->cons_array.members[j]);
		}
		printf("] type-slot=%i count-slot=%i ",
				slot->cons_array.member_type,
				slot->cons_array.member_count);
		ast_print_slot(ctx, env, target);
		printf("\n");
	}
#endif

	return BIND_OK(target);
}

ast_slot_id
ast_bind_slot_error(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_error(
				ctx, env, target));
}

ast_slot_id
ast_bind_slot_wildcard(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_wildcard(
				ctx, env, target, type));
}

ast_slot_id
ast_bind_slot_const(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct object obj)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_const(
				ctx, env, target, obj));
}

ast_slot_id
ast_bind_slot_const_type(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		type_id tid)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_const_type(
				ctx, env, target, tid));
}

ast_slot_id
ast_bind_slot_param(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		int64_t param_index, ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_param(
				ctx, env, target, param_index, type));
}

ast_slot_id
ast_bind_slot_templ(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_templ(
				ctx, env, target, type));
}

ast_slot_id
ast_bind_slot_closure(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		ast_slot_id type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_closure(
				ctx, env, target, type));
}

ast_slot_id
ast_bind_slot_cons(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		struct ast_object_def *def)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_cons(
				ctx, env, target, def));
}

ast_slot_id
ast_bind_slot_cons_array(struct ast_context *ctx,
		struct ast_env *env, ast_slot_id target,
		ast_slot_id *members, size_t num_members, ast_slot_id member_type)
{
	return ast_bind_result_to_slot(
			ast_try_bind_slot_cons_array(
				ctx, env, target,
				members, num_members, member_type));
}


struct ast_bind_result
ast_try_unpack_arg_named(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id obj, ast_slot_id target, struct atom *arg_name)
{
	struct ast_env_slot slot = ast_env_slot(ctx, env, obj);

	if (slot.kind == AST_SLOT_CONST ||
			slot.kind == AST_SLOT_CONST_TYPE) {
		struct ast_bind_result res;
		res = ast_try_bind_slot_cons(
				ctx, env, obj, NULL);
		BIND_EXPECT_OK(res);
		obj = res.ok.result;

		slot = ast_env_slot(ctx, env, obj);
	} else if (slot.kind != AST_SLOT_CONS) {
#if AST_BIND_ERROR_DEBUG_PRINT
		printf("Warning: Attempted to unpack named argument from %s slot.\n",
				ast_slot_name(slot.kind));
#endif
		return BIND_COMPILER_ERROR;
	}

	// Check that the object does not have an indirection.
	assert(env->slots[obj].kind == AST_SLOT_CONS);

	for (size_t i = 0; i < slot.cons.num_present_args; i++) {
		if (slot.cons.args[i].name == arg_name) {
			ast_slot_id res;
			res = slot.cons.args[i].slot;
			if (target != AST_BIND_NEW) {
				struct ast_bind_result bres;
				bres = ast_try_union_slot(ctx, env,
						res, target);
				BIND_EXPECT_OK(bres);
				res = bres.ok.result;
			}
			return BIND_OK(res);
		}
	}

	if (slot.cons.def) {
		// If the argument would be present in the object definition, it should
		// already have been found on the object.
#if AST_BIND_ERROR_DEBUG_PRINT
		printf("Warning: Attempted to unpack non-existent member %.*s.\n",
				ALIT(arg_name));
#endif
		return BIND_OBJ_MISSING_MEMBER(arg_name);
	} else {
		struct ast_object_arg *tmp_args;
		size_t tmp_num_args = slot.cons.num_present_args + 1;
		tmp_args = realloc(slot.cons.args, tmp_num_args * sizeof(struct ast_object_arg));

		assert(tmp_args);

		env->slots[obj].cons.args = tmp_args;

		tmp_args[tmp_num_args - 1].name = arg_name;
		if (target == AST_BIND_NEW) {
			struct ast_bind_result res;
			res = ast_try_bind_slot_wildcard(
					ctx, env, AST_BIND_NEW,
					ast_bind_require_ok(
						ast_try_bind_slot_wildcard(
							ctx, env, AST_BIND_NEW,
							AST_SLOT_TYPE)));
			BIND_EXPECT_OK(res);
			tmp_args[tmp_num_args - 1].slot = res.ok.result;
		} else {
			tmp_args[tmp_num_args - 1].slot = target;
			// Make sure the type of the type is type.
			TYPE_BIND_EXPECT_OK(ast_try_bind_slot_const_type(ctx, env,
					ast_env_slot(ctx, env,
						ast_env_slot(ctx, env, target).type).type,
					ctx->types.type));
		}

		env->slots[obj].cons.num_present_args = tmp_num_args;

		return BIND_OK(tmp_args[tmp_num_args - 1].slot);
	}
}

ast_slot_id
ast_unpack_arg_named(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id obj, ast_slot_id target, struct atom *name)
{
	return ast_bind_result_to_slot(
			ast_try_unpack_arg_named(
				ctx, env, obj, target, name));
}

#if AST_DEBUG_UNION
static void
print_indent(int depth)
{
	for (int i = 0; i < depth; ++i) {
		printf("| ");
	}
}
#endif

static struct ast_bind_result
ast_union_slot_internal(struct ast_union_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot)
{
#if AST_DEBUG_UNION
	static int union_depth = -1;

	union_depth += 1;

	print_indent(union_depth);
	printf("union (%p)%i -> (%p)%i\n", (void *)src, src_slot, (void *)dest, target);
#endif

	if (src_slot == AST_SLOT_TYPE) {
#if AST_DEBUG_UNION
		print_indent(union_depth);
		printf(" -> -1 (type)\n");
		union_depth -= 1;
#endif
		return ast_try_bind_slot_const_type(
				ctx->ctx, dest, target,
				ctx->ctx->types.type);
	} else if (src_slot == AST_BIND_FAILED) {
#if AST_DEBUG_UNION
		print_indent(union_depth);
		printf(" -> fail\n");
		union_depth -= 1;
#endif
		return BIND_OK(AST_BIND_FAILED);
	}

	if (dest == src && target == src_slot && !ctx->copy_mode) {
#if AST_DEBUG_UNION
	print_indent(union_depth);
		printf(" -> %i\n", target);
		union_depth -= 1;
#endif
		return BIND_OK(target);
	}

	assert(src_slot >= 0 && src_slot < src->num_slots);

	if (src_slot >= ctx->slot_map_len) {
		ast_slot_id *new_slot_map;
		if (ctx->slot_map_freeable) {
			new_slot_map = realloc(
					ctx->slot_map, src->num_slots * sizeof(ast_slot_id));
		} else {
			new_slot_map = calloc(
					src->num_slots, sizeof(ast_slot_id));
			memcpy(new_slot_map, ctx->slot_map,
					ctx->slot_map_len * sizeof(ast_slot_id));
			ctx->slot_map_freeable = true;
		}

		for (size_t i = ctx->slot_map_len; i < src->num_slots; i++) {
			new_slot_map[i] = AST_SLOT_NOT_FOUND;
		}

		ctx->slot_map = new_slot_map;
		ctx->slot_map_len = src->num_slots;
	}

	if (ctx->slot_map[src_slot] != AST_SLOT_NOT_FOUND) {
		struct ast_env_slot mapped_slot;
		mapped_slot = ast_env_slot(ctx->ctx, dest, ctx->slot_map[src_slot]);
		while (mapped_slot.kind == AST_SLOT_SUBST) {
			ctx->slot_map[src_slot] = mapped_slot.subst;
			mapped_slot = ast_env_slot(ctx->ctx, dest, ctx->slot_map[src_slot]);
		}

		struct ast_bind_result res;
		res = ast_try_union_slot(ctx->ctx, dest,
				target, ctx->slot_map[src_slot]);
		BIND_EXPECT_OK(res);

#if AST_DEBUG_UNION
		print_indent(union_depth);
		printf(" -> %i (map)\n", res.ok.result);
		union_depth -= 1;
#endif
		return res;
	}

	struct ast_env_slot slot = ast_env_slot(ctx->ctx, src, src_slot);

	ast_slot_id type_target = AST_BIND_NEW;

	if (target != AST_BIND_NEW && !ctx->copy_mode) {
		struct ast_env_slot target_slot = ast_env_slot(ctx->ctx, dest, target);

		type_target = target_slot.type;
	}

	struct ast_bind_result result = BIND_COMPILER_ERROR;

	switch (slot.kind) {
		case AST_SLOT_ERROR:
#if AST_DEBUG_UNION
			print_indent(union_depth);
			printf(" -> fail\n");
			union_depth -= 1;
#endif
			return BIND_OK(AST_BIND_FAILED);

		case AST_SLOT_WILDCARD:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_wildcard(
						ctx->ctx, dest, target,
						res.ok.result);
			}
			break;

		case AST_SLOT_CONST_TYPE:
			result = ast_try_bind_slot_const_type(
					ctx->ctx, dest, target, slot.const_type);
			break;

		case AST_SLOT_CONST:
			result = ast_try_bind_slot_const(
					ctx->ctx, dest, target, slot.const_object);
			break;

		case AST_SLOT_PARAM:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_param(
						ctx->ctx, dest, target, slot.param_index,
						res.ok.result);
			}
			break;

		case AST_SLOT_TEMPL:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_templ(
						ctx->ctx, dest, target, res.ok.result);
			}
			break;

		case AST_SLOT_CLOSURE:
			{
				struct ast_bind_result res;
				res = ast_union_slot_internal(
						ctx, dest, type_target,
						src, slot.type);
				TYPE_BIND_EXPECT_OK(res);
				result = ast_try_bind_slot_closure(
						ctx->ctx, dest, target, res.ok.result);
			}
			break;

		case AST_SLOT_CONS:
			result = ast_try_bind_slot_cons(
					ctx->ctx, dest, target, slot.cons.def);
			BIND_EXPECT_OK(result);

			for (size_t i = 0; i < slot.cons.num_present_args; i++) {
				ast_slot_id arg_slot =
					ast_bind_require_ok(
							ast_try_unpack_arg_named(
								ctx->ctx, dest, result.ok.result,
								AST_BIND_NEW,
								slot.cons.args[i].name));

				BIND_EXPECT_OK(ast_union_slot_internal(ctx,
						dest, arg_slot,
						src, slot.cons.args[i].slot));
			}

			BIND_EXPECT_OK(ast_union_slot_internal(ctx,
					dest, ast_env_slot(ctx->ctx, dest, result.ok.result).type,
					src, slot.type));
			// TODO: Union type?
			break;

		case AST_SLOT_CONS_ARRAY: {
			size_t num_members = slot.cons_array.num_members;
			ast_slot_id members[num_members];
			ast_slot_id member_type_slot = AST_BIND_NEW;

			for (size_t i = 0; i < num_members; i++) {
				members[i] = AST_BIND_NEW;
			}

			if (target != AST_BIND_NEW) {
				struct ast_env_slot target_slot;
				target_slot = ast_env_slot(ctx->ctx, dest, target);

				if (target_slot.kind == AST_SLOT_CONS_ARRAY) {
					if (target_slot.cons_array.num_members != num_members) {
						printf("Attempted to bind a CONS_ARRAY of length %zu with "
								"one of length %zu.\n",
								target_slot.cons_array.num_members,
								num_members);
#if AST_DEBUG_UNION
						union_depth -= 1;
#endif
						return BIND_ARRAY_LENGTH_MISMATCH(
								target_slot.cons_array.num_members, num_members);
					}

					for (size_t i = 0; i < num_members; i++) {
						members[i] = target_slot.cons_array.members[i];
					}

					member_type_slot = target_slot.cons_array.member_type;
				}
			}

			for (size_t i = 0; i < num_members; i++) {
				struct ast_bind_result res;
				res = ast_union_slot_internal(ctx,
						dest, members[i],
						src, slot.cons_array.members[i]);
				BIND_EXPECT_OK(res);
				members[i] = res.ok.result;
			}


			struct ast_bind_result res;
			res = ast_union_slot_internal(
					ctx, dest, member_type_slot,
					src, slot.cons_array.member_type);
			TYPE_BIND_EXPECT_OK(res);
			member_type_slot = res.ok.result;

			// Because unioning some members might change others, we have to
			// resolve the slots to unwrap substitutions.
			for (size_t i = 0; i < num_members; i++) {
				ast_node_resolve_slot(dest, &members[i]);
			}

			result = ast_try_bind_slot_cons_array(
					ctx->ctx, dest, target,
					members, num_members, member_type_slot);
		} break;

		case AST_SLOT_SUBST:
#if 0
			if (src != dest) {
				printf("src:\n");
				ast_env_print(ctx->ctx->vm, src);
				printf("dest:\n");
			}
			ast_env_print(ctx->ctx->vm, dest);
			printf("failed bind (%p)%i -> (%p)%i\n", (void *)src, src_slot, (void *)dest, target);
			panic("SUBST-slot in union (%i(%i) -> %i).", src_slot, slot.subst, target);
#endif
#if AST_DEBUG_UNION
			union_depth -= 1;
#endif
			return ast_union_slot_internal(ctx,
					dest, target,
					src,  slot.subst);
	}
	BIND_EXPECT_OK(result);

#if AST_DEBUG_UNION
	print_indent(union_depth);
	printf(" -> %i\n", result.ok.result);
	union_depth -= 1;
#endif

	assert(ctx->slot_map[src_slot] == AST_SLOT_NOT_FOUND);
	ctx->slot_map[src_slot] = result.ok.result;

	ast_substitute(ctx->ctx, dest, result.ok.result, target);

	if (src == dest && !ctx->copy_mode) {
		ast_substitute(ctx->ctx, dest, result.ok.result, src_slot);
	}

	return result;
}

struct ast_bind_result
ast_try_union_slot(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id target, ast_slot_id src_slot)
{
	if (target == src_slot) {
		return BIND_OK(target);
	} else if (target == AST_BIND_NEW) {
		return BIND_OK(src_slot);
	}

	ast_slot_id slot_map[env->num_slots];

	for (size_t i = 0; i < env->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;
	cpy_ctx.slot_map_len = env->num_slots;
	cpy_ctx.copy_mode = false;

	struct ast_bind_result res;
	res = ast_union_slot_internal(
			&cpy_ctx, env, target, env, src_slot);
	return res;
}

ast_slot_id
ast_union_slot(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id target, ast_slot_id src_slot)
{
	return ast_bind_result_to_slot(
			ast_try_union_slot(
				ctx, env, target, src_slot));
}

ast_slot_id
ast_copy_slot(struct ast_context *ctx,
		struct ast_env *dest, ast_slot_id target,
		struct ast_env *src,  ast_slot_id src_slot)
{
	ast_slot_id slot_map[src->num_slots];

	for (size_t i = 0; i < src->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;
	cpy_ctx.slot_map_len = src->num_slots;
	cpy_ctx.copy_mode = true;

	ast_slot_id res;
	res = ast_bind_result_to_slot(
			ast_union_slot_internal(
				&cpy_ctx, dest, target, src, src_slot));
	return res;
}

void
ast_substitute(struct ast_context *ctx, struct ast_env *env,
		ast_slot_id new_slot, ast_slot_id target)
{
	if (target < 0) {
		return;
	}

	assert(new_slot < (int)env->num_slots);
	assert(target < env->num_slots);

	if (new_slot == target) {
		return;
	}

	assert(env->slots[target].kind != AST_SLOT_SUBST);

	memset(&env->slots[target], 0, sizeof(struct ast_env_slot));
	env->slots[target].kind = AST_SLOT_SUBST;
	env->slots[target].subst = new_slot;
	env->slots[target].type = AST_BIND_FAILED;

#if AST_DEBUG_SUBST
	printf("subst %i -> %i\n", target, new_slot);
#endif

	for (ast_slot_id i = 0; i < env->num_slots; i++) {
		struct ast_env_slot *slot;
		slot = &env->slots[i];

		if (slot->type == target) {
#if AST_DEBUG_SUBST
			printf("  (type on %i) %i -> %i\n", i, slot->type, new_slot);
#endif
			slot->type = new_slot;
		}

		switch (slot->kind) {
			case AST_SLOT_CONS:
				for (size_t arg_i = 0; arg_i < slot->cons.num_present_args; arg_i++) {
					if (slot->cons.args[arg_i].slot == target) {
#if AST_DEBUG_SUBST
						printf("  (cons arg %zu '%.*s' on %i) %i -> %i\n",
								arg_i, ALIT(slot->cons.args[arg_i].name), i,
								slot->cons.args[arg_i].slot, new_slot);
#endif
						slot->cons.args[arg_i].slot = new_slot;
					}
				}
				break;

			case AST_SLOT_CONS_ARRAY:
				if (slot->cons_array.member_type == target) {
#if AST_DEBUG_SUBST
					printf("  (cons_array member_type on %i) %i -> %i\n",
							i, slot->cons_array.member_type, new_slot);
#endif
					slot->cons_array.member_type = new_slot;
				}
				if (slot->cons_array.member_count == target) {
#if AST_DEBUG_SUBST
					printf("  (cons_array member_type on %i) %i -> %i\n",
							i, slot->cons_array.member_count, new_slot);
#endif
					slot->cons_array.member_count = new_slot;
				}
				for (size_t member_i = 0; member_i < slot->cons_array.num_members; member_i++) {
					if (slot->cons_array.members[member_i] == target) {
#if AST_DEBUG_SUBST
						printf("  (cons_array member %zu on %i) %i -> %i\n", member_i,
								i, slot->cons_array.members[member_i], new_slot);
#endif
						slot->cons_array.members[member_i] = new_slot;
					}
				}
				break;

			case AST_SLOT_SUBST:
				if (slot->subst == target) {
#if AST_DEBUG_SUBST
					printf("  (subst on %i) %i -> %i\n",
							i, slot->subst, new_slot);
#endif
					slot->subst = new_slot;
				}
				break;

			case AST_SLOT_ERROR:
			case AST_SLOT_WILDCARD:
			case AST_SLOT_CONST_TYPE:
			case AST_SLOT_CONST:
			case AST_SLOT_PARAM:
			case AST_SLOT_TEMPL:
			case AST_SLOT_CLOSURE:
				break;
		}
	}
}

struct ast_env_slot
ast_env_slot(struct ast_context *ctx, struct ast_env *env, ast_slot_id slot)
{
	if (slot >= 0) {
		assert(slot < env->num_slots);
		return env->slots[slot];
	} else if (slot == AST_SLOT_TYPE) {
		return (struct ast_env_slot) {
			.type = AST_SLOT_TYPE,
			.kind = AST_SLOT_CONST_TYPE,
			.const_type = ctx->types.type,
		};
	} else {
		return (struct ast_env_slot) {
			.type = AST_BIND_FAILED,
			.kind = AST_SLOT_ERROR,
		};
	}
}

bool
ast_object_def_from_cons(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *out, ast_slot_id obj)
{
	struct ast_env_slot slot;
	slot = ast_env_slot(ctx, env, obj);
	assert(slot.kind == AST_SLOT_CONS);

	ast_slot_id slot_map[env->num_slots];

	for (size_t i = 0; i < env->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;
	cpy_ctx.slot_map_len = env->num_slots;
	cpy_ctx.copy_mode = true;

	out->num_params = slot.cons.num_present_args;
	out->params = calloc(out->num_params,
			sizeof(struct ast_object_def_param));
	for (size_t i = 0; i < out->num_params; i++) {
		out->params[i].name = slot.cons.args[i].name;
		out->params[i].slot =
			ast_bind_result_to_slot(
				ast_union_slot_internal(&cpy_ctx,
						&out->env, AST_BIND_NEW,
						env,       slot.cons.args[i].slot));
	}

	out->ret_type =
		ast_bind_result_to_slot(
			ast_union_slot_internal(&cpy_ctx,
					&out->env, AST_BIND_NEW,
					env,       slot.type));

	return true;
}

struct ast_node *
ast_node_deep_copy_internal(
		struct ast_union_context *ctx, struct ast_env *dest_env,
		struct ast_env *src_env, struct ast_node *src)
{
	struct ast_node *result;
	result = calloc(1, sizeof(struct ast_node));

#define DCP_NODE(name)                               \
	do {                                             \
	result->name =                                   \
		ast_node_deep_copy_internal(                 \
				ctx, dest_env, src_env, src->name);  \
	} while (0);

#define DCP_SLOT(name)                               \
	do {                                             \
		if (src->name < 0) {                         \
			result->name = src->name;                \
		} else {                                     \
			result->name =                           \
				ast_bind_result_to_slot(             \
					ast_union_slot_internal(ctx,     \
							dest_env, AST_BIND_NEW,  \
							src_env,  src->name));   \
		}                                            \
	} while (0);

#define DCP_LIT(name)                                \
	do { result->name = src->name; } while (0);

#define DCP_DLIST(array_name, count_name)            \
	do {                                             \
		DCP_LIT(count_name);                         \
		if ((result->count_name) > 0) {              \
			result->array_name = calloc(             \
					result->count_name,              \
					sizeof(*result->array_name));    \
		} else {                                     \
			result->array_name = NULL;               \
		}                                            \
	} while (0);

	DCP_LIT(kind);
	DCP_LIT(loc);

	switch (src->kind) {
	case AST_NODE_FUNC_NATIVE:
	case AST_NODE_FUNC:
		if (src->kind == AST_NODE_FUNC_NATIVE) {
			DCP_LIT(func.native);
		} else {
			DCP_NODE(func.body);
			DCP_LIT(func.closure);
		}
		DCP_DLIST(func.params, func.num_params);
		for (size_t i = 0; i < result->func.num_params; i++) {
			DCP_LIT(func.params[i].name);
			DCP_NODE(func.params[i].type);
			DCP_SLOT(func.params[i].slot);
		}

		DCP_NODE(func.return_type);
		DCP_SLOT(func.return_type_slot);
		DCP_SLOT(func.type);
		DCP_SLOT(func.slot);
		DCP_LIT(func.instance);
		DCP_LIT(func.closure);
		break;

		break;

	case AST_NODE_CONS:
		DCP_SLOT(call.cons);
		// fallthrough

	case AST_NODE_CALL:
		DCP_DLIST(call.args, call.num_args);
		for (size_t i = 0; i < result->call.num_args; i++) {
			DCP_LIT(call.args[i].name);
			DCP_NODE(call.args[i].value);
		}

		DCP_NODE(call.func);
		DCP_SLOT(call.ret_type);
		break;

	case AST_NODE_FUNC_TYPE:
		DCP_DLIST(func_type.param_types, func_type.num_params);
		for (size_t i = 0; i < result->func_type.num_params; i++) {
			DCP_NODE(func_type.param_types[i]);
		}
		DCP_NODE(func_type.ret_type);
		break;

	case AST_NODE_TEMPL:
		DCP_NODE(templ.body);

		DCP_DLIST(templ.params, templ.num_params);
		for (size_t i = 0; i < result->templ.num_params; i++) {
			DCP_LIT(templ.params[i].name);
			DCP_SLOT(templ.params[i].slot);
			if (src->templ.params[i].type) {
				DCP_NODE(templ.params[i].type);
			} else {
				result->templ.params[i].type = NULL;
			}
			DCP_LIT(templ.params[i].loc);
		}

		DCP_SLOT(templ.slot);
		DCP_LIT(templ.def);

		DCP_LIT(templ.closure);
		break;

	case AST_NODE_ACCESS:
		DCP_LIT(access.name);
		DCP_NODE(access.target);
		DCP_SLOT(access.slot);
		break;

	case AST_NODE_LIT:
		DCP_LIT(lit);
		break;

	case AST_NODE_LOOKUP:
		DCP_LIT(lookup.name);
		DCP_SLOT(lookup.slot);
		DCP_LIT(lookup.ref);
		break;

	case AST_NODE_COMPOSITE:
		DCP_DLIST(composite.members, composite.num_members);
		for (size_t i = 0; i < result->composite.num_members; i++) {
			DCP_LIT(composite.members[i].name);
			DCP_NODE(composite.members[i].type);
		}

		DCP_DLIST(composite.binds, composite.num_binds);
		for (size_t i = 0; i < result->composite.num_binds; i++) {
			DCP_NODE(composite.binds[i].target);
			DCP_NODE(composite.binds[i].value);
			DCP_LIT(composite.binds[i].overridable);
		}

		DCP_DLIST(composite.free_exprs, composite.num_free_exprs);
		for (size_t i = 0; i < result->composite.num_free_exprs; i++) {
			DCP_NODE(composite.free_exprs[i]);
		}

		DCP_LIT(composite.closure);

		DCP_SLOT(composite.ret_value);
		break;

	case AST_NODE_VARIANT:
		DCP_DLIST(variant.options, variant.num_options);
		for (size_t i = 0; i < result->variant.num_options; i++) {
			DCP_LIT(variant.options[i].name);
			if (src->variant.options[i].data_type) {
				DCP_NODE(variant.options[i].data_type);
			}
		}

		DCP_LIT(variant.closure);

		DCP_SLOT(variant.ret_value);
		break;
	}

#undef DCP_NODE
#undef DCP_SLOT
#undef DCP_LIT
#undef DCP_DLIST

	return result;
}

// NOTE: This function is here instead of in ast_nodes.c because it needs
// ast_union_slot_internal to keep copy-context between copy calls.
struct ast_node *
ast_node_deep_copy(struct ast_context *ctx, struct ast_env *dest_env,
		struct ast_env *src_env, struct ast_node *src)
{
	ast_slot_id slot_map[src_env->num_slots];

	for (size_t i = 0; i < src_env->num_slots; i++) {
		slot_map[i] = AST_SLOT_NOT_FOUND;
	}

	struct ast_union_context cpy_ctx = {0};
	cpy_ctx.ctx = ctx;
	cpy_ctx.slot_map = slot_map;
	cpy_ctx.slot_map_len = src_env->num_slots;
	cpy_ctx.copy_mode = true;

	struct ast_node *res;
#if AST_DEBUG_UNION
	printf("\n=== begin deep copy ===\n");
#endif
	res = ast_node_deep_copy_internal(
			&cpy_ctx, dest_env, src_env, src);
#if AST_DEBUG_UNION
	printf("===  end deep copy  ===\n");
#endif
	return res;
}
