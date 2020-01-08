#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "base/mod.h"
#include "dlist.h"

#include <unistd.h>
#include <sys/mman.h>

void
ast_env_free(struct ast_env *env)
{
	if (env->constraint_pages) {
		for (size_t i = env->num_borrowed_pages; i < env->num_pages; i++) {
			int err;
			err = munmap(env->constraint_pages[i], env->page_size);
			if (err) {
				perror("munmap");
			}
		}

		free(env->constraint_pages);
		env->constraint_pages = NULL;
	}
}


ast_slot_id
ast_slot_alloc(struct ast_env *env)
{
	ast_slot_id new_slot;

	new_slot = env->num_alloced_slots;
	env->num_alloced_slots += 1;

	return new_slot;
}

#if AST_DEBUG_SLOT_SOLVE
#	define PASS_DEBUG_PARAM , constr_loc
#else
#	define PASS_DEBUG_PARAM
#endif

static struct ast_slot_constraint *
ast_alloc_constraint(
		struct ast_env *env, enum ast_constraint_kind kind,
		enum ast_constraint_source source,
		struct stg_location loc, ast_slot_id target
		AST_SLOT_DEBUG_PARAM)
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
#if AST_DEBUG_SLOT_SOLVE
	res->reason.impose_loc = constr_loc;
#endif

	return res;
}

#if AST_DEBUG_SLOT_SOLVE
#	undef ast_slot_value_error
#	undef ast_slot_require_is_obj
#	undef ast_slot_require_is_type
#	undef ast_slot_require_is_func_type
#	undef ast_slot_require_cons
#	undef ast_slot_require_inst
#	undef ast_slot_require_equals
#	undef ast_slot_require_type
#	undef ast_slot_require_member_named
#	undef ast_slot_require_member_index
#endif

void
ast_slot_value_error(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_ERROR, source, loc, target
			PASS_DEBUG_PARAM);
}

void
ast_slot_require_is_obj(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, struct object val
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_IS_OBJ, source, loc, target
			PASS_DEBUG_PARAM);

	constr->is.obj = val;
}

void
ast_slot_require_is_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, type_id val
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_IS_TYPE, source, loc, target
			PASS_DEBUG_PARAM);

	constr->is.type = val;
}

void
ast_slot_require_is_func_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id ret_type,
		ast_slot_id *param_types, size_t num_params
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_IS_FUNC_TYPE, source, loc, target
			PASS_DEBUG_PARAM);

	ast_slot_require_member_index(
			env, loc, source, target, 0, ret_type
			PASS_DEBUG_PARAM);

	for (size_t i = 0; i < num_params; i++) {
		ast_slot_require_member_index(
				env, loc, source, target, i+1, param_types[i]
				PASS_DEBUG_PARAM);
	}
}

void
ast_slot_require_equals(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id slot
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_EQUALS, source, loc, target
			PASS_DEBUG_PARAM);

	constr->equals = slot;
}
void
ast_slot_require_type(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id type
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_TYPE, source, loc, target
			PASS_DEBUG_PARAM);

	constr->type = type;
}

void
ast_slot_require_member_named(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, struct atom *name, ast_slot_id member
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_MEMBER_NAMED, source, loc, target
			PASS_DEBUG_PARAM);

	constr->member.slot = member;
	constr->member.name = name;
}

void
ast_slot_require_member_index(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, size_t index, ast_slot_id member
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_MEMBER_INDEXED, source, loc, target
			PASS_DEBUG_PARAM);

	constr->member.slot = member;
	constr->member.index = index;
}

void
ast_slot_require_cons(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id cons
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_CONS, source, loc, target
			PASS_DEBUG_PARAM);

	constr->cons = cons;
}

void
ast_slot_require_inst(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id inst
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_INST, source, loc, target
			PASS_DEBUG_PARAM);

	constr->inst = inst;
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

enum ast_slot_state_flags {
	AST_SLOT_HAS_ERROR      = (1<<0),
	AST_SLOT_HAS_SUBST      = (1<<1),
	AST_SLOT_HAS_CONS       = (1<<2),
	AST_SLOT_IS_FUNC_TYPE   = (1<<3),
	AST_SLOT_IS_INST        = (1<<4),
	AST_SLOT_HAS_TYPE       = (1<<5),
	AST_SLOT_HAS_VALUE      = (1<<6),
	AST_SLOT_VALUE_IS_TYPE  = (1<<7),
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
	ast_slot_id cons;

	union {
		type_id type;
		struct object obj;
	} value;

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
	type_id cons;
	type_id inst;

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
#define SRC_NAME(name) case AST_CONSTR_SRC_##name: return #name;
	switch (source) {
		SRC_NAME(EXPECTED);
		SRC_NAME(DT_DECL);
		SRC_NAME(FUNC_DECL);
		SRC_NAME(TEMPL_PARAM_DECL);
		SRC_NAME(CLOSURE);
		SRC_NAME(CALL_ARG);
		SRC_NAME(CONS_ARG);
		SRC_NAME(ACCESS);
		SRC_NAME(LIT);
		SRC_NAME(LOOKUP);
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
ast_get_real_slot(struct solve_context *ctx, ast_slot_id slot)
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
ast_get_slot(struct solve_context *ctx, ast_slot_id slot)
{
	return ast_get_real_slot(ctx, ast_slot_resolve_subst(ctx, slot));
}

static inline bool
is_more_authorative(struct ast_slot_constraint *lhs, struct ast_slot_constraint *rhs)
{
	return lhs->source < rhs->source;
}

#if AST_DEBUG_SLOT_SOLVE
static void
ast_print_constraint(
		struct solve_context *ctx,
		ast_constraint_id constr_id)
{
	struct ast_slot_constraint *constr;
	constr = ast_get_constraint(ctx, constr_id);

	printf("%s:%zu ",
			constr->reason.impose_loc.filename,
			constr->reason.impose_loc.line);

	switch (constr->kind) {
		case AST_SLOT_REQ_ERROR:
			printf("error %i", constr->target);
			break;

		case AST_SLOT_REQ_IS_OBJ:
			printf("is obj %i = ", constr->target);
			print_obj_repr(ctx->vm, constr->is.obj);
			break;

		case AST_SLOT_REQ_IS_TYPE:
			printf("is type %i = ", constr->target);
			print_type_repr(ctx->vm,
					vm_get_type(ctx->vm, constr->is.type));
			break;

		case AST_SLOT_REQ_IS_FUNC_TYPE:
			printf("is func type %i", constr->target);
			break;

		case AST_SLOT_REQ_EQUALS:
			printf("equals %i = %i",
					constr->target, constr->equals);
			break;

		case AST_SLOT_REQ_TYPE:
			printf("type typeof(%i) = %i",
					constr->target, constr->type);
			break;

		case AST_SLOT_REQ_MEMBER_NAMED:
			printf("member named %i.%.*s = %i",
					constr->target, ALIT(constr->member.name),
					constr->member.slot);
			break;

		case AST_SLOT_REQ_MEMBER_INDEXED:
			printf("member indexed %i[%zu] = %i",
					constr->target, constr->member.index,
					constr->member.slot);
			break;

		case AST_SLOT_REQ_CONS:
			printf("cons %i = %i",
					constr->target, constr->cons);
			break;

		case AST_SLOT_REQ_INST:
			printf("inst %i = %i",
					constr->target, constr->inst);
			break;
	}
}

static void
ast_print_slot(
		struct solve_context *ctx,
		ast_slot_id slot_id)
{
	struct ast_slot_resolve *slot;
	slot = ast_get_real_slot(ctx, slot_id);

	printf("%i:", slot_id);
	if ((slot->flags & AST_SLOT_HAS_SUBST) != 0) {
		printf(" subst %i\n", slot->subst);
		return;
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
		} else if ((slot->flags & AST_SLOT_IS_INST) != 0) {
			printf("inst %i", slot->cons);
		} else {
			printf("%i", slot->cons);
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
		type_id tid = *(type_id *)obj.data;
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
		ast_constraint_id constr_id, ast_slot_id slot,
		ast_slot_id cons, bool is_inst)
{
#if AST_DEBUG_SLOT_SOLVE
	printf("%3i apply %s %i = %i",
			constr_id, is_inst ? "inst" : "cons", slot, cons);
#endif
	struct ast_slot_resolve *res;
	res = ast_get_slot(ctx, slot);

	ast_slot_id old_cons, new_cons;

	if ((res->flags & AST_SLOT_HAS_TYPE) != 0) {
		old_cons = res->cons;
		new_cons = ast_slot_join(
				ctx, res->cons, cons);
	} else {
		old_cons = AST_SLOT_NOT_FOUND;
		new_cons = cons;
	}

	if (ast_solve_should_apply(
			ctx, constr_id, slot,
			AST_SLOT_PARAM_CONS)) {
		res->cons = new_cons;
		res->flags &= ~AST_SLOT_IS_FUNC_TYPE;
		if (is_inst) {
			res->flags |= AST_SLOT_IS_INST;
		} else {
			res->flags &= ~AST_SLOT_IS_INST;
		}
	}

	return old_cons != res->cons;
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
		res->cons = 0;
		res->flags |= AST_SLOT_IS_FUNC_TYPE;
		res->flags &= ~AST_SLOT_IS_INST;
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
	res = ast_get_slot(ctx, slot);

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
	for (size_t i = 0; i < slot->num_members; i++) {
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
	slot = ast_get_slot(ctx, slot_id);

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

	to_slot   = ast_get_real_slot(ctx, to);
	from_slot = ast_get_real_slot(ctx, from);

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
					to, from_slot->cons,
					(from_slot->flags & AST_SLOT_IS_INST) != 0);
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
	AST_SLOT_GET_NO_CONS_SLOT = 3,
	AST_SLOT_GET_IS_FUNC_TYPE_CONS = 4,
	AST_SLOT_GET_IS_CONS = 5,
	AST_SLOT_GET_IS_INST = 6,
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

static enum ast_slot_get_error
ast_slot_try_get_value_cons(
		struct solve_context *ctx, ast_slot_id slot_id,
		struct object_cons **out_cons)
{
	int err;
	struct object obj;
	err = ast_slot_try_get_value(ctx, slot_id, ctx->cons, &obj);
	if (err) {
		return err;
	}

	assert_type_equals(ctx->vm, obj.type, ctx->cons);
	*out_cons = *(struct object_cons **)obj.data;
	return AST_SLOT_GET_OK;
}

static enum ast_slot_get_error
ast_slot_try_get_value_inst(
		struct solve_context *ctx, ast_slot_id slot_id,
		struct object_inst **out_inst)
{
	int err;
	struct object obj;
	err = ast_slot_try_get_value(ctx, slot_id, ctx->inst, &obj);
	if (err) {
		return err;
	}

	assert_type_equals(ctx->vm, obj.type, ctx->inst);
	*out_inst = *(struct object_inst **)obj.data;
	return AST_SLOT_GET_OK;
}

static enum ast_slot_get_error
ast_slot_try_get_cons(
		struct solve_context *ctx, ast_slot_id slot_id,
		struct object_cons **out_cons)
{
	struct ast_slot_resolve *slot;
	slot = ast_get_slot(ctx, slot_id);

	enum ast_slot_state_flags cons_flags;
	cons_flags = slot->flags & (
			  AST_SLOT_HAS_CONS
			| AST_SLOT_IS_INST
			| AST_SLOT_IS_FUNC_TYPE);

	if (cons_flags == (AST_SLOT_HAS_CONS)) {
		return ast_slot_try_get_value_cons(
				ctx, slot->cons, out_cons);
	} else if (cons_flags == (AST_SLOT_HAS_CONS|AST_SLOT_IS_FUNC_TYPE)) {
		return AST_SLOT_GET_IS_FUNC_TYPE_CONS;
	} else if (cons_flags == (AST_SLOT_HAS_CONS|AST_SLOT_IS_INST)) {
		return AST_SLOT_GET_IS_INST;
	} else {
		return AST_SLOT_GET_NO_CONS_SLOT;
	}
}

static enum ast_slot_get_error
ast_slot_try_get_inst(
		struct solve_context *ctx, ast_slot_id slot_id,
		struct object_inst **out_inst)
{
	struct ast_slot_resolve *slot;
	slot = ast_get_slot(ctx, slot_id);

	enum ast_slot_state_flags cons_flags;
	cons_flags = slot->flags & (
			  AST_SLOT_HAS_CONS
			| AST_SLOT_IS_INST
			| AST_SLOT_IS_FUNC_TYPE);

	if (cons_flags == (AST_SLOT_HAS_CONS|AST_SLOT_IS_INST)) {
		return ast_slot_try_get_value_inst(
				ctx, slot->cons, out_inst);
	} else if (cons_flags == (AST_SLOT_HAS_CONS|AST_SLOT_IS_FUNC_TYPE)) {
		return AST_SLOT_GET_IS_FUNC_TYPE_CONS;
	} else if (cons_flags == (AST_SLOT_HAS_CONS)) {
		return AST_SLOT_GET_IS_CONS;
	} else {
		return AST_SLOT_GET_NO_CONS_SLOT;
	}
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
				target = ast_get_slot(ctx, constr->target);
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

		case AST_SLOT_REQ_CONS:
			ast_solve_apply_cons(
					ctx, constr_id,
					constr->target, constr->cons, false);
			break;

		case AST_SLOT_REQ_INST:
			ast_solve_apply_cons(
					ctx, constr_id,
					constr->target, constr->inst, true);
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
	slot = ast_get_real_slot(ctx, slot_id);

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

				struct object_cons *cons;
				int err;
				err = ast_slot_try_get_value_cons(
						ctx, slot->cons, &cons);
				if (err) {
					printf("Failed to resolve cons.\n");
					continue;
				}

				ssize_t param_i;
				param_i = object_cons_find_param(
						cons, slot->members[i].ref.name);
				if (param_i < 0) {
					printf("Cons does not have member %.*s.\n",
							ALIT(slot->members[i].ref.name));
					continue;
				}
				assert(param_i < cons->num_params);

				struct object res = {0};
				res.type = cons->params[i].type;

				struct type *param_type;
				param_type = vm_get_type(ctx->vm, res.type);

				uint8_t buffer[param_type->size];
				memset(buffer, 0, param_type->size);
				res.data = buffer;

				assert(cons->unpack);
				cons->unpack(ctx->vm, cons->data,
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

				if ((ssize_t)slot->members[i].ref.index > max_param_i) {
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
			} else {
				printf("Not all args are set for func type.\n");
			}

		} else {
			struct object_cons *cons;
			int err;
			err = ast_slot_try_get_value_cons(
					ctx, slot->cons, &cons);
			if (err) {
				printf("Failed to resolve cons.\n");
				return false;
			}

			struct object args[cons->num_params];
			void *arg_data[cons->num_params];
			bool arg_set[cons->num_params];
			memset(arg_set, 0, sizeof(bool) * cons->num_params);

			for (size_t i = 0; i < slot->num_members; i++) {
				if (!slot->members[i].ref.named) {
					printf("Expected named arguments for cons, got indexed arg.\n");
					slot->flags |= AST_SLOT_HAS_ERROR;
					continue;
				}

				ssize_t param_i;
				param_i = object_cons_find_param(
						cons, slot->members[i].ref.name);

				if (param_i < 0) {
					printf("Got unexpected parameter '%.*s' to cons.\n",
							ALIT(slot->members[i].ref.name));
					continue;
				}
				assert(param_i < cons->num_params);

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
			for (size_t i = 0; i < cons->num_params; i++) {
				all_args_set &= arg_set[i];
			}

			if (all_args_set) {
				struct object res = {0};

				assert(cons->pack_type);
				res.type = cons->pack_type(
						ctx->vm, cons->data,
						arg_data, cons->num_params);

				assert(res.type != TYPE_UNSET);

				struct type *res_type;
				res_type = vm_get_type(ctx->vm, res.type);

				uint8_t buffer[res_type->size];
				memset(buffer, 0, res_type->size);
				res.data = buffer;

				assert(cons->pack);
				cons->pack(
						ctx->vm, cons->data, buffer,
						arg_data, cons->num_params);

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

#if AST_DEBUG_SLOT_SOLVE
__attribute__((__format__ (__printf__, 4, 5))) static void
ast_slot_verify_fail(
		struct solve_context *ctx, ast_constraint_id constr_id,
		struct stg_location loc, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	stg_msgv(ctx->err, loc, STG_ERROR, fmt, ap);

	printf("%i [", constr_id);
	ast_print_constraint(ctx, constr_id);
	printf("] validation failed: ");
	vprintf(fmt, ap);
	printf("\n");

	va_end(ap);
}
#endif

int
ast_slot_verify_constraint(
		struct solve_context *ctx, ast_constraint_id constr_id)
{
#if AST_DEBUG_SLOT_SOLVE
#define stg_error(err, loc, ...) \
	ast_slot_verify_fail(ctx, constr_id, loc, __VA_ARGS__)
#endif
	struct ast_slot_constraint *constr;
	constr = ast_get_constraint(ctx, constr_id);

	struct ast_slot_resolve *target;
	target = ast_get_slot(ctx, constr->target);

	switch (constr->kind) {
		case AST_SLOT_REQ_ERROR:
			return -1;

		case AST_SLOT_REQ_IS_OBJ:
		case AST_SLOT_REQ_IS_TYPE:
			{
				assert((target->flags & AST_SLOT_HAS_VALUE) != 0);

				struct object expected = {0};
				if (constr->kind == AST_SLOT_REQ_IS_TYPE) {
					expected.type = ctx->type;
					expected.data = &constr->is.type;
				} else {
					expected = constr->is.obj;
				}

				struct object got = {0};
				int err;
				err = ast_slot_try_get_value(
						ctx, constr->target, TYPE_UNSET, &got);
				if (err) {
					// TODO: Error message.
					stg_error(ctx->err, constr->reason.loc,
							"Failed to resolve this value.");
					return -1;
				}

				if (!obj_equals(ctx->vm, expected, got)) {
					struct string exp_str, got_str;
					const char *value_kind_expectation;

					if (expected.type == ctx->type && got.type == ctx->type) {
						type_id expected_type = *(type_id *)expected.data;
						type_id got_type = *(type_id *)got.data;

						exp_str = type_repr_to_alloced_string(
								ctx->vm, vm_get_type(ctx->vm, expected_type));
						got_str = type_repr_to_alloced_string(
								ctx->vm, vm_get_type(ctx->vm, got_type));
						value_kind_expectation = "type";
					} else if (expected.type == ctx->type && got.type != ctx->type) {
						type_id expected_type = *(type_id *)expected.data;

						exp_str = type_repr_to_alloced_string(
								ctx->vm, vm_get_type(ctx->vm, expected_type));
						got_str = obj_repr_to_alloced_string(
								ctx->vm, got);
						value_kind_expectation = "type";
					} else {
						exp_str = obj_repr_to_alloced_string(
								ctx->vm, expected);
						got_str = obj_repr_to_alloced_string(
								ctx->vm, got);
						value_kind_expectation = "value";
					}

					stg_error(ctx->err, constr->reason.loc,
							"Expected %s '%.*s', got '%.*s'.",
							value_kind_expectation,
							LIT(exp_str), LIT(got_str));

					return -1;
				}
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
				type_slot = ast_get_slot(ctx, target->type);
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

				struct object_cons *cons;
				err = ast_slot_try_get_value_cons(
						ctx, target->cons, &cons);
				if (err) {
					return -1;
				}

				ssize_t param_i;
				param_i = object_cons_find_param(
						cons, constr->member.name);

				struct object exp_val = {0};
				exp_val.type = cons->params[param_i].type;
				struct type *exp_type;
				exp_type = vm_get_type(ctx->vm, exp_val.type);

				uint8_t buffer[exp_type->size];
				memset(buffer, 0, exp_type->size);
				exp_val.data = buffer;

				assert(cons->unpack);
				cons->unpack(
						ctx->vm, cons->data,
						exp_val.data, target_val.data, param_i);

				if (!obj_equals(ctx->vm, mbr_val, exp_val)) {
					struct string exp_str, got_str;

					exp_str = obj_repr_to_alloced_string(
							ctx->vm, exp_val);
					got_str = obj_repr_to_alloced_string(
							ctx->vm, mbr_val);

					stg_error(ctx->err, constr->reason.loc,
							"Expected '%.*s' to be %.*s, got %.*s.",
							ALIT(cons->params[param_i].name),
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

		case AST_SLOT_REQ_CONS:
			assert((target->flags & AST_SLOT_HAS_CONS) != 0);
			if ((target->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
				// TODO: Better error message.
				stg_error(ctx->err, constr->reason.loc,
						"Expected constructor, got function type constructor.");
				return -1;
			} else if ((target->flags & AST_SLOT_IS_INST) != 0) {
				// TODO: Better error message.
				stg_error(ctx->err, constr->reason.loc,
						"Expected constructor, got object instantiation.");
				return -1;
			}
			return 0;

		case AST_SLOT_REQ_INST:
			assert((target->flags & AST_SLOT_HAS_CONS) != 0);
			if ((target->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
				// TODO: Better error message.
				stg_error(ctx->err, constr->reason.loc,
						"Expected object instantiation, got function type constructor.");
				return -1;
			} else if ((target->flags & AST_SLOT_IS_INST) == 0) {
				// TODO: Better error message.
				stg_error(ctx->err, constr->reason.loc,
						"Expected object instantiation, got constructor.");
				return -1;
			}
			return 0;

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

#if AST_DEBUG_SLOT_SOLVE
#undef stg_error
#endif
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
	ctx->cons = ast_ctx->types.cons;
	ctx->inst = ast_ctx->types.inst;
	ctx->env = &_env;
	ctx->num_slots = ctx->env->num_alloced_slots;

	struct ast_slot_resolve _slots[ctx->num_slots];
	memset(_slots, 0, sizeof(struct ast_slot_resolve) * ctx->num_slots);
	ctx->slots = _slots;

#if AST_DEBUG_SLOT_SOLVE
	printf("====== begin solve slots ======\n");
	printf("Constraints:\n");
	{
		size_t num_constraints = ast_env_num_constraints(ctx);
		for (size_t constr_id = 0; constr_id < num_constraints; constr_id++) {
			ast_print_constraint(ctx, constr_id);
			printf("\n");
		}
	}

	printf("\n");
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
		int err;
		err = ast_slot_verify_constraint(
				ctx, constr_id);
		if (err < 0) {
			struct ast_slot_constraint *constr;
			constr = ast_get_constraint(ctx, constr_id);

			struct ast_slot_resolve *target;
			target = ast_get_slot(ctx, constr->target);

			target->flags |= AST_SLOT_HAS_ERROR;
		}
	}

#if AST_DEBUG_SLOT_SOLVE
	printf("final slots (%zu slot%s):\n", ctx->num_slots,
			(ctx->num_slots == 1) ? "" : "s");

	for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
		ast_print_slot(ctx, slot_id);
	}
#endif

	// Produce result.
	int num_errors = 0;
	memset(out_result, 0, sizeof(struct ast_slot_result) * ctx->num_slots);
	for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
		struct ast_slot_resolve *slot;
		slot = ast_get_real_slot(ctx, slot_id);

		struct ast_slot_result *res;
		res = &out_result[slot_id];
		res->result = 0;

		if ((slot->flags & AST_SLOT_HAS_ERROR) != 0) {
			res->result = AST_SLOT_RES_ERROR;
			num_errors += 1;
			continue;
		}

		bool error = false;

		if ((slot->flags & AST_SLOT_HAS_SUBST) != 0) {
			// We will produce the result of substitutions in the next step.
			continue;
		}

		if ((slot->flags & AST_SLOT_HAS_VALUE) != 0) {
			if ((slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
				res->result |= AST_SLOT_RES_VALUE_FOUND_TYPE;
				res->value.type = slot->value.type;
				res->type = ctx->type;
			} else {
				res->result |= AST_SLOT_RES_VALUE_FOUND_OBJ;
				res->value.obj = slot->value.obj;
				res->type = res->value.obj.type;
			}
		} else {
			int err;
			err = ast_slot_try_get_type(
					ctx, slot_id, &res->type);
			if (err) {
				res->result |= AST_SLOT_RES_VALUE_UNKNOWN;
			} else {
				res->result |= AST_SLOT_RES_TYPE_FOUND;
			}
		}

		if ((slot->flags & AST_SLOT_HAS_CONS) != 0) {
			if ((slot->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
				res->result |= AST_SLOT_RES_CONS_FOUND_FUNC_TYPE;
			} else if ((slot->flags & AST_SLOT_IS_INST) != 0) {
				int err;
				err = ast_slot_try_get_inst(
						ctx, slot_id, &res->inst);
				if (err > 0) {
					res->result |= AST_SLOT_RES_CONS_UNKNOWN;
				} else if (err < 0) {
					error = true;
				} else {
					res->result |= AST_SLOT_RES_CONS_FOUND_INST;
				}
			} else {
				int err;
				err = ast_slot_try_get_cons(
						ctx, slot_id, &res->cons);
				if (err > 0) {
					res->result |= AST_SLOT_RES_CONS_UNKNOWN;
				} else if (err < 0) {
					error = true;
				} else {
					res->result |= AST_SLOT_RES_CONS_FOUND;
				}
			}
		} else {
			res->result |= AST_SLOT_RES_CONS_UNKNOWN;
		}

		if (error) {
			res->result = AST_SLOT_RES_ERROR;
			num_errors += 1;
		}
	}

	// Produce the result of substitutions by copying from the already produced
	// subst target.
	for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
		struct ast_slot_resolve *slot;
		slot = ast_get_real_slot(ctx, slot_id);

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

	ast_env_free(ctx->env);

	return num_errors;
}

struct ast_node *
ast_node_deep_copy_internal(
		struct ast_context *ctx, struct ast_env *dest_env,
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
			/*result->name =                           \
				ast_bind_result_to_slot(             \
					ast_union_slot_internal(ctx,     \
							dest_env, AST_BIND_NEW,  \
							src_env,  src->name));*/   \
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
	case AST_NODE_INST:
		DCP_LIT(call.cons);
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
		DCP_LIT(templ.cons);

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
	struct ast_node *res;
#if AST_DEBUG_UNION
	printf("\n=== begin deep copy ===\n");
#endif
	res = ast_node_deep_copy_internal(
			ctx, dest_env, src_env, src);
#if AST_DEBUG_UNION
	printf("===  end deep copy  ===\n");
#endif
	return res;
}
