#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"
#include "term_color.h"
#include "base/mod.h"
#include "dlist.h"
#include "module.h"

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
#	undef ast_slot_require_cons_or_value_from
#	undef ast_slot_require_type
#	undef ast_slot_require_member_named
#	undef ast_slot_require_member_index

#	define SLOT_DEBUG_ARG , AST_SLOT_DEBUG_ARG
#else
#	define SLOT_DEBUG_ARG
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
		type_id type,
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
			SLOT_DEBUG_ARG);

	ast_slot_id ret_type_type;
	ret_type_type = ast_slot_alloc(env);

	ast_slot_require_is_type(
			env, loc, source, ret_type_type, type
			SLOT_DEBUG_ARG);
	ast_slot_require_type(
			env, loc, source, ret_type, ret_type_type
			SLOT_DEBUG_ARG);

	for (size_t i = 0; i < num_params; i++) {
		ast_slot_require_member_index(
				env, loc, source, target, i+1, param_types[i]
				SLOT_DEBUG_ARG);

		ast_slot_id param_type_type;
		param_type_type = ast_slot_alloc(env);

		ast_slot_require_is_type(
				env, loc, source, param_type_type, type
				SLOT_DEBUG_ARG);
		ast_slot_require_type(
				env, loc, source, param_types[i], param_type_type
				SLOT_DEBUG_ARG);

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
ast_slot_require_cons_or_value_from(
		struct ast_env *env, struct stg_location loc,
		enum ast_constraint_source source,
		ast_slot_id target, ast_slot_id slot
		AST_SLOT_DEBUG_PARAM)
{
	struct ast_slot_constraint *constr;
	constr = ast_alloc_constraint(env,
			AST_SLOT_REQ_CONS_OR_VALUE_FROM, source, loc, target
			PASS_DEBUG_PARAM);

	constr->cons_or_value_from = slot;
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
	AST_SLOT_HAS_CONS_OR_VALUE_FROM = (1<<8),
	AST_SLOT_CONS_DECAYED   = (1<<9),
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
	ast_slot_id cons_or_value_from;

	union {
		type_id type;
		struct object obj;
	} value;

	struct ast_slot_resolve_member *members;
	size_t num_members;

	// Keeps track of what constructor has had impose_constraints called on
	// this slot.
	struct object_cons *imposed_cons;

	// Designates the constraints that has the highest priority for the
	// parameter.
	struct {
		ast_constraint_id type;
		ast_constraint_id value;
		ast_constraint_id cons;
		ast_constraint_id cons_or_value_from;
	} authority;
};

struct solve_context {
	type_id type;
	type_id cons;
	type_id inst;

	struct vm *vm;
	struct stg_error_context *err;
	struct stg_module *mod;
	struct ast_env *env;
	struct ast_slot_resolve *slots;
	struct ast_context *ast_ctx;
	size_t num_slots;

	size_t num_extra_slots;
	struct ast_slot_resolve *extra_slots;
};

#if AST_DEBUG_SLOT_SOLVE || AST_DEBUG_SLOT_SOLVE_APPLY
static const char *
ast_constraint_source_name(enum ast_constraint_source source)
{
#define SRC_NAME(name) case AST_CONSTR_SRC_##name: return #name;
	switch (source) {
		SRC_NAME(EXPECTED);
		SRC_NAME(DT_DECL);
		SRC_NAME(FUNC_DECL);
		SRC_NAME(TEMPL_PARAM_DECL);
		SRC_NAME(TEMPL_PARAM_VAL);
		SRC_NAME(CLOSURE);
		SRC_NAME(CALL_ARG);
		SRC_NAME(CONS_ARG);
		SRC_NAME(ACCESS);
		SRC_NAME(MOD);
		SRC_NAME(LIT);
		SRC_NAME(LOOKUP);
		SRC_NAME(DECAY);
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
	assert(slot < ctx->num_slots+ctx->num_extra_slots && slot >= 0);
	if (slot < ctx->num_slots) {
		return &ctx->slots[slot];
	} else {
		return &ctx->extra_slots[slot-ctx->num_slots];
	}
}

static inline ast_slot_id
ast_slot_resolve_subst(struct solve_context *ctx, ast_slot_id slot)
{
	while (slot >= 0 && (ast_get_real_slot(ctx, slot)->flags & AST_SLOT_HAS_SUBST) != 0) {
		slot = ast_get_real_slot(ctx, slot)->subst;
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

	printf("%s:%zu %s ",
			constr->reason.impose_loc.filename,
			constr->reason.impose_loc.line,
			ast_constraint_source_name(constr->source));

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

		case AST_SLOT_REQ_CONS_OR_VALUE_FROM:
			printf("cons or value from %i <- %i",
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
ast_print_slot_result(
		struct vm *vm,
		struct ast_slot_result *result,
		ast_slot_id slot_id)
{
	struct ast_slot_result res;
	res = result[slot_id];

	printf("  result: 0x%02x\n", res.result);
	switch (ast_slot_value_result(res.result)) {
		case AST_SLOT_RES_VALUE_UNKNOWN:
			printf("   -value: unknown\n");
			printf("   -type:  unknown\n");
			break;

		case AST_SLOT_RES_TYPE_FOUND:
			printf("   -value: unknown\n");
			printf("   -type:  ");
			print_type_repr(vm, vm_get_type(vm, res.type));
			printf("\n");
			break;

		case AST_SLOT_RES_VALUE_FOUND_OBJ:
			printf("   -value: ");
			print_obj_repr(vm, res.value.obj);
			printf("\n   -type:  ");
			print_type_repr(vm, vm_get_type(vm, res.type));
			printf("\n");
			break;

		case AST_SLOT_RES_VALUE_FOUND_TYPE:
			printf("   -value: Type(");
			print_type_repr(vm, vm_get_type(vm, res.value.type));
			printf(")\n   -type:  ");
			print_type_repr(vm, vm_get_type(vm, res.type));
			printf("\n");
			break;

		default:
			printf("   -value: " TC(TC_BRIGHT_RED, "error") "\n");
			printf("   -type:  " TC(TC_BRIGHT_RED, "error") "\n");
			break;
	}

	switch (ast_slot_cons_result(res.result)) {
		case AST_SLOT_RES_CONS_UNKNOWN:
			printf("   -cons:  unknown\n");
			break;

		case AST_SLOT_RES_CONS_FOUND:
			printf("   -cons:  cons %p\n", (void *)res.cons);
			break;

		case AST_SLOT_RES_CONS_FOUND_INST:
			printf("   -cons:  inst %p\n", (void *)res.inst);
			break;

		case AST_SLOT_RES_CONS_FOUND_FUNC_TYPE:
			printf("   -cons:  func type\n");
			break;

		default:
			printf("   -cons:  " TC(TC_BRIGHT_RED, "error") "\n");
			break;
	}
}

static void
ast_print_slot(
		struct solve_context *ctx,
		struct ast_slot_result *result,
		ast_slot_id slot_id)
{
	struct ast_slot_resolve *slot;
	slot = ast_get_real_slot(ctx, slot_id);

	printf("%i:", slot_id);

	printf("\n");
	if ((slot->flags & AST_SLOT_HAS_SUBST) != 0) {
		printf(" subst %i\n",
				ast_slot_resolve_subst(
					ctx, slot->subst));
	}
	if ((slot->flags & AST_SLOT_HAS_ERROR) != 0) {
		printf(" " TC(TC_BRIGHT_RED, "(error)\n"));
	}

	if (slot_id < ctx->num_slots) {
		ast_print_slot_result(ctx->vm, result, slot_id);
	}

	if ((slot->flags & AST_SLOT_HAS_SUBST) != 0) {
		printf("\n");
		return;
	}

	printf(" -value: ");
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
	if ((slot->flags & AST_SLOT_CONS_DECAYED) != 0) {
		printf(" (decayed)");
	}

	printf("\n -type: ");
	if ((slot->flags & AST_SLOT_HAS_TYPE) != 0) {
		printf("[%i] %i", slot->authority.type, slot->type);
	} else {
		printf("none");
	}

	printf("\n -members:\n");
	if (slot->num_members == 0) {
		printf("    (none)\n");
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
	AST_SLOT_GET_TYPE_HAS_NO_INST = 7,
	AST_SLOT_GET_TYPE_HAS_NO_CONS = 8,
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
		type_id *out_type);

static ast_slot_id
ast_slot_join(
		struct solve_context *ctx,
		ast_slot_id lhs, ast_slot_id rhs);

enum ast_solve_slot_param {
	AST_SLOT_PARAM_TYPE,
	AST_SLOT_PARAM_VALUE,
	AST_SLOT_PARAM_CONS,
	AST_SLOT_PARAM_CONS_OR_VALUE_FROM,
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
#if AST_DEBUG_SLOT_SOLVE_APPLY
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

#if AST_DEBUG_SLOT_SOLVE_APPLY
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

		case AST_SLOT_PARAM_CONS_OR_VALUE_FROM:
			flag = AST_SLOT_HAS_CONS_OR_VALUE_FROM;
			authority = &res->authority.cons_or_value_from;
			break;
	}

	assert(authority && flag != 0);

	if ((res->flags & flag) != 0) {
		return ast_solve_should_apply_internal(
				ctx, constr_id, authority);
	} else {
		*authority = constr_id;
		res->flags |= flag;
#if AST_DEBUG_SLOT_SOLVE_APPLY
		printf(" yes (new)\n");
#endif
		return true;
	}
}

// Attemptes to decay a cons object slot into a slot that is constructed by the
// cons object.
// Returns 0 if the slot did not decay and obj can be applied as normal.
// Returns 1 if the slot did decay and reset the slot's value. In this case the
// obj can also be applied as normal.
// Returns 2 if the slot did already has decayed. In this case the obj can be
// applied as normal.
// Returns -1 if the slot decayed because of obj's cons. In this case the
// caller should not apply obj.
// Returns -2 if the slot had already decayed because of obj's cons. In this
// case the caller should not apply obj, and no change was made.
static int
ast_solve_try_decay(
		struct solve_context *ctx, ast_slot_id slot, struct object obj)
{
#if AST_DEBUG_SLOT_SOLVE_APPLY
	const char *decay_reason = NULL;
#endif
	bool should_decay = false;
	bool should_clear_value = false;

	struct ast_slot_resolve *res;
	res = ast_get_slot(ctx, slot);

	if ((res->flags & AST_SLOT_CONS_DECAYED) != 0) {
		return obj.type == ctx->cons ? -2 : 2;
	}

	struct object_cons *target_cons = NULL;
	// TODO: We keep the pointer to the cons-ptr to avoid having to reallocate
	// the memory for the pointer. In the future we should have some temporary
	// memory for the solve.
	struct object_cons **tmp_target_cons_ptr = NULL;

	if (obj.type == ctx->cons) {
		if (obj.data) {
			target_cons = *(struct object_cons **)obj.data;
			// TODO: Remove.
			tmp_target_cons_ptr = obj.data;
		}
	}


	// We do not use ast_slot_try_get_value here because said function will
	// fail if the slot's value's type and the slot's type-slot's value do not
	// match.
	if ((res->flags & AST_SLOT_HAS_VALUE) != 0) {
		if ((res->flags & AST_SLOT_VALUE_IS_TYPE) == 0 &&
				res->value.obj.type == ctx->cons) {
			struct object_cons *current_cons;
			current_cons = *(struct object_cons **)res->value.obj.data;

			if (target_cons && target_cons != current_cons) {
				// There is a conflict trying to apply two different cons to this
				// slot. We will let the verifictaion step report this error.
				return 0;
			}

			// The slot should decay if the new object is a value other than a
			// cons.
			if (obj.type != TYPE_UNSET &&
					obj.type != ctx->cons) {
				should_decay = true;
			}

			should_clear_value = true;
			target_cons = current_cons;
			// TODO: Remove.
			tmp_target_cons_ptr = res->value.obj.data;
		} else {
			should_decay = true;
#if AST_DEBUG_SLOT_SOLVE_APPLY
			decay_reason = "conflicting value";
#endif
		}
	}

	if (res->num_members > 0) {
		should_decay = true;
#if AST_DEBUG_SLOT_SOLVE_APPLY
		decay_reason = "has members";
#endif
	}

	if ((res->flags & AST_SLOT_HAS_TYPE) != 0) {
		struct ast_slot_resolve *type_slot;
		type_slot = ast_get_slot(ctx, res->type);


		if ((type_slot->flags & (AST_SLOT_HAS_VALUE|AST_SLOT_VALUE_IS_TYPE)) ==
				(AST_SLOT_HAS_VALUE|AST_SLOT_VALUE_IS_TYPE) &&
				type_slot->value.type != ctx->cons) {

			// printf("type of slot %i - %i is cons\n", slot, res->type);
			should_decay = true;
#if AST_DEBUG_SLOT_SOLVE_APPLY
			decay_reason = "type is func type";
#endif
		}

		if ((type_slot->flags & (AST_SLOT_HAS_CONS|AST_SLOT_IS_FUNC_TYPE)) ==
				(AST_SLOT_HAS_CONS|AST_SLOT_IS_FUNC_TYPE)) {
			// printf("type of slot %i - %i has func cons\n", slot, res->type);
			should_decay = true;
#if AST_DEBUG_SLOT_SOLVE_APPLY
			decay_reason = "type is func type";
#endif
		}
	}

	if (!target_cons || !should_decay) {
		/*
		if (!target_cons) {
			printf("no target cons.\n");
		} else {
			printf("should not decay.\n");
		}
		*/
		return 0;
	}

#if AST_DEBUG_SLOT_SOLVE_APPLY
	printf("decay slot %i (reason: %s)\n", slot,
			decay_reason ? decay_reason : "unknown");
#endif

	struct object cons_obj = {0};
	cons_obj.type = ctx->cons;
	// TODO: Remove.
	assert(*tmp_target_cons_ptr == target_cons);
	cons_obj.data = tmp_target_cons_ptr;

	ast_slot_id cons_id;

	if ((res->flags & AST_SLOT_HAS_CONS) == 0) {
		cons_id = ast_slot_alloc(ctx->env);

		ast_slot_require_cons(ctx->env, STG_NO_LOC,
				AST_CONSTR_SRC_DECAY,
				slot, cons_id
				SLOT_DEBUG_ARG);
	} else {
		cons_id = res->cons;
	}

	// TODO: Location.
	ast_slot_require_is_obj(ctx->env, STG_NO_LOC,
			AST_CONSTR_SRC_DECAY,
			cons_id, cons_obj
			SLOT_DEBUG_ARG);

	res->flags |= AST_SLOT_CONS_DECAYED;

	if (should_clear_value) {
		res->flags &= ~(AST_SLOT_HAS_VALUE | AST_SLOT_VALUE_IS_TYPE);
	}

	return obj.type == ctx->cons ? -1 : 1;
}

static bool
ast_solve_apply_value_type(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot, type_id type)
{
#if AST_DEBUG_SLOT_SOLVE_APPLY
	printf("%3i apply value type %i = ",
			constr_id, slot);
	print_type_repr(ctx->vm, vm_get_type(ctx->vm, type));
#endif

	struct object type_obj = {0};
	type_obj.type = ctx->type;
	type_obj.data = &type;
	// TODO: We should have temporary memory for the solve.
	type_obj = register_object(ctx->vm, &ctx->mod->store, type_obj);

	// We only abort the application if the decay was as a result of type_obj's
	// cons. In reality this should never happen for type values.
	if (ast_solve_try_decay(ctx, slot, type_obj) < 0) {
		return true;
	}

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

#if AST_DEBUG_SLOT_SOLVE_APPLY
	printf("%3i apply value obj %i = ",
			constr_id, slot);
	print_obj_repr(ctx->vm, obj);
#endif

	// We only abort the application if the decay was as a result of obj's
	// cons.
	int err;
	err = ast_solve_try_decay(ctx, slot, obj);
	if (err < 0) {
		return err == -1;
	}

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
#if AST_DEBUG_SLOT_SOLVE_APPLY
	printf("%3i apply %s %i = %i",
			constr_id, is_inst ? "inst" : "cons", slot, cons);
#endif
	struct ast_slot_resolve *res;
	res = ast_get_slot(ctx, slot);

	ast_slot_id old_cons, new_cons;

	if ((res->flags & (AST_SLOT_HAS_CONS|AST_SLOT_IS_FUNC_TYPE)) ==
			AST_SLOT_HAS_CONS) {
		old_cons = res->cons;
		new_cons = ast_slot_join(
				ctx, res->cons, cons);
	} else {
		old_cons = -1;
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
#if AST_DEBUG_SLOT_SOLVE_APPLY
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
ast_solve_apply_cons_or_value_from(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot,
		ast_slot_id from)
{
#if AST_DEBUG_SLOT_SOLVE_APPLY
	printf("%3i apply cons or value from %i <- %i",
			constr_id, slot, from);
#endif
	struct ast_slot_resolve *res;
	res = ast_get_slot(ctx, slot);

	ast_slot_id old_from, new_from;

	if ((res->flags & AST_SLOT_HAS_CONS_OR_VALUE_FROM) != 0) {
		old_from = res->cons_or_value_from;
		new_from = ast_slot_join(
				ctx, res->cons_or_value_from, from);
	} else {
		old_from = -1;
		new_from = from;
	}

	if (ast_solve_should_apply(
			ctx, constr_id, slot,
			AST_SLOT_PARAM_CONS_OR_VALUE_FROM)) {
		res->cons_or_value_from = new_from;
	}

	return old_from != res->cons_or_value_from;
}


static bool
ast_solve_apply_type(
		struct solve_context *ctx,
		ast_constraint_id constr_id, ast_slot_id slot,
		ast_slot_id type_id)
{
#if AST_DEBUG_SLOT_SOLVE_APPLY
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
		old_type = -1;
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
#if AST_DEBUG_SLOT_SOLVE_APPLY
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

#if AST_DEBUG_SLOT_SOLVE_APPLY
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

	ast_solve_try_decay(
			ctx, slot_id, OBJ_UNSET);

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
#if AST_DEBUG_SLOT_SOLVE_APPLY
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

#if AST_DEBUG_SLOT_SOLVE_APPLY
	printf("join finished\n");
#endif

	return to;
}

static enum ast_slot_get_error
ast_slot_try_get_type(
		struct solve_context *ctx, ast_slot_id slot_id,
		type_id *out_type)
{
	struct ast_slot_resolve *slot;
	slot = ast_get_slot(ctx, slot_id);

	type_id value_type = TYPE_UNSET;

	if ((slot->flags & AST_SLOT_HAS_VALUE) != 0) {
		if ((slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
			value_type = ctx->type;
		} else {
			value_type = slot->value.obj.type;
		}
	}

	if ((slot->flags & AST_SLOT_HAS_TYPE) != 0) {
		enum ast_slot_get_error err;
		type_id res;
		err = ast_slot_try_get_value_type(
				ctx, slot->type, &res);

		if (value_type == TYPE_UNSET || err < 0) {
			if (err == 0) {
				*out_type = res;
			}
			return err;
		}

		if (err == 0 && !type_equals(ctx->vm, value_type, res)) {
			return AST_SLOT_GET_TYPE_ERROR;
		}

		*out_type = value_type;
		return AST_SLOT_GET_OK;
	} else {
		if (value_type == TYPE_UNSET) {
			return AST_SLOT_GET_NO_TYPE_SLOT;
		} else {
			*out_type = value_type;
			return AST_SLOT_GET_OK;
		}
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
		return AST_SLOT_GET_TYPE_ERROR;
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
	err = ast_slot_try_get_value(ctx, slot_id, TYPE_UNSET, &obj);
	if (err) {
		return err;
	}


	if (type_equals(ctx->vm, obj.type, ctx->cons)) {
		*out_cons = *(struct object_cons **)obj.data;
		return AST_SLOT_GET_OK;
	} else if (type_equals(ctx->vm, obj.type, ctx->type)) {
		type_id tid = *(type_id *)obj.data;
		struct type *type = vm_get_type(ctx->vm, tid);

		if (!type->obj_def) {
			return AST_SLOT_GET_TYPE_HAS_NO_CONS;
		}

		*out_cons = type->obj_def;
		return AST_SLOT_GET_OK;
	} else {
		return AST_SLOT_GET_TYPE_ERROR;
	}
}

static enum ast_slot_get_error
ast_slot_try_get_value_inst(
		struct solve_context *ctx, ast_slot_id slot_id,
		struct object_inst **out_inst)
{
	int err;
	struct object obj;
	err = ast_slot_try_get_value(ctx, slot_id, TYPE_UNSET, &obj);
	if (err) {
		return err;
	}

	if (type_equals(ctx->vm, obj.type, ctx->inst)) {
		*out_inst = *(struct object_inst **)obj.data;
		return AST_SLOT_GET_OK;
	} else if (type_equals(ctx->vm, obj.type, ctx->type)) {
		type_id tid = *(type_id *)obj.data;
		struct type *type = vm_get_type(ctx->vm, tid);

		if (!type->obj_inst) {
			return AST_SLOT_GET_TYPE_HAS_NO_INST;
		}

		*out_inst = type->obj_inst;
		return AST_SLOT_GET_OK;
	} else {
		return AST_SLOT_GET_TYPE_ERROR;
	}
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

		case AST_SLOT_REQ_CONS_OR_VALUE_FROM:
			ast_solve_apply_cons_or_value_from(
					ctx, constr_id,
					constr->target, constr->cons_or_value_from);
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
			ast_solve_apply_type(
					ctx, constr_id,
					constr->target, constr->inst);
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

	if ((slot->flags & AST_SLOT_HAS_ERROR) != 0) {
		return false;
	}

	bool made_change = false;

	int decay_res;
	decay_res = ast_solve_try_decay(
			ctx, slot_id, OBJ_UNSET);
	// Tag as changed only if the slot did decay.
	if (decay_res == 1) {
		made_change = true;
	}

	if ((slot->flags & AST_SLOT_HAS_CONS_OR_VALUE_FROM) != 0) {
		int err;
		struct object from_value;
		err = ast_slot_try_get_value(
				ctx, slot->cons_or_value_from,
				TYPE_UNSET, &from_value);
		if (!err) {
			ast_constraint_id auth_id;
			auth_id = slot->authority.cons_or_value_from;

			if (from_value.type == ctx->cons) {
				// TODO: Is it correct to bind the cons_or_value_from slot as
				// this slot's cons?
				made_change |= ast_solve_apply_cons(
						ctx, auth_id, slot_id,
						slot->cons_or_value_from, false);
			} else {
				made_change |= ast_solve_apply_value_obj(
						ctx, auth_id, slot_id, from_value);
			}
		}
	}

	if ((slot->flags & (AST_SLOT_HAS_VALUE|AST_SLOT_HAS_TYPE)) ==
			(AST_SLOT_HAS_VALUE|AST_SLOT_HAS_TYPE)) {
		made_change |=
			ast_slot_update_type(ctx, slot->authority.value, slot_id);
	}

	if ((slot->flags & AST_SLOT_HAS_CONS) == 0 && slot->num_members > 0) {
		type_id type_id;

		enum ast_slot_get_error err;
		err = ast_slot_try_get_type(ctx, slot_id, &type_id);

		if (!err) {
			struct type *type;
			type = vm_get_type(ctx->vm, type_id);

			if (type->obj_def) {
				if ((slot->flags & AST_SLOT_HAS_TYPE) != 0) {
					made_change |= ast_solve_apply_cons(
							ctx, slot->authority.type,
							slot_id, slot->type, false);
				} else {
					assert((slot->flags & AST_SLOT_HAS_VALUE) != 0);

					ast_slot_id new_type_slot;
					new_type_slot = ast_slot_alloc(ctx->env);

					struct ast_slot_constraint *val_constr;
					val_constr = ast_get_constraint(
							ctx, slot->authority.value);

					ast_slot_require_is_type(
							ctx->env, val_constr->reason.loc,
							AST_CONSTR_SRC_EXPECTED,
							new_type_slot, type_id
							SLOT_DEBUG_ARG);

					ast_slot_require_type(
							ctx->env, val_constr->reason.loc,
							AST_CONSTR_SRC_EXPECTED,
							slot_id, new_type_slot
							SLOT_DEBUG_ARG);

					made_change = true;
				}
			}
		}
	}

	if ((slot->flags & AST_SLOT_HAS_CONS) != 0 &&
			slot->num_members > 0) {
		// Unpack value and push down to the members.
		if ((slot->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
			if ((slot->flags & AST_SLOT_HAS_VALUE) != 0) {
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
					}
				}
			}

		} else {
			struct object obj = {0};
			if ((slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
				obj.type = ctx->type;
				obj.data = &slot->value.type;
			} else {
				obj = slot->value.obj;
			}

			struct object_cons *cons;
			int err;
			err = ast_slot_try_get_value_cons(
					ctx, slot->cons, &cons);
			if (!err) {
				for (size_t mbr_i = 0; mbr_i < slot->num_members; mbr_i++) {
					ssize_t param_i = -1;
					if (slot->members[mbr_i].ref.named) {
						param_i = object_cons_find_param(
								cons, slot->members[mbr_i].ref.name);
					} else if (slot->members[mbr_i].ref.index < cons->num_params) {
						param_i = slot->members[mbr_i].ref.index;
					} else {
						// Object has no such member (index out of range).
						continue;
					}

					if (param_i < 0) {
						// Object has no such member.
						continue;
					}
					assert(param_i < cons->num_params);

					if ((slot->flags & AST_SLOT_HAS_VALUE) != 0) {
						struct object res = {0};
						res.type = cons->params[param_i].type;

						struct type *param_type;
						param_type = vm_get_type(ctx->vm, res.type);

						uint8_t buffer[param_type->size];
						memset(buffer, 0, param_type->size);
						res.data = buffer;

						int err;
						err = object_ct_unpack_param(
								ctx->ast_ctx, ctx->mod,
								cons, obj, param_i, &res);
						if (err) {
							slot->flags |= AST_SLOT_HAS_ERROR;
							return false;
						}

						res = register_object(ctx->vm, ctx->env->store, res);

						made_change |= ast_solve_apply_value_obj(
								ctx, slot->authority.value,
								slot->members[mbr_i].slot, res);
					} else {
						struct ast_slot_resolve *mbr;
						mbr = ast_get_slot(ctx, slot->members[mbr_i].slot);

						if ((mbr->flags & AST_SLOT_HAS_TYPE) == 0) {
							ast_slot_id new_type_slot;
							new_type_slot = ast_slot_alloc(ctx->env);

							struct ast_slot_constraint *mbr_constr;
							mbr_constr = ast_get_constraint(
									ctx, slot->members[mbr_i].authority);

							// TODO: Proper source.
							ast_slot_require_is_type(
									ctx->env, mbr_constr->reason.loc,
									AST_CONSTR_SRC_EXPECTED,
									new_type_slot, cons->params[param_i].type
									SLOT_DEBUG_ARG);

							ast_slot_require_type(
									ctx->env, mbr_constr->reason.loc,
									AST_CONSTR_SRC_EXPECTED,
									slot->members[mbr_i].slot, new_type_slot
									SLOT_DEBUG_ARG);

							made_change = true;
						} else {
							ast_solve_apply_value_type(
									ctx, slot->members[mbr_i].authority,
									mbr->type, cons->params[param_i].type);
						}
					}

				}
			}
		}
	}

	if ((slot->flags & (AST_SLOT_HAS_VALUE|AST_SLOT_HAS_CONS)) ==
			AST_SLOT_HAS_CONS) {
		// Try pack this slot's value from its members.

		if ((slot->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
			ssize_t max_param_i = -1;
			for (size_t mbr_i = 0; mbr_i < slot->num_members; mbr_i++) {
				if (slot->members[mbr_i].ref.named) {
					printf("Expected indexed arguments for func type cons, got named arg.\n");
					slot->flags |= AST_SLOT_HAS_ERROR;
					continue;
				}

				if ((ssize_t)slot->members[mbr_i].ref.index > max_param_i) {
					max_param_i = slot->members[mbr_i].ref.index;
				}
			}

			assert(max_param_i >= 0);

			type_id arg_types[max_param_i+1];
			bool arg_set[max_param_i+1];
			memset(arg_set, 0, sizeof(bool) * max_param_i+1);

			for (size_t mbr_i = 0; mbr_i < slot->num_members; mbr_i++) {
				if (slot->members[mbr_i].ref.named) {
					continue;
				}

				size_t param_i = slot->members[mbr_i].ref.index;
				assert(param_i <= max_param_i);

				int err;
				arg_types[param_i] = TYPE_UNSET;
				err = ast_slot_try_get_value_type(
						ctx, slot->members[mbr_i].slot, &arg_types[param_i]);
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
						ctx->mod, arg_types[0],
						&arg_types[1], max_param_i);

				made_change |= ast_solve_apply_value_type(
						ctx, slot->authority.value,
						slot_id, res);
			}

		} else {
			struct object_cons *cons;
			int err;
			err = ast_slot_try_get_value_cons(
					ctx, slot->cons, &cons);
			if (err) {
				return false;
			}

			if (!slot->imposed_cons) {
				// The first time we see a constructor for slot we let it
				// impose constraints. To avoid duplicatly creating
				// constraints we set slot->imposed_cons. This also
				// prevents applying conflicting constructor's constraints.

				if (cons->impose_constraints) {
					ast_slot_id param_slots[cons->num_params];

					struct ast_slot_constraint *cons_constr;
					cons_constr = ast_get_constraint(
							ctx, slot->authority.cons);

					for (size_t i = 0; i < cons->num_params; i++) {
						param_slots[i] = ast_slot_alloc(ctx->env);

						ast_slot_require_member_named(
								ctx->env, cons_constr->reason.loc,
								AST_CONSTR_SRC_EXPECTED,
								slot_id, cons->params[i].name,
								param_slots[i]
								SLOT_DEBUG_ARG);
					}

					ast_slot_id ret_type_slot;
					ret_type_slot = ast_slot_alloc(ctx->env);

					ast_slot_require_type(
							ctx->env, cons_constr->reason.loc,
							AST_CONSTR_SRC_EXPECTED,
							slot_id, ret_type_slot
							SLOT_DEBUG_ARG);

					cons->impose_constraints(
							ctx->ast_ctx, ctx->mod,
							cons->data, ctx->env,
							ret_type_slot, param_slots);
				}

				made_change = true;

				slot->imposed_cons = cons;
			}


			struct object args[cons->num_params];
			void *arg_data[cons->num_params];
			bool arg_set[cons->num_params];
			ast_slot_id param_slot[cons->num_params];
			memset(arg_set, 0, sizeof(bool) * cons->num_params);

			for (size_t i = 0 ; i < cons->num_params; i++) {
				param_slot[i] = -1;
			}

			for (size_t i = 0; i < slot->num_members; i++) {
				ssize_t param_i = -1;
				if (slot->members[i].ref.named) {
					param_i = object_cons_find_param(
							cons, slot->members[i].ref.name);
				} else if (slot->members[i].ref.index < cons->num_params) {
						param_i = slot->members[i].ref.index;
				} else {
					printf("Got unexpected parameter %zu to cons.\n",
							slot->members[i].ref.index);
					continue;
				}

				if (param_i < 0) {
					printf("Got unexpected parameter '%.*s' to cons.\n",
							ALIT(slot->members[i].ref.name));
					continue;
				}
				assert(param_i < cons->num_params);

				if (param_slot[param_i] >= 0) {
					if (cons == slot->imposed_cons) {
						param_slot[param_i] =
							ast_slot_join(ctx,
									slot->members[i].slot,
									param_slot[param_i]);
					}
				} else {
					param_slot[param_i] = slot->members[i].slot;
				}

				int err;
				err = ast_slot_try_get_value(
						ctx, param_slot[param_i],
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

				int err;
				err = object_ct_pack_type(
						ctx->ast_ctx, ctx->mod,
						cons, arg_data, cons->num_params,
						&res.type);
				if (err) {
					slot->flags |= AST_SLOT_HAS_ERROR;
					return false;
				}

				assert(res.type != TYPE_UNSET);

				struct type *res_type;
				res_type = vm_get_type(ctx->vm, res.type);

				uint8_t buffer[res_type->size];
				memset(buffer, 0, res_type->size);
				res.data = buffer;

				err = object_ct_pack(
						ctx->ast_ctx, ctx->mod,
						cons, arg_data, cons->num_params,
						&res);
				if (err) {
					slot->flags |= AST_SLOT_HAS_ERROR;
					return false;
				}

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

	printf("%i [", constr_id);
	ast_print_constraint(ctx, constr_id);
	printf("] " TC(TC_BRIGHT_RED, "validation failed") ": ");
	vprintf(fmt, ap);
	printf("\n");

	va_end(ap);

	// It seems that the ap is corrupted during the call to vpritnf. Starting
	// ap again seems to help.
	va_start(ap, fmt);

	stg_msgv(ctx->err, loc, STG_ERROR, fmt, ap);

	va_end(ap);
}
#endif

int
ast_slot_verify_member(
		struct solve_context *ctx, struct stg_location loc,
		ast_slot_id target_id, ast_slot_id member_id,
		struct ast_slot_member_ref ref)
{
	struct ast_slot_resolve *target;
	target = ast_get_slot(ctx, target_id);

	int err;

	struct object_cons *cons;
	err = ast_slot_try_get_cons(
			ctx, target_id, &cons);
	if (err < 0) {
		stg_error(ctx->err, loc,
				"Object has no members.");
		return -1;
	} else if (err > 0) {
		return 1;
	}

	size_t param_i = -1;
	if (ref.named) {
		ssize_t lookup_res;
		lookup_res = object_cons_find_param(
				cons, ref.name);
		if (lookup_res < 0) {
			stg_error(ctx->err, loc,
					"Object has no member '%.*s'.\n",
					ALIT(ref.name));
			return -1;
		}

		param_i = lookup_res;
	} else if (ref.index < cons->num_params) {
		param_i = ref.index;
	} else {
		stg_error(ctx->err, loc,
				"Object has no member %zu.\n",
				ref.index);
		return -1;
	}

	struct object mbr_val = {0};
	err = ast_slot_try_get_value(
			ctx, member_id, TYPE_UNSET, &mbr_val);
	if (err < 0) {
		// TODO: Better error message.
		stg_error(ctx->err, loc,
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
			ctx, target_id, TYPE_UNSET, &target_val);
	if (err < 0) {
		// TODO: Better error message.
		stg_error(ctx->err, loc,
				"Failed to resolve the value of the target.");
		return -1;
	} else if (err > 0) {
		return 1;
	}

	struct object exp_val = {0};
	exp_val.type = cons->params[param_i].type;
	struct type *exp_type;
	exp_type = vm_get_type(ctx->vm, exp_val.type);

	uint8_t buffer[exp_type->size];
	memset(buffer, 0, exp_type->size);
	exp_val.data = buffer;

	err = object_ct_unpack_param(
			ctx->ast_ctx, ctx->mod,
			cons, target_val, param_i, &exp_val);
	if (err) {
		return -1;
	}

	if (!obj_equals(ctx->vm, mbr_val, exp_val)) {
		struct string exp_str, got_str;

		exp_str = obj_repr_to_alloced_string(
				ctx->vm, exp_val);
		got_str = obj_repr_to_alloced_string(
				ctx->vm, mbr_val);

		stg_error(ctx->err, loc,
				"Expected '%.*s' to be %.*s, got %.*s.",
				ALIT(cons->params[param_i].name),
				LIT(exp_str), LIT(got_str));

		free(exp_str.text);
		free(got_str.text);

		return -1;
	}

	return 0;
}

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
				struct object expected = {0};
				if (constr->kind == AST_SLOT_REQ_IS_TYPE) {
					expected.type = ctx->type;
					expected.data = &constr->is.type;
				} else {
					expected = constr->is.obj;
				}

				if (expected.type == ctx->cons &&
						(target->flags & AST_SLOT_CONS_DECAYED) != 0) {
					// assert((target->flags & AST_SLOT_HAS_CONS) != 0);

					int err;
					struct object_cons *cons;
					err = ast_slot_try_get_cons(
							ctx, constr->target, &cons);
					if (err) {
						// TODO: Error message. We should report the
						// authorative and expected types.
						stg_error(ctx->err, constr->reason.loc,
								"Failed to resolve this value.");
						return -1;
					}

					if (cons != *(struct object_cons **)expected.data) {
						// TODO: Error message.
						stg_error(ctx->err, constr->reason.loc,
								"Mismatching constructors.");
						return -1;
					}
				} else {
					assert((target->flags & AST_SLOT_HAS_VALUE) != 0);

					bool got_type_only = false;
					struct object got = {0};
					int err;
					err = ast_slot_try_get_value(
							ctx, constr->target, TYPE_UNSET, &got);
					if (err == AST_SLOT_GET_TYPE_ERROR) {
						struct ast_slot_resolve *target;
						target = ast_get_slot(ctx, constr->target);

						if ((target->flags & AST_SLOT_HAS_VALUE) != 0) {
							if ((target->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
								got.type = ctx->type;
								got.data = &target->value.type;
							} else {
								got = target->value.obj;
							}
						} else if ((target->flags & AST_SLOT_HAS_TYPE) != 0) {
							struct ast_slot_resolve *type_slot;
							type_slot = ast_get_slot(ctx, target->type);

							if (((type_slot->flags &
											(AST_SLOT_HAS_VALUE|AST_SLOT_VALUE_IS_TYPE)) !=
										(AST_SLOT_HAS_VALUE|AST_SLOT_VALUE_IS_TYPE))) {
								got.type = type_slot->value.type;
								got_type_only = true;
							} else {
								// We let errors with the type slot being the
								// wrong type be reported by the type
								// constraint.
								return -1;
							}
						} else {
							stg_error(ctx->err, constr->reason.loc,
									"Type mismatch.");
							return -1;
						}

					} else if (err == AST_SLOT_GET_NOT_ENOUGH_INFO) {
						stg_error(ctx->err, constr->reason.loc,
								"Not enough information to determine this type.");
						return -1;
					} else if (err) {
						// TODO: Error message.
						stg_error(ctx->err, constr->reason.loc,
								"Failed to resolve this value.");
						return -1;
					}

					if (got_type_only || !obj_equals(ctx->vm, expected, got)) {
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
						} else if (got_type_only) {
							exp_str = type_repr_to_alloced_string(
									ctx->vm, vm_get_type(ctx->vm, expected.type));
							got_str = type_repr_to_alloced_string(
									ctx->vm, vm_get_type(ctx->vm, got.type));
							value_kind_expectation = "object of type";
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
			}
			return 0;

		case AST_SLOT_REQ_EQUALS:
			// Slots that are supposed to be equal have already been joined
			// together.
			assert(ast_slot_resolve_subst(ctx, constr->target) ==
					ast_slot_resolve_subst(ctx, constr->equals));
			return 0;

		case AST_SLOT_REQ_CONS_OR_VALUE_FROM:
			{
				int err;
				struct object from_value;

				err = ast_slot_try_get_value(
						ctx, constr->cons_or_value_from, TYPE_UNSET,
						&from_value);
				if (err) {
					return 0;
				}

				if (from_value.type == ctx->cons) {
					struct object_cons *from_cons;

					from_cons = *(struct object_cons **)from_value.data;

					struct object_cons *to_cons;
					err = ast_slot_try_get_cons(
							ctx, constr->target, &to_cons);
					if (err) {
						return 0;
					}

					if (from_cons != to_cons) {
						stg_error(ctx->err, constr->reason.loc,
								"Got an unexpected object constructor.");
						return -1;
					}
				} else {
					struct object to_value;
					err = ast_slot_try_get_value(
							ctx, constr->target, TYPE_UNSET,
							&to_value);
					if (err) {
						return 0;
					}

					if (!obj_equals(ctx->vm, from_value, to_value)) {
						struct string exp_str, got_str;

						exp_str = obj_repr_to_alloced_string(
								ctx->vm, from_value);
						got_str = obj_repr_to_alloced_string(
								ctx->vm, to_value);

						stg_error(ctx->err, constr->reason.loc,
								"Expected value '%.*s', got '%.*s'.",
								LIT(exp_str), LIT(got_str));
						free(exp_str.text);
						free(got_str.text);
					}
				}
			}
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

				struct ast_slot_constraint *exp_constr;
				exp_constr = ast_get_constraint(ctx, target->authority.value);

				struct ast_slot_resolve *type_slot;
				type_slot = ast_get_slot(ctx, target->type);

				struct ast_slot_constraint *got_constr;
				got_constr = ast_get_constraint(ctx, type_slot->authority.value);

				if ((type_slot->flags & AST_SLOT_HAS_VALUE) != 0) {
					if ((type_slot->flags & AST_SLOT_VALUE_IS_TYPE) != 0) {
						if (!type_equals(ctx->vm, type_slot->value.type, expected_type)) {
							type_id exp_type, got_type;
							if (is_more_authorative(exp_constr, got_constr)) {
								exp_type = expected_type;
								got_type = type_slot->value.type;
							} else {
								exp_type = type_slot->value.type;
								got_type = expected_type;
							}
							struct string exp_str, got_str;
							exp_str = type_repr_to_alloced_string(
									ctx->vm, vm_get_type(ctx->vm, exp_type));
							got_str = type_repr_to_alloced_string(
									ctx->vm, vm_get_type(ctx->vm, got_type));

							stg_error(ctx->err, constr->reason.loc,
									"Expected type '%.*s', got '%.*s'.",
									LIT(exp_str), LIT(got_str));
							return -1;
						}
					} else {
						struct string exp_str, got_str;
						exp_str = type_repr_to_alloced_string(
								ctx->vm, vm_get_type(ctx->vm, expected_type));
						got_str = obj_repr_to_alloced_string(
								ctx->vm, type_slot->value.obj);

						stg_error(ctx->err, constr->reason.loc,
								"Expected type '%.*s', got object '%.*s'.",
								LIT(exp_str), LIT(got_str));
						return -1;
					}
				}
			}
			return 0;

		case AST_SLOT_REQ_MEMBER_NAMED:
			{
				if ((target->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
					stg_error(ctx->err, constr->reason.loc,
							"Got invalid parameter '%.*s' to function type constructor.",
							ALIT(constr->member.name));
					return -1;
				}

				struct ast_slot_member_ref ref = {0};
				ref.named = true;
				ref.name = constr->member.name;

				return ast_slot_verify_member(
						ctx, constr->reason.loc,
						constr->target, constr->member.slot,
						ref);
			}

		case AST_SLOT_REQ_MEMBER_INDEXED:
			{
				if ((target->flags & AST_SLOT_HAS_CONS) == 0) {
					stg_error(ctx->err, constr->reason.loc,
							"Attempted to unpack a value that has no constructor.");
					return -1;
				}

				if ((target->flags & AST_SLOT_IS_FUNC_TYPE) != 0) {
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

					if (!stg_type_is_func(ctx->vm, target_val)) {
						return -1;
					}

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
				} else {
					struct ast_slot_member_ref ref = {0};
					ref.named = false;
					ref.index = constr->member.index;

					return ast_slot_verify_member(
							ctx, constr->reason.loc,
							constr->target, constr->member.slot,
							ref);
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
			} else {
				int err;

				struct object_cons *cons;
				err = ast_slot_try_get_cons(
						ctx, constr->target, &cons);
				if (err < 0) {

					type_id cons_obj_type;
					err = ast_slot_try_get_type(
							ctx, constr->cons, &cons_obj_type);

					if (err) {
						// TODO: Better error message.
						stg_error(ctx->err, constr->reason.loc,
								"The constructor was invalid.");
					} else {
						struct string got_str = {0};
						got_str = type_repr_to_alloced_string(
								ctx->vm, vm_get_type(ctx->vm, cons_obj_type));

						stg_error(ctx->err, constr->reason.loc,
								"Expected constructor, got %.*s.",
								LIT(got_str));
						free(got_str.text);
					}
					return -1;
				}
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
			} else {
				int err;

				struct object_inst *inst;
				err = ast_slot_try_get_inst(
						ctx, constr->target, &inst);
				if (err < 0) {
					type_id inst_obj_type;
					err = ast_slot_try_get_type(
							ctx, constr->inst, &inst_obj_type);

					if (err) {
						// TODO: Better error message.
						stg_error(ctx->err, constr->reason.loc,
								"The object instantiation was invalid.");
					} else {
						struct string got_str = {0};
						got_str = type_repr_to_alloced_string(
								ctx->vm, vm_get_type(ctx->vm, inst_obj_type));

						stg_error(ctx->err, constr->reason.loc,
								"Expected object instantiation, got %.*s.",
								LIT(got_str));
						free(got_str.text);
					}
					return -1;
				}
			}
			return 0;

		case AST_SLOT_REQ_IS_FUNC_TYPE:
			{
				assert((target->flags & AST_SLOT_HAS_CONS) != 0);
				if ((target->flags & AST_SLOT_IS_FUNC_TYPE) == 0) {
					// TODO: Better error message.
					stg_error(ctx->err, constr->reason.loc,
							"Expected function type, got object constructor.");
					return -1;
				}

				type_id target_val;
				int err;
				err = ast_slot_try_get_value_type(
						ctx, constr->target, &target_val);
				if (err < 0) {
					// TODO: Better error message.
					stg_error(ctx->err, constr->reason.loc,
							"Failed to resolve the value of this object.");
					return -1;
				} else if (err > 0) {
					return 1;
				}

				if (!stg_type_is_func(ctx->vm, target_val)) {
					struct string got_str = {0};
					got_str = type_repr_to_alloced_string(
							ctx->vm, vm_get_type(ctx->vm, target_val));
					stg_error(ctx->err, constr->reason.loc,
							"Expected function type, got %.*s.",
							LIT(got_str));
					free(got_str.text);
					return -1;
				}
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
		struct ast_context *ast_ctx, struct stg_module *mod,
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
	ctx->ast_ctx = ast_ctx;
	ctx->num_slots = ctx->env->num_alloced_slots;

	struct ast_slot_resolve _slots[ctx->num_slots];
	memset(_slots, 0, sizeof(struct ast_slot_resolve) * ctx->num_slots);
	ctx->slots = _slots;

#if AST_DEBUG_SLOT_SOLVE
	printf("====== begin solve slots ======\n");
	size_t original_num_constraints = ast_env_num_constraints(ctx);
#endif

	size_t num_applied_constraints = 0;
	bool made_progress = true;

	while (made_progress) {
		made_progress = false;

		if (ctx->num_slots + ctx->num_extra_slots < ctx->env->num_alloced_slots) {
			size_t new_num_extra_slots = ctx->env->num_alloced_slots - ctx->num_slots;
			ctx->extra_slots = realloc(ctx->extra_slots,
					new_num_extra_slots * sizeof(struct ast_slot_resolve));
			if (!ctx->extra_slots) {
				panic("Failed to alloc new slots during solve.");
				return -1;
			}

			memset(&ctx->extra_slots[ctx->num_extra_slots], 0,
					(new_num_extra_slots-ctx->num_extra_slots) *
					sizeof(struct ast_slot_resolve));

			ctx->num_extra_slots = new_num_extra_slots;
		}

		size_t num_constraints = ast_env_num_constraints(ctx);
		for (ast_constraint_id constr_id = num_applied_constraints;
				constr_id < num_constraints; constr_id++) {
			ast_slot_solve_impose_constraint(
					ctx, constr_id);
		}
		num_applied_constraints = num_constraints;

		// Immediatly apply new constraints and slots.
		if (num_applied_constraints < ast_env_num_constraints(ctx)) {
			made_progress = true;
			continue;
		}

		for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
			bool this_slot_made_progress;
			
			this_slot_made_progress = ast_slot_solve_push_value(
					ctx, slot_id);
			made_progress |= this_slot_made_progress;
		}
	};

	// Verify the solution.
	for (ast_constraint_id constr_id = 0;
			constr_id < num_applied_constraints; constr_id++) {
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
	printf("Constraints:\n");
	{
		int num_digits_constr_id;
		num_digits_constr_id =
			(int)ceil(log10((double)num_applied_constraints));
		if (num_digits_constr_id <= 0) {
			num_digits_constr_id = 1;
		}

		for (size_t constr_id = 0; constr_id < num_applied_constraints; constr_id++) {
			if (constr_id == original_num_constraints) {
				printf("\nextra constraints:\n");
			}
			printf("%*zu ",
					num_digits_constr_id, constr_id);
			ast_print_constraint(ctx, constr_id);
			printf("\n");
		}
	}

	printf("\nfinal slots (%zu slot%s):\n", ctx->num_slots,
			(ctx->num_slots == 1) ? "" : "s");

	for (ast_slot_id slot_id = 0; slot_id < ctx->num_slots; slot_id++) {
		ast_print_slot(ctx, out_result, slot_id);
	}

	if (ctx->num_extra_slots > 0) {
		printf("extra slots:\n");
		for (ast_slot_id slot_id = 0; slot_id < ctx->num_extra_slots; slot_id++) {
			ast_print_slot(ctx, out_result,
					ctx->num_slots+slot_id);
		}
	}

	printf("======  end solve slots  ======\n");
#endif

	ast_env_free(ctx->env);
	free(ctx->extra_slots);

	return -num_errors;
}
