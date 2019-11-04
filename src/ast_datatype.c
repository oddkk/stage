#include "vm.h"
#include "ast.h"
#include "dlist.h"
#include "module.h"
#include "base/mod.h"
#include <stdlib.h>
#include <string.h>

// For sysconf
#include <unistd.h>

// For mmap
#include <sys/mman.h>

typedef int ast_dt_job_id;

struct ast_dt_expr_jobs {
	ast_dt_job_id resolve_names;
	ast_dt_job_id resolve_types;
	ast_dt_job_id codegen;
};

struct ast_dt_bind {
	ast_member_id target;
	bool overridable;

	enum ast_object_def_bind_kind kind;
	union {
		struct {
			struct ast_node *node;
			func_id func;
		} value;
		struct ast_object_def *pack;
	};

	struct stg_location loc;

	ast_member_id *deps;
	size_t num_deps;

	struct ast_dt_expr_jobs value_jobs;
	struct ast_dt_bind *next_alloced;
};

struct ast_dt_dependency;

enum ast_dt_member_flags {
	AST_DT_MEMBER_IS_LOCAL = 0x1,
	AST_DT_MEMBER_IS_CONST = 0x2,
};

struct ast_dt_member {
	struct atom *name;
	type_id type;
	struct ast_node *type_node;

	struct stg_location decl_loc;
	struct ast_dt_bind *bound;
	struct ast_dt_bind *overridden_bind;

	enum ast_dt_member_flags flags;
	union {
		// If flags IS_LOCAL is true.
		ast_member_id first_child;

		// If flags IS_LOCAL is false.
		ast_member_id anscestor_local_member;
	};

	struct ast_dt_expr_jobs type_jobs;

	struct object const_value;

	ast_member_id persistant_id;

	// Used as part of the terminal_nodes linked list until after the member
	// has been sorted. Then next becomes the id of the next member in the
	// topological order. The index of another member.
	ast_member_id next;
};

struct ast_dt_dependency {
	ast_member_id from, to;
	bool visited;
};

enum ast_dt_job_kind {
	AST_DT_JOB_FREE = 0,
	AST_DT_JOB_RESOLVE_NAMES,
	AST_DT_JOB_RESOLVE_TYPES,
	AST_DT_JOB_CODEGEN,
};

enum ast_dt_job_expr {
	AST_DT_JOB_EXPR_TARGET,
	AST_DT_JOB_EXPR_BIND,
	AST_DT_JOB_EXPR_TYPE,
};

struct ast_dt_job_dep {
	bool visited;
	ast_dt_job_id to;
};

struct ast_dt_job {
	enum ast_dt_job_kind kind;
	enum ast_dt_job_expr expr;

	size_t num_incoming_deps;
	size_t num_outgoing_deps;
	struct ast_dt_job_dep *outgoing_deps;

	// Used for the linked list terminal_jobs in ast_dt_context.
	ast_dt_job_id terminal_jobs;

	union {
		// For AST_DT_JOB_EXPR_TYPE
		ast_member_id member;

		// For AST_DT_JOB_EXPR_BIND and AST_DT_JOB_EXPR_TARGET
		struct ast_dt_bind *bind;

		// Used when the job is not allocated.
		ast_dt_job_id free_list;
	};
};

struct ast_dt_context {
	struct ast_dt_member *members;
	size_t num_members;

	struct ast_node *root_node;
	// An array of the ids of all local members, in the same order as
	// root_node->composite.members. The lenght is root_node->composite.num_members.
	ast_member_id *local_member_ids;

	struct ast_dt_bind *alloced_binds;

	// A linked list of all nodes with no incoming edges. The index of another
	// member.
	ast_member_id terminal_nodes;
	size_t unvisited_edges;

	struct ast_dt_job **job_pages;
	size_t num_job_pages;
	size_t page_size;
	ast_dt_job_id free_list;
	// A linked list of nodes that have no incoming edges.
	ast_dt_job_id terminal_jobs;
	size_t unvisited_job_deps;

	struct ast_context *ast_ctx;
	struct ast_env *ast_env;

	size_t num_errors;
};

static inline struct ast_dt_job *
get_job(struct ast_dt_context *ctx, ast_dt_job_id id)
{
	assert(ctx->page_size > 0);
	size_t jobs_per_page = ctx->page_size / sizeof(struct ast_dt_job);
	assert(id < jobs_per_page * ctx->num_job_pages);

	return &ctx->job_pages[id / jobs_per_page][id % jobs_per_page];
}

static inline struct ast_dt_member *
get_member(struct ast_dt_context *ctx, ast_member_id id)
{
	assert(id < ctx->num_members);
	return &ctx->members[id];
}

static inline void
ast_dt_free_job(struct ast_dt_context *ctx, ast_dt_job_id id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, id);
	assert(job->kind != AST_DT_JOB_FREE);
	memset(job, 0, sizeof(struct ast_dt_job));
	job->kind = AST_DT_JOB_FREE;

	job->free_list = ctx->free_list;
	ctx->free_list = id;
}

static ast_dt_job_id
ast_dt_alloc_job(struct ast_dt_context *ctx)
{
	if (ctx->page_size == 0) {
		ctx->page_size = sysconf(_SC_PAGESIZE);
	}

	size_t jobs_per_page = ctx->page_size / sizeof(struct ast_dt_job);

	if (ctx->job_pages == NULL || ctx->free_list == -1) {
		size_t new_size = (ctx->num_job_pages + 1) * sizeof(struct ast_dt_job *);
		struct ast_dt_job **new_pages = realloc(ctx->job_pages, new_size);

		if (!new_pages) {
			perror("realloc");
			return -1;
		}

		ctx->job_pages = new_pages;

		struct ast_dt_job *new_jobs;
		new_jobs = mmap(
				NULL, ctx->page_size,
				PROT_READ|PROT_WRITE,
				MAP_PRIVATE|MAP_ANONYMOUS,
				-1, 0);

		new_pages[ctx->num_job_pages] = new_jobs;

		if (new_pages[ctx->num_job_pages] == MAP_FAILED) {
			perror("mmap");
			return -1;
		}

		ctx->num_job_pages += 1;

		for (size_t i = 0; i < jobs_per_page-1; i++) {
			new_jobs[i].free_list = i+1;
		}

		new_jobs[jobs_per_page-1].free_list = -1;
		ctx->free_list = jobs_per_page * (ctx->num_job_pages-1);
	}

	assert(ctx->free_list >= 0);

	ast_dt_job_id res;
	res = ctx->free_list;

	struct ast_dt_job *job;
	job = get_job(ctx, res);

	ctx->free_list = job->free_list;
	job->free_list = -1;

	job->terminal_jobs = ctx->terminal_jobs;
	ctx->terminal_jobs = res;

	return res;
}

static ast_dt_job_id
ast_dt_job_type(struct ast_dt_context *ctx,
		ast_member_id mbr_id, enum ast_dt_job_kind kind)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = kind;

	job->expr = AST_DT_JOB_EXPR_TYPE;
	job->member = mbr_id;

	return job_id;
}

static ast_dt_job_id
ast_dt_job_bind(struct ast_dt_context *ctx,
		struct ast_dt_bind *bind, enum ast_dt_job_kind kind)
{
	ast_dt_job_id job_id;
	job_id = ast_dt_alloc_job(ctx);

	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	job->kind = kind;

	job->expr = AST_DT_JOB_EXPR_BIND;
	job->bind = bind;

	return job_id;
}

static void
ast_dt_job_remove_from_terminal_jobs(struct ast_dt_context *ctx,
		ast_dt_job_id target_id)
{
	struct ast_dt_job *target;
	target = get_job(ctx, target_id);

	for (ast_dt_job_id *job_id = &ctx->terminal_jobs;
			(*job_id) >= 0;
			job_id = &get_job(ctx, *job_id)->terminal_jobs) {
		if ((*job_id) == target_id) {
			(*job_id) = target->terminal_jobs;
			target->terminal_jobs = -1;
			return;
		}
	}
}

static const char *
ast_dt_job_kind_name(enum ast_dt_job_kind kind) {
	switch (kind) {
		case AST_DT_JOB_RESOLVE_NAMES:
			return "RESOLVE_NAMES";

		case AST_DT_JOB_RESOLVE_TYPES:
			return "RESOLVE_TYPES";

		case AST_DT_JOB_CODEGEN:
			return "CODEGEN";

		case AST_DT_JOB_FREE:
			return "FREE";
	}
	return "(unknown)";
}

static const char *
ast_dt_job_expr_name(enum ast_dt_job_expr expr) {
	switch (expr) {
		case AST_DT_JOB_EXPR_TARGET:
			return "TARGET";

		case AST_DT_JOB_EXPR_BIND:
			return "BIND";

		case AST_DT_JOB_EXPR_TYPE:
			return "TYPE";
	}
	return "(unknown)";
}

static void
ast_dt_print_job_desc(struct ast_dt_context *ctx,
		ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);
	printf("0x%03x (%13s, %s: ", job_id,
			ast_dt_job_kind_name(job->kind),
			ast_dt_job_expr_name(job->expr));

	ast_member_id mbr_id = -1;
	switch (job->expr) {
		case AST_DT_JOB_EXPR_TARGET:
		case AST_DT_JOB_EXPR_BIND:
			mbr_id = job->bind->target;
			break;

		case AST_DT_JOB_EXPR_TYPE:
			mbr_id = job->member;
			break;
	}

	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);
	printf("mbr 0x%03x[%-10.*s])", mbr_id, ALIT(mbr->name));
}

// Requests that from must be evaluated before to.
static void
ast_dt_job_depndency(struct ast_dt_context *ctx,
		ast_dt_job_id from_id, ast_dt_job_id to_id)
{
	struct ast_dt_job *from, *to;
	from = get_job(ctx, from_id);
	to = get_job(ctx, to_id);

	printf("job dep ");
	ast_dt_print_job_desc(ctx, from_id);
	printf(" -> ");
	ast_dt_print_job_desc(ctx, to_id);
	printf("\n");

	struct ast_dt_job_dep dep = {0};
	dep.visited = false;
	dep.to = to_id;

	dlist_append(
			from->outgoing_deps,
			from->num_outgoing_deps,
			&dep);

	if (to->num_incoming_deps == 0) {
		ast_dt_job_remove_from_terminal_jobs(ctx, to_id);
	}

	assert(to->terminal_jobs < 0);

	to->num_incoming_deps += 1;
	ctx->unvisited_job_deps += 1;
}

static struct ast_dt_bind *
ast_dt_register_bind(struct ast_dt_context *ctx,
		struct stg_location loc, ast_member_id target,
		struct ast_node *value, bool overridable)
{
	struct ast_dt_member *member;
	member = get_member(ctx, target);

	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));
	new_bind->target = target;
	new_bind->kind = AST_OBJECT_DEF_BIND_VALUE;
	new_bind->value.node = value;
	new_bind->overridable = overridable;
	new_bind->loc = loc;

	new_bind->value_jobs.resolve_names =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_RESOLVE_NAMES);

	new_bind->value_jobs.resolve_types =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_RESOLVE_TYPES);

	new_bind->value_jobs.codegen =
		ast_dt_job_bind(ctx, new_bind,
				AST_DT_JOB_CODEGEN);

	ast_dt_job_depndency(ctx,
			new_bind->value_jobs.resolve_names,
			new_bind->value_jobs.resolve_types);

	ast_dt_job_depndency(ctx,
			new_bind->value_jobs.resolve_types,
			new_bind->value_jobs.codegen);

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	if (member->bound) {
		if (!member->bound->overridable && !overridable) {
			stg_error(ctx->ast_ctx->err, loc,
					"'%.*s' is bound multiple times.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return new_bind;
		}

		if (member->bound->overridable && overridable) {
			stg_error(ctx->ast_ctx->err, loc,
					"'%.*s' has multiple default binds.", ALIT(member->name));
			stg_appendage(ctx->ast_ctx->err,
					member->bound->loc, "Also bound here.");
			ctx->num_errors += 1;
			return new_bind;
		}

		if (overridable) {
			member->overridden_bind = new_bind;
		} else {
			member->overridden_bind = member->bound;
			member->bound = new_bind;
		}
	} else {
		member->bound = new_bind;
	}

	return new_bind;
}

/*
static struct ast_dt_bind *
ast_dt_register_bind_func(struct ast_dt_context *ctx,
		ast_member_id target, func_id func,
		ast_member_id *value_params, size_t num_value_params,
		bool overridable)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));
	new_bind->target = target;
	new_bind->kind = AST_OBJECT_DEF_BIND_VALUE;
	new_bind->value.func = func;
	new_bind->overridable = overridable;
	new_bind->deps = value_params;
	new_bind->num_deps = num_value_params;

	new_bind->value_jobs.resolve_names = -1;
	new_bind->value_jobs.resolve_types = -1;
	new_bind->value_jobs.codegen = -1;


	new_bind->loc = STG_NO_LOC;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	return new_bind;
}

static struct ast_dt_bind *
ast_dt_register_bind_pack(struct ast_dt_context *ctx,
		ast_member_id target, struct ast_object_def *pack,
		ast_member_id *value_params, size_t num_value_params)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));
	new_bind->target = target;
	new_bind->kind = AST_OBJECT_DEF_BIND_PACK;
	new_bind->pack = pack;
	new_bind->overridable = false;
	new_bind->deps = value_params;
	new_bind->num_deps = num_value_params;

	new_bind->value_jobs.resolve_names = -1;
	new_bind->value_jobs.resolve_types = -1;
	new_bind->value_jobs.codegen = -1;

	new_bind->loc = STG_NO_LOC;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	return new_bind;
}
*/

static ast_member_id
ast_dt_register_member(struct ast_dt_context *ctx,
		struct atom *name, struct stg_location decl_loc)
{
	struct ast_dt_member new_mbr = {0};

	new_mbr.name = name;
	new_mbr.decl_loc = decl_loc;
	new_mbr.next = ctx->terminal_nodes;
	new_mbr.first_child = -1;
	new_mbr.anscestor_local_member = -1;
	new_mbr.persistant_id = -1;

	ast_member_id mbr_id;
	mbr_id = dlist_append(
			ctx->members,
			ctx->num_members,
			&new_mbr);

	ctx->terminal_nodes = mbr_id;

	struct ast_dt_member *mbr_ptr;
	mbr_ptr = get_member(ctx, mbr_id);

	mbr_ptr->type_jobs.resolve_names =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_RESOLVE_NAMES);

	mbr_ptr->type_jobs.resolve_types =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_RESOLVE_TYPES);

	mbr_ptr->type_jobs.codegen =
		ast_dt_job_type(ctx, mbr_id,
				AST_DT_JOB_CODEGEN);

	ast_dt_job_depndency(ctx,
			mbr_ptr->type_jobs.resolve_names,
			mbr_ptr->type_jobs.resolve_types);

	ast_dt_job_depndency(ctx,
			mbr_ptr->type_jobs.resolve_types,
			mbr_ptr->type_jobs.codegen);


	return mbr_id;
}

enum ast_node_flags {
	AST_NODE_FLAG_OK           = 0x00,
	AST_NODE_FLAG_ERROR        = 0x01,
	AST_NODE_FLAG_NOT_TYPED    = 0x02,
	AST_NODE_FLAG_NOT_BOUND    = 0x04,
	AST_NODE_FLAG_NOT_CONST    = 0x08,
	AST_NODE_FLAG_NOT_RESOLVED = 0x10,
};

enum ast_node_flags
ast_slot_analyze(struct ast_dt_context *ctx, ast_slot_id slot_id)
{
	enum ast_node_flags result = AST_NODE_FLAG_OK;

	if (slot_id == AST_BIND_FAILED) {
		result |= AST_NODE_FLAG_ERROR;
		return result;
	}

	struct ast_env_slot slot;
	slot = ast_env_slot(ctx->ast_ctx, ctx->ast_env, slot_id);

	if (slot.type != AST_SLOT_TYPE &&
			ast_slot_analyze(ctx, slot.type) != AST_NODE_FLAG_OK) {
		result |= AST_NODE_FLAG_NOT_TYPED;
	}

	switch (slot.kind) {
		case AST_SLOT_WILDCARD:
			result |= AST_NODE_FLAG_NOT_RESOLVED;
			break;

		case AST_SLOT_CONST_TYPE:
			assert((result & AST_NODE_FLAG_NOT_TYPED) == 0);
			break;

		case AST_SLOT_CONST:
			assert((result & AST_NODE_FLAG_NOT_TYPED) == 0);
			break;

		case AST_SLOT_PARAM:
			result |= AST_NODE_FLAG_NOT_CONST;
			break;

		case AST_SLOT_MEMBER:
			if (slot.member_id >= 0) {
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, slot.member_id);

				if (mbr->type == TYPE_UNSET) {
					result |= AST_NODE_FLAG_NOT_TYPED;
				}

				if (!mbr->bound || mbr->bound->overridable) {
					result |= AST_NODE_FLAG_NOT_CONST;
				}

				// TODO: We might want to do a recursive check on each member
				// dependency to determine if they are constant as they might
				// not have been visited yet.
				if ((mbr->flags & AST_DT_MEMBER_IS_CONST) == 0) {
					result |= AST_NODE_FLAG_NOT_CONST;
				}
			} else {
				result |= AST_NODE_FLAG_NOT_BOUND;
			}
			break;

		case AST_SLOT_CLOSURE:
			result |= AST_NODE_FLAG_NOT_CONST;
			break;

		case AST_SLOT_TEMPL:
			break;

		case AST_SLOT_CONS:
			if (!slot.cons.def) {
				result |= AST_NODE_FLAG_NOT_RESOLVED;
			}
			for (size_t i = 0; i < slot.cons.num_present_args; i++) {
				result |= ast_slot_analyze(ctx, slot.cons.args[i].slot);
			}
			break;

		case AST_SLOT_CONS_ARRAY:
			break;

		case AST_SLOT_ERROR:
			result |= AST_NODE_FLAG_ERROR;
			break;

		case AST_SLOT_SUBST:
			return ast_slot_analyze(ctx, slot.subst);
	}

	// printf("Analyze slot %s: 0x%x\n", ast_slot_name(slot.kind), result);

	return result;
}

enum ast_node_flags
ast_node_analyze_name_ref(struct ast_dt_context *ctx, struct ast_name_ref ref)
{
	enum ast_node_flags result = AST_NODE_FLAG_OK;

	switch (ref.kind) {
		case AST_NAME_REF_NOT_FOUND:
			result |= AST_NODE_FLAG_ERROR;
			break;

		case AST_NAME_REF_MEMBER:
			// TODO: Get const and type information about member.
			break;

		case AST_NAME_REF_PARAM:
			// TODO: Get const and type information about param.
			break;

		case AST_NAME_REF_CLOSURE:
			// TODO: Get const and type information about closure.
			break;
	}

	return result;
}

enum ast_node_flags
ast_node_analyze_closure(struct ast_dt_context *ctx, struct ast_closure_target *closure)
{
	enum ast_node_flags result = AST_NODE_FLAG_OK;

	for (size_t i = 0; i < closure->num_members; i++) {
		result |= ast_node_analyze_name_ref(ctx, closure->members[i].ref);
	}

	return result;
}

enum ast_node_flags
ast_node_analyze(struct ast_dt_context *ctx, struct ast_node *node)
{
	enum ast_node_flags result = AST_NODE_FLAG_OK;

	switch (node->kind) {
		case AST_NODE_FUNC:
			result |= ast_node_analyze_closure(ctx,
					&node->func.closure);
			// fallthrough

		case AST_NODE_FUNC_NATIVE:
			for (size_t i = 0; i < node->func.num_params; i++) {
				result |= ast_node_analyze(ctx, node->func.params[i].type);
			}
			break;

		case AST_NODE_CALL:
			// TODO: Check params.
			// TODO: Check if function is pure or not.
			break;

		case AST_NODE_CONS:
			// TODO: Check params.
			// TODO: Check if cons is pure or not.
			break;

		case AST_NODE_TEMPL:
			break;

		case AST_NODE_ACCESS:
			result |= ast_node_analyze(ctx, node->access.target);
			if ((result & AST_NODE_FLAG_NOT_TYPED) == 0) {
			}
			break;

		case AST_NODE_SLOT:
			result |= ast_slot_analyze(ctx,
					ast_node_resolve_slot(ctx->ast_env, &node->slot));
			break;

		case AST_NODE_LIT:
			result |= ast_slot_analyze(ctx,
					ast_node_resolve_slot(ctx->ast_env, &node->lit.slot));
			break;

		case AST_NODE_LOOKUP:
			result |= ast_node_analyze_name_ref(ctx, node->lookup.ref);
			break;

		case AST_NODE_COMPOSITE:
			result |= ast_node_analyze_closure(ctx,
					&node->composite.closure);
			break;

		case AST_NODE_VARIANT:
			break;
	}

	return result;
}

/*
static void
ast_dt_tag_member_const(struct ast_dt_context *ctx,
		ast_member_id mbr_id, struct object obj)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	mbr->flags |= AST_DT_MEMBER_IS_CONST;
	mbr->const_value = obj;
}

static bool
ast_dt_try_bind_const_member(struct ast_dt_context *ctx,
		struct ast_module *mod, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	switch (mbr->bound->kind) {
		case AST_OBJECT_DEF_BIND_VALUE:
			if (mbr->bound->value.func != FUNC_UNSET) {
				bool is_const = true;
				for (size_t i = 0; i < mbr->bound->num_deps; i++) {
					struct ast_dt_member *dep = get_member(ctx, mbr->bound->deps[i]);
					if ((dep->flags & AST_DT_MEMBER_IS_CONST) == 0) {
						is_const = false;
						break;
					}
				}

				if (is_const) {
					mbr->flags |= AST_DT_MEMBER_IS_CONST;

					assert(mbr->type != TYPE_UNSET);

					struct type *mbr_type;
					mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

					uint8_t obj_buffer[mbr_type->size];
					struct object obj = {0};
					obj.type = mbr->type;
					obj.data = obj_buffer;

					struct object args[mbr->bound->num_deps];
					for (size_t i = 0; i < mbr->bound->num_deps; i++) {
						struct ast_dt_member *dep = get_member(ctx, mbr->bound->deps[i]);
						args[i] = dep->const_value;
					}

					int err;

					err = vm_call_func(ctx->ast_ctx->vm,
							mbr->bound->value.func, args,
							mbr->bound->num_deps, &obj);

					obj = register_object(
							ctx->ast_ctx->vm, mod->env.store, obj);

					ast_dt_tag_member_const(ctx, mbr_id, obj);
					return true;
				}
			} else {
				enum ast_node_flags flags;
				flags = ast_node_analyze(ctx, mbr->bound->value.node);

				if (flags == AST_NODE_FLAG_OK) {
					int err;
					struct object obj;
					err = ast_node_eval(ctx->ast_ctx, mod,
							ctx->ast_env, mbr->bound->value.node, &obj);
					if (err) {
						printf("Failed to evaluate value.\n");
						return false;
					}

					ast_dt_tag_member_const(ctx, mbr_id, obj);

					return true;
				}
			}
			break;

		case AST_OBJECT_DEF_BIND_PACK:
			{
				void *args[mbr->bound->num_deps];
				for (size_t i = 0; i < mbr->bound->num_deps; i++) {
					struct ast_dt_member *dep;
					dep = get_member(ctx, mbr->bound->deps[i]);

					if ((dep->flags & AST_DT_MEMBER_IS_CONST) == 0) {
						return false;
					}

					args[i] = dep->const_value.data;
				}

				struct type *mbr_type;
				mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

				uint8_t buffer[mbr_type->size];

				mbr->bound->pack->pack_func(ctx->ast_ctx->vm,
						mbr->bound->pack->data, buffer,
						args, mbr->bound->num_deps);

				struct object obj = {0};
				obj.data = buffer;
				obj.type = mbr->type;

				obj = register_object(ctx->ast_ctx->vm,
						mod->env.store, obj);

				ast_dt_tag_member_const(ctx, mbr_id, obj);
			}
			break;
	}

	return false;
}
*/

static void
ast_dt_composite_populate(struct ast_dt_context *ctx, struct ast_node *node)
{
	assert(node->kind == AST_NODE_COMPOSITE);

	ast_member_id *members;
	members = calloc(node->composite.num_members, sizeof(ast_member_id));
	ctx->local_member_ids = members;

	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_datatype_member *mbr;
		mbr = &node->composite.members[i];

		// TODO: Better location.
		ast_member_id mbr_id;
		mbr_id = ast_dt_register_member(ctx, mbr->name,
				node->composite.members[i].loc);

		members[i] = mbr_id;

		struct ast_dt_member *new_mbr;
		new_mbr = get_member(ctx, mbr_id);

		new_mbr->flags |= AST_DT_MEMBER_IS_LOCAL;
		new_mbr->first_child = -1;
	}

	for (size_t i = 0; i < node->composite.num_binds; i++) {
		ast_composite_node_resolve_names(
				ctx->ast_ctx, ctx->ast_env, NULL, NULL, false,
				node, node->composite.binds[i].target, members);
	}

	for (size_t i = 0; i < node->composite.num_binds; i++) {
		struct ast_node *target_node;
		target_node = node->composite.binds[i].target;

		// TODO: Support more complex bind targets.
		assert(target_node->kind == AST_NODE_LOOKUP);
		assert(target_node->lookup.ref.kind == AST_NAME_REF_MEMBER);

		ast_dt_register_bind(ctx, node->composite.binds[i].loc,
				target_node->lookup.ref.member,
				node->composite.binds[i].value,
				node->composite.binds[i].overridable);
	}

	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_datatype_member *node_mbr;
		node_mbr = &node->composite.members[i];

		ast_member_id mbr_id;
		mbr_id = members[i];

		struct ast_dt_member *mbr;
		mbr = get_member(ctx, mbr_id);

		if (mbr->bound) {
			// TODO: We should evaluate the binds that is being overridden.

			if (!mbr->type_node) {
				ast_dt_job_depndency(ctx,
						mbr->bound->value_jobs.resolve_types,
						mbr->type_jobs.resolve_names);
			}
		} else {
			assert(mbr->type_node);
		}
	}
}

static void
ast_dt_add_dependency_on_member(struct ast_dt_context *ctx,
		ast_dt_job_id target_job, enum ast_name_dep_requirement req,
		ast_member_id mbr_id)
{
	struct ast_dt_member *mbr;
	mbr = get_member(ctx, mbr_id);

	switch (req) {
		case AST_NAME_DEP_REQUIRE_TYPE:
			ast_dt_job_depndency(ctx,
					mbr->type_jobs.codegen,
					target_job);
			break;

		case AST_NAME_DEP_REQUIRE_VALUE:
			if (!mbr->bound) {
				printf("Can not depend on the value of this"
						"member as it is not bound.\n");
				return;
			}
			if (mbr->bound->overridable) {
				printf("Can not depend on the value of this"
						"member as it is overridable.\n");
				return;
			}

			ast_dt_job_depndency(ctx,
					mbr->bound->value_jobs.codegen,
					target_job);
			break;
	}
}

static void
ast_dt_find_named_dependencies(struct ast_dt_context *ctx,
		ast_dt_job_id target_job, enum ast_name_dep_requirement req,
		struct ast_node *node)
{
	struct ast_name_dep *deps = NULL;
	size_t num_deps = 0;

	ast_node_find_named_dependencies(
			node, req, &deps, &num_deps);

	for (size_t i = 0; i < num_deps; i++) {
		if (deps[i].ref.kind == AST_NAME_REF_MEMBER) {
			ast_dt_add_dependency_on_member(
					ctx, target_job, deps[i].req, deps[i].ref.member);
		}
	}

	free(deps);
}

static inline int
ast_dt_dispatch_job(struct ast_dt_context *ctx, ast_dt_job_id job_id)
{
	struct ast_dt_job *job;
	job = get_job(ctx, job_id);

	printf("Dispatch job ");
	ast_dt_print_job_desc(ctx, job_id);
	printf("\n");

	struct ast_node *node;

	switch (job->expr) {
		case AST_DT_JOB_EXPR_TARGET:
			return 0;

		case AST_DT_JOB_EXPR_BIND:
			node = job->bind->value.node;
			break;

		case AST_DT_JOB_EXPR_TYPE:
			{
				struct ast_dt_member *mbr;
				mbr = get_member(ctx, job->member);
				node = mbr->type_node;

				if (!node) {
					assert(mbr->type != TYPE_UNSET);
					return 0;
				}
			}
			break;
	}

	switch (job->kind) {
		case AST_DT_JOB_RESOLVE_NAMES:
			{
				bool require_const = false;
				ast_dt_job_id next_job = -1;
				enum ast_name_dep_requirement dep_req;
				dep_req = AST_NAME_DEP_REQUIRE_TYPE;

				switch (job->expr) {
					case AST_DT_JOB_EXPR_TARGET:
						break;

					case AST_DT_JOB_EXPR_BIND:
						next_job = job->bind->value_jobs.resolve_types;
						break;

					case AST_DT_JOB_EXPR_TYPE:
						{
							struct ast_dt_member *mbr;
							mbr = get_member(ctx, job->member);

							next_job = mbr->type_jobs.resolve_types;

							require_const = true;
							dep_req = AST_NAME_DEP_REQUIRE_VALUE;
						}
						break;
				}

				int err;
				err = ast_composite_node_resolve_names(ctx->ast_ctx, ctx->ast_env,
						NULL, NULL, require_const, ctx->root_node, node,
						ctx->local_member_ids);
				if (err) {
					printf("Failed to resolve names.\n");
					break;
				}

				ast_dt_find_named_dependencies(
						ctx, next_job, dep_req, node);
			}
			return 0;

		case AST_DT_JOB_RESOLVE_TYPES:
			break;

		case AST_DT_JOB_CODEGEN:
			break;

		case AST_DT_JOB_FREE:
			panic("Attempted to dispatch job that has been freed.");
			break;
	}

	return -1;
}

static int
ast_dt_run_jobs(struct ast_dt_context *ctx)
{
	// Dispatch the jobs in topological order. (Kahn's algorithm.)
	while (ctx->terminal_jobs >= 0) {
		ast_dt_job_id job_id;
		struct ast_dt_job *job;

		job_id = ctx->terminal_jobs;
		job = get_job(ctx, job_id);
		ctx->terminal_jobs = job->terminal_jobs;
		job->terminal_jobs = -1;

		ast_dt_dispatch_job(ctx, job_id);

		if (job->num_incoming_deps > 0) {
			// If the node gave itself new dependencies we don't mark it as
			// visited to allow it to pass through again.
			continue;
		}

		for (size_t i = 0; i < job->num_outgoing_deps; i++) {
			struct ast_dt_job_dep *dep;
			dep = &job->outgoing_deps[i];

			if (!dep->visited) {
				dep->visited = true;

				struct ast_dt_job *to;
				to = get_job(ctx, dep->to);

				assert(to->num_incoming_deps > 0);
				to->num_incoming_deps -= 1;

				assert(ctx->unvisited_job_deps > 0);
				ctx->unvisited_job_deps -= 1;

				if (to->num_incoming_deps == 0) {
					to->terminal_jobs = ctx->terminal_jobs;
					ctx->terminal_jobs = dep->to;
				}
			}
		}

		ast_dt_free_job(ctx, job_id);
	}

	if (ctx->unvisited_job_deps > 0) {
		printf("Failed to evalutate datatype because we found one or more cycles.\n");
		printf("Problematic jobs: \n");
		size_t jobs_per_page = ctx->page_size / sizeof(struct ast_dt_job);
		size_t cap_jobs = ctx->num_job_pages * jobs_per_page;
		for (ast_dt_job_id job_i = 0; job_i < cap_jobs; job_i++) {
			struct ast_dt_job *job;
			job = get_job(ctx, job_i);
			if (job->kind != AST_DT_JOB_FREE) {
				printf(" - 0x%03x (%zu):", job_i, job->num_incoming_deps);

				// Find all refs to this job.
				for (ast_dt_job_id job_j = 0; job_j < cap_jobs; job_j++) {
					struct ast_dt_job *other;
					other = get_job(ctx, job_j);
					if (other->kind == AST_DT_JOB_FREE) {
						continue;
					}
					for (size_t i = 0; i < other->num_outgoing_deps; i++) {
						if (other->outgoing_deps[i].to == job_i) {
							printf(" 0x%03x", job_j);
						}
					}
				}

				printf("\n");
			}
		}

		printf("\n");
		return -1;
	}

	return 0;
}

static ast_member_id
ast_dt_num_descendant_members(struct ast_dt_context *ctx,
		struct ast_module *mod, type_id type)
{
	struct type *member_type;
	member_type = vm_get_type(ctx->ast_ctx->vm, type);

	if (member_type->obj_def) {
		return ast_object_def_num_descendant_members(
				ctx->ast_ctx, mod, member_type->obj_def);
	} else {
		return 0;
	}
}

static ast_member_id
ast_dt_calculate_persistant_id(struct ast_dt_context *ctx, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr = get_member(ctx, mbr_id);

	if ((mbr->flags & AST_DT_MEMBER_IS_LOCAL) != 0) {
		assert(mbr->persistant_id >= 0 && mbr->persistant_id < ctx->num_members);
		return mbr->persistant_id;
	} else {
		struct ast_dt_member *local_anscestor;
		local_anscestor = get_member(ctx, mbr->anscestor_local_member);
		assert((local_anscestor->flags & AST_DT_MEMBER_IS_LOCAL) != 0 &&
				local_anscestor->first_child >= 0 &&
				local_anscestor->first_child <= mbr_id);

		ast_member_id result;

		result = 1 + local_anscestor->persistant_id +
			(mbr_id - local_anscestor->first_child);

		assert(result >= 0 && result < ctx->num_members);

		return result;
	}
}

static int
ast_dt_make_bind_func(struct ast_dt_context *dt_ctx, struct ast_module *mod,
		struct ast_dt_bind *bind)
{
	struct ast_context *ctx = dt_ctx->ast_ctx;
	struct ast_env *env = dt_ctx->ast_env;

	if (bind->kind != AST_OBJECT_DEF_BIND_VALUE) {
		return 0;
	}

	assert(bind->value.node || bind->value.func);

	if (bind->value.func != FUNC_UNSET) {
		// The function is already generated.
		return 0;
	}

	struct func func = {0};

	type_id value_type;

	int err;
	err = ast_node_eval_type_of(ctx, mod, env,
			bind->value.node, &value_type);
	if (err) {
		printf("Failed to eval bind value type.\n");
		return -1;
	}

	size_t num_deps = bind->num_deps;
	struct atom *dep_names[num_deps];
	type_id dep_types[num_deps];

	for (size_t i = 0; i < num_deps; i++) {
		ast_member_id dep_i = bind->deps[i];
		struct ast_dt_member *dep = get_member(dt_ctx, dep_i);
		dep_names[i] = dep->name;

		assert(dep->type != TYPE_UNSET);
		dep_types[i] = dep->type;
	}

	func.type = stg_register_func_type(mod->stg_mod,
			value_type, dep_types, num_deps);

	func.kind = FUNC_BYTECODE;
	func.bytecode = ast_composite_bind_gen_bytecode(ctx, mod, env,
			dep_names, dep_types, num_deps, bind->value.node);

	func_id func_id;
	func_id = stg_register_func(mod->stg_mod, func);

	bind->value.func = func_id;

	return 0;
}

/*
static int
ast_dt_gen_bind(struct ast_dt_context *dt_ctx, struct ast_module *mod,
		struct ast_dt_bind *bind, struct ast_object_def_bind *binds,
		size_t *bind_i, ast_member_id target)
{
	struct ast_context *ctx = dt_ctx->ast_ctx;
	struct ast_env *env = dt_ctx->ast_env;

	struct ast_dt_member *mbr;
	mbr = get_member(dt_ctx, target);

	binds[*bind_i].target = target;

	binds[*bind_i].num_value_params = mbr->bound->num_deps;
	binds[*bind_i].value_params = calloc(mbr->bound->num_deps,
			sizeof(ast_slot_id));

	for (size_t dep_i = 0; dep_i < mbr->bound->num_deps; dep_i++) {
		binds[*bind_i].value_params[dep_i] = mbr->bound->deps[dep_i];
	}

	binds[*bind_i].value =

	binds[*bind_i].overridable = mbr->bound->overridable;
	(*bind_i) += 1;

	return 0;
}

static void
ast_dt_gen_binds_for_target(struct ast_dt_context *ctx, struct ast_module *mod,
		struct ast_dt_bind *bind, struct ast_object_def_bind *binds,
		size_t *bind_i, struct ast_node *target)
{
	switch (target->kind) {
		case AST_NODE_SLOT:
			{
				struct ast_env_slot slot;
				slot = ast_env_slot(ctx->ast_ctx, ctx->ast_env, target->slot);
				assert(slot.member_id >= 0);

				ast_dt_gen_bind(ctx, mod, bind, binds, bind_i, slot.member_id);
			}
			break;

		default:
			panic("TODO: Implement support for more complex bind targets.");
			break;
	}
}
*/

struct ast_dt_local_member {
	struct atom *name;
	type_id type;
	size_t location;
	size_t size;
};

struct ast_dt_composite_info {
	struct ast_dt_local_member *members;
	size_t num_members;
};

static struct string
ast_dt_composite_repr(struct vm *vm, struct arena *mem, struct type *type)
{
	struct ast_dt_composite_info *info = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("Struct { "));

	for (size_t i = 0; i < info->num_members; i++) {
		struct type *member_type;
		member_type = vm_get_type(vm, info->members[i].type);
		arena_string_append_sprintf(mem, &res, "%.*s: ", ALIT(info->members[i].name));
		arena_string_append_type_repr(&res, vm, mem, member_type);
		arena_string_append(mem, &res, STR("; "));
	}

	arena_string_append(mem, &res, STR("}"));

	return res;
}

static struct string
ast_dt_composite_obj_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	struct type *type = vm_get_type(vm, obj->type);
	struct ast_dt_composite_info *info = type->data;
	struct string res = arena_string_init(mem);

	arena_string_append(mem, &res, STR("{ "));

	for (size_t i = 0; i < info->num_members; i++) {
		struct object mbr = {0};
		mbr.type = info->members[i].type;
		mbr.data = (void *)((uint8_t *)obj->data + info->members[i].location);

		arena_string_append_sprintf(mem, &res, "%.*s=", ALIT(info->members[i].name));

		arena_string_append_obj_repr(&res, vm, mem, &mbr);
		arena_string_append(mem, &res, STR("; "));
	}

	arena_string_append(mem, &res, STR("}"));

	return res;
}

static void
ast_dt_pack_func(struct vm *vm, void *data, void *out,
		void **params, size_t num_params)
{
	struct ast_dt_composite_info *info = data;
	assert(info->num_members == num_params);

	uint8_t *out_ptr = out;

	for (size_t i = 0; i < num_params; i++) {
		memcpy(out_ptr+info->members[i].location,
				params[i], info->members[i].size);
	}
}

static struct object
ast_dt_unpack_func(struct ast_context *ctx, struct ast_env *env,
		struct ast_object_def *def, int param_id, struct object obj)
{
	struct ast_dt_composite_info *info = def->data;

	assert(param_id >= 0 && param_id < info->num_members);

	struct object result = {0};
	result.data = ((uint8_t *)obj.data) + info->members[param_id].location;
	result.type = info->members[param_id].type;

	return result;
}

struct type_base ast_dt_composite_base = {
	.name     = STR("Struct"),
	.repr     = ast_dt_composite_repr,
	.obj_repr = ast_dt_composite_obj_repr,

	// TODO: Implement
	// .free = ...,
};

static type_id
ast_dt_composite_make_type(struct ast_dt_context *ctx, struct ast_module *mod)
{
	struct ast_node *comp = ctx->root_node;

	struct ast_object_def *def;
	def = ast_object_def_register(mod->env.store);
	def->pack_func = ast_dt_pack_func;
	def->unpack    = ast_dt_unpack_func;

	struct ast_dt_local_member *local_members;
	local_members = calloc(comp->composite.num_members,
			sizeof(struct ast_dt_local_member));

	struct ast_object_def_param *params;
	params = calloc(comp->composite.num_members,
			sizeof(struct ast_object_def_param));

	size_t num_bound_members = 0;
	for (ast_member_id mbr = 0; mbr < ctx->num_members; mbr++) {
		if (get_member(ctx, mbr)->bound) {
			num_bound_members += 1;
		}
	}
	struct ast_object_def_bind *binds;
	binds = calloc(num_bound_members,
			sizeof(struct ast_object_def_bind));

	size_t offset = 0;
	ast_member_id cumulative_persistant_id = 0;
	for (size_t i = 0; i < comp->composite.num_members; i++) {
		params[i].param_id = i;
		params[i].name = comp->composite.members[i].name;
		local_members[i].name = comp->composite.members[i].name;
		local_members[i].location = offset;

		ast_member_id mbr_id;
		mbr_id = ctx->local_member_ids[i];

		struct ast_dt_member *mbr;
		mbr = get_member(ctx, mbr_id);

		params[i].slot =
			ast_bind_slot_member(ctx->ast_ctx, &def->env, AST_BIND_NEW, NULL,
					ast_bind_slot_const_type(ctx->ast_ctx, &def->env, AST_BIND_NEW,
						NULL, mbr->type));

		// NOTE: We keep the old member_id here so that we can replace all
		// slot member ids after the binds have been added.
		def->env.slots[params[i].slot].member_id = mbr_id;

		mbr->persistant_id = cumulative_persistant_id;
		cumulative_persistant_id += 1 +
			ast_dt_num_descendant_members(ctx, mod, mbr->type);

		struct type *member_type;
		member_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

		local_members[i].type = mbr->type;
		local_members[i].size = member_type->size;

		offset += member_type->size;
	}

	// NOTE: ast_dt_calculate_persistant_id can only be used after this
	// point because it is dependent on persistant_id being set for local
	// members.

	def->params = params;
	def->num_params = comp->composite.num_members;

	size_t bind_i = 0;
	for (size_t mbr_i = 0; mbr_i < comp->composite.num_members; mbr_i++) {
		struct ast_dt_member *mbr = get_member(ctx, mbr_i);

		if (!mbr->bound) {
			continue;
		}

		int err;
		err = ast_dt_make_bind_func(ctx, mod, mbr->bound);

		binds[bind_i].kind = mbr->bound->kind;
		switch (mbr->bound->kind) {
			case AST_OBJECT_DEF_BIND_VALUE:
				assert(mbr->bound->value.func  != FUNC_UNSET);
				binds[bind_i].value.func        = mbr->bound->value.func;
				binds[bind_i].value.overridable = mbr->bound->overridable;
				break;

			case AST_OBJECT_DEF_BIND_PACK:
				assert(mbr->bound->pack);
				binds[bind_i].pack = mbr->bound->pack;
				break;
		}

		for (size_t i = 0; i < mbr->bound->num_deps; i++) {
			mbr->bound->deps[i] =
				ast_dt_calculate_persistant_id(
						ctx, mbr->bound->deps[i]);
		}

		binds[bind_i].value_params     = mbr->bound->deps;
		binds[bind_i].num_value_params = mbr->bound->num_deps;
		binds[bind_i].target =
			ast_dt_calculate_persistant_id(
					ctx, mbr->bound->target);

		bind_i += 1;
	}

	assert(bind_i == num_bound_members);

	def->binds = binds;
	def->num_binds = num_bound_members;

	struct ast_dt_composite_info *info;
	info = calloc(1, sizeof(struct ast_dt_composite_info));

	info->num_members = comp->composite.num_members;
	info->members = local_members;

	def->data = info;

	struct type dt_type = {0};
	dt_type.base = &ast_dt_composite_base;
	dt_type.obj_def = def;
	dt_type.size = offset;
	dt_type.data = info;

	type_id result;
	result = stg_register_type(mod->stg_mod, dt_type);

	def->ret_type = ast_bind_slot_const_type(
			ctx->ast_ctx, &def->env, AST_BIND_NEW, NULL, result);

	return result;
}

type_id
ast_dt_finalize_composite(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *comp)
{
	if (comp->composite.type != TYPE_UNSET) {
		return comp->composite.type;
	}

	struct ast_dt_context dt_ctx = {0};
	dt_ctx.ast_ctx = ctx;
	dt_ctx.ast_env = env;
	dt_ctx.root_node = comp;
	dt_ctx.terminal_nodes = -1;
	dt_ctx.terminal_jobs  = -1;

	int err;

	ast_dt_composite_populate(&dt_ctx, comp);

	err = ast_dt_run_jobs(&dt_ctx);
	if (err) {
		printf("One or more jobs failed when resolving datastructure.\n");
	}

	printf("done with datastructure for now.\n");
	return -1;

	type_id result = TYPE_UNSET;

	if (dt_ctx.num_errors == 0) {
		result = ast_dt_composite_make_type(&dt_ctx, mod);
	}

	free(dt_ctx.members);
	free(dt_ctx.local_member_ids);

	for (struct ast_dt_bind *bind = dt_ctx.alloced_binds;
			bind != NULL;) {
		struct ast_dt_bind *this_bind = bind;
		bind = bind->next_alloced;
		free(this_bind);
	}

	comp->composite.type = result;
	return result;
}
