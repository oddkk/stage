#include "vm.h"
#include "ast.h"
#include "dlist.h"
#include "module.h"
#include "base/mod.h"
#include <stdlib.h>
#include <string.h>

struct ast_dt_bind {
	struct ast_node *target;
	struct ast_node *value;
	bool overridable;

	ast_member_id *deps;
	size_t num_deps;

	struct ast_dt_bind *next_alloced;
};

struct ast_dt_dependency;

struct ast_dt_member {
	struct atom *name;
	ast_slot_id slot;
	type_id type;

	struct stg_location decl_loc;
	struct ast_dt_bind *bound;

	struct ast_dt_dependency *outgoing_edges;
	size_t num_outgoing_edges;

	size_t num_incoming_edges;

	bool is_local_member;
	union {
		// If is_local_member is true.
		ast_member_id first_child;

		// If is_local_member is false.
		ast_member_id anscestor_local_member;
	};

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

struct ast_dt_context {
	struct ast_dt_member *members;
	size_t num_members;

	struct ast_dt_bind *alloced_binds;

	// A linked list of all nodes with no incoming edges. The index of another
	// member.
	ast_member_id terminal_nodes;
	size_t unvisited_edges;

	struct ast_context *ast_ctx;
	struct ast_env *ast_env;

	size_t num_errors;
};

static inline struct ast_dt_member *
get_member(struct ast_dt_context *ctx, ast_member_id id)
{
	assert(id < ctx->num_members);
	return &ctx->members[id];
}

static struct ast_dt_bind *
ast_dt_register_bind(struct ast_dt_context *ctx,
		struct ast_node *target, struct ast_node *value, bool overridable)
{
	struct ast_dt_bind *new_bind = calloc(1, sizeof(struct ast_dt_bind));
	new_bind->target = target;
	new_bind->value = value;
	new_bind->overridable = overridable;

	new_bind->next_alloced = ctx->alloced_binds;
	ctx->alloced_binds = new_bind;

	return new_bind;
}

static void
ast_dt_remove_terminal_member(struct ast_dt_context *ctx,
		ast_member_id member)
{
	ast_member_id *node;
	node = &ctx->terminal_nodes;

	while (*node >= 0) {
		if (*node == member) {
			*node = get_member(ctx, member)->next;
			get_member(ctx, member)->next = -1;
			return;
		}
		node = &get_member(ctx, *node)->next;
	}
}

static void
ast_dt_dependency(struct ast_dt_context *ctx,
		ast_member_id from, ast_member_id to)
{
	struct ast_dt_dependency new_edge = {0};

	new_edge.from = from;
	new_edge.to = to;

	struct ast_dt_member *from_mbr;
	from_mbr = get_member(ctx, from);

	dlist_append(
			from_mbr->outgoing_edges,
			from_mbr->num_outgoing_edges,
			&new_edge);

	get_member(ctx, to)->num_incoming_edges += 1;
	ctx->unvisited_edges += 1;

	if (get_member(ctx, to)->num_incoming_edges == 1) {
		ast_dt_remove_terminal_member(ctx, to);
	}
}

static ast_member_id
ast_dt_register_member(struct ast_dt_context *ctx,
		struct atom *name, ast_slot_id slot,
		struct stg_location decl_loc)
{
	struct ast_dt_member new_mbr = {0};

	new_mbr.name = name;
	new_mbr.slot = ast_node_resolve_slot(ctx->ast_env, &slot);
	new_mbr.decl_loc = decl_loc;
	new_mbr.next = ctx->terminal_nodes;
	new_mbr.first_child = -1;
	new_mbr.anscestor_local_member = -1;
	new_mbr.persistant_id = -1;

	ast_member_id old_id = ast_env_slot(ctx->ast_ctx, ctx->ast_env, slot).member_id;
	if (old_id >= 0) {
		return old_id;
	}

	ast_member_id mbr_id;
	mbr_id = dlist_append(
			ctx->members,
			ctx->num_members,
			&new_mbr);

	ctx->ast_env->slots[slot].member_id = mbr_id;

	ctx->terminal_nodes = mbr_id;

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

				if (!mbr->bound) {
					result |= AST_NODE_FLAG_NOT_CONST;
				}

				// TODO: Apply not_const flag if the member is not trivial or
				// dependent only on trivial members.
				result |= AST_NODE_FLAG_NOT_CONST;
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
ast_node_analyze_closure(struct ast_dt_context *ctx, struct ast_closure_target *closure)
{
	enum ast_node_flags result = AST_NODE_FLAG_OK;

	for (size_t i = 0; i < closure->num_members; i++) {
		result |= ast_slot_analyze(ctx, closure->members[i].outer_slot);
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

		case AST_NODE_SLOT:
			result |= ast_slot_analyze(ctx,
					ast_node_resolve_slot(ctx->ast_env, &node->slot));
			break;

		case AST_NODE_LIT:
			result |= ast_slot_analyze(ctx,
					ast_node_resolve_slot(ctx->ast_env, &node->lit.slot));
			break;

		case AST_NODE_LOOKUP:
			if (node->lookup.value == AST_SLOT_NOT_FOUND) {
				result |= AST_NODE_FLAG_ERROR;
			}
			result |= ast_slot_analyze(ctx,
					ast_node_resolve_slot(ctx->ast_env, &node->lookup.value));
			break;

		case AST_NODE_COMPOSITE:
			result |= ast_node_analyze_closure(ctx,
					&node->composite.closure);
			break;

		case AST_NODE_VARIANT:
			break;

		case AST_NODE_FUNC_UNINIT:
			panic("Got uninitialized function in analyze.");
			break;
	}

	return result;
}

static void
ast_dt_node_try_bind_const_member(struct ast_dt_context *ctx,
		struct ast_module *mod, struct ast_node *node)
{
	assert(node->kind == AST_NODE_COMPOSITE);
	for (size_t i = 0; i < node->composite.num_members; i++) {
		struct ast_datatype_member *mbr_def;
		mbr_def = &node->composite.members[i];

		ast_slot_id slot_id;
		slot_id = ast_unpack_arg_named(ctx->ast_ctx, ctx->ast_env,
				node->composite.cons, AST_BIND_NEW, mbr_def->name);

		struct ast_env_slot slot;
		slot = ast_env_slot(ctx->ast_ctx, ctx->ast_env, slot_id);

		if (slot.kind == AST_SLOT_MEMBER) {
			ast_member_id mbr_id;
			mbr_id = slot.member_id;
			assert(mbr_id >= 0);

			struct ast_dt_member *mbr;
			mbr = get_member(ctx, mbr_id);

			if (mbr->bound && !mbr->bound->overridable) {
				enum ast_node_flags flags;
				flags = ast_node_analyze(ctx, mbr->bound->value);

				if (flags == AST_NODE_FLAG_OK) {
					int err;
					struct object obj;
					err = ast_node_eval(ctx->ast_ctx, mod,
							ctx->ast_env, mbr->bound->value, &obj);
					if (err) {
						printf("Failed to evaluate value.\n");
						continue;
					}

					// TODO: Hack. There should probably be a better way of
					// overriding a member value.
					assert(ctx->ast_env->slots[slot_id].kind == AST_SLOT_MEMBER);
					ctx->ast_env->slots[slot_id].kind = AST_SLOT_WILDCARD;

					ast_bind_slot_const(ctx->ast_ctx, ctx->ast_env,
							slot_id, NULL, obj);
				}
			}
		}
	}
}

static void
ast_dt_composite_populate_type(struct ast_dt_context *ctx, struct ast_module *mod,
		ast_member_id anscestor, ast_member_id member)
{
	struct ast_dt_member *mbr = get_member(ctx, member);
	assert(mbr->type != TYPE_UNSET);

	struct type *mbr_type = vm_get_type(ctx->ast_ctx->vm, mbr->type);

	if (mbr_type->obj_def) {
		ast_slot_id cons_slot;
		cons_slot = ast_bind_slot_cons(ctx->ast_ctx, ctx->ast_env,
				mbr->slot, NULL, mbr_type->obj_def);

		for (size_t i = 0; i < mbr_type->obj_def->num_params; i++) {
			struct ast_object_def_param *param;
			param = &mbr_type->obj_def->params[i];

			ast_slot_id mbr_slot =
				ast_unpack_arg_named(ctx->ast_ctx, ctx->ast_env,
					cons_slot, AST_BIND_NEW, param->name);

			type_id tid;
			struct object tid_obj;
			int err;
			err = ast_slot_pack(ctx->ast_ctx, mod, ctx->ast_env,
					ast_env_slot(ctx->ast_ctx, ctx->ast_env, mbr_slot).type, &tid_obj);
			if (err) {
				printf("Failed to evaluate type of member.\n");
				return;
			}

			assert_type_equals(ctx->ast_ctx->vm, tid_obj.type, ctx->ast_ctx->types.type);
			tid = *(type_id *)tid_obj.data;

			ast_member_id mbr_id;
			mbr_id = ast_dt_register_member(ctx, param->name,
					mbr_slot, STG_NO_LOC);

			struct ast_dt_member *new_mbr;
			new_mbr = get_member(ctx, mbr_id);
			new_mbr->is_local_member = false;
			new_mbr->anscestor_local_member = anscestor;

			struct ast_dt_member *anscestor_mbr;
			anscestor_mbr = get_member(ctx, anscestor);
			if (anscestor_mbr->first_child == -1) {
				anscestor_mbr->first_child = mbr_id;
			} else {
				assert(anscestor_mbr->first_child < mbr_id);
			}

			ast_dt_composite_populate_type(ctx, mod, anscestor, mbr_id);
		}
	} else {
		ast_bind_slot_const_type(ctx->ast_ctx, ctx->ast_env,
				ast_env_slot(ctx->ast_ctx, ctx->ast_env, mbr->slot).type,
				NULL, mbr->type);
	}
}

static ast_member_id
ast_dt_find_member_by_slot(struct ast_dt_context *ctx, ast_slot_id target_slot)
{
	return ast_env_slot(ctx->ast_ctx, ctx->ast_env, target_slot).member_id;
}

struct ast_dt_find_member_ref_res {
	ast_member_id *nodes;
	size_t num_nodes;
};

static int
ast_dt_find_member_refs_for_slot(struct ast_dt_context *ctx,
		ast_slot_id slot, struct ast_dt_find_member_ref_res *res)
{
	struct ast_env_slot slt;
	slt = ast_env_slot(ctx->ast_ctx, ctx->ast_env, slot);
	if (slt.kind == AST_SLOT_MEMBER) {
		if (slt.member_id >= 0) {
			dlist_append(
					res->nodes,
					res->num_nodes,
					&slt.member_id);
			return 0;
		} else {
			return 1;
		}
	}

	return 0;
}

static int
ast_dt_find_member_refs_closure(struct ast_dt_context *ctx,
		struct ast_closure_target *closure,
		struct ast_dt_find_member_ref_res *res)
{
	int missing = 0;

	for (size_t i = 0; i < closure->num_members; i++) {
		missing +=
			ast_dt_find_member_refs_for_slot(
					ctx, closure->members[i].outer_slot, res);
	}

	return missing;
}

static int
ast_dt_find_member_refs(struct ast_dt_context *ctx,
		struct ast_node *node, struct ast_dt_find_member_ref_res *res)
{
	int missing = 0;

	assert(res);

	switch (node->kind) {
		case AST_NODE_FUNC_NATIVE:
		case AST_NODE_FUNC_UNINIT:
		case AST_NODE_TEMPL:
		case AST_NODE_LIT:
		case AST_NODE_VARIANT:
			break;

		case AST_NODE_CONS:
		case AST_NODE_CALL:
			missing += ast_dt_find_member_refs(ctx, node->call.func, res);
			for (size_t i = 0; i < node->call.num_args; i++) {
				missing += ast_dt_find_member_refs(ctx, node->call.args[i].value, res);
			}
			break;

		case AST_NODE_SLOT:
			ast_dt_find_member_refs_for_slot(ctx, node->slot, res);
			break;

		case AST_NODE_LOOKUP:
			if (node->lookup.value != AST_SLOT_NOT_FOUND) {
				ast_dt_find_member_refs_for_slot(ctx, node->lookup.value, res);
			}
			break;

		case AST_NODE_FUNC:
			missing +=
				ast_dt_find_member_refs_closure(
						ctx, &node->func.closure, res);
			break;

		case AST_NODE_COMPOSITE:
			missing +=
				ast_dt_find_member_refs_closure(
						ctx, &node->composite.closure, res);
			break;
	}

	return missing;
}

static void
ast_dt_bind(struct ast_dt_context *ctx, struct ast_node *target, struct ast_node *value,
		bool overridable)
{
	ast_slot_id target_slot;
	target_slot = ast_node_value(ctx->ast_ctx, ctx->ast_env, target);

	// TODO: Handle composite targets/value unpacking.

	struct ast_dt_find_member_ref_res value_members = {0};
	ast_dt_find_member_refs(ctx, value, &value_members);

	struct ast_dt_find_member_ref_res target_members = {0};
	ast_dt_find_member_refs(ctx, target, &target_members);

	struct ast_dt_bind *bind;
	bind = ast_dt_register_bind(
			ctx, target, value, overridable);

	for (size_t target_i = 0; target_i < target_members.num_nodes; target_i++) {
		ast_member_id member_id;
		member_id = target_members.nodes[target_i];

		struct ast_dt_member *member;
		member = get_member(ctx, member_id);

		if (member->bound) {
			if (!member->bound->overridable && !overridable) {
				stg_error(ctx->ast_ctx->err, target->loc,
						"'%.*s' is bound multiple times.", ALIT(member->name));
				stg_appendage(ctx->ast_ctx->err, member->bound->target->loc, "Also bound here.");
				ctx->num_errors += 1;
				return;
			}

			if (member->bound->overridable && overridable) {
				stg_error(ctx->ast_ctx->err, target->loc,
						"'%.*s' has multiple default binds.", ALIT(member->name));
				stg_appendage(ctx->ast_ctx->err, member->bound->target->loc, "Also bound here.");
				ctx->num_errors += 1;
				return;
			}

			if (overridable) {
				return;
			}
		}

		member->bound = bind;

		for (size_t val_i = 0; val_i < value_members.num_nodes; val_i++) {
			ast_dt_dependency(ctx, value_members.nodes[val_i], member_id);
		}
	}

	bind->deps = value_members.nodes;
	bind->num_deps = value_members.num_nodes;
	free(target_members.nodes);
}

static bool
ast_dt_output_cycle_errors(struct ast_dt_context *ctx,
		ast_member_id origin, ast_member_id node)
{
	if (node == origin) {
		return true;
	}

	bool found = false;
	for (size_t i = 0; i < get_member(ctx, node)->num_outgoing_edges; i++) {
		struct ast_dt_dependency *edge;
		edge = &get_member(ctx, node)->outgoing_edges[i];
		if (!edge->visited) {
			if (ast_dt_output_cycle_errors(ctx, origin, edge->to)) {
				edge->visited = true;
				found = true;

				stg_appendage(ctx->ast_ctx->err,
						get_member(ctx, edge->from)->bound->target->loc,
						"Through");
			}
		}
	}

	return found;
}

static ast_member_id
ast_dt_composite_order_binds(struct ast_dt_context *ctx)
{
	ast_member_id result = -1;
	ast_member_id result_tail = -1;

	// Topological Sort.
	while (ctx->terminal_nodes >= 0) {
		ast_member_id member_id;
		struct ast_dt_member *member;
		member_id = ctx->terminal_nodes;
		member = get_member(ctx, member_id);

		ctx->terminal_nodes = member->next;
		member->next = -1;

		if (result < 0) {
			result = member_id;
			result_tail = member_id;
		} else {
			get_member(ctx, result_tail)->next = member_id;
			result_tail = member_id;
		}

		for (size_t i = 0; i < member->num_outgoing_edges; i++) {
			struct ast_dt_dependency *edge;
			edge = &member->outgoing_edges[i];

			if (!edge->visited) {
				edge->visited = true;

				struct ast_dt_member *edge_to;
				edge_to = get_member(ctx, edge->to);

				assert(edge_to->num_incoming_edges > 0);
				edge_to->num_incoming_edges -= 1;

				assert(ctx->unvisited_edges > 0);
				ctx->unvisited_edges -= 1;

				if (edge_to->num_incoming_edges == 0) {
					edge_to->next = ctx->terminal_nodes;
					ctx->terminal_nodes = edge->to;
				}
			}
		}
	}

	if (ctx->unvisited_edges != 0) {
		// We found one or more cycles. Report them to the user.

		for (size_t member_i = 0; member_i < ctx->num_members; member_i++) {
			struct ast_dt_member *member;
			member = get_member(ctx, member_i);

			for (size_t edge_i = 0; edge_i < member->num_outgoing_edges; edge_i++) {
				struct ast_dt_dependency *edge;
				edge = &member->outgoing_edges[edge_i];
				if (!edge->visited) {
					edge->visited = true;
					stg_error(ctx->ast_ctx->err, member->bound->target->loc,
							"'%.*s' can not be dependent on itself.",
							ALIT(member->name));
					ast_dt_output_cycle_errors(ctx, member_i, edge->to);
					ctx->num_errors += 1;
				}
			}
		}
		return -1;
	}

	return result;
}

static void
ast_dt_composite_populate(struct ast_dt_context *ctx, struct ast_node *node)
{
	assert(node->kind == AST_NODE_COMPOSITE);

	for (size_t i = 0; i < node->composite.num_members; i++) {
		ast_slot_id slot;
		struct ast_datatype_member *mbr;
		mbr = &node->composite.members[i];

		slot = ast_unpack_arg_named(
				ctx->ast_ctx, ctx->ast_env,
				node->composite.cons, AST_BIND_NEW,
				mbr->name);

		// TODO: Better location.
		ast_member_id mbr_id;
		mbr_id = ast_dt_register_member(ctx, mbr->name, slot,
				node->composite.members[i].type->loc);

		struct ast_dt_member *new_mbr;
		new_mbr = get_member(ctx, mbr_id);

		new_mbr->is_local_member = true;
		new_mbr->first_child = -1;
	}

	for (size_t i = 0; i < node->composite.num_binds; i++) {
		ast_dt_bind(ctx,
				node->composite.binds[i].target,
				node->composite.binds[i].value,
				node->composite.binds[i].overridable);
	}
}

static int
ast_dt_composite_resolve_types(struct ast_dt_context *ctx,
		struct ast_module *mod, struct ast_node *node)
{
	bool made_progress = true;
	bool error = false;
	bool wait = true;

	while (made_progress && wait) {
		made_progress = false;
		wait = false;

		ast_dt_node_try_bind_const_member(ctx, mod, node);

		for (size_t i = 0; i < node->composite.num_members; i++) {

			ast_slot_id slot;
			struct ast_datatype_member *mbr_def;
			mbr_def = &node->composite.members[i];

			slot = ast_unpack_arg_named(
					ctx->ast_ctx, ctx->ast_env,
					node->composite.cons, AST_BIND_NEW,
					mbr_def->name);

			ast_member_id mbr_id = ast_env_slot(
						ctx->ast_ctx, ctx->ast_env, slot).member_id;
			assert(mbr_id >= 0);
			struct ast_dt_member *mbr;
			mbr = get_member(ctx, mbr_id);

			if (mbr->type == TYPE_UNSET) {
				// The member has not been registered yet.
				enum ast_node_flags flags;
				flags = ast_node_analyze(ctx, mbr_def->type);

				if ((flags & AST_NODE_FLAG_ERROR) != 0) {
					error = true;
					continue;
				} else if (flags != AST_NODE_FLAG_OK) {
					wait = true;
					continue;
				}

				type_id type;

				int err;
				err = ast_node_eval_type(ctx->ast_ctx,
						mod, ctx->ast_env, mbr_def->type, &type);
				if (err) {
					printf("Failed to evaluate type.\n");
					error = true;
					continue;
				}

				mbr->type = type;

				ast_dt_composite_populate_type(ctx, mod, mbr_id, mbr_id);

				made_progress = true;
			}
		}
	}

	if (wait) {
		// We could not resolve the type of one or more members, so go through
		// and figure out which ones failed and give an error message.
		printf("Could not evaluate type of ");
		for (size_t i = 0; i < node->composite.num_members; i++) {
			ast_slot_id slot;
			struct ast_datatype_member *mbr_def;
			mbr_def = &node->composite.members[i];

			slot = ast_unpack_arg_named(
					ctx->ast_ctx, ctx->ast_env,
					node->composite.cons, AST_BIND_NEW,
					mbr_def->name);

			struct ast_dt_member *mbr = get_member(ctx,
					ast_env_slot(ctx->ast_ctx, ctx->ast_env, slot).member_id);

			if (mbr->type == TYPE_UNSET) {
				if (i > 0) {
					printf(", ");
				}
				printf("%.*s", ALIT(mbr_def->name));
			}
		}

		printf(".\n");
	}

	return error || wait;
}

static ast_member_id
ast_dt_num_descendant_members(struct ast_dt_context *ctx,
		struct ast_module *mod, type_id type)
{
	struct type *member_type;
	member_type = vm_get_type(ctx->ast_ctx->vm, type);

	ast_member_id count = 0;

	if (member_type->obj_def) {
		count += member_type->obj_def->num_params;

		for (size_t i = 0; i < member_type->obj_def->num_params; i++) {
			int err;
			type_id mbr_type;

			ast_slot_id type_slot;
			type_slot = ast_env_slot(ctx->ast_ctx, ctx->ast_env,
					member_type->obj_def->params[i].slot).type;

			err = ast_slot_pack_type(ctx->ast_ctx, mod,
					ctx->ast_env, type_slot, &mbr_type);
			if (err) {
				printf("Failed to pack cons param type.\n");
				continue;
			}

			count += ast_dt_num_descendant_members(ctx, mod, mbr_type);
		}
	}

	return count;
}

static ast_member_id
ast_dt_calculate_persistant_id(struct ast_dt_context *ctx, ast_member_id mbr_id)
{
	struct ast_dt_member *mbr = get_member(ctx, mbr_id);

	if (mbr->is_local_member) {
		assert(mbr->persistant_id >= 0);
		return mbr->persistant_id;
	} else {
		struct ast_dt_member *local_anscestor;
		local_anscestor = get_member(ctx, mbr->anscestor_local_member);
		assert(local_anscestor->is_local_member &&
				local_anscestor->first_child >= 0 &&
				local_anscestor->first_child < mbr_id);
		return mbr_id - local_anscestor->first_child;
	}
}

static void
ast_dt_env_recalculate_persistant_member_ids(struct ast_dt_context *ctx, struct ast_env *env)
{
	for (size_t i = 0; i < env->num_slots; i++) {
		if (env->slots[i].member_id >= 0) {
			ast_member_id old_id, new_id;
			old_id = env->slots[i].member_id;
			new_id = ast_dt_calculate_persistant_id(ctx, old_id);

			env->slots[i].member_id = new_id;
		}
	}
}

struct ast_dt_local_member {
	struct atom *name;
	type_id type;
	size_t location;
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

		arena_string_append_sprintf(mem, &res, "%.*s = ", ALIT(info->members[i].name));

		arena_string_append_obj_repr(&res, vm, mem, &mbr);
		arena_string_append(mem, &res, STR(";"));
	}

	arena_string_append(mem, &res, STR("}"));

	return res;
}

struct type_base ast_dt_composite_base = {
	.name     = STR("Struct"),
	.repr     = ast_dt_composite_repr,
	.obj_repr = ast_dt_composite_obj_repr,

	// TODO: Implement
	// .free = ...,
};

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
	dt_ctx.terminal_nodes = -1;

	ast_dt_composite_populate(&dt_ctx, comp);

	int err;
	err = ast_dt_composite_resolve_types(&dt_ctx, mod, comp);

	for (size_t i = 0; i < dt_ctx.num_members; i++) {
		printf("%.*s (%i): ", ALIT(dt_ctx.members[i].name), dt_ctx.members[i].slot);
		print_type_repr(ctx->vm, vm_get_type(ctx->vm, dt_ctx.members[i].type));
		printf("\n");
	}

	if (err) {
		printf("Failed to popluate members.\n");
		return TYPE_UNSET;
	}

	ast_member_id bind_order;
	bind_order = ast_dt_composite_order_binds(&dt_ctx);

	type_id result = TYPE_UNSET;

	if (dt_ctx.num_errors == 0) {
		assert(bind_order);

		struct ast_object_def *def;
		def = ast_object_def_register(mod->env.store);

		struct ast_dt_local_member *local_members;
		local_members = calloc(comp->composite.num_members,
				sizeof(struct ast_dt_local_member));

		struct ast_object_def_param *params;
		params = calloc(comp->composite.num_members,
				sizeof(struct ast_object_def_param));

		size_t num_bound_members = 0;
		for (ast_member_id mbr = bind_order;
				mbr >= 0; mbr = dt_ctx.members[mbr].next) {
			num_bound_members += 1;
		}

		struct ast_object_bind *binds;
		binds = calloc(num_bound_members,
				sizeof(struct ast_object_bind));

		size_t offset = 0;
		ast_member_id cumulative_persistant_id = 0;
		for (size_t i = 0; i < comp->composite.num_members; i++) {
			printf("finalizing %.*s  (type flags=%i):\n",
					ALIT(comp->composite.members[i].name),
					ast_node_analyze(&dt_ctx, comp->composite.members[i].type));
			ast_print(ctx, env, comp->composite.members[i].type);

			params[i].param_id = i;
			params[i].name = comp->composite.members[i].name;
			local_members[i].name = comp->composite.members[i].name;
			local_members[i].location = offset;

			ast_slot_id mbr_slot;
			mbr_slot = ast_unpack_arg_named(ctx, env, comp->composite.cons,
					AST_BIND_NEW, comp->composite.members[i].name);

			ast_member_id mbr_id;
			mbr_id = ast_dt_find_member_by_slot(&dt_ctx, mbr_slot);

			struct ast_dt_member *mbr;
			mbr = get_member(&dt_ctx, mbr_id);

			params[i].slot =
				ast_bind_slot_member(ctx, &def->env, AST_BIND_NEW, NULL,
						ast_bind_slot_const_type(ctx, &def->env, AST_BIND_NEW,
							NULL, mbr->type));

			// NOTE: We keep the old member_id here so that we can replace all
			// slot member ids after the binds have been added.
			def->env.slots[params[i].slot].member_id = mbr_id;

			cumulative_persistant_id += 1 +
				ast_dt_num_descendant_members(&dt_ctx, mod, mbr->type);
			mbr->persistant_id = cumulative_persistant_id;

			struct type *member_type;
			member_type = vm_get_type(ctx->vm, mbr->type);

			local_members[i].type = mbr->type;

			offset += member_type->size;
		}

		// NOTE: ast_dt_calculate_persistant_id can only be used after this
		// point because it is dependent on persistant_id being set for local
		// members.

		def->params = params;
		def->num_params = comp->composite.num_members;

		size_t bind_i = 0;
		for (ast_member_id mbr_i = bind_order;
				mbr_i >= 0; mbr_i = get_member(&dt_ctx, mbr_i)->next) {
			struct ast_dt_member *mbr;
			mbr = get_member(&dt_ctx, mbr_i);

			if (!mbr->bound) {
				continue;
			}

			struct func func = {0};

			type_id value_type;

			int err;
			err = ast_node_eval_type_of(ctx, mod, env,
					mbr->bound->value, &value_type);
			if (err) {
				printf("Failed to eval bind value  type.\n");
				continue;
			}

			size_t num_deps = mbr->bound->num_deps;
			struct atom *dep_names[num_deps];
			type_id dep_types[num_deps];

			for (size_t i = 0; i < num_deps; i++) {
				ast_member_id dep_i = mbr->bound->deps[i];
				struct ast_dt_member *dep = get_member(&dt_ctx, dep_i);
				dep_names[i] = dep->name;

				struct object dep_type_obj;
				err = ast_slot_pack(ctx, mod, env,
						ast_env_slot(ctx, env, dep->slot).type, &dep_type_obj);
				if (err) {
					printf("Failed to pack member type.\n");
					continue;
				}

				assert_type_equals(ctx->vm, dep_type_obj.type, ctx->types.type);

				dep_types[i] = *(type_id *)dep_type_obj.data;
			}

			func.type = stg_register_func_type(mod->stg_mod,
					value_type, dep_types, num_deps);

			func.kind = FUNC_BYTECODE;
			func.bytecode = ast_composite_bind_gen_bytecode(ctx, mod, env,
					dep_names, dep_types, num_deps, mbr->bound->value);

			binds[bind_i].target = ast_copy_slot_with_member_id(ctx,
					&def->env, AST_BIND_NEW,
					env,       ast_node_value(ctx, env, mbr->bound->target));

			binds[bind_i].num_value_params = mbr->bound->num_deps;
			binds[bind_i].value_params = calloc(mbr->bound->num_deps,
					sizeof(ast_slot_id));

			for (size_t dep_i = 0; dep_i < mbr->bound->num_deps; dep_i++) {
				struct ast_dt_member *dep = get_member(&dt_ctx, mbr->bound->deps[dep_i]);
				binds[bind_i].value_params[dep_i] = dep->slot;
			}

			binds[bind_i].value =
				stg_register_func(mod->stg_mod, func);

			bind_i += 1;
		}

		def->binds = binds;
		def->num_binds = num_bound_members;

		ast_dt_env_recalculate_persistant_member_ids(&dt_ctx, &def->env);

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

		result = stg_register_type(mod->stg_mod, dt_type);

		def->ret_type = result;
	}

	for (size_t i = 0; i < dt_ctx.num_members; i++) {
		free(dt_ctx.members[i].outgoing_edges);
	}
	free(dt_ctx.members);

	for (struct ast_dt_bind *bind = dt_ctx.alloced_binds;
			bind != NULL;) {
		struct ast_dt_bind *this_bind = bind;
		bind = bind->next_alloced;
		free(this_bind);
	}

	comp->composite.type = result;
	return result;
}
