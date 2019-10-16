#include "ast.h"
#include "dlist.h"
#include <stdlib.h>
#include <string.h>

struct ast_dt_bind {
	struct ast_node *target;
	struct ast_node *value;
	bool overridable;

	struct ast_dt_bind *next_alloced;
};

struct ast_dt_dependency;

struct ast_dt_member {
	struct atom *name;
	ast_slot_id slot;
	struct stg_location decl_loc;
	struct ast_dt_bind *bound;

	struct ast_dt_dependency *outgoing_edges;
	size_t num_outgoing_edges;

	size_t num_incoming_edges;

	// Used as part of the terminal_nodes linked list until after the member
	// has been sorted. Then next becomes the id of the next member in the
	// topological order. The index of another member.
	struct ast_dt_member *next;
};

struct ast_dt_dependency {
	struct ast_dt_member *from, *to;
	bool visited;
};

struct ast_dt_context {
	struct ast_dt_member *members;
	size_t num_members;
	size_t cap_members;

	struct ast_dt_bind *alloced_binds;

	// A linked list of all nodes with no incoming edges. The index of another
	// member.
	struct ast_dt_member *terminal_nodes;
	size_t unvisited_edges;

	struct ast_context *ast_ctx;
	struct ast_env *ast_env;

	size_t num_errors;
};

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
		struct ast_dt_member *member)
{
	struct ast_dt_member **node;
	node = &ctx->terminal_nodes;

	while (*node) {
		if (*node == member) {
			*node = member->next;
			member->next = NULL;
			return;
		}
		node = &(*node)->next;
	}
}

static void
ast_dt_dependency(struct ast_dt_context *ctx,
		struct ast_dt_member *from, struct ast_dt_member *to)
{
	struct ast_dt_dependency new_edge = {0};

	new_edge.from = from;
	new_edge.to = to;

	dlist_append(
			from->outgoing_edges,
			from->num_outgoing_edges,
			&new_edge);

	to->num_incoming_edges += 1;
	ctx->unvisited_edges += 1;

	if (to->num_incoming_edges == 1) {
		ast_dt_remove_terminal_member(ctx, to);
	}
}

static struct ast_dt_member *
ast_dt_register_member(struct ast_dt_context *ctx,
		struct atom *name, ast_slot_id slot,
		struct stg_location decl_loc)
{
	assert(ctx->num_members < ctx->cap_members);

	size_t member_i = ctx->num_members;
	ctx->num_members += 1;

	struct ast_dt_member *member;
	member = &ctx->members[member_i];


	memset(member, 0, sizeof(struct ast_dt_member));

	member->name = name;
	member->slot = slot;
	member->decl_loc = decl_loc;

	member->next = ctx->terminal_nodes;
	ctx->terminal_nodes = member;

	return member;
}

static size_t
ast_dt_type_node_num_bindable_members(struct ast_node *node)
{
	// TODO: Implement.
	return 0;
}

static size_t
ast_dt_composite_num_bindable_members(struct ast_node *node)
{
	assert(node->kind == AST_NODE_COMPOSITE);

	size_t count = 0;
	for (size_t i = 0; i < node->composite.num_members; i++) {
		count += ast_dt_type_node_num_bindable_members(
				node->composite.members[i].type);
	}

	return count + node->composite.num_members;
}

static void
ast_dt_type_node_populate(struct ast_dt_context *ctx,
		struct ast_node *node)
{
	// TODO: Implement
	return;
}

static void
ast_dt_composite_populate(struct ast_dt_context *ctx, struct ast_node *node)
{
	assert(node->kind == AST_NODE_COMPOSITE);

	for (size_t i = 0; i < node->composite.num_members; i++) {
		ast_dt_register_member(ctx, node->composite.members[i].name,
				ast_unpack_arg_named(
					ctx->ast_ctx, ctx->ast_env,
					node->composite.cons, AST_BIND_NEW,
					node->composite.members[i].name),
				node->loc);
		ast_dt_type_node_populate(ctx, node->composite.members[i].type);
	}
}

static struct ast_dt_member *
ast_dt_find_member_by_slot(struct ast_dt_context *ctx, ast_slot_id target_slot)
{
	for (size_t i = 0; i < ctx->num_members; i++) {
		// TODO: We should base target resolution on something more consistent
		// than slots.
		if (ast_node_resolve_slot(ctx->ast_env, &target_slot) ==
				ast_node_resolve_slot(ctx->ast_env, &ctx->members[i].slot)) {
			return &ctx->members[i];
		}
	}

	return NULL;
}

struct ast_dt_find_member_ref_res {
	struct ast_dt_member **nodes;
	size_t num_nodes;
};

static void
ast_dt_find_member_refs(struct ast_dt_context *ctx,
		struct ast_node *node, struct ast_dt_find_member_ref_res *res)
{
	assert(res);

	switch (node->kind) {
		case AST_NODE_FUNC:
		case AST_NODE_FUNC_NATIVE:
		case AST_NODE_FUNC_UNINIT:
		case AST_NODE_TEMPL:
		case AST_NODE_COMPOSITE:
		case AST_NODE_VARIANT:
			break;

		case AST_NODE_CONS:
		case AST_NODE_CALL:
			ast_dt_find_member_refs(ctx, node->call.func, res);
			for (size_t i = 0; i < node->call.num_args; i++) {
				ast_dt_find_member_refs(ctx, node->call.args[i].value, res);
			}
			break;

		case AST_NODE_SLOT:
			{
				struct ast_dt_member *member;
				member = ast_dt_find_member_by_slot(
						ctx, node->slot);
				if (member) {
					dlist_append(
							res->nodes, res->num_nodes, &member);
				}
			}
			break;

		case AST_NODE_LOOKUP:
			if (node->lookup.value != AST_SLOT_NOT_FOUND) {
				struct ast_dt_member *member;
				member = ast_dt_find_member_by_slot(
						ctx, node->lookup.value);
				if (member) {
					dlist_append(
							res->nodes, res->num_nodes, &member);
				}
			}
			break;
	}
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
		struct ast_dt_member *member;
		member = target_members.nodes[target_i];

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
			ast_dt_dependency(ctx, value_members.nodes[val_i], member);
		}
	}

	free(target_members.nodes);
}

static void
ast_dt_node_bind(struct ast_dt_context *ctx, struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_FUNC_NATIVE:
		case AST_NODE_FUNC_UNINIT:
		case AST_NODE_TEMPL:
		case AST_NODE_COMPOSITE:
		case AST_NODE_VARIANT:
		case AST_NODE_CALL:
		case AST_NODE_SLOT:
		case AST_NODE_LOOKUP:
			break;

		case AST_NODE_FUNC:
			// TODO: Implement unpacking functions that return partially
			// initialized objects.
			break;

		case AST_NODE_CONS:
			// TODO: Add cons binds.
			// for (size_t i = 0; i < node->call.num_args; i++) {
			// }
			break;
	}
}

static void
ast_dt_composite_bind(struct ast_dt_context *ctx, struct ast_node *node)
{
	for (size_t i = 0; i < node->composite.num_binds; i++) {
		ast_dt_bind(ctx,
				node->composite.binds[i].target,
				node->composite.binds[i].value,
				node->composite.binds[i].overridable);

		ast_dt_node_bind(ctx, node->composite.binds[i].value);
	}
}

static bool
ast_dt_output_cycle_errors(struct ast_dt_context *ctx,
		struct ast_dt_member *origin, struct ast_dt_member *node)
{
	if (node == origin) {
		return true;
	}

	bool found = false;
	for (size_t i = 0; i < node->num_outgoing_edges; i++) {
		struct ast_dt_dependency *edge;
		edge = &node->outgoing_edges[i];
		if (!edge->visited) {
			if (ast_dt_output_cycle_errors(ctx, origin, edge->to)) {
				edge->visited = true;
				found = true;

				stg_appendage(ctx->ast_ctx->err, edge->from->bound->target->loc,
						"Through");
			}
		}
	}

	return found;
}

static struct ast_dt_member *
ast_dt_composite_order_binds(struct ast_dt_context *ctx)
{
	struct ast_dt_member *result = NULL;
	struct ast_dt_member *result_tail = NULL;

	// Topological Sort.
	while (ctx->terminal_nodes) {
		struct ast_dt_member *member;
		member = ctx->terminal_nodes;

		ctx->terminal_nodes = member->next;
		member->next = NULL;

		if (!result) {
			result = member;
			result_tail = member;
		} else {
			result_tail->next = member;
			result_tail = member;
		}

		for (size_t i = 0; i < member->num_outgoing_edges; i++) {
			struct ast_dt_dependency *edge;
			edge = &member->outgoing_edges[i];

			if (!edge->visited) {
				edge->visited = true;

				assert(edge->to->num_incoming_edges > 0);
				edge->to->num_incoming_edges -= 1;

				assert(ctx->unvisited_edges > 0);
				ctx->unvisited_edges -= 1;

				if (edge->to->num_incoming_edges == 0) {
					edge->to->next = ctx->terminal_nodes;
					ctx->terminal_nodes = edge->to;
				}
			}
		}
	}

	if (ctx->unvisited_edges != 0) {
		for (size_t member_i = 0; member_i < ctx->num_members; member_i++) {
			struct ast_dt_member *member;
			member = &ctx->members[member_i];

			for (size_t edge_i = 0; edge_i < member->num_outgoing_edges; edge_i++) {
				struct ast_dt_dependency *edge;
				edge = &member->outgoing_edges[edge_i];
				if (!edge->visited) {
					edge->visited = true;
					stg_error(ctx->ast_ctx->err, member->bound->target->loc,
							"'%.*s' can not be dependent on itself.",
							ALIT(member->name));
					ast_dt_output_cycle_errors(ctx, member, edge->to);
					ctx->num_errors += 1;
				}
			}
		}
		return NULL;
	}

	return result;
}

bool
ast_dt_is_valid(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *comp)
{
	size_t num_members;
	num_members = ast_dt_composite_num_bindable_members(comp);
	struct ast_dt_member members[num_members];

	struct ast_dt_context dt_ctx = {0};
	dt_ctx.ast_ctx = ctx;
	dt_ctx.ast_env = env;
	dt_ctx.members = members;
	dt_ctx.cap_members = num_members;

	ast_dt_composite_populate(&dt_ctx, comp);
	ast_dt_composite_bind(&dt_ctx, comp);

	struct ast_dt_member *bind_order;
	bind_order = ast_dt_composite_order_binds(&dt_ctx);

	for (size_t i = 0; i < num_members; i++) {
		free(members[i].outgoing_edges);
	}

	for (struct ast_dt_bind *bind = dt_ctx.alloced_binds;
			bind != NULL;) {
		struct ast_dt_bind *this_bind = bind;
		bind = bind->next_alloced;
		free(this_bind);
	}

	return dt_ctx.num_errors == 0;
}

void
ast_dt_finalize_composite(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *comp)
{
	size_t num_members;
	num_members = ast_dt_composite_num_bindable_members(comp);
	struct ast_dt_member members[num_members];

	struct ast_dt_context dt_ctx = {0};
	dt_ctx.ast_ctx = ctx;
	dt_ctx.ast_env = env;
	dt_ctx.members = members;
	dt_ctx.cap_members = num_members;

	ast_dt_composite_populate(&dt_ctx, comp);
	ast_dt_composite_bind(&dt_ctx, comp);

	struct ast_dt_member *bind_order;
	bind_order = ast_dt_composite_order_binds(&dt_ctx);

	for (struct ast_dt_member *mbr = bind_order;
			mbr != NULL; mbr = mbr->next) {
		printf("member '%.*s'\n", ALIT(mbr->name));
		if (mbr->bound) {
			printf("bind\n");
			ast_print(ctx, env, mbr->bound->target);
			printf("to\n");
			ast_print(ctx, env, mbr->bound->value);
		} else {
			printf("not bound\n");
		}
		printf("\n");
	}

	for (size_t i = 0; i < num_members; i++) {
		free(members[i].outgoing_edges);
	}

	for (struct ast_dt_bind *bind = dt_ctx.alloced_binds;
			bind != NULL;) {
		struct ast_dt_bind *this_bind = bind;
		bind = bind->next_alloced;
		free(this_bind);
	}
}
