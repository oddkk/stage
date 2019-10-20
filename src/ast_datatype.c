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

	struct stg_location decl_loc;
	struct ast_dt_bind *bound;

	struct ast_dt_dependency *outgoing_edges;
	size_t num_outgoing_edges;

	size_t num_incoming_edges;

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
	new_mbr.slot = slot;
	new_mbr.decl_loc = decl_loc;
	new_mbr.next = ctx->terminal_nodes;

	ast_member_id mbr_id;
	mbr_id = dlist_append(
			ctx->members,
			ctx->num_members,
			&new_mbr);

	ctx->terminal_nodes = mbr_id;

	return mbr_id;
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

static ast_member_id
ast_dt_find_member_by_slot(struct ast_dt_context *ctx, ast_slot_id target_slot)
{
	for (size_t i = 0; i < ctx->num_members; i++) {
		// TODO: We should base target resolution on something more consistent
		// than slots.
		if (ast_node_resolve_slot(ctx->ast_env, &target_slot) ==
				ast_node_resolve_slot(ctx->ast_env, &get_member(ctx, i)->slot)) {
			return i;
		}
	}

	return -1;
}

struct ast_dt_find_member_ref_res {
	ast_member_id *nodes;
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
				ast_member_id member;
				member = ast_dt_find_member_by_slot(
						ctx, node->slot);
				if (member >= 0) {
					dlist_append(
							res->nodes,
							res->num_nodes,
							&member);
				}
			}
			break;

		case AST_NODE_LOOKUP:
			if (node->lookup.value != AST_SLOT_NOT_FOUND) {
				ast_member_id member;
				member = ast_dt_find_member_by_slot(
						ctx, node->lookup.value);
				if (member >= 0) {
					dlist_append(
							res->nodes,
							res->num_nodes,
							&member);
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

struct type_base ast_dt_composite_base = {
	.name = STR("Struct"),
	.repr = ast_dt_composite_repr,

	// TODO: Implement
	// .obj_repr = ...,
	// .free = ...,
};

int
ast_dt_try_resolve_types(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *node)
{
	/*
	switch (node->kind) {
		case AST_NODE_COMPOSITE:
			break;
	}
	*/

	return -1;
}

type_id
ast_dt_finalize_composite(struct ast_context *ctx, struct ast_module *mod,
		struct ast_env *env, struct ast_node *comp)
{
	struct ast_dt_context dt_ctx = {0};
	dt_ctx.ast_ctx = ctx;
	dt_ctx.ast_env = env;
	dt_ctx.terminal_nodes = -1;

	ast_dt_composite_populate(&dt_ctx, comp);
	ast_dt_composite_bind(&dt_ctx, comp);

	ast_member_id bind_order;
	bind_order = ast_dt_composite_order_binds(&dt_ctx);

	type_id result = TYPE_UNSET;

	if (dt_ctx.num_errors == 0) {
		assert(bind_order);

		struct ast_object_def *def;
		def = ast_object_def_register(mod->env.store);

		struct ast_dt_local_member *local_members; //[comp->composite.num_members];
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
		for (size_t i = 0; i < comp->composite.num_members; i++) {
			printf("finalizing %.*s:\n", ALIT(comp->composite.members[i].name));
			ast_env_print(ctx->vm, env);
			ast_print(ctx, env, comp->composite.members[i].type);

			type_id member_type_id;
			int err;
			err = ast_node_eval_type(ctx, mod, env,
					comp->composite.members[i].type,
					&member_type_id);
			if (err) {
				printf("Failed to resolve type of composit member.\n");
				continue;
			}

			params[i].param_id = i;
			params[i].name = comp->composite.members[i].name;
			params[i].slot =
				ast_bind_slot_member(ctx, env, AST_BIND_NEW,
						NULL, params[i].name,
						ast_bind_slot_const_type(ctx, env, AST_BIND_NEW,
							NULL, member_type_id));

			struct type *member_type;
			member_type = vm_get_type(ctx->vm, member_type_id);

			local_members[i].name = comp->composite.members[i].name;
			local_members[i].type = member_type_id;
			local_members[i].location = offset;

			offset += member_type->size;
		}

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

			binds[bind_i].target =
				ast_node_deep_copy(ctx, &def->env, env,
						mbr->bound->target);
			binds[bind_i].value =
				stg_register_func(mod->stg_mod, func);

			bind_i += 1;
		}

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

	return result;
}
