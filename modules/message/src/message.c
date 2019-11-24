#include "message.h"
#include <bytecode.h>
#include <native_bytecode.h>
#include <base/mod.h>
#include <stdlib.h>
#include <string.h>

// For sysconf
#include <unistd.h>
// For mmap
#include <sys/mman.h>

#define MSG_GEN_SHOW_BC 1

static inline void
append_bc_instr(struct ast_gen_bc_result *res, struct bc_instr *instr)
{
	if (res->last) {
		assert(res->first);
		res->last->next = instr;
	} else {
		res->first = instr;
	}
	res->last = instr;
}

static inline void
append_bc_instrs(struct ast_gen_bc_result *res, struct ast_gen_bc_result instrs)
{
	if (!instrs.first) {
		return;
	}
	assert(instrs.first && instrs.last);
	append_bc_instr(res, instrs.first);
	res->last = instrs.last;
}

struct msg_pipe_node *
msg_pipe_get_node(struct msg_system *sys, msg_node_id node_i)
{
	assert(node_i < sys->num_alloced_nodes);

	const size_t cons_per_page =
		sys->page_size / sizeof(struct msg_pipe_node);

	return &sys->node_pages[node_i / cons_per_page][node_i % cons_per_page];
}

static inline struct msg_pipe_connection *
msg_pipe_get_connection(struct msg_system *sys, size_t connection_i)
{
	assert(connection_i < sys->num_alloced_connections);

	const size_t cons_per_page =
		sys->page_size / sizeof(struct msg_pipe_connection);

	return &sys->connection_pages[connection_i / cons_per_page][connection_i % cons_per_page];
}

int
msg_pipe_iter_outgoing_connections(
		struct msg_system *sys, msg_node_id from,
		msg_node_id *out_to, size_t *iter)
{
	for (; *iter < sys->num_alloced_connections; (*iter)++) {
		struct msg_pipe_connection *con;
		con = msg_pipe_get_connection(sys, *iter);
		if (con->from == from) {
			*out_to = con->to;
			*iter += 1;
			return 1;
		}
	}
	return 0;
}

static inline void
msg_init_page_size(struct msg_system *sys)
{
	if (!sys->page_size) {
		sys->page_size = sysconf(_SC_PAGESIZE);
	}
}

void
msg_pipe_connect(struct msg_system *sys,
		msg_node_id from, msg_node_id to)
{
	msg_init_page_size(sys);

	assert(from < sys->num_alloced_nodes);
	assert(to   < sys->num_alloced_nodes);

	assert(msg_pipe_get_node(sys, from)->kind != MSG_PIPE_ENDPOINT);
	assert(msg_pipe_get_node(sys, to  )->kind != MSG_PIPE_ENTRYPOINT);

	const size_t cons_per_page =
		sys->page_size / sizeof(struct msg_pipe_connection);

	if (cons_per_page * sys->num_connection_pages >= sys->num_alloced_connections) {
		struct msg_pipe_connection **new_pages;

		new_pages = realloc(sys->connection_pages,
				sizeof(struct msg_pipe_connection *) * (sys->num_connection_pages+1));

		if (!new_pages) {
			printf("Failed to allocate connection page.\n");
			return;
		}

		sys->connection_pages = new_pages;

		struct msg_pipe_connection *new_page;
		new_page = mmap(
				NULL, sys->page_size,
				PROT_READ|PROT_WRITE,
				MAP_PRIVATE|MAP_ANONYMOUS,
				-1, 0);

		if (new_page == MAP_FAILED) {
			perror("mmap");
			return;
		}

		sys->connection_pages[sys->num_connection_pages] = new_page;
		sys->num_connection_pages += 1;
	}

	size_t con_i;
	con_i = sys->num_alloced_connections;
	sys->num_alloced_connections += 1;

	struct msg_pipe_connection *con;
	con = msg_pipe_get_connection(sys, con_i);
	memset(con, 0, sizeof(struct msg_pipe_connection));

	con->from = from;
	con->to   = to;
}

static msg_node_id
msg_alloc_node(struct msg_system *sys)
{
	msg_init_page_size(sys);

	const size_t cons_per_page =
		sys->page_size / sizeof(struct msg_pipe_node);

	if (cons_per_page * sys->num_node_pages >= sys->num_alloced_nodes) {
		struct msg_pipe_node **new_pages;

		new_pages = realloc(sys->node_pages,
				sizeof(struct msg_pipe_node *) * (sys->num_node_pages+1));

		if (!new_pages) {
			printf("Failed to allocate node page.\n");
			return -1;
		}

		sys->node_pages = new_pages;

		struct msg_pipe_node *new_page;
		new_page = mmap(
				NULL, sys->page_size,
				PROT_READ|PROT_WRITE,
				MAP_PRIVATE|MAP_ANONYMOUS,
				-1, 0);

		if (new_page == MAP_FAILED) {
			perror("mmap");
			return -1;
		}

		sys->node_pages[sys->num_node_pages] = new_page;
		sys->num_node_pages += 1;
	}

	msg_node_id node_i;
	node_i = sys->num_alloced_nodes;
	sys->num_alloced_nodes += 1;

	struct msg_pipe_node *node;
	node = msg_pipe_get_node(sys, node_i);
	memset(node, 0, sizeof(struct msg_pipe_node));

	return node_i;
}

msg_node_id
msg_pipe_map(struct msg_system *sys, struct stg_func_object func)
{
	msg_node_id node_i;
	node_i = msg_alloc_node(sys);

	struct msg_pipe_node *node;
	node = msg_pipe_get_node(sys, node_i);
	node->kind = MSG_PIPE_MAP;

	node->map.func = func;

	return node_i;
}

msg_node_id
msg_pipe_filter(struct msg_system *sys, struct stg_func_object func)
{
	msg_node_id node_i;
	node_i = msg_alloc_node(sys);

	struct msg_pipe_node *node;
	node = msg_pipe_get_node(sys, node_i);
	node->kind = MSG_PIPE_FILTER;

	node->filter.func = func;

	return node_i;
}

msg_node_id
msg_pipe_entrypoint(struct msg_system *sys, type_id type)
{
	msg_node_id node_i;
	node_i = msg_alloc_node(sys);

	struct msg_pipe_node *node;
	node = msg_pipe_get_node(sys, node_i);
	node->kind = MSG_PIPE_ENTRYPOINT;

	node->entrypoint.pipe_func = FUNC_UNSET;
	node->entrypoint.type = type;

	return node_i;
}

msg_node_id
msg_pipe_endpoint(struct msg_system *sys, func_id callback, void *closure)
{
	msg_node_id node_i;
	node_i = msg_alloc_node(sys);

	struct msg_pipe_node *node;
	node = msg_pipe_get_node(sys, node_i);
	node->kind = MSG_PIPE_ENDPOINT;

	node->endpoint.callback = callback;
	node->endpoint.closure = closure;

	return node_i;
}

static struct ast_gen_bc_result
msg_system_compile_node(
		struct msg_system *sys, struct bc_env *bc_env,
		msg_node_id node_id, bc_var in_var)
{
	struct ast_gen_bc_result result = {0};

	struct msg_pipe_node *node;
	node = msg_pipe_get_node(sys, node_id);

	bc_var out_var = BC_VAR_NEW;

	switch (node->kind) {
		case MSG_PIPE_MAP:
			{
				struct bc_instr *func_load;
				func_load = bc_gen_load(bc_env, BC_VAR_NEW,
						stg_register_func_object(sys->vm, &sys->mod->store,
							node->map.func.func, node->map.func.closure));
				append_bc_instr(&result, func_load);

				append_bc_instr(&result,
						bc_gen_push_arg(bc_env, in_var));
				struct bc_instr *map_op;
				map_op = bc_gen_vcall(bc_env, BC_VAR_NEW,
						func_load->load.target);
				append_bc_instr(&result, map_op);
				out_var = map_op->vcall.target;
			}
			break;

		case MSG_PIPE_FILTER:
			{
				struct bc_instr *func_load;
				func_load = bc_gen_load(bc_env, BC_VAR_NEW,
						stg_register_func_object(sys->vm, &sys->mod->store,
							node->filter.func.func, node->filter.func.closure));
				append_bc_instr(&result, func_load);

				append_bc_instr(&result,
						bc_gen_push_arg(bc_env, in_var));
				struct bc_instr *filter_op;
				filter_op = bc_gen_vcall(bc_env, BC_VAR_NEW,
						func_load->load.target);
				append_bc_instr(&result, filter_op);
				// TODO: Conditionals in byte code.
				out_var = in_var;
			}
			break;

		case MSG_PIPE_ENTRYPOINT:
			out_var = in_var;
			break;

		case MSG_PIPE_ENDPOINT:
			{
				struct func *func;
				func = vm_get_func(sys->vm, node->endpoint.callback);

				append_bc_instr(&result,
						bc_gen_push_arg(bc_env, in_var));

				if ((func->flags & FUNC_CLOSURE) != 0) {
					append_bc_instr(&result,
							bc_gen_clcall(bc_env, BC_VAR_NEW,
								node->endpoint.callback,
								node->endpoint.closure));
				} else {
					append_bc_instr(&result,
							bc_gen_lcall(bc_env, BC_VAR_NEW,
								node->endpoint.callback));
				}
			}
			return result;
	}

	assert(out_var != BC_VAR_NEW);

	size_t iter = 0;
	msg_node_id child;
	while (msg_pipe_iter_outgoing_connections(
				sys, node_id, &child, &iter)) {
		append_bc_instrs(&result,
			msg_system_compile_node(sys, bc_env, child, out_var));
	}

	return result;
}

static int
msg_system_compile_entrypoint(
		struct msg_system *sys, msg_node_id entrypoint)
{
	struct msg_pipe_node *node;
	node = msg_pipe_get_node(sys, entrypoint);
	assert(node->kind == MSG_PIPE_ENTRYPOINT);

	struct bc_env *bc_env;
	bc_env = calloc(1, sizeof(struct bc_env));

	bc_env->vm = sys->vm;
	bc_env->store = sys->vm->instr_store;

	struct ast_gen_bc_result result = {0};

	result = msg_system_compile_node(
			sys, bc_env, entrypoint,
			bc_alloc_param(bc_env, 0, node->entrypoint.type));

	struct object unit = {0};
	unit.type = bc_env->vm->default_types.unit;
	append_bc_instr(&result,
			bc_gen_load(bc_env, BC_VAR_NEW, unit));
	append_bc_instr(&result,
			bc_gen_ret(bc_env, result.last->load.target));

	bc_env->entry_point = result.first;

#if MSG_GEN_SHOW_BC
	printf("\nunpack bc:\n");
	bc_print(bc_env, bc_env->entry_point);
#endif

	bc_env->nbc = calloc(1, sizeof(struct nbc_func));
	nbc_compile_from_bc(bc_env->nbc, bc_env);

#if MSG_GEN_SHOW_BC
	printf("\nunpack nbc:\n");
	nbc_print(bc_env->nbc);
#endif

	struct func func = {0};
	func.kind = FUNC_BYTECODE;
	func.type = stg_register_func_type(
			sys->mod, sys->vm->default_types.unit,
			&node->entrypoint.type, 1);
	func.bytecode = bc_env;

	node->entrypoint.pipe_func =
		stg_register_func(sys->mod, func);

	return 0;
}

int
msg_system_compile(struct msg_system *sys)
{
	int result = 0;

	for (msg_node_id i = 0; i < sys->num_alloced_nodes; i++) {
		struct msg_pipe_node *node;
		node = msg_pipe_get_node(sys, i);
		if (node->kind == MSG_PIPE_ENTRYPOINT) {
			int err;
			err = msg_system_compile_entrypoint(sys, i);
			if (err) {
				result = -1;
			}
		}
	}

	return result;
}

void
msg_post(struct msg_system *sys,
		msg_node_id entrypoint, struct object obj)
{
	struct msg_pipe_node *node;
	node = msg_pipe_get_node(sys, entrypoint);
	assert(node->kind == MSG_PIPE_ENTRYPOINT);
	assert(node->entrypoint.pipe_func != FUNC_UNSET);

	struct object ret = {0};
	ret.type = sys->vm->default_types.unit;
	int err;

	err = vm_call_func(sys->vm, node->entrypoint.pipe_func,
			&obj, 1, &ret);
	if (err) {
		printf("Failed to post message.\n");
	}
}
