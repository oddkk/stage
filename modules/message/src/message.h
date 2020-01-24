#ifndef STAGE_MESSAGE_MESSAGE_H
#define STAGE_MESSAGE_MESSAGE_H

#include <module.h>

typedef unsigned int msg_node_id;
typedef unsigned char msg_node_point_id;

typedef void (*msg_callback)(void *data, struct object obj);

enum msg_pipe_node_kind {
	MSG_PIPE_MAP,
	MSG_PIPE_FILTER,
	MSG_PIPE_ENTRYPOINT,
	MSG_PIPE_ENDPOINT,
};

struct msg_pipe_node {
	enum msg_pipe_node_kind kind;

	union {
		struct {
			struct stg_func_object func;
		} map;

		struct {
			struct stg_func_object func;
		} filter;

		struct {
			type_id type;
			func_id pipe_func;
		} entrypoint;

		struct {
			func_id callback;
			void *closure;
		} endpoint;
	};
};

struct msg_pipe_connection {
	msg_node_id from, to;
};

struct msg_system {
	struct vm *vm;
	struct stg_module *mod;

	struct msg_entrypoint *entrypoints;
	size_t num_entrypoints;

	size_t page_size;

	struct msg_pipe_node **node_pages;
	size_t num_node_pages;
	size_t num_alloced_nodes;

	struct msg_pipe_connection **connection_pages;
	size_t num_connection_pages;
	size_t num_alloced_connections;
};

struct msg_pipe_node *
msg_pipe_get_node(struct msg_system *sys, msg_node_id);

int
msg_pipe_iter_outgoing_connections(
		struct msg_system *sys, msg_node_id from,
		msg_node_id *out_to, size_t *iter);

msg_node_id
msg_pipe_map(struct msg_system *sys, struct stg_func_object func);

msg_node_id
msg_pipe_filter(struct msg_system *sys, struct stg_func_object func);

msg_node_id
msg_pipe_entrypoint(struct msg_system *sys, type_id type);

msg_node_id
msg_pipe_endpoint(struct msg_system *sys, func_id, void *closure);

void
msg_pipe_connect(struct msg_system *sys,
		msg_node_id from, msg_node_id to);

int
msg_system_compile(struct msg_system *sys);

void
msg_post(struct msg_system *sys,
		msg_node_id entrypoint, struct object obj);

#endif
