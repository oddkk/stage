#ifndef STAGE_MESSAGE_MESSAGE_H
#define STAGE_MESSAGE_MESSAGE_H

#include "mod.h"

typedef unsigned int msg_node_id;

struct msg_endpoint {
	int _dc;
};

enum msg_pipe_node_type {
	MSG_PIPE_FILTER,
	MSG_PIPE_MAP,
	MSG_PIPE_MATCH,
	MSG_PIPE_ENDPOINT,
};

struct expr_node;
struct msg_pipe_node;

struct msg_pipe_connection {
	struct msg_pipe_node *node;
};

struct msg_match_case {
	struct object value;
	struct msg_pipe_connection *drain;
};

struct msg_pipe_node {
	enum msg_pipe_node_type type;

	struct msg_pipe_node *src;

	type_id out_type;

	union {
		struct {
			struct object func;
			struct msg_pipe_connection *drain;
		} filter;

		struct {
			struct object func;
			struct msg_pipe_connection *drain;
		} map;

		struct {
			struct object value_func;
			struct msg_match_connection *cases;
		} match;

		struct {
			vm_builtin_func func;
			void *user_data;
		} endpoint;
	};
};

struct msg_entrypoint {
	struct atom *name;
	struct msg_pipe_node *pipe;
};

struct msg_system {
	struct msg_entrypoint *entrypoints;
};

struct msg_pipe_node *
msg_filter_node(struct msg_system *, func_id func);

struct msg_pipe_node *
msg_map_node(struct msg_system *, func_id func);

struct msg_pipe_node *
msg_match_node(struct msg_system *, func_id value_func,
               struct object *cases, size_t num_cases);

struct msg_pipe_node *
msg_endpoint_node(struct msg_system *, vm_builtin_func func, void *user_data);

bool
msg_pipe_connect(struct msg_system *, struct msg_pipe_connection *src, struct msg_pipe_node *drain);

msg_node_id
msg_register_message(struct msg_system *, type_id);

void
msg_post(struct msg_system *, msg_node_id, struct object);

#endif
