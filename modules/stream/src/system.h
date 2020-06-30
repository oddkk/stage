#ifndef STAGE_STREAM_SYSTEM_H
#define STAGE_STREAM_SYSTEM_H

#include "freq.h"

#include <vm.h>
#include <arena.h>
#include <bytecode.h>
#include <pthread.h>

struct bc_env;
typedef int bc_var;

struct stream_node_param {
	struct atom *name;
	struct object value;
};

struct stream_node_param_map {
	struct stream_node_param *params;
	size_t num_params;
};

struct stream_node_kind {
	stg_mod_id mod_id;
	struct atom *name;
	struct bc_result (*gen_bytecode)(struct bc_env *, struct arena *, void *data);
	void (*copy_node)(struct stg_exec *, void *new_data);
};

struct stream_node {
	struct stream_node_kind *kind;
	void *data;
	size_t data_size;
};

struct stream_pipe_config {
	freq_t freq;
	size_t buffer;
	size_t compute_width;
};

struct stream_pipe {
	stg_mod_id mod_id;
	struct stream_node *end_point;
	struct bc_env *bc;

	struct stream_pipe_config config;
};

enum stream_system_state {
	STREAM_STATE_STOPPED = 0,
	STREAM_STATE_STARTING,
	STREAM_STATE_RUNNING,
	STREAM_STATE_STOPPING,
};

struct stream_system {
	struct paged_list node_kinds;
	struct paged_list pipes;

	enum stream_system_state state;

	pthread_t thread_handle;
};

void
stream_system_init(struct vm *, struct stream_system *);

int
stream_system_start(struct stream_system *);

void
stream_system_stop(struct stream_system *);

struct stream_system *
stream_get_system(struct vm *);

struct stream_node_kind *
stream_get_node_kind(struct stg_module *mod, struct atom *name);

#define stream_get_node_kinds(mod, name) stream_get_node_kind((mod), mod_atoms((mod), name))

int
stream_register_node_kind(struct stg_module *mod, struct stream_node_kind);

struct stream_node
stream_copy_node(struct stg_exec *ctx, struct stream_node node);

struct stream_node *
stream_copy_node_ref(struct stg_exec *ctx, struct stream_node node);

struct stream_node *
stream_alloc_node(struct stg_exec *, struct stream_node_kind *,
		void *data, size_t data_size);

struct bc_result
stream_compile_node(struct bc_env *, struct arena *, struct stream_node *);

void
stream_register_endpoint(struct stg_module *mod,
		struct stream_node *node, struct stream_pipe_config);

#endif
