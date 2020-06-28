#ifndef STAGE_STREAM_SYSTEM_H
#define STAGE_STREAM_SYSTEM_H

#include <vm.h>
#include <arena.h>
#include <bytecode.h>

struct bc_env;
typedef int bc_var;

/*
static inline bool
freq_eq(struct freq lhs, struct freq rhs)
{
	return lhs.unit == rhs.unit &&
		lhs.value == rhs.value;
}
*/

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

	// void *(*init)(struct stg_exec *, struct stream_node_param *params, size_t num_params);
	// void (*destroy)(struct stg_exec *, void *);
};

struct stream_node {
	struct stream_node_kind *kind;
	void *data;
	size_t data_size;
};

struct stream_pipe {
	stg_mod_id mod_id;
	struct stream_node *end_point;

	struct bc_env *bc;
};

struct stream_system {
	struct paged_list node_kinds;
	struct paged_list nodes;
};

void
stream_system_init(struct vm *, struct stream_system *);

struct stream_system *
stream_get_system(struct vm *);

struct stream_node_kind *
stream_get_node_kind(struct stg_module *mod, struct atom *name);

int
stream_register_node_kind(struct stg_module *mod, struct stream_node_kind);

#endif
