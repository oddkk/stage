#include "system.h"
#include "mod.h"

#include <module.h>
#include <bytecode.h>
#include <native_bytecode.h>

#include <string.h>

#include <string.h>
#include <pthread.h>
#include <time.h>

static void *
stream_system_thread(void *);

void
stream_system_init(struct vm *vm, struct stream_system *sys)
{
	paged_list_init(
			&sys->node_kinds,
			&vm->mem,
			sizeof(struct stream_node_kind));

	paged_list_init(
			&sys->pipes,
			&vm->mem,
			sizeof(struct stream_pipe));

	sys->state = STREAM_STATE_STOPPED;
}

static inline bool
sys_transition_state(struct stream_system *sys,
		enum stream_system_state from,
		enum stream_system_state to)
{
	return __sync_bool_compare_and_swap(
			&sys->state, from, to);
}

int
stream_system_start(struct stream_system *sys)
{
	if (!sys_transition_state(sys,
				STREAM_STATE_STOPPED, STREAM_STATE_STARTING)) {
		return -1;
	}

	int err;

	pthread_attr_t attr;

	err = pthread_attr_init(&attr);
	if (err) {
		perror("pthread_attr_init");
		return -1;
	}

	err = pthread_create(
			&sys->thread_handle,
			NULL, &stream_system_thread, sys);
	if (err) {
		perror("pthread_create");
		return -1;
	}

	pthread_attr_destroy(&attr);

	return 0;
}

void
stream_system_stop(struct stream_system *sys)
{
	if (!sys_transition_state(sys,
				STREAM_STATE_RUNNING, STREAM_STATE_STOPPING)) {
		pthread_cancel(sys->thread_handle);
	}


	int err;
	err = pthread_join(sys->thread_handle, NULL);
	if (err) {
		perror("pthread_join");
		return;
	}

	assert(sys->state == STREAM_STATE_STOPPED);
}


struct stream_system *
stream_get_system(struct vm *vm)
{
	struct stream_mod_info *info;
	info = stream_mod_get_info(vm);

	return info->sys;
}

struct stream_node_kind *
stream_get_node_kind(struct stg_module *mod, struct atom *name)
{
	struct stream_system *sys;
	sys = stream_get_system(mod->vm);

	for (size_t i = 0; i < sys->node_kinds.length; i++) {
		struct stream_node_kind *kind;
		kind = paged_list_get(&sys->node_kinds, i);
		if (kind->mod_id == mod->id &&
				kind->name == name) {
			return kind;
		}
	}

	return NULL;
}

int
stream_register_node_kind(struct stg_module *mod, struct stream_node_kind kind)
{
	struct stream_system *sys;
	sys = stream_get_system(mod->vm);

	size_t id;
	id = paged_list_push(&sys->node_kinds);

	struct stream_node_kind *new_kind;
	new_kind = paged_list_get(&sys->node_kinds, id);
	*new_kind = kind;

	new_kind->mod_id = mod->id;

	return 0;
}

struct stream_node
stream_copy_node(struct stg_exec *ctx, struct stream_node node)
{
	void *new_data;
	new_data = stg_alloc(ctx, 1, node.data_size);
	memcpy(new_data, node.data, node.data_size);
	node.data = new_data;

	if (node.kind->copy_node) {
		node.kind->copy_node(ctx, node.data);
	}

	return node;
}

struct stream_node *
stream_copy_node_ref(struct stg_exec *ctx, struct stream_node node)
{
	struct stream_node *new_node;
	new_node = stg_alloc(ctx, 1, sizeof(struct stream_node));
	*new_node = stream_copy_node(ctx, node);
	return new_node;
}

struct stream_node *
stream_alloc_node(struct stg_exec *ctx, struct stream_node_kind *kind,
		void *data, size_t data_size)
{
	struct stream_node *node;
	node = stg_alloc(ctx, 1, sizeof(struct stream_node));

	assert(kind);
	node->kind = kind;
	node->data = data;
	node->data_size = data_size;

	return node;
}

struct bc_result
stream_compile_node(struct bc_env *env, struct arena *mem, struct stream_node *node)
{
	assert(node->kind && node->kind->gen_bytecode);

	struct bc_result res;
	res = node->kind->gen_bytecode(env, mem, node->data);

	return res;
}

void
stream_register_endpoint(struct stg_module *mod,
		struct stream_node *node, struct stream_pipe_config cfg)
{
	struct stream_pipe pipe = {0};
	pipe.mod_id = mod->id;

	struct stg_exec heap = {0};
	heap.vm = mod->vm;
	heap.heap = &mod->mem;

	pipe.end_point = stream_copy_node_ref(&heap, *node);
	pipe.config = cfg;

	pipe.bc = stg_alloc(&heap, 1, sizeof(struct bc_env));

	pipe.bc->vm = mod->vm;
	pipe.bc->store = mod->vm->instr_store;

	struct bc_result instrs;
	instrs = stream_compile_node(pipe.bc, &mod->mem, pipe.end_point);

	if (instrs.err) {
		panic("Failed to compile stream pipe.");
		return;
	}

	pipe.bc->entry_point = instrs.first;

	pipe.bc->nbc = stg_alloc(&heap, 1, sizeof(struct nbc_func));
	nbc_compile_from_bc(&mod->vm->transient,
			&mod->mem, pipe.bc->nbc, pipe.bc);

	struct stream_system *sys;
	sys = stream_get_system(mod->vm);

	size_t pipe_i;
	pipe_i = paged_list_push(&sys->pipes);

	struct stream_pipe *new_pipe;
	new_pipe = paged_list_get(&sys->pipes, pipe_i);

	*new_pipe = pipe;
}

static void *
stream_system_thread(void *sys_ptr)
{
	struct stream_system *sys = sys_ptr;

	struct stg_memory memory = {0};

	int err;
	err = stg_memory_init(&memory);
	if (err) {
		print_error("stream", "Failed to initialize memory context.");
		return NULL;
	}

	struct arena mem = {0};
	err = arena_init(&mem, &memory);
	if (err) {
		print_error("stream", "Failed to initialize memory context.");
		return NULL;
	}

	struct stg_exec exec_ctx = {0};
	exec_ctx.heap = &mem;

	if (!sys_transition_state(sys,
				STREAM_STATE_STARTING, STREAM_STATE_RUNNING)) {
		print_error("stream",
				"Attempting to start stream system while it is not ready to be "
				"started.");
		return NULL;
	}

	arena_mark mem_base;
	mem_base = arena_checkpoint(&mem);

	while (sys->state == STREAM_STATE_RUNNING) {
		struct timespec duration = {0};
		duration.tv_sec = 1;
		nanosleep(&duration, NULL);
		// nbc_exec(sys->vm, &exec_ctx, );

		arena_reset(&mem, mem_base);
	}

	if (!sys_transition_state(sys,
				STREAM_STATE_STOPPING, STREAM_STATE_STOPPED)) {
		print_error("stream",
				"Attempting to stop stream system while it is not ready to be "
				"stopped.");
		return NULL;
	}

	arena_destroy(&mem);
	stg_memory_destroy(&memory);

	return NULL;
}
