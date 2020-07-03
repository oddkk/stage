#include "system.h"
#include "mod.h"

#include <module.h>
#include <bytecode.h>
#include <native_bytecode.h>

#include <string.h>

#include <signal.h>
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

	sys->vm = vm;
}

static inline struct stream_pipe *
stream_get_pipe(struct stream_system *sys, stream_pipe_id id)
{
	return paged_list_get(&sys->pipes, id);
}

static inline bool
pipe_transition_state(struct stream_pipe *pipe,
		enum stream_pipe_state from,
		enum stream_pipe_state to)
{
	return __sync_bool_compare_and_swap(
			&pipe->state, from, to);
}

int
stream_system_start_pipe(struct stream_system *sys, stream_pipe_id id)
{
	struct stream_pipe *pipe;
	pipe = stream_get_pipe(sys, id);

	if (!pipe_transition_state(pipe,
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
			&pipe->thread_handle,
			NULL, &stream_system_thread, pipe);
	if (err) {
		perror("pthread_create");
		return -1;
	}

	pthread_attr_destroy(&attr);

	return 0;
}

void
stream_system_stop_pipe(struct stream_system *sys, stream_pipe_id id)
{
	struct stream_pipe *pipe;
	pipe = stream_get_pipe(sys, id);

	if (!pipe_transition_state(pipe,
				STREAM_STATE_RUNNING, STREAM_STATE_STOPPING)) {
		pthread_cancel(pipe->thread_handle);
		pipe->state = STREAM_STATE_STOPPED;
	} else {
		pthread_kill(pipe->thread_handle, SIGUSR1);
	}


	int err;
	err = pthread_join(pipe->thread_handle, NULL);
	if (err) {
		perror("pthread_join");
		return;
	}

	assert(pipe->state == STREAM_STATE_STOPPED);
}

int
stream_system_start(struct stream_system *sys)
{
	for (size_t i = 0; i < sys->pipes.length; i++) {
		stream_system_start_pipe(sys, i);
	}

	return 0;
}

void
stream_system_stop(struct stream_system *sys)
{
	for (size_t i = 0; i < sys->pipes.length; i++) {
		stream_system_stop_pipe(sys, i);
	}
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

stream_pipe_id
stream_register_endpoint(struct stg_module *mod,
		struct stream_node *node, struct stream_pipe_config cfg)
{
	struct stream_system *sys;
	sys = stream_get_system(mod->vm);

	struct stream_pipe pipe = {0};
	pipe.mod_id = mod->id;
	pipe.sys = sys;

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
		return -1;
	}

	pipe.out_type = bc_get_var_type(pipe.bc, instrs.out_var);

	append_bc_instr(&instrs,
			bc_gen_ret(pipe.bc, instrs.out_var));

	pipe.bc->entry_point = instrs.first;

	pipe.bc->nbc = stg_alloc(&heap, 1, sizeof(struct nbc_func));
	nbc_compile_from_bc(&mod->vm->transient,
			&mod->mem, pipe.bc->nbc, pipe.bc);

	stream_pipe_id pipe_i;
	pipe_i = paged_list_push(&sys->pipes);

	struct stream_pipe *new_pipe;
	new_pipe = paged_list_get(&sys->pipes, pipe_i);

	*new_pipe = pipe;

	return pipe_i;
}

static void
stream_signal_ignore(int sig)
{
	(void)sig;
}

static void *
stream_system_thread(void *pipe_ptr)
{
	struct stream_pipe *pipe = pipe_ptr;
	struct stream_system *sys = pipe->sys;

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

	// Suppress any message printed indicating the signal was passed. SIGUSR1
	// is used to indicate this thread should re-check it's state and quit if
	// requested.
	signal(SIGUSR1, stream_signal_ignore);

	if (!pipe_transition_state(pipe,
				STREAM_STATE_STARTING, STREAM_STATE_RUNNING)) {
		print_error("stream",
				"Attempting to start stream while it is not ready to be "
				"started.");
		return NULL;
	}

	struct type *type;
	type = vm_get_type(sys->vm, pipe->out_type);

	struct object out_obj = {0};
	out_obj.type = pipe->out_type;
	out_obj.data = arena_alloc(&mem, type->size);

	arena_mark mem_base;
	mem_base = arena_checkpoint(&mem);

	struct nbc_func *func;
	func = pipe->bc->nbc;

	uint64_t frame = 0;

	void *args[] = {&frame};

	while (pipe->state == STREAM_STATE_RUNNING) {
		nbc_exec(sys->vm, &exec_ctx, func,
				args, sizeof(args)/sizeof(*args), NULL, out_obj.data);

		print_obj_repr(sys->vm, out_obj);
		printf("\n");

		arena_reset(&mem, mem_base);

		struct timespec duration = {0};
		duration.tv_nsec = 250000000;
		nanosleep(&duration, NULL);
	}

	if (!pipe_transition_state(pipe,
				STREAM_STATE_STOPPING, STREAM_STATE_STOPPED)) {
		print_error("stream",
				"Attempting to stop stream while it is not ready to be "
				"stopped.");
		return NULL;
	}

	arena_destroy(&mem);
	stg_memory_destroy(&memory);

	return NULL;
}
