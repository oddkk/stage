#include "message.h"
#include <ast.h>
#include <bytecode.h>
#include <native_bytecode.h>
#include <base/mod.h>
#include <stdlib.h>
#include <string.h>

void
msg_system_init(struct msg_system *sys, struct stg_module *mod)
{
	memset(sys, 0, sizeof(struct msg_system));
	sys->mod = mod;
	sys->transient = &mod->vm->transient;

	paged_list_init(
			&sys->triggers,
			&mod->vm->mem,
			sizeof(struct msg_trigger));

	paged_list_init(
			&sys->subscribers,
			&mod->vm->mem,
			sizeof(struct msg_subscriber));
}

static struct msg_trigger *
msg_get_trigger(struct msg_system *sys, msg_trigger_id id)
{
	return paged_list_get(&sys->triggers, id);
}

void
msg_system_destroy(struct msg_system *sys)
{
	paged_list_destroy(&sys->triggers);
	paged_list_destroy(&sys->subscribers);

	memset(sys, 0, sizeof(struct msg_system));
}

msg_trigger_id
msg_register_trigger(struct msg_system *sys, type_id type)
{
	msg_trigger_id id;
	id = paged_list_push(&sys->triggers);

	struct msg_trigger *trigger;
	trigger = msg_get_trigger(sys, id);
	trigger->id = id;
	trigger->type = type;
	trigger->first_subscriber = NULL;

	return id;
}

int
msg_trigger_subscribe(struct msg_system *sys,
		msg_trigger_id trigger_id, struct stg_func_object func)
{
	struct vm *vm = sys->mod->vm;

	struct msg_trigger *trigger;
	trigger = msg_get_trigger(sys, trigger_id);

	struct func *fn;
	fn = vm_get_func(vm, func.func);

	struct type *fn_type;
	fn_type = vm_get_type(vm, fn->type);

	struct stg_func_type *fn_type_info;
	fn_type_info = fn_type->data;

	if (fn_type_info->num_params != 1) {
		return -1;
	}
	if (!type_equals(vm, trigger->type, fn_type_info->params[0])) {
		return -1;
	}

	if (!msg_type_is_inst(vm, fn_type_info->return_type)) {
		return -1;
	}

	int sub_id;
	sub_id = paged_list_push(&sys->subscribers);

	struct msg_subscriber *sub;
	sub = paged_list_get(&sys->subscribers, sub_id);

	sub->trigger = trigger_id;
	// TODO: The function closure is currently being calloced. We should have a
	// better system for memory ownership for function closures.
	sub->func = func;

	sub->out_monad_type = fn_type_info->return_type;
	sub->out_inner_type = msg_return_type(vm, sub->out_monad_type);

	struct type *ret_type;
	ret_type = vm_get_type(vm, sub->out_inner_type);
	sub->out_inner_size = ret_type->size;

	sub->next = trigger->first_subscriber;
	trigger->first_subscriber = sub;

	return 0;
}

int
msg_system_compile(struct msg_system *sys)
{
	return 0;
}

static void
msg_notify(struct msg_system *sys,
		struct msg_subscriber *sub,
		struct object obj)
{
	struct stg_exec ctx = {0};
	ctx.heap = sys->transient;

	struct msg_monad_data monad = {0};
	struct object monad_obj = {0};
	monad_obj.type = sub->out_monad_type;
	monad_obj.data = &monad;

	arena_mark cp = arena_checkpoint(ctx.heap);
	vm_call_func_obj(sys->mod->vm, &ctx, sub->func, &obj, 1, &monad_obj);

	struct object out = {0};
	out.type = sub->out_inner_type;
	out.data = stg_alloc(&ctx, sub->out_inner_size, 1);

	msg_monad_call(sys->mod->vm, &ctx, monad_obj, &out);

	arena_reset(ctx.heap, cp);
}

void
msg_post(struct msg_system *sys,
		msg_trigger_id id, struct object obj)
{
	struct msg_trigger *trigger;
	trigger = msg_get_trigger(sys, id);

	struct msg_subscriber *sub;
	sub = trigger->first_subscriber;

	assert_type_equals(sys->mod->vm, trigger->type, obj.type);

	while (sub) {
		msg_notify(sys, sub, obj);

		sub = sub->next;
	}
}
