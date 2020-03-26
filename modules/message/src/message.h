#ifndef STAGE_MESSAGE_MESSAGE_H
#define STAGE_MESSAGE_MESSAGE_H

#include <module.h>
#include "monad.h"

typedef unsigned int msg_trigger_id;

struct msg_trigger {
	msg_trigger_id id;
	type_id type;

	struct msg_subscriber *first_subscriber;
};

struct msg_subscriber {
	msg_trigger_id trigger;

	struct stg_func_object func;
	type_id out_monad_type;

	type_id out_inner_type;
	size_t out_inner_size;

	struct msg_subscriber *next;
};

struct msg_system {
	struct stg_module *mod;

	struct arena *transient;

	struct paged_list triggers;
	struct paged_list subscribers;
};

void
msg_system_init(struct msg_system *, struct stg_module *mod);

void
msg_system_destroy(struct msg_system *);

msg_trigger_id
msg_register_trigger(struct msg_system *, type_id type);

int
msg_trigger_subscribe(struct msg_system *,
		msg_trigger_id, struct stg_func_object);

int
msg_system_compile(struct msg_system *sys);

void
msg_post(struct msg_system *sys,
		msg_trigger_id trigger, struct object obj);

#endif
