#ifndef STG_MESSAGE_MOD_H
#define STG_MESSAGE_MOD_H

#define MOD_MESSAGE "message"
#include <module.h>
#include "message.h"
#include "monad.h"
#include "trigger.h"

struct msg_context {
	struct msg_system sys;
	struct object_cons *msg_type_cons;
	struct object_cons *msg_trigger_cons;
	msg_trigger_id on_start_msg;
};

struct msg_system *
msg_get_system(struct vm *vm);

#endif
