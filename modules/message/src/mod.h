#ifndef STG_MESSAGE_MOD_H
#define STG_MESSAGE_MOD_H

#define MOD_MESSAGE "message"
#include <module.h>
#include "message.h"
#include "monad.h"

struct msg_context {
	struct msg_system sys;
	struct object_cons *msg_type_cons;
	msg_node_id on_start_msg;
};

#endif
