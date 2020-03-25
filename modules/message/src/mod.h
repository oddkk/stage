#ifndef STG_MESSAGE_MOD_H
#define STG_MESSAGE_MOD_H

#define MOD_MESSAGE "message"
#include <module.h>
#include "message.h"
#include "monad.h"

struct msg_context {
	struct msg_system sys;
	struct object_cons *msg_type_cons;
	struct object_cons *msg_trigger_cons;
	msg_trigger_id on_start_msg;
};

type_id msg_trigger_register_type(struct stg_module *mod, type_id res_type);
bool    msg_trigger_is_inst(struct vm *mod, type_id res_type);
type_id msg_trigger_return_type(struct vm *mod, type_id res_type);

#endif
