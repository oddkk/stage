#ifndef STG_MESSAGE_TRIGGER_H
#define STG_MESSAGE_TRIGGER_H

#include <vm.h>
#include <module.h>
#include "message.h"

struct msg_trigger_data {
	msg_trigger_id trigger;
};

struct msg_trigger_type_info {
	type_id type;
};

type_id msg_trigger_register_type(struct stg_module *mod, type_id res_type);
bool    msg_trigger_is_inst(struct vm *mod, type_id res_type);
type_id msg_trigger_return_type(struct vm *mod, type_id res_type);

#endif
