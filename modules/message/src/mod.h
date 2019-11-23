#ifndef STG_MESSAGE_MOD_H
#define STG_MESSAGE_MOD_H

#define MOD_MESSAGE "message"
#include <module.h>

struct msg_message_type_info {
	type_id type;
};

type_id
msg_register_message_type(struct stg_module *mod, type_id msg_type);

#endif
