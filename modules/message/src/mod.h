#ifndef STG_MESSAGE_MOD_H
#define STG_MESSAGE_MOD_H

#define MOD_MESSAGE "message"
#include <module.h>
#include "message.h"

struct msg_type_info {
	type_id type;
};

type_id
msg_register_msg_type(struct stg_module *mod, type_id msg_type);

typedef msg_node_id (*msg_functor_callback)(
		struct vm *, struct msg_system *, void *data);

typedef void (*msg_functor_copy)(
		struct stg_exec *, void *data);

struct msg_functor_data {
	msg_functor_callback call;
	msg_functor_copy copy;
	void *data;
	size_t data_size;
};

msg_node_id
msg_call_functor(struct vm *vm, struct msg_system *sys,
		struct msg_functor_data msg);

struct msg_functor_data
msg_copy_functor(struct stg_exec *, struct msg_functor_data);


#endif
