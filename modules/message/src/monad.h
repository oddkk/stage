#ifndef STG_MESSAGE_MONAD_H
#define STG_MESSAGE_MONAD_H

#include <vm.h>
#include <module.h>

struct msg_system;

typedef void (*msg_monad_callback)(
		struct vm *vm, struct stg_exec *,
		void *data, void *out);

typedef void (*msg_monad_copy_func)(
		struct stg_exec *, void *data);

struct msg_monad_data {
	msg_monad_callback call;
	msg_monad_copy_func copy;
	void *data;
	size_t data_size;
};

struct msg_monad_type_info {
	type_id type;
};

type_id msg_register_type(struct stg_module *, type_id);
bool    msg_type_is_inst(struct vm *, type_id);
type_id msg_return_type(struct vm *, type_id);
void    msg_monad_copy(struct stg_exec *, struct msg_monad_data *);
void    msg_monad_call(struct vm *, struct stg_exec *,
		struct object, struct object *out);

#endif
