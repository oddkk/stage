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

#endif
