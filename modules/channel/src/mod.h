#ifndef STG_CHANNEL_MOD_H
#define STG_CHANNEL_MOD_H
#include "module.h"

typedef size_t channel_id;

struct cnl_channel_type_info {
	type_id type;
};

struct cnl_node {
	channel_id cnl;
	type_id type;
};

type_id
cnl_register_channel_type(struct stg_module *, type_id type);

#endif
