#ifndef STG_CHANNEL_MOD_H
#define STG_CHANNEL_MOD_H
#include "module.h"

typedef size_t channel_id;

struct cnl_node {
	channel_id cnl;
	type_id type;
};

#endif
