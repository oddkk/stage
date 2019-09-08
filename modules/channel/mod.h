#ifndef STG_CHANNEL_MOD_H
#define STG_CHANNEL_MOD_H
#include "../../module.h"

typedef size_t channel_id;

struct cnl_node {
	channel_id cnl;
	type_id type;
};

#define MOD_CHANNEL "channel"

#define CNL_NODE DEF_TYPE(MOD_CHANNEL, "node", struct cnl_node)

struct cnl_channel {
	struct cnl_node src, drain;
};

#define CNL_CHANNEL DEF_TYPE(MOD_CHANNEL, "channel", struct cnl_channel)

#endif
