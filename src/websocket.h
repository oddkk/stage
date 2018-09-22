#ifndef STAGE_WEBSOCKET_H
#define STAGE_WEBSOCKET_H

#include "net.h"

struct websocket_context {
	struct net_context net;
};

int websocket_init(struct websocket_context *ctx, char *node, char *service);

#endif
