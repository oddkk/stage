#ifndef STAGE_STREAM_MOD_H
#define STAGE_STREAM_MOD_H

#include <vm.h>
#include "system.h"

struct stream_mod_info {
	struct object_cons *stream_cons;
	type_id freq;
	struct stream_system *sys;
};

struct stream_mod_info *
stream_mod_get_info(struct vm *);

#endif
