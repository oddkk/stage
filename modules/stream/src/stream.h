#ifndef STAGE_STREAM_STREAM_H
#define STAGE_STREAM_STREAM_H

#include <vm.h>
#include "freq.h"
#include "system.h"

struct stream_type_info {
	type_id type;
	freq_t freq;
};

struct stream_data {
	struct stream_node *node;
};

struct stg_module;
struct stg_native_module;

struct stream_data
stream_copy_stream_data(struct stg_exec *, struct stream_data);

void
stream_register_stream(struct stg_module *mod);

void
stream_mod_load_stream(struct stg_native_module *mod);

#endif
