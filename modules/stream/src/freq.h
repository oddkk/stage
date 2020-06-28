#ifndef STAGE_STREAM_FREQ_H
#define STAGE_STREAM_FREQ_H

#include <stdint.h>

/*
enum freq_unit {
	FREQ_HZ = 0,
	FREQ_US = 1,
};
struct freq {
	enum freq_unit unit : 1;
	uint32_t value : 31;
};
*/

typedef uint32_t freq_t;

struct stg_module;
struct stg_native_module;

void
stream_register_freq_type(struct stg_module *);

void
stream_mod_load_freq_type(struct stg_native_module *mod);

#endif
