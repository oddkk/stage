#include "freq.h"
#include "mod.h"
#include <module.h>
#include <native.h>

#include <ffi.h>

static struct type_base stream_freq_type_base = {
	.name = STR("Freq"),
};

void
stream_register_freq_type(struct stg_module *mod)
{
	struct type freq_type = {0};

	freq_type = init_plain_type(
			&stream_freq_type_base,
			mod_atoms(mod, "Freq"),
			uint32_t);

	freq_type.ffi_type = &ffi_type_uint32;

	type_id tid;
	tid = stg_register_type(mod, freq_type);

	stg_mod_register_native_type(mod,
			mod_atoms(mod, "Freq"), tid);

	struct stream_mod_info *mod_info;
	mod_info = mod->data;

	mod_info->freq = tid;
}

static freq_t
stream_freq_from_hz(int64_t f)
{
	assert(f < UINT32_MAX);
	return f;
}

void
stream_mod_load_freq_type(struct stg_native_module *mod)
{
	stg_native_register_funcs(mod,
			stream_freq_from_hz, 0);
}
