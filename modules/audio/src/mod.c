#include "mod.h"
#include <module.h>
#include <native.h>

int
mod_audio_load(struct stg_native_module *mod)
{
	return 0;
}

STAGE_MODULE(audio, mod_audio_load);
