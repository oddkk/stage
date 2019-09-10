#ifndef STAGE_BASE_MOD_H
#define STAGE_BASE_MOD_H

#include "../vm.h"

void
stg_base_load(struct vm *vm);

void
base_register_type(struct stg_module *mod);

#endif
