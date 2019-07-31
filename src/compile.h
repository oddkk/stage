#ifndef STAGE_CONFIG_H
#define STAGE_CONFIG_H

#include "vm.h"
#include "module.h"
#include "errors.h"

enum cfg_job_type {
#define CFG_JOB(name, data) CFG_JOB_##name,
	#include "compile_job_defs.h"
#undef CFG_JOB
	CFG_JOBS_LEN
};

extern struct string cfg_job_names[CFG_JOBS_LEN];

int cfg_compile(struct vm *vm, struct string cfg_dir);

#endif
