#ifndef STAGE_CONFIG_H
#define STAGE_CONFIG_H

#include "vm.h"
#include "ast.h"
#include "module.h"
#include "errors.h"

enum complie_job_type {
#define COMPILE_JOB(name, data) COMPILE_JOB_##name,
	#include "compile_job_defs.h"
#undef COMPILE_JOB
	COMPILE_JOBS_LEN
};

extern struct string complie_job_names[COMPILE_JOBS_LEN];

struct stg_compile_options {
	struct string *module_locations;
	size_t num_module_locations;
};

int stg_compile(struct vm *vm, struct ast_context *,
		struct stg_compile_options, struct string src_dir);

#endif
