#include "config.h"
/* #include "stage.h" */
/* #include "device_type.h" */
/* #include "device.h" */
#include "utils.h"
#include "dlist.h"
/* #include "scope_lookup.h" */
/* #include "access_pattern.h" */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define APPLY_DEBUG 0

#include "objstore.h"
#include "scope.h"

/* #include <ftw.h> */
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>

struct string cfg_node_names[] = {
#define CFG_NODE(name, data) STR(#name),
	CFG_NODES
#undef CFG_NODE
};

struct string cfg_job_names[] = {
#define CFG_JOB(name, data) STR(#name),
	CFG_JOBS
#undef CFG_JOB
};

#define CFG_JOB(name, data) typedef data job_##name##_t;
CFG_JOBS
#undef CFG_JOB

struct cfg_job {
	enum cfg_job_type type;

	struct cfg_job *next_job;

#define CFG_JOB(name, data) job_##name##_t name;
	union {
		CFG_JOBS
	};
#undef CFG_JOB
};

struct cfg_ctx {
	struct vm *vm;

	struct cfg_job *job_head;
	struct cfg_job *job_end;

	size_t num_errors;
};

static void dispatch_job(struct cfg_ctx *ctx, struct cfg_job job)
{
	struct cfg_job *new_job = calloc(1, sizeof(struct cfg_job));
	*new_job = job;
	new_job->next_job = NULL;

	if (ctx->job_end) {
		ctx->job_end->next_job = new_job;

		if (!ctx->job_head) {
			ctx->job_head = new_job;
		}
	} else {
		assert(!ctx->job_head);
		ctx->job_end = new_job;
		ctx->job_head = new_job;
	}
}

static void dispatch_stmt(struct cfg_ctx *ctx, struct cfg_node *stmt)
{
	switch (stmt->type) {
	case CFG_NODE_DECL_STMT: {
		/* struct cfg_job job = {0}; */
		/* job.type = CFG_JOB_ */
		/* dispatch_job(); */
	} break;

	case CFG_NODE_USE_STMT:
		break;

	case CFG_NODE_FUNC_STMT:
		break;

	case CFG_NODE_ASSIGN_STMT:
		break;

	case CFG_NODE_BIND:
		break;

	case CFG_NODE_NAMESPACE:
		break;

	default:
		printf("Got unexpected node '%.*s'.\n", LIT(cfg_node_names[stmt->type]));
		ctx->num_errors += 1;
		break;
	}
}

int parse_config_file(struct string filename, struct atom_table *table, struct arena *memory, struct cfg_node **out_node);

static int job_parse_file(struct cfg_ctx *ctx, job_parse_file_t *data)
{
	int err;
	struct cfg_node *result;

	printf("parse file '%.*s'\n", LIT(data->file_name));

	err = parse_config_file(data->file_name, &ctx->vm->atom_table, &ctx->vm->memory, &result);
	if (err) {
		return -1;
	}

	assert(result);

	struct cfg_job job = {0};

	job.type = CFG_JOB_visit_module;
	job.visit_module.root_scope = &ctx->vm->root_scope;
	job.visit_module.node = result;

	dispatch_job(ctx, job);

	return 0;
}

static int job_visit_module(struct cfg_ctx *ctx, job_visit_module_t *data)
{
	assert(data->node->type == CFG_NODE_MODULE);

	struct scope *mod_scope;

	mod_scope = scope_push(data->root_scope);

	struct cfg_node *stmt = data->node->MODULE.body;
	while (stmt) {
		dispatch_stmt(ctx, stmt);

		stmt = stmt->next_sibling;
	}

	return 0;
}

static bool has_extension(struct string str, struct string ext)
{
	if (str.length < ext.length + 1) {
		return false;
	}

	struct string file_ext;
	file_ext.length = ext.length;
	file_ext.text = (str.text + str.length) - ext.length;

	return string_equal(file_ext, ext);
}

static void discover_config_files(struct cfg_ctx *ctx, struct string cfg_dir)
{
	/* // TODO: Ensure zero-terminated */
	char *paths[] = {cfg_dir.text, NULL};

	FTS *ftsp;
	ftsp = fts_open(paths, FTS_PHYSICAL, NULL);

	FTSENT *f;
	while ((f = fts_read(ftsp)) != NULL) {
		switch (f->fts_info) {
		case FTS_F: {
			struct cfg_job job = {0};

			struct string path;
			path.text = f->fts_path;
			path.length = f->fts_pathlen;

			if (has_extension(path, STR(".stg"))) {
				job.type = CFG_JOB_parse_file;
				string_duplicate(&ctx->vm->memory, &job.parse_file.file_name, path);

				dispatch_job(ctx, job);
			}
		} break;
		}
	}

	if (errno != 0) {
		perror("fts");
	}

	fts_close(ftsp);
}

int cfg_compile(struct vm *vm, struct string cfg_dir)
{
	struct cfg_ctx ctx = {0};

	ctx.vm = vm;

	discover_config_files(&ctx, cfg_dir);

	while (ctx.job_head) {
		struct cfg_job *job = ctx.job_head;
		ctx.job_head = job->next_job;

		printf("Dispatching %.*s\n", LIT(cfg_job_names[job->type]));

		int err = -1;

		switch (job->type) {
#define CFG_JOB(name, data) case CFG_JOB_##name: err = job_##name(&ctx, &job->name); break;
	CFG_JOBS
#undef CFG_JOB

		case CFG_JOBS_LEN:
			assert(false);
			break;
		}

		if (err) {
			return -1;
		}
	}

	return 0;
}
