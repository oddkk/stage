#include "init_monad.h"
#include "dlist.h"

void
stg_init_register_entry(struct stg_init_context *ctx,
		struct atom *name, void *data)
{
	for (size_t i = 0; i < ctx->num_entries; i++) {
		if (ctx->entries[i].name == name) {
			panic("Init context name '%.*s' registered multiple times.");
			ctx->entries[i].data = data;
			return;
		}
	}

	struct stg_init_context_entry entry = {0};
	entry.name = name;
	entry.data = data;

	dlist_append(
			ctx->entries,
			ctx->num_entries,
			&entry);
}

void *
stg_init_get_entry(struct stg_init_context *ctx, struct atom *name)
{
	for (size_t i = 0; i < ctx->num_entries; i++) {
		if (ctx->entries[i].name == name) {
			return ctx->entries[i].data;
		}
	}

	return NULL;
}

