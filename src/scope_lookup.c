#include "scope_lookup.h"
#include "stage.h"
#include "type.h"
#include "dlist.h"
#include "utils.h"
#include <stdio.h>

static int scope_lookup_entry(struct scope_lookup *ctx, struct scope_entry entry)
{
	unsigned int offset = 0;
	unsigned int length = 0;

	if (ctx->num_steps > 0) {
		offset = ctx->steps[ctx->num_steps - 1].offset;
		length = ctx->steps[ctx->num_steps - 1].length;
	}

	// @TODO: Implement support for arrays of devices.

	if (ctx->scope->kind == SCOPE_ENTRY_DEVICE_CHANNEL ||
		ctx->scope->kind == SCOPE_ENTRY_DEVICE_ATTRIBUTE) {
		struct type *type;
		type = get_type(ctx->stage, entry.type);
		if (!type) {
			return -1;
		}

		offset = entry.id;
		length = type->num_scalars;
	}
	else if (ctx->scope->kind == SCOPE_ENTRY_DEVICE) {
		printf("@TODO: Device array.\n");
	}
	else {
		printf("Cannot take index of this.\n");
		return -1;
	}

	struct scope_lookup_step *step;
	if (ctx->num_steps == 0) {
		struct scope_lookup_step tmp_step = {0};
		tmp_step.repetitions = 1;
		tmp_step.stride = 0;
		dlist_append(ctx->steps, ctx->num_steps, &tmp_step);
	}

	step = &ctx->steps[ctx->num_steps - 1];
	step->offset      = offset;
	step->length      = length;

	ctx->scope = entry.scope;
	ctx->local_lookup = true;

	return 0;
}

int scope_lookup_ident(struct scope_lookup *ctx, struct atom *name)
{
	assert((ctx->scope == NULL) != (ctx->type == NULL));

	if (ctx->scope != NULL) {
		assert(ctx->scope);

		struct scope_entry entry;
		int err;

		if (ctx->local_lookup) {
			err = scoped_hash_lookup(ctx->scope, name, &entry);
		} else {
			err = scoped_hash_local_lookup(ctx->scope, name, &entry);
		}

		if (err) {
			return -1;
		}

		return scope_lookup_entry(ctx, entry);
	}
	else if (ctx->type != NULL) {
		assert(ctx->type);

		struct type *next_type = NULL;
		unsigned int offset = 0;
		unsigned int length = 0;

		if (ctx->type->kind == TYPE_KIND_NAMED_TUPLE) {
			for (size_t mid = 0; mid < ctx->type->named_tuple.length; mid++) {
				struct type *member;
				member = get_type(ctx->stage, ctx->type->named_tuple.members[mid].type);
				if (!member) {
					return -1;
				}
				if (ctx->type->named_tuple.members[mid].name == name) {
					next_type = member;
					break;
				} else {
					offset += member->num_scalars;
				}
			}

			if (!next_type) {
				printf("Has no named member '%.*s'\n", ALIT(name));
				return -1;
			}
		}
		else {
			printf("Has no named members.\n");
			return -1;
		}

		length = next_type->num_scalars;

		struct scope_lookup_step *step;
		if (ctx->num_steps == 0) {
			dlist_alloc(ctx->steps, ctx->num_steps);
		}

		step = &ctx->steps[ctx->num_steps - 1];
		step->offset      = offset;
		step->length      = length;

		ctx->type = next_type;
	}

	return 0;
}

int scope_lookup_index(struct scope_lookup *ctx, size_t i)
{
	assert((ctx->scope == NULL) != (ctx->type == NULL));

	if (ctx->scope != NULL) {
		assert(ctx->scope);

		struct scope_entry entry;
		int err;

		err = scoped_hash_lookup_index(ctx->scope, i, &entry);
		if (err) {
			return -1;
		}

		return scope_lookup_entry(ctx, entry);
	}
	else if (ctx->type != NULL) {
		assert(ctx->type);

		struct type *next_type;
		unsigned int offset = 0;
		unsigned int length = 0;

		if (ctx->num_steps > 0) {
			offset = ctx->steps[ctx->num_steps - 1].offset;
			length = ctx->steps[ctx->num_steps - 1].length;
		}

		if (ctx->type->kind == TYPE_KIND_TUPLE) {
			if (i > ctx->type->tuple.length) {
				printf("Out of range of tuple.\n");
				return -1;
			}

			for (size_t mid = 0; mid < i; mid++) {
				struct type *member;
				member = get_type(ctx->stage, ctx->type->tuple.types[mid]);
				if (!member) {
					return -1;
				}
				offset += member->num_scalars;
			}

			next_type = get_type(ctx->stage, ctx->type->tuple.types[i]);
			if (!next_type) {
				return -1;
			}
		}
		else if (ctx->type->kind == TYPE_KIND_NAMED_TUPLE) {
			if (i > ctx->type->named_tuple.length) {
				printf("Out of range of named tuple.\n");
				return -1;
			}

			for (size_t mid = 0; mid < i; mid++) {
				struct type *member;
				member = get_type(ctx->stage, ctx->type->named_tuple.members[mid].type);
				if (!member) {
					return -1;
				}
				offset += member->num_scalars;
			}

			next_type = get_type(ctx->stage, ctx->type->named_tuple.members[i].type);
			if (!next_type) {
				return -1;
			}
		}
		else if (ctx->type->kind == TYPE_KIND_ARRAY) {
			if (i > ctx->type->array.length) {
				printf("Out of range of array.\n");
				return -1;
			}

			next_type = get_type(ctx->stage, ctx->type->array.type);
			if (!next_type) {
				return -1;
			}
			offset += next_type->num_scalars * i;
		} else {
			printf("Cannot take index of this.\n");
			return -1;
		}

		assert(length == 0 || next_type->num_scalars < length);
		length = next_type->num_scalars;

		struct scope_lookup_step *step;
		if (ctx->num_steps == 0) {
			dlist_alloc(ctx->steps, ctx->num_steps);
		}

		step = &ctx->steps[ctx->num_steps - 1];
		step->offset      = offset;
		step->length      = length;

		ctx->type = next_type;
	}

	return 0;
}

int scope_lookup_range(struct scope_lookup *ctx, size_t begin, size_t end)
{
	assert((ctx->scope == NULL) != (ctx->type == NULL));
	assert(begin < end);

	struct type *type;

	type = ctx->type;

	unsigned int offset = 0;
	unsigned int length = 0;

	if (ctx->num_steps > 0) {
		offset = ctx->steps[ctx->num_steps - 1].offset;
		length = ctx->steps[ctx->num_steps - 1].length;
	}

	if (ctx->scope != NULL) {

		if (!ctx->scope->array) {
			printf("Not an array.\n");
			return -1;
		}

		if (ctx->scope->kind == SCOPE_ENTRY_DEVICE_CHANNEL ||
			ctx->scope->kind == SCOPE_ENTRY_DEVICE_ATTRIBUTE) {
			type = get_type(ctx->stage, ctx->scope->parent->entries[ctx->scope->entry_id].type);
			if (!type) {
				return -1;
			}

			offset = ctx->scope->id;
			length = type->num_scalars;
		}
		else {
			printf("Cannot take index of this.\n");
			return -1;
		}
	}

	if (type->kind != TYPE_KIND_ARRAY) {
		return -1;
	}

	struct type *member_type;

	member_type = get_type(ctx->stage, type->array.type);
	if (!member_type) {
		return -1;
	}

	if (end >= type->array.length) {
		printf("The range is outside the array bounds.\n");
		return -1;
	}

	offset += begin * member_type->num_scalars;

	assert(length >= member_type->num_scalars);
	length = member_type->num_scalars;

	unsigned int repetitions = end - begin;
	unsigned int stride = member_type->num_scalars;

	struct scope_lookup_step *step;

	if (ctx->num_steps == 0) {
		dlist_alloc(ctx->steps, ctx->num_steps);
	}

	step = &ctx->steps[ctx->num_steps - 1];
	step->offset      = offset;
	step->length      = length;
	step->stride      = stride;
	step->repetitions = repetitions;

	dlist_alloc(ctx->steps, ctx->num_steps);
	step = &ctx->steps[ctx->num_steps - 1];
	step->offset = 0;
	step->length = length;
	step->stride = length;
	step->repetitions = 1;

	ctx->type = type;
	ctx->scope = NULL;
	ctx->local_lookup = true;

	return 0;
}
