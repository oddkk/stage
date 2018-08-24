#include "scope_lookup.h"
#include "stage.h"
#include "type.h"
#include "dlist.h"
#include "utils.h"
#include <stdio.h>

struct scope_lookup scope_lookup_init(struct stage *stage, struct scoped_hash *root_scope)
{
	struct scope_lookup lookup = {0};

	lookup.stage = stage;
	lookup.scope = root_scope;
	if (!lookup.scope) {
		lookup.scope = &lookup.stage->root_scope;
	}
	lookup.kind = lookup.scope->kind;
	lookup.owner = -1;

	return lookup;
}

static int scope_lookup_entry(struct scope_lookup *ctx, struct scope_entry entry)
{
	unsigned int offset = 0;
	unsigned int length = 0;
	struct type *next_type = NULL;

	if (ctx->num_steps > 0) {
		offset = ctx->steps[ctx->num_steps - 1].offset;
		length = ctx->steps[ctx->num_steps - 1].length;
	}

	// @TODO: Implement support for arrays of devices.

	if (entry.kind == SCOPE_ENTRY_DEVICE_CHANNEL ||
		entry.kind == SCOPE_ENTRY_DEVICE_ATTRIBUTE) {
		next_type = get_type(ctx->stage, entry.type);
		if (!next_type) {
			printf("Could not find type.\n");
			return -1;
		}

		offset = entry.id;
		length = next_type->num_scalars;
	}
	else if (entry.kind == SCOPE_ENTRY_TYPE) {
		offset = entry.id;
		length = 1;
		next_type = get_type(ctx->stage, ctx->stage->standard_types.type);
	}
	else {
		offset = entry.id;
		length = 1;
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
	if (step->repetitions <= 1) {
		step->repetitions = 1;
		step->stride = length;
	}

	ctx->kind  = entry.kind;
	ctx->owner = entry.owner;
	ctx->scope = entry.scope;

	// ctx->type -> next_type (implication)
	assert((ctx->type == NULL) || ((ctx->type != NULL) && (next_type != NULL)));
	ctx->type  = next_type;
	ctx->local_lookup = true;

	return 0;
}

int scope_lookup_ident(struct scope_lookup *ctx, struct atom *name)
{
	assert((ctx->scope != NULL) || (ctx->type != NULL));

	if (ctx->scope != NULL) {
		assert(ctx->scope);

		struct scope_entry entry;
		int err;

		if (ctx->local_lookup) {
			err = scoped_hash_local_lookup(ctx->scope, name, &entry);
		} else {
			err = scoped_hash_lookup(ctx->scope, name, &entry);
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
					printf("Tuple '%.*s' has no member '%.*s'.\n", ALIT(ctx->type->name), ALIT(name));
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
	assert((ctx->scope != NULL) || (ctx->type != NULL));

	if (ctx->scope != NULL) {
		assert(ctx->scope);

		struct scope_entry entry;
		int err;

		err = scoped_hash_lookup_index(ctx->scope, i, &entry);
		if (err) {
			printf("Has no index %zu.\n", i);
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
					printf("Invalid type.\n");
					return -1;
				}
				offset += member->num_scalars;
			}

			next_type = get_type(ctx->stage, ctx->type->tuple.types[i]);
			if (!next_type) {
				printf("Invalid type.\n");
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
					printf("Invalid type.\n");
					return -1;
				}
				offset += member->num_scalars;
			}

			next_type = get_type(ctx->stage, ctx->type->named_tuple.members[i].type);
			if (!next_type) {
				printf("Invalid type.\n");
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
				printf("Invalid type.\n");
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
	assert((ctx->scope != NULL) || (ctx->type != NULL));
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
				printf("Invalid type.\n");
				return -1;
			}

			offset = ctx->scope->id;
			length = type->num_scalars;
		}
		else {
			printf("Cannot take index of this.\n");
			return -1;
		}

		ctx->kind = ctx->scope->kind;

		if (ctx->scope->owner != -1) {
			ctx->owner = ctx->scope->owner;
		}
	}

	if (type->kind != TYPE_KIND_ARRAY) {
		printf("Cannot take range of non-array.\n");
		return -1;
	}

	struct type *member_type;

	member_type = get_type(ctx->stage, type->array.type);
	if (!member_type) {
		printf("Invalid type.\n");
		return -1;
	}

	if (end == SCOPE_LOOKUP_RANGE_END) {
		end = type->array.length;
	}

	if (end > type->array.length) {
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

	ctx->type = member_type;
	ctx->scope = NULL;
	ctx->local_lookup = true;

	return 0;
}

void print_steps(struct scope_lookup ctx)
{
	printf("off len rep str\n");
	for (size_t i = 0; i < ctx.num_steps; i++) {
		struct scope_lookup_step *step = &ctx.steps[i];

		printf("%3u %3u %3u %3u\n",
			   step->offset,
			   step->length,
			   step->repetitions,
			   step->stride);
	}
}

int scope_lookup_result_single(struct scope_lookup ctx, struct scope_lookup_range *result)
{
	//print_steps(ctx);
	if (ctx.num_steps != 1) {
		printf("Expected 1 step, got %zu\n", ctx.num_steps);
		return -1;
	}
	if (ctx.steps[0].length != 1 ||
		ctx.steps[0].repetitions != 1) {
		printf("Exptected single value, got %u (%u %u)\n.",
			   ctx.steps[0].length * ctx.steps[0].repetitions,
			   ctx.steps[0].length,  ctx.steps[0].repetitions);
		return -1;
	}

	result->begin = ctx.steps[0].offset;
	result->length = ctx.steps[0].length;
	result->owner = ctx.owner;
	result->kind = ctx.kind;
	result->type = ctx.type;
	return 0;
}

int scope_lookup_iterate(struct scope_lookup ctx, size_t *iter,
						 struct scope_lookup_range *out)
{
	size_t low, high, it, next_it;
	int owner;
	low = 0;
	high = -1;
	it = *iter;
	next_it = it;
	owner = -1;

	for (size_t i = 0; i < ctx.num_steps; i++) {
		struct scope_lookup_step *step = &ctx.steps[i];
		size_t rep = 0;

		low += step->offset;

		if (it >= low) {
			rep = (it - low) / step->stride;
		}

		/* printf("b %zu %zu %zu %zu\n", low, high, it, rep); */

		if (rep >= step->repetitions) {
			return LOOKUP_END;
		}

		low += rep * step->stride;

		if (low + step->length > high) {
			print_steps(ctx);
		}

		assert(low + step->length <= high);
		high = low + step->length;

		next_it = low + (step->stride);

		/* printf("a %zu %zu %zu %zu\n", low, high, it, rep); */
	}

	*iter = next_it;
	out->begin = low;
	out->length = high - low;
	if (owner == -1) {
		owner = ctx.owner;
	}
	out->kind = ctx.kind;
	out->owner = owner;
	out->type = ctx.type;
	return LOOKUP_FOUND;
}

size_t scope_lookup_instances(struct scope_lookup ctx)
{
	size_t instances = 1;
	for (size_t i = 0; i < ctx.num_steps; i++) {
		struct scope_lookup_step *step = &ctx.steps[i];
		instances *= step->repetitions;
	}
	return instances;
}

size_t scope_lookup_instance_size(struct scope_lookup ctx)
{
	return ctx.steps[ctx.num_steps - 1].length;
}

int eval_lookup_result(struct stage *stage, struct scope_lookup_range range, struct value_ref *out)
{
	switch (range.kind) {
	case SCOPE_ENTRY_NONE:
	case SCOPE_ENTRY_NAMESPACE:
		return -1;
		break;

	case SCOPE_ENTRY_DEVICE_TYPE:
	case SCOPE_ENTRY_DEVICE:
	case SCOPE_ENTRY_DEVICE_CHANNEL:
	case SCOPE_ENTRY_DEVICE_ATTRIBUTE:
		printf("@TODO: Implement result\n");
		return -1;

		// @TODO: Device arguments

	case SCOPE_ENTRY_TYPE:
		out->data[0] = range.begin;
		break;

	}

	return 0;
}

void describe_lookup_result_type(FILE *fp, struct scope_lookup ctx)
{
	if (ctx.type) {
		print_type(fp, ctx.stage, ctx.type);
	}
	for (int i = (int)ctx.num_steps - 1; i >= 0; i--) {
		struct scope_lookup_step *step = &ctx.steps[i];
		if (step->repetitions > 1) {
			fprintf(fp, "[%u]", step->repetitions);
		}
	}
}
