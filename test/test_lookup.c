#include <assert.h>
#include "scope_lookup.h"
#include <stdio.h>

struct scope_lookup _init_from_steps(struct scope_lookup_step *steps, size_t length)
{
	struct scope_lookup result;
	result.steps = steps;
	result.num_steps = length;

	return result;
}
#define init_from_steps(steps) _init_from_steps(steps, sizeof(steps)/sizeof(steps[0]))

void test_single_offset() {
	struct scope_lookup lookup;

	struct scope_lookup_step steps[] = {
		{.offset=0, .length=3, .repetitions=1, .stride=3},
	};

	lookup = init_from_steps(steps);

	int err;
	size_t iter = 0;
	struct scope_lookup_range res;
	err = scope_lookup_iterate(lookup, &iter, &res);
	assert(err == LOOKUP_FOUND);
	assert(res.begin  == 0);
	assert(res.length == 3);

	err = scope_lookup_iterate(lookup, &iter, &res);
	assert(err == LOOKUP_END);
}

void test_repeated_single_offset() {
	struct scope_lookup lookup;

	struct scope_lookup_step steps[] = {
		{.offset=5, .length=2, .repetitions=2, .stride=3},
	};

	lookup = init_from_steps(steps);

	int err;
	size_t iter = 0;
	struct scope_lookup_range res;
	err = scope_lookup_iterate(lookup, &iter, &res);
	assert(err == LOOKUP_FOUND);
	printf("%zu %zu\n", res.begin, res.length);
	assert(res.begin  == 5);
	assert(res.length == 2);

	err = scope_lookup_iterate(lookup, &iter, &res);
	assert(err == LOOKUP_FOUND);
	printf("%zu %zu\n", res.begin, res.length);
	assert(res.begin  == 8);
	assert(res.length == 2);

	err = scope_lookup_iterate(lookup, &iter, &res);
	assert(err == LOOKUP_END);
}

void test_repeated_offset() {
	struct scope_lookup lookup;

	struct scope_lookup_step steps[] = {
		{.offset=5, .length=2, .repetitions=2, .stride=3},
		{.offset=5, .length=2, .repetitions=2, .stride=3},
	};

	lookup = init_from_steps(steps);

	int err;
	size_t iter = 0;
	struct scope_lookup_range res;
	err = scope_lookup_iterate(lookup, &iter, &res);
	assert(err == LOOKUP_FOUND);
	printf("%zu %zu\n", res.begin, res.length);
	assert(res.begin  == 5);
	assert(res.length == 2);

	err = scope_lookup_iterate(lookup, &iter, &res);
	assert(err == LOOKUP_FOUND);
	printf("%zu %zu\n", res.begin, res.length);
	assert(res.begin  == 8);
	assert(res.length == 2);

	err = scope_lookup_iterate(lookup, &iter, &res);
	assert(err == LOOKUP_END);
}


int main()
{
	test_single_offset();
	test_repeated_single_offset();
}
