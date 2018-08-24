#include <assert.h>
#include "type.h"

void test_consolidate_typed_value_into_type()
{
	struct stage *stage;
	int err = stage_init(stage);
	assert(!err);

	struct named_tuple_members members[] = {
		{}
	};
}

int main()
{
	test_consolidate_typed_value_into_type();
}
