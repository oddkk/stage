#include "stage.h"
#include "channel.h"
#include "device.h"
#include "device_type.h"
#include "utils.h"
#include "config.h"

#include <stdio.h>
#include <stdlib.h>

scalar_value device_add_eval(struct stage *stage, struct device *device, struct device_type *type, int output, int output_subindex) {
	scalar_value lhs, rhs;

	lhs = eval_channel(stage, device->input_begin);
	rhs = eval_channel(stage, device->input_begin + 1);

	return lhs + rhs;
}

int main(int argc, char *argv[])
{
	int err;
	struct stage stage;

	err = stage_init(&stage);
	if (err) {
		return err;
	}

	register_default_types(&stage);

	struct device_type *device_add;
	device_add = register_device_type(&stage, STR("add"));
	device_add->eval = device_add_eval;

	device_type_add_attribute(&stage, device_add, STR("foo_attr"),
							  stage.standard_types.integer, (struct value){.scalar=SCALAR_OFF});
	device_type_add_attribute(&stage, device_add, STR("bar_attr"),
							  stage.standard_types.integer, (struct value){.scalar=3});

	device_type_add_input(&stage, device_add, STR("left"),
	                   stage.standard_types.integer);
	device_type_add_input(&stage, device_add, STR("right"),
	                   stage.standard_types.integer);

	device_type_add_output(&stage, device_add, STR("out"),
	                    stage.standard_types.integer);
	device_type_add_output(&stage, device_add, STR("out2"),
	                    stage.standard_types.integer);

	describe_device_type(&stage, device_add);
	printf("\n");

	struct device *dev_test, *dev_test2;

	dev_test = register_device(&stage, device_add->id, NULL, 0);
	dev_test->name = atom_create(&stage.atom_table, STR("hello"));

	dev_test2 = register_device(&stage, device_add->id, NULL, 0);
	dev_test2->name = atom_create(&stage.atom_table, STR("hello2"));

	describe_device(&stage, dev_test);
	printf("\n");
	describe_device(&stage, dev_test2);

	dependency_matrix_bind(&stage.channel_deps, 0, 1);
	dependency_matrix_print(&stage.channel_deps);

	return 0;

	/* parse_config_file(STR("config/main.conf"), &stage.atom_table, &stage.memory, &node); */

	/* apply_config(&stage, node); */

	/* config_print_tree(node); */

	/* printf("config_node: %lu\n", sizeof(struct config_node)); */

	/* return 0; */
}
