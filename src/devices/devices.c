#include "devices.h"

void register_device_types(struct stage *stage)
{
	register_device_type_constant(stage);
	register_device_type_ease(stage);
	register_device_type_add(stage);
	register_device_type_print(stage);
	register_device_type_tick(stage);
}
