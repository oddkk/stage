#include "devices.h"

void register_device_types(struct stage *stage)
{
	register_device_type_constant(stage);
	register_device_type_ease(stage);
	register_device_type_add(stage);
	register_device_type_print(stage);
	register_device_type_tick(stage);
	register_device_type_toggle(stage);
	register_device_type_midi(stage);
	register_device_type_blink(stage);
	register_device_type_tap_tempo(stage);
	register_device_type_mux(stage);
	register_device_type_demux(stage);
	register_device_type_radio(stage);
	register_device_type_dmx(stage);
}
