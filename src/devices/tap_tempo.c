#include "../stage.h"
#include "../device.h"
#include <stdlib.h>

#define TAP_TEMPO_MAX (NSEC * 2)
#define TAP_TEMPO_AVG_SAMPLES (1)

struct device_tap_tempo {
	channel_id channel_trigger;
	channel_id channel_out;

	scalar_value period;

	bool is_high;
	uint64_t last_down;

	uint64_t times[TAP_TEMPO_AVG_SAMPLES];
	size_t times_i;
	size_t times_limit;
};

static scalar_value device_tap_tempo_eval(struct stage *stage, channel_id cnl_id, struct channel *cnl)
{
	struct device *device;
	struct device_tap_tempo *data;

	device = get_device(stage, cnl->device.id);
	data = (struct device_tap_tempo *)device->data;

	return data->period;
}

static void device_tap_tempo_tick(struct stage *stage, struct device *dev)
{
	struct device_tap_tempo *data;
	scalar_value trigger;

	data = (struct device_tap_tempo *)dev->data;
	trigger = eval_channel(stage, data->channel_trigger);

	if (trigger > 0 && !data->is_high) {
		uint64_t delta = stage->tick - data->last_down;

		// Nano-seconds since last tick.
		uint64_t delta_time = delta * stage->tick_period;

		if (delta_time > TAP_TEMPO_MAX) {
			data->times_i = 0;
			data->times_limit = 0;
		} else {
			data->times[data->times_i] = delta_time;

			data->times_i += 1;

			if (data->times_i > data->times_limit) {
				data->times_limit = data->times_i;
			}

			data->times_i %= TAP_TEMPO_AVG_SAMPLES;

			uint64_t sum_times = 0;
			for (size_t i = 0; i < data->times_limit; i++) {
				/* printf("%zu ", data->times[i]); */
				sum_times += data->times[i];
			}

			uint64_t period = sum_times / data->times_limit;
			/* printf(": %zu  %zu\n", sum_times, period); */

			data->period = period / stage->tick_period;
		}

		data->last_down = stage->tick;
	}

	data->is_high = (trigger > 0);
}

static int device_tap_tempo_init(struct stage *stage, struct device_type *type, struct device *dev)
{
	struct device_tap_tempo *data = calloc(1, sizeof(struct device_tap_tempo));

	dev->data = data;

	data->channel_trigger
		= device_get_input_channel_id_by_name(stage, dev, STR("trigger"));
	data->channel_out
		= device_get_output_channel_id_by_name(stage, dev, STR("out"));

	register_device_tick_callback(stage, dev, 1, device_tap_tempo_tick);
	channel_bind_callback(stage, data->channel_out, device_tap_tempo_eval);

	return 0;
}

struct device_type *register_device_type_tap_tempo(struct stage *stage)
{
	type_id integer = stage->standard_types.integer;

	struct device_type_channel channels[] = {
		{ .kind=DEVICE_CHANNEL_INPUT, .name=STR("trigger"), .type=integer, .self=true },
		{ .kind=DEVICE_CHANNEL_OUTPUT, .name=STR("out"), .type=integer, .self=true },
	};

	struct device_type_def device = {
		.name = STR("basic.tap_tempo"),
		.init = device_tap_tempo_init,

		DEVICE_TYPE_DEF_CHANNELS(channels),
	};

	return register_device_type(stage, device);
}
