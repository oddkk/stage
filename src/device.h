#ifndef STAGE_DEVICE_H
#define STAGE_DEVICE_H

#include "device_type.h"
#include "channel.h"
#include "type.h"

typedef unsigned int device_id;

struct attribute_value {
	type_id type;
	struct value value;
};

struct device {
	device_id id;
	device_type_id type;
	channel_id input_begin;
	channel_id output_begin;
	struct string name;
	struct attribute_value *attributes;
};

struct stage;

struct device_attribute {
	struct string name;
	struct type type;
	struct value value;
};

struct device *register_device(struct stage *stage, device_type_id type,
			       struct device_attribute *attributes,
			       size_t num_attributes);

struct attribute_value *device_get_attr(struct stage *stage, struct device *device, struct string attr_name);

#endif
