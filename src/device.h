#ifndef STAGE_DEVICE_H
#define STAGE_DEVICE_H

#include "device_type.h"
#include "channel.h"
#include "type.h"

typedef unsigned int device_id;

struct attribute_value {
	scalar_value value;
};

struct device {
	device_id id;
	device_type_id type;
	channel_id input_begin;
	channel_id output_begin;
	void *data;
	struct atom *name;
	struct attribute_value *attributes;
	struct scoped_hash *scope;
};

struct stage;

struct device_attribute {
	struct atom *name;
	scalar_value value;
};

struct device *register_device_scoped(struct stage *stage, device_type_id type,
				      struct atom *name,
				      struct scoped_hash *parent_scope,
				      struct device_attribute *attributes,
				      size_t num_attributes, void *data);

struct device *register_device(struct stage *stage, device_type_id type,
			       struct atom *name,
			       struct device_attribute *attributes,
			       size_t num_attributes, void *data);

struct attribute_value *device_get_attr(struct stage *stage,
					struct device *device,
					struct atom *attr_name);

channel_id device_get_channel_by_name(struct stage *stage,
				      struct device *device, struct atom *cnl);
channel_id device_get_input_channel_id(struct stage *stage,
				       struct device *device,
				       struct atom *name);
channel_id device_get_output_channel_id(struct stage *stage,
					struct device *device,
					struct atom *name);

channel_id device_get_input_channel_id_by_name(struct stage *stage,
					       struct device *device,
					       struct string name);
channel_id device_get_output_channel_id_by_name(struct stage *stage,
						struct device *device,
						struct string name);

void describe_device(struct stage *stage, struct device *dev);

#endif
