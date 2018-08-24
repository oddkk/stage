#ifndef STAGE_DEVICE_H
#define STAGE_DEVICE_H

#include "device_type.h"
#include "channel.h"
#include "type.h"
#include "scope_lookup.h"

typedef unsigned int device_id;

struct device {
	device_id id;
	device_type_id type;
	channel_id input_begin;
	channel_id output_begin;
	type_id *input_types;
	type_id *output_types;

	void *data;
	struct atom *name;
	struct value_ref args;
	struct scoped_hash *scope;
};

struct stage;

struct device *register_device(struct stage *stage, device_type_id type,
							   struct scoped_hash *parent_scope,
							   struct atom *name,
							   struct value_ref args);

struct device *register_device_with_context(struct stage *stage, device_type_id type,
											struct scoped_hash *parent_scope,
											struct atom *name,
											struct value_ref args,
											void *context);

int device_assign_input_type_by_name(struct stage *stage,
									 struct device *dev,
									 struct atom *name,
									 type_id type);

struct scope_lookup device_lookup(struct stage *, struct device *);

struct value_ref device_get_attr_from_lookup(struct stage *, struct device *,
											 struct scope_lookup_range);

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

int device_assign_input_by_name(struct stage *stage,
								struct device *dev,
								struct string name,
								type_id type);

void describe_device(struct stage *stage, struct device *dev);

#endif
