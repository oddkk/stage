#ifndef STAGE_DEVICE_TYPE_H
#define STAGE_DEVICE_TYPE_H

#include "type.h"
#include "intdef.h"
#include "idlookuptable.h"

typedef unsigned int device_type_id;

struct stage;
struct device;
struct device_type;
struct config_node;
typedef void (*device_init_callback) (struct stage *, struct device *);
typedef scalar_value (*device_output_eval_callback) (struct stage *, struct device *,
											 struct device_type *, int /* output id */,
											 int /* subindex */);

struct device_input_def {
	int id;
	struct string name;
	type_id type;
};

struct device_output_def {
	int id;
	struct string name;
	type_id type;
};

struct device_attribute_def {
	int id;
	struct string name;
	type_id type;
	struct value def;
};

struct device_type {
	device_type_id id;
	struct string name;

	struct id_lookup_table attribute_ids;
	struct device_attribute_def *attributes;
	size_t num_attributes;

	struct id_lookup_table input_ids;
	struct device_input_def *inputs;
	size_t num_inputs;

	struct id_lookup_table output_ids;
	struct device_output_def *outputs;
	size_t num_outputs;

	device_init_callback device_init;
	device_output_eval_callback eval;

	void *user_data;
};

struct device_attribute_def *device_type_add_attribute(struct device_type
						       *dev_type,
						       struct string name,
						       type_id type,
						       struct value def);
struct device_input_def *device_type_add_input(struct device_type *dev_type,
					       struct string name,
					       type_id type);
struct device_output_def *device_type_add_output(struct device_type *dev_type,
						 struct string name,
						 type_id type);

struct stage;

struct device_type *register_device_type(struct stage *stage,
					 struct string name);

#endif
