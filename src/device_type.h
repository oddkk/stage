#ifndef STAGE_DEVICE_TYPE_H
#define STAGE_DEVICE_TYPE_H

#include "type.h"
#include "intdef.h"
#include "atom.h"
#include "scoped_hash.h"
/* #include "idlookuptable.h" */

typedef unsigned int device_type_id;

struct stage;
struct device;
struct device_type;
struct config_node;
typedef int (*device_init_callback) (struct stage *, struct device_type *,
				     struct device *);
typedef scalar_value(*device_output_eval_callback) (struct stage *,
						    struct device *,
						    struct device_type *,
						    int /* output id */ ,
						    int /* subindex */ );

struct device_channel_def {
	int id;
	struct atom *name;
	type_id type;
};

struct device_attribute_def {
	int id;
	struct atom *name;
	scalar_value def;
	type_id type;
};

struct device_type {
	device_type_id id;
	struct atom *name;

	struct device_attribute_def *attributes;
	size_t num_attributes;

	struct device_channel_def *inputs;
	size_t num_inputs;

	struct device_channel_def *outputs;
	size_t num_outputs;

	struct scoped_hash *scope;

	int self_input;
	int self_output;

	device_init_callback device_init;
	device_output_eval_callback eval;

	void *user_data;
};

struct device_attribute_def *device_type_add_attribute(struct stage *, struct device_type
						       *dev_type,
						       struct string name,
													   scalar_value def,
													   type_id type);
struct device_channel_def *device_type_add_input(struct stage *,
						 struct device_type *dev_type,
						 struct string name,
						 type_id type);
struct device_channel_def *device_type_add_output(struct stage *,
						  struct device_type *dev_type,
						  struct string name,
						  type_id type);

struct device_type *register_device_type(struct stage *stage,
					 struct string name);

struct device_type *register_device_type_scoped(struct stage *stage,
						struct string name,
						struct scoped_hash
						*parent_scope);

void describe_device_type(struct stage *stage, struct device_type *dev_type);

#endif
