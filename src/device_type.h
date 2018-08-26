#ifndef STAGE_DEVICE_TYPE_H
#define STAGE_DEVICE_TYPE_H

#include "type.h"
#include "intdef.h"
#include "atom.h"
#include "scoped_hash.h"
#include "channel.h"
/* #include "idlookuptable.h" */

typedef unsigned int device_type_id;

struct stage;
struct device;
struct device_type;
struct config_node;
typedef int (*device_init_callback) (struct stage *, struct device_type *,
				     struct device *);
typedef int (*device_init_context_callback) (struct stage *, struct device_type *,
											 struct device *, void *);
typedef scalar_value(*device_output_eval_callback) (struct stage *,
						    struct device *,
						    struct device_type *,
						    int /* output id */ ,
						    int /* subindex */ );

struct device_channel_def {
	int id;
	struct atom *name;
	struct type_template_context type;
	/* type_id type; */
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

	struct type_template_context params;

	struct device_channel_def *inputs;
	size_t num_inputs;

	struct device_channel_def *outputs;
	size_t num_outputs;

	struct scoped_hash *scope;

	int self_input;
	int self_output;

	bool takes_context;
	union {
		struct {
			device_init_callback device_template_init;
			device_init_callback device_init;
		};
		struct {
			device_init_context_callback device_context_template_init;
			device_init_context_callback device_context_init;
		};
	};

	device_output_eval_callback eval;

	bool finalized;
	void *user_data;
};

struct device_channel_def *device_type_add_input(struct stage *,
						 struct device_type *dev_type,
						 struct string name,
						 type_id type);

struct device_channel_def *device_type_add_output(struct stage *,
						  struct device_type *dev_type,
						  struct string name,
						  type_id type);

struct device_channel_def *device_type_add_input_template(struct stage *,
						  struct device_type *dev_type,
						  struct string name,
						  struct type_template_context type);

struct device_channel_def *device_type_add_output_template(struct stage *,
						  struct device_type *dev_type,
						  struct string name,
						  struct type_template_context type);

int device_type_get_input_id(struct stage *,
							 struct device_type *,
							 struct atom *name);

int device_type_get_output_id(struct stage *,
							  struct device_type *,
							  struct atom *name);

struct device_type *register_device_type_two_phase(struct stage *,
												   struct string name,
												   struct type_template_context params,
												   struct scoped_hash *parent_scope);
void finalize_device_type_two_phase(struct device_type *dev_type);

struct device_type_param {
	struct string name;
	type_id type;
	struct string template;
};

struct device_type_channel {
	enum device_channel_kind kind;
	struct string name;
	type_id type;
	struct string template;
	bool self;
};

#ifndef ARRAY_LENGTH
#define ARRAY_LENGTH(a) (sizeof(a) / sizeof(a[0]))
#endif

#define DEVICE_TYPE_DEF_CHANNELS(cnls) .channels={.channels=(cnls), .num_channels=ARRAY_LENGTH(cnls)}
#define DEVICE_TYPE_DEF_PARAMS(par) .params={.params=(par), .num_params=ARRAY_LENGTH(par)}

struct device_type_def {
	struct string name;

	device_init_callback template_init;
	device_init_callback init;

	device_init_context_callback context_template_init;
	device_init_context_callback context_init;

	struct {
		struct device_type_channel *channels;
		size_t                      num_channels;
	} channels;

	struct {
		struct device_type_param   *params;
		size_t                      num_params;
	} params;
};

struct device_type *register_device_type(struct stage *stage,
										 struct device_type_def);

void describe_device_type(struct stage *stage, struct device_type *dev_type);

#endif
