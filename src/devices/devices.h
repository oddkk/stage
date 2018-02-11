#ifndef STAGE_DEVICES_DEVICE_H
#define STAGE_DEVICES_DEVICE_H

struct stage *stage;
struct stage *device_type;

void register_device_types(struct stage *);

struct device_type *register_device_type_ease(struct stage *);
struct device_type *register_device_type_add(struct stage *);

#endif
