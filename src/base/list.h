#ifndef STAGE_LIST_H
#define STAGE_LIST_H

#include "../vm.h"

struct stg_list_type_info {
	type_id type;
};

struct stg_list_data;

typedef int (*stg_list_head_callback)(
		struct stg_exec *, struct stg_list_data *, void *out);
typedef struct stg_list_data (*stg_list_tail_callback)(
		struct stg_exec *, struct stg_list_data *);
typedef void (*stg_list_copy_callback)(
		struct stg_exec *, void *list_data);

struct stg_list_data {
	stg_list_head_callback head;
	stg_list_tail_callback tail;
	stg_list_copy_callback copy;
	void *data;
	size_t data_size;
	type_id element_type;
};

void
stg_list_copy(struct stg_exec *heap, struct stg_list_data *list);

type_id
stg_list_return_type(struct vm *, type_id);

bool
stg_list_type_is_inst(struct vm *, type_id);

#endif
