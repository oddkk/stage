#ifndef STAGE_DLIST_H
#define STAGE_DLIST_H

#include "intdef.h"

// Heap allocated dynamic list without preallocation.
// TODO: Replace all occurances of this with more specialized
// allocation schemes.
int _dlist_append(void **list, size_t * length, void *new_element,
		  size_t element_size);

#define dlist_append(list, list_len, new_element) _dlist_append((void**)&(list), &(list_len), new_element, sizeof(*(list)))

#endif
