#include "dlist.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

int _dlist_append(void **list, size_t * length, void *new_element,
		  size_t element_size)
{
	unsigned char *new_list =
	    realloc(*list, ((*length) + 1) * element_size);
	size_t id = *length;
	if (!new_list) {
		return -1;
	}
	*list = new_list;

	if (new_element) {
		memcpy(new_list + (id * element_size), new_element,
		       element_size);
	} else {
		memset(new_list + (id * element_size), 0, element_size);
	}

	*length += 1;

	return id;
}
