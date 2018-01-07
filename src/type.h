#ifndef STAGE_TYPE_H
#define STAGE_TYPE_H

#include "intdef.h"
#include "string.h"
#include <limits.h>

typedef unsigned int type_id;

typedef int scalar_value;
#define SCALAR_OFF INT_MIN
#define SCALAR_MIN INT_MIN + 1
#define SCALAR_MAX INT_MAX

#define TYPE_KIND_SCALAR 0
#define TYPE_KIND_STRING 1
#define TYPE_KIND_TUPLE 2
//#define TYPE_KIND_ANY 3

struct scalar_type {
	scalar_value min, max;
};

struct type {
	type_id id;
	struct string name;
	int num_scalars;
	int kind;
	union {
		struct {
			//type_id type;
			scalar_value min, max;
		} scalar;
		struct {
			type_id *types;
			size_t num_types;
		} tuple;
		//TODO: Array type?
	};
};

#define VALUE_KIND_SCALAR 0

#define VALUE_INLINE_TUPLE 4

struct value {
	union {
		scalar_value scalar;
		scalar_value tuple[VALUE_INLINE_TUPLE];
		scalar_value *long_tuple;
		struct string string;
	};
};

struct typed_value {
	struct type type;
	struct value value;
};

struct stage;

int type_count_scalars(struct stage *, struct type *type);

void print_type(struct stage *, struct type *type);
void expand_type(struct stage *, struct type *type, bool recurse_expand);

struct type *register_type(struct stage *stage, struct type def);
struct type *register_scalar_type(struct stage *stage, struct string name,
				  scalar_value min, scalar_value max);
struct type *register_tuple_type(struct stage *stage, struct string name,
				 type_id * subtypes, size_t num_subtypes);

int assign_value(struct value *dest, struct type *dest_type, struct value *src,
		 struct type *src_type);

void register_default_types(struct stage *stage);

void print_scalar(scalar_value val);

#endif
