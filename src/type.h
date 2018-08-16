#ifndef STAGE_TYPE_H
#define STAGE_TYPE_H

#include "intdef.h"
#include "string.h"
#include "atom.h"
#include "scoped_hash.h"
#include <limits.h>

typedef unsigned int type_id;

#define TYPE_TEMPLATE ((type_id)-1)

typedef int scalar_value;
#define SCALAR_OFF INT_MIN
#define SCALAR_MIN INT_MIN + 1
#define SCALAR_MAX INT_MAX

enum type_kind {
	TYPE_KIND_SCALAR = 0,
	TYPE_KIND_STRING = 1,
	TYPE_KIND_TYPE = 2,
	TYPE_KIND_TUPLE = 3,
	TYPE_KIND_NAMED_TUPLE = 4,
};

struct scalar_type {
	scalar_value min, max;
};

struct named_tuple_member {
	struct atom *name;
	type_id type;
};

struct type {
	type_id id;
	struct atom *name;
	int num_scalars;
	bool templated;
	enum type_kind kind;
	union {
		struct scalar_type scalar;
		struct {
			type_id *types;
			size_t length;
		} tuple;
		struct {
			struct named_tuple_member *members;
			size_t length;
		} named_tuple;
		struct {
			type_id type;
			size_t length;
		} array;
		struct {
			size_t id;
		} template;
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
void print_type_id(struct stage *, type_id id);
void expand_type(struct stage *, struct type *type, bool recurse_expand);
void expand_type_id(struct stage *, type_id id, bool recurse_expand);

int register_type_name(struct stage *stage, type_id type, struct scoped_hash *scope, struct atom *name);

struct type *register_type(struct stage *stage, struct type def);
struct type *register_scalar_type(struct stage *stage, struct atom *name,
				  scalar_value min, scalar_value max);
struct type *register_tuple_type(struct stage *stage, struct atom *name,
				 type_id * subtypes, size_t num_subtypes);

struct type *register_named_tuple_type(struct stage *stage, struct atom *name,
				 struct named_tuple_member * members, size_t num_members);

int assign_value(struct value *dest, struct type *dest_type, struct value *src,
		 struct type *src_type);

void register_default_types(struct stage *stage);

void print_scalar(scalar_value val);

#endif
