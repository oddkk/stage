#ifndef STAGE_TYPE_H
#define STAGE_TYPE_H

#include "intdef.h"
#include "string.h"
#include "atom.h"
#include "scoped_hash.h"
#include "access_pattern.h"
#include <limits.h>
#include <stdio.h>

typedef unsigned int type_id;

// #define TYPE_TEMPLATE ((type_id)-1)
#define TYPE_ARRAY_LENGTH_TEMPLATE ((size_t)-1)

typedef int scalar_value;
#define SCALAR_OFF INT_MIN
#define SCALAR_MIN INT_MIN + 1
#define SCALAR_MAX INT_MAX

enum type_kind {
	TYPE_KIND_NONE = 0,
	TYPE_KIND_TEMPLATE,
	TYPE_KIND_SCALAR,
	TYPE_KIND_STRING,
	TYPE_KIND_TYPE,
	TYPE_KIND_TUPLE,
	TYPE_KIND_NAMED_TUPLE,
	TYPE_KIND_ARRAY,
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
	size_t templated;
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
			bool length_templated;
			struct access_pattern length_template_field;
			size_t length_template_id;
			size_t length;
		} array;
		struct {
			struct access_pattern field;
			size_t id;
		} template;
	};
};

struct type_template_field {
	type_id expected_type;
	type_id type;
};

struct type_template_context {
	type_id type;
	struct type_template_field *fields;
	size_t num_fields;
	struct scoped_hash *scope;
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

struct value_ref {
	type_id type;
	scalar_value *data;
};

struct typed_value {
	struct type type;
	struct value value;
};

struct stage;

int type_count_scalars(struct stage *, struct type *type);

void print_type(FILE *fp, struct stage *, struct type *type);
void print_type_id(FILE *fp, struct stage *, type_id id);
void expand_type(FILE *fp, struct stage *, struct type *type, bool recurse_expand);
void expand_type_id(FILE *fp, struct stage *, type_id id, bool recurse_expand);

int register_type_name(struct stage *stage, type_id type, struct scoped_hash *scope, struct atom *name);

struct type *register_type(struct stage *stage, struct type def);
struct type *register_scalar_type(struct stage *stage, struct atom *name,
				  scalar_value min, scalar_value max);
struct type *register_tuple_type(struct stage *stage, struct atom *name,
				 type_id * subtypes, size_t num_subtypes);

struct type *register_named_tuple_type(struct stage *stage, struct atom *name,
				 struct named_tuple_member * members, size_t num_members);

struct type *register_array_type(struct stage *stage, struct atom *name,
								 type_id type, size_t length);

struct type *register_template_length_array_type(struct stage *stage,
												 struct atom *name,
												 type_id type,
												 struct access_pattern length_pattern,
												 struct type_template_context *);

struct type *register_template_type(struct stage *stage, struct atom *name,
									struct access_pattern field,
									struct type_template_context *);

struct type *register_template_type_str(struct stage *stage, struct atom *name,
										struct string pattern,
										struct type_template_context *);

int assign_value(struct value *dest, struct type *dest_type, struct value *src,
		 struct type *src_type);

void register_default_types(struct stage *stage);

void print_scalar(scalar_value val);

int register_typed_member_in_scope(struct stage *, struct atom *name,
								   type_id type, struct scoped_hash *scope,
								   enum scope_entry_kind kind, int start_id);

struct value_ref alloc_value(struct stage *, type_id);

struct type_iterator {
	type_id type;
	int subindex;
};

int type_find_member(struct type_iterator *out,
					 struct stage *stage,
					 struct type_iterator iter,
					 struct atom *name);

int type_find_by_pattern(struct stage *stage,
						 struct value_ref input,
						 struct access_pattern,
						 struct value_ref *result);

int consolidate_typed_value_into(struct stage *, type_id expected_type,
								 struct value_ref input, struct value_ref *result);

int resolve_templated_type(struct stage *stage, struct scoped_hash *scope,
						   type_id input, type_id *result);

int resolve_templated_type_value(struct stage *stage,
								 struct type_template_context expected,
								 struct value_ref input,
								 struct value_ref *result);

bool types_compatible(struct stage *, type_id t1, type_id t2);

/* int consolidate_typed_value_into_type(struct stage *, type_id expected_type, */
/* 									  struct value_ref input, struct value_ref *result); */

void print_typed_value(struct stage *, type_id tid, scalar_value *values, size_t num_values);
void print_value_ref(struct stage *, struct value_ref);

char *humanreadable_type_kind(enum type_kind);

#endif
