#ifndef STAGE_SCOPED_HASH_H
#define STAGE_SCOPED_HASH_H

#include "idlookuptable.h"

enum scope_entry_kind {
	SCOPE_ENTRY_NONE,
	SCOPE_ENTRY_NAMESPACE,
	SCOPE_ENTRY_TYPE,
	SCOPE_ENTRY_DEVICE,
	SCOPE_ENTRY_DEVICE_TYPE,
	SCOPE_ENTRY_DEVICE_CHANNEL,
	SCOPE_ENTRY_DEVICE_INPUT,
	SCOPE_ENTRY_DEVICE_OUTPUT,
	SCOPE_ENTRY_DEVICE_ATTRIBUTE,
};

struct scoped_hash;
struct config_node;

struct scope_entry {
	struct atom *name;
	enum scope_entry_kind kind;
	struct scoped_hash *scope;
	struct config_node *config_node;

	int id;
	int end;

	int type;
};

struct scoped_hash {
	struct scoped_hash *parent;
	struct scoped_hash *instance;

	struct id_lookup_table lookup;

	struct scope_entry *entries;
	size_t num_entries;

	struct scoped_hash *children;
	size_t num_children;

	bool array;
	enum scope_entry_kind kind;
	int id;
};

int scoped_hash_insert(struct scoped_hash *scope, struct atom *name,
		       enum scope_entry_kind kind, int id,
		       struct config_node *node,
		       struct scoped_hash *child_scope);

int scoped_hash_insert_ranged(struct scoped_hash *scope, struct atom *name,
							  enum scope_entry_kind kind, int id, int end,
							  struct config_node *node,
							  struct scoped_hash *child_scope);

int scoped_hash_insert_typed_ranged(struct scoped_hash *scope, struct atom *name,
									enum scope_entry_kind kind, int id, int end,
									int type, struct config_node *node,
									struct scoped_hash *child_scope);

struct scoped_hash *scoped_hash_namespace(struct scoped_hash *parent,
										  struct atom *name);

int scoped_hash_lookup(struct scoped_hash *scope, struct atom *name,
		       struct scope_entry *result);
int scoped_hash_lookup_owner(struct scoped_hash *scope, struct atom *name,
			     struct scope_entry *result,
			     struct scoped_hash **owner);
int scoped_hash_local_lookup(struct scoped_hash *scope, struct atom *name,
			     struct scope_entry *result);

int scoped_hash_lookup_index(struct scoped_hash *, size_t i, struct scope_entry *result);

struct scoped_hash *scoped_hash_push(struct scoped_hash *parent,
				     enum scope_entry_kind kind, int id);

void scoped_hash_print(struct scoped_hash *hash, int indent);

#endif
