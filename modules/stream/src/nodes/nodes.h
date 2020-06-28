#ifndef STAGE_STREAM_NODES_NODES_H
#define STAGE_STREAM_NODES_NODES_H

#include <bytecode.h>

#define STREAM_DEFAULT_NODES \
	NODE_DEF(const) \
	NODE_DEF(fmap) \
	NODE_DEF(debug_print)

struct arena;
struct stg_exec;
struct stg_module;
struct stg_native_module;

#define NODE_DEF(name) \
	void stream_mod_register_node_##name(struct stg_native_module *); \
	void stream_mod_init_node_##name(struct stg_module *);
	STREAM_DEFAULT_NODES
#undef NODE_DEF

struct stream_node_kind_decl {
	struct bc_result (*gen_bytecode)(struct bc_env *, struct arena *, void *data);
	void (*copy_node)(struct stg_exec *, void *new_data);
};

#define STREAM_NODE_KIND_DECL(_name, _decl, constr_func, constr_func_flags) \
	void \
	stream_mod_init_node_##_name(struct stg_module *mod) \
	{ \
		struct stream_node_kind_decl decl; \
		decl = (_decl); \
		struct stream_node_kind kind = {0}; \
		kind.name = mod_atom(mod, (struct string){ \
					.text=#_name, \
					.length=sizeof(#_name)-1, \
				}); \
		kind.gen_bytecode = decl.gen_bytecode; \
		kind.copy_node    = decl.copy_node;    \
		stream_register_node_kind(mod, kind); \
	} \
	void \
	stream_mod_register_node_##_name(struct stg_native_module *mod) \
	{ \
		stg_native_register_func(mod, \
				(struct string){ \
					.text=#constr_func, \
					.length=sizeof(#constr_func)-1}, \
				(void *)(constr_func), (constr_func_flags)); \
	}

#endif
