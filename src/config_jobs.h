CFG_JOB(parse_file, struct {
	struct string file_name;
	struct scope *mod_scope;
})

CFG_JOB(visit_stmt_list, struct {
	struct scope *scope;
	struct cfg_node *first_stmt;
})

CFG_JOB(visit_decl_stmt, struct {
	struct scope *scope;
	struct cfg_node *stmt;

	int scope_entry_id;
	bool initialized;
	type_id type;
	struct scope *child_scope;
})

CFG_JOB(enum_decl, struct {
	struct scope *scope;
	struct cfg_node *node;
	struct cfg_node *args;
	type_id *out_type;
	struct scope *child_scope;
})

CFG_JOB(obj_decl, struct {
	struct scope *scope;
	struct cfg_node *node;
	struct cfg_node *args;
	type_id *out_type;
})

CFG_JOB(tuple_decl, struct {
	struct scope *scope;
	struct cfg_node *node;
	struct cfg_node *args;
	type_id *out_type;

	enum cfg_tuple_decl_state state;
	union {
		struct type_tuple_item *named_items;
		type_id *unnamed_items;
	};
	type_id access_func_params;
	size_t num_items;
	struct cfg_node *next_node_to_resolve;
	size_t num_nodes_resolved;
})

CFG_JOB(func_proto_decl, struct {
	bool initialized;
	struct scope *scope;
	struct cfg_node *node;
	type_id params;
	type_id ret;
	type_id *out_type;
	enum cfg_func_proto_decl_state state;
})

CFG_JOB(func_decl, struct {
	struct scope *scope;
	struct cfg_node *node;

	int scope_entry_id;
	obj_id func_object;
	bool initialized;
	obj_id *out_object;
})

CFG_JOB(compile_func, struct {
	enum cfg_compile_func_state state;
	struct scope *scope;
	struct cfg_node *proto_node;
	struct cfg_node *body_node;
	obj_id *out_func_obj;
	type_id proto;
	struct cfg_func_context func_ctx;
	struct cfg_func_node *func;
})

CFG_JOB(visit_expr, struct {
	struct cfg_func_context *func_ctx;
	struct cfg_node *node;
	struct cfg_func_node **out_func;
	struct scope *local_scope;

	struct cfg_func_node *tmp_func;
	unsigned int iter;
})

CFG_JOB(resolve_type_l_expr, struct {
	struct scope *scope;
	struct cfg_node *node;

	bool dispatched;
	struct cfg_func_context func_ctx;
	struct cfg_func_node *func;
	/* struct object obj; */
	struct scope_entry entry;
	type_id *out_type;
})
