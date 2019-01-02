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
	bool initialized;
	type_id type;
	struct scope *child_scope;
})

CFG_JOB(enum_decl, struct {
	struct scope *scope;
	struct cfg_node *node;
	struct cfg_node *args;
	type_id *out_type;
	struct scope **out_child_scope;
})

CFG_JOB(obj_decl, struct {
	struct scope *scope;
	struct cfg_node *node;
	struct cfg_node *args;
	type_id *out_type;
	struct scope **out_child_scope;
})

CFG_JOB(tuple_decl, struct {
	struct scope *scope;
	struct cfg_node *node;
	struct cfg_node *args;
	type_id *out_type;
	enum cfg_tuple_decl_state state;
	struct type_tuple_item *items;
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
	obj_id func_object;
	bool initialized;
	obj_id *out_object;
	struct scope **out_child_scope;
})

CFG_JOB(compile_func, struct {
	enum cfg_compile_func_state state;
	struct scope *scope;
	struct cfg_node *proto_node;
	struct cfg_node *body_node;
	obj_id *out_func_obj;
	type_id proto;
})

CFG_JOB(resolve_type_l_expr, struct {
	struct scope *scope;
	struct cfg_node *node;

	bool dispatched;
	struct scope_entry entry;
	type_id *out_type;
})

CFG_JOB(resolve_l_expr, struct {
	struct scope *scope;
	struct cfg_node *node;

	bool local;
	bool rhs;
	enum cfg_binop_state state;
	struct scope_entry entry;
	struct scope_entry *out_entry;
	struct cfg_node *l_expr_top_node;
})
