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
	struct expr expr;
})
