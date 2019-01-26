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
	bool initialized;
	struct expr expr;
})

CFG_JOB(typecheck_expr, struct {
	struct expr *expr;
})
