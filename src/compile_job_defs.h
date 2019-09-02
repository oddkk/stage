COMPILE_JOB(discover_module, struct {
	struct string module_src_dir;
})

COMPILE_JOB(parse_file, struct {
	struct string file_name;
	struct scope *mod_scope;
})

COMPILE_JOB(visit_stmt_list, struct {
	struct scope *scope;
	struct st_node *first_stmt;
})

COMPILE_JOB(use_stmt, struct {
	struct scope *scope;
	struct st_node *node;
})

COMPILE_JOB(visit_decl_stmt, struct {
	struct scope *scope;
	struct st_node *stmt;

	int scope_entry_id;
	bool initialized;
	struct expr expr;
})

COMPILE_JOB(assign_stmt, struct {
	struct scope *scope;
	struct st_node *node;

	int scope_entry_id;
	bool initialized;
	struct expr expr;
})

COMPILE_JOB(assert_stmt, struct {
	struct scope *scope;
	struct st_node *node;

	bool initialized;
	struct expr expr;
})

COMPILE_JOB(expr_stmt, struct {
	struct scope *scope;
	struct st_node *node;

	bool initialized;
	struct expr expr;
})

COMPILE_JOB(typecheck_expr, struct {
	struct expr *expr;
})
