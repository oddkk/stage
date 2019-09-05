COMPILE_JOB(discover_module, struct {
	struct string module_src_dir;
})

COMPILE_JOB(parse_file, struct {
	struct string file_name;
	struct ast_namespace *ns;
})

COMPILE_JOB(assign_stmt, struct {
	struct ast_namespace *ns;
	struct st_node *node;

	int scope_entry_id;
	bool initialized;
	struct ast_env env;
	struct ast_node *expr;
})
