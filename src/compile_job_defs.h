COMPILE_JOB(load_module, struct {
	struct atom *module_name;
	struct string module_src_dir;

	struct stg_module *stg_mod;
	struct ast_module *mod;
	enum job_load_module_state state;
	int num_unparsed_files;
	int num_uncompiled_exprs;
	struct stg_native_module *native_mod;

	// This module struct is used in place of stg_mod->mod until stg_mod is
	// registered. We delay the registration of stg_mod until after all
	// dependencies have been resolved. Do not use _tmp_module after the module
	// is registered.
	struct ast_module _tmp_module;

	struct ast_module **out_module;
})

COMPILE_JOB(compile_expr, struct {
	struct ast_module *mod;
	struct ast_node *expr;
	ast_slot_id out_value;
	int *num_uncompiled_exprs;

	enum job_compile_expr_state state;
})

COMPILE_JOB(parse_file, struct {
	struct ast_module *mod;
	struct string file_name;
	struct ast_namespace *ns;

	int *num_unparsed_files;
})
