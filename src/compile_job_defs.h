COMPILE_JOB(load_module, struct {
	struct atom *module_name;
	struct string module_src_dir;

	struct stg_module *stg_mod;
	struct ast_module *mod;
	enum job_load_module_state state;
	int num_unparsed_files;
	struct stg_native_module *native_mod;

	struct ast_module **out_module;
})

COMPILE_JOB(parse_file, struct {
	struct ast_module *mod;
	struct string file_name;
	struct ast_namespace *ns;

	int *num_unparsed_files;
})
