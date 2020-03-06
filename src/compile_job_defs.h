COMPILE_JOB(load_module, struct {
	struct atom *module_name;
	struct string module_src_dir;

	struct stg_module *stg_mod;
	struct ast_node *mod_root;

	enum job_load_module_state state;
	int num_unparsed_files;
})

COMPILE_JOB(parse_file, struct {
	struct stg_module *mod;
	struct string file_name;
	struct ast_node *scope;

	int *num_unparsed_files;
})
