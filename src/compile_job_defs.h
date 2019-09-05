COMPILE_JOB(discover_module, struct {
	struct string module_src_dir;
})

COMPILE_JOB(parse_file, struct {
	struct string file_name;
	struct ast_namespace *ns;
})
