#include "native.h"
#include "vm.h"
#include "utils.h"
#include <stdlib.h>
#include <dlfcn.h>

void
stg_native_register_func(struct stg_native_module *mod,
		struct string name, void *func, enum stg_native_func_flags flags)
{
	struct stg_native_func *tmp_funcs;
	size_t tmp_num_funcs;
	size_t func_id;

	func_id = mod->num_funcs;

	tmp_num_funcs = mod->num_funcs + 1;
	tmp_funcs = realloc(mod->funcs,
			tmp_num_funcs * sizeof(struct stg_native_func));

	if (!tmp_funcs) {
		panic("Failed to realloc native funcs list.");
		return;
	}

	mod->num_funcs = tmp_num_funcs;
	mod->funcs = tmp_funcs;

	mod->funcs[func_id].name  = name;
	mod->funcs[func_id].func  = func;
	mod->funcs[func_id].flags = flags;
}

void
stg_native_register_type(struct stg_native_module *mod,
		struct string name, struct type type)
{
	struct stg_native_type *tmp_types;
	size_t tmp_num_types;
	size_t type_id;

	type_id = mod->num_types;

	tmp_num_types = mod->num_types + 1;
	tmp_types = realloc(mod->types,
			tmp_num_types * sizeof(struct stg_native_type));

	if (!tmp_types) {
		panic("Failed to realloc native types list.");
		return;
	}

	mod->num_types = tmp_num_types;
	mod->types = tmp_types;

	mod->types[type_id].name = name;
	mod->types[type_id].type = type;
}

struct stg_native_module *
stg_native_load_module_ext(struct vm *vm, struct string name)
{
	struct stg_native_module *mod;

	char zero_term_name[name.length + 1];
	memcpy(zero_term_name, name.text, name.length);
	zero_term_name[name.length] = '\0';

	void *ext_handle = dlopen(zero_term_name, RTLD_LAZY | RTLD_LOCAL);
	if (ext_handle == NULL) {
		printf("Failed to open module extension '%.*s':\n%s\n",
				LIT(name), dlerror());
		return NULL;
	}

	stg_magic_func magic_fun;
	struct stg_module_magic *magic;

	magic_fun = (stg_magic_func)dlsym(
			ext_handle, STAGE_MODULE_MAGIC_FUNC_NAME_STR);
	if (magic_fun == NULL) {
		printf("Module extension '%.*s' does not contiain stage module magic:\n%s.\n",
				LIT(name), dlerror());
		dlclose(ext_handle);
		return NULL;
	}

	magic = magic_fun();
	if (!magic) {
		printf("Module extension '%.*s' magic function did not return a magic object.\n",
				LIT(name));
		dlclose(ext_handle);
		return NULL;
	}

	if (magic->magic_size != sizeof(struct stg_module_magic)) {
		printf("Module extension '%.*s' magic size does not match the magic size of the runtime.\n",
				LIT(name));
		dlclose(ext_handle);
		return NULL;
	}

	if (magic->name.length == 0 || magic->name.text == NULL) {
		printf("Module extension '%.*s' magic is missing a name.\n",
				LIT(name));
		dlclose(ext_handle);
		return NULL;
	}

	if (!magic->load) {
		printf("Module extension '%.*s' is missing an initialization procedure.\n",
				LIT(name));
		dlclose(ext_handle);
		return NULL;
	}

	mod = calloc(1, sizeof(struct stg_native_module));
	mod->dl_handle = ext_handle;
	mod->name = vm_atom(vm, magic->name);

	int err;
	err = magic->load(mod);
	if (err != 0) {
		printf("Module extension '%.*s' failed to initialize (returned %i).\n",
				LIT(name), err);
		dlclose(ext_handle);
		free(mod);
		return NULL;
	}

	return mod;
}

void
stg_native_module_destroy(struct stg_native_module *mod)
{
	if (mod->dl_handle) {
		dlclose(mod->dl_handle);
	}

	free(mod->funcs);
	free(mod->types);

	memset(mod, 0, sizeof(struct stg_native_module));
}
