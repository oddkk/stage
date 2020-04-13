#include "mod.h"
#include "../module.h"
#include <ffi.h>
#include <string.h>

static struct string
stg_string_obj_repr(struct vm *vm, struct arena *mem, struct object *obj)
{
	struct string str = *(struct string *)obj->data;
	return arena_sprintf(mem, "\"%.*s\"", LIT(str));
}

static void
stg_string_obj_copy(struct stg_exec *heap, void *type_data, void *obj_data)
{
	struct string *str = obj_data;
	char *new_text = stg_alloc(heap, str->length+1, sizeof(char));
	memcpy(new_text, str->text, str->length);
	new_text[str->length] = '\0';
	str->text = new_text;
}

static struct type_base stg_string_type_base = {
	.name     = STR("String"),
	.obj_repr = stg_string_obj_repr,
	.obj_copy = stg_string_obj_copy,
};

static ffi_type *ffi_type_stg_string_members[] = {
	&ffi_type_pointer,
	&ffi_type_uint64,
	NULL,
};

static ffi_type ffi_type_stg_string = {
	.size = 0,
	.alignment = 0,
	.type = FFI_TYPE_STRUCT,
	.elements = ffi_type_stg_string_members,
};

void
base_bootstrap_register_string(struct stg_module *mod)
{
	struct type str_type;
	str_type = init_plain_type(
			&stg_string_type_base,
			mod_atoms(mod, "String"),
			struct string);
	str_type.ffi_type = &ffi_type_stg_string;

	type_id tid;
	tid = stg_register_type(mod, str_type);
	mod->vm->default_types.string = tid;
}
