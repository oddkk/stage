// Provides a basic single type parameter wrapping type and type constructor.
//
// The template mainly provides the following functions:
//
// type_id CNAME_register_type(struct stg_module *mod, type_id res_type)
// Creates a new type with the given type as its member.
//
// struct object_cons *CNAME_register_cons(struct stg_module *mod)
// Creates the monad's type constructor. The constructor should be created
// during the register hook. The caller is responsible for storing the returned
// cons in a way such that it can be retreived through CNAME_cons_from_vm.
//
// bool CNAME_type_is_inst(struct vm *, type_id tid)
// Returns true if the given type is an instance of this monad.
//
// type_id CNAME_return_type(struct vm *, type_id tid)
// Returns the type of the monad type's member type.

// A prefix applied to all members of this template.
#ifndef CNAME
#error "CNAME macro must be defined when importing the native monad template."
#endif

// REAL_NAME should be the name of the monad used in the language.
#ifndef REAL_NAME
#error "REAL_NAME must be defined when importing the native monad template."
#endif

#ifndef UWT_OBJ_COPY_FUNC
#error "UWT_OBJ_COPY_FUNC macro must be defined when importing the native monad template"
#endif

#ifndef UWT_OBJ_DATA_TYPE
#error "UWT_OBJ_DATA_TYPE macro must be defined when importing the native monad template"
#endif

#ifndef UWT_OBJ_FFI_TYPE
#error "UWT_OBJ_FFI_TYPE macro must be defined when importing the native monad template"
#endif

// The datatype named to by UWT_TYPE_INFO_TYPE is expected to contain a member
// `type_id type` and will be used as the type's data.
#ifndef UWT_TYPE_INFO_TYPE
#error "UWT_TYPE_INFO_TYPE must be defined when importing the native monad template."
#endif

#ifdef EXPOSE_FUNCS
#define _EXPOSE_FUNCS EXPOSE_FUNCS
#else
#define _EXPOSE_FUNCS 0
#endif

#define UWT_FUNC_CONCAT1(prefix, suffix) prefix ## _ ## suffix
#define UWT_FUNC_CONCAT(prefix, suffix) UWT_FUNC_CONCAT1(prefix, suffix)
#define UWT_FUNC(name) UWT_FUNC_CONCAT(CNAME, name)

#if EXPOSE_FUNCS
#define UWT_FUNC_EXPOSED
#else
#define UWT_FUNC_EXPOSED static
#endif


// The caller is expected to have defined this function to return this monad's
// type cons.
static struct object_cons *
UWT_FUNC(cons_from_vm)(struct vm *vm);

static bool
UWT_FUNC(type_equals)(struct vm *vm, struct type *lhs, struct type *rhs)
{
	UWT_TYPE_INFO_TYPE *lhs_info;
	UWT_TYPE_INFO_TYPE *rhs_info;

	lhs_info = (UWT_TYPE_INFO_TYPE *)lhs->data;
	rhs_info = (UWT_TYPE_INFO_TYPE *)rhs->data;

	return type_equals(vm, lhs_info->type, rhs_info->type);
}

static struct string
UWT_FUNC(type_repr)(struct vm *vm, struct arena *mem, struct type *type)
{
	UWT_TYPE_INFO_TYPE *info;
	info = (UWT_TYPE_INFO_TYPE *)type->data;

	struct string res = {0};
	arena_string_append(mem, &res, STR(REAL_NAME "["));

	struct type *item_type;
	item_type = vm_get_type(vm, info->type);
	arena_string_append_type_repr(&res, vm, mem, item_type);

	arena_string_append(mem, &res, STR("]"));

	return res;
}

static struct type_base UWT_FUNC(type_base) = {
	.name = STR(REAL_NAME),
	.equals = UWT_FUNC(type_equals),
	.repr = UWT_FUNC(type_repr),
	.obj_copy = UWT_OBJ_COPY_FUNC,
};

UWT_FUNC_EXPOSED type_id
UWT_FUNC(register_type)(struct stg_module *mod, type_id res_type);

static int
UWT_FUNC(type_pack)(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, void **params, size_t num_params)
{
	assert(num_params == 1);

	type_id tid = *(type_id *)params[0];

	type_id result_type;
	result_type = UWT_FUNC(register_type)(
			mod, tid);

	memcpy(out, &result_type, sizeof(type_id));

	return 0;
}

static type_id
UWT_FUNC(type_pack_type)(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void **params, size_t num_params)
{
	return ctx->vm->default_types.type;
}

int
UWT_FUNC(type_unpack)(
		struct ast_context *ctx, struct stg_module *mod,
		void *data, void *out, struct object obj, int param_id)
{
	assert_type_equals(ctx->vm,
			obj.type, ctx->vm->default_types.type);

	type_id tid = *(type_id *)obj.data;

	struct type *type;
	type = vm_get_type(ctx->vm, tid);
	// TODO: Properly report type mismatch error.
	if (type->base != &UWT_FUNC(type_base)) {
		stg_error(ctx->err, STG_NO_LOC,
				"Expected " REAL_NAME " type, got %.*s.",
				LIT(type->base->name));
		return -1;
	}

	UWT_TYPE_INFO_TYPE *info = type->data;
	memcpy(out, &info->type, sizeof(type_id));

	return 0;
}

UWT_FUNC_EXPOSED type_id
UWT_FUNC(register_type)(struct stg_module *mod, type_id res_type)
{
	UWT_TYPE_INFO_TYPE *info;
	info = arena_alloc(&mod->mem, sizeof(UWT_TYPE_INFO_TYPE));

	info->type = res_type;

	struct type type = {0};
	type.base = &UWT_FUNC(type_base);
	type.data = info;
	type.size = sizeof(UWT_OBJ_DATA_TYPE);
	type.type_def = UWT_FUNC(cons_from_vm)(mod->vm);
	type.ffi_type = &UWT_FUNC(ffi_type);

	return stg_register_type(mod, type);
}

static struct object_cons *
UWT_FUNC(register_cons)(struct stg_module *mod)
{
	struct object_cons *cons;
	cons = arena_alloc(&mod->mem,
			sizeof(struct object_cons));

	cons->num_params = 1;
	cons->params = arena_allocn(&mod->mem,
			cons->num_params, sizeof(struct object_cons_param));

	cons->params[0].name = mod_atoms(mod, "T");
	cons->params[0].type = mod->vm->default_types.type;

	cons->ct_pack      = UWT_FUNC(type_pack);
	cons->ct_pack_type = UWT_FUNC(type_pack_type);
	cons->ct_unpack    = UWT_FUNC(type_unpack);

	return cons;
}

UWT_FUNC_EXPOSED bool
UWT_FUNC(type_is_inst)(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	return type->base == &UWT_FUNC(type_base);
}

UWT_FUNC_EXPOSED type_id
UWT_FUNC(return_type)(struct vm *vm, type_id tid)
{
	struct type *type;
	type = vm_get_type(vm, tid);
	assert(type->base == &UWT_FUNC(type_base));

	UWT_TYPE_INFO_TYPE *info;
	info = type->data;

	return info->type;
}

#undef UWT_FUNC_CONCAT1
#undef UWT_FUNC_CONCAT
#undef UWT_FUNC
#undef UWT_FUNC_EXPOSED
#undef EXPOSE_FUNCS
