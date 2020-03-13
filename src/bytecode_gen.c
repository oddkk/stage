#include "bytecode.h"
#include "vm.h"
#include "dlist.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

static struct bc_instr *
bc_instr_alloc(struct bc_instr_store *store, struct bc_instr instr)
{
	if (store->page_size == 0) {
		store->page_size = sysconf(_SC_PAGESIZE);
		store->instr_per_page = store->page_size / sizeof(struct bc_instr);
	}

	if (store->last_page_num_used == 0 ||
			store->last_page_num_used >= store->instr_per_page) {
		size_t new_num_pages = store->num_pages + 1;
		struct bc_instr **new_pages = realloc(
				store->pages, new_num_pages * sizeof(struct bc_instr *));
		if (!new_pages) {
			perror("realloc");
			return NULL;
		}

		store->pages = new_pages;

		store->pages[store->num_pages] = mmap(
				NULL, store->page_size,
				PROT_READ|PROT_WRITE,
				MAP_PRIVATE|MAP_ANONYMOUS,
				-1, 0);

		if (store->pages[store->num_pages] == MAP_FAILED) {
			perror("mmap");
			return NULL;
		}

		store->num_pages = new_num_pages;
		store->last_page_num_used = 0;
	}

	struct bc_instr *res;

	res = &store->pages[store->num_pages - 1][store->last_page_num_used];
	store->last_page_num_used += 1;

	*res = instr;
	res->label = -1;

	return res;
}

/*
static void
bc_instr_free(struct bc_env *env, struct bc_instr *instr)
{
	// TODO: Freelist
	memset(instr, 0, sizeof(struct bc_instr));
}
*/

bc_var
bc_alloc_var(struct bc_env *env, type_id type)
{
	return dlist_append(env->var_types, env->num_vars, &type);
}

bc_var
bc_alloc_param(struct bc_env *env, unsigned int param_id, type_id tid)
{
	if (param_id >= env->num_params) {
		size_t new_num_params = param_id + 1;
		type_id *new_param_types;
		new_param_types = realloc(env->param_types,
				new_num_params * sizeof(type_id));
		
		if (!new_param_types) {
			printf("Failed to alloc memory for param types");
			return BC_VAR_NEW;
		}

		memset(&new_param_types[env->num_params], 0,
				(new_num_params - env->num_params) * sizeof(type_id));

		env->param_types = new_param_types;
		env->num_params = new_num_params;
	}

	if (env->param_types[param_id] != 0) {
		assert_type_equals(env->vm, env->param_types[param_id], tid);
	}
	env->param_types[param_id] = tid;

	return (-param_id) - 1;
}

static inline bc_var
bc_use_or_alloc_var(struct bc_env *env, bc_var var, type_id type)
{
	assert(type != TYPE_UNSET);
	if (var == BC_VAR_NEW) {
		return dlist_append(env->var_types, env->num_vars, &type);
	} else {
		return var;
	}
}

type_id
bc_get_var_type(struct bc_env *env, bc_var var)
{
	if (var < 0) {
		int param_id = (-var) - 1;
		assert(param_id < env->num_params);
		return env->param_types[param_id];
	} else {
		assert(var < env->num_vars);
		return env->var_types[var];
	}
}

type_id
bc_get_closure_type(struct bc_env *env, bc_closure closure)
{
	assert(closure < env->num_closures);
	return env->closure_types[closure];
}

bool
bc_valid_var(struct bc_env *env, bc_var var)
{
	return (
		var >= -(bc_var)env->num_params &&
		var <   (bc_var)env->num_vars
	);
}


struct bc_instr *
bc_gen_nop(struct bc_env *env)
{
	struct bc_instr instr = {0};
	instr.op = BC_NOP;

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_load(struct bc_env *env, bc_var target, struct object obj)
{
	assert(obj.type != TYPE_UNSET);

	struct bc_instr instr = {0};
	instr.op = BC_LOAD;
	instr.load.target = bc_use_or_alloc_var(env, target, obj.type);
	instr.load.obj = dlist_append(env->consts, env->num_consts, &obj);

	assert_type_equals(env->vm,
			bc_get_var_type(env, instr.load.target), obj.type);

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_copy(struct bc_env *env, bc_var target, bc_var src)
{
	struct bc_instr instr = {0};
	instr.op = BC_COPY;
	instr.copy.target = bc_use_or_alloc_var(
			env, target, bc_get_var_type(env, src));
	instr.copy.src = src;

	assert_type_equals(env->vm,
			bc_get_var_type(env, target),
			bc_get_var_type(env, src));

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_copy_closure(struct bc_env *env, bc_var target, bc_closure closure)
{
	assert(closure < env->num_closures);

	type_id closure_type;
	closure_type = bc_get_closure_type(env, closure);

	struct bc_instr instr = {0};
	instr.op = BC_COPY_CLOSURE;
	instr.copy_closure.target =
		bc_use_or_alloc_var(env, target, closure_type);
	instr.copy_closure.closure = closure;

	assert_type_equals(env->vm,
			bc_get_var_type(env, instr.copy_closure.target),
			closure_type);

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_push_arg(struct bc_env *env, bc_var var)
{
	struct bc_instr instr = {0};
	instr.op = BC_PUSH_ARG;
	assert(bc_valid_var(env, var));
	instr.push_arg.var = var;

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_lcall(struct bc_env *env, bc_var target, func_id func)
{
	struct func *func_inst = vm_get_func(env->vm, func);
	type_id func_ret_type =
		func_return_type(env->vm, func_inst->type);

	struct bc_instr instr = {0};
	instr.op = BC_LCALL;
	instr.lcall.target = bc_use_or_alloc_var(
			env, target, func_ret_type);
	instr.lcall.func = func;

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_clcall(struct bc_env *env, bc_var target, func_id func, void *closure)
{
	struct func *func_inst = vm_get_func(env->vm, func);
	type_id func_ret_type =
		func_return_type(env->vm, func_inst->type);

	struct bc_instr instr = {0};
	instr.op = BC_CLCALL;
	instr.clcall.target = bc_use_or_alloc_var(
			env, target, func_ret_type);
	instr.clcall.func = func;
	instr.clcall.closure = closure;

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_vcall(struct bc_env *env, bc_var target, bc_var func)
{
	assert(bc_valid_var(env, func));

	type_id func_ret_type =
		func_return_type(env->vm,
				bc_get_var_type(env, func));

	struct bc_instr instr = {0};
	instr.op = BC_VCALL;
	instr.vcall.target = bc_use_or_alloc_var(
			env, target, func_ret_type);
	instr.vcall.func = func;

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_testeq(struct bc_env *env, bc_var target, bc_var lhs, bc_var rhs)
{
	assert(bc_valid_var(env, lhs));
	assert(bc_valid_var(env, rhs));

	struct bc_instr instr = {0};
	instr.op = BC_TESTEQ;
	instr.testeq.target = bc_use_or_alloc_var(
			env, target, env->vm->default_types.boolean);
	instr.testeq.lhs = lhs;
	instr.testeq.rhs = rhs;

	type_id lhs_type = bc_get_var_type(env, lhs);
	type_id rhs_type = bc_get_var_type(env, rhs);

	assert_type_equals(env->vm, lhs_type, rhs_type);

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_jmp(struct bc_env *env, struct bc_instr *dest)
{
	struct bc_instr instr = {0};
	instr.op = BC_JMP;
	instr.jmp = dest;

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_jmpif(struct bc_env *env, struct bc_instr *dest)
{
	struct bc_instr instr = {0};
	instr.op = BC_JMPIF;
	instr.jmp = dest;

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_pack(struct bc_env *env, bc_var target,
		object_pack_func func, void *data, type_id ret_type)
{
	struct bc_instr instr = {0};
	instr.op = BC_PACK;
	instr.pack.target = bc_use_or_alloc_var(
			env, target, ret_type);
	instr.pack.func = func;
	instr.pack.data = data;

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_unpack(struct bc_env *env, bc_var target,
		object_unpack_func func, void *data,
		int param_id, type_id ret_type)
{
	struct bc_instr instr = {0};
	instr.op = BC_UNPACK;
	instr.unpack.target = bc_use_or_alloc_var(
			env, target, ret_type);
	instr.unpack.func = func;
	instr.unpack.data = data;
	instr.unpack.param_id = param_id;

	assert(instr.unpack.func);

	return bc_instr_alloc(env->store, instr);
}

struct bc_instr *
bc_gen_ret(struct bc_env *env, bc_var var)
{
	struct bc_instr instr = {0};
	instr.op = BC_RET;
	assert(bc_valid_var(env, var));
	instr.ret.var = var;

	return bc_instr_alloc(env->store, instr);
}
