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
				store->pages, new_num_pages * sizeof(struct instr *));
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
	struct bc_instr instr = {0};
	instr.op = BC_LOAD;
	instr.load.target = bc_use_or_alloc_var(env, target, obj.type);
	instr.load.obj = dlist_append(env->consts, env->num_consts, &obj);

	assert_type_equals(env->vm,
			bc_get_var_type(env, instr.load.target), obj.type);

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
bc_gen_ret(struct bc_env *env, bc_var var)
{
	struct bc_instr instr = {0};
	instr.op = BC_RET;
	assert(bc_valid_var(env, var));
	instr.ret.var = var;

	return bc_instr_alloc(env->store, instr);
}