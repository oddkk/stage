#include "native_bytecode.h"
#include "dlist.h"
#include "vm.h"
#include "base/mod.h"
#include <string.h>
#include <ffi.h>

static void
nbc_append_instr(struct nbc_func *func, struct nbc_instr instr)
{
	dlist_append(func->instrs, func->num_instr, &instr);
}

struct nbc_stack_var_info {
	size_t offset;
	size_t size;
};

void
nbc_compile_from_bc(struct nbc_func *out_func, struct bc_env *env)
{
	struct bc_instr *ip = env->entry_point;

	struct nbc_stack_var_info vars[env->num_vars];

	{
		size_t offset = 0;
		for (size_t i = 0; i < env->num_vars; i++) {
			struct type *var_type;
			var_type = vm_get_type(env->vm, env->var_types[i]);
			vars[i].offset = offset;
			vars[i].size = var_type->size;
			offset += var_type->size;
		}

		out_func->stack_size = offset;
	}

	out_func->max_callee_args = 0;
	size_t num_pushed_args = 0;

	while (ip) {
		switch (ip->op) {
			case BC_NOP:
				// Ignore.
				break;

			case BC_LOAD:
				{
					struct object const_obj = env->consts[ip->load.obj];
					struct nbc_instr instr = {0};

					instr.op = NBC_LOAD;
					instr.load.target = vars[ip->load.target].offset;
					instr.load.size   = vars[ip->load.target].size;
					instr.load.data   = const_obj.data;

					nbc_append_instr(out_func, instr);

					num_pushed_args += 1;
					if (num_pushed_args > out_func->max_callee_args) {
						out_func->max_callee_args = num_pushed_args;
					}
				}
				break;

			case BC_PUSH_ARG:
				{
					struct nbc_instr instr = {0};

					if (ip->push_arg.var < 0) {
						instr.op = NBC_PUSH_ARG_PARAM;
						instr.push_arg.param = (-ip->push_arg.var) - 1;
					} else {
						instr.op = NBC_PUSH_ARG;
						instr.push_arg.var = vars[ip->push_arg.var].offset;
					}

					nbc_append_instr(out_func, instr);

					num_pushed_args += 1;
					if (num_pushed_args > out_func->max_callee_args) {
						out_func->max_callee_args = num_pushed_args;
					}
				}
				break;

			case BC_LCALL:
				{
					struct func *func;
					func = vm_get_func(env->vm, ip->lcall.func);

					struct nbc_instr instr = {0};

					// We can not write to param slots.
					assert(ip->lcall.target >= 0);
					instr.call.target = vars[ip->lcall.target].offset;

					switch (func->kind) {
						case FUNC_NATIVE:
							{
								instr.op = NBC_CALL_NBC;
								instr.call.func.native.cif =
									stg_func_ffi_cif(env->vm, func->type);
								instr.call.func.native.fp = func->native;
							}
							break;

						case FUNC_BYTECODE:
							instr.op = NBC_CALL_NBC;
							instr.call.func.nbc = func->bytecode->nbc;
							break;
					}

					nbc_append_instr(out_func, instr);
					num_pushed_args = 0;
				}
				break;

			case BC_VCALL:
				{
					struct nbc_instr instr = {0};

					// We can not write to param slots.
					assert(ip->vcall.target >= 0);
					instr.call.target = vars[ip->vcall.target].offset;

					if (ip->vcall.func < 0) {
						instr.op = NBC_CALL_ARG;
						instr.call.func.param = (-ip->vcall.func) - 1;
					} else {
						instr.op = NBC_CALL;
						instr.call.func.var = vars[ip->vcall.func].offset;
					}

					nbc_append_instr(out_func, instr);

					num_pushed_args = 0;
				}
				break;

			case BC_PACK:
				{
					struct nbc_instr instr = {0};

					assert(ip->pack.target >= 0);

					instr.op = NBC_PACK;
					instr.pack.func = ip->pack.func;
					instr.pack.data = ip->pack.data;
					instr.pack.target = vars[ip->pack.target].offset;

					nbc_append_instr(out_func, instr);

					num_pushed_args = 0;
				}
				break;

			case BC_RET:
				{
					struct nbc_instr instr = {0};

					if (ip->push_arg.var < 0) {
						instr.op = NBC_RET_PARAM;

						struct type *ret_type;
						ret_type = vm_get_type(env->vm,
								bc_get_var_type(env, ip->push_arg.var));

						instr.ret.size = ret_type->size;
						instr.ret.param = (-ip->ret.var) - 1;
					} else {
						instr.op = NBC_RET;
						instr.ret.var  = vars[ip->ret.var].offset;
						instr.ret.size = vars[ip->ret.var].size;
					}

					nbc_append_instr(out_func, instr);
				}
				break;
		}

		ip = ip->next;
	}
}

static void
nbc_call_func(struct vm *vm, func_id fid, void **args, size_t num_args, void *ret)
{
	struct func *func = vm_get_func(vm, fid);

	switch (func->kind) {
		case FUNC_NATIVE:
			{
				ffi_cif *cif = stg_func_ffi_cif(vm, func->type);

				ffi_call(cif, FFI_FN(func->native), ret, args);
			}
			break;

		case FUNC_BYTECODE:
			nbc_exec(vm, func->bytecode->nbc, args, num_args, ret);
			break;
	}
}

void
nbc_exec(struct vm *vm, struct nbc_func *func,
		void **params, size_t num_params, void *ret)
{
	uint8_t stack[func->stack_size];
	void *args[func->max_callee_args];
	size_t num_args = 0;
	struct nbc_instr *ip = func->instrs;

	memset(stack, 0, func->stack_size);
	memset(args, 0, sizeof(void *) * func->max_callee_args);

	while (ip < func->instrs + func->num_instr) {
		switch (ip->op) {
			case NBC_NOP:
				break;

			case NBC_LOAD:
				memcpy(&stack[ip->load.target],
						ip->load.data, ip->load.size);
				break;

			case NBC_PUSH_ARG:
				args[num_args] = &stack[ip->push_arg.var];
				num_args += 1;
				break;

			case NBC_PUSH_ARG_PARAM:
				args[num_args] = params[ip->push_arg.param];
				num_args += 1;
				break;

			case NBC_CALL:
				nbc_call_func(vm, *(func_id *)&stack[ip->call.func.var],
						args, num_args, &stack[ip->call.target]);
				num_args = 0;
				break;

			case NBC_CALL_ARG:
				nbc_call_func(vm, *(func_id *)params[ip->call.func.param],
						args, num_args, &stack[ip->call.target]);
				num_args = 0;
				break;

			case NBC_CALL_NBC:
				nbc_exec(vm, ip->call.func.nbc,
						args, num_args, &stack[ip->call.target]);
				num_args = 0;
				break;

			case NBC_CALL_NATIVE:
				ffi_call((ffi_cif *)ip->call.func.native.cif,
						(void (*)(void))ip->call.func.native.fp,
						 &stack[ip->call.target], args);
				num_args = 0;
				break;

			case NBC_PACK:
				ip->pack.func(vm, ip->pack.data,
						&stack[ip->pack.target],
						args, num_args);
				num_args = 0;
				break;

			case NBC_RET:
				memcpy(ret, &stack[ip->ret.var], ip->ret.size);
				break;

			case NBC_RET_PARAM:
				memcpy(ret, params[ip->ret.param], ip->ret.size);
				break;
		}

		ip++;
	}
}

void
nbc_print(struct nbc_func *func)
{
	struct nbc_instr *ip = func->instrs;

	while (ip < func->instrs + func->num_instr) {
		switch (ip->op) {
			case NBC_NOP:
				printf("NOP\n");
				break;

			case NBC_LOAD:
				printf("LOAD sp+0x%zx = %p +%zu\n",
						ip->load.target,
						(void *)ip->load.data,
						ip->load.size);
				break;

			case NBC_PUSH_ARG:
				printf("PUSH_ARG sp+0x%zx\n", ip->push_arg.var);
				break;

			case NBC_PUSH_ARG_PARAM:
				printf("PUSH_ARG_PARAM p%zu\n", ip->push_arg.param);
				break;

			case NBC_CALL:
				printf("sp+0x%zx = CALL sp+0x%zx\n",
						ip->call.target,
						ip->call.func.var);
				break;

			case NBC_CALL_ARG:
				printf("sp+0x%zx = CALL_ARG p%zu\n",
						ip->call.target,
						ip->call.func.param);
				break;

			case NBC_CALL_NBC:
				printf("sp+0x%zx = CALL_NBC %p\n",
						ip->call.target,
						(void *)ip->call.func.nbc);
				break;

			case NBC_CALL_NATIVE:
				printf("sp+0x%zx = CALL_NATIVE %p (%p)\n",
						ip->call.target,
						ip->call.func.native.fp,
						ip->call.func.native.cif);
				break;

			case NBC_PACK:
				printf("sp+0x%zx = PACK %p (%p)\n",
						ip->pack.target,
						(void *)ip->pack.func,
						ip->pack.data);
				break;

			case NBC_RET:
				printf("RET sp+0x%zx\n",
						ip->ret.var);
				break;

			case NBC_RET_PARAM:
				printf("RET p%zu\n",
						ip->ret.param);
				break;
		}

		ip++;
	}
}
