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

	struct nbc_stack_var_info closures[env->num_closures];

	{
		size_t offset = 0;
		for (size_t i = 0; i < env->num_closures; i++) {
			struct type *closure_type;
			closure_type = vm_get_type(env->vm, env->closure_types[i]);
			closures[i].offset = offset;
			closures[i].size = closure_type->size;
			offset += closure_type->size;
		}

		out_func->closure_size = offset;
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
				}
				break;

			case BC_COPY:
				{
					struct nbc_instr instr = {0};

					// Can not copy into a parameter.
					assert(ip->copy.target >= 0);
					instr.copy.target = vars[ip->copy.target].offset;
					instr.copy.size   = vars[ip->copy.target].size;

					if (ip->copy.src < 0) {
						instr.op = NBC_COPY_PARAM;
						instr.copy.src_param = (-ip->copy.src) - 1;;
					} else {
						instr.op = NBC_COPY;
						instr.copy.src = vars[ip->copy.src].offset;
					}

					nbc_append_instr(out_func, instr);
				}
				break;

			case BC_COPY_CLOSURE:
				{
					struct nbc_instr instr = {0};

					instr.op = NBC_COPY_CLOSURE;

					// Can not copy into a parameter.
					assert(ip->copy_closure.target >= 0);

					instr.copy.target = vars[ip->copy_closure.target].offset;
					instr.copy.size   = vars[ip->copy_closure.target].size;
					instr.copy.src_closure_offset =
						closures[ip->copy_closure.closure].offset;

					assert(instr.copy.size == closures[ip->copy_closure.closure].size);

					nbc_append_instr(out_func, instr);
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

					assert((func->flags & FUNC_CLOSURE) == 0);

					struct nbc_instr instr = {0};

					// We can not write to param slots.
					assert(ip->lcall.target >= 0);
					instr.call.target = vars[ip->lcall.target].offset;

					switch (func->kind) {
						case FUNC_NATIVE:
							{
								instr.op = ((func->flags & FUNC_HEAP) != 0)
									? NBC_CALL_NATIVE_HEAP
									: NBC_CALL_NATIVE;
								instr.call.func.native.cif =
									stg_func_ffi_cif(env->vm, func->type,
											func->flags);
								instr.call.func.native.fp = func->native;
							}
							break;

						case FUNC_BYTECODE:
							instr.op = NBC_CALL_NBC;
							instr.call.func.nbc.fp = func->bytecode->nbc;
							break;
					}

					nbc_append_instr(out_func, instr);
					num_pushed_args = 0;
				}
				break;

			case BC_CLCALL:
				{
					struct func *func;
					func = vm_get_func(env->vm, ip->clcall.func);

					assert((func->flags & FUNC_CLOSURE) != 0);

					struct nbc_instr instr = {0};

					// We can not write to param slots.
					assert(ip->clcall.target >= 0);
					instr.call.target = vars[ip->clcall.target].offset;

					switch (func->kind) {
						case FUNC_NATIVE:
							{
								instr.op = ((func->flags & FUNC_HEAP) != 0)
									? NBC_CALL_NATIVE_HEAP_CLOSURE
									: NBC_CALL_NATIVE_CLOSURE;
								instr.call.func.native.cif =
									stg_func_ffi_cif(env->vm, func->type,
											func->flags);
								instr.call.func.native.fp = func->native;
								instr.call.func.native.closure = ip->clcall.closure;
							}
							break;

						case FUNC_BYTECODE:
							instr.op = NBC_CALL_NBC_CLOSURE;
							instr.call.func.nbc.fp = func->bytecode->nbc;
							instr.call.func.nbc.closure = ip->clcall.closure;
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

			case BC_UNPACK:
				{
					struct nbc_instr instr = {0};

					assert(ip->unpack.target >= 0);

					instr.op = NBC_UNPACK;
					instr.unpack.func = ip->unpack.func;
					instr.unpack.data = ip->unpack.data;
					instr.unpack.target = vars[ip->unpack.target].offset;
					instr.unpack.param_id = ip->unpack.param_id;

					nbc_append_instr(out_func, instr);
					assert(num_pushed_args == 1);
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
nbc_call_func(struct vm *vm, struct stg_exec *ctx, struct stg_func_object func_obj,
		void **args, size_t num_args, void *ret)
{
	struct func *func = vm_get_func(vm, func_obj.func);

	switch (func->kind) {
		case FUNC_NATIVE:
			{
				ffi_cif *cif = stg_func_ffi_cif(
						vm, func->type, func->flags);
				void *closure_args[num_args+2];
				size_t prefix_i = 0;

				if ((func->flags & FUNC_HEAP) != 0) {
					closure_args[prefix_i] = &ctx;
					prefix_i += 1;
				}

				if ((func->flags & FUNC_CLOSURE) != 0) {
					closure_args[prefix_i] = &func_obj.closure;
					prefix_i += 1;
				}

				memcpy(&closure_args[prefix_i], args, sizeof(void *) * num_args);

				ffi_call(cif, FFI_FN(func->native), ret, closure_args);
			}
			break;

		case FUNC_BYTECODE:
			nbc_exec(vm, ctx, func->bytecode->nbc, args, num_args, func_obj.closure, ret);
			break;
	}
}

void
nbc_exec(struct vm *vm, struct stg_exec *ctx, struct nbc_func *func,
		void **params, size_t num_params, void *closure, void *ret)
{
	uint8_t stack[func->stack_size];
	void *closure_args[func->max_callee_args+2];
	void **args = &closure_args[2];
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

			case NBC_COPY:
				memcpy(&stack[ip->copy.target],
						&stack[ip->copy.src], ip->copy.size);
				break;

			case NBC_COPY_PARAM:
				memcpy(&stack[ip->copy.target],
						params[ip->copy.src_param], ip->copy.size);
				break;

			case NBC_COPY_CLOSURE:
				memcpy(&stack[ip->copy.target],
						(uint8_t *)closure + ip->copy.src_closure_offset, ip->copy.size);
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
				nbc_call_func(vm, ctx, *(struct stg_func_object *)&stack[ip->call.func.var],
						args, num_args, &stack[ip->call.target]);
				num_args = 0;
				break;

			case NBC_CALL_ARG:
				nbc_call_func(vm, ctx, *(struct stg_func_object *)params[ip->call.func.param],
						args, num_args, &stack[ip->call.target]);
				num_args = 0;
				break;

			case NBC_CALL_NBC:
				nbc_exec(vm, ctx, ip->call.func.nbc.fp,
						args, num_args, NULL, &stack[ip->call.target]);
				num_args = 0;
				break;

			case NBC_CALL_NBC_CLOSURE:
				nbc_exec(vm, ctx, ip->call.func.nbc.fp,
						args, num_args, ip->call.func.nbc.closure, &stack[ip->call.target]);
				num_args = 0;
				break;

			case NBC_CALL_NATIVE:
				ffi_call((ffi_cif *)ip->call.func.native.cif,
						(void (*)(void))ip->call.func.native.fp,
						 &stack[ip->call.target], args);
				num_args = 0;
				break;

			case NBC_CALL_NATIVE_CLOSURE:
				closure_args[1] = &ip->call.func.native.closure;
				ffi_call((ffi_cif *)ip->call.func.native.cif,
						(void (*)(void))ip->call.func.native.fp,
						 &stack[ip->call.target], closure_args+1);
				num_args = 0;
				break;

			case NBC_CALL_NATIVE_HEAP:
				closure_args[1] = &ctx;
				ffi_call((ffi_cif *)ip->call.func.native.cif,
						(void (*)(void))ip->call.func.native.fp,
						 &stack[ip->call.target], closure_args+1);
				num_args = 0;
				break;

			case NBC_CALL_NATIVE_HEAP_CLOSURE:
				closure_args[0] = &ctx;
				closure_args[1] = &ip->call.func.native.closure;
				ffi_call((ffi_cif *)ip->call.func.native.cif,
						(void (*)(void))ip->call.func.native.fp,
						 &stack[ip->call.target], closure_args);
				num_args = 0;
				break;

			case NBC_PACK:
				ip->pack.func(vm, ip->pack.data,
						&stack[ip->pack.target],
						args, num_args);
				num_args = 0;
				break;

			case NBC_UNPACK:
				ip->unpack.func(vm, ip->unpack.data,
						&stack[ip->unpack.target],
						args[0], ip->unpack.param_id);
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

			case NBC_COPY:
				printf("COPY sp+0x%zx = sp+0x%zx +%zu\n",
						ip->copy.target,
						ip->copy.src,
						ip->copy.size);
				break;

			case NBC_COPY_PARAM:
				printf("COPY_PARAM sp+0x%zx = p%zu +%zu\n",
						ip->copy.target,
						ip->copy.src_param,
						ip->copy.size);
				break;

			case NBC_COPY_CLOSURE:
				printf("COPY_CLOSURE sp+0x%zx = closure+%zu +%zu\n",
						ip->copy.target,
						ip->copy.src_closure_offset,
						ip->copy.size);
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
						(void *)ip->call.func.nbc.fp);
				break;

			case NBC_CALL_NBC_CLOSURE:
				printf("sp+0x%zx = CALL_NBC_CLOSURE %p %p\n",
						ip->call.target,
						(void *)ip->call.func.nbc.fp,
						(void *)ip->call.func.nbc.closure);
				break;

			case NBC_CALL_NATIVE:
				printf("sp+0x%zx = CALL_NATIVE %p (%p)\n",
						ip->call.target,
						ip->call.func.native.fp,
						ip->call.func.native.cif);
				break;

			case NBC_CALL_NATIVE_CLOSURE:
				printf("sp+0x%zx = CALL_NATIVE %p[%p] (%p)\n",
						ip->call.target,
						ip->call.func.native.fp,
						ip->call.func.native.cif,
						ip->call.func.native.closure);
				break;

			case NBC_CALL_NATIVE_HEAP:
				printf("sp+0x%zx = CALL_NATIVE_HEAP %p (%p)\n",
						ip->call.target,
						ip->call.func.native.fp,
						ip->call.func.native.cif);
				break;

			case NBC_CALL_NATIVE_HEAP_CLOSURE:
				printf("sp+0x%zx = CALL_NATIVE_HEAP_CLOSURE %p[%p] (%p)\n",
						ip->call.target,
						ip->call.func.native.fp,
						ip->call.func.native.cif,
						ip->call.func.native.closure);
				break;

			case NBC_PACK:
				printf("sp+0x%zx = PACK %p (%p)\n",
						ip->pack.target,
						(void *)ip->pack.func,
						ip->pack.data);
				break;

			case NBC_UNPACK:
				printf("sp+0x%zx = UNPACK %p (%p, %i)\n",
						ip->unpack.target,
						(void *)ip->unpack.func,
						ip->unpack.data,
						ip->unpack.param_id);
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
