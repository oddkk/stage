#include "native_bytecode.h"
#include "dlist.h"
#include "vm.h"
#include "base/mod.h"
#include <string.h>
#include <stdlib.h>
#include <ffi.h>

static size_t
nbc_append_instr(struct nbc_func *func, struct nbc_instr instr)
{
	return dlist_append(func->instrs, func->num_instr, &instr);
}

struct nbc_stack_var_info {
	size_t offset;
	size_t size;
};

struct nbc_label {
	struct bc_instr *instr;
	ssize_t offset;
};

struct nbc_jmp {
	size_t addr;
	size_t label;
};

struct nbc_instr
nbc_instr_push_arg(struct nbc_stack_var_info *vars, bc_var var)
{
	struct nbc_instr instr = {0};

	if (var < 0) {
		instr.op = NBC_PUSH_ARG_PARAM;
		instr.push_arg.param = (-var) - 1;
	} else {
		instr.op = NBC_PUSH_ARG;
		instr.push_arg.var = vars[var].offset;
	}

	return instr;
}

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

	bc_tag_labels(env);

	struct nbc_label labels[env->num_labels];
	memset(labels, 0, sizeof(struct nbc_label) * env->num_labels);
	for (size_t i = 0; i < env->num_labels; i++) {
		labels[i].offset = -1;
	}

	struct nbc_jmp *jmp_instrs = NULL;
	size_t num_jmp_instrs = 0;

	out_func->max_callee_args = 0;
	size_t num_pushed_args = 0;

	while (ip) {
		if (ip->label >= 0) {
			assert(ip->label < env->num_labels);
			labels[ip->label].instr = ip;
			labels[ip->label].offset = out_func->num_instr;
		}

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
					instr = nbc_instr_push_arg(
							vars, ip->push_arg.var);
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

					switch (func->kind) {
						case FUNC_NATIVE:
							{
								instr.op = ((func->flags & FUNC_HEAP) != 0)
									? NBC_CALL_NATIVE_HEAP
									: NBC_CALL_NATIVE;
								instr.call.target = vars[ip->lcall.target].offset;
								instr.call.func.native.cif =
									stg_func_ffi_cif(env->vm, func->type,
											func->flags);
								instr.call.func.native.fp = func->native;
							}
							break;

						case FUNC_CONS:
							instr.op = NBC_PACK;
							instr.pack.target = vars[ip->lcall.target].offset;
							assert(func->cons->pack);
							instr.pack.func = func->cons->pack;
							instr.pack.data = func->cons->data;
							break;

						case FUNC_BYTECODE:
							instr.op = NBC_CALL_NBC;
							instr.call.target = vars[ip->lcall.target].offset;
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

					switch (func->kind) {
						case FUNC_NATIVE:
							{
								instr.op = ((func->flags & FUNC_HEAP) != 0)
									? NBC_CALL_NATIVE_HEAP_CLOSURE
									: NBC_CALL_NATIVE_CLOSURE;
								instr.call.target = vars[ip->clcall.target].offset;
								instr.call.func.native.cif =
									stg_func_ffi_cif(env->vm, func->type,
											func->flags);
								instr.call.func.native.fp = func->native;
								instr.call.func.native.closure = ip->clcall.closure;
							}
							break;

						case FUNC_CONS:
							instr.op = NBC_PACK;
							instr.pack.target = vars[ip->clcall.target].offset;
							assert(func->cons->pack);
							instr.pack.func = func->cons->pack;
							instr.pack.data = func->cons->data;
							break;

						case FUNC_BYTECODE:
							instr.op = NBC_CALL_NBC_CLOSURE;
							instr.call.target = vars[ip->clcall.target].offset;
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

			case BC_TESTEQ:
				{
					assert(num_pushed_args == 0);

					struct nbc_instr lhs_instr = {0};
					lhs_instr = nbc_instr_push_arg(
							vars, ip->testeq.lhs);
					nbc_append_instr(out_func, lhs_instr);

					struct nbc_instr rhs_instr = {0};
					rhs_instr = nbc_instr_push_arg(
							vars, ip->testeq.rhs);
					nbc_append_instr(out_func, rhs_instr);

					struct nbc_instr instr = {0};
					assert(ip->testeq.target >= 0);

					instr.op = NBC_TESTEQ;
					instr.testeq.target = vars[ip->testeq.target].offset;

					assert(vars[ip->testeq.lhs].size == vars[ip->testeq.rhs].size);
					instr.testeq.size = vars[ip->testeq.lhs].size;

					nbc_append_instr(out_func, instr);

					if (2 > out_func->max_callee_args) {
						out_func->max_callee_args = 2;
					}
					num_pushed_args = 0;
				}
				break;

			case BC_LNOT:
				{
					struct nbc_instr instr = {0};
					instr.op = NBC_LNOT;
					assert(ip->lnot.target >= 0);
					instr.lnot.target = vars[ip->lnot.target].offset;

					nbc_append_instr(out_func, instr);

					assert(num_pushed_args == 1);
					num_pushed_args = 0;
				}
				break;

			case BC_JMP:
				{
					struct nbc_instr instr = {0};
					instr.op = NBC_JMP;

					size_t instr_addr;
					instr_addr = nbc_append_instr(out_func, instr);

					struct nbc_jmp jmp = {0};
					jmp.addr = instr_addr;
					jmp.label = ip->jmp->label;
					dlist_append(jmp_instrs, num_jmp_instrs, &jmp);

					assert(num_pushed_args == 0);
					num_pushed_args = 0;
				}
				break;

			case BC_JMPIF:
				{
					struct nbc_instr instr = {0};
					instr.op = NBC_JMPIF;

					size_t instr_addr;
					instr_addr = nbc_append_instr(out_func, instr);

					struct nbc_jmp jmp = {0};
					jmp.addr = instr_addr;
					jmp.label = ip->jmp->label;
					dlist_append(jmp_instrs, num_jmp_instrs, &jmp);

					assert(num_pushed_args == 1);
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

			case BC_TEST_UNPACK:
				{
					struct nbc_instr instr = {0};

					assert(ip->unpack.target >= 0);

					instr.op = NBC_TEST_UNPACK;
					instr.test_unpack.func = ip->test_unpack.func;
					instr.test_unpack.data = ip->test_unpack.data;
					instr.test_unpack.target = vars[ip->test_unpack.target].offset;

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

	for (size_t i = 0; i < num_jmp_instrs; i++) {
		struct nbc_jmp *jmp = &jmp_instrs[i];
		struct nbc_instr *instr;
		instr = &out_func->instrs[jmp->addr];

		assert(labels[jmp->label].offset >= 0);

		size_t dest = (size_t)labels[jmp->label].offset;

		switch (instr->op) {
			case NBC_JMP:
				instr->jmp.dest = dest;
				break;

			case NBC_JMPIF:
				instr->jmpif.dest = dest;
				break;

			default:
				panic("Non-jump instruction was tagged as a jump.");
				break;
		}
	}

	free(jmp_instrs);
}

static void
nbc_call_func(struct vm *vm, struct stg_exec *ctx, struct stg_func_object func_obj,
		void **args, size_t num_args, void *ret)
{
	struct func *func = vm_get_func(vm, func_obj.func);

	switch (func->kind) {
		case FUNC_NATIVE:
			{
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

				if ((func->flags & FUNC_REFS) != 0) {
					native_ref_func fp;
					fp = (native_ref_func)func->native;
					fp(closure_args, num_args+prefix_i, ret);
				} else {
					ffi_cif *cif = stg_func_ffi_cif(
							vm, func->type, func->flags);

					ffi_call(cif, FFI_FN(func->native), ret, closure_args);
				}
			}
			break;

		case FUNC_CONS:
			assert(func->cons->pack);
			func->cons->pack(vm, func->cons->data, ret, args, num_args);
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

			case NBC_TEST_UNPACK:
				{
					int val;
					val = ip->test_unpack.func(vm, ip->test_unpack.data, args[0]);
					memcpy(&stack[ip->test_unpack.target], &val, !!val);
					num_args = 0;
				}
				break;
			case NBC_TESTEQ:
				{
					int res;
					// TODO: Support user-specified equality comparison.
					res = memcmp(
							args[num_args-2],
							args[num_args-1],
							ip->testeq.size) == 0;
					memcpy(&stack[ip->testeq.target], &res, sizeof(int));
					num_args = 0;
				}
				break;

			case NBC_LNOT:
				{
					int val;
					val = !(*(int *)args[0]);
					memcpy(&stack[ip->lnot.target], &val, sizeof(int));
					num_args = 0;
				}
				break;

			case NBC_JMP:
				ip = func->instrs + ip->jmp.dest;
				// TODO: Should keeping arguments past jumps be allowed?
				num_args = 0;
				// Skip incrementing the instruction pointer.
				continue;

			case NBC_JMPIF:
				{
					int val;
					val = *(int *)args[num_args-1];
					num_args = 0;

					if (val != 0) {
						ip = func->instrs + ip->jmp.dest;
						// Skip incrementing the instruction pointer.
						continue;
					}
				}
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
		printf("%03zx ", ip - func->instrs);
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

			case NBC_TESTEQ:
				printf("sp+0x%zu = TESTEQ (%zu)\n",
						ip->testeq.target,
						ip->testeq.size);
				break;

			case NBC_LNOT:
				printf("sp+0x%zu = LNOT\n",
						ip->lnot.target);
				break;

			case NBC_JMP:
				printf("JMP %03zx\n",
						ip->jmp.dest);
				break;

			case NBC_JMPIF:
				printf("JMPIF %03zx\n",
						ip->jmpif.dest);
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

			case NBC_TEST_UNPACK:
				printf("sp+0x%zx = TEST_UNPACK %p (%p)\n",
						ip->unpack.target,
						(void *)ip->unpack.func,
						ip->unpack.data);
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
