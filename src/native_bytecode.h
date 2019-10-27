#ifndef STAGE_NATIVE_BYTECODE_H
#define STAGE_NATIVE_BYTECODE_H

#include "bytecode.h"

enum nbc_op {
	NBC_NOP,
	NBC_LOAD,
	NBC_PUSH_ARG,
	NBC_PUSH_ARG_PARAM,
	NBC_CALL,
	NBC_CALL_ARG,
	NBC_CALL_NBC,
	NBC_CALL_NATIVE,
	NBC_PACK,
	NBC_RET,
	NBC_RET_PARAM,
};

struct nbc_instr {
	enum nbc_op op;
	union {
		struct {
			size_t target;
			void *data;
			size_t size;
		} load;

		union {
			size_t var;
			size_t param;
		} push_arg;

		struct {
			size_t target;
			union {
				size_t var;
				size_t param;
				struct nbc_func *nbc;
				struct {
					void *cif;
					void *fp;
				} native;
			} func;
		} call;

		struct {
			size_t target;
			object_pack_func func;
			void *data;
		} pack;

		struct {
			size_t size;
			union {
				size_t var;
				size_t param;
			};
		} ret;
	};
};

struct nbc_func {
	struct nbc_instr *instrs;
	size_t num_instr;
	size_t max_callee_args;
	size_t stack_size;
};

void
nbc_compile_from_bc(struct nbc_func *out_func, struct bc_env *);

void
nbc_exec(struct vm *vm, struct nbc_func *func,
		void **params, size_t num_params, void *ret);

void
nbc_print(struct nbc_func *func);

#endif
