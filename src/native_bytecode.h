#ifndef STAGE_NATIVE_BYTECODE_H
#define STAGE_NATIVE_BYTECODE_H

#include "bytecode.h"

enum nbc_op {
	NBC_NOP,
	NBC_LOAD,
	NBC_COPY,
	NBC_COPY_PARAM,
	NBC_COPY_CLOSURE,
	NBC_PUSH_ARG,
	NBC_PUSH_ARG_PARAM,
	NBC_CALL,
	NBC_CALL_ARG,
	NBC_CALL_NBC,
	NBC_CALL_NBC_CLOSURE,
	NBC_CALL_NATIVE,
	NBC_CALL_NATIVE_CLOSURE,
	NBC_CALL_NATIVE_HEAP,
	NBC_CALL_NATIVE_HEAP_CLOSURE,
	NBC_TESTEQ,
	NBC_JMP,
	NBC_JMPIF,
	NBC_PACK,
	NBC_UNPACK,
	NBC_TEST_UNPACK,
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

		struct {
			size_t target;
			union {
				size_t src;
				size_t src_param;
				size_t src_closure_offset;
			};
			size_t size;
		} copy;

		union {
			size_t var;
			size_t param;
		} push_arg;

		struct {
			size_t target;
			union {
				size_t var;
				size_t param;
				struct {
					struct nbc_func *fp;
					void *closure;
				} nbc;
				struct {
					void *cif;
					void *fp;
					void *closure;
				} native;
			} func;
		} call;

		struct {
			size_t target;
			size_t size;
		} testeq;

		struct {
			size_t dest;
		} jmp;

		struct {
			size_t dest;
		} jmpif;

		struct {
			size_t target;
			object_pack_func func;
			void *data;
		} pack;

		struct {
			size_t target;
			object_unpack_func func;
			void *data;
			int param_id;
		} unpack;

		struct {
			size_t target;
			object_can_unpack_func func;
			void *data;
		} test_unpack;

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
	size_t closure_size;
};

void
nbc_compile_from_bc(struct nbc_func *out_func, struct bc_env *);

void
nbc_exec(struct vm *vm, struct stg_exec *, struct nbc_func *func,
		void **params, size_t num_params, void *closure, void *ret);

void
nbc_print(struct nbc_func *func);

#endif
