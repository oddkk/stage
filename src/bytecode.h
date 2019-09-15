#ifndef STAGE_BYTECODE_H
#define STAGE_BYTECODE_H

typedef bc_var unsigned int;

enum bc_op {
	BC_NOP,
	BC_LOAD,
	BC_PUSH_PARAM,
	BC_CALL,
};

struct {
	enum bc_op op;
	union {
	};
} bc_instr;

#endif
