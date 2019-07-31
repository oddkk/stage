#include "syntax_tree.h"
#include "utils.h"
#include <string.h>

struct string st_node_names[] = {
#define ST_NODE(name, data) STR(#name),
#include "syntax_tree_node_defs.h"
#undef ST_NODE
};

struct string st_bin_op_sym[] = {
#define OP(name, sym) STR(sym),
	ST_BIN_OPS
#undef OP
};

struct atom *binop_atom(struct atom_table *atom_table,
						enum st_bin_op op)
{
	assert(op < ST_OP_LEN);

	char buffer[2 + ST_BIN_OPS_MAX_LEN] = {0};

	buffer[0] = 'o';
	buffer[1] = 'p';

	struct string sym = st_bin_op_sym[op];

	assert(sym.length <= ST_BIN_OPS_MAX_LEN);

	memcpy(&buffer[2], sym.text, sym.length);

	struct string name;
	name.text = buffer;
	name.length = 2 + sym.length;

	return atom_create(atom_table, name);
}

