#include "syntax_tree.h"

struct string cfg_node_names[] = {
#define CFG_NODE(name, data) STR(#name),
#include "syntax_tree_node_defs.h"
#undef CFG_NODE
};

