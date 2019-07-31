CFG_NODE(INTERNAL_LIST, struct {
	struct cfg_node *head;
	struct cfg_node *tail;
})

CFG_NODE(MODULE, struct {
	struct cfg_node *body;
})

CFG_NODE(STMT, struct {
	struct cfg_node *attrs;
	struct cfg_node *stmt;
})

CFG_NODE(DECL_STMT, struct {
	struct cfg_node *name;
	struct cfg_node *args;
	struct cfg_node *decl;
})

CFG_NODE(OBJ_DECL, struct {
	struct cfg_node *ident;
	struct cfg_node *body;
})

CFG_NODE(ENUM_DECL, struct {
	struct cfg_node *items;
})

CFG_NODE(ENUM_ITEM, struct {
	struct atom *name;
	struct cfg_node *data;
})

CFG_NODE(USE_STMT, struct {
	struct cfg_node *ident;
})

CFG_NODE(USE_ALL, struct {
	int _dc;
})

CFG_NODE(ASSERT_STMT, struct {
	struct cfg_node *expr;
})

CFG_NODE(FUNC_STMT, struct {
	struct cfg_node *ident;
	struct cfg_node *proto;
	struct cfg_node *body;
})

CFG_NODE(FUNC_PROTO, struct {
	struct cfg_node *params;
	struct cfg_node *ret;
})

CFG_NODE(ASSIGN_STMT, struct {
	struct cfg_node *ident;
	struct cfg_node *type;
	struct cfg_node *body;
})

CFG_NODE(BIND, struct {
	struct cfg_node *src;
	struct cfg_node *drain;
})

CFG_NODE(NAMESPACE, struct {
	struct cfg_node *name;
	struct cfg_node *body;
})

CFG_NODE(TEMPLATE_VAR, struct {
	struct atom *name;
})

CFG_NODE(ACCESS, struct {
	struct cfg_node *lhs;
	struct cfg_node *rhs;
})

CFG_NODE(BIN_OP, struct {
	enum cfg_bin_op op;
	struct cfg_node *lhs;
	struct cfg_node *rhs;
})

CFG_NODE(LAMBDA, struct {
	struct cfg_node *proto;
	struct cfg_node *body;
})

CFG_NODE(FUNC_CALL, struct {
	struct cfg_node *ident;
	struct cfg_node *params;
})

CFG_NODE(TUPLE_DECL, struct {
	struct cfg_node *items;
	bool named;
})

CFG_NODE(TUPLE_DECL_ITEM, struct {
	struct atom *name;
	struct cfg_node *type;
})

CFG_NODE(TUPLE_LIT, struct {
	struct cfg_node *items;
	bool named;
})

CFG_NODE(TUPLE_LIT_ITEM, struct {
	struct atom *name;
	struct cfg_node *value;
})

CFG_NODE(ARRAY_LIT, struct {
	struct cfg_node *items;
})

CFG_NODE(NUM_LIT, int64_t)

CFG_NODE(STR_LIT, struct string)

CFG_NODE(IDENT, struct atom *)
