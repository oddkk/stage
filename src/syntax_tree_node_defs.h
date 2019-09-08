ST_NODE(INTERNAL_LIST, struct {
	struct st_node *head;
	struct st_node *tail;
})

ST_NODE(MODULE, struct {
	struct st_node *body;
})

ST_NODE(STMT, struct {
	struct st_node *attrs;
	struct st_node *stmt;
})

ST_NODE(DECL_STMT, struct {
	struct st_node *name;
	struct st_node *args;
	struct st_node *decl;
})

ST_NODE(OBJ_DECL, struct {
	struct st_node *ident;
	struct st_node *body;
})

ST_NODE(ENUM_DECL, struct {
	struct st_node *items;
})

ST_NODE(ENUM_ITEM, struct {
	struct atom *name;
	struct st_node *data;
})

ST_NODE(USE_STMT, struct {
	struct st_node *ident;
})

ST_NODE(USE_ALL, struct {
	int _dc;
})

ST_NODE(ASSERT_STMT, struct {
	struct st_node *expr;
})

ST_NODE(FUNC_PROTO, struct {
	struct st_node *params;
	struct st_node *ret;
})

ST_NODE(ASSIGN_STMT, struct {
	struct st_node *ident;
	struct st_node *type;
	struct st_node *body;
})

ST_NODE(BIND, struct {
	struct st_node *src;
	struct st_node *drain;
})

ST_NODE(NAMESPACE, struct {
	struct st_node *name;
	struct st_node *body;
})

ST_NODE(TEMPLATE_VAR, struct {
	struct atom *name;
})

ST_NODE(ACCESS, struct {
	struct st_node *lhs;
	struct st_node *rhs;
})

ST_NODE(BIN_OP, struct {
	enum st_bin_op op;
	struct st_node *lhs;
	struct st_node *rhs;
})

ST_NODE(LAMBDA, struct {
	struct st_node *proto;
	struct st_node *body;
})

ST_NODE(FUNC_CALL, struct {
	struct st_node *ident;
	struct st_node *params;
})

ST_NODE(TUPLE_DECL, struct {
	struct st_node *items;
	bool named;
})

ST_NODE(TUPLE_DECL_ITEM, struct {
	struct atom *name;
	struct st_node *type;
})

ST_NODE(TUPLE_LIT, struct {
	struct st_node *items;
	bool named;
})

ST_NODE(TUPLE_LIT_ITEM, struct {
	struct atom *name;
	struct st_node *value;
})

ST_NODE(ARRAY_LIT, struct {
	struct st_node *items;
})

ST_NODE(SPECIAL, struct {
	struct atom *name;
	struct st_node *args;
})

ST_NODE(NUM_LIT, int64_t)

ST_NODE(STR_LIT, struct string)

ST_NODE(IDENT, struct atom *)
