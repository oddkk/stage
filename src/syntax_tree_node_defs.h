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

ST_NODE(VARIANT_DECL, struct {
	struct st_node *items;
	struct st_node *params;
})

ST_NODE(VARIANT_ITEM, struct {
	struct atom *name;
	struct st_node *data_type;
})

ST_NODE(USE_STMT, struct {
	struct st_node *ident;
})

ST_NODE(MOD_STMT, struct {
	struct atom *ident;
})

ST_NODE(USE_ALL, struct {
	struct st_node *target;
})

ST_NODE(TYPE_CLASS_DECL, struct {
	struct st_node *params;
	struct st_node *body;
})

ST_NODE(IMPL_STMT, struct {
	struct st_node *target;
	struct st_node *args;
	struct st_node *body;
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
	bool decl;
	bool overridable;
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
	struct st_node *type;
})

ST_NODE(ACCESS, struct {
	struct st_node *target;
	struct atom *name;
})

ST_NODE(BIN_OP, struct {
	enum st_bin_op op;
	struct st_node *lhs;
	struct st_node *rhs;
	struct stg_location loc;
})

ST_NODE(LAMBDA, struct {
	struct st_node *proto;
	struct st_node *body;
	bool special;
})

ST_NODE(FUNC_CALL, struct {
	struct st_node *ident;
	struct st_node *params;
})

ST_NODE(TEMPL_INST, struct {
	struct st_node *ident;
	struct st_node *params;
})

ST_NODE(OBJECT_INST, struct {
	struct st_node *name;
	struct st_node *body;
})

ST_NODE(OBJECT_DECL, struct {
	struct st_node *params;
	struct st_node *body;
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
	bool ellipsis;
})

ST_NODE(SPECIAL, struct {
	struct atom *name;
	struct st_node *args;
})

ST_NODE(MATCH_EXPR, struct {
	struct st_node *value;
	struct st_node *cases;
})

ST_NODE(MATCH_CASE, struct {
	struct st_node *pattern;
	struct st_node *expr;
})

ST_NODE(INIT_EXPR, struct {
	struct st_node *expr;
})

ST_NODE(DO_EXPR, struct {
	struct st_node *body;
})

ST_NODE(DO_EXPR_STMT, struct {
	struct st_node *target;
	struct st_node *expr;
})

ST_NODE(NUM_LIT, int64_t)

ST_NODE(STR_LIT, struct string)

ST_NODE(IDENT, struct atom *)
ST_NODE(WILDCARD, struct { int _dc; })
