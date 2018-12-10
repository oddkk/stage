%define api.value.type union
%define api.pure full
%define parse.error verbose
%locations

%code requires
{
	#include "config.h"
	#include "string.h"
	#include "arena.h"
	#include "utils.h"
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>

	#define YYDEBUG 1

	#define CONFIG_PARSER_STACK_SIZE 128
	#define BUFFER_SIZE 4096
	/*!max:re2c*/

	struct lex_context;
	struct tmp_range {
		struct config_node *low;
		struct config_node *high;
	};
}

%param { struct lex_context *ctx }

%code
{
	struct lex_context {
		char *cur;
		char *lim;
		char *tok;
		bool eof;
		struct arena *memory;
		struct atom_table *atom_table;
		FILE *fp;
		char buf[BUFFER_SIZE + YYMAXFILL];
		size_t head;
		struct cfg_node *module;
	};

	bool config_parse_fill(struct lex_context *ctx, size_t need) {
		if (ctx->eof) {
			return false;
		}
		size_t free = ctx->tok - ctx->buf;
		if (free < need) {
			return false;
		}
		memmove(ctx->buf, ctx->tok, ctx->lim - ctx->tok);
		ctx->lim -= free;
		ctx->cur -= free;
		ctx->tok -= free;
		ctx->lim += fread(ctx->lim, 1, free, ctx->fp);
		if (ctx->lim < ctx->buf + BUFFER_SIZE) {
			if (feof(ctx->fp)) {
				ctx->eof = true;
				memset(ctx->lim, 0, YYMAXFILL);
				ctx->lim += YYMAXFILL;
			} else {
				perror("config");
			}
		}
		return true;
	}

	struct config_node *_alloc_node_old(struct lex_context *ctx, enum config_node_type type, YYLTYPE loc) {
		struct config_node *res;

		res = arena_alloc_struct(ctx->memory, struct config_node);
		res->type = type;

		res->from.line   = loc.first_line;
		res->from.column = loc.first_column;

		res->to.line     = loc.last_line;
		res->to.column   = loc.last_column;

		return res;
	}

#define alloc_node(ctx, type) _alloc_node(ctx, type, yylloc)

	void append_child(struct config_node **first_child, struct config_node *node) {
		node->next_sibling = *first_child;
		*first_child = node;
	}

	/* static struct config_node *make_list(struct lex_context *ctx, struct config_node *head, struct config_node *tail) { */
	/* 	struct config_node *list_node; */
	/* 	YYLTYPE loc = {0}; */

	/* 	list_node = _alloc_node(ctx, CONFIG_NODE_INTERNAL_LIST, loc); */
	/* 	list_node->internal_list.head = head; */
	/* 	list_node->internal_list.tail = tail; */

	/* 	return list_node; */
	/* } */

	int yylex(YYSTYPE *, YYLTYPE *, struct lex_context *);
	void yyerror(YYLTYPE *loc, struct lex_context *, const char *);

	struct cfg_node *_alloc_node(struct lex_context *ctx, enum cfg_node_type type, YYLTYPE loc) {
		struct cfg_node *res;

		res = arena_alloc_struct(ctx->memory, struct cfg_node);
		res->type = type;

		res->from.line   = loc.first_line;
		res->from.column = loc.first_column;

		res->to.line     = loc.last_line;
		res->to.column   = loc.last_column;

		return res;
	}

#define CFG_NODE(name, data) \
	struct cfg_node *_mknode##name(struct lex_context *ctx,	\
								   YYLTYPE loc,				\
								   name##_t value) {		\
		struct cfg_node *result;							\
		result = _alloc_node(ctx, CFG_NODE_##name, loc);	\
		result->name = value;								\
		return result;										\
	}

	CFG_NODES
#undef CFG_NODE

#define MKNODE(type, ...) _mknode##type(ctx, yylloc, (type##_t){__VA_ARGS__})
}

%token END 0
%token IDENTIFIER NUMLIT
%token NAMESPACE "namespace" ENUM "Enum" USE "use"
%token BIND_LEFT "<-" BIND_RIGHT "->" RANGE ".." DECL "::" // ELLIPSIS "..."
%token EQ "==" NEQ "!=" LTE "<=" GTE ">="

/* %token DEVICETYPE "device_type" DEVICE "device" TYPE "type" INPUT "input" */
/* %token OUTPUT "output" DEFAULT "default" ATTR "attr" IDENTIFIER NUMLIT */
/* %token BIND "<-" RANGE ".." VERSION "version" NAMESPACE "namespace" */

/* %type	<struct atom*> IDENTIFIER device_name */
/* %type	<scalar_value> NUMLIT */
/* %type	<struct config_node*> module module_stmt_list module_stmt device_type device_type_body */
/* %type	<struct config_node*> device_type_body_stmt device device_body device_body_stmt */
/* %type	<struct config_node*> l_expr type_decl type type_l_expr array_type enum_list enum_label */
/* %type	<struct config_node*> tuple_decl tuple_list named_tuple_list tuple_item named_tuple_item expr */
/* %type	<struct config_node*> subrange_type namespace bind_stmt device_args device_type_params */
/* %type	<struct config_node*> device_body_stmt_list */
/* %type	<struct config_node*> tuple_lit tuple_lit_body tuple_lit_item */
/* %type	<struct config_node*> named_tuple_lit named_tuple_lit_body named_tuple_lit_item */
/* %type	<struct config_node*> array_lit array_lit_body */
/* %type	<struct tmp_range> range */

%type <struct cfg_node *> module stmt_list stmt stmt1 attr_list attr
%type <struct cfg_node *> decl_stmt decl enum_items enum_item use_stmt
%type <struct cfg_node *> use_expr use_expr1 func_stmt assign_stmt
%type <struct cfg_node *> bind_stmt bind_left_expr namespace_stmt
%type <struct cfg_node *> namespace_ident expr expr1 subscript_expr
%type <struct cfg_node *> l_expr type_expr lambda func_proto func_call
%type <struct cfg_node *> tuple_decl named_tuple_decl named_tuple_decl_item
%type <struct cfg_node *> unnamed_tuple_decl unnamed_tuple_decl_item
%type <struct cfg_node *> tuple_lit named_tuple_lit named_tuple_lit_item
%type <struct cfg_node *> unnamed_tuple_lit unnamed_tuple_lit_item
%type <struct cfg_node *> array_lit array_lit_body array_lit_item
%type <struct cfg_node *> ident numlit


%type <struct atom *> IDENTIFIER
%type <scalar_value> NUMLIT

%left '<' '>' "<=" ">=" "!=" "=="
%nonassoc ".."
%left '+' '-'
%left '*' '/'
%left '.' '[' '{'

%start module

%%

module:			stmt_list {
	$$ = MKNODE(MODULE, .body=$1);
	assert(ctx->module == NULL);
	ctx->module = $$;
 }
		;

stmt_list:		stmt_list stmt              { $$ = MKNODE(INTERNAL_LIST, .head=$2, .tail=$1); }
		|		%empty                      { $$ = NULL; }
		;

stmt:			stmt1                       { $$ = MKNODE(STMT, .stmt=$1); }
		|		'[' attr_list ']' stmt1     { $$ = MKNODE(STMT, .stmt=$4, .attrs=$2); }
		|		'[' attr_list ',' ']' stmt1 { $$ = MKNODE(STMT, .stmt=$5, .attrs=$2); }
		;

attr_list:		attr_list ',' attr { $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		attr               { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

attr:			expr               { $$ = NULL; }
		;

stmt1:			';'                { $$ = NULL; }
		|		decl_stmt          { $$ = $1; }
		|		namespace_stmt     { $$ = $1; }
		|		func_stmt      ';' { $$ = $1; }
		|		assign_stmt    ';' { $$ = $1; }
		|		bind_stmt      ';' { $$ = $1; }
		|		use_stmt       ';' { $$ = $1; }
		|		error          ';' { $$ = NULL; }
		;

decl_stmt:		ident "::" decl            { $$ = MKNODE(DECL_STMT, .name=$1, .decl=$3); }
		|		ident tuple_decl "::" decl { $$ = MKNODE(DECL_STMT, .name=$1, .decl=$4, .args=$2); }
		;

decl:			l_expr ';'                 { $$ = $1; }
		|		"Enum" '{' enum_items '}'  { $$ = MKNODE(ENUM_DECL, .items=$3); }
		|		"Enum" '{' error '}'       { $$ = NULL; }
		|		l_expr '{' stmt_list '}'   { $$ = MKNODE(OBJ_DECL, .ident=$1, .body=$3); }
		/* |		l_expr '{' error '}' */
		|		tuple_decl                 { $$ = $1; }
		;

enum_items:		enum_items ',' enum_item   { $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		enum_item                  { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

enum_item:		IDENTIFIER                 { $$ = MKNODE(ENUM_ITEM, .name=$1); }
		|		IDENTIFIER tuple_decl      { $$ = MKNODE(ENUM_ITEM, .name=$1, .data=$2); }
		;

use_stmt:		"use" use_expr             { $$ = MKNODE(USE_STMT, .ident=$2); }
		;

use_expr:		use_expr1                  { $$ = $1; }
		|		use_expr1 '.' '*'
					{ $$ = MKNODE(ACCESS, .lhs=$1, .rhs=alloc_node(ctx, CFG_NODE_USE_ALL)); }
		;

use_expr1:		use_expr1 '.' ident        { $$ = MKNODE(ACCESS, .lhs=$1, .rhs=$3); }
		|		ident                      { $$ = $1; }
		;

func_stmt:		ident func_proto '=' expr  { $$ = MKNODE(FUNC_STMT, .ident=$1, .proto=$2, .body=$4); }
		|		ident '=' expr             { $$ = MKNODE(FUNC_STMT, .ident=$1, .body=$3); }
		;

assign_stmt:	ident ':' '=' expr         { $$ = MKNODE(ASSIGN_STMT, .ident=$1, .body=$4); }
		|		ident ':' type_expr '=' expr
					{ $$ = MKNODE(ASSIGN_STMT, .ident=$1, .body=$5, .type=$3); }
		;

bind_stmt:		l_expr "<-" bind_left_expr { $$ = MKNODE(BIND, .src=$3, .drain=$1); }
		/* |		bind_right_expr "->" l_expr */
		;

/* bind_right_expr: */
/* 				bind_right_expr "->" expr */
/* 		|		expr */
/* 		; */

bind_left_expr:
				bind_left_expr "<-" expr { $$ = MKNODE(BIND, .src=$3, .drain=$1); }
		|		expr                     { $$ = $1; }
		;

namespace_stmt:	"namespace" namespace_ident '{' stmt_list '}'
					{ $$ = MKNODE(NAMESPACE, .name=$2, .body=$4); }
		;

namespace_ident:
				ident                    { $$ = $1; }
		|		namespace_ident '.' ident
					{ $$ = MKNODE(ACCESS, .lhs=$1, .rhs=$3); }
		;

expr:			expr1                    { $$ = $1; }
		|		lambda                   { $$ = $1; }
		;

expr1:  		tuple_lit                { $$ = $1; }
		|		array_lit                { $$ = $1; }
		|		l_expr                   { $$ = $1; }
		|		numlit                   { $$ = $1; }
		|		func_call                { $$ = $1; }
		|		'$' IDENTIFIER           { $$ = MKNODE(TEMPLATE_VAR, .name=$2); }

		|		expr1 '+' expr1          { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_ADD); }
		|		expr1 '-' expr1          { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_SUB); }
		|		expr1 '*' expr1          { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_MUL); }
		|		expr1 '/' expr1          { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_DIV); }
				// Should ranges be allowed?
		/* |		expr1 ".." expr1 */
		|		expr1 "==" expr1         { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_EQ);  }
		|		expr1 "!=" expr1         { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_NEQ); }
		|		expr1 "<=" expr1         { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_LTE); }
		|		expr1 ">=" expr1         { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_GTE); }
		|		expr1 '<' expr1          { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_LT);  }
		|		expr1 '>' expr1          { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=CFG_OP_GT);  }
		/* |		'(' expr ')' */
		;

subscript_expr:	expr                    { $$ = $1;  }
		|		".."                    { $$ = MKNODE(BIN_OP, .lhs=NULL, .rhs=NULL, .op=CFG_OP_RANGE); }
		|		expr ".."               { $$ = MKNODE(BIN_OP, .lhs=$1,   .rhs=NULL, .op=CFG_OP_RANGE); }
		|		".." expr               { $$ = MKNODE(BIN_OP, .lhs=NULL, .rhs=$2,   .op=CFG_OP_RANGE); }
		|		expr ".." expr          { $$ = MKNODE(BIN_OP, .lhs=$1,   .rhs=$3,   .op=CFG_OP_RANGE); }
		;

l_expr:			ident                   { $$ = $1; }
		|		l_expr '.' ident        { $$ = MKNODE(ACCESS, .lhs=$1, .rhs=$3); }
		|		l_expr '[' subscript_expr ']'
					{ $$ = MKNODE(SUBSCRIPT, .lhs=$1, .index=$3); }
		;

type_expr:		l_expr                  { $$ = $1; }
		|		'$' IDENTIFIER          { $$ = MKNODE(TEMPLATE_VAR, .name=$2); }
		|		'$' IDENTIFIER '[' expr ']'
					{ $$ = MKNODE(SUBSCRIPT, .lhs=MKNODE(TEMPLATE_VAR, .name=$2), .index=$4); }
		|		func_proto              { $$ = $1; }
		|		tuple_decl              { $$ = $1; }
		//		Array declarations are handled by l_expr
		//		 |		array_decl
		|		func_call               { $$ = $1; }
		;

lambda:			'\\' ident expr1       { $$ = MKNODE(LAMBDA, .proto=$2, .body=$3); }
		|		'\\' tuple_decl expr1  { $$ = MKNODE(LAMBDA, .proto=$2, .body=$3); }
				/* '\\' func_proto expr1 */
		/* |		'\\' '(' func_params ')' expr */
		;

func_proto:		tuple_decl "->" type_expr
					{ $$ = MKNODE(FUNC_PROTO, .params=$1, .ret=$3); }
		;

func_call:		l_expr tuple_lit       { $$ = MKNODE(FUNC_CALL, .ident=$1, .params=$2); }
		;


tuple_decl:		'(' named_tuple_decl ')'       { $$ = MKNODE(TUPLE_DECL, .items=$2, .named=true);  }
		|		'(' named_tuple_decl ',' ')'   { $$ = MKNODE(TUPLE_DECL, .items=$2, .named=true);  }
		|		'(' unnamed_tuple_decl ')'     { $$ = MKNODE(TUPLE_DECL, .items=$2, .named=false); }
		|		'(' unnamed_tuple_decl ',' ')' { $$ = MKNODE(TUPLE_DECL, .items=$2, .named=false); }
		;

named_tuple_decl:
				named_tuple_decl ',' named_tuple_decl_item
					{ $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		named_tuple_decl_item          { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

named_tuple_decl_item:
				IDENTIFIER ':' type_expr       { $$ = MKNODE(TUPLE_DECL_ITEM, .name=$1, .type=$3); }
		;

unnamed_tuple_decl:
				unnamed_tuple_decl ',' unnamed_tuple_decl_item
					{ $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		unnamed_tuple_decl_item        { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

unnamed_tuple_decl_item:
				type_expr                      { $$ = MKNODE(TUPLE_DECL_ITEM, .name=NULL, .type=$1); }
		;

tuple_lit:		'(' named_tuple_lit ')'        { $$ = MKNODE(TUPLE_LIT, .items=$2, .named=true); }
		|		'(' named_tuple_lit ',' ')'    { $$ = MKNODE(TUPLE_LIT, .items=$2, .named=true); }
		/* |		'(' named_tuple_lit_item ',' "..." ')' */
		|		'(' unnamed_tuple_lit ')'      { $$ = MKNODE(TUPLE_LIT, .items=$2, .named=false); }
		|		'(' unnamed_tuple_lit ',' ')'  { $$ = MKNODE(TUPLE_LIT, .items=$2, .named=false); }
		/* |		'(' unnamed_tuple_lit_item ',' "..." ')' */
		;

named_tuple_lit:
				named_tuple_lit ',' named_tuple_lit_item
					{ $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		named_tuple_lit_item           { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

named_tuple_lit_item:
				IDENTIFIER '=' expr            { $$ = MKNODE(TUPLE_LIT_ITEM, .name=$1, .value=$3); }
		;

unnamed_tuple_lit:
				unnamed_tuple_lit ',' unnamed_tuple_lit_item
					{ $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		unnamed_tuple_lit_item         { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

unnamed_tuple_lit_item:
				expr                           { $$ = MKNODE(TUPLE_LIT_ITEM, .name=NULL, .value=$1); }
		;

/* array_decl:		type_expr '[' expr ']' */
/* 		; */

array_lit:		'[' array_lit_body ']'         { $$ = MKNODE(ARRAY_LIT, .items=$2); }
		|		'[' array_lit_body ',' ']'     { $$ = MKNODE(ARRAY_LIT, .items=$2); }
		|		'[' ']'                        { $$ = MKNODE(ARRAY_LIT, .items=NULL); }
		;

array_lit_body:
				array_lit_body ',' array_lit_item
					{ $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		array_lit_item                 { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

array_lit_item:	expr                           { $$ = $1; }
		;

ident:			IDENTIFIER
					{ $$ = alloc_node(ctx, CFG_NODE_IDENT); $$->IDENT = $1; }
		;

numlit:			NUMLIT
					{ $$ = alloc_node(ctx, CFG_NODE_NUM_LIT); $$->NUM_LIT = $1; }
		;




/* module: */
/* 				"version" NUMLIT '.' NUMLIT '.' NUMLIT ';' */
/* 				module_stmt_list { */
/* 	$$ = alloc_node(ctx, CONFIG_NODE_MODULE); */
/* 	$$->module.first_child = $8; */
/* 	$$->module.version.major = $2; */
/* 	$$->module.version.minor = $4; */
/* 	$$->module.version.patch = $6; */

/* 	assert(!ctx->module); */
/* 	ctx->module = $$; */
/*  } */
/* 		; */
/* module_stmt_list: */
/* 				module_stmt_list module_stmt { $$ = $1; append_child(&$$, $2); } */
/* 		|		%empty                       { $$ = NULL; } */
/* 		; */
/* module_stmt: 	device_type */
/* 		|		device */
/* 		|		type_decl */
/* 		|		namespace */
/* 		|		bind_stmt */
/* 		; */
/* device_type:	"device_type" IDENTIFIER device_type_params '{' device_type_body '}' { */
/* 					$$ = alloc_node(ctx, CONFIG_NODE_DEVICE_TYPE); */
/* 					$$->device_type.name = $2; */
/* 					$$->device_type.params = $3; */
/* 					$$->device_type.first_child = $5; */
/* 				} */
/* 		; */
/* device_type_params: */
/* 				'(' named_tuple_list ')' { */
/* 					$$ = alloc_node(ctx, CONFIG_NODE_TUPLE); */
/* 					$$->tuple.first_child = $2; */
/* 					$$->tuple.named = true; */
/* 				} */
/* 		|		%empty { $$ = NULL; } */
/* 		; */
/* device_type_body: */
/* 				device_type_body device_type_body_stmt { $$ = $1; append_child(&$$, $2); } */
/* 		|		%empty                                 { $$ = NULL; } */
/* 		; */
/* device_type_body_stmt: */
/* 				"input"  IDENTIFIER ':' type ';'           { $$ = alloc_node(ctx, CONFIG_NODE_INPUT);     $$->input.name = $2;  $$->input.type = $4; $$->input.def = false; } */
/* 		|		"input" "default" IDENTIFIER ':' type ';'  { $$ = alloc_node(ctx, CONFIG_NODE_INPUT);     $$->input.name = $3;  $$->input.type = $5; $$->input.def = true; } */
/* 		|		"output" IDENTIFIER ':' type ';'           { $$ = alloc_node(ctx, CONFIG_NODE_OUTPUT);    $$->output.name = $2; $$->output.type = $4; $$->output.def = false; } */
/* 		|		"output" "default" IDENTIFIER ':' type ';' { $$ = alloc_node(ctx, CONFIG_NODE_OUTPUT);    $$->output.name = $3; $$->output.type = $5; $$->output.def = true; } */
/* 		|		"attr" IDENTIFIER ':' type   ';'           { $$ = alloc_node(ctx, CONFIG_NODE_ATTR);      $$->attr.name = $2;   $$->attr.type = $4; } */
/* 		|		"attr" IDENTIFIER ':' type   '=' expr ';'  { $$ = alloc_node(ctx, CONFIG_NODE_ATTR);      $$->attr.name = $2;   $$->attr.type = $4; $$->attr.def_value = $6; } */
/* 		|		bind_stmt */
/* 		|		l_expr '=' expr ';'                     { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ASSIGN; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		device                                     { $$ = $1; } */
/* 		|		type_decl                                  { $$ = $1; } */
/* 		; */
/* device:			"device" l_expr device_name device_args device_body { */
/* 					$$ = alloc_node(ctx, CONFIG_NODE_DEVICE); */
/* 					$$->device.type = $2; */
/* 					$$->device.name = $3; */
/* 					$$->device.args = $4; */
/* 					$$->device.first_child = $5; */
/* 				} */
/* 		; */
/* device_name:	IDENTIFIER */
/* 		|		%empty                        { $$ = NULL; } */
/* 		; */
/* device_args:	named_tuple_lit */
/* 		|		tuple_lit */
/* 		|		%empty                        { $$ = NULL; } */
/* 		; */
/* device_body:	'{' device_body_stmt_list '}' { $$ = $2; } */
/* 		|		';'                           { $$ = NULL; } */
/* 		; */
/* device_body_stmt_list: */
/* 				device_body_stmt_list device_body_stmt { $$ = $1; append_child(&$$, $2); } */
/* 		|		%empty                       { $$ = NULL; } */
/* 		; */
/* device_body_stmt: */
/* 				l_expr '=' expr ';'          { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ASSIGN; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		bind_stmt                    { $$ = $1; } */
/* 		|		device                       { $$ = $1; } */
/* 		; */
/* bind_stmt:		l_expr "<-" expr ';'         { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_BIND;   $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		//		|		l_expr "<-"   expr ';'       { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_BIND;   $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		; */
/* l_expr:			IDENTIFIER                   { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); $$->ident = $1; } */
/* 		|		'_'                          { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); } */
/* 		|		l_expr '.' l_expr            { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ACCESS; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		l_expr '[' expr ']'          { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_SUBSCRIPT; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		l_expr '[' range ']'         { $$ = alloc_node(ctx, CONFIG_NODE_SUBSCRIPT_RANGE); $$->subscript_range.lhs = $1; $$->subscript_range.low = $3.low; $$->subscript_range.high = $3.high; } */
/* 		; */
/* type_decl:		"type" IDENTIFIER '=' type ';'   { $$ = alloc_node(ctx, CONFIG_NODE_TYPE_DECL); $$->type_decl.name = $2; $$->type_decl.type = $4; }; */

/* type: 			type_l_expr                  { $$ = $1; } */
/* 		|		'$' type_l_expr              { $$ = alloc_node(ctx, CONFIG_NODE_TYPE_TEMPLATE_PARAM); $$->type_template_param.expr = $2; } */
/* 		|		array_type                   { $$ = alloc_node(ctx, CONFIG_NODE_TYPE); $$->type_def.first_child = $1; } */
/* 		|		subrange_type                { $$ = alloc_node(ctx, CONFIG_NODE_TYPE); $$->type_def.first_child = $1; } */
/* 		|		'{' enum_list '}'            { $$ = NULL; printf("TODO: enumlist\n"); } */
/* 		|		'(' tuple_decl ')'           { $$ = alloc_node(ctx, CONFIG_NODE_TYPE); $$->type_def.first_child = $2; } */
/* 		|		"type"                       { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); $$->ident = atom_create(ctx->atom_table, STR("type")); } */
/* 		; */

/* type_l_expr:	IDENTIFIER                   { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); $$->ident = $1; } */
/* 		|		'_'                          { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); } */
/* 		|		type_l_expr '.' type_l_expr  { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ACCESS; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		; */

/* array_type:		type '[' expr ']'            { $$ = alloc_node(ctx, CONFIG_NODE_ARRAY_TYPE); $$->array_type.lhs = $1; $$->array_type.length = $3; $$->array_type.template_length = false; } */
/* 		|		type '[' '$' l_expr ']'      { $$ = alloc_node(ctx, CONFIG_NODE_ARRAY_TYPE); $$->array_type.lhs = $1; $$->array_type.length = $4; $$->array_type.template_length = true;  } */
/* 		; */

/* subrange_type:	type_l_expr '{' range '}'    { $$ = alloc_node(ctx, CONFIG_NODE_SUBRANGE); $$->subrange.lhs = $1; $$->subrange.low = $3.low; $$->subrange.high = $3.high; } */
/* 		; */
/* enum_list: 		enum_label                   { $$ = NULL; } */
/* 		| 		enum_list ',' enum_label     { $$ = NULL; } */
/* 		; */
/* enum_label:		IDENTIFIER                   { $$ = NULL; } */
/* 		; */

/* tuple_decl:   	tuple_list                   { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE); $$->tuple.first_child = $1; $$->tuple.named = false; } */
/* 		|		named_tuple_list             { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE); $$->tuple.first_child = $1; $$->tuple.named = true;  } */
/* 		; */
/* tuple_list:		tuple_item                   { $$ = make_list(ctx, $1, NULL); } */
/* 		|		tuple_list ',' tuple_item    { $$ = make_list(ctx, $3, $1); } */
/* 		; */
/* named_tuple_list: */
/* 				named_tuple_item                      { $$ = make_list(ctx, $1, NULL); } */
/* 		|		named_tuple_list ',' named_tuple_item { $$ = make_list(ctx, $3, $1); } */
/* 		; */
/* tuple_item: 	type                         { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE_ITEM); $$->tuple_item.name = NULL; $$->tuple_item.type = $1; } */
/* 		; */
/* named_tuple_item: */
/* 				IDENTIFIER ':' type          { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE_ITEM); $$->tuple_item.name = $1; $$->tuple_item.type = $3; } */
/* 		; */


/* range:			expr ".." expr     { $$.low = $1;   $$.high = $3; } */
/* 		|		expr ".."          { $$.low = $1;   $$.high = NULL; } */
/* 		|		     ".." expr     { $$.low = NULL; $$.high = $2; } */
/* 		|		     ".."          { $$.low = NULL; $$.high = NULL; } */
/* 		; */
/* expr:			NUMLIT             { $$ = alloc_node(ctx, CONFIG_NODE_NUMLIT); $$->numlit = $1; } */
/* 		|		IDENTIFIER         { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); $$->ident = $1; } */
/* 		|		tuple_lit */
/* 		|		named_tuple_lit */
/* 		|		array_lit */
/* 		|		expr '.' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ACCESS;    $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		expr '+' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ADD;       $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		expr '-' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_SUB;       $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		expr '*' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_MUL;       $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		expr '/' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_DIV;       $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		expr '[' expr ']'  { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_SUBSCRIPT; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; } */
/* 		|		expr '[' range ']' { $$ = alloc_node(ctx, CONFIG_NODE_SUBSCRIPT_RANGE); $$->subscript_range.lhs = $1; $$->subscript_range.low = $3.low; $$->subscript_range.high = $3.high; } */
/* 		; */

/* named_tuple_lit: */
/* 				'(' named_tuple_lit_body ')' { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE_LIT); $$->tuple_lit.named = true; $$->tuple_lit.first_child = $2; } */
/* 		; */
/* named_tuple_lit_body: */
/* 				named_tuple_lit_item         { $$ = make_list(ctx, $1, NULL); } */
/* 		|		named_tuple_lit_body ',' named_tuple_lit_item { $$ = make_list(ctx, $3, $1); } */
/* 		; */
/* named_tuple_lit_item: */
/* 				IDENTIFIER '=' expr          { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE_LIT_ITEM); $$->tuple_lit_item.name = $1; $$->tuple_lit_item.expr = $3; } */
/* 		; */

/* tuple_lit:		'(' tuple_lit_body ')' { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE_LIT); $$->tuple_lit.named = false; $$->tuple_lit.first_child = $2; } */
/* 		; */
/* tuple_lit_body: */
/* 				tuple_lit_item               { $$ = make_list(ctx, $1, NULL); } */
/* 		|		tuple_lit_body ',' tuple_lit_item { $$ = make_list(ctx, $3, $1); } */
/* 		; */
/* tuple_lit_item:	expr                         { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE_LIT_ITEM); $$->tuple_lit_item.name = NULL; $$->tuple_lit_item.expr = $1; } */
/* 		; */

/* array_lit:		'[' array_lit_body ']' { $$ = alloc_node(ctx, CONFIG_NODE_ARRAY_LIT); $$->array_lit.first_child = $2; } */
/* 		; */
/* array_lit_body: */
/* 				expr                    { $$ = make_list(ctx, $1, NULL); } */
/* 		|		array_lit_body ',' expr { $$ = make_list(ctx, $3, $1); } */
/* 		; */

/* namespace: 		"namespace" IDENTIFIER '{' module_stmt_list '}' { $$ = alloc_node(ctx, CONFIG_NODE_NAMESPACE); $$->namespace.name = $2; $$->namespace.first_child = $4; } */
/* 		; */

%%

#define CURRENT_TOKEN STR_BE(ctx->tok, ctx->cur)
#define CURRENT_LEN (ctx->cur - ctx->tok)

void _yydebug_print(int state, char sym) {
	printf("%i '%c'\n", state, sym);
}
#undef YYDEBUG
#define YYDEBUG _yydebug_print

static void lloc_col(YYLTYPE *lloc, int col) {
	lloc->first_line = lloc->last_line;
	lloc->first_column = lloc->last_column;
	lloc->last_column += col;
}

static void lloc_line(YYLTYPE *lloc) {
	lloc->last_line += 1;
	lloc->first_line = lloc->last_line;
	lloc->last_column = 1;
	lloc->first_column = 1;
}

int yylex(YYSTYPE *lval, YYLTYPE *lloc, struct lex_context *ctx) {
	ctx->tok = ctx->cur;
	char *YYMARKER = 0;

%{ /* Begin re2c lexer */
re2c:yyfill:enable = 1;
re2c:define:YYCTYPE = "char";
re2c:define:YYCURSOR = "ctx->cur";
re2c:define:YYLIMIT = "ctx->lim";
re2c:define:YYFILL = "if (!config_parse_fill(ctx, @@)) return END;";
re2c:define:YYFILL:naked = 1;

/* "version"     { lloc_col(lloc, CURRENT_LEN); return VERSION; } */
/* "device_type" { lloc_col(lloc, CURRENT_LEN); return DEVICETYPE; } */
/* "device"      { lloc_col(lloc, CURRENT_LEN); return DEVICE; } */
/* "type"        { lloc_col(lloc, CURRENT_LEN); return TYPE; } */
/* "input"       { lloc_col(lloc, CURRENT_LEN); return INPUT; } */
/* "output"      { lloc_col(lloc, CURRENT_LEN); return OUTPUT; } */
/* "attr"        { lloc_col(lloc, CURRENT_LEN); return ATTR; } */
/* "default"     { lloc_col(lloc, CURRENT_LEN); return DEFAULT; } */
"Enum"        { lloc_col(lloc, CURRENT_LEN); return ENUM; }
"namespace"   { lloc_col(lloc, CURRENT_LEN); return NAMESPACE; }
"use"         { lloc_col(lloc, CURRENT_LEN); return USE; }
".."          { lloc_col(lloc, CURRENT_LEN); return RANGE; }
/* "..."         { lloc_col(lloc, CURRENT_LEN); return ELLIPSIS; } */
"<-"          { lloc_col(lloc, CURRENT_LEN); return BIND_LEFT; }
"->"          { lloc_col(lloc, CURRENT_LEN); return BIND_RIGHT; }
"::"          { lloc_col(lloc, CURRENT_LEN); return DECL; }
"=="          { lloc_col(lloc, CURRENT_LEN); return EQ; }
"!="          { lloc_col(lloc, CURRENT_LEN); return NEQ; }
"<="          { lloc_col(lloc, CURRENT_LEN); return LTE; }
">="          { lloc_col(lloc, CURRENT_LEN); return GTE; }

"\x00"        { lloc_col(lloc, CURRENT_LEN); return END; }
"\r\n" | [\r\n] { lloc_line(lloc); return yylex(lval, lloc, ctx); }
 "#" [^\r\n]*  { lloc_col(lloc, CURRENT_LEN); return yylex(lval, lloc, ctx); }
[\t\v\b\f ]   { lloc_col(lloc, CURRENT_LEN); return yylex(lval, lloc, ctx); }

[a-zA-Z][a-zA-Z0-9_]* | [a-zA-Z_][a-zA-Z0-9_]+ {
	lloc_col(lloc, CURRENT_LEN);
	lval->IDENTIFIER = atom_create(ctx->atom_table, CURRENT_TOKEN);
	return IDENTIFIER;
 }
'0b' [01]+    {
	lloc_col(lloc, CURRENT_LEN);
	lval->NUMLIT = string_to_int64_base2(CURRENT_TOKEN);
	return NUMLIT;
 }
[1-9][0-9]* | '0'   {
	lloc_col(lloc, CURRENT_LEN);
	lval->NUMLIT = string_to_int64_base10(CURRENT_TOKEN);
	return NUMLIT;
 }
'0x' [0-9a-fA-F]+ {
	lloc_col(lloc, CURRENT_LEN);
	lval->NUMLIT = string_to_int64_base16(CURRENT_TOKEN);
	return NUMLIT;
 }

[-+*/:;={}()\[\].,_$@\\] {
	lloc_col(lloc, CURRENT_LEN);
	return *ctx->tok;
 }

"op" ([-+*/:;={}()\[\].,_$@\\] | "->" | "<-" | "==" | "!=" | "<=" | ">=") {
	lloc_col(lloc, CURRENT_LEN);
	lval->IDENTIFIER = atom_create(ctx->atom_table, CURRENT_TOKEN);
	return IDENTIFIER;
 }

* {
	lloc_col(lloc, CURRENT_LEN);
	printf("unexpected char '%c' %i\n", *ctx->tok, *ctx->tok);
	return END;
 }

%} /* End lexer */
}

void yyerror(YYLTYPE *lloc, struct lex_context *ctx, const char *error) {
	printf("Error %i:%i: %s\n",
		   lloc->first_line, lloc->first_column, error);
}

void config_tree_clean(struct config_node *tree);

int parse_config_file(struct string filename, struct atom_table *table, struct arena *memory, struct config_node **out_node) {
	struct lex_context ctx;

	memset(&ctx, 0, sizeof(struct lex_context));

	ctx.lim = ctx.buf + BUFFER_SIZE;
	ctx.cur = ctx.lim;
	ctx.tok = ctx.lim;
	ctx.eof = false;
	ctx.atom_table = table;
	ctx.memory = memory;

	ctx.fp = fopen(filename.text, "rb");

	if (!ctx.fp) {
		perror("config");
		return -1;
	}

	config_parse_fill(&ctx, BUFFER_SIZE);

	yydebug = 0;

	int err;
	err = yyparse(&ctx);

	if (err) {
		return err;
	}

	cfg_tree_print(ctx.module);

	/* config_tree_clean(ctx.module); */
	/* *out_node = ctx.module; */
	*out_node = NULL;

	return 0;
}
