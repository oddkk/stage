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
		unsigned int file_id;
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

#define alloc_node(ctx, type) _alloc_node(ctx, type, yylloc)

	int yylex(YYSTYPE *, YYLTYPE *, struct lex_context *);
	void yyerror(YYLTYPE *loc, struct lex_context *, const char *);

	struct cfg_node *_alloc_node(struct lex_context *ctx, enum cfg_node_type type, YYLTYPE loc) {
		struct cfg_node *res;

		res = arena_alloc_struct(ctx->memory, struct cfg_node);
		res->type = type;

		res->file_id     = ctx->file_id;
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

#include "config_nodes.h"
#undef CFG_NODE

#define MKNODE(type, ...) _mknode##type(ctx, yylloc, (type##_t){__VA_ARGS__})
}

%token END 0
%token IDENTIFIER NUMLIT STRINGLIT
%token NAMESPACE "namespace" ENUM "Enum" USE "use" ASSERT "assert"
%token BIND_LEFT "<-" BIND_RIGHT "->" RANGE ".." DECL "::" // ELLIPSIS "..."
%token EQ "==" NEQ "!=" LTE "<=" GTE ">="

%type <struct cfg_node *> module stmt_list stmt stmt1 attr_list attr
%type <struct cfg_node *> decl_stmt decl enum_items enum_item use_stmt
%type <struct cfg_node *> use_expr use_expr1 func_stmt assign_stmt assert_stmt
%type <struct cfg_node *> bind_stmt bind_left_expr namespace_stmt
%type <struct cfg_node *> namespace_ident expr expr1 subscript_expr
%type <struct cfg_node *> l_expr type_expr lambda func_proto func_call
%type <struct cfg_node *> tuple_decl named_tuple_decl named_tuple_decl_item
%type <struct cfg_node *> unnamed_tuple_decl unnamed_tuple_decl_item
%type <struct cfg_node *> tuple_lit named_tuple_lit named_tuple_lit_item
%type <struct cfg_node *> unnamed_tuple_lit unnamed_tuple_lit_item
%type <struct cfg_node *> array_lit array_lit_body array_lit_item
%type <struct cfg_node *> ident numlit strlit


%type <struct atom *> IDENTIFIER
%type <struct string> STRINGLIT
%type <int64_t> NUMLIT

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

attr:			expr               { $$ = $1; }
		;

stmt1:			';'                { $$ = NULL; }
		|		decl_stmt          { $$ = $1; }
		|		namespace_stmt     { $$ = $1; }
		|		func_stmt      ';' { $$ = $1; }
		|		assign_stmt    ';' { $$ = $1; }
		|		bind_stmt      ';' { $$ = $1; }
		|		use_stmt       ';' { $$ = $1; }
		|		assert_stmt    ';' { $$ = $1; }
		|		error          ';' { $$ = NULL; }
		;

decl_stmt:		ident "::" decl            { $$ = MKNODE(DECL_STMT, .name=$1, .decl=$3); }
		|		ident tuple_decl "::" decl { $$ = MKNODE(DECL_STMT, .name=$1, .decl=$4, .args=$2); }
		;

decl:			l_expr ';'                 { $$ = $1; }
		|		"Enum" '{' enum_items '}'  { $$ = MKNODE(ENUM_DECL, .items=$3); }
		|		"Enum" '{' enum_items ',' '}'
					{ $$ = MKNODE(ENUM_DECL, .items=$3); }
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

assert_stmt:	"assert" expr              { $$ = NULL; }
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
		|		strlit                   { $$ = $1; }
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

strlit:			STRINGLIT
					{ $$ = alloc_node(ctx, CFG_NODE_STR_LIT); $$->STR_LIT = $1; }
		;

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
"assert"      { lloc_col(lloc, CURRENT_LEN); return ASSERT; }
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

"\"" [^\"]* "\"" {
	lloc_col(lloc, CURRENT_LEN);
	string_duplicate(ctx->memory, &lval->STRINGLIT, CURRENT_TOKEN);
	return STRINGLIT;
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

void cfg_tree_clean(struct cfg_node **tree);

int parse_config_file(struct string filename,
					  struct atom_table *table,
					  struct arena *memory,
					  unsigned int file_id,
					  struct cfg_node **out_node) {
	struct lex_context ctx;

	memset(&ctx, 0, sizeof(struct lex_context));

	ctx.lim = ctx.buf + BUFFER_SIZE;
	ctx.cur = ctx.lim;
	ctx.tok = ctx.lim;
	ctx.eof = false;
	ctx.atom_table = table;
	ctx.memory = memory;
	ctx.file_id = file_id;

	ctx.fp = fopen(filename.text, "rb");

	if (!ctx.fp) {
		perror("open");
		return -1;
	}

	config_parse_fill(&ctx, BUFFER_SIZE);

	/* yydebug = 1; */

	int err;
	err = yyparse(&ctx);

	if (err) {
		return err;
	}

	cfg_tree_clean(&ctx.module);
	/* cfg_tree_print(ctx.module); */

	*out_node = ctx.module;

	return 0;
}
