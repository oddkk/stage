%define api.value.type union
%define api.pure full
%define parse.error verbose
%locations

%code requires
{
	#include "syntax_tree.h"
	#include "str.h"
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

typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
	int first_line;
	int first_column;
	int last_line;
	int last_column;
	size_t byte_from;
	size_t byte_to;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
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
		size_t buffer_begin;
		size_t buffer_end;
		struct st_node *module;
		file_id_t file_id;
		struct stg_error_context *err;
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
		size_t bytes_read = fread(ctx->lim, 1, free, ctx->fp);
		ctx->lim += bytes_read;
		ctx->buffer_begin = ctx->buffer_end;
		ctx->buffer_end += bytes_read;
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

	struct st_node *_alloc_node(struct lex_context *ctx, enum st_node_type type, YYLTYPE loc) {
		struct st_node *res;

		res = arena_alloc_struct(ctx->memory, struct st_node);
		res->type = type;

		res->loc.file_id     = ctx->file_id;
		res->loc.line_from   = loc.first_line;
		res->loc.col_from    = loc.first_column;

		res->loc.line_to     = loc.last_line;
		res->loc.col_to      = loc.last_column;

		res->loc.byte_from   = loc.byte_from;
		res->loc.byte_to     = loc.byte_to;

		return res;
	}

#define ST_NODE(name, data) \
	struct st_node *_mknode##name(struct lex_context *ctx,	\
								   YYLTYPE loc,				\
								   name##_t value) {		\
		struct st_node *result;							\
		result = _alloc_node(ctx, ST_NODE_##name, loc);	\
		result->name = value;								\
		return result;										\
	}

#include "syntax_tree_node_defs.h"
#undef ST_NODE

#define MKNODE(type, ...) _mknode##type(ctx, yylloc, (type##_t){__VA_ARGS__})
}

%token END 0
%token IDENTIFIER NUMLIT STRINGLIT
%token NAMESPACE "namespace" ENUM "Enum" USE "use"
%token BIND_LEFT "<-" BIND_RIGHT "->" RANGE ".." DECL "::" // ELLIPSIS "..."
%token EQ "==" NEQ "!=" LTE "<=" GTE ">=" LAMBDA "=>"

%type <struct st_node *> module stmt_list stmt stmt1 func_decl func_proto func_params func_params1 func_param expr expr1	subscript_expr l_expr ident numlit strlit use_stmt use_expr use_expr1 func_call func_args func_args1 func_arg assign_stmt special special_args special_args1 special_arg


%type <struct atom *> IDENTIFIER
%type <struct string> STRINGLIT
%type <int64_t> NUMLIT

%right "->"
%left "<-"
%left '<' '>' "<=" ">=" "!=" "=="
%nonassoc ".."
%left '+' '-'
%left '*' '/'
%left '.' '[' '{' '('

%start module

%%

module:			stmt_list {
	$$ = MKNODE(MODULE, .body=$1);
	assert(ctx->module == NULL);
	ctx->module = $$;
 }
		;

stmt_list:		stmt_list stmt {
	if ($2 != NULL) {
		$$ = MKNODE(INTERNAL_LIST, .head=$2, .tail=$1);
	} else {
		$$ = $1;
	}
}
		|		%empty                      { $$ = NULL; }
		;


stmt:			stmt1					{ $$ = MKNODE(STMT, .stmt=$1); }
		;

stmt1:			';'                     { $$ = NULL; }
		|		use_stmt    ';'         { $$ = $1; }
		|		assign_stmt ';'         { $$ = $1; }
		|		expr        ';'         { $$ = $1; }
		|		error       ';'         { $$ = NULL; }
		;

assign_stmt:
		  		ident ':' expr          { $$ = MKNODE(ASSIGN_STMT, .ident=$1, .type=$3,   .body=NULL); }
		|		ident ':'      '=' expr { $$ = MKNODE(ASSIGN_STMT, .ident=$1, .type=NULL, .body=$4  ); }
		|		ident ':' expr '=' expr { $$ = MKNODE(ASSIGN_STMT, .ident=$1, .type=$3,   .body=$5  ); }
		;

special: 		'@' IDENTIFIER '(' special_args ')' { $$ = MKNODE(SPECIAL, .name=$2, .args=$4  ); }
		;

special_args:	special_args1 ','                   { $$ = $1; }
		|		special_args1                       { $$ = $1; }
		;

special_args1:	special_args1 ',' special_arg       { $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		special_arg                         { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

special_arg:	numlit
		|		strlit
		;

func_decl:		func_proto "=>" expr    { $$ = MKNODE(LAMBDA, .proto=$1, .body=$3, .special=false); }
		|		func_proto special		{ $$ = MKNODE(LAMBDA, .proto=$1, .body=$2, .special=true ); }
		;

func_proto:		'(' func_params ')'             { $$ = MKNODE(FUNC_PROTO, .params=$2, .ret=NULL); }
		|		'(' func_params ')' "->" expr   { $$ = MKNODE(FUNC_PROTO, .params=$2, .ret=$5  ); }
		;

func_params:	func_params1                { $$ = MKNODE(TUPLE_DECL, .items=$1, .named=true); }
		|		func_params1 ','            { $$ = MKNODE(TUPLE_DECL, .items=$1, .named=true); }
		;

func_params1:	func_params1 ',' func_param { $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1  ); }
		|		func_param                  { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

func_param:		IDENTIFIER ':' expr         { $$ = MKNODE(TUPLE_DECL_ITEM, .name=$1, .type=$3); }
		/*|		ident ':' expr '=' expr TODO: Default value */
		;

/*
decl_stmt:		ident "::" decl            { $$ = MKNODE(DECL_STMT, .name=$1, .decl=$3); }
		|		ident tuple_decl "::" decl { $$ = MKNODE(DECL_STMT, .name=$1, .decl=$4, .args=$2); }
		;

decl:			l_expr ';'                 { $$ = $1; }
		|		"Enum" '{' enum_items '}'  { $$ = MKNODE(ENUM_DECL, .items=$3); }
		|		"Enum" '{' enum_items ',' '}'
					{ $$ = MKNODE(ENUM_DECL, .items=$3); }
		|		"Enum" '{' error '}'       { $$ = NULL; }
		|		l_expr '{' stmt_list '}'   { $$ = MKNODE(OBJ_DECL, .ident=$1, .body=$3); }
		|		tuple_decl                 { $$ = $1; }
		;

enum_items:		enum_items ',' enum_item   { $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		enum_item                  { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

enum_item:		IDENTIFIER                 { $$ = MKNODE(ENUM_ITEM, .name=$1); }
		|		IDENTIFIER tuple_decl      { $$ = MKNODE(ENUM_ITEM, .name=$1, .data=$2); }
		;

*/

use_stmt:		"use" use_expr             { $$ = MKNODE(USE_STMT, .ident=$2); }
		;

use_expr:		use_expr1                  { $$ = $1; }
		|		use_expr1 '.' '*'
					{ $$ = MKNODE(ACCESS, .lhs=$1, .rhs=alloc_node(ctx, ST_NODE_USE_ALL)); }
		;

use_expr1:		use_expr1 '.' ident        { $$ = MKNODE(ACCESS, .lhs=$1, .rhs=$3); }
		|		ident                      { $$ = $1; }
		;

/*
namespace_stmt:	"namespace" namespace_ident '{' stmt_list '}'
					{ $$ = MKNODE(NAMESPACE, .name=$2, .body=$4); }
		;

namespace_ident:
				ident                    { $$ = $1; }
		|		namespace_ident '.' ident
					{ $$ = MKNODE(ACCESS, .lhs=$1, .rhs=$3); }
		;
*/

expr:			expr1                   { $$ = $1; }
		|		func_decl				{ $$ = $1; }
		;

expr1:			l_expr                  { $$ = $1; }
		|		numlit                  { $$ = $1; }
		/*|		numlit ident            { $$ = $1; } TODO: suffix "operators" */
		|		strlit                  { $$ = $1; }
		|		func_call               { $$ = $1; }
		/*|		func_proto              { $$ = $1; } */
		|		special                 { $$ = $1; }
		|		'$' IDENTIFIER          { $$ = MKNODE(TEMPLATE_VAR, .name=$2); }

		|		expr1 '+'  expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_ADD); }
		|		expr1 '-'  expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_SUB); }
		|		expr1 '*'  expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_MUL); }
		|		expr1 '/'  expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_DIV); }
		|		expr1 "==" expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_EQ);  }
		|		expr1 "!=" expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_NEQ); }
		|		expr1 "<=" expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_LTE); }
		|		expr1 ">=" expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_GTE); }
		|		expr1 '<'  expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_LT);  }
		|		expr1 '>'  expr1        { $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_GT);  }
		|		expr1 "->" expr1        { $$ = MKNODE(BIND, .src=$1, .drain=$3);  }
		|		expr1 "<-" expr1        { $$ = MKNODE(BIND, .drain=$1, .src=$3);  }
		|		'(' expr ')'            { $$ = $2;  }
		;

subscript_expr:	expr                    { $$ = $1;  }
		|		".."                    { $$ = MKNODE(BIN_OP, .lhs=NULL, .rhs=NULL, .op=ST_OP_RANGE); }
		|		expr ".."               { $$ = MKNODE(BIN_OP, .lhs=$1,   .rhs=NULL, .op=ST_OP_RANGE); }
		|		".." expr               { $$ = MKNODE(BIN_OP, .lhs=NULL, .rhs=$2,   .op=ST_OP_RANGE); }
		|		expr ".." expr          { $$ = MKNODE(BIN_OP, .lhs=$1,   .rhs=$3,   .op=ST_OP_RANGE); }
		;

l_expr:			ident                   { $$ = $1; }
		|		l_expr '.' ident        { $$ = MKNODE(ACCESS, .lhs=$1, .rhs=$3); }
		|		l_expr '[' subscript_expr ']'
		{ $$ = MKNODE(BIN_OP, .lhs=$1, .rhs=$3, .op=ST_OP_SUBSCRIPT); }
		;

func_call:		expr1 '(' func_args ')' { $$ = MKNODE(FUNC_CALL, .ident=$1, .params=$3); }
		;

func_args:		func_args1              { $$ = MKNODE(TUPLE_LIT, .items=$1, .named=false); }
		|		func_args1 ','          { $$ = MKNODE(TUPLE_LIT, .items=$1, .named=false); }
		;

func_args1:		func_args1 ',' func_arg { $$ = MKNODE(INTERNAL_LIST, .head=$3, .tail=$1); }
		|		func_arg                { $$ = MKNODE(INTERNAL_LIST, .head=$1, .tail=NULL); }
		;

func_arg: 		expr                    { $$ = MKNODE(TUPLE_LIT_ITEM, .name=NULL, .value=$1); }
		/*|		ident '=' expr TODO: named args */
		;

/*
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
*/

ident:			IDENTIFIER
					{ $$ = alloc_node(ctx, ST_NODE_IDENT); $$->IDENT = $1; }
		;

numlit:			NUMLIT
					{ $$ = alloc_node(ctx, ST_NODE_NUM_LIT); $$->NUM_LIT = $1; }
		;

strlit:			STRINGLIT
					{ $$ = alloc_node(ctx, ST_NODE_STR_LIT); $$->STR_LIT = $1; }
		;

%%

#define CURRENT_TOKEN STR_BE(ctx->tok, ctx->cur)
#define CURRENT_LEN (ctx->cur - ctx->tok)

void _yydebug_print(int state, char sym) {
	printf("%i '%c'\n", state, sym);
}
#undef YYDEBUG
#define YYDEBUG _yydebug_print

static void lloc_col(struct lex_context *ctx, YYLTYPE *lloc, int col) {
	lloc->first_line = lloc->last_line;
	lloc->first_column = lloc->last_column;
	lloc->last_column += col;
	lloc->byte_from = ctx->buffer_begin + (ctx->tok - ctx->buf);
	lloc->byte_to = lloc->byte_from + col;
}

static void lloc_line(struct lex_context *ctx, YYLTYPE *lloc) {
	lloc->last_line += 1;
	lloc->first_line = lloc->last_line;
	lloc->last_column = 1;
	lloc->first_column = 1;
	lloc->byte_from = ctx->buffer_begin + (ctx->tok - ctx->buf);
	lloc->byte_to = lloc->byte_from + 1;
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

/* "version"     { lloc_col(ctx, lloc, CURRENT_LEN); return VERSION; } */
/* "device_type" { lloc_col(ctx, lloc, CURRENT_LEN); return DEVICETYPE; } */
/* "device"      { lloc_col(ctx, lloc, CURRENT_LEN); return DEVICE; } */
/* "type"        { lloc_col(ctx, lloc, CURRENT_LEN); return TYPE; } */
/* "input"       { lloc_col(ctx, lloc, CURRENT_LEN); return INPUT; } */
/* "output"      { lloc_col(ctx, lloc, CURRENT_LEN); return OUTPUT; } */
/* "attr"        { lloc_col(ctx, lloc, CURRENT_LEN); return ATTR; } */
/* "default"     { lloc_col(ctx, lloc, CURRENT_LEN); return DEFAULT; } */
"Enum"        { lloc_col(ctx, lloc, CURRENT_LEN); return ENUM; }
"namespace"   { lloc_col(ctx, lloc, CURRENT_LEN); return NAMESPACE; }
"use"         { lloc_col(ctx, lloc, CURRENT_LEN); return USE; }
".."          { lloc_col(ctx, lloc, CURRENT_LEN); return RANGE; }
/* "..."         { lloc_col(ctx, lloc, CURRENT_LEN); return ELLIPSIS; } */
"<-"          { lloc_col(ctx, lloc, CURRENT_LEN); return BIND_LEFT; }
"->"          { lloc_col(ctx, lloc, CURRENT_LEN); return BIND_RIGHT; }
"::"          { lloc_col(ctx, lloc, CURRENT_LEN); return DECL; }
"=="          { lloc_col(ctx, lloc, CURRENT_LEN); return EQ; }
"!="          { lloc_col(ctx, lloc, CURRENT_LEN); return NEQ; }
"<="          { lloc_col(ctx, lloc, CURRENT_LEN); return LTE; }
">="          { lloc_col(ctx, lloc, CURRENT_LEN); return GTE; }
"=>"          { lloc_col(ctx, lloc, CURRENT_LEN); return LAMBDA; }

"\x00"        { lloc_col(ctx, lloc, CURRENT_LEN); return END; }
"\r\n" | [\r\n] { lloc_line(ctx, lloc); return yylex(lval, lloc, ctx); }
 "#" [^\r\n]*  { lloc_col(ctx, lloc, CURRENT_LEN); return yylex(lval, lloc, ctx); }
[\t\v\b\f ]   { lloc_col(ctx, lloc, CURRENT_LEN); return yylex(lval, lloc, ctx); }

[a-zA-Z][a-zA-Z0-9_]* | [a-zA-Z_][a-zA-Z0-9_]+ {
	lloc_col(ctx, lloc, CURRENT_LEN);
	lval->IDENTIFIER = atom_create(ctx->atom_table, CURRENT_TOKEN);
	return IDENTIFIER;
 }
'0b' [01]+    {
	lloc_col(ctx, lloc, CURRENT_LEN);
	lval->NUMLIT = string_to_int64_base2(CURRENT_TOKEN);
	return NUMLIT;
 }
[1-9][0-9]* | '0'   {
	lloc_col(ctx, lloc, CURRENT_LEN);
	lval->NUMLIT = string_to_int64_base10(CURRENT_TOKEN);
	return NUMLIT;
 }
'0x' [0-9a-fA-F]+ {
	lloc_col(ctx, lloc, CURRENT_LEN);
	lval->NUMLIT = string_to_int64_base16(CURRENT_TOKEN);
	return NUMLIT;
 }

[-+*/:;={}()\[\].,_$@\\] {
	lloc_col(ctx, lloc, CURRENT_LEN);
	return *ctx->tok;
 }

"op" ([-+*/:;={}()\[\].,_$@\\] | "->" | "<-" | "==" | "!=" | "<=" | ">=") {
	lloc_col(ctx, lloc, CURRENT_LEN);
	lval->IDENTIFIER = atom_create(ctx->atom_table, CURRENT_TOKEN);
	return IDENTIFIER;
 }

"\"" [^\"]* "\"" {
	lloc_col(ctx, lloc, CURRENT_LEN);

	// Trim surrounding "".
	struct string content;
	content.text = CURRENT_TOKEN.text + 1;
	content.length = CURRENT_TOKEN.length - 2;

	string_duplicate(ctx->memory, &lval->STRINGLIT, content);
	return STRINGLIT;
 }

* {
	lloc_col(ctx, lloc, CURRENT_LEN);
	printf("unexpected char '%c' %i\n", *ctx->tok, *ctx->tok);
	return END;
 }

%} /* End lexer */
}

void yyerror(YYLTYPE *lloc, struct lex_context *ctx, const char *error) {
	struct stg_location loc = {0};

	loc.file_id     = ctx->file_id;
	loc.line_from   = lloc->first_line;
	loc.col_from    = lloc->first_column;

	loc.line_to     = lloc->last_line;
	loc.col_to      = lloc->last_column;

	loc.byte_from   = lloc->byte_from;
	loc.byte_to     = lloc->byte_to;

	stg_error(ctx->err, loc, "%s", error);
}

int parse_config_file(struct string filename,
					  struct atom_table *table,
					  struct arena *memory,
					  unsigned int file_id,
					  struct stg_error_context *err_ctx,
					  struct st_node **out_node) {
	struct lex_context ctx;

	memset(&ctx, 0, sizeof(struct lex_context));

	ctx.lim = ctx.buf + BUFFER_SIZE;
	ctx.cur = ctx.lim;
	ctx.tok = ctx.lim;
	ctx.eof = false;
	ctx.atom_table = table;
	ctx.memory = memory;
	ctx.file_id = file_id;
	ctx.buffer_begin = 0;
	ctx.buffer_end = 0;
	ctx.err = err_ctx;

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

	st_clean(&ctx.module);
	//st_print(ctx.module);

	*out_node = ctx.module;

	return 0;
}

