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
		struct config_node *module;
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
		return false;
	}

	struct config_node *_alloc_node(struct lex_context *ctx, enum config_node_type type, YYLTYPE loc) {
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

	static struct config_node *make_list(struct lex_context *ctx, struct config_node *head, struct config_node *tail) {
		struct config_node *list_node;
		YYLTYPE loc = {0};

		list_node = _alloc_node(ctx, CONFIG_NODE_INTERNAL_LIST, loc);
		list_node->internal_list.head = head;
		list_node->internal_list.tail = tail;

		return list_node;
	}

	int yylex(YYSTYPE *, YYLTYPE *, struct lex_context *);
	void yyerror(YYLTYPE *loc, struct lex_context *, const char *);
}

%token END 0
%token DEVICETYPE "device_type" DEVICE "device" TYPE "type" INPUT "input"
%token OUTPUT "output" DEFAULT "default" ATTR "attr" IDENTIFIER NUMLIT
%token BIND "<-" RANGE ".." VERSION "version" NAMESPACE "namespace"

%type	<struct atom*> IDENTIFIER
%type	<scalar_value> NUMLIT
%type	<struct config_node*> module module_stmt_list module_stmt device_type device_type_body
%type	<struct config_node*> device_type_body_stmt device device_body device_body_stmt
%type	<struct config_node*> l_expr type_decl type type_l_expr array_type enum_list enum_label
%type	<struct config_node*> tuple_decl tuple_list named_tuple_list tuple_item named_tuple_item expr /* type_litteral_name */
%type	<struct config_node*> subrange_type namespace bind_stmt
%type	<struct tmp_range> range


%left '+' '-'
%left '*' '/'
%left ".."
%left '.' '[' '{'

%%

module:
				"version" NUMLIT '.' NUMLIT '.' NUMLIT ';'
				module_stmt_list {
	$$ = alloc_node(ctx, CONFIG_NODE_MODULE);
	$$->module.first_child = $8;
	$$->module.version.major = $2;
	$$->module.version.minor = $4;
	$$->module.version.patch = $6;

	assert(!ctx->module);
	ctx->module = $$;
 }
		;
module_stmt_list:
				module_stmt_list module_stmt { $$ = $1; append_child(&$$, $2); }
		|		%empty                       { $$ = NULL; }
		;
module_stmt: 	device_type
		|		device
		|		type_decl
		|		namespace
		|		bind_stmt
		;
device_type:	"device_type" IDENTIFIER '{' device_type_body '}' { $$ = alloc_node(ctx, CONFIG_NODE_DEVICE_TYPE); $$->device_type.name = $2; $$->device_type.first_child = $4; }
		;
device_type_body:
				device_type_body device_type_body_stmt { $$ = $1; append_child(&$$, $2); }
		|		%empty                                 { $$ = NULL; }
		;
device_type_body_stmt:
				"input"  IDENTIFIER ':' type ';'           { $$ = alloc_node(ctx, CONFIG_NODE_INPUT);     $$->input.name = $2;  $$->input.type = $4; $$->input.def = false; }
		|		"input" "default" IDENTIFIER ':' type ';'  { $$ = alloc_node(ctx, CONFIG_NODE_INPUT);     $$->input.name = $3;  $$->input.type = $5; $$->input.def = true; }
		|		"output" IDENTIFIER ':' type ';'           { $$ = alloc_node(ctx, CONFIG_NODE_OUTPUT);    $$->output.name = $2; $$->output.type = $4; $$->output.def = false; }
		|		"output" "default" IDENTIFIER ':' type ';' { $$ = alloc_node(ctx, CONFIG_NODE_OUTPUT);    $$->output.name = $3; $$->output.type = $5; $$->output.def = true; }
		|		"attr" IDENTIFIER ':' type   ';'           { $$ = alloc_node(ctx, CONFIG_NODE_ATTR);      $$->attr.name = $2;   $$->attr.type = $4; }
		|		"attr" IDENTIFIER ':' type   '=' expr ';'  { $$ = alloc_node(ctx, CONFIG_NODE_ATTR);      $$->attr.name = $2;   $$->attr.type = $4; $$->attr.def_value = $6; }
		|		bind_stmt
		|		l_expr '=' expr ';'                     { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ASSIGN; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		device                                     { $$ = $1; }
		|		type_decl                                  { $$ = $1; }
		;
device:			"device" l_expr '{' device_body '}'            { $$ = alloc_node(ctx, CONFIG_NODE_DEVICE); $$->device.type = $2;                       $$->device.first_child = $4; }
		|		"device" l_expr IDENTIFIER '{' device_body '}' { $$ = alloc_node(ctx, CONFIG_NODE_DEVICE); $$->device.type = $2; $$->device.name = $3; $$->device.first_child = $5; }
		|		"device" l_expr ';'                            { $$ = alloc_node(ctx, CONFIG_NODE_DEVICE); $$->device.type = $2;                       $$->device.first_child = NULL; }
		|		"device" l_expr IDENTIFIER ';'                 { $$ = alloc_node(ctx, CONFIG_NODE_DEVICE); $$->device.type = $2; $$->device.name = $3; $$->device.first_child = NULL; }
		;
device_body:	device_body device_body_stmt { $$ = $1; append_child(&$$, $2); }
		|		%empty                       { $$ = NULL; }
		;
device_body_stmt:
				l_expr '=' expr ';'          { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ASSIGN; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		bind_stmt                    { $$ = $1; }
		|		device                       { $$ = $1; }
		;
bind_stmt:		l_expr "<-" expr ';'         { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_BIND;   $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		//		|		l_expr "<-"   expr ';'       { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_BIND;   $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		;
l_expr:			IDENTIFIER                   { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); $$->ident = $1; }
		|		'_'                          { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); }
		|		l_expr '.' l_expr            { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ACCESS; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		l_expr '[' expr ']'          { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_SUBSCRIPT; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		l_expr '[' range ']'         { $$ = alloc_node(ctx, CONFIG_NODE_SUBSCRIPT_RANGE); $$->subscript_range.lhs = $1; $$->subscript_range.low = $3.low; $$->subscript_range.high = $3.high; }
		;
type_decl:		"type" IDENTIFIER '=' type ';'   { $$ = alloc_node(ctx, CONFIG_NODE_TYPE_DECL); $$->type_decl.name = $2; $$->type_decl.type = $4; };

type: 			type_l_expr                  { $$ = $1; }
		|		array_type                   { $$ = alloc_node(ctx, CONFIG_NODE_TYPE); $$->type_def.first_child = $1; }
		|		subrange_type                { $$ = alloc_node(ctx, CONFIG_NODE_TYPE); $$->type_def.first_child = $1; }
		|		'{' enum_list '}'            { $$ = NULL; printf("TODO: enumlist\n"); }
		|		'(' tuple_decl ')'           { $$ = alloc_node(ctx, CONFIG_NODE_TYPE); $$->type_def.first_child = $2; }
		;

type_l_expr:	IDENTIFIER                   { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); $$->ident = $1; }
		|		'_'                          { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); }
		|		type_l_expr '.' type_l_expr  { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ACCESS; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		;

array_type:		type '[' expr ']'            { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_SUBSCRIPT; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		;

subrange_type:	type_l_expr '{' range '}'    { $$ = alloc_node(ctx, CONFIG_NODE_SUBRANGE); $$->subrange.lhs = $1; $$->subrange.low = $3.low; $$->subrange.high = $3.high; }
		;
enum_list: 		enum_label                   { $$ = NULL; }
		| 		enum_list ',' enum_label     { $$ = NULL; }
		;
enum_label:		IDENTIFIER                   { $$ = NULL; }
		;

tuple_decl:   	tuple_list                   { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE); $$->tuple.first_child = $1; $$->tuple.named = false; }
		|		named_tuple_list             { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE); $$->tuple.first_child = $1; $$->tuple.named = true;  }
		;
tuple_list:		tuple_item                   { $$ = make_list(ctx, $1, NULL); }
		|		tuple_list ',' tuple_item    { $$ = make_list(ctx, $3, $1); }
		;
named_tuple_list:
				named_tuple_item                      { $$ = make_list(ctx, $1, NULL); }
		|		named_tuple_list ',' named_tuple_item { $$ = make_list(ctx, $3, $1); }
		;
tuple_item: 	type_l_expr                  { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE_ITEM); $$->tuple_item.name = NULL; $$->tuple_item.type = $1; }
		;
named_tuple_item:
				IDENTIFIER ':' type_l_expr   { $$ = alloc_node(ctx, CONFIG_NODE_TUPLE_ITEM); $$->tuple_item.name = $1; $$->tuple_item.type = $3; }
		;


range:			expr ".." expr     { $$.low = $1;   $$.high = $3; }
		|		expr ".."          { $$.low = $1;   $$.high = NULL; }
		|		     ".." expr     { $$.low = NULL; $$.high = $2; }
		|		     ".."          { $$.low = NULL; $$.high = NULL; }
		;
expr:			NUMLIT             { $$ = alloc_node(ctx, CONFIG_NODE_NUMLIT); $$->numlit = $1; }
		|		IDENTIFIER         { $$ = alloc_node(ctx, CONFIG_NODE_IDENT); $$->ident = $1; }
		|		expr '.' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ACCESS;    $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		expr '+' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_ADD;       $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		expr '-' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_SUB;       $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		expr '*' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_MUL;       $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		expr '/' expr      { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_DIV;       $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		expr '[' expr ']'  { $$ = alloc_node(ctx, CONFIG_NODE_BINARY_OP); $$->binary_op.op = CONFIG_OP_SUBSCRIPT; $$->binary_op.lhs = $1; $$->binary_op.rhs = $3; }
		|		expr '[' range ']' { $$ = alloc_node(ctx, CONFIG_NODE_SUBSCRIPT_RANGE); $$->subscript_range.lhs = $1; $$->subscript_range.low = $3.low; $$->subscript_range.high = $3.high; }
		;
namespace: 		"namespace" IDENTIFIER '{' module_stmt_list '}' { $$ = alloc_node(ctx, CONFIG_NODE_NAMESPACE); $$->namespace.name = $2; $$->namespace.first_child = $4; }
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

"version"     { lloc_col(lloc, CURRENT_LEN); return VERSION; }
"device_type" { lloc_col(lloc, CURRENT_LEN); return DEVICETYPE; }
"device"      { lloc_col(lloc, CURRENT_LEN); return DEVICE; }
"type"        { lloc_col(lloc, CURRENT_LEN); return TYPE; }
"input"       { lloc_col(lloc, CURRENT_LEN); return INPUT; }
"output"      { lloc_col(lloc, CURRENT_LEN); return OUTPUT; }
"attr"        { lloc_col(lloc, CURRENT_LEN); return ATTR; }
"default"     { lloc_col(lloc, CURRENT_LEN); return DEFAULT; }
"namespace"   { lloc_col(lloc, CURRENT_LEN); return NAMESPACE; }
".."          { lloc_col(lloc, CURRENT_LEN); return RANGE; }
"<-"          { lloc_col(lloc, CURRENT_LEN); return BIND; }

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

[-+*/:;={}()\[\].,_] {
	lloc_col(lloc, CURRENT_LEN);
	return *ctx->tok;
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

	yyparse(&ctx);

	config_tree_clean(ctx.module);
	*out_node = ctx.module;

	return 0;
}
