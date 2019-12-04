#include <module.h>
#include <native.h>
#include <dlist.h>
#include <stdlib.h>

#include <libpq-fe.h>

struct sql_connection {
	PGconn *con;
};

struct sql_context {
	struct sql_connection *connections;
	size_t num_connections;
};

int64_t
sql_db_connect(struct stg_module *mod, struct string kind, struct string con_str)
{
	struct sql_context *ctx = mod->data;

	// TODO: Support more database systems.
	assert(string_equal(kind, STR("postgresql")));

	int64_t connection_id;

	struct sql_connection con = {0};

	char con_str_zero_term[con_str.length+1];
	memcpy(con_str_zero_term, con_str.text, con_str.length);
	con_str_zero_term[con_str.length] = 0;

	con.con = PQconnectdb(con_str_zero_term);

	if (PQstatus(con.con) == CONNECTION_BAD) {
		char *error_message;
		error_message = PQerrorMessage(con.con);
		printf("PQconnect failed: %s\n", error_message);
		return -1;
	}

	connection_id = dlist_append(
			ctx->connections, ctx->num_connections, &con);

	return 0;
}

int
mod_sql_init(struct ast_context *ast_ctx, struct stg_module *mod)
{
	struct sql_context *ctx;
	ctx = calloc(1, sizeof(struct sql_context));
	mod->data = ctx;

	return 0;
}

void
mod_sql_free(struct stg_module *mod)
{
	struct sql_context *ctx = mod->data;

	for (size_t i = 0; i < ctx->num_connections; i++) {
		PQfinish(ctx->connections[i].con);
	}
}

int
mod_sql_load(struct stg_native_module *mod)
{
	mod->hook_init = mod_sql_init;
	mod->hook_free = mod_sql_free;

	stg_native_register_funcs(mod, sql_db_connect,
			STG_NATIVE_FUNC_IMPURE | STG_NATIVE_FUNC_MODULE_CLOSURE);

	return 0;
}

STAGE_MODULE(sql, mod_sql_load);
