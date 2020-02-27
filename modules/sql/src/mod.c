#include <module.h>
#include <native.h>
#include <base/mod.h>
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

struct sql_connect_data {
	struct string kind;
	struct string con_str;
};

void
sql_db_connect_unsafe(struct vm *vm, struct stg_exec *heap,
		void *data, void *out)
{
	struct sql_connect_data *closure = data;

	struct stg_module *sql_mod;
	sql_mod = vm_get_module(vm, STR("sql"));

	struct sql_context *ctx = sql_mod->data;

	// TODO: Support more database systems.
	assert(string_equal(closure->kind, STR("postgresql")));

	int64_t connection_id;

	struct sql_connection con = {0};

	char con_str_zero_term[closure->con_str.length+1];
	memcpy(con_str_zero_term, closure->con_str.text, closure->con_str.length);
	con_str_zero_term[closure->con_str.length] = 0;

	con.con = PQconnectdb(con_str_zero_term);

	if (PQstatus(con.con) == CONNECTION_BAD) {
		char *error_message;
		error_message = PQerrorMessage(con.con);
		printf("PQconnect failed: %s\n", error_message);
		return;
	}

	connection_id = dlist_append(
			ctx->connections, ctx->num_connections, &con);

	memcpy(out, &connection_id, sizeof(int64_t));
}

struct stg_init_data
sql_db_connect(struct stg_exec *ctx, struct string kind, struct string con_str)
{
	struct stg_init_data monad = {0};
	monad.call = sql_db_connect_unsafe;
	// TODO: Copy strings.
	// monad.copy = ...;
	monad.data_size = sizeof(struct sql_connect_data);
	monad.data = stg_alloc(ctx, 1, monad.data_size);

	struct sql_connect_data *closure;
	closure = monad.data;
	// TODO: Copy strings.
	closure->kind = kind;
	closure->con_str = con_str;

	return monad;
}

struct sql_query_data {
	int64_t db;
	struct string query;
};

void
sql_db_query_unsafe(struct vm *vm, struct stg_exec *heap,
		void *data, void *out)
{
	struct sql_query_data *closure = data;

	struct stg_module *sql_mod;
	sql_mod = vm_get_module(vm, STR("sql"));

	struct sql_context *ctx = sql_mod->data;

	assert(closure->db < ctx->num_connections);
	struct sql_connection *db;
	db = &ctx->connections[closure->db];

	printf("Query db %zi '%.*s'\n",
			closure->db, LIT(closure->query));
}

struct stg_init_data
sql_db_query(struct stg_exec *ctx, int64_t db,
		struct string query, type_id param_type,
		type_id result_type)
{
	struct stg_init_data monad = {0};
	monad.call = sql_db_query_unsafe;
	// TODO: Copy strings.
	// monad.copy = ...;
	monad.data_size = sizeof(struct sql_query_data);
	monad.data = stg_alloc(ctx, 1, monad.data_size);

	struct sql_query_data *closure;
	closure = monad.data;
	// TODO: Copy strings.
	closure->db = db;
	closure->query = query;

	return monad;
}

int
mod_sql_pre_compile(struct ast_context *ast_ctx, struct stg_module *mod)
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
	mod->hook_pre_compile = mod_sql_pre_compile;
	mod->hook_free = mod_sql_free;

	stg_native_register_funcs(mod, sql_db_connect,
			STG_NATIVE_FUNC_HEAP);
	stg_native_register_funcs(mod, sql_db_query,
			STG_NATIVE_FUNC_HEAP);

	return 0;
}

STAGE_MODULE(sql, mod_sql_load);
