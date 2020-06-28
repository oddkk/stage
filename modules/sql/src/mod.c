#include <module.h>
#include <native.h>
#include <base/mod.h>
#include <dlist.h>
#include <stdlib.h>
#include <base/list.h>

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
	sql_mod = vm_get_module(vm, vm_atoms(vm, "sql"));

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

void
sql_db_connect_copy(struct stg_exec *heap, void *data)
{
	struct sql_connect_data *closure = data;
	closure->con_str = stg_exec_copy_string(heap, closure->con_str);
}

struct stg_io_data
sql_db_connect(struct stg_exec *ctx, struct string kind, struct string con_str)
{
	struct stg_io_data monad = {0};
	monad.call = sql_db_connect_unsafe;
	monad.copy = sql_db_connect_copy;
	monad.data_size = sizeof(struct sql_connect_data);
	monad.data = stg_alloc(ctx, 1, monad.data_size);

	struct sql_connect_data *closure;
	closure = monad.data;
	closure->kind = kind;
	closure->con_str = con_str;

	return monad;
}

struct sql_query_data {
	int64_t db;
	struct string query;
	struct string *args;
	size_t num_args;
};

void
sql_db_query_unsafe(struct vm *vm, struct stg_exec *heap,
		void *data, void *out)
{
	struct sql_query_data *closure = data;

	struct stg_module *sql_mod;
	sql_mod = vm_get_module(vm, vm_atoms(vm, "sql"));

	struct sql_context *ctx = sql_mod->data;

	assert(closure->db < ctx->num_connections);
	struct sql_connection *db;
	db = &ctx->connections[closure->db];

	struct arena *tmp_mem = &vm->transient;
	arena_mark cp = arena_checkpoint(tmp_mem);

	// To Zero-terminated string
	char *query;
	query = arena_alloc(tmp_mem, closure->query.length + 1);
	memcpy(query, closure->query.text, closure->query.length);
	query[closure->query.length] = '\0';

	char const *args[closure->num_args];
	for (size_t i = 0; i < closure->num_args; i++) {
		assert(closure->args[i].text[closure->args[i].length] == 0);

		args[i] = closure->args[i].text;
	}

	PGresult *res;
	res = PQexecParams(db->con, query,
			closure->num_args,
			NULL, // paramTypes
			args, // paramValues
			NULL, // paramLengths: This argument is ignored for text params.
			NULL, // paramFormats: When NULL, all args are presumed to be text.
			0     // resultFormat: 0 indicates results should be text.
		);

	arena_reset(tmp_mem, cp);

	ExecStatusType status;
	status = PQresultStatus(res);

	struct stg_list_data *out_list = out;
	memset(out_list, 0, sizeof(struct stg_list_data));

	type_id out_type = stg_list_register_type(
			sql_mod, vm->default_types.string);

	switch (status) {
		case PGRES_EMPTY_QUERY:
		case PGRES_COMMAND_OK:
			*out_list = stg_list_empty(
					vm, heap, out_type);
			break;

		case PGRES_TUPLES_OK:
			{
				int num_rows, num_cols;
				num_rows = PQntuples(res);
				num_cols = PQnfields(res);

				struct string *result;
				result = stg_alloc(heap, num_rows * num_cols, sizeof(struct string));

				for (int r = 0; r < num_rows; r++) {
					for (int c = 0; c < num_cols; c++) {
						struct string *field;
						field = &result[r*num_cols + c];

						field->length = PQgetlength(res, r, c);
						field->text = PQgetvalue(res, r, c);
						*field = stg_exec_copy_string(heap, *field);
					}
				}

				struct stg_list_data *rows;
				rows = stg_alloc(heap, num_rows, sizeof(struct stg_list_data));

				for (int r = 0; r < num_rows; r++) {
					rows[r] = stg_list_from_carray(
							vm, heap, vm->default_types.string,
							&result[r*num_cols], num_cols);
				}

				*out_list = stg_list_from_carray(
						vm, heap, out_type, rows, num_rows);
			}
			break;

		case PGRES_BAD_RESPONSE:
		case PGRES_NONFATAL_ERROR:
		case PGRES_FATAL_ERROR:
		default:
			panic("Query failed:\n%s", PQresultErrorMessage(res));
			break;
	}

	PQclear(res);
}

void
sql_db_query_copy(struct stg_exec *heap, void *data)
{
	struct sql_query_data *closure = data;
	closure->query = stg_exec_copy_string(heap, closure->query);

	struct string *new_args = stg_alloc(heap, closure->num_args, sizeof(struct string));

	for (size_t i = 0; i < closure->num_args; i++) {
		new_args[i] = stg_exec_copy_string(heap, closure->args[i]);
	}

	closure->args = new_args;
}

struct tmp_arg {
	struct string str;
	struct tmp_arg *next;
};

struct stg_io_data
sql_db_query(struct stg_exec *ctx, struct stg_module *mod,
		int64_t db, struct string query, struct stg_list_data args_list)
{
	struct stg_io_data monad = {0};
	monad.call = sql_db_query_unsafe;
	monad.copy = sql_db_query_copy;
	monad.data_size = sizeof(struct sql_query_data);
	monad.data = stg_alloc(ctx, 1, monad.data_size);

	struct sql_query_data *closure;
	closure = monad.data;
	closure->db = db;
	closure->query = query;

	struct arena *tmp;
	tmp = &mod->vm->transient;
	arena_mark cp = arena_checkpoint(tmp);

	size_t num_args = 0;
	struct tmp_arg *head = NULL;
	struct tmp_arg **end = &head;

	assert_type_equals(mod->vm,
			args_list.element_type,
			mod->vm->default_types.string);

	struct string current_arg = {0};
	int eol;
	while (args_list.head && !(eol = args_list.head(ctx, &args_list, &current_arg))) {
		struct tmp_arg *new_arg;
		new_arg = arena_alloc(tmp, sizeof(struct tmp_arg));
		new_arg->str = current_arg;

		*end = new_arg;
		end = &new_arg->next;
		num_args += 1;

		args_list = args_list.tail(ctx, &args_list);
	}

	closure->args = stg_alloc(ctx,
			num_args, sizeof(struct string));
	closure->num_args = num_args;

	{
		struct tmp_arg *it = head;
		for (size_t i = 0; i < num_args; i++) {
			closure->args[i] = it->str;
			
			it = it->next;
		}
		assert(it == NULL);
	}

	if (ctx->heap == tmp) {
		closure->args = arena_reset_and_keep(tmp, cp, closure->args,
				closure->num_args * sizeof(struct string));
	} else {
		arena_reset(tmp, cp);
	}

	return monad;
}

int
mod_sql_register(struct stg_module *mod)
{
	struct sql_context *ctx;
	ctx = calloc(1, sizeof(struct sql_context));
	mod->data = ctx;

	return 0;
}

void
mod_sql_destroy(struct stg_module *mod)
{
	struct sql_context *ctx = mod->data;

	for (size_t i = 0; i < ctx->num_connections; i++) {
		PQfinish(ctx->connections[i].con);
	}
}

int
mod_sql_load(struct stg_native_module *mod)
{
	mod->hook_register = mod_sql_register;
	mod->hook_destroy = mod_sql_destroy;

	stg_native_register_funcs(mod, sql_db_connect,
			STG_NATIVE_FUNC_HEAP);
	stg_native_register_funcs(mod, sql_db_query,
			STG_NATIVE_FUNC_HEAP|STG_NATIVE_FUNC_MODULE_CLOSURE);

	return 0;
}

STAGE_MODULE(sql, mod_sql_load);
