#include "syntax_tree.h"
#include "utils.h"
#include "ast.h"
#include "module.h"
#include <stdlib.h>
#include <string.h>

struct string st_node_names[] = {
#define ST_NODE(name, data) STR(#name),
#include "syntax_tree_node_defs.h"
#undef ST_NODE
};

struct string st_bin_op_sym[] = {
#define OP(name, sym) STR(sym),
	ST_BIN_OPS
#undef OP
};

struct atom *binop_atom(struct atom_table *atom_table,
						enum st_bin_op op)
{
	assert(op < ST_OP_LEN);

	char buffer[2 + ST_BIN_OPS_MAX_LEN] = {0};

	buffer[0] = 'o';
	buffer[1] = 'p';

	struct string sym = st_bin_op_sym[op];

	assert(sym.length <= ST_BIN_OPS_MAX_LEN);

	memcpy(&buffer[2], sym.text, sym.length);

	struct string name;
	name.text = buffer;
	name.length = 2 + sym.length;

	return atom_create(atom_table, name);
}

struct st_tuple_members {
	struct atom **names;
	struct st_node **types;
	size_t num_members;
};

static void
st_node_unpack_tuple_nodes(struct st_node *tuple_node,
		struct st_tuple_members *out_members)
{
	assert(tuple_node->type == ST_NODE_TUPLE_DECL);

	struct st_tuple_members members = {0};

	for (struct st_node *param = tuple_node->TUPLE_DECL.items;
			param; param = param->next_sibling) {
		members.num_members += 1;
	}

	members.names = calloc(members.num_members, sizeof(struct atom *));
	members.types = calloc(members.num_members, sizeof(struct st_node *));

	size_t i = 0;

	for (struct st_node *param = tuple_node->TUPLE_DECL.items;
			param; param = param->next_sibling) {
		members.names[i] = param->TUPLE_DECL_ITEM.name;
		members.types[i] = param->TUPLE_DECL_ITEM.type;

		i += 1;
	}

	*out_members = members;
}

static void
st_node_unpack_func_proto(struct st_node *proto_node,
		struct st_tuple_members *out_params,
		struct st_node **out_ret_type)
{
	if (proto_node) {
		switch (proto_node->type) {
			case ST_NODE_FUNC_PROTO:
				st_node_unpack_tuple_nodes(
						proto_node->FUNC_PROTO.params, out_params);
				*out_ret_type = proto_node->FUNC_PROTO.ret;
				break;

			case ST_NODE_TUPLE_DECL:
				st_node_unpack_tuple_nodes(
						proto_node->FUNC_PROTO.params, out_params);
				*out_ret_type = NULL;
				break;

			case ST_NODE_IDENT:
				out_params->num_members = 1;
				out_params->names = calloc(1, sizeof(struct atom *));
				out_params->types = calloc(1, sizeof(struct st_node*));
				out_params->names[0] = proto_node->IDENT;
				out_params->types[0] = NULL;
				*out_ret_type = NULL;
				break;

			default:
				panic("Invalid node '%.*s' as function prototype.",
						LIT(st_node_names[proto_node->type]));
				break;
		}
	} else {
		out_params->names = NULL;
		out_params->types = NULL;
		out_params->num_members = 0;
		*out_ret_type = NULL;
	}
}

void
st_node_visit_stmt(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *struct_node, struct st_node *stmt)
{
	assert(stmt->type == ST_NODE_STMT);
	assert(stmt->STMT.stmt);

	switch (stmt->STMT.stmt->type) {
		case ST_NODE_USE_STMT:
			panic("TODO: Use statement in composite datatype");
			break;

		case ST_NODE_ASSIGN_STMT:
			{
				struct st_node *assign = stmt->STMT.stmt;

				struct ast_node *target =
					st_node_visit_expr(
							ctx, env, NULL,
							assign->ASSIGN_STMT.ident);

				struct st_node *expr_node;
				struct ast_node *expr = NULL;

				expr_node = assign->ASSIGN_STMT.body;
				if (expr_node) {
					expr = st_node_visit_expr(
							ctx, env, NULL, expr_node);
				}

				bool overridable;
				overridable = assign->ASSIGN_STMT.overridable;


				if (assign->ASSIGN_STMT.decl) {
					struct st_node *type_node;
					struct ast_node *type = NULL;

					// TODO: Allow more complex targets.
					assert(target->kind == AST_NODE_LOOKUP);
					struct atom *name = target->lookup.name;

					type_node = assign->ASSIGN_STMT.type;

					assert(expr || type_node);

					if (type_node) {
						type = st_node_visit_expr(
								ctx, env, NULL, type_node);
					} else {
						assert(expr);
					}

					int err;
					err = ast_node_composite_add_member(
							ctx, env, struct_node,
							name, type);
					if (err) {
						stg_error(ctx->err, target->loc,
								"'%.*s' is already declared.",
								ALIT(name));
						return;
					}
				} else {
					assert(!assign->ASSIGN_STMT.type);
					assert(expr);
				}

				if (expr) {
					ast_node_composite_bind(
							ctx, env, struct_node,
							target, expr, overridable);
				}
			}
			break;

		default:
			{
				struct st_node *expr_node;
				expr_node = stmt->STMT.stmt;

				struct ast_node *expr;
				expr = st_node_visit_expr(
						ctx, env, NULL, expr_node);


				ast_node_composite_add_free_expr(
						ctx, env, struct_node, expr);
			}
			break;
	}
}

bool
st_node_has_templ_params(struct st_node *node)
{
	if (node->type == ST_NODE_TEMPLATE_VAR) {
		return true;
	}

	bool has_param = false;
#define TREE_VISIT_NODE(node, type, member) \
	has_param |= st_node_has_templ_params(node->type.member);
#define TREE_VISIT_ATOM(node, type, member)
	ST_NODE_VISIT(node)
#undef TREE_VISIT_NODE
#undef TREE_VISIT_ATOM

	return false;
}

struct ast_node *
st_node_visit_expr(struct ast_context *ctx, struct ast_env *env,
		struct ast_node *templ_node, struct st_node *node)
{
	switch (node->type) {

		case ST_NODE_ACCESS:
			{
				struct ast_node *target;
				target = st_node_visit_expr(
						ctx, env, templ_node,
						node->ACCESS.target);
				struct ast_node *access;
				access = ast_init_node_access(
						ctx, env, AST_NODE_NEW, node->loc,
						target, node->ACCESS.name);

				return access;
			}
			break;

	case ST_NODE_BIN_OP: {
		struct ast_func_arg func_args[] = {
			{vm_atoms(ctx->vm, "lhs"),
				st_node_visit_expr(ctx, env, templ_node,
						node->BIN_OP.lhs)},
			{vm_atoms(ctx->vm, "rhs"),
				st_node_visit_expr(ctx, env, templ_node,
						node->BIN_OP.rhs)},
		};

		struct atom *op_name;
		op_name =
			binop_atom(&ctx->vm->atom_table,
					   node->BIN_OP.op);

		struct ast_node *func;

		func = ast_init_node_lookup(ctx, env,
				AST_NODE_NEW, node->loc, op_name);

		struct ast_node *call;
		call = ast_init_node_call(ctx, env,
				AST_NODE_NEW, node->loc, func,
				func_args, ARRAY_LENGTH(func_args));

		return call;
	 }

	case ST_NODE_BIND: {
		struct ast_func_arg func_args[] = {
			{vm_atoms(ctx->vm, "src"),
				st_node_visit_expr(ctx, env, templ_node,
						node->BIN_OP.lhs)},
			{vm_atoms(ctx->vm, "drain"),
				st_node_visit_expr(ctx, env, templ_node,
						node->BIN_OP.rhs)},
		};

		struct ast_node *func;

		// TODO: Lookup.

		struct atom *op_name;
		op_name = vm_atoms(ctx->vm, "op->");

		func = ast_init_node_lookup(ctx, env,
				AST_NODE_NEW, node->loc, op_name);

		struct ast_node *call;
		call = ast_init_node_call(ctx, env,
				AST_NODE_NEW, node->loc, func,
				func_args, ARRAY_LENGTH(func_args));


		return call;
	} break;

	case ST_NODE_LAMBDA: {
		struct st_node *proto_node;
		proto_node = node->LAMBDA.proto;

		struct st_tuple_members params_decl;
		struct st_node *ret_type_decl;

		st_node_unpack_func_proto(proto_node,
				&params_decl, &ret_type_decl);

		struct ast_node *params[params_decl.num_members];
		struct ast_node *ret_type = NULL, *body = NULL;

		struct ast_node *templ = NULL;
		bool has_templ_params = false;

		if (ret_type_decl) {
			has_templ_params |= st_node_has_templ_params(ret_type_decl);
		}

		for (size_t i = 0; i < params_decl.num_members && !has_templ_params; i++) {
			has_templ_params |= st_node_has_templ_params(params_decl.types[i]);
		}

		if (has_templ_params) {
			// func is assigned to body further down.
			templ = ast_init_node_templ(ctx, env, AST_NODE_NEW,
					proto_node->loc, NULL);
		}


		for (size_t i = 0; i < params_decl.num_members; i++) {
			params[i] = st_node_visit_expr(ctx, env,
					templ, params_decl.types[i]);
		}

		if (ret_type_decl) {
			ret_type = st_node_visit_expr(ctx, env,
					templ, ret_type_decl);
		}

		struct ast_node *func;

		struct st_node *body_decl;
		body_decl = node->LAMBDA.body;

		if (node->LAMBDA.special) {
			if (body_decl->SPECIAL.name == vm_atoms(ctx->vm, "native")) {
				struct st_node *args = body_decl->SPECIAL.args;

				if (!args || args->next_sibling) {
					size_t num_args_provided = 0;
					for (struct st_node *nd = args;
							nd != NULL; nd = nd->next_sibling) {
						num_args_provided += 1;
					}
					stg_error(ctx->err, body_decl->loc,
							"@native expected exactly one argument, got %zu.",
							num_args_provided);
					return NULL;
				}

				if (args->type != ST_NODE_STR_LIT) {
					char *arg_kind = NULL;
					switch (args->type) {
						case ST_NODE_NUM_LIT:
							arg_kind = "integer";
							break;

						default:
							panic("Invalid node as argument for SPECIAL.");
					}

					assert(arg_kind);

					stg_error(ctx->err, args->loc,
							"Expected string, got %s.", arg_kind);
					return NULL;
				}

			func = ast_init_node_func_native(ctx, env,
					AST_NODE_NEW, node->loc,
					params_decl.names, params, params_decl.num_members,
					ret_type, args->STR_LIT);
			} else {
				stg_error(ctx->err, body_decl->loc,
						"Invalid special expression.");
				return NULL;
			}
		} else {
			body = st_node_visit_expr(ctx, env, NULL, body_decl);

			func = ast_init_node_func(ctx, env,
					AST_NODE_NEW, node->loc,
					params_decl.names, params, params_decl.num_members,
					ret_type, body);
		}

		free(params_decl.names);
		free(params_decl.types);

		if (templ) {
			templ->templ.body = func;
			return templ;
		} else {
			return func;
		}
	} break;

	case ST_NODE_FUNC_CALL: {

		struct ast_node *func;

		func = st_node_visit_expr(ctx, env, templ_node, node->FUNC_CALL.ident);

		size_t num_args = 0;

		assert(node->FUNC_CALL.params);
		assert(node->FUNC_CALL.params->type == ST_NODE_TUPLE_LIT);

		struct st_node *args = node->FUNC_CALL.params->TUPLE_LIT.items;

		for (struct st_node *arg = args;
				arg != NULL;
				arg = arg->next_sibling) {
			num_args += 1;
		}

		struct ast_func_arg func_args[num_args];

		{
			struct st_node *arg = args;
			for (size_t i = 0; i < num_args; i++) {
				assert(arg != NULL);
				assert(arg->type == ST_NODE_TUPLE_LIT_ITEM);
				func_args[i].name = arg->TUPLE_LIT_ITEM.name;
				func_args[i].value =
					st_node_visit_expr(ctx, env,
							templ_node,
							arg->TUPLE_LIT_ITEM.value);
				arg = arg->next_sibling;
			}
		}

		return ast_init_node_call(
				ctx, env, AST_NODE_NEW, node->loc,
				func, func_args, num_args);
	} break;

	case ST_NODE_MODULE:
	case ST_NODE_OBJECT_DECL:
	{
		struct ast_node *struct_node;
		struct_node = ast_init_node_composite(
				ctx, env, AST_NODE_NEW, node->loc);

		struct st_node *member;
		if (node->type == ST_NODE_OBJECT_DECL) {
			member = node->OBJECT_DECL.body;
		} else {
			member = node->MODULE.body;
		}

		while (member) {
			st_node_visit_stmt(ctx, env, struct_node, member);
			member = member->next_sibling;
		}

		return struct_node;
	}

	case ST_NODE_OBJECT_INST:
	{
		break;
	}

	case ST_NODE_TUPLE_DECL: {
		/*
		struct expr_node *first_arg = NULL, *last_arg = NULL;

		struct st_node *args_tuple;
		args_tuple = node;
		assert(args_tuple->type == ST_NODE_TUPLE_DECL);
		// @TODO: Named tuples
		assert(!args_tuple->TUPLE_DECL.named);

		struct st_node *arg;
		arg = args_tuple->TUPLE_DECL.items;


		while (arg) {
			struct expr_node *n;

			n = st_node_visit_expr(ctx, expr,
									NULL, func_scope,
									arg->TUPLE_DECL_ITEM.type);

			if (!first_arg) {
				assert(!last_arg);
				first_arg = n;
				last_arg = n;
			} else {
				last_arg->next_arg = n;
			}

			arg = arg->next_sibling;
		}

		struct expr_node *local_scope, *func;
		struct atom *tuple_func_name;
		tuple_func_name =
			atom_create(mod->atom_table,
						STR("tuple"));
		local_scope =
			expr_scope(mod, expr, node->loc, scope);
		func =
			expr_lookup(mod, expr, node->loc,
						tuple_func_name, local_scope,
						EXPR_LOOKUP_GLOBAL);

		struct expr_node *call;
		call = expr_call(mod, expr, node->loc, func, first_arg);

		return call;
		*/
	} break;

	case ST_NODE_TUPLE_LIT:
		break;

	case ST_NODE_ARRAY_LIT:
		break;

	case ST_NODE_NUM_LIT: {
		struct object obj;

		obj.data = &node->NUM_LIT;
		obj.type = ctx->types.integer;

		return ast_init_node_lit(
				ctx, env, AST_NODE_NEW, node->loc,
				register_object(ctx->vm, env->store, obj));
	} break;

	case ST_NODE_STR_LIT:
		// return expr_lit_str(mod, expr, node->loc, node->STR_LIT);
		break;

	case ST_NODE_IDENT:
		return ast_init_node_lookup(ctx, env,
				AST_NODE_NEW, node->loc, node->IDENT);

	case ST_NODE_TEMPLATE_VAR:
		if (!templ_node) {
			stg_error(ctx->err, node->loc,
					"Template params can not be used here.");
		}

		ast_node_templ_register_param(
				ctx, env, templ_node,
				node->TEMPLATE_VAR.name,
				node->loc);

		return ast_init_node_lookup(ctx, env,
				AST_NODE_NEW, node->loc,
				node->TEMPLATE_VAR.name);

	default:
		panic("Invalid node '%.*s' in expr.",
			  LIT(st_node_names[node->type]));
		break;
	}

	panic("Unhandled node '%.*s' in expr.",
		  LIT(st_node_names[node->type]));
	return NULL;
}
