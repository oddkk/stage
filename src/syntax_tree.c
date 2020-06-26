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
	if (!tuple_node) {
		out_members->names = NULL;
		out_members->types = NULL;
		out_members->num_members = 0;
		return;
	}

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
		assert(param->type == ST_NODE_TUPLE_DECL_ITEM);
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

struct atom *
ast_node_use_expr_name(struct ast_node *node)
{
	switch (node->kind) {
		case AST_NODE_ACCESS:
			return node->access.name;

		case AST_NODE_LOOKUP:
			return node->access.name;

		case AST_NODE_MOD:
			return node->mod.name;

		default:
			return NULL;
	}
}

void
st_node_visit_templ_param(
		struct ast_context *ctx, struct stg_module *mod,
		struct ast_pattern *pat, struct st_node *node,
		type_id expected_type)
{
	switch (node->type) {
		case ST_NODE_TEMPLATE_VAR:
			{
				struct ast_node *type_node = NULL;
				if (node->TEMPLATE_VAR.type) {
					assert(expected_type == TYPE_UNSET);
					struct st_expr_context expr_ctx = {0};
					expr_ctx.pattern = pat;
					type_node = st_node_visit_expr(
							ctx, mod, &expr_ctx,
							node->TEMPLATE_VAR.type);
				} else if (expected_type != TYPE_UNSET) {
					struct object cons_type_obj = {0};
					cons_type_obj.type = ctx->vm->default_types.type;
					cons_type_obj.data = &expected_type;
					cons_type_obj = stg_register_object(mod, cons_type_obj);

					type_node = ast_init_node_lit(
							ctx, AST_NODE_NEW, node->loc, cons_type_obj);
				}

				ast_pattern_register_param(
						ctx, pat, node->TEMPLATE_VAR.name,
						type_node, node->loc);
			}
			break;

		case ST_NODE_TEMPL_INST:
			{
				struct st_node *ident, *params;
				ident = node->TEMPL_INST.ident;
				params = node->TEMPL_INST.params;

				st_node_visit_templ_param(
						ctx, mod, pat, ident,
						ctx->vm->default_types.cons);

				if (params) {
					assert(params->type == ST_NODE_TUPLE_LIT);
					params = params->TUPLE_LIT.items;

					while (params) {
						st_node_visit_templ_param(
								ctx, mod, pat, params->TUPLE_LIT_ITEM.value,
								TYPE_UNSET);
						params = params->next_sibling;
					}
				}
			}
			break;

			// TODO: More complex pattern variables, such as type
			// constraints.

		default:
			stg_error(ctx->err, node->loc,
					"Expected template parameters.");
			break;
	}
}

void
st_node_visit_stmt(struct ast_context *ctx, struct stg_module *mod,
		struct ast_node *struct_node, struct st_node *stmt)
{
	assert(stmt->type == ST_NODE_STMT);

	if (!stmt->STMT.stmt) {
		// There was a parse error. This should have been reported by the parser.
		assert(ctx->err->num_errors > 0);
		return;
	}

	switch (stmt->STMT.stmt->type) {
		case ST_NODE_USE_STMT:
			{
				struct st_node *use_stmt;
				use_stmt = stmt->STMT.stmt;

				struct st_node *target_expr;
				target_expr = use_stmt->USE_STMT.ident;
				bool use_all = false;

				if (target_expr->type == ST_NODE_USE_ALL) {
					use_all = true;
					target_expr = target_expr->USE_ALL.target;
				}

				struct st_expr_context expr_ctx = {0};
				expr_ctx.init_target = struct_node;

				struct ast_node *target;
				target = st_node_visit_expr(
						ctx, mod, &expr_ctx, target_expr);
				if (target) {
					struct atom *as_name = NULL;

					if (!use_all) {
						as_name = ast_node_use_expr_name(target);
					}

					ast_node_composite_add_use(
							ctx, use_stmt->loc,
							struct_node, target,
							as_name);
				}
			}
			break;

		case ST_NODE_MOD_STMT:
			{
				struct st_node *mod_stmt;
				mod_stmt = stmt->STMT.stmt;

				struct st_expr_context expr_ctx = {0};

				struct ast_node *target_node;
				target_node = st_node_visit_expr(
						ctx, mod, &expr_ctx, mod_stmt);

				ast_node_composite_add_use(
						ctx, mod_stmt->loc,
						struct_node, target_node,
						mod_stmt->MOD_STMT.ident);
			}
			break;

		case ST_NODE_ASSIGN_STMT:
			{
				struct st_node *assign = stmt->STMT.stmt;

				struct st_expr_context def_ctx = {0};

				struct ast_node *target =
					st_node_visit_expr(
							ctx, mod, &def_ctx,
							assign->ASSIGN_STMT.ident);

				struct st_node *expr_node;
				struct ast_node *expr = NULL;

				expr_node = assign->ASSIGN_STMT.body;
				if (expr_node) {
					struct st_expr_context expr_ctx = {0};
					expr_ctx.init_target = struct_node;

					expr = st_node_visit_expr(
							ctx, mod, &expr_ctx, expr_node);
					if (!expr) {
						return;
					}
				}

				bool overridable;
				overridable = assign->ASSIGN_STMT.overridable;

				int type_giving_bind = AST_NO_TYPE_GIVING_BIND;

				if (expr) {
					type_giving_bind = ast_node_composite_bind(
							ctx, struct_node,
							target, expr, overridable);
				}

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
								ctx, mod, &def_ctx, type_node);
					} else {
						assert(expr);
					}

					int err;
					err = ast_node_composite_add_member(
							ctx, struct_node,
							name, type, type_giving_bind);
					if (err) {
						stg_error(ctx->err, target->loc,
								"'%.*s' is already declared.",
								ALIT(name));
						if (type_giving_bind != AST_NO_TYPE_GIVING_BIND) {
							ast_node_composite_tag_bind_erroneous(
									ctx, struct_node, type_giving_bind);
						}
						return;
					}
				} else {
					assert(!assign->ASSIGN_STMT.type);
					assert(expr);
				}
			}
			break;

		case ST_NODE_IMPL_STMT:
			{
				struct st_node *impl = stmt->STMT.stmt;

				struct st_expr_context target_ctx = {0};
				target_ctx.init_target = struct_node;

				struct ast_node *target;
				target = st_node_visit_expr(
						ctx, mod, &target_ctx,
						impl->IMPL_STMT.target);

				struct st_node *arg_items;
				assert(impl->IMPL_STMT.args->type == ST_NODE_TUPLE_LIT);
				arg_items = impl->IMPL_STMT.args->TUPLE_LIT.items;

				size_t num_args = 0;
				struct st_node *arg_iter;
				arg_iter = arg_items;
				while (arg_iter) {
					num_args += 1;
					arg_iter = arg_iter->next_sibling;
				}

				struct ast_datatype_impl_arg args[num_args];

				size_t arg_i = 0;
				arg_iter = arg_items;
				while (arg_iter) {
					assert(arg_iter->type == ST_NODE_TUPLE_LIT_ITEM);
					args[arg_i].name = arg_iter->TUPLE_LIT_ITEM.name;
					args[arg_i].value = st_node_visit_expr(
							ctx, mod, &target_ctx, arg_iter->TUPLE_LIT_ITEM.value);

					arg_i += 1;
					arg_iter = arg_iter->next_sibling;
				}
				assert(arg_i == num_args);


				struct ast_node *body_node;
				body_node = ast_init_node_composite(
						ctx, AST_NODE_NEW, impl->loc,
						AST_COMPOSITE_TYPE_CLASS_IMPL);

				struct st_node *body_iter;
				body_iter = impl->IMPL_STMT.body;
				while (body_iter) {
					st_node_visit_stmt(
							ctx, mod, body_node, body_iter);
					body_iter = body_iter->next_sibling;
				}

				ast_node_composite_add_impl(
						ctx, impl->loc, struct_node,
						target, args, num_args, body_node);
			}
			break;

		default:
			{
				struct st_node *expr_node;
				expr_node = stmt->STMT.stmt;

				struct st_expr_context expr_ctx = {0};
				expr_ctx.init_target = struct_node;

				struct ast_node *expr;
				expr = st_node_visit_expr(
						ctx, mod, &expr_ctx, expr_node);

				if (!expr) {
					// Something went wrong.
					return;
				}

				ast_node_composite_add_free_expr(
						ctx, struct_node, expr);
			}
			break;
	}
}

bool
st_node_has_templ_params(struct st_node *node)
{
	if (!node) {
		// There was an error parsing this expression. A message should have
		// been posted already.
		return false;
	}

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

	if (node->next_sibling) {
		has_param |= st_node_has_templ_params(node->next_sibling);
	}

	return has_param;
}

struct ast_node *
st_node_create_template(struct ast_context *ctx, struct stg_module *mod,
		struct st_node *params_node, struct ast_node *node)
{
	struct st_tuple_members params;
	st_node_unpack_tuple_nodes(
			params_node, &params);

	struct ast_node *templ_node;

	templ_node = ast_init_node_templ(
			ctx, AST_NODE_NEW,
			node->loc, node);

	for (size_t i = 0; i < params.num_members; i++) {
		struct ast_node *type_node;

		type_node = st_node_visit_expr(
				ctx, mod, NULL, params.types[i]);

		// TODO: Better location.
		ast_pattern_register_param(
				ctx, &templ_node->templ.pattern,
				params.names[i], type_node, node->loc);
	}

	free(params.names);
	free(params.types);

	return templ_node;
}

static struct ast_node *
st_node_visit_do_stmts(struct ast_context *ctx, struct stg_module *mod,
		struct st_expr_context *expr_ctx, struct st_node *stmt)
{
	assert(stmt->type == ST_NODE_DO_EXPR_STMT);
	struct atom *target = NULL;
	if (stmt->DO_EXPR_STMT.target) {
		assert(stmt->DO_EXPR_STMT.target->type == ST_NODE_IDENT);
		target = stmt->DO_EXPR_STMT.target->IDENT;
	}

	struct ast_node *expr;
	expr = st_node_visit_expr(
			ctx, mod, expr_ctx, stmt->DO_EXPR_STMT.expr);

	if (!stmt->next_sibling) {
		return expr;
	}

	struct ast_node *tail;
	tail = st_node_visit_do_stmts(
			ctx, mod, expr_ctx, stmt->next_sibling);

	struct ast_node *rhs = NULL;
	struct atom *bind_op = NULL;

	if (target) {
		// target_type is intentionally NULL.
		struct ast_node *target_type = NULL;
		rhs = ast_init_node_func(
				ctx, AST_NODE_NEW, stmt->loc,
				&target, &target_type, 1, NULL, tail);

		bind_op = binop_atom(&ctx->vm->atom_table, ST_OP_BIND);
	} else {
		bind_op = binop_atom(&ctx->vm->atom_table, ST_OP_RSFT);
		rhs = tail;
	}

	struct ast_node *bind_func;
	bind_func = ast_init_node_lookup(
			ctx, AST_NODE_NEW,
			stmt->loc, bind_op);

	struct ast_func_arg bind_args[] = {
		{.value=expr},
		{.value=rhs},
	};

	struct ast_node *result;
	result = ast_init_node_call(
			ctx, AST_NODE_NEW, stmt->loc,
			bind_func, bind_args, 2);

	return result;
}

static struct ast_node *
st_node_visit_array_lit(struct ast_context *ctx, struct stg_module *mod,
		struct stg_location list_loc, struct st_expr_context *expr_ctx,
		struct st_node *node, bool ellipsis)
{
	if (!node) {
		if (ellipsis) {
			// TODO: Better location.
			return ast_init_node_wildcard(
					ctx, AST_NODE_NEW, list_loc);
		} else {
			struct ast_node *nil_lookup;
			nil_lookup = ast_init_node_lookup(
					ctx, AST_NODE_NEW, list_loc,
					mod_atoms(mod, "nil"));

			// TODO: Make nil work without explicit cons.
			return ast_init_node_cons(
					ctx, AST_NODE_NEW, list_loc,
					nil_lookup, NULL, 0);
		}
	}

	struct ast_node *head;
	head = st_node_visit_expr(
			ctx, mod, expr_ctx, node);

	struct ast_node *tail;
	tail = st_node_visit_array_lit(
			ctx, mod, list_loc, expr_ctx,
			node->next_sibling, ellipsis);

	struct ast_func_arg cons_args[] = {
		{.value=head}, {.value=tail}
	};

	struct ast_node *cons_lookup;
	cons_lookup = ast_init_node_lookup(
			ctx, AST_NODE_NEW, node->loc,
			mod_atoms(mod, "cons"));

	return ast_init_node_call(
			ctx, AST_NODE_NEW, node->loc,
			cons_lookup, cons_args, 2);
}

struct ast_node *
st_node_visit_expr(struct ast_context *ctx, struct stg_module *mod,
		struct st_expr_context *expr_ctx, struct st_node *node)
{
	switch (node->type) {

		case ST_NODE_ACCESS:
			{
				struct ast_node *target;
				target = st_node_visit_expr(
						ctx, mod, expr_ctx,
						node->ACCESS.target);
				struct ast_node *access;
				access = ast_init_node_access(
						ctx, AST_NODE_NEW, node->loc,
						target, node->ACCESS.name);

				return access;
			}
			break;

	case ST_NODE_BIN_OP: {
		struct ast_func_arg func_args[] = {
			{vm_atoms(ctx->vm, "lhs"),
				st_node_visit_expr(ctx, mod, expr_ctx,
						node->BIN_OP.lhs)},
			{vm_atoms(ctx->vm, "rhs"),
				st_node_visit_expr(ctx, mod, expr_ctx,
						node->BIN_OP.rhs)},
		};

		struct atom *op_name;
		op_name =
			binop_atom(&ctx->vm->atom_table,
					   node->BIN_OP.op);

		struct ast_node *func;

		func = ast_init_node_lookup(
				ctx, AST_NODE_NEW, node->BIN_OP.loc, op_name);

		struct ast_node *call;
		call = ast_init_node_call(
				ctx, AST_NODE_NEW, node->loc, func,
				func_args, ARRAY_LENGTH(func_args));

		return call;
	 }

	case ST_NODE_BIND: {
		struct ast_func_arg func_args[] = {
			{vm_atoms(ctx->vm, "src"),
				st_node_visit_expr(ctx, mod, expr_ctx,
						node->BIN_OP.lhs)},
			{vm_atoms(ctx->vm, "drain"),
				st_node_visit_expr(ctx, mod, expr_ctx,
						node->BIN_OP.rhs)},
		};

		struct ast_node *func;

		// TODO: Lookup.

		struct atom *op_name;
		op_name = vm_atoms(ctx->vm, "op->");

		func = ast_init_node_lookup(
				ctx, AST_NODE_NEW, node->loc, op_name);

		struct ast_node *call;
		call = ast_init_node_call(
				ctx, AST_NODE_NEW, node->loc, func,
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

		struct st_expr_context sub_expr_ctx = *expr_ctx;

		if (has_templ_params) {
			// func is assigned to body further down.
			templ = ast_init_node_templ(ctx, AST_NODE_NEW,
					proto_node->loc, NULL);
			sub_expr_ctx.pattern = &templ->templ.pattern;
		}


		for (size_t i = 0; i < params_decl.num_members; i++) {
			if (params_decl.types[i]) {
				params[i] = st_node_visit_expr(
						ctx, mod, &sub_expr_ctx, params_decl.types[i]);
			} else {
				params[i] = NULL;
			}
		}

		if (ret_type_decl) {
			ret_type = st_node_visit_expr(
					ctx, mod, &sub_expr_ctx, ret_type_decl);
		}

		struct ast_node *func;

		struct st_node *body_decl;
		body_decl = node->LAMBDA.body;

		if (node->LAMBDA.special) {
			if (body_decl->SPECIAL.name == vm_atoms(ctx->vm, "native") ||
					body_decl->SPECIAL.name == vm_atoms(ctx->vm, "nativeImpure")) {
				struct st_node *args = body_decl->SPECIAL.args;

				if (!args || args->next_sibling) {
					size_t num_args_provided = 0;
					for (struct st_node *nd = args;
							nd != NULL; nd = nd->next_sibling) {
						num_args_provided += 1;
					}
					stg_error(ctx->err, body_decl->loc,
							"@%.*s expected exactly one argument, got %zu.",
							ALIT(body_decl->SPECIAL.name),
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

			func = ast_init_node_func_native(
					ctx, AST_NODE_NEW, node->loc,
					params_decl.names, params, params_decl.num_members,
					ret_type, args->STR_LIT);
			} else {
				stg_error(ctx->err, body_decl->loc,
						"Invalid special expression.");
				return NULL;
			}
		} else {
			struct st_expr_context body_ctx = *expr_ctx;
			body_ctx.init_target = NULL;
			body = st_node_visit_expr(ctx, mod, &body_ctx, body_decl);

			if (!body) {
				return NULL;
			}

			func = ast_init_node_func(
					ctx, AST_NODE_NEW, node->loc,
					params_decl.names, params, params_decl.num_members,
					ret_type, body);
		}

		free(params_decl.names);
		free(params_decl.types);

		if (templ) {
			templ->templ.pattern.node = func;
			return templ;
		} else {
			return func;
		}
	} break;

	case ST_NODE_FUNC_CALL:
	case ST_NODE_TEMPL_INST:
	 {
		struct st_node *args, *func_node;
		if (node->type == ST_NODE_FUNC_CALL) {
			assert(node->FUNC_CALL.params);
			assert(node->FUNC_CALL.params->type == ST_NODE_TUPLE_LIT);

			args = node->FUNC_CALL.params->TUPLE_LIT.items;
			func_node = node->FUNC_CALL.ident;
		} else {
			assert(node->TEMPL_INST.params);
			assert(node->TEMPL_INST.params->type == ST_NODE_TUPLE_LIT);

			args = node->TEMPL_INST.params->TUPLE_LIT.items;
			func_node = node->TEMPL_INST.ident;
		}

		struct ast_node *func;
		func = st_node_visit_expr(
				ctx, mod, expr_ctx,
				func_node);

		size_t num_args = 0;
		for (struct st_node *arg = args;
				arg != NULL;
				arg = arg->next_sibling) {
			num_args += 1;
		}

		struct ast_func_arg func_args[num_args];
		bool failed = false;

		{
			struct st_node *arg = args;
			for (size_t i = 0; i < num_args; i++) {
				assert(arg != NULL);
				assert(arg->type == ST_NODE_TUPLE_LIT_ITEM);
				func_args[i].name = arg->TUPLE_LIT_ITEM.name;
				func_args[i].value =
					st_node_visit_expr(ctx, mod,
							expr_ctx,
							arg->TUPLE_LIT_ITEM.value);
				if (!func_args[i].value) {
					failed = true;
				}

				arg = arg->next_sibling;
			}
		}

		if (failed) {
			return NULL;
		}

		if (node->type == ST_NODE_FUNC_CALL) {
			return ast_init_node_call(
					ctx, AST_NODE_NEW, node->loc,
					func, func_args, num_args);
		} else {
			return ast_init_node_cons(
					ctx, AST_NODE_NEW, node->loc,
					func, func_args, num_args);
		}
	} break;

	case ST_NODE_FUNC_PROTO:
	{
		struct st_tuple_members params_decl;
		struct st_node *ret_type_decl;

		st_node_unpack_func_proto(node,
				&params_decl, &ret_type_decl);

		assert(ret_type_decl);

		struct ast_node *param_types[params_decl.num_members];

		for (size_t i = 0; i < params_decl.num_members; i++) {
			param_types[i] = st_node_visit_expr(
					ctx, mod, expr_ctx,
					params_decl.types[i]);
		}

		struct ast_node *ret_type;
		ret_type = st_node_visit_expr(
				ctx, mod, expr_ctx,
				ret_type_decl);

		free(params_decl.names);
		free(params_decl.types);

		return ast_init_node_func_type(
				ctx, AST_NODE_NEW, node->loc,
				param_types, params_decl.num_members,
				ret_type);
	}
	break;

	case ST_NODE_MODULE:
	case ST_NODE_OBJECT_DECL:
	{
		enum ast_composite_kind kind;
		struct st_node *member;
		if (node->type == ST_NODE_OBJECT_DECL) {
			member = node->OBJECT_DECL.body;
			kind = AST_COMPOSITE_STRUCT;
		} else {
			member = node->MODULE.body;
			kind = AST_COMPOSITE_MODULE;
		}

		struct ast_node *struct_node;
		struct_node = ast_init_node_composite(
				ctx, AST_NODE_NEW, node->loc, kind);

		while (member) {
			st_node_visit_stmt(ctx, mod, struct_node, member);
			member = member->next_sibling;
		}

		if (node->type == ST_NODE_OBJECT_DECL &&
				node->OBJECT_DECL.params) {
			return st_node_create_template(
					ctx, mod, node->OBJECT_DECL.params,
					struct_node);
		} else {
			return struct_node;
		}
	}

	case ST_NODE_OBJECT_INST:
	{
		struct ast_node *func;

		func = st_node_visit_expr(ctx, mod, expr_ctx, node->OBJECT_INST.name);

		size_t num_args = 0;

		struct st_node *args = node->OBJECT_INST.body;

		for (struct st_node *arg = args;
				arg != NULL;
				arg = arg->next_sibling) {
			num_args += 1;
		}

		struct ast_func_arg func_args[num_args];
		memset(func_args, 0, num_args * sizeof(struct ast_func_arg));

		{
			struct st_node *arg = args;

			bool err = false;

			for (size_t i = 0; i < num_args; i++) {
				assert(arg != NULL);

				bool invalid_arg = false;
				const char *node_kind_name = NULL;

				assert(arg->type == ST_NODE_STMT);

				struct st_node *stmt;
				stmt = arg->STMT.stmt;

				if (!stmt) {
					// Something went wrong while parsing this object. An error
					// should already have been emitted.
					return NULL;
				}

				switch (stmt->type) {
					case ST_NODE_ASSIGN_STMT:
						if (stmt->ASSIGN_STMT.decl) {
							stg_error(ctx->err, stmt->loc,
									"Members can not be declared on object instantiation.");
							err = true;
							break;
						}
						if (stmt->ASSIGN_STMT.ident->type != ST_NODE_IDENT) {
							stg_error(ctx->err, stmt->ASSIGN_STMT.ident->loc,
									"[TODO] Object instantiation assignments must be trivial.");
							err = true;
							break;
						}
						if (stmt->ASSIGN_STMT.overridable) {
							stg_error(ctx->err, stmt->ASSIGN_STMT.ident->loc,
									"[TODO] Object instantiation assignments can not be overridable.");
							err = true;
							break;
						}

						func_args[i].name  = stmt->ASSIGN_STMT.ident->IDENT;
						func_args[i].value =
							st_node_visit_expr(
									ctx, mod, expr_ctx,
									stmt->ASSIGN_STMT.body);
						break;

					case ST_NODE_MOD_STMT:
						invalid_arg = true;
						node_kind_name = "mod";
						break;

					case ST_NODE_USE_STMT:
						invalid_arg = true;
						node_kind_name = "use";
						break;

					default:
						assert(stmt->type < ST_NODES_LEN);
						invalid_arg = true;
						node_kind_name = "Expression";
						break;
				}

				if (invalid_arg) {
					err = true;
					assert(node_kind_name != NULL);
					stg_error(ctx->err, stmt->loc,
							"%s statements are not allowed in object instantiations (yet).",
							node_kind_name);
				}

				arg = arg->next_sibling;
			}

			if (err) {
				return NULL;
			}
		}

		return ast_init_node_inst(
				ctx, AST_NODE_NEW, node->loc,
				func, func_args, num_args);
		break;
	}

	case ST_NODE_VARIANT_DECL:
	{
		struct st_node *option;
		option = node->VARIANT_DECL.items;
		if (!option) {
			return NULL;
		}

		struct ast_node *variant;
		variant = ast_init_node_variant(
				ctx, AST_NODE_NEW, node->loc);

		while (option) {
			assert(option->type == ST_NODE_VARIANT_ITEM);

			struct atom *name;
			name = option->VARIANT_ITEM.name;

			struct ast_node *data_type = NULL;
			if (option->VARIANT_ITEM.data_type) {
				data_type = st_node_visit_expr(
						ctx, mod, expr_ctx,
						option->VARIANT_ITEM.data_type);
			}

			ast_node_variant_add_option(
					ctx, variant, option->loc,
					name, data_type);

			option = option->next_sibling;
		}

		if (node->VARIANT_DECL.params) {
			return st_node_create_template(
					ctx, mod, node->VARIANT_DECL.params,
					variant);
		}

		return variant;
	}

	case ST_NODE_SPECIAL:
		if (node->SPECIAL.name == vm_atoms(ctx->vm, "native")) {
			struct st_node *args = node->SPECIAL.args;

			if (!args || args->next_sibling) {
				size_t num_args_provided = 0;
				for (struct st_node *nd = args;
						nd != NULL; nd = nd->next_sibling) {
					num_args_provided += 1;
				}
				stg_error(ctx->err, node->loc,
						"@%.*s expected exactly one argument, got %zu.",
						ALIT(node->SPECIAL.name),
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

			struct atom *native_obj_name;
			native_obj_name = vm_atom(ctx->vm, args->STR_LIT);

			return ast_init_node_lit_native(
					ctx, AST_NODE_NEW, node->loc, native_obj_name);
		} else {
			stg_error(ctx->err, node->loc,
					"Invalid special expression '%.*s'.",
					ALIT(node->SPECIAL.name));
			return NULL;
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

			n = st_node_visit_expr(ctx, mod, expr,
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

	case ST_NODE_MATCH_EXPR:
		{
			struct ast_node *value;

			value = st_node_visit_expr(
					ctx, mod, expr_ctx,
					node->MATCH_EXPR.value);

			size_t num_cases = 0;
			struct st_node *case_iter;
			case_iter = node->MATCH_EXPR.cases;
			while (case_iter) {
				assert(case_iter->type == ST_NODE_MATCH_CASE);
				num_cases += 1;
				case_iter = case_iter->next_sibling;
			}

			struct ast_match_case cases[num_cases];
			memset(cases, 0, sizeof(struct ast_match_case) * num_cases);
			size_t case_i = 0;

			case_iter = node->MATCH_EXPR.cases;
			while (case_iter) {
				assert(case_i < num_cases);
				assert(case_iter->type == ST_NODE_MATCH_CASE);

				struct st_expr_context case_pat_ctx = *expr_ctx;
				case_pat_ctx.pattern = &cases[case_i].pattern;
				cases[case_i].pattern.node =
					st_node_visit_expr(
							ctx, mod, &case_pat_ctx,
							case_iter->MATCH_CASE.pattern);

				cases[case_i].expr =
					st_node_visit_expr(
							ctx, mod, expr_ctx,
							case_iter->MATCH_CASE.expr);

				case_i += 1;
				case_iter = case_iter->next_sibling;
			}

			assert(case_i == num_cases);

			return ast_init_node_match(
					ctx, AST_NODE_NEW, node->loc,
					value, cases, num_cases);
		}
		break;

	case ST_NODE_WILDCARD:
		if (!expr_ctx->pattern) {
			stg_error(ctx->err, node->loc,
					"Wildcards, '_', can only appear inside of patterns.");
			return NULL;
		}

		return ast_init_node_wildcard(
				ctx, AST_NODE_NEW, node->loc);

	case ST_NODE_INIT_EXPR:
		{
			if (!expr_ctx->init_target) {
				stg_error(ctx->err, node->loc,
						"Init expressions can not appear here.");
				return NULL;
			}

			struct ast_node *expr;
			expr = st_node_visit_expr(
					ctx, mod, expr_ctx, node->INIT_EXPR.expr);

			ast_init_expr_id expr_id;
			expr_id = ast_node_composite_add_init_expr(
					ctx, expr_ctx->init_target, expr);
			if (expr_id < 0) {
				return NULL;
			}

			return ast_init_node_init_expr(
					ctx, AST_NODE_NEW, node->loc, expr_id);
		}
		break;

	case ST_NODE_DO_EXPR:
		{
			struct st_node *stmt;
			stmt = node->DO_EXPR.body;

			if (!stmt) {
				stg_error(ctx->err, node->loc,
						"Expected at least one statement in the do-expression.");
				return NULL;
			}

			return st_node_visit_do_stmts(
					ctx, mod, expr_ctx, stmt);
		}
		break;

	case ST_NODE_TUPLE_LIT:
		break;

	case ST_NODE_ARRAY_LIT:
		return st_node_visit_array_lit(
				ctx, mod, node->loc, expr_ctx,
				node->ARRAY_LIT.items,
				node->ARRAY_LIT.ellipsis);

	case ST_NODE_NUM_LIT: {
		struct object obj;

		obj.data = &node->NUM_LIT;
		obj.type = ctx->vm->default_types.integer;

		return ast_init_node_lit(
				ctx, AST_NODE_NEW, node->loc,
				register_object(ctx->vm, &mod->store, obj));
	} break;

	case ST_NODE_STR_LIT:
	{
		struct object obj;

		obj.data = &node->STR_LIT;
		obj.type = ctx->vm->default_types.string;

		return ast_init_node_lit(
				ctx, AST_NODE_NEW, node->loc,
				register_object(ctx->vm, &mod->store, obj));
	}
	break;

	case ST_NODE_IDENT:
		return ast_init_node_lookup(
				ctx, AST_NODE_NEW, node->loc, node->IDENT);

	case ST_NODE_TEMPLATE_VAR:
		if (!expr_ctx->pattern) {
			stg_error(ctx->err, node->loc,
					"Template params can not be used here.");
		} else {
			ast_pattern_register_param(
					ctx, expr_ctx->pattern,
					node->TEMPLATE_VAR.name,
					NULL, node->loc);
		}

		return ast_init_node_lookup(
				ctx, AST_NODE_NEW, node->loc,
				node->TEMPLATE_VAR.name);

	case ST_NODE_MOD_STMT:
		{
			vm_request_module(ctx->vm,
					mod->id,
					node->MOD_STMT.ident,
					VM_REQUEST_MOD_NO_LOC);

			return ast_init_node_mod(
					ctx, AST_NODE_NEW, node->loc,
					node->MOD_STMT.ident);
		}

	case ST_NODE_TYPE_CLASS_DECL:
		{
			size_t num_members = 0;
			struct st_node *body_stmt_iter;
			body_stmt_iter = node->TYPE_CLASS_DECL.body;

			while (body_stmt_iter) {
				assert(body_stmt_iter->type == ST_NODE_STMT);
				struct st_node *body_stmt = body_stmt_iter->STMT.stmt;
				if (body_stmt->type != ST_NODE_ASSIGN_STMT) {
					stg_error(ctx->err, body_stmt->loc,
							"Only member declarations can appear in type class declarations.");
					continue;
				}

				if (!body_stmt->ASSIGN_STMT.decl) {
					stg_error(ctx->err, body_stmt->loc,
							"Only member declarations can appear in type class declarations.");
					continue;
				}

				if (body_stmt->ASSIGN_STMT.body != NULL) {
					// TODO: Allow template declarations of type class member values.
					stg_error(ctx->err, body_stmt->loc,
							"Type class members can not be assigned during type class declaration.");
					continue;
				}

				if (body_stmt->ASSIGN_STMT.type == NULL) {
					// TODO: Allow template declarations of type class member values.
					stg_error(ctx->err, body_stmt->loc,
							"Type class members must have an explicit type.");
					continue;
				}

				if (body_stmt->ASSIGN_STMT.ident->type != ST_NODE_IDENT) {
					stg_error(ctx->err, body_stmt->ASSIGN_STMT.ident->loc,
							"Type class members must have trivial names.");
					continue;
				}

				num_members += 1;
				body_stmt_iter = body_stmt_iter->next_sibling;
			}

			body_stmt_iter = node->TYPE_CLASS_DECL.body;

			struct st_expr_context member_ctx = {0};
			struct ast_type_class_member members[num_members];
			memset(members, 0, sizeof(struct ast_type_class_member) * num_members);

			size_t member_i = 0;
			while (body_stmt_iter) {
				assert(body_stmt_iter->type == ST_NODE_STMT);
				struct st_node *body_stmt = body_stmt_iter->STMT.stmt;

				if (body_stmt->type != ST_NODE_ASSIGN_STMT ||
						!body_stmt->ASSIGN_STMT.decl ||
						body_stmt->ASSIGN_STMT.body != NULL ||
						body_stmt->ASSIGN_STMT.type  == NULL ||
						body_stmt->ASSIGN_STMT.ident->type != ST_NODE_IDENT) {
					continue;
				}

				struct atom *name = body_stmt->ASSIGN_STMT.ident->IDENT;
				struct st_node *type_node = body_stmt->ASSIGN_STMT.type;

				member_ctx.pattern = &members[member_i].type;

				struct ast_node *type;
				type = st_node_visit_expr(
						ctx, mod, &member_ctx, type_node);

				members[member_i].name = name;
				members[member_i].type.node = type;

				member_i += 1;
				body_stmt_iter = body_stmt_iter->next_sibling;
			}
			assert(member_i == num_members);

			struct ast_node *tc;
			tc = ast_init_node_type_class(
					ctx, AST_NODE_NEW, node->loc,
					members, num_members);

			struct st_expr_context param_ctx = *expr_ctx;
			param_ctx.pattern = &tc->type_class.pattern;

			struct st_node *param_iter;
			param_iter = node->TYPE_CLASS_DECL.params;
			while (param_iter) {
				st_node_visit_templ_param(ctx, mod,
						&tc->type_class.pattern, param_iter,
						TYPE_UNSET);
				param_iter = param_iter->next_sibling;
			}

			return tc;
		}

	default:
		panic("Invalid node '%.*s' in expr.",
			  LIT(st_node_names[node->type]));
		break;
	}

	panic("Unhandled node '%.*s' in expr.",
		  LIT(st_node_names[node->type]));
	return NULL;
}
