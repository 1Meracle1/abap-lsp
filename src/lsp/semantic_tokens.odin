package lsp

import "../cache"
import "core:encoding/json"
import "core:fmt"

import "../lang/ast"
import "../lang/lexer"
import "../lang/symbols"

handle_semantic_tokens :: proc(srv: ^Server, id: json.Value, params: json.Value) {
	semantic_params: SemanticTokensParams
	if err := unmarshal(params, semantic_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("semanticTokens request unmarshal failed: %v", err)
		log_trace(srv, descr)
		reply_error(srv, id, .ParseError, descr)
		return
	}

	snap := cache.get_snapshot(srv.storage, semantic_params.textDocument.uri)
	if snap == nil {
		result := SemanticTokens {
			data = {},
		}
		reply(srv, id, result)
		return
	}
	defer cache.release_snapshot(snap)

	tokens := collect_semantic_tokens(snap)
	encoded := encode_semantic_tokens(snap.text, tokens[:])
	result := SemanticTokens {
		data = encoded[:],
	}
	reply(srv, id, result)
}

// A raw semantic token before encoding
SemanticToken :: struct {
	offset:    int,
	length:    int,
	type:      SemanticTokenType,
	modifiers: u32,
}

// Collects all semantic tokens from the snapshot's AST and symbol table
collect_semantic_tokens :: proc(snap: ^cache.Snapshot) -> [dynamic]SemanticToken {
	tokens := make([dynamic]SemanticToken, context.temp_allocator)

	// Collect tokens from declarations
	for decl in snap.ast.decls {
		collect_tokens_from_stmt(&tokens, decl, snap)
	}

	// Collect tokens from comments
	for comment in snap.ast.comments {
		append(
			&tokens,
			SemanticToken {
				offset = comment.range.start,
				length = comment.range.end - comment.range.start,
				type = .Comment,
				modifiers = 0,
			},
		)
	}

	// Sort tokens by offset for proper delta encoding
	sort_tokens(&tokens)

	return tokens
}

collect_tokens_from_stmt :: proc(
	tokens: ^[dynamic]SemanticToken,
	stmt: ^ast.Stmt,
	snap: ^cache.Snapshot,
) {
	if stmt == nil {
		return
	}

	#partial switch s in stmt.derived_stmt {
	case ^ast.Data_Inline_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		collect_tokens_from_expr(tokens, s.value, snap, nil)

	case ^ast.Data_Typed_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		if s.typed != nil {
			collect_tokens_from_type_expr(tokens, s.typed)
		}

	case ^ast.Data_Typed_Chain_Decl:
		for decl in s.decls {
			if decl.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = decl.ident.range.start,
						length = decl.ident.range.end - decl.ident.range.start,
						type = .Variable,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
			if decl.typed != nil {
				collect_tokens_from_type_expr(tokens, decl.typed)
			}
		}

	case ^ast.Types_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Type,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		if s.typed != nil {
			collect_tokens_from_type_expr(tokens, s.typed)
		}

	case ^ast.Types_Chain_Decl:
		for decl in s.decls {
			if decl.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = decl.ident.range.start,
						length = decl.ident.range.end - decl.ident.range.start,
						type = .Type,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
			if decl.typed != nil {
				collect_tokens_from_type_expr(tokens, decl.typed)
			}
		}

	case ^ast.Types_Struct_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Type,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		collect_tokens_from_struct_components(tokens, s.components[:], snap)

	case ^ast.Const_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
					1 << u32(SemanticTokenModifier.Readonly),
				},
			)
		}
		if s.typed != nil {
			collect_tokens_from_type_expr(tokens, s.typed)
		}
		if s.value != nil {
			collect_tokens_from_expr(tokens, s.value, snap, nil)
		}

	case ^ast.Const_Chain_Decl:
		for decl in s.decls {
			if decl.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = decl.ident.range.start,
						length = decl.ident.range.end - decl.ident.range.start,
						type = .Variable,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
						1 << u32(SemanticTokenModifier.Readonly),
					},
				)
			}
			if decl.typed != nil {
				collect_tokens_from_type_expr(tokens, decl.typed)
			}
			if decl.value != nil {
				collect_tokens_from_expr(tokens, decl.value, snap, nil)
			}
		}

	case ^ast.Const_Struct_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
					1 << u32(SemanticTokenModifier.Readonly),
				},
			)
		}
		collect_tokens_from_const_struct_components(tokens, s.components[:], snap)

	case ^ast.Data_Struct_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		collect_tokens_from_data_struct_components(tokens, s.components[:], snap)

	case ^ast.Form_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Function,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
					1 << u32(SemanticTokenModifier.Definition),
				},
			)
		}

		form_scope: ^symbols.SymbolTable
		if s.ident != nil {
			if form_sym, ok := snap.symbol_table.symbols[s.ident.name]; ok {
				form_scope = form_sym.child_scope
			}
		}

		for param in s.tables_params {
			collect_tokens_from_form_param(tokens, param)
		}
		for param in s.using_params {
			collect_tokens_from_form_param(tokens, param)
		}
		for param in s.changing_params {
			collect_tokens_from_form_param(tokens, param)
		}

		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}

	case ^ast.Assign_Stmt:
		for lhs in s.lhs {
			collect_tokens_from_expr(tokens, lhs, snap, nil)
		}
		for rhs in s.rhs {
			collect_tokens_from_expr(tokens, rhs, snap, nil)
		}

	case ^ast.Expr_Stmt:
		collect_tokens_from_expr(tokens, s.expr, snap, nil)

	case ^ast.If_Stmt:
		collect_tokens_from_expr(tokens, s.cond, snap, nil)
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}
		for branch in s.elseif_branches {
			collect_tokens_from_expr(tokens, branch.cond, snap, nil)
			for branch_stmt in branch.body {
				collect_tokens_from_stmt(tokens, branch_stmt, snap)
			}
		}
		for else_stmt in s.else_body {
			collect_tokens_from_stmt(tokens, else_stmt, snap)
		}

	case ^ast.Case_Stmt:
		collect_tokens_from_expr(tokens, s.expr, snap, nil)
		for branch in s.branches {
			if !branch.is_others {
				collect_tokens_from_expr(tokens, branch.expr, snap, nil)
			}
			for body_stmt in branch.body {
				collect_tokens_from_stmt(tokens, body_stmt, snap)
			}
		}

	case ^ast.While_Stmt:
		collect_tokens_from_expr(tokens, s.cond, snap, nil)
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}

	case ^ast.Clear_Stmt:
		for expr in s.exprs {
			collect_tokens_from_expr(tokens, expr, snap, nil)
		}

	case ^ast.Block_Stmt:
		for block_stmt in s.stmts {
			collect_tokens_from_stmt(tokens, block_stmt, snap)
		}

	case ^ast.Return_Stmt:
		for result in s.results {
			collect_tokens_from_expr(tokens, result, snap, nil)
		}

	case ^ast.Class_Def_Decl:
		// CLASS name DEFINITION
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Class,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
					1 << u32(SemanticTokenModifier.Definition),
				},
			)
		}
		if s.inheriting_from != nil {
			collect_tokens_from_type_expr(tokens, s.inheriting_from)
		}
		for section in s.sections {
			collect_tokens_from_class_section(tokens, section, snap)
		}

	case ^ast.Class_Impl_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Class,
					modifiers = 0,
				},
			)
		}
		for method in s.methods {
			collect_tokens_from_stmt(tokens, method, snap)
		}

	case ^ast.Interface_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Interface,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
					1 << u32(SemanticTokenModifier.Definition),
				},
			)
		}
		for method in s.methods {
			collect_tokens_from_stmt(tokens, method, snap)
		}
		for type_decl in s.types {
			collect_tokens_from_stmt(tokens, type_decl, snap)
		}
		for data_decl in s.data {
			collect_tokens_from_stmt(tokens, data_decl, snap)
		}

	case ^ast.Method_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Method,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		for param in s.params {
			collect_tokens_from_method_param(tokens, param)
		}
		for exc in s.raising {
			collect_tokens_from_type_expr(tokens, exc)
		}

	case ^ast.Method_Impl:
		if s.ident != nil {
			collect_tokens_from_expr(tokens, s.ident, snap, nil)
		}
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}

	case ^ast.Attr_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Property,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		if s.typed != nil {
			collect_tokens_from_type_expr(tokens, s.typed)
		}
		if s.value != nil {
			collect_tokens_from_expr(tokens, s.value, snap, nil)
		}

	case ^ast.Interfaces_Decl:
		for name in s.names {
			append(
				tokens,
				SemanticToken {
					offset = name.range.start,
					length = name.range.end - name.range.start,
					type = .Interface,
					modifiers = 0,
				},
			)
		}

	case ^ast.Report_Decl:
		if s.name != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.name.range.start,
					length = s.name.range.end - s.name.range.start,
					type = .Namespace,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}

	case ^ast.Include_Decl:
		if s.name != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.name.range.start,
					length = s.name.range.end - s.name.range.start,
					type = .Namespace,
					modifiers = 0,
				},
			)
		}

	case ^ast.Event_Block:
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}

	case ^ast.Call_Screen_Stmt:
		if s.screen_no != nil {
			collect_tokens_from_expr(tokens, s.screen_no, snap, nil)
		}

	case ^ast.Module_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Function,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
					1 << u32(SemanticTokenModifier.Definition),
				},
			)
		}
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}

	case ^ast.Message_Stmt:
		if s.msg_expr != nil {
			collect_tokens_from_expr(tokens, s.msg_expr, snap, nil)
		}
		if s.msg_type != nil {
			collect_tokens_from_expr(tokens, s.msg_type, snap, nil)
		}
		if s.display_like != nil {
			collect_tokens_from_expr(tokens, s.display_like, snap, nil)
		}
		for arg in s.with_args {
			collect_tokens_from_expr(tokens, arg, snap, nil)
		}
		if s.into_target != nil {
			collect_tokens_from_expr(tokens, s.into_target, snap, nil)
		}

	case ^ast.Insert_Stmt:
		if s.value_expr != nil {
			collect_tokens_from_expr(tokens, s.value_expr, snap, nil)
		}
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}
		if s.source != nil {
			collect_tokens_from_expr(tokens, s.source, snap, nil)
		}

	case ^ast.Sort_Stmt:
		if s.itab != nil {
			collect_tokens_from_expr(tokens, s.itab, snap, nil)
		}
		for col in s.cols_by {
			if col.col != nil {
				collect_tokens_from_expr(tokens, col.col, snap, nil)
			}
		}

	case ^ast.Append_Stmt:
		if s.source != nil {
			collect_tokens_from_expr(tokens, s.source, snap, nil)
		}
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}
		if s.assigning_target != nil {
			collect_tokens_from_expr(tokens, s.assigning_target, snap, nil)
		}

	case ^ast.Delete_Stmt:
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}
		if s.where_cond != nil {
			collect_tokens_from_expr(tokens, s.where_cond, snap, nil)
		}
		if s.index_expr != nil {
			collect_tokens_from_expr(tokens, s.index_expr, snap, nil)
		}

	case ^ast.Field_Symbol_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		if s.typed != nil {
			collect_tokens_from_type_expr(tokens, s.typed)
		}

	case ^ast.Controls_Decl:
		if s.ident != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.ident.range.start,
					length = s.ident.range.end - s.ident.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		if s.screen_dynnr != nil {
			collect_tokens_from_expr(tokens, s.screen_dynnr, snap, nil)
		}

	case ^ast.Controls_Chain_Decl:
		for decl in s.decls {
			if decl.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = decl.ident.range.start,
						length = decl.ident.range.end - decl.ident.range.start,
						type = .Variable,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
			if decl.screen_dynnr != nil {
				collect_tokens_from_expr(tokens, decl.screen_dynnr, snap, nil)
			}
		}
	
	case ^ast.Condense_Stmt:
		if s.text != nil {
			collect_tokens_from_expr(tokens, s.text, snap, nil)
		}

	case ^ast.Loop_Stmt:
		if s.itab != nil {
			collect_tokens_from_expr(tokens, s.itab, snap, nil)
		}
		if s.into_target != nil {
			collect_tokens_from_expr(tokens, s.into_target, snap, nil)
		}
		if s.assigning_target != nil {
			collect_tokens_from_expr(tokens, s.assigning_target, snap, nil)
		}
		if s.from_expr != nil {
			collect_tokens_from_expr(tokens, s.from_expr, snap, nil)
		}
		if s.to_expr != nil {
			collect_tokens_from_expr(tokens, s.to_expr, snap, nil)
		}
		if s.where_cond != nil {
			collect_tokens_from_expr(tokens, s.where_cond, snap, nil)
		}
		if s.group_var != nil {
			collect_tokens_from_expr(tokens, s.group_var, snap, nil)
		}
		if s.group_by != nil {
			for comp in s.group_by.components {
				if comp.name != nil {
					append(
						tokens,
						SemanticToken {
							offset = comp.name.range.start,
							length = comp.name.range.end - comp.name.range.start,
							type = .Property,
							modifiers = 0,
						},
					)
				}
				if comp.value != nil {
					collect_tokens_from_expr(tokens, comp.value, snap, nil)
				}
			}
		}
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}

	case ^ast.Read_Table_Stmt:
		if s.itab != nil {
			collect_tokens_from_expr(tokens, s.itab, snap, nil)
		}
		if s.index_expr != nil {
			collect_tokens_from_expr(tokens, s.index_expr, snap, nil)
		}
		if s.using_key != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.using_key.range.start,
					length = s.using_key.range.end - s.using_key.range.start,
					type = .Property,
					modifiers = 0,
				},
			)
		}
		if s.into_target != nil {
			collect_tokens_from_expr(tokens, s.into_target, snap, nil)
		}
		if s.assigning_target != nil {
			collect_tokens_from_expr(tokens, s.assigning_target, snap, nil)
		}
		if s.key != nil {
			for comp in s.key.components {
				if comp.name != nil {
					append(
						tokens,
						SemanticToken {
							offset = comp.name.range.start,
							length = comp.name.range.end - comp.name.range.start,
							type = .Property,
							modifiers = 0,
						},
					)
				}
				if comp.value != nil {
					collect_tokens_from_expr(tokens, comp.value, snap, nil)
				}
			}
		}

	case ^ast.Call_Function_Stmt:
		// Highlight the function name as a function
		if s.func_name != nil {
			collect_tokens_from_expr(tokens, s.func_name, snap, nil)
		}
		// Highlight destination if present
		if s.destination != nil {
			collect_tokens_from_expr(tokens, s.destination, snap, nil)
		}
		// Collect tokens from all parameter sections
		collect_tokens_from_call_function_params(tokens, s.exporting[:], snap)
		collect_tokens_from_call_function_params(tokens, s.importing[:], snap)
		collect_tokens_from_call_function_params(tokens, s.tables[:], snap)
		collect_tokens_from_call_function_params(tokens, s.changing[:], snap)
		collect_tokens_from_call_function_params(tokens, s.exceptions[:], snap)

	case ^ast.Select_Stmt:
		// Collect tokens from field list
		for field in s.fields {
			collect_tokens_from_expr(tokens, field, snap, nil)
		}
		// Collect FROM table
		if s.from_table != nil {
			collect_tokens_from_expr(tokens, s.from_table, snap, nil)
		}
		// Collect FROM alias
		if s.from_alias != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.from_alias.range.start,
					length = s.from_alias.range.end - s.from_alias.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		// Collect tokens from joins
		for join in s.joins {
			if join.table != nil {
				collect_tokens_from_expr(tokens, join.table, snap, nil)
			}
			if join.alias != nil {
				append(
					tokens,
					SemanticToken {
						offset = join.alias.range.start,
						length = join.alias.range.end - join.alias.range.start,
						type = .Variable,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
			if join.on_cond != nil {
				collect_tokens_from_expr(tokens, join.on_cond, snap, nil)
			}
		}
		// Collect INTO target
		if s.into_target != nil {
			collect_tokens_from_expr(tokens, s.into_target, snap, nil)
		}
		// Collect WHERE condition
		if s.where_cond != nil {
			collect_tokens_from_expr(tokens, s.where_cond, snap, nil)
		}
		// Collect ORDER BY columns
		for col in s.order_by {
			collect_tokens_from_expr(tokens, col, snap, nil)
		}
		// Collect GROUP BY columns
		for col in s.group_by {
			collect_tokens_from_expr(tokens, col, snap, nil)
		}
		// Collect HAVING condition
		if s.having_cond != nil {
			collect_tokens_from_expr(tokens, s.having_cond, snap, nil)
		}
		// Collect FOR ALL ENTRIES
		if s.for_all_entries != nil {
			collect_tokens_from_expr(tokens, s.for_all_entries, snap, nil)
		}
		// Collect UP TO ROWS
		if s.up_to_rows != nil {
			collect_tokens_from_expr(tokens, s.up_to_rows, snap, nil)
		}
		// Collect tokens from body statements
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}
	}
}

collect_tokens_from_call_function_params :: proc(
	tokens: ^[dynamic]SemanticToken,
	params: []^ast.Call_Function_Param,
	snap: ^cache.Snapshot,
) {
	for param in params {
		if param == nil {
			continue
		}
		// Highlight parameter name as a parameter
		if param.name != nil {
			append(
				tokens,
				SemanticToken {
					offset = param.name.range.start,
					length = param.name.range.end - param.name.range.start,
					type = .Parameter,
					modifiers = 0,
				},
			)
		}
		// Collect tokens from the value expression
		if param.value != nil {
			collect_tokens_from_expr(tokens, param.value, snap, nil)
		}
	}
}

collect_tokens_from_struct_components :: proc(
	tokens: ^[dynamic]SemanticToken,
	components: []^ast.Stmt,
	snap: ^cache.Snapshot,
) {
	for comp in components {
		if comp == nil {
			continue
		}

		#partial switch c in comp.derived_stmt {
		case ^ast.Types_Decl:
			if c.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = c.ident.range.start,
						length = c.ident.range.end - c.ident.range.start,
						type = .Property,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
			if c.typed != nil {
				collect_tokens_from_type_expr(tokens, c.typed)
			}

		case ^ast.Types_Struct_Decl:
			if c.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = c.ident.range.start,
						length = c.ident.range.end - c.ident.range.start,
						type = .Type,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
			collect_tokens_from_struct_components(tokens, c.components[:], snap)
		}
	}
}

collect_tokens_from_const_struct_components :: proc(
	tokens: ^[dynamic]SemanticToken,
	components: []^ast.Stmt,
	snap: ^cache.Snapshot,
) {
	for comp in components {
		if comp == nil {
			continue
		}

		#partial switch c in comp.derived_stmt {
		case ^ast.Const_Decl:
			if c.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = c.ident.range.start,
						length = c.ident.range.end - c.ident.range.start,
						type = .Property,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
						1 << u32(SemanticTokenModifier.Readonly),
					},
				)
			}
			if c.typed != nil {
				collect_tokens_from_type_expr(tokens, c.typed)
			}
			if c.value != nil {
				collect_tokens_from_expr(tokens, c.value, snap, nil)
			}

		case ^ast.Const_Struct_Decl:
			if c.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = c.ident.range.start,
						length = c.ident.range.end - c.ident.range.start,
						type = .Variable,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration) |
						1 << u32(SemanticTokenModifier.Readonly),
					},
				)
			}
			collect_tokens_from_const_struct_components(tokens, c.components[:], snap)
		}
	}
}

collect_tokens_from_data_struct_components :: proc(
	tokens: ^[dynamic]SemanticToken,
	components: []^ast.Stmt,
	snap: ^cache.Snapshot,
) {
	for comp in components {
		if comp == nil {
			continue
		}

		#partial switch c in comp.derived_stmt {
		case ^ast.Data_Typed_Decl:
			if c.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = c.ident.range.start,
						length = c.ident.range.end - c.ident.range.start,
						type = .Property,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
			if c.typed != nil {
				collect_tokens_from_type_expr(tokens, c.typed)
			}
			if c.value != nil {
				collect_tokens_from_expr(tokens, c.value, snap, nil)
			}

		case ^ast.Data_Struct_Decl:
			if c.ident != nil {
				append(
					tokens,
					SemanticToken {
						offset = c.ident.range.start,
						length = c.ident.range.end - c.ident.range.start,
						type = .Property,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
			collect_tokens_from_data_struct_components(tokens, c.components[:], snap)
		}
	}
}

collect_tokens_from_form_param :: proc(tokens: ^[dynamic]SemanticToken, param: ^ast.Form_Param) {
	if param == nil {
		return
	}
	if param.ident != nil {
		append(
			tokens,
			SemanticToken {
				offset = param.ident.range.start,
				length = param.ident.range.end - param.ident.range.start,
				type = .Parameter,
				modifiers = 1 << u32(SemanticTokenModifier.Declaration),
			},
		)
	}
	if param.typed != nil {
		collect_tokens_from_type_expr(tokens, param.typed)
	}
}

collect_tokens_from_class_section :: proc(
	tokens: ^[dynamic]SemanticToken,
	section: ^ast.Class_Section,
	snap: ^cache.Snapshot,
) {
	if section == nil {
		return
	}

	for type_decl in section.types {
		collect_tokens_from_stmt(tokens, type_decl, snap)
	}

	for data_decl in section.data {
		collect_tokens_from_stmt(tokens, data_decl, snap)
	}

	for method_decl in section.methods {
		collect_tokens_from_stmt(tokens, method_decl, snap)
	}

	for iface_decl in section.interfaces {
		collect_tokens_from_stmt(tokens, iface_decl, snap)
	}
}

collect_tokens_from_method_param :: proc(
	tokens: ^[dynamic]SemanticToken,
	param: ^ast.Method_Param,
) {
	if param == nil {
		return
	}
	if param.ident != nil {
		append(
			tokens,
			SemanticToken {
				offset = param.ident.range.start,
				length = param.ident.range.end - param.ident.range.start,
				type = .Parameter,
				modifiers = 1 << u32(SemanticTokenModifier.Declaration),
			},
		)
	}
	if param.typed != nil {
		collect_tokens_from_type_expr(tokens, param.typed)
	}
	if param.default != nil {
		collect_tokens_from_expr(tokens, param.default, nil, nil)
	}
}

collect_tokens_from_expr :: proc(
	tokens: ^[dynamic]SemanticToken,
	expr: ^ast.Expr,
	snap: ^cache.Snapshot,
	form_scope: ^symbols.SymbolTable,
) {
	if expr == nil {
		return
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		token_type := SemanticTokenType.Variable
		modifiers: u32 = 0

		if form_scope != nil {
			if sym, ok := form_scope.symbols[e.name]; ok {
				token_type, modifiers = symbol_to_token_type(sym)
			}
		}
		if snap != nil && snap.symbol_table != nil {
			if sym, ok := snap.symbol_table.symbols[e.name]; ok {
				token_type, modifiers = symbol_to_token_type(sym)
			}
		}

		append(
			tokens,
			SemanticToken {
				offset = e.range.start,
				length = e.range.end - e.range.start,
				type = token_type,
				modifiers = modifiers,
			},
		)

	case ^ast.Basic_Lit:
		token_type: SemanticTokenType
		#partial switch e.tok.kind {
		case .String:
			token_type = .String
		case .Number:
			token_type = .Number
		case:
			return // Skip other literals
		}
		append(
			tokens,
			SemanticToken {
				offset = e.range.start,
				length = e.range.end - e.range.start,
				type = token_type,
				modifiers = 0,
			},
		)

	case ^ast.Binary_Expr:
		collect_tokens_from_expr(tokens, e.left, snap, form_scope)
		collect_tokens_from_expr(tokens, e.right, snap, form_scope)

	case ^ast.Unary_Expr:
		collect_tokens_from_expr(tokens, e.expr, snap, form_scope)

	case ^ast.Paren_Expr:
		collect_tokens_from_expr(tokens, e.expr, snap, form_scope)

	case ^ast.Selector_Expr:
		collect_tokens_from_expr(tokens, e.expr, snap, form_scope)
		if e.field != nil {
			append(
				tokens,
				SemanticToken {
					offset = e.field.range.start,
					length = e.field.range.end - e.field.range.start,
					type = .Property,
					modifiers = 0,
				},
			)
		}

	case ^ast.Index_Expr:
		collect_tokens_from_expr(tokens, e.expr, snap, form_scope)
		collect_tokens_from_expr(tokens, e.index, snap, form_scope)

	case ^ast.Call_Expr:
		if call_ident, ok := e.expr.derived_expr.(^ast.Ident); ok {
			append(
				tokens,
				SemanticToken {
					offset = call_ident.range.start,
					length = call_ident.range.end - call_ident.range.start,
					type = .Function,
					modifiers = 0,
				},
			)
		} else {
			collect_tokens_from_expr(tokens, e.expr, snap, form_scope)
		}
		for arg in e.args {
			collect_tokens_from_expr(tokens, arg, snap, form_scope)
		}

	case ^ast.New_Expr:
		// For NEW expressions, highlight the type if specified
		if e.type_expr != nil {
			collect_tokens_from_type_expr(tokens, e.type_expr)
		}
		// Collect tokens from arguments
		for arg in e.args {
			collect_tokens_from_expr(tokens, arg, snap, form_scope)
		}

	case ^ast.Named_Arg:
		// Highlight the parameter name as a parameter
		if e.name != nil {
			append(
				tokens,
				SemanticToken {
					offset = e.name.range.start,
					length = e.name.range.end - e.name.range.start,
					type = .Parameter,
					modifiers = 0,
				},
			)
		}
		// Collect tokens from the value expression
		if e.value != nil {
			collect_tokens_from_expr(tokens, e.value, snap, form_scope)
		}

	case ^ast.Predicate_Expr:
		collect_tokens_from_expr(tokens, e.expr, snap, form_scope)

	case ^ast.String_Template_Expr:
		// Highlight the entire string template as a string
		append(
			tokens,
			SemanticToken {
				offset = e.range.start,
				length = e.range.end - e.range.start,
				type = .String,
				modifiers = 0,
			},
		)
		// Also collect tokens from embedded expressions
		for part in e.parts {
			if part.is_expr && part.expr != nil {
				collect_tokens_from_expr(tokens, part.expr, snap, form_scope)
			}
		}

	case ^ast.For_Expr:
		// Highlight the loop variable as a variable
		if e.var_name != nil {
			append(
				tokens,
				SemanticToken {
					offset = e.var_name.range.start,
					length = e.var_name.range.end - e.var_name.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		// Collect tokens from the internal table expression
		if e.itab != nil {
			collect_tokens_from_expr(tokens, e.itab, snap, form_scope)
		}
		// Collect tokens from the WHERE condition
		if e.where_cond != nil {
			collect_tokens_from_expr(tokens, e.where_cond, snap, form_scope)
		}
		// Collect tokens from result arguments (new field)
		for arg in e.result_args {
			collect_tokens_from_expr(tokens, arg, snap, form_scope)
		}
		// Also check legacy result_expr for backward compatibility
		if e.result_expr != nil && len(e.result_args) == 0 {
			collect_tokens_from_expr(tokens, e.result_expr, snap, form_scope)
		}

	case ^ast.Constructor_Expr:
		// Collect tokens from the type expression if present
		if e.type_expr != nil {
			collect_tokens_from_type_expr(tokens, e.type_expr)
		}
		// Collect tokens from all arguments
		for arg in e.args {
			collect_tokens_from_expr(tokens, arg, snap, form_scope)
		}
	}
}

collect_tokens_from_type_expr :: proc(tokens: ^[dynamic]SemanticToken, expr: ^ast.Expr) {
	if expr == nil {
		return
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		append(
			tokens,
			SemanticToken {
				offset = e.range.start,
				length = e.range.end - e.range.start,
				type = .Type,
				modifiers = 0,
			},
		)

	case ^ast.Table_Type:
		collect_tokens_from_type_expr(tokens, e.elem)
		// Also collect tokens from key components
		if e.primary_key != nil {
			for comp in e.primary_key.components {
				append(
					tokens,
					SemanticToken {
						offset = comp.range.start,
						length = comp.range.end - comp.range.start,
						type = .Property,
						modifiers = 0,
					},
				)
			}
		}

	case ^ast.Ref_Type:
		collect_tokens_from_type_expr(tokens, e.target)

	case ^ast.Line_Type:
		collect_tokens_from_type_expr(tokens, e.table)

	case ^ast.Selector_Expr:
		collect_tokens_from_expr(tokens, e.expr, nil, nil)
		if e.field != nil {
			append(
				tokens,
				SemanticToken {
					offset = e.field.range.start,
					length = e.field.range.end - e.field.range.start,
					type = .Type,
					modifiers = 0,
				},
			)
		}
	}
}

symbol_to_token_type :: proc(sym: symbols.Symbol) -> (SemanticTokenType, u32) {
	modifiers: u32 = 0

	#partial switch sym.kind {
	case .Variable:
		return .Variable, modifiers
	case .Constant:
		modifiers |= 1 << u32(SemanticTokenModifier.Readonly)
		return .Variable, modifiers
	case .Parameter, .FormParameter:
		return .Parameter, modifiers
	case .Field:
		return .Property, modifiers
	case .Method:
		return .Method, modifiers
	case .Class:
		return .Class, modifiers
	case .Interface:
		return .Interface, modifiers
	case .Form:
		return .Function, modifiers
	case .TypeDef:
		return .Type, modifiers
	case .Report, .Include:
		return .Namespace, modifiers
	case .Event:
		return .Event, modifiers
	case .Module:
		return .Function, modifiers
	case .FieldSymbol:
		return .Variable, modifiers
	case .Control:
		return .Variable, modifiers
	}

	return .Variable, modifiers
}

sort_tokens :: proc(tokens: ^[dynamic]SemanticToken) {
	for i := 1; i < len(tokens); i += 1 {
		key := tokens[i]
		j := i - 1
		for j >= 0 && tokens[j].offset > key.offset {
			tokens[j + 1] = tokens[j]
			j -= 1
		}
		tokens[j + 1] = key
	}
}

encode_semantic_tokens :: proc(text: string, tokens: []SemanticToken) -> [dynamic]u32 {
	encoded := make([dynamic]u32, context.temp_allocator)

	prev_line := 0
	prev_char := 0

	for token in tokens {
		pos := offset_to_position(text, token.offset)
		line := pos.line
		char := pos.character

		delta_line := line - prev_line
		delta_char: int
		if delta_line == 0 {
			delta_char = char - prev_char
		} else {
			delta_char = char
		}

		append(&encoded, u32(delta_line))
		append(&encoded, u32(delta_char))
		append(&encoded, u32(token.length))
		append(&encoded, u32(token.type))
		append(&encoded, token.modifiers)

		prev_line = line
		prev_char = char
	}

	return encoded
}
