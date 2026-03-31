package lsp

import "../cache"
import "core:encoding/json"
import "core:fmt"

import "../lang/ast"
import "../lang/lexer"
import "../lang/symbols"
import "core:strings"

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

	symbol_table := cache.get_effective_symbol_table(srv.storage, semantic_params.textDocument.uri)
	effective_snap := snap^
	if symbol_table != nil {
		effective_snap.symbol_table = symbol_table
	}

	tokens := collect_semantic_tokens(&effective_snap)
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
			collect_tokens_from_expr(tokens, s.ident, snap, nil)
		}
		if s.length != nil {
			collect_tokens_from_expr(tokens, s.length, snap, nil)
		}
		if s.typed != nil {
			collect_tokens_from_type_expr(tokens, s.typed)
		}

	case ^ast.Data_Typed_Chain_Decl:
		for part in s.parts {
			#partial switch d in part.derived_stmt {
			case ^ast.Data_Typed_Decl:
				if d.ident != nil {
					collect_tokens_from_expr(tokens, d.ident, snap, nil)
				}
				if d.length != nil {
					collect_tokens_from_expr(tokens, d.length, snap, nil)
				}
				if d.typed != nil {
					collect_tokens_from_type_expr(tokens, d.typed)
				}
			case ^ast.Data_Struct_Decl:
				if d.ident != nil {
					append(
						tokens,
						SemanticToken {
							offset = d.ident.range.start,
							length = d.ident.range.end - d.ident.range.start,
							type = .Variable,
							modifiers = 1 << u32(SemanticTokenModifier.Declaration),
						},
					)
				}
				collect_tokens_from_data_struct_components(tokens, d.components[:], snap)
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
		for part in s.parts {
			#partial switch d in part.derived_stmt {
			case ^ast.Types_Decl:
				if d.ident != nil {
					append(
						tokens,
						SemanticToken {
							offset = d.ident.range.start,
							length = d.ident.range.end - d.ident.range.start,
							type = .Type,
							modifiers = 1 << u32(SemanticTokenModifier.Declaration),
						},
					)
				}
				if d.typed != nil {
					collect_tokens_from_type_expr(tokens, d.typed)
				}
			case ^ast.Types_Struct_Decl:
				if d.ident != nil {
					append(
						tokens,
						SemanticToken {
							offset = d.ident.range.start,
							length = d.ident.range.end - d.ident.range.start,
							type = .Type,
							modifiers = 1 << u32(SemanticTokenModifier.Declaration),
						},
					)
				}
				collect_tokens_from_struct_components(tokens, d.components[:], snap)
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
		if s.length != nil {
			collect_tokens_from_expr(tokens, s.length, snap, nil)
		}
		if s.typed != nil {
			collect_tokens_from_type_expr(tokens, s.typed)
		}
		if s.value != nil {
			collect_tokens_from_expr(tokens, s.value, snap, nil)
		}

	case ^ast.Const_Chain_Decl:
		for part in s.parts {
			#partial switch decl in part.derived_stmt {
			case ^ast.Const_Decl:
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
				if decl.length != nil {
					collect_tokens_from_expr(tokens, decl.length, snap, nil)
				}
				if decl.typed != nil {
					collect_tokens_from_type_expr(tokens, decl.typed)
				}
				if decl.value != nil {
					collect_tokens_from_expr(tokens, decl.value, snap, nil)
				}
			case ^ast.Const_Struct_Decl:
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
				collect_tokens_from_const_struct_components(tokens, decl.components[:], snap)
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

	case ^ast.Move_Corresponding_Stmt:
		if s.source != nil {
			collect_tokens_from_expr(tokens, s.source, snap, nil)
		}
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}

	case ^ast.Assign_Field_Symbol_Stmt:
		if s.component != nil {
			collect_tokens_from_expr(tokens, s.component, snap, nil)
		}
		if s.structure != nil {
			collect_tokens_from_expr(tokens, s.structure, snap, nil)
		}
		if s.source != nil {
			collect_tokens_from_expr(tokens, s.source, snap, nil)
		}
		if s.offset != nil {
			collect_tokens_from_expr(tokens, s.offset, snap, nil)
		}
		if s.length != nil {
			collect_tokens_from_expr(tokens, s.length, snap, nil)
		}
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}

	case ^ast.Expr_Stmt:
		collect_tokens_from_expr(tokens, s.expr, snap, nil)

	case ^ast.Macro_Call_Stmt:
		collect_tokens_from_expr(tokens, s.name, snap, nil)
		for arg in s.args {
			collect_tokens_from_expr(tokens, arg, snap, nil)
		}

	case ^ast.Try_Stmt:
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}
		for branch in s.catch_branches {
			for class_ref in branch.class_refs {
				collect_tokens_from_expr(tokens, class_ref, snap, nil)
			}
			if branch.into_target != nil {
				collect_tokens_from_expr(tokens, branch.into_target, snap, nil)
			}
			for branch_stmt in branch.body {
				collect_tokens_from_stmt(tokens, branch_stmt, snap)
			}
		}
		if s.cleanup_branch != nil {
			if s.cleanup_branch.into_target != nil {
				collect_tokens_from_expr(tokens, s.cleanup_branch.into_target, snap, nil)
			}
			for cleanup_stmt in s.cleanup_branch.body {
				collect_tokens_from_stmt(tokens, cleanup_stmt, snap)
			}
		}

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

	case ^ast.Do_Stmt:
		if s.times != nil {
			collect_tokens_from_expr(tokens, s.times, snap, nil)
		}
		for body_stmt in s.body {
			collect_tokens_from_stmt(tokens, body_stmt, snap)
		}

	case ^ast.Clear_Stmt:
		for expr in s.exprs {
			collect_tokens_from_expr(tokens, expr, snap, nil)
		}
		if s.with_expr != nil {
			collect_tokens_from_expr(tokens, s.with_expr, snap, nil)
		}

	case ^ast.Free_Stmt:
		for expr in s.exprs {
			collect_tokens_from_expr(tokens, expr, snap, nil)
		}

	case ^ast.Unassign_Stmt:
		for target in s.targets {
			if target != nil {
				collect_tokens_from_expr(tokens, target, snap, nil)
			}
		}

	case ^ast.Write_Stmt:
		for &op in s.operands {
			if op.format_len != nil {
				collect_tokens_from_expr(tokens, op.format_len, snap, nil)
			}
			if op.data != nil {
				collect_tokens_from_expr(tokens, op.data, snap, nil)
			}
			if op.to_target != nil {
				collect_tokens_from_expr(tokens, op.to_target, snap, nil)
			}
			if op.decimals != nil {
				collect_tokens_from_expr(tokens, op.decimals, snap, nil)
			}
			if op.time_zone != nil {
				collect_tokens_from_expr(tokens, op.time_zone, snap, nil)
			}
		}

	case ^ast.Block_Stmt:
		for block_stmt in s.stmts {
			collect_tokens_from_stmt(tokens, block_stmt, snap)
		}

	case ^ast.Return_Stmt:
		for result in s.results {
			collect_tokens_from_expr(tokens, result, snap, nil)
		}

	case ^ast.Get_Time_Stamp_Stmt:
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}

	case ^ast.Convert_Date_Time_To_Time_Stamp_Stmt:
		if s.date != nil {
			collect_tokens_from_expr(tokens, s.date, snap, nil)
		}
		if s.time != nil {
			collect_tokens_from_expr(tokens, s.time, snap, nil)
		}
		if s.stamp != nil {
			collect_tokens_from_expr(tokens, s.stamp, snap, nil)
		}
		if s.time_zone != nil {
			collect_tokens_from_expr(tokens, s.time_zone, snap, nil)
		}

	case ^ast.Convert_Time_Stamp_To_Date_Time_Stmt:
		if s.stamp != nil {
			collect_tokens_from_expr(tokens, s.stamp, snap, nil)
		}
		if s.time_zone != nil {
			collect_tokens_from_expr(tokens, s.time_zone, snap, nil)
		}
		if s.date != nil {
			collect_tokens_from_expr(tokens, s.date, snap, nil)
		}
		if s.time != nil {
			collect_tokens_from_expr(tokens, s.time, snap, nil)
		}

	case ^ast.Get_Badi_Stmt:
		if s.badi_ref != nil {
			collect_tokens_from_expr(tokens, s.badi_ref, snap, nil)
		}
		for f in s.filters {
			collect_tokens_from_expr(tokens, f, snap, nil)
		}

	case ^ast.Set_Handler_Stmt:
		for h in s.handlers {
			collect_tokens_from_expr(tokens, h, snap, nil)
		}
		if s.for_ref != nil {
			collect_tokens_from_expr(tokens, s.for_ref, snap, nil)
		}

	case ^ast.Set_Bit_Stmt:
		if s.bit_position != nil {
			collect_tokens_from_expr(tokens, s.bit_position, snap, nil)
		}
		if s.of_target != nil {
			collect_tokens_from_expr(tokens, s.of_target, snap, nil)
		}
		if s.to_value != nil {
			collect_tokens_from_expr(tokens, s.to_value, snap, nil)
		}

	case ^ast.Get_Bit_Stmt:
		if s.bit_position != nil {
			collect_tokens_from_expr(tokens, s.bit_position, snap, nil)
		}
		if s.of_target != nil {
			collect_tokens_from_expr(tokens, s.of_target, snap, nil)
		}
		if s.into_target != nil {
			collect_tokens_from_expr(tokens, s.into_target, snap, nil)
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
		if s.behavior_of != nil {
			collect_tokens_from_type_expr(tokens, s.behavior_of)
		}
		for friend in s.friends {
			collect_tokens_from_type_expr(tokens, friend)
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

	case ^ast.Method_Chain_Decl:
		for decl in s.decls {
			collect_tokens_from_stmt(tokens, decl, snap)
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

	case ^ast.Call_Transaction_Stmt:
		if s.transaction != nil {
			collect_tokens_from_expr(tokens, s.transaction, snap, nil)
		}
		if s.bdc_tab != nil {
			collect_tokens_from_expr(tokens, s.bdc_tab, snap, nil)
		}
		if s.mode != nil {
			collect_tokens_from_expr(tokens, s.mode, snap, nil)
		}

	case ^ast.Call_Transformation_Stmt:
		if s.transformation != nil {
			collect_tokens_from_expr(tokens, s.transformation, snap, nil)
		}
		if s.options != nil {
			collect_tokens_from_expr(tokens, s.options, snap, nil)
		}
		if s.source != nil {
			collect_tokens_from_expr(tokens, s.source, snap, nil)
		}
		if s.result_stream != nil {
			collect_tokens_from_expr(tokens, s.result_stream, snap, nil)
		}
		for root in s.result_roots {
			collect_tokens_from_expr(tokens, &root.node, snap, nil)
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
		if s.id_class != nil {
			collect_tokens_from_expr(tokens, s.id_class, snap, nil)
		}
		if s.msg_type != nil {
			collect_tokens_from_expr(tokens, s.msg_type, snap, nil)
		}
		if s.msg_number != nil {
			collect_tokens_from_expr(tokens, s.msg_number, snap, nil)
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
		if s.index_expr != nil {
			collect_tokens_from_expr(tokens, s.index_expr, snap, nil)
		}
		if s.assigning_target != nil {
			collect_tokens_from_expr(tokens, s.assigning_target, snap, nil)
		}

	case ^ast.Modify_From_Stmt:
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
		if s.lines_from != nil {
			collect_tokens_from_expr(tokens, s.lines_from, snap, nil)
		}
		if s.lines_to != nil {
			collect_tokens_from_expr(tokens, s.lines_to, snap, nil)
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
		if s.from_source != nil {
			collect_tokens_from_expr(tokens, s.from_source, snap, nil)
		}
		if s.where_cond != nil {
			collect_tokens_from_expr(tokens, s.where_cond, snap, nil)
		}
		if s.index_expr != nil {
			collect_tokens_from_expr(tokens, s.index_expr, snap, nil)
		}

	case ^ast.Split_Stmt:
		if s.source != nil {
			collect_tokens_from_expr(tokens, s.source, snap, nil)
		}
		if s.separator != nil {
			collect_tokens_from_expr(tokens, s.separator, snap, nil)
		}
		for target in s.targets {
			if target != nil {
				collect_tokens_from_expr(tokens, target, snap, nil)
			}
		}
		if s.table_target != nil {
			collect_tokens_from_expr(tokens, s.table_target, snap, nil)
		}

	case ^ast.Concatenate_Stmt:
		for source in s.sources {
			if source != nil {
				collect_tokens_from_expr(tokens, source, snap, nil)
			}
		}
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}
		if s.separator != nil {
			collect_tokens_from_expr(tokens, s.separator, snap, nil)
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

	case ^ast.Field_Symbol_Chain_Decl:
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

	case ^ast.Translate_Stmt:
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}
		if s.using_pattern != nil {
			collect_tokens_from_expr(tokens, s.using_pattern, snap, nil)
		}

	case ^ast.Replace_Stmt:
		if s.pattern != nil {
			collect_tokens_from_expr(tokens, s.pattern, snap, nil)
		}
		if s.subject != nil {
			collect_tokens_from_expr(tokens, s.subject, snap, nil)
		}
		if s.replacement != nil {
			collect_tokens_from_expr(tokens, s.replacement, snap, nil)
		}

	case ^ast.Raise_Exception_Stmt:
		if s.type_ref != nil {
			collect_tokens_from_expr(tokens, s.type_ref, snap, nil)
		}
		if s.oref != nil {
			collect_tokens_from_expr(tokens, s.oref, snap, nil)
		}
		if s.legacy_exception != nil {
			collect_tokens_from_expr(tokens, s.legacy_exception, snap, nil)
		}
		for arg in s.exporting {
			collect_tokens_from_expr(tokens, &arg.node, snap, nil)
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

	case ^ast.Loop_At_Control_Stmt:
		if s.field != nil {
			collect_tokens_from_expr(tokens, s.field, snap, nil)
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
			if s.key.key_name != nil {
				append(
					tokens,
					SemanticToken {
						offset = s.key.key_name.range.start,
						length = s.key.key_name.range.end - s.key.key_name.range.start,
						type = .Property,
						modifiers = 0,
					},
				)
			}
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

	case ^ast.Read_Report_Stmt:
		if s.prog != nil {
			collect_tokens_from_expr(tokens, s.prog, snap, nil)
		}
		if s.itab != nil {
			collect_tokens_from_expr(tokens, s.itab, snap, nil)
		}

	case ^ast.Describe_Table_Stmt:
		if s.table != nil {
			collect_tokens_from_expr(tokens, s.table, snap, nil)
		}
		if s.lines_target != nil {
			collect_tokens_from_expr(tokens, s.lines_target, snap, nil)
		}

	case ^ast.Check_Stmt:
		if s.cond != nil {
			collect_tokens_from_expr(tokens, s.cond, snap, nil)
		}

	case ^ast.Assert_Stmt:
		if s.cond != nil {
			collect_tokens_from_expr(tokens, s.cond, snap, nil)
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
		if s.starting_new_task != nil {
			collect_tokens_from_expr(tokens, s.starting_new_task, snap, nil)
		}
		// Collect tokens from all parameter sections
		collect_tokens_from_call_function_params(tokens, s.exporting[:], snap)
		collect_tokens_from_call_function_params(tokens, s.importing[:], snap)
		collect_tokens_from_call_function_params(tokens, s.tables[:], snap)
		collect_tokens_from_call_function_params(tokens, s.changing[:], snap)
		collect_tokens_from_call_function_params(tokens, s.exceptions[:], snap)

	case ^ast.Call_Badi_Stmt:
		if s.badi_target != nil {
			collect_tokens_from_expr(tokens, s.badi_target, snap, nil)
		}
		collect_tokens_from_call_function_params(tokens, s.exporting[:], snap)
		collect_tokens_from_call_function_params(tokens, s.importing[:], snap)
		collect_tokens_from_call_function_params(tokens, s.changing[:], snap)
		collect_tokens_from_call_function_params(tokens, s.receiving[:], snap)
		collect_tokens_from_call_function_params(tokens, s.exceptions[:], snap)

	case ^ast.Call_System_Stmt:
		if s.module != nil {
			collect_tokens_from_expr(tokens, s.module, snap, nil)
		}
		for param in s.params {
			if param.id_name != nil {
				collect_tokens_from_expr(tokens, param.id_name, snap, nil)
			}
			if param.field != nil {
				collect_tokens_from_expr(tokens, param.field, snap, nil)
			}
		}

	case ^ast.Create_Object_Stmt:
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}
		if s.type_ref != nil {
			collect_tokens_from_expr(tokens, s.type_ref, snap, nil)
		}
		if s.area_handle != nil {
			collect_tokens_from_expr(tokens, s.area_handle, snap, nil)
		}
		for arg in s.exporting {
			collect_tokens_from_expr(tokens, &arg.node, snap, nil)
		}
		for arg in s.exceptions {
			collect_tokens_from_expr(tokens, &arg.node, snap, nil)
		}

	case ^ast.Create_Data_Stmt:
		if s.target != nil {
			collect_tokens_from_expr(tokens, s.target, snap, nil)
		}
		if s.type_ref != nil {
			collect_tokens_from_type_expr(tokens, s.type_ref)
		}
		if s.like_ref != nil {
			collect_tokens_from_expr(tokens, s.like_ref, snap, nil)
		}
		if s.type_handle != nil {
			collect_tokens_from_expr(tokens, s.type_handle, snap, nil)
		}

	case ^ast.Open_Cursor_Stmt:
		if s.cursor != nil {
			append(
				tokens,
				SemanticToken {
					offset = s.cursor.range.start,
					length = s.cursor.range.end - s.cursor.range.start,
					type = .Variable,
					modifiers = 1 << u32(SemanticTokenModifier.Declaration),
				},
			)
		}
		if s.select_stmt != nil {
			collect_tokens_from_stmt(tokens, s.select_stmt, snap)
		}

	case ^ast.Fetch_Cursor_Stmt:
		if s.cursor != nil {
			collect_tokens_from_expr(tokens, &s.cursor.node, snap, nil)
		}
		if s.into_target != nil {
			collect_tokens_from_expr(tokens, s.into_target, snap, nil)
		}
		if s.package_size != nil {
			collect_tokens_from_expr(tokens, s.package_size, snap, nil)
		}

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
		// Highlight parameter name; EXCEPTIONS OTHERS is a language keyword (catch-all → sy-subrc)
		if param.name != nil {
			name_tok_type := SemanticTokenType.Parameter
			if param.kind == .Exceptions && param.is_others {
				name_tok_type = .Keyword
			}
			append(
				tokens,
				SemanticToken {
					offset = param.name.range.start,
					length = param.name.range.end - param.name.range.start,
					type = name_tok_type,
					modifiers = 0,
				},
			)
		}
		// Collect tokens from the value expression
		if param.value != nil {
			collect_tokens_from_expr(tokens, param.value, snap, nil)
		}
		if param.message_value != nil {
			collect_tokens_from_expr(tokens, param.message_value, snap, nil)
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

		case ^ast.Types_Include_Type_Decl:
			if c.included != nil {
				collect_tokens_from_type_expr(tokens, c.included)
			}
			if c.as_name != nil {
				append(
					tokens,
					SemanticToken {
						offset = c.as_name.range.start,
						length = c.as_name.range.end - c.as_name.range.start,
						type = .Property,
						modifiers = 1 << u32(SemanticTokenModifier.Declaration),
					},
				)
			}
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
			if c.length != nil {
				collect_tokens_from_expr(tokens, c.length, snap, nil)
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
				collect_tokens_from_expr(tokens, c.ident, snap, nil)
			}
			if c.length != nil {
				collect_tokens_from_expr(tokens, c.length, snap, nil)
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
	if param.likes != nil {
		collect_tokens_from_type_expr(tokens, param.likes)
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
		case .Ident:
			// Open SQL: NULL after IS [NOT] is a keyword/literal, not a data object.
			if strings.to_upper(e.tok.lit, context.temp_allocator) != "NULL" {
				return
			}
			token_type = .Keyword
		case:
			return // Skip other literals (e.g. *)
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
			if id, ok := e.field.derived_expr.(^ast.Ident); ok {
				append(
					tokens,
					SemanticToken {
						offset = id.range.start,
						length = id.range.end - id.range.start,
						type = .Property,
						modifiers = 0,
					},
				)
			} else {
				collect_tokens_from_expr(tokens, e.field, snap, form_scope)
			}
		}

	case ^ast.Index_Expr:
		collect_tokens_from_expr(tokens, e.expr, snap, form_scope)
		if e.table_key_name != nil {
			collect_tokens_from_expr(tokens, cast(^ast.Expr)e.table_key_name, snap, form_scope)
		}
		collect_tokens_from_expr(tokens, e.index, snap, form_scope)

	case ^ast.Substring_Expr:
		collect_tokens_from_expr(tokens, e.expr, snap, form_scope)
		if e.offset != nil {
			collect_tokens_from_expr(tokens, e.offset, snap, form_scope)
		}
		if e.length != nil {
			collect_tokens_from_expr(tokens, e.length, snap, form_scope)
		}

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

	case ^ast.Value_Row_Expr:
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

	case ^ast.Range_Type:
		collect_tokens_from_type_expr(tokens, e.elem)

	case ^ast.Selector_Expr:
		collect_tokens_from_expr(tokens, e.expr, nil, nil)
		if e.field != nil {
			if id, ok := e.field.derived_expr.(^ast.Ident); ok {
				append(
					tokens,
					SemanticToken {
						offset = id.range.start,
						length = id.range.end - id.range.start,
						type = .Type,
						modifiers = 0,
					},
				)
			} else {
				collect_tokens_from_type_expr(tokens, e.field)
			}
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
