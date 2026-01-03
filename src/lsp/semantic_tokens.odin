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
