package lang_parser

import "../ast"
import "../lexer"

parse_class_decl :: proc(p: ^Parser) -> ^ast.Decl {
	class_tok := expect_token(p, .Ident)
	ident_tok := expect_token(p, .Ident)

	if check_keyword(p, "DEFINITION") {
		return parse_class_def_decl(p, class_tok, ident_tok)
	} else if check_keyword(p, "IMPLEMENTATION") {
		return parse_class_impl_decl(p, class_tok, ident_tok)
	}

	error(p, p.curr_tok.range, "expected DEFINITION or IMPLEMENTATION after class name")
	end_tok := skip_to_new_line(p)
	bad_decl := ast.new(ast.Bad_Decl, class_tok, end_tok)
	return bad_decl
}

parse_class_def_decl :: proc(
	p: ^Parser,
	class_tok: lexer.Token,
	ident_tok: lexer.Token,
) -> ^ast.Decl {
	expect_keyword_token(p, "DEFINITION")

	class_decl := ast.new(ast.Class_Def_Decl, class_tok.range)
	class_decl.ident = ast.new_ident(ident_tok)
	class_decl.sections = make([dynamic]^ast.Class_Section)

	if check_keyword(p, "FOR") {
		advance_token(p)
		if check_keyword(p, "TESTING") {
			advance_token(p)
			class_decl.flags += {.Testing}

			if check_keyword(p, "DURATION") {
				advance_token(p)
				keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
				switch keyword {
				case "SHORT":
					advance_token(p)
					class_decl.duration = .Short
				case "MEDIUM":
					advance_token(p)
					class_decl.duration = .Medium
				case "LONG":
					advance_token(p)
					class_decl.duration = .Long
				case:
					error(p, p.curr_tok.range, "expected DURATION token (SHORT|MEDIUM|LONG)")
				}
			}

			if check_keyword(p, "RISK") {
				advance_token(p)
				if check_keyword(p, "LEVEL") {
					advance_token(p)
					keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
					switch keyword {
					case "CRITICAL":
						advance_token(p)
						class_decl.risk_level = .Critical
					case "DANGEROUS":
						advance_token(p)
						class_decl.risk_level = .Dangerous
					case "HARMLESS":
						advance_token(p)
						class_decl.risk_level = .Harmless
					case:
						error(
							p,
							p.curr_tok.range,
							"expected RISK LEVEL token (CRITICAL|DANGEROUS|HARMLESS)",
						)
					}
				} else {
					error(
						p,
						p.curr_tok.range,
						"expected LEVEL after RISK in testing class definition",
					)
				}
			}
		} else {
			error(p, p.curr_tok.range, "expected TESTING after FOR in class definition")
		}
	}

	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
			keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
			match_not_found := false
			switch keyword {
			case "ABSTRACT":
				advance_token(p)
				class_decl.flags += {.Abstract}
			case "FINAL":
				advance_token(p)
				class_decl.flags += {.Final}
			case "INHERITING":
				advance_token(p)
				expect_keyword_token(p, "FROM")
				class_decl.inheriting_from = parse_expr(p)
			case "CREATE":
				advance_token(p)
				keyword = to_upper(p.keyword_buffer[:], p.curr_tok.lit)
				switch keyword {
				case "PUBLIC":
					advance_token(p)
				case "PROTECTED":
					advance_token(p)
					class_decl.create_kind = .Protected
				case "PRIVATE":
					advance_token(p)
					class_decl.create_kind = .Private
				case:
					error(
						p,
						p.curr_tok.range,
						"expected CREATE visibility token (PUBLIC|PROTECTED|PRIVATE)",
					)
				}
			case:
				match_not_found = true
			}
			if match_not_found {
				break
			}
		} else {
			break
		}
	}

	expect_token(p, .Period)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDCLASS") {
			break
		}

		if section := parse_class_section(p); section != nil {
			append(&class_decl.sections, section)
		} else {
			skip_to_new_line(p)
		}
	}

	endclass_tok := expect_keyword_token(p, "ENDCLASS")
	period_tok := expect_token(p, .Period)
	class_decl.range.end = period_tok.range.end
	_ = endclass_tok

	return class_decl
}

parse_class_impl_decl :: proc(
	p: ^Parser,
	class_tok: lexer.Token,
	ident_tok: lexer.Token,
) -> ^ast.Decl {
	expect_keyword_token(p, "IMPLEMENTATION")
	expect_token(p, .Period)

	class_impl := ast.new(ast.Class_Impl_Decl, class_tok.range)
	class_impl.ident = ast.new_ident(ident_tok)
	class_impl.methods = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDCLASS") {
			break
		}

		if check_keyword(p, "METHOD") {
			method_impl := parse_method_impl(p)
			if method_impl != nil {
				append(&class_impl.methods, method_impl)
			}
		} else {
			skip_to_new_line(p)
		}
	}

	endclass_tok := expect_keyword_token(p, "ENDCLASS")
	period_tok := expect_token(p, .Period)
	class_impl.range.end = period_tok.range.end
	_ = endclass_tok

	return class_impl
}

parse_class_section :: proc(p: ^Parser) -> ^ast.Class_Section {
	access: ast.Access_Modifier

	if check_keyword(p, "PUBLIC") {
		advance_token(p)
		access = .Public
	} else if check_keyword(p, "PROTECTED") {
		advance_token(p)
		access = .Protected
	} else if check_keyword(p, "PRIVATE") {
		advance_token(p)
		access = .Private
	} else {
		return nil
	}

	section_tok := expect_keyword_token(p, "SECTION")
	expect_token(p, .Period)

	section := ast.new(ast.Class_Section, section_tok.range)
	section.access = access
	section.types = make([dynamic]^ast.Stmt)
	section.data = make([dynamic]^ast.Stmt)
	section.methods = make([dynamic]^ast.Stmt)
	section.interfaces = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "PUBLIC") ||
		   check_keyword(p, "PROTECTED") ||
		   check_keyword(p, "PRIVATE") ||
		   check_keyword(p, "ENDCLASS") {
			break
		}

		if check_keyword(p, "TYPES") {
			types_decl := parse_types_decl(p)
			if types_decl != nil {
				append(&section.types, types_decl)
			}
		} else if check_keyword(p, "DATA") {
			data_decl := parse_class_data_decl(p, false)
			if data_decl != nil {
				append(&section.data, data_decl)
			}
		} else if check_class_keyword(p, "CLASS", "DATA") {
			data_decl := parse_class_data_decl(p, true)
			if data_decl != nil {
				append(&section.data, data_decl)
			}
		} else if check_keyword(p, "METHODS") {
			method_decl := parse_method_decl(p, false)
			if method_decl != nil {
				append(&section.methods, method_decl)
			}
		} else if check_class_keyword(p, "CLASS", "METHODS") {
			method_decl := parse_method_decl(p, true)
			if method_decl != nil {
				append(&section.methods, method_decl)
			}
		} else if check_keyword(p, "INTERFACES") {
			ifaces_decl := parse_interfaces_decl(p)
			if ifaces_decl != nil {
				append(&section.interfaces, ifaces_decl)
			}
		} else {
			skip_to_new_line(p)
		}
	}

	return section
}

parse_class_data_decl :: proc(p: ^Parser, is_class: bool) -> ^ast.Stmt {
	data_tok: lexer.Token
	if is_class {
		data_tok = p.prev_tok
	} else {
		data_tok = advance_token(p)
	}

	if allow_token(p, .Colon) {
		return parse_class_data_chain_decl(p, data_tok, is_class)
	}

	return parse_class_data_single_decl(p, data_tok, is_class)
}

parse_class_data_single_decl :: proc(
	p: ^Parser,
	data_tok: lexer.Token,
	is_class: bool,
) -> ^ast.Stmt {
	ident_tok := expect_token(p, .Ident)
	expect_keyword_token(p, "TYPE")
	type_expr := parse_type_expr(p)

	is_read_only := false
	if check_keyword(p, "READ-ONLY") {
		advance_token(p)
		is_read_only = true
	}

	period_tok := expect_token(p, .Period)

	attr_decl := ast.new(ast.Attr_Decl, data_tok, period_tok)
	attr_decl.ident = ast.new_ident(ident_tok)
	attr_decl.typed = type_expr
	attr_decl.is_class = is_class
	attr_decl.is_read_only = is_read_only
	attr_decl.derived_stmt = attr_decl
	return attr_decl
}

parse_class_data_chain_decl :: proc(
	p: ^Parser,
	data_tok: lexer.Token,
	is_class: bool,
) -> ^ast.Stmt {
	chain_decl := ast.new(ast.Data_Typed_Chain_Decl, data_tok.range)
	chain_decl.decls = make([dynamic]^ast.Data_Typed_Decl)

	for {
		// Parse identifier, which may be a selector expression (e.g., screen0100-serial)
		ident_expr := parse_data_decl_ident(p)
		expect_keyword_token(p, "TYPE")
		type_expr := parse_type_expr(p)

		decl := ast.new(
			ast.Data_Typed_Decl,
			lexer.TextRange{ident_expr.range.start, p.prev_tok.range.end},
		)
		decl.ident = ident_expr
		decl.typed = type_expr
		append(&chain_decl.decls, decl)

		if allow_token(p, .Comma) {
			continue
		}

		period_tok := expect_token(p, .Period)
		chain_decl.range.end = period_tok.range.end
		break
	}

	return chain_decl
}

parse_method_decl :: proc(p: ^Parser, is_class: bool) -> ^ast.Stmt {
	method_tok: lexer.Token
	if is_class {
		method_tok = p.prev_tok
	} else {
		method_tok = advance_token(p)
	}

	if allow_token(p, .Colon) {
		return parse_method_chain_decl(p, method_tok, is_class)
	}

	method_decl := parse_method_single_decl(p, method_tok, is_class)
	period_tok := expect_token(p, .Period)
	method_decl.range.end = period_tok.range.end
	return method_decl
}

parse_method_single_decl :: proc(
	p: ^Parser,
	method_tok: lexer.Token,
	is_class: bool,
) -> ^ast.Method_Decl {
	ident_tok := expect_token(p, .Ident)

	method_decl := ast.new(ast.Method_Decl, method_tok.range)
	method_decl.ident = ast.new_ident(ident_tok)
	if is_class {
		method_decl.flags += {.Class}
	}
	method_decl.params = make([dynamic]^ast.Method_Param)
	method_decl.raising = make([dynamic]^ast.Expr)

	if check_keyword(p, "FOR") {
		advance_token(p)
		if check_keyword(p, "TESTING") {
			advance_token(p)
			method_decl.flags += {.Testing}
		} else {
			error(p, p.curr_tok.range, "Expected TESTING after FOR")
		}
	} else {
		for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
			if check_keyword(p, "ABSTRACT") {
				advance_token(p)
				method_decl.flags += {.Abstract}
			} else if check_keyword(p, "FINAL") {
				advance_token(p)
				method_decl.flags += {.Final}
			} else if check_keyword(p, "REDEFINITION") {
				advance_token(p)
				method_decl.flags += {.Redefinition}
			} else if check_keyword(p, "IMPORTING") {
				advance_token(p)
				parse_method_params(p, &method_decl.params, .Importing)
			} else if check_keyword(p, "EXPORTING") {
				advance_token(p)
				parse_method_params(p, &method_decl.params, .Exporting)
			} else if check_keyword(p, "CHANGING") {
				advance_token(p)
				parse_method_params(p, &method_decl.params, .Changing)
			} else if check_keyword(p, "RETURNING") {
				advance_token(p)
				parse_method_params(p, &method_decl.params, .Returning)
			} else if check_keyword(p, "RAISING") {
				advance_token(p)
				parse_raising_clause(p, &method_decl.raising)
			} else {
				break
			}
		}
	}

	return method_decl
}

parse_method_chain_decl :: proc(p: ^Parser, method_tok: lexer.Token, is_class: bool) -> ^ast.Stmt {
	chain_decl := ast.new(ast.Method_Chain_Decl, method_tok.range)
	chain_decl.decls = make([dynamic]^ast.Method_Decl)

	for {
		decl := parse_method_single_decl(p, method_tok, is_class)
		append(&chain_decl.decls, decl)

		if allow_token(p, .Comma) {
			continue
		}

		period_tok := expect_token(p, .Period)
		chain_decl.range.end = period_tok.range.end
		break
	}

	return chain_decl
}

parse_method_params :: proc(
	p: ^Parser,
	params: ^[dynamic]^ast.Method_Param,
	kind: ast.Method_Param_Kind,
) {
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if check_keyword(p, "IMPORTING") ||
		   check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "CHANGING") ||
		   check_keyword(p, "RETURNING") ||
		   check_keyword(p, "RAISING") ||
		   check_keyword(p, "ABSTRACT") ||
		   check_keyword(p, "FINAL") ||
		   check_keyword(p, "REDEFINITION") {
			break
		}

		if kind == .Returning && check_keyword(p, "VALUE") {
			advance_token(p)
			expect_token(p, .LParen)
			ident_tok := expect_token(p, .Ident)
			expect_token(p, .RParen)

			param := ast.new(ast.Method_Param, ident_tok.range)
			param.kind = kind
			param.ident = ast.new_ident(ident_tok)

			if check_keyword(p, "TYPE") {
				advance_token(p)
				param.typed = parse_type_expr(p)
			}

			append(params, param)
			continue
		}

		ident_tok := advance_token(p)

		param := ast.new(ast.Method_Param, ident_tok.range)
		param.kind = kind
		param.ident = ast.new_ident(ident_tok)

		if check_keyword(p, "TYPE") {
			advance_token(p)
			param.typed = parse_type_expr(p)
		}

		if check_keyword(p, "OPTIONAL") {
			advance_token(p)
			param.optional = true
		}

		if check_keyword(p, "DEFAULT") {
			advance_token(p)
			param.default = parse_expr(p)
		}

		append(params, param)
	}
}

parse_raising_clause :: proc(p: ^Parser, raising: ^[dynamic]^ast.Expr) {
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if check_keyword(p, "IMPORTING") ||
		   check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "CHANGING") ||
		   check_keyword(p, "RETURNING") {
			break
		}

		exception_name := parse_expr(p)
		append(raising, exception_name)
	}
}

parse_interfaces_decl :: proc(p: ^Parser) -> ^ast.Stmt {
	ifaces_tok := expect_keyword_token(p, "INTERFACES")

	ifaces_decl := ast.new(ast.Interfaces_Decl, ifaces_tok.range)
	ifaces_decl.names = make([dynamic]^ast.Ident)

	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		ident_tok := advance_token(p)
		append(&ifaces_decl.names, ast.new_ident(ident_tok))
	}

	period_tok := expect_token(p, .Period)
	ifaces_decl.range.end = period_tok.range.end
	return ifaces_decl
}

parse_method_impl :: proc(p: ^Parser) -> ^ast.Stmt {
	method_tok := expect_keyword_token(p, "METHOD")

	name_expr := parse_expr(p)

	expect_token(p, .Period)

	method_impl := ast.new(ast.Method_Impl, method_tok.range)
	method_impl.ident = name_expr
	method_impl.body = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDMETHOD") {
			break
		}

		stmt := parse_stmt(p)
		if stmt != nil {
			append(&method_impl.body, stmt)
		}
	}

	endmethod_tok := expect_keyword_token(p, "ENDMETHOD")
	period_tok := expect_token(p, .Period)
	method_impl.range.end = period_tok.range.end

	return method_impl
}

parse_interface_decl :: proc(p: ^Parser) -> ^ast.Decl {
	iface_tok := expect_token(p, .Ident)
	ident_tok := expect_token(p, .Ident)

	expect_token(p, .Period)

	iface_decl := ast.new(ast.Interface_Decl, iface_tok.range)
	iface_decl.ident = ast.new_ident(ident_tok)
	iface_decl.methods = make([dynamic]^ast.Stmt)
	iface_decl.types = make([dynamic]^ast.Stmt)
	iface_decl.data = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDINTERFACE") {
			break
		}

		if check_keyword(p, "METHODS") {
			method_decl := parse_method_decl(p, false)
			if method_decl != nil {
				append(&iface_decl.methods, method_decl)
			}
		} else if check_class_keyword(p, "CLASS", "METHODS") {
			method_decl := parse_method_decl(p, true)
			if method_decl != nil {
				append(&iface_decl.methods, method_decl)
			}
		} else if check_keyword(p, "TYPES") {
			types_decl := parse_types_decl(p)
			if types_decl != nil {
				append(&iface_decl.types, types_decl)
			}
		} else if check_keyword(p, "DATA") {
			data_decl := parse_class_data_decl(p, false)
			if data_decl != nil {
				append(&iface_decl.data, data_decl)
			}
		} else {
			skip_to_new_line(p)
		}
	}

	endiface_tok := expect_keyword_token(p, "ENDINTERFACE")
	period_tok := expect_token(p, .Period)
	iface_decl.range.end = period_tok.range.end
	return iface_decl
}