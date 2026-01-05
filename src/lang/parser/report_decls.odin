package lang_parser

import "../ast"
import "../lexer"

parse_report_decl :: proc(p: ^Parser) -> ^ast.Decl {
	report_tok := expect_keyword_token(p, "REPORT")
	name_tok := expect_token(p, .Ident)
	period_tok := expect_token(p, .Period)

	report_decl := ast.new(ast.Report_Decl, report_tok, period_tok)
	report_decl.name = ast.new_ident(name_tok)
	return report_decl
}

parse_include_decl :: proc(p: ^Parser) -> ^ast.Decl {
	include_tok := expect_keyword_token(p, "INCLUDE")
	name_tok := expect_token(p, .Ident)
	period_tok := expect_token(p, .Period)

	include_decl := ast.new(ast.Include_Decl, include_tok, period_tok)
	include_decl.name = ast.new_ident(name_tok)
	return include_decl
}

parse_event_block :: proc(p: ^Parser, keyword: string) -> ^ast.Stmt {
	// Note: For compound keywords like START-OF-SELECTION, the caller (check_compound_keyword)
	// has already advanced past the entire keyword. For simple keywords like INITIALIZATION,
	// we still need to advance.
	start_tok := p.prev_tok // Use prev_tok since compound keyword was already consumed
	event_kind: ast.Event_Kind

	switch keyword {
	case "START-OF-SELECTION":
		// Already consumed by check_compound_keyword
		event_kind = .StartOfSelection
	case "END-OF-SELECTION":
		// Already consumed by check_compound_keyword
		event_kind = .EndOfSelection
	case "INITIALIZATION":
		start_tok = p.curr_tok
		advance_token(p)
		event_kind = .Initialization
	case "TOP-OF-PAGE":
		// Already consumed by check_compound_keyword
		event_kind = .TopOfPage
	case "END-OF-PAGE":
		// Already consumed by check_compound_keyword
		event_kind = .EndOfPage
	case:
		// Unknown event
		start_tok = p.curr_tok
		end_tok := skip_to_new_line(p)
		bad_decl := ast.new(ast.Bad_Decl, start_tok, end_tok)
		return bad_decl
	}

	expect_token(p, .Period)

	event_block := ast.new(ast.Event_Block, start_tok.range)
	event_block.kind = event_kind
	event_block.body = make([dynamic]^ast.Stmt)

	// Parse body until we hit another event or EOF
	for p.curr_tok.kind != .EOF {
		// Check for event keywords that would end this block
		if p.curr_tok.kind == .Ident {
			if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
				kw := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
				// Check for simple keywords that end the block
				if kw == "INITIALIZATION" ||
				   kw == "AT" ||
				   kw == "FORM" ||
				   kw == "ENDFORM" ||
				   kw == "CLASS" ||
				   kw == "ENDCLASS" ||
				   kw == "REPORT" ||
				   kw == "INCLUDE" {
					break
				}
				// Check for compound keywords like START-OF-SELECTION
				if kw == "START" || kw == "END" || kw == "TOP" {
					break
				}
			}
		}

		stmt := parse_stmt(p)
		if stmt != nil {
			append(&event_block.body, stmt)
		}
	}

	event_block.range.end = p.prev_tok.range.end
	return event_block
}

parse_at_event_block :: proc(p: ^Parser) -> ^ast.Stmt {
	start_tok := p.curr_tok
	advance_token(p) // consume AT

	// Check for SELECTION-SCREEN
	if check_class_keyword(p, "SELECTION", "SCREEN") {
		// check_class_keyword already advanced past SELECTION-SCREEN
		expect_token(p, .Period)

		event_block := ast.new(ast.Event_Block, start_tok.range)
		event_block.kind = .AtSelectionScreen
		event_block.body = make([dynamic]^ast.Stmt)

		// Parse body until we hit another event or EOF
		for p.curr_tok.kind != .EOF {
			if p.curr_tok.kind == .Ident {
				if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
					kw := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
					if kw == "START" ||
					   kw == "END" ||
					   kw == "INITIALIZATION" ||
					   kw == "AT" ||
					   kw == "TOP" ||
					   kw == "FORM" ||
					   kw == "ENDFORM" ||
					   kw == "CLASS" ||
					   kw == "ENDCLASS" {
						break
					}
				}
			}

			stmt := parse_stmt(p)
			if stmt != nil {
				append(&event_block.body, stmt)
			}
		}

		event_block.range.end = p.prev_tok.range.end
		return event_block
	}

	// Unknown AT event, skip line
	end_tok := skip_to_new_line(p)
	error(p, lexer.range_between(start_tok, end_tok), "unknown AT event")
	bad_decl := ast.new(ast.Bad_Decl, start_tok, end_tok)
	return bad_decl
}

parse_module_decl :: proc(p: ^Parser) -> ^ast.Decl {
	module_tok := expect_keyword_token(p, "MODULE")
	ident_tok := expect_token(p, .Ident)

	module_type: ast.Module_Type
	if check_keyword(p, "OUTPUT") {
		advance_token(p)
		module_type = .Output
	} else if check_keyword(p, "INPUT") {
		advance_token(p)
		module_type = .Input
	} else {
		module_type = .Input
	}

	expect_token(p, .Period)

	module_decl := ast.new(ast.Module_Decl, module_tok.range)
	module_decl.ident = ast.new_ident(ident_tok)
	module_decl.module_type = module_type
	module_decl.body = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDMODULE") {
			break
		}

		stmt := parse_stmt(p)
		if stmt != nil {
			append(&module_decl.body, stmt)
		}
	}

	endmodule_tok := expect_keyword_token(p, "ENDMODULE")
	period_tok := expect_token(p, .Period)
	module_decl.range.end = period_tok.range.end

	return module_decl
}