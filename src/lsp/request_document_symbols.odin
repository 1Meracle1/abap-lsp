package abap_frontend_lsp

import "src:ast"

import json "core:encoding/json"
import "core:mem"

SYMBOL_KIND_MODULE :: 2
SYMBOL_KIND_CLASS :: 5
SYMBOL_KIND_METHOD :: 6
SYMBOL_KIND_INTERFACE :: 11
SYMBOL_KIND_FUNCTION :: 12

handle_document_symbols :: proc(ctx: ^Request_Context, params: json.Value) {
	uri := uri_from_text_document_params(params)
	if uri == "" {
		send_success(ctx.output, ctx.id, []Document_Symbol{}, ctx.state.allocator)
		return
	}
	doc, ok := ctx.state.documents[uri]
	if !ok || !doc.has_parse || doc.parse_root == nil {
		send_success(ctx.output, ctx.id, []Document_Symbol{}, ctx.state.allocator)
		return
	}
	symbols := document_symbols_for_file(doc.parse_root, doc.text, context.temp_allocator)
	send_success(ctx.output, ctx.id, symbols, context.temp_allocator)
}

document_symbols_for_file :: proc(
	file: ^ast.File,
	source: string,
	allocator: mem.Allocator,
) -> []Document_Symbol {
	if file == nil {
		return nil
	}
	return document_symbols_for_statements(file.stmts, source, allocator)
}

document_symbols_for_statements :: proc(
	stmts: [dynamic]^ast.Stmt,
	source: string,
	allocator: mem.Allocator,
) -> []Document_Symbol {
	out := make([dynamic]Document_Symbol, 0, 8, allocator)
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		#partial switch n in stmt.derived_stmt {
		case ^ast.Oop_Simple_Stmt:
			if n.kind == .Methods || n.kind == .Class_Methods {
				for member in n.members {
					if member.name.text == "" || member.name.range.end <= member.name.range.start {
						continue
					}
					append(&out, Document_Symbol {
						name = member.name.text,
						detail = "CLASS-METHOD DECLARATION" if n.kind == .Class_Methods else "METHOD DECLARATION",
						kind = SYMBOL_KIND_METHOD,
						range = range_from_offsets(source, member.range.start, member.range.end),
						selection_range = range_from_offsets(
							source,
							member.name.range.start,
							member.name.range.end,
						),
					})
				}
			}
		}
		if symbol, ok := document_symbol_for_statement(stmt, source, allocator); ok {
			append(&out, symbol)
		}
	}
	return out[:]
}

document_symbol_for_statement :: proc(
	stmt: ^ast.Stmt,
	source: string,
	allocator: mem.Allocator,
) -> (Document_Symbol, bool) {
	if stmt == nil {
		return {}, false
	}
	name := ast.Token_Text{}
	detail := ""
	kind := 0
	children: []Document_Symbol
	#partial switch n in stmt.derived_stmt {
	case ^ast.Class_Decl:
		name = n.name
		kind = SYMBOL_KIND_CLASS
		detail = "CLASS IMPLEMENTATION" if .Implementation in n.flags else "CLASS DEFINITION"
		children = document_symbols_for_statements(n.body, source, allocator)
	case ^ast.Interface_Decl:
		name = n.name
		kind = SYMBOL_KIND_INTERFACE
		detail = "INTERFACE"
		children = document_symbols_for_statements(n.body, source, allocator)
	case ^ast.Method_Decl:
		name = n.name
		kind = SYMBOL_KIND_METHOD
		detail = "METHOD"
		children = document_symbols_for_statements(n.body, source, allocator)
	case ^ast.Form_Decl:
		name = n.name
		kind = SYMBOL_KIND_FUNCTION
		detail = "FORM"
		children = document_symbols_for_statements(n.body, source, allocator)
	case ^ast.Function_Decl:
		name = n.name
		kind = SYMBOL_KIND_FUNCTION
		detail = "FUNCTION"
		children = document_symbols_for_statements(n.body, source, allocator)
	case ^ast.Module_Decl:
		name = n.name
		kind = SYMBOL_KIND_MODULE
		switch n.flow {
		case .Input:
			detail = "MODULE INPUT"
		case .Output:
			detail = "MODULE OUTPUT"
		case .None:
			detail = "MODULE"
		}
		children = document_symbols_for_statements(n.body, source, allocator)
	case:
		return {}, false
	}
	if name.text == "" || name.range.end <= name.range.start {
		return {}, false
	}
	return Document_Symbol {
		name = name.text,
		detail = detail,
		kind = kind,
		range = range_from_offsets(source, stmt.range.start, stmt.range.end),
		selection_range = range_from_offsets(source, name.range.start, name.range.end),
		children = children,
	}, true
}
