package lsp

import "../cache"
import "core:encoding/json"
import "core:fmt"
import "core:strings"

import "../lang/ast"
import "../lang/symbols"

handle_hover :: proc(srv: ^Server, id: json.Value, params: json.Value) {
	hover_params: HoverParams
	if err := unmarshal(params, hover_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("hover request unmarshal failed: %v", err)
		log_trace(srv, descr)
		reply_error(srv, id, .ParseError, descr)
		return
	}
	log_trace(srv, fmt.tprintf("hover_params: %v", hover_params))

	snap := cache.get_snapshot(srv.storage, hover_params.textDocument.uri)
	if snap == nil {
		reply_error(srv, id, .InvalidParams, "Document not found")
		return
	}
	defer cache.release_snapshot(snap)

	offset := position_to_offset(snap.text, hover_params.position)
	if offset < 0 {
		reply(srv, id, json.Null(nil))
		return
	}
	log_trace(srv, fmt.tprintf("hover at offset: %d", offset))

	node := ast.find_node_at_offset(&snap.ast.node, offset)
	if node == nil {
		log_trace(srv, "no node found at offset")
		reply(srv, id, json.Null(nil))
		return
	}

	hover_text := ""

	#partial switch n in node.derived {
	case ^ast.Ident:
		log_trace(srv, fmt.tprintf("found ident: %s", n.name))
		// Look up symbol in the correct scope chain
		if sym, ok := lookup_symbol_at_offset(snap, n.name, offset); ok {
			if sym.kind == .Form {
				// For FORM symbols, show the full signature
				hover_text = format_form_signature(sym)
			} else {
				type_str := symbols.format_type(sym.type_info)
				hover_text = fmt.tprintf("%s: %s", sym.name, type_str)
			}
		} else {
			hover_text = fmt.tprintf("(unknown) %s", n.name)
		}
	case:
	// For other nodes, maybe just show the type of node?
	// or nothing
	}

	if hover_text != "" {
		result := Hover {
			contents = MarkupContent{kind = MarkupKind_Markdown, value = hover_text},
		}
		reply(srv, id, result)
	} else {
		reply(srv, id, json.Null(nil))
	}
}

// format_form_signature formats a complete FORM signature from the Form symbol.
// Output example:
//   FORM process_data TABLES it_input
//                     USING p_mode TYPE string
//                     CHANGING c_count TYPE i
format_form_signature :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .Form || sym.child_scope == nil {
		return sym.name
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)

	// Wrap in code block for proper Markdown rendering with preserved newlines
	strings.write_string(&b, "```abap\n")

	// Write FORM name
	strings.write_string(&b, "FORM ")
	strings.write_string(&b, sym.name)

	// Calculate indent for continuation lines (align to after "FORM ")
	indent := 5 + len(sym.name) // "FORM " is 5 chars

	// Collect parameters by kind
	tables_params := make([dynamic]symbols.Symbol, context.temp_allocator)
	using_params := make([dynamic]symbols.Symbol, context.temp_allocator)
	changing_params := make([dynamic]symbols.Symbol, context.temp_allocator)

	for _, param_sym in sym.child_scope.symbols {
		if param_sym.kind == .FormParameter {
			switch param_sym.form_param_kind {
			case .Tables:
				append(&tables_params, param_sym)
			case .Using:
				append(&using_params, param_sym)
			case .Changing:
				append(&changing_params, param_sym)
			case .None:
			// Skip non-parameters (like local variables)
			}
		}
	}

	// Helper to write parameter list
	write_params :: proc(b: ^strings.Builder, keyword: string, params: []symbols.Symbol, indent: int, is_first: ^bool) {
		if len(params) == 0 {
			return
		}

		if is_first^ {
			strings.write_byte(b, ' ')
			is_first^ = false
		} else {
			strings.write_byte(b, '\n')
			for _ in 0 ..< indent {
				strings.write_byte(b, ' ')
			}
		}

		strings.write_string(b, keyword)
		for param, i in params {
			if i > 0 {
				strings.write_byte(b, ' ')
			}
			strings.write_byte(b, ' ')
			strings.write_string(b, param.name)
			if param.type_info != nil && param.type_info.kind != .Unknown {
				strings.write_string(b, " TYPE ")
				strings.write_string(b, symbols.format_type(param.type_info))
			}
		}
	}

	is_first := true
	write_params(&b, "TABLES", tables_params[:], indent, &is_first)
	write_params(&b, "USING", using_params[:], indent, &is_first)
	write_params(&b, "CHANGING", changing_params[:], indent, &is_first)

	// Close the code block
	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

// lookup_symbol_at_offset looks up a symbol by name, considering the scope at the given offset.
// It first checks if the offset is inside a form/function and looks in the local scope,
// then falls back to the global scope.
lookup_symbol_at_offset :: proc(snap: ^cache.Snapshot, name: string, offset: int) -> (symbols.Symbol, bool) {
	// Check if we're inside a form declaration
	if enclosing_form := ast.find_enclosing_form(snap.ast, offset); enclosing_form != nil {
		// Get the form's name to find its symbol (which has the child_scope)
		form_name := enclosing_form.ident.name
		if form_sym, ok := snap.symbol_table.symbols[form_name]; ok {
			// Look up in the form's local scope first
			if form_sym.child_scope != nil {
				if sym, found := form_sym.child_scope.symbols[name]; found {
					return sym, true
				}
			}
		}
	}

	// Fall back to global scope
	if sym, ok := snap.symbol_table.symbols[name]; ok {
		return sym, true
	}

	return {}, false
}