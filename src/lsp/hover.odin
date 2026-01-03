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
			#partial switch sym.kind {
			case .Form:
				hover_text = format_form_signature(sym)
			case .Class:
				hover_text = format_class_signature(sym)
			case .Interface:
				hover_text = format_interface_signature(sym)
			case .Method:
				hover_text = format_method_signature(sym)
			case .TypeDef:
				if sym.type_info != nil && sym.type_info.kind == .Structure {
					hover_text = format_struct_type(sym)
				} else {
					type_str := symbols.format_type(sym.type_info)
					hover_text = fmt.tprintf("(type) %s = %s", sym.name, type_str)
				}
			case .Report:
				hover_text = fmt.tprintf("(report) %s", sym.name)
			case .Include:
				hover_text = fmt.tprintf("(include) %s", sym.name)
			case .Event:
				hover_text = format_event_signature(sym)
			case .Module:
				hover_text = format_module_signature(sym)
			case:
				type_str := symbols.format_type(sym.type_info)
				hover_text = fmt.tprintf("%s: %s", sym.name, type_str)
			}
		} else {
			hover_text = fmt.tprintf("(unknown) %s", n.name)
		}

	case ^ast.New_Expr:
		log_trace(srv, "found NEW expression")
		if n.is_inferred {
			hover_text = "NEW #( ) - creates instance with inferred type"
		} else if n.type_expr != nil {
			if type_ident, ok := n.type_expr.derived_expr.(^ast.Ident); ok {
				if sym, found := lookup_symbol_at_offset(snap, type_ident.name, offset); found {
					if sym.kind == .Class {
						hover_text = fmt.tprintf(
							"NEW %s( ) - creates instance of class %s",
							type_ident.name,
							type_ident.name,
						)
					} else {
						hover_text = fmt.tprintf(
							"NEW %s( ) - creates reference to %s",
							type_ident.name,
							type_ident.name,
						)
					}
				} else {
					hover_text = fmt.tprintf("NEW %s( ) - creates reference", type_ident.name)
				}
			} else {
				hover_text = "NEW type( ) - creates instance"
			}
		} else {
			hover_text = "NEW expression"
		}

	case ^ast.Call_Expr:
		log_trace(srv, "found Call expression")
		// Get the method name being called
		method_name := get_call_method_name(n)
		if method_name != "" {
			hover_text = fmt.tprintf("(method call) %s( )", method_name)
		} else {
			hover_text = "(method call)"
		}

	case ^ast.Selector_Expr:
		log_trace(srv, "found Selector expression")
		if n.field != nil {
			field_name := n.field.name
			if sym, ok := lookup_symbol_at_offset(snap, field_name, offset); ok {
				type_str := symbols.format_type(sym.type_info)
				hover_text = fmt.tprintf("%s: %s", sym.name, type_str)
			}
		}

	case ^ast.Named_Arg:
		log_trace(srv, "found Named argument")
		if n.name != nil {
			hover_text = fmt.tprintf("(parameter) %s", n.name.name)
		}

	case ^ast.String_Template_Expr:
		log_trace(srv, "found String Template expression")
		// Build a preview of the string template
		hover_text = "(string template) |...|"

	case ^ast.Binary_Expr:
		log_trace(srv, "found Binary expression")
		hover_text = format_binary_expr_hover(n)

	case ^ast.Paren_Expr:
		log_trace(srv, "found Parenthesized expression")
		hover_text = "(parenthesized expression)"

	case ^ast.Message_Stmt:
		log_trace(srv, "found MESSAGE statement")
		hover_text = "(statement) MESSAGE - displays a message to the user"

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
			}
		}
	}

	write_params :: proc(
		b: ^strings.Builder,
		keyword: string,
		params: []symbols.Symbol,
		indent: int,
		is_first: ^bool,
	) {
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

format_struct_type :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .TypeDef || sym.type_info == nil || sym.type_info.kind != .Structure {
		return symbols.format_type(sym.type_info)
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)

	// Wrap in code block for proper Markdown rendering
	strings.write_string(&b, "```abap\n")
	strings.write_string(&b, "TYPES: BEGIN OF ")
	strings.write_string(&b, sym.name)

	format_struct_fields(&b, sym.type_info, 2)

	strings.write_string(&b, ",\n       END OF ")
	strings.write_string(&b, sym.name)
	strings.write_string(&b, ".\n```")

	return strings.to_string(b)
}

format_struct_fields :: proc(b: ^strings.Builder, t: ^symbols.Type, indent: int) {
	if t == nil || t.kind != .Structure {
		return
	}

	for field in t.fields {
		strings.write_string(b, ",\n")
		for _ in 0 ..< indent + 5 {
			strings.write_byte(b, ' ')
		}

		if field.type_info != nil && field.type_info.kind == .Structure {
			// Nested structure
			strings.write_string(b, "BEGIN OF ")
			strings.write_string(b, field.name)
			format_struct_fields(b, field.type_info, indent + 2)
			strings.write_string(b, ",\n")
			for _ in 0 ..< indent + 5 {
				strings.write_byte(b, ' ')
			}
			strings.write_string(b, "END OF ")
			strings.write_string(b, field.name)
		} else {
			// Regular field
			strings.write_string(b, field.name)
			strings.write_string(b, " TYPE ")
			strings.write_string(b, symbols.format_type(field.type_info))
		}
	}
}

lookup_symbol_at_offset :: proc(
	snap: ^cache.Snapshot,
	name: string,
	offset: int,
) -> (
	symbols.Symbol,
	bool,
) {
	if enclosing_form := ast.find_enclosing_form(snap.ast, offset); enclosing_form != nil {
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

	if enclosing_class := ast.find_enclosing_class_def(snap.ast, offset); enclosing_class != nil {
		class_name := enclosing_class.ident.name
		if class_sym, ok := snap.symbol_table.symbols[class_name]; ok {
			if class_sym.child_scope != nil {
				if sym, found := class_sym.child_scope.symbols[name]; found {
					return sym, true
				}
			}
		}
	}

	if enclosing_iface := ast.find_enclosing_interface(snap.ast, offset); enclosing_iface != nil {
		iface_name := enclosing_iface.ident.name
		if iface_sym, ok := snap.symbol_table.symbols[iface_name]; ok {
			if iface_sym.child_scope != nil {
				if sym, found := iface_sym.child_scope.symbols[name]; found {
					return sym, true
				}
			}
		}
	}

	if enclosing_module := ast.find_enclosing_module(snap.ast, offset); enclosing_module != nil {
		module_name := enclosing_module.ident.name
		if module_sym, ok := snap.symbol_table.symbols[module_name]; ok {
			if module_sym.child_scope != nil {
				if sym, found := module_sym.child_scope.symbols[name]; found {
					return sym, true
				}
			}
		}
	}

	if sym, ok := snap.symbol_table.symbols[name]; ok {
		return sym, true
	}

	return {}, false
}

format_class_signature :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .Class {
		return sym.name
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)

	strings.write_string(&b, "```abap\n")
	strings.write_string(&b, "CLASS ")
	strings.write_string(&b, sym.name)
	strings.write_string(&b, " DEFINITION")

	if sym.child_scope != nil {
		method_count := 0
		attr_count := 0
		type_count := 0
		for _, member in sym.child_scope.symbols {
			#partial switch member.kind {
			case .Method:
				method_count += 1
			case .Field:
				attr_count += 1
			case .TypeDef:
				type_count += 1
			}
		}
		if method_count > 0 || attr_count > 0 || type_count > 0 {
			strings.write_string(&b, "\n  * Methods: ")
			strings.write_string(&b, fmt.tprintf("%d", method_count))
			strings.write_string(&b, "\n  * Attributes: ")
			strings.write_string(&b, fmt.tprintf("%d", attr_count))
			if type_count > 0 {
				strings.write_string(&b, "\n  * Types: ")
				strings.write_string(&b, fmt.tprintf("%d", type_count))
			}
		}
	}

	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

format_interface_signature :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .Interface {
		return sym.name
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)

	strings.write_string(&b, "```abap\n")
	strings.write_string(&b, "INTERFACE ")
	strings.write_string(&b, sym.name)

	if sym.child_scope != nil {
		method_count := 0
		for _, member in sym.child_scope.symbols {
			if member.kind == .Method {
				method_count += 1
			}
		}
		if method_count > 0 {
			strings.write_string(&b, "\n  * Methods: ")
			strings.write_string(&b, fmt.tprintf("%d", method_count))
		}
	}

	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

format_method_signature :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .Method {
		return sym.name
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)

	strings.write_string(&b, "```abap\n")
	strings.write_string(&b, "METHODS ")
	strings.write_string(&b, sym.name)

	if sym.child_scope != nil {
		importing := make([dynamic]symbols.Symbol, context.temp_allocator)
		exporting := make([dynamic]symbols.Symbol, context.temp_allocator)
		changing := make([dynamic]symbols.Symbol, context.temp_allocator)
		returning := make([dynamic]symbols.Symbol, context.temp_allocator)

		for _, param in sym.child_scope.symbols {
			if param.kind == .Parameter {
				append(&importing, param)
			}
		}

		if len(importing) > 0 {
			strings.write_string(&b, "\n  IMPORTING")
			for param in importing {
				strings.write_string(&b, " ")
				strings.write_string(&b, param.name)
				if param.type_info != nil && param.type_info.kind != .Unknown {
					strings.write_string(&b, " TYPE ")
					strings.write_string(&b, symbols.format_type(param.type_info))
				}
			}
		}
	}

	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

format_event_signature :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .Event {
		return sym.name
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)

	strings.write_string(&b, "```abap\n")

	// Convert the event name to uppercase for display
	event_name := strings.to_upper(sym.name, context.temp_allocator)
	strings.write_string(&b, event_name)
	strings.write_string(&b, ".")
	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

format_module_signature :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .Module {
		return sym.name
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)

	strings.write_string(&b, "```abap\n")
	strings.write_string(&b, "MODULE ")
	strings.write_string(&b, sym.name)
	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

// get_call_method_name extracts the method name from a Call_Expr
get_call_method_name :: proc(call: ^ast.Call_Expr) -> string {
	if call == nil || call.expr == nil {
		return ""
	}

	#partial switch e in call.expr.derived_expr {
	case ^ast.Ident:
		return e.name
	case ^ast.Selector_Expr:
		if e.field != nil {
			return e.field.name
		}
	}
	return ""
}

// format_binary_expr_hover formats hover text for a binary expression
format_binary_expr_hover :: proc(expr: ^ast.Binary_Expr) -> string {
	if expr == nil {
		return ""
	}

	op_str := expr.op.lit
	if op_str == "" {
		// Use kind for operators that don't have literal text
		#partial switch expr.op.kind {
		case .Plus:
			op_str = "+"
		case .Minus:
			op_str = "-"
		case .Star:
			op_str = "*"
		case .Slash:
			op_str = "/"
		case .Ampersand:
			op_str = "&"
		case:
			op_str = "?"
		}
	}

	// Determine the operation type
	op_type := ""
	#partial switch expr.op.kind {
	case .Plus, .Minus:
		op_type = "arithmetic"
	case .Star, .Slash:
		op_type = "arithmetic"
	case .Ampersand:
		op_type = "string concatenation"
	case .Ident:
		upper_op := strings.to_upper(op_str, context.temp_allocator)
		if upper_op == "MOD" || upper_op == "DIV" {
			op_type = "arithmetic"
		} else if upper_op == "AND" || upper_op == "OR" {
			op_type = "logical"
		} else {
			op_type = "comparison"
		}
	case:
		op_type = "binary"
	}

	return fmt.tprintf("(%s operation) %s", op_type, op_str)
}
