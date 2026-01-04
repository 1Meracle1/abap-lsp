package lsp

import "../cache"
import "../lang/ast"
import "../lang/symbols"
import "core:encoding/json"
import "core:fmt"
import "core:strings"

// ABAP keywords for completion
ABAP_KEYWORDS :: []string{
	// Declarations
	"DATA",
	"TYPES",
	"CONSTANTS",
	"FIELD-SYMBOLS",
	"PARAMETERS",
	"SELECT-OPTIONS",
	"TABLES",
	// Program structure
	"REPORT",
	"PROGRAM",
	"INCLUDE",
	"FORM",
	"ENDFORM",
	"PERFORM",
	// Events
	"START-OF-SELECTION",
	"END-OF-SELECTION",
	"INITIALIZATION",
	"AT SELECTION-SCREEN",
	"TOP-OF-PAGE",
	"END-OF-PAGE",
	"FUNCTION",
	"ENDFUNCTION",
	"METHOD",
	"ENDMETHOD",
	"METHODS",
	"CLASS",
	"ENDCLASS",
	"INTERFACE",
	"ENDINTERFACE",
	"MODULE",
	"ENDMODULE",
	"DEFINITION",
	"IMPLEMENTATION",
	// Control flow
	"IF",
	"ELSE",
	"ELSEIF",
	"ENDIF",
	"CASE",
	"WHEN",
	"ENDCASE",
	"DO",
	"ENDDO",
	"WHILE",
	"ENDWHILE",
	"LOOP",
	"ENDLOOP",
	"AT",
	"ENDAT",
	"CHECK",
	"EXIT",
	"CONTINUE",
	"RETURN",
	"SET PF-STATUS",
	"SET TITLEBAR",
	"SET CURSOR FIELD",
	"SET SCREEN",
	// Database operations
	"SELECT",
	"ENDSELECT",
	"FROM",
	"INTO",
	"WHERE",
	"ORDER BY",
	"GROUP BY",
	"HAVING",
	"INSERT",
	"UPDATE",
	"MODIFY TABLE",
	"MODIFY SCREEN",
	"DELETE",
	"LEAVE PROGRAM",
	// Internal table operations
	"APPEND",
	"COLLECT",
	"READ TABLE",
	"SORT",
	"CLEAR",
	"REFRESH",
	"FREE",
	// Exception handling
	"TRY",
	"CATCH",
	"CLEANUP",
	"ENDTRY",
	"RAISE",
	"RAISING",
	// I/O
	"WRITE",
	"FORMAT",
	"ULINE",
	"SKIP",
	"NEW-LINE",
	// Type keywords
	"TYPE",
	"LIKE",
	"REF TO",
	"VALUE",
	"OPTIONAL",
	"DEFAULT",
	"BEGIN OF",
	"END OF",
	"STRUCTURE",
	// Parameter keywords
	"USING",
	"CHANGING",
	"IMPORTING",
	"EXPORTING",
	"RETURNING",
	"EXCEPTIONS",
	// Access modifiers
	"PUBLIC SECTION",
	"PRIVATE SECTION",
	"PROTECTED SECTION",
	"ABSTRACT",
	"FINAL",
	"STATIC",
	"READ-ONLY",
	"REDEFINITION",
	"INTERFACES",
	// Assignment
	"MOVE",
	"MOVE-CORRESPONDING",
	"ASSIGN",
	"UNASSIGN",
	// Other
	"CALL",
	"CALL SCREEN",
	"CALL FUNCTION",
	"CALL METHOD",
	"ASSERT",
	"MESSAGE",
	"NEW",
	"CREATE OBJECT",
	"INITIAL",
	"IS INITIAL",
	"IS NOT INITIAL",
	"IS BOUND",
	"IS ASSIGNED",
	"AND",
	"OR",
	"NOT",
	"EQ",
	"NE",
	"LT",
	"LE",
	"GT",
	"GE",
	"BETWEEN",
	"IN",
	"LIKE",
	"CO",
	"CN",
	"CA",
	"NA",
	"CS",
	"NS",
	"CP",
	"NP",
}

handle_completion :: proc(srv: ^Server, id: json.Value, params: json.Value) {
	completion_params: CompletionParams
	if err := unmarshal(params, completion_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("completion request unmarshal failed: %v", err)
		log_trace(srv, descr)
		reply_error(srv, id, .ParseError, descr)
		return
	}
	log_trace(srv, fmt.tprintf("completion_params: %v", completion_params))

	snap := cache.get_snapshot(srv.storage, completion_params.textDocument.uri)
	if snap == nil {
		reply_error(srv, id, .InvalidParams, "Document not found")
		return
	}
	defer cache.release_snapshot(snap)

	offset := position_to_offset(snap.text, completion_params.position)
	if offset < 0 {
		reply(srv, id, CompletionList{isIncomplete = false, items = {}})
		return
	}
	log_trace(srv, fmt.tprintf("completion at offset: %d", offset))

	items := collect_completion_items(snap, offset)

	result := CompletionList {
		isIncomplete = false,
		items        = items[:],
	}
	reply(srv, id, result)
}

collect_completion_items :: proc(snap: ^cache.Snapshot, offset: int) -> [dynamic]CompletionItem {
	items := make([dynamic]CompletionItem, context.temp_allocator)

	if struct_type := find_struct_type_at_cursor(snap, offset); struct_type != nil {
		return struct_fields_to_completion_items(struct_type)
	}

	for keyword in ABAP_KEYWORDS {
		append(&items, CompletionItem{
			label  = keyword,
			kind   = .Keyword,
			detail = "keyword",
		})
	}

	if enclosing_form := ast.find_enclosing_form(snap.ast, offset); enclosing_form != nil {
		form_name := enclosing_form.ident.name
		if form_sym, ok := snap.symbol_table.symbols[form_name]; ok {
			if form_sym.child_scope != nil {
				for _, sym in form_sym.child_scope.symbols {
					append(&items, symbol_to_completion_item(sym))
				}
			}
		}
	}

	if enclosing_class := ast.find_enclosing_class_def(snap.ast, offset); enclosing_class != nil {
		class_name := enclosing_class.ident.name
		if class_sym, ok := snap.symbol_table.symbols[class_name]; ok {
			if class_sym.child_scope != nil {
				for _, sym in class_sym.child_scope.symbols {
					append(&items, symbol_to_completion_item(sym))
				}
			}
		}
	}

	if enclosing_iface := ast.find_enclosing_interface(snap.ast, offset); enclosing_iface != nil {
		iface_name := enclosing_iface.ident.name
		if iface_sym, ok := snap.symbol_table.symbols[iface_name]; ok {
			if iface_sym.child_scope != nil {
				for _, sym in iface_sym.child_scope.symbols {
					append(&items, symbol_to_completion_item(sym))
				}
			}
		}
	}

	if enclosing_module := ast.find_enclosing_module(snap.ast, offset); enclosing_module != nil {
		module_name := enclosing_module.ident.name
		if module_sym, ok := snap.symbol_table.symbols[module_name]; ok {
			if module_sym.child_scope != nil {
				for _, sym in module_sym.child_scope.symbols {
					append(&items, symbol_to_completion_item(sym))
				}
			}
		}
	}

	for _, sym in snap.symbol_table.symbols {
		append(&items, symbol_to_completion_item(sym))
	}

	return items
}

find_struct_type_at_cursor :: proc(snap: ^cache.Snapshot, offset: int) -> ^symbols.Type {
	if len(snap.text) == 0 || offset < 2 {
		return nil
	}

	chain := parse_access_chain_backwards(snap.text, offset)
	if len(chain) == 0 {
		return nil
	}
	defer delete(chain)

	return resolve_access_chain(snap, chain[:], offset)
}

parse_access_chain_backwards :: proc(text: string, offset: int) -> [dynamic]string {
	chain := make([dynamic]string, context.temp_allocator)

	if offset < 2 || offset > len(text) {
		return chain
	}

	pos := offset - 1

	// Check for structure access (-) or arrow access (->)
	is_arrow := false
	if pos >= 1 && text[pos] == '>' && text[pos - 1] == '-' {
		is_arrow = true
		pos -= 1 // Move past the '>'
	} else if text[pos] != '-' {
		return chain
	}

	for {
		pos -= 1
		if pos < 0 {
			break
		}

		for pos >= 0 && (text[pos] == ' ' || text[pos] == '\t') {
			pos -= 1
		}
		if pos < 0 {
			break
		}

		// Skip past closing parenthesis if this is after a method call
		if text[pos] == ')' {
			paren_depth := 1
			pos -= 1
			for pos >= 0 && paren_depth > 0 {
				if text[pos] == ')' {
					paren_depth += 1
				} else if text[pos] == '(' {
					paren_depth -= 1
				}
				pos -= 1
			}
			if pos < 0 {
				break
			}
		}

		ident_end := pos + 1

		for pos >= 0 && is_ident_char(text[pos]) {
			pos -= 1
		}
		ident_start := pos + 1

		if ident_start >= ident_end {
			break
		}

		ident := strings.to_lower(text[ident_start:ident_end], context.temp_allocator)

		inject_at(&chain, 0, ident)

		for pos >= 0 && (text[pos] == ' ' || text[pos] == '\t') {
			pos -= 1
		}

		// Check for next separator (- or ->)
		if pos >= 1 && text[pos] == '>' && text[pos - 1] == '-' {
			pos -= 1 // Skip '>'
		} else if pos < 0 || text[pos] != '-' {
			break
		}
	}

	return chain
}

resolve_access_chain :: proc(snap: ^cache.Snapshot, chain: []string, offset: int) -> ^symbols.Type {
	if len(chain) == 0 {
		return nil
	}

	var_name := chain[0]
	var_type := lookup_variable_type(snap, var_name, offset)
	if var_type == nil {
		return nil
	}

	current_type := resolve_to_struct_type(snap, var_type)
	if current_type == nil {
		return nil
	}

	for i := 1; i < len(chain); i += 1 {
		field_name := chain[i]

		field_type: ^symbols.Type = nil
		for &field in current_type.fields {
			if strings.to_lower(field.name, context.temp_allocator) == field_name {
				field_type = field.type_info
				break
			}
		}

		if field_type == nil {
			return nil
		}

		current_type = resolve_to_struct_type(snap, field_type)
		if current_type == nil {
			return nil
		}
	}

	return current_type
}

lookup_variable_type :: proc(snap: ^cache.Snapshot, var_name: string, offset: int) -> ^symbols.Type {
	if enclosing_form := ast.find_enclosing_form(snap.ast, offset); enclosing_form != nil {
		form_name := enclosing_form.ident.name
		if form_sym, ok := snap.symbol_table.symbols[form_name]; ok {
			if form_sym.child_scope != nil {
				if local_sym, found := form_sym.child_scope.symbols[var_name]; found {
					return local_sym.type_info
				}
			}
		}
	}

	if global_sym, found := snap.symbol_table.symbols[var_name]; found {
		return global_sym.type_info
	}

	return nil
}

resolve_to_struct_type :: proc(snap: ^cache.Snapshot, type_info: ^symbols.Type) -> ^symbols.Type {
	if type_info == nil {
		return nil
	}

	#partial switch type_info.kind {
	case .Structure:
		return type_info

	case .Named:
		// Look up the type definition
		if type_sym, found := snap.symbol_table.symbols[type_info.name]; found {
			if type_sym.type_info != nil {
				return resolve_to_struct_type(snap, type_sym.type_info)
			}
		}
		return nil

	case .Table:
		return nil
	}

	return nil
}

struct_fields_to_completion_items :: proc(struct_type: ^symbols.Type) -> [dynamic]CompletionItem {
	items := make([dynamic]CompletionItem, context.temp_allocator)

	if struct_type == nil || struct_type.kind != .Structure {
		return items
	}

	for field in struct_type.fields {
		detail := symbols.format_type(field.type_info)

		append(&items, CompletionItem{
			label  = field.name,
			kind   = .Field,
			detail = detail if len(detail) > 0 else nil,
		})
	}

	return items
}

is_ident_char :: proc(c: u8) -> bool {
	return (c >= 'a' && c <= 'z') ||
	       (c >= 'A' && c <= 'Z') ||
	       (c >= '0' && c <= '9') ||
	       c == '_'
}

symbol_to_completion_item :: proc(sym: symbols.Symbol) -> CompletionItem {
	kind: CompletionItemKind
	detail: string

	#partial switch sym.kind {
	case .Variable:
		kind = .Variable
		detail = symbols.format_type(sym.type_info)
	case .Constant:
		kind = .Constant
		detail = symbols.format_type(sym.type_info)
	case .Parameter:
		kind = .Variable
		detail = symbols.format_type(sym.type_info)
	case .Field:
		kind = .Field
		detail = symbols.format_type(sym.type_info)
	case .Method:
		kind = .Method
		detail = "METHOD"
	case .Class:
		kind = .Class
		detail = "CLASS"
	case .Interface:
		kind = .Interface
		detail = "INTERFACE"
	case .Form:
		kind = .Function
		detail = "FORM"
	case .FormParameter:
		kind = .Variable
		#partial switch sym.form_param_kind {
		case .Tables:
			detail = fmt.tprintf("TABLES %s", symbols.format_type(sym.type_info))
		case .Using:
			detail = fmt.tprintf("USING %s", symbols.format_type(sym.type_info))
		case .Changing:
			detail = fmt.tprintf("CHANGING %s", symbols.format_type(sym.type_info))
		case:
			detail = symbols.format_type(sym.type_info)
		}
	case .TypeDef:
		kind = .Struct
		detail = symbols.format_type(sym.type_info)
	case .Report:
		kind = .Module
		detail = "REPORT"
	case .Include:
		kind = .File
		detail = "INCLUDE"
	case .Event:
		kind = .Event
		detail = "EVENT"
	case .Module:
		kind = .Function
		detail = "MODULE"
	case .FieldSymbol:
		kind = .Variable
		detail = fmt.tprintf("FIELD-SYMBOL %s", symbols.format_type(sym.type_info))
	}

	return CompletionItem {
		label  = sym.name,
		kind   = kind,
		detail = detail if len(detail) > 0 else nil,
	}
}

