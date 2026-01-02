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
	"MODIFY",
	"DELETE",
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
	"PUBLIC",
	"PRIVATE",
	"PROTECTED",
	"ABSTRACT",
	"FINAL",
	"STATIC",
	"READ-ONLY",
	// Assignment
	"MOVE",
	"MOVE-CORRESPONDING",
	"ASSIGN",
	"UNASSIGN",
	// Other
	"CALL",
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

// collect_completion_items gathers all symbols available at the given offset
collect_completion_items :: proc(snap: ^cache.Snapshot, offset: int) -> [dynamic]CompletionItem {
	items := make([dynamic]CompletionItem, context.temp_allocator)

	// Check if we're completing struct fields (after typing "var-" or "var-field-")
	if struct_type := find_struct_type_at_cursor(snap, offset); struct_type != nil {
		// Return only struct fields - don't mix with keywords
		return struct_fields_to_completion_items(struct_type)
	}

	// Add ABAP keywords
	for keyword in ABAP_KEYWORDS {
		append(&items, CompletionItem{
			label  = keyword,
			kind   = .Keyword,
			detail = "keyword",
		})
	}

	// If we're inside a form, add local symbols first
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

	// Add global symbols
	for _, sym in snap.symbol_table.symbols {
		append(&items, symbol_to_completion_item(sym))
	}

	return items
}

// find_struct_type_at_cursor checks if the cursor is positioned after a '-' following
// a structured variable (e.g., "my_struct-" or "my_struct-field-"), and returns
// the structure type whose fields should be completed.
// Supports nested access like: parent-child-| where child is also a structure.
find_struct_type_at_cursor :: proc(snap: ^cache.Snapshot, offset: int) -> ^symbols.Type {
	if len(snap.text) == 0 || offset < 2 {
		return nil
	}

	// Parse the access chain backwards from cursor position
	// e.g., for "my_struct-field-|" we get ["my_struct", "field"]
	chain := parse_access_chain_backwards(snap.text, offset)
	if len(chain) == 0 {
		return nil
	}
	defer delete(chain)

	// Resolve the chain to get the final struct type
	return resolve_access_chain(snap, chain[:], offset)
}

// parse_access_chain_backwards scans backwards from the cursor to extract
// the chain of identifiers separated by '-'.
// For "parent-child-|" it returns ["parent", "child"]
// For "parent-|" it returns ["parent"]
// Returns empty if cursor is not after a '-'
parse_access_chain_backwards :: proc(text: string, offset: int) -> [dynamic]string {
	chain := make([dynamic]string, context.temp_allocator)

	if offset < 2 || offset > len(text) {
		return chain
	}

	pos := offset - 1

	// Check if we're immediately after a '-'
	if text[pos] != '-' {
		return chain
	}

	// Scan backwards collecting identifiers
	for {
		// Move past the '-'
		pos -= 1
		if pos < 0 {
			break
		}

		// Skip whitespace between identifier and '-'
		for pos >= 0 && (text[pos] == ' ' || text[pos] == '\t') {
			pos -= 1
		}
		if pos < 0 {
			break
		}

		// Find the end of the identifier
		ident_end := pos + 1

		// Scan backwards to find start of identifier
		for pos >= 0 && is_ident_char(text[pos]) {
			pos -= 1
		}
		ident_start := pos + 1

		if ident_start >= ident_end {
			break
		}

		// Extract identifier (uppercase for ABAP case-insensitivity)
		ident := strings.to_lower(text[ident_start:ident_end], context.temp_allocator)

		// Insert at beginning of chain (we're parsing backwards)
		inject_at(&chain, 0, ident)

		// Check if there's another '-' before this identifier
		// Skip whitespace
		for pos >= 0 && (text[pos] == ' ' || text[pos] == '\t') {
			pos -= 1
		}

		if pos < 0 || text[pos] != '-' {
			// No more chain segments
			break
		}
		// Continue to next segment (there's a '-' before this identifier)
	}

	return chain
}

// resolve_access_chain takes a chain of identifiers like ["parent", "child"]
// and resolves through the type system to find the final struct type.
// For "parent-child-|", it finds parent's type, then finds child field's type.
resolve_access_chain :: proc(snap: ^cache.Snapshot, chain: []string, offset: int) -> ^symbols.Type {
	if len(chain) == 0 {
		return nil
	}

	// First element is the variable name - look it up in scope
	var_name := chain[0]
	var_type := lookup_variable_type(snap, var_name, offset)
	if var_type == nil {
		return nil
	}

	// Resolve the type if it's a named type
	current_type := resolve_to_struct_type(snap, var_type)
	if current_type == nil {
		return nil
	}

	// Walk through the rest of the chain
	for i := 1; i < len(chain); i += 1 {
		field_name := chain[i]

		// Find the field in the current struct
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

		// Resolve to struct type for next iteration
		current_type = resolve_to_struct_type(snap, field_type)
		if current_type == nil {
			return nil
		}
	}

	return current_type
}

// lookup_variable_type finds a variable by name in the appropriate scope
// and returns its type.
lookup_variable_type :: proc(snap: ^cache.Snapshot, var_name: string, offset: int) -> ^symbols.Type {
	// First check local scope if we're inside a form
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

	// Check global scope
	if global_sym, found := snap.symbol_table.symbols[var_name]; found {
		return global_sym.type_info
	}

	return nil
}

// resolve_to_struct_type resolves a type to its underlying structure type.
// Handles named types by looking up the type definition.
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
		// For tables, we could complete on the element type if it's a struct
		// This handles cases like: itab[1]-field
		// But for just "itab-", we don't complete (tables use [] for access)
		return nil
	}

	return nil
}

// struct_fields_to_completion_items converts structure fields to completion items
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

// is_ident_char returns true if the character is valid in an ABAP identifier
is_ident_char :: proc(c: u8) -> bool {
	return (c >= 'a' && c <= 'z') ||
	       (c >= 'A' && c <= 'Z') ||
	       (c >= '0' && c <= '9') ||
	       c == '_'
}

// symbol_to_completion_item converts a Symbol to a CompletionItem
symbol_to_completion_item :: proc(sym: symbols.Symbol) -> CompletionItem {
	kind: CompletionItemKind
	detail: string

	switch sym.kind {
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
	}

	return CompletionItem {
		label  = sym.name,
		kind   = kind,
		detail = detail if len(detail) > 0 else nil,
	}
}

