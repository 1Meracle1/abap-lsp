package lsp

import "../cache"
import "../lang/ast"
import "../lang/symbols"
import "core:encoding/json"
import "core:fmt"

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
		detail = "method"
	case .Class:
		kind = .Class
		detail = "class"
	case .Interface:
		kind = .Interface
		detail = "interface"
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
	}

	return CompletionItem {
		label  = sym.name,
		kind   = kind,
		detail = detail if len(detail) > 0 else nil,
	}
}

