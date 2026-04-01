package lsp

import "../cache"
import "../lang/ast"
import "../lang/symbols"
import "core:encoding/json"
import "core:fmt"
import "core:strings"

// ABAP keywords for completion
ABAP_KEYWORDS :: []string {
	// Declarations
	"DATA",
	"STATICS",
	"TYPES",
	"CONSTANTS",
	"FIELD-SYMBOLS",
	"CONTROLS",
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
	"CONDENSE",
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

	symbol_table := cache.get_effective_symbol_table(
		srv.storage,
		completion_params.textDocument.uri,
	)
	if symbol_table == nil {
		symbol_table = snap.symbol_table
	}

	offset := position_to_offset(snap.text, completion_params.position)
	if offset < 0 {
		reply(srv, id, CompletionList{isIncomplete = false, items = {}})
		return
	}
	log_trace(srv, fmt.tprintf("completion at offset: %d", offset))

	items := collect_completion_items(snap, offset, symbol_table)

	result := CompletionList {
		isIncomplete = false,
		items        = items[:],
	}
	reply(srv, id, result)
}

collect_completion_items :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	symbol_table: ^symbols.SymbolTable = nil,
) -> [dynamic]CompletionItem {
	items := make([dynamic]CompletionItem, context.temp_allocator)

	// Use provided symbol table or fall back to snapshot's own table
	table := symbol_table if symbol_table != nil else snap.symbol_table

	// Check for member access patterns (-, ->, =>)
	member_access := find_member_access_at_cursor(snap, offset, table)

	#partial switch member_access.access_kind {
	case .Structure:
		if member_access.struct_type != nil {
			return struct_fields_to_completion_items(member_access.struct_type)
		}
	case .Instance:
		if member_access.class_symbol != nil {
			return class_instance_members_to_completion_items(member_access.class_symbol)
		}
	case .Static:
		if member_access.class_symbol != nil {
			return class_static_members_to_completion_items(member_access.class_symbol)
		}
	}

	// Selector context (`foo-`, `<fs>-`, `obj->`, etc.) but type/member resolution failed —
	// do not fall back to keywords and every symbol in scope.
	if member_access.has_member_access_base && member_access.access_kind != .None {
		member_resolved := false
		#partial switch member_access.access_kind {
		case .Structure:
			member_resolved = member_access.struct_type != nil
		case .Instance, .Static:
			member_resolved = member_access.class_symbol != nil
		}
		if !member_resolved {
			return items
		}
	}

	for keyword in ABAP_KEYWORDS {
		append(&items, CompletionItem{label = keyword, kind = .Keyword, detail = "keyword"})
	}

	if table == nil {
		return items
	}

	if enclosing_form := ast.find_enclosing_form(snap.ast, offset); enclosing_form != nil {
		form_name := enclosing_form.ident.name
		if form_sym, ok := table.symbols[form_name]; ok {
			if form_sym.child_scope != nil {
				for _, sym in form_sym.child_scope.symbols {
					append(&items, symbol_to_completion_item(sym))
				}
			}
		}
	}

	if method_impl := ast.find_enclosing_method_impl(snap.ast, offset); method_impl != nil {
		if class_impl := ast.find_enclosing_class_impl(snap.ast, offset); class_impl != nil &&
		   class_impl.ident != nil {
			class_name := strings.to_lower(class_impl.ident.name, context.temp_allocator)
			if class_sym, ok := table.symbols[class_name]; ok && class_sym.child_scope != nil {
				method_name := strings.to_lower(
					symbols.decl_name_from_expr(method_impl.ident),
					context.temp_allocator,
				)
				if method_sym, ok2 := class_sym.child_scope.symbols[method_name]; ok2 &&
				   method_sym.child_scope != nil {
					for _, sym in method_sym.child_scope.symbols {
						append(&items, symbol_to_completion_item(sym))
					}
				}
			}
		}
	}

	if enclosing_class := ast.find_enclosing_class_def(snap.ast, offset); enclosing_class != nil {
		class_name := enclosing_class.ident.name
		if class_sym, ok := table.symbols[class_name]; ok {
			if class_sym.child_scope != nil {
				for _, sym in class_sym.child_scope.symbols {
					append(&items, symbol_to_completion_item(sym))
				}
			}
		}
	}

	if enclosing_iface := ast.find_enclosing_interface(snap.ast, offset); enclosing_iface != nil {
		iface_name := enclosing_iface.ident.name
		if iface_sym, ok := table.symbols[iface_name]; ok {
			if iface_sym.child_scope != nil {
				for _, sym in iface_sym.child_scope.symbols {
					append(&items, symbol_to_completion_item(sym))
				}
			}
		}
	}

	if enclosing_module := ast.find_enclosing_module(snap.ast, offset); enclosing_module != nil {
		module_name := enclosing_module.ident.name
		if module_sym, ok := table.symbols[module_name]; ok {
			if module_sym.child_scope != nil {
				for _, sym in module_sym.child_scope.symbols {
					append(&items, symbol_to_completion_item(sym))
				}
			}
		}
	}

	for _, sym in table.symbols {
		append(&items, symbol_to_completion_item(sym))
	}

	return items
}

// Result of member access analysis at cursor position
Member_Access_Result :: struct {
	struct_type:  ^symbols.Type,
	class_symbol: ^symbols.Symbol,
	access_kind:  Access_Kind,
	// True when a `-` / `->` / `=>` chain has a non-empty base (e.g. `<fs>` or `var`) before the operator.
	has_member_access_base: bool,
}

find_member_access_at_cursor :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	symbol_table: ^symbols.SymbolTable = nil,
) -> Member_Access_Result {
	result := Member_Access_Result{}

	if len(snap.text) == 0 || offset < 2 {
		return result
	}

	chain_result := parse_access_chain_backwards(snap.text, offset)
	defer delete(chain_result.chain)

	result.access_kind = chain_result.access_kind
	result.has_member_access_base = len(chain_result.chain) > 0
	if len(chain_result.chain) == 0 {
		return result
	}

	#partial switch chain_result.access_kind {
	case .Static:
		result.class_symbol = resolve_class_for_static_access(
			snap,
			chain_result.chain[:],
			symbol_table,
		)
	case .Instance:
		result.class_symbol = resolve_class_for_instance_access(
			snap,
			chain_result.chain[:],
			offset,
			symbol_table,
		)
	case .Structure:
		result.struct_type = resolve_access_chain(
			snap,
			chain_result.chain[:],
			offset,
			symbol_table,
		)
	}

	return result
}

// Legacy function for backward compatibility
find_struct_type_at_cursor :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	symbol_table: ^symbols.SymbolTable = nil,
) -> ^symbols.Type {
	result := find_member_access_at_cursor(snap, offset, symbol_table)
	return result.struct_type
}

Access_Chain_Result :: struct {
	chain:       [dynamic]string,
	access_kind: Access_Kind,
}

parse_access_chain_backwards :: proc(text: string, offset: int) -> Access_Chain_Result {
	result := Access_Chain_Result {
		chain       = make([dynamic]string, context.temp_allocator),
		access_kind = .None,
	}

	if offset < 2 || offset > len(text) {
		return result
	}

	pos := offset - 1

	// Check for static access (=>), instance access (->), or structure access (-)
	if pos >= 1 && text[pos] == '>' && text[pos - 1] == '=' {
		result.access_kind = .Static
		pos -= 1 // Move past the '>'
	} else if pos >= 1 && text[pos] == '>' && text[pos - 1] == '-' {
		result.access_kind = .Instance
		pos -= 1 // Move past the '>'
	} else if text[pos] == '-' {
		result.access_kind = .Structure
	} else {
		return result
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

		// Field symbol <fs> — component name is only inside angle brackets
		if text[pos] == '>' {
			gt_pos := pos
			scan := pos - 1
			for scan >= 0 && is_ident_char(text[scan]) {
				scan -= 1
			}
			if scan >= 0 &&
			   text[scan] == '<' &&
			   scan + 1 < gt_pos {
				full_fs := text[scan:gt_pos + 1]
				ident := strings.to_lower(full_fs, context.temp_allocator)
				inject_at(&result.chain, 0, ident)
				pos = scan - 1
				for pos >= 0 && (text[pos] == ' ' || text[pos] == '\t') {
					pos -= 1
				}
				if pos >= 1 && text[pos] == '>' && text[pos - 1] == '=' {
					pos -= 1
				} else if pos >= 1 && text[pos] == '>' && text[pos - 1] == '-' {
					pos -= 1
				} else if pos < 0 || text[pos] != '-' {
					break
				}
				continue
			}
			pos = gt_pos
		}

		for pos >= 0 && is_ident_char(text[pos]) {
			pos -= 1
		}
		ident_start := pos + 1

		if ident_start >= ident_end {
			break
		}

		ident := strings.to_lower(text[ident_start:ident_end], context.temp_allocator)

		inject_at(&result.chain, 0, ident)

		for pos >= 0 && (text[pos] == ' ' || text[pos] == '\t') {
			pos -= 1
		}

		// Check for next separator (-, ->, or =>)
		if pos >= 1 && text[pos] == '>' && text[pos - 1] == '=' {
			pos -= 1 // Skip '>'
		} else if pos >= 1 && text[pos] == '>' && text[pos - 1] == '-' {
			pos -= 1 // Skip '>'
		} else if pos < 0 || text[pos] != '-' {
			break
		}
	}

	return result
}

// Symbol visible at cursor: form locals, method locals, class / interface / module scopes, then globals.
lookup_scoped_symbol :: proc(
	snap: ^cache.Snapshot,
	name: string,
	offset: int,
	symbol_table: ^symbols.SymbolTable,
) -> (
	sym: symbols.Symbol,
	ok: bool,
) {
	table := symbol_table if symbol_table != nil else snap.symbol_table
	if table == nil {
		return {}, false
	}

	if enclosing_form := ast.find_enclosing_form(snap.ast, offset); enclosing_form != nil {
		form_name := strings.to_lower(enclosing_form.ident.name, context.temp_allocator)
		if form_sym, o1 := table.symbols[form_name]; o1 && form_sym.child_scope != nil {
			if s, f := form_sym.child_scope.symbols[name]; f {
				return s, true
			}
		}
	}

	if method_impl := ast.find_enclosing_method_impl(snap.ast, offset); method_impl != nil {
		if class_impl := ast.find_enclosing_class_impl(snap.ast, offset); class_impl != nil &&
		   class_impl.ident != nil {
			class_name := strings.to_lower(class_impl.ident.name, context.temp_allocator)
			if class_sym, o1 := table.symbols[class_name]; o1 && class_sym.child_scope != nil {
				method_name := strings.to_lower(
					symbols.decl_name_from_expr(method_impl.ident),
					context.temp_allocator,
				)
				if method_sym, o2 := class_sym.child_scope.symbols[method_name]; o2 &&
				   method_sym.child_scope != nil {
					if s, f := method_sym.child_scope.symbols[name]; f {
						return s, true
					}
				}
				// Constants, attributes, etc. from the class definition (including PRIVATE
				// section) are in the class child_scope — visible in IMPLEMENTATION but not
				// found while the cursor is outside the DEFINITION node's range.
				if s, f := class_sym.child_scope.symbols[name]; f {
					return s, true
				}
			}
		}
	}

	if enclosing_class := ast.find_enclosing_class_def(snap.ast, offset); enclosing_class != nil {
		class_name := strings.to_lower(enclosing_class.ident.name, context.temp_allocator)
		if class_sym, o1 := table.symbols[class_name]; o1 && class_sym.child_scope != nil {
			if s, f := class_sym.child_scope.symbols[name]; f {
				return s, true
			}
		}
	}

	if enclosing_iface := ast.find_enclosing_interface(snap.ast, offset); enclosing_iface != nil {
		iface_name := strings.to_lower(enclosing_iface.ident.name, context.temp_allocator)
		if iface_sym, o1 := table.symbols[iface_name]; o1 && iface_sym.child_scope != nil {
			if s, f := iface_sym.child_scope.symbols[name]; f {
				return s, true
			}
		}
	}

	if enclosing_module := ast.find_enclosing_module(snap.ast, offset); enclosing_module != nil {
		module_name := strings.to_lower(enclosing_module.ident.name, context.temp_allocator)
		if module_sym, o1 := table.symbols[module_name]; o1 && module_sym.child_scope != nil {
			if s, f := module_sym.child_scope.symbols[name]; f {
				return s, true
			}
		}
	}

	if s, f := table.symbols[name]; f {
		return s, true
	}
	return {}, false
}

resolve_access_chain :: proc(
	snap: ^cache.Snapshot,
	chain: []string,
	offset: int,
	symbol_table: ^symbols.SymbolTable = nil,
) -> ^symbols.Type {
	if len(chain) == 0 {
		return nil
	}

	var_name := chain[0]
	var_type := lookup_variable_type(snap, var_name, offset, symbol_table)
	if var_type == nil {
		return nil
	}

	current_type := resolve_to_struct_type(snap, offset, var_type, symbol_table)
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

		current_type = resolve_to_struct_type(snap, offset, field_type, symbol_table)
		if current_type == nil {
			return nil
		}
	}

	return current_type
}

lookup_variable_type :: proc(
	snap: ^cache.Snapshot,
	var_name: string,
	offset: int,
	symbol_table: ^symbols.SymbolTable = nil,
) -> ^symbols.Type {
	if sym, ok := lookup_scoped_symbol(snap, var_name, offset, symbol_table); ok {
		return sym.type_info
	}
	return nil
}

expr_value_type_for_completion :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	table: ^symbols.SymbolTable,
	expr: ^ast.Expr,
) -> ^symbols.Type {
	if expr == nil || table == nil {
		return nil
	}
	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		name := strings.to_lower(e.name, context.temp_allocator)
		return lookup_variable_type(snap, name, offset, table)
	case ^ast.Selector_Expr:
		if e.op.kind != .Minus && e.op.kind != .Tilde && e.op.kind != .Arrow {
			return nil
		}
		base_ty := expr_value_type_for_completion(snap, offset, table, e.expr)
		struct_ty := structure_for_field_lookup_for_completion(snap, offset, table, base_ty)
		if struct_ty == nil {
			return nil
		}
		field_name := ast.selector_field_ident_name(e)
		if field_name == "" {
			return nil
		}
		ln := strings.to_lower(field_name, context.temp_allocator)
		for f in struct_ty.fields {
			if f.name == ln {
				return f.type_info
			}
		}
		return nil
	case ^ast.Paren_Expr:
		return expr_value_type_for_completion(snap, offset, table, e.expr)
	}
	return nil
}

unwrap_typedef_structure_for_completion :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	table: ^symbols.SymbolTable,
	start: ^symbols.Type,
) -> ^symbols.Type {
	if start == nil {
		return nil
	}
	cur := start
	for _ in 0 ..< 16 {
		if cur.kind == .Structure {
			return cur
		}
		if cur.kind != .Named {
			return nil
		}
		if sym, ok := lookup_scoped_symbol(snap, cur.name, offset, table); ok &&
		   sym.kind == .TypeDef &&
		   sym.type_info != nil {
			cur = sym.type_info
			continue
		}
		return nil
	}
	return nil
}

named_or_table_row_structure_for_completion :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	table: ^symbols.SymbolTable,
	value_ty: ^symbols.Type,
) -> ^symbols.Type {
	if value_ty == nil {
		return nil
	}
	t := value_ty
	if t.kind == .Named {
		if sym, ok := lookup_scoped_symbol(snap, t.name, offset, table); ok &&
		   sym.kind == .TypeDef &&
		   sym.type_info != nil {
			t = sym.type_info
		} else {
			return nil
		}
	}
	if t.kind == .Table && t.elem_type != nil {
		return structure_for_field_lookup_for_completion(snap, offset, table, t.elem_type)
	}
	return nil
}

structure_for_field_lookup_for_completion :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	table: ^symbols.SymbolTable,
	value_ty: ^symbols.Type,
) -> ^symbols.Type {
	if value_ty == nil {
		return nil
	}
	if value_ty.kind == .Structure {
		return value_ty
	}
	if value_ty.kind == .Inferred && value_ty.infer_source != nil {
		src_ty := expr_value_type_for_completion(snap, offset, table, value_ty.infer_source)
		return named_or_table_row_structure_for_completion(snap, offset, table, src_ty)
	}
	if value_ty.kind == .Named {
		if u := unwrap_typedef_structure_for_completion(snap, offset, table, value_ty); u != nil {
			return u
		}
		return named_or_table_row_structure_for_completion(snap, offset, table, value_ty)
	}
	if value_ty.kind == .LineOf && value_ty.target_type != nil {
		return named_or_table_row_structure_for_completion(snap, offset, table, value_ty.target_type)
	}
	return nil
}

resolve_to_struct_type :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	type_info: ^symbols.Type,
	symbol_table: ^symbols.SymbolTable = nil,
) -> ^symbols.Type {
	if type_info == nil {
		return nil
	}

	table := symbol_table if symbol_table != nil else snap.symbol_table

	#partial switch type_info.kind {
	case .Structure:
		return type_info

	case .Inferred:
		if table != nil {
			return structure_for_field_lookup_for_completion(snap, offset, table, type_info)
		}
		return nil

	case .Named:
		if table != nil {
			if sym, ok := lookup_scoped_symbol(snap, type_info.name, offset, table); ok &&
			   sym.type_info != nil {
				return resolve_to_struct_type(snap, offset, sym.type_info, table)
			}
		}
		return nil

	case .Table:
		if type_info.elem_type != nil {
			return resolve_to_struct_type(snap, offset, type_info.elem_type, table)
		}
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

		append(
			&items,
			CompletionItem {
				label = field.name,
				kind = .Field,
				detail = detail if len(detail) > 0 else nil,
			},
		)
	}

	return items
}

// Resolve class symbol for static access (class_name=>)
resolve_class_for_static_access :: proc(
	snap: ^cache.Snapshot,
	chain: []string,
	symbol_table: ^symbols.SymbolTable = nil,
) -> ^symbols.Symbol {
	if len(chain) == 0 {
		return nil
	}

	table := symbol_table if symbol_table != nil else snap.symbol_table
	if table == nil {
		return nil
	}

	// For static access, the first element should be a class name
	class_name := chain[0]
	if class_sym, found := table.symbols[class_name]; found {
		if class_sym.kind == .Class || class_sym.kind == .Interface {
			return &table.symbols[class_name]
		}
	}

	return nil
}

// Resolve class symbol for instance access (obj->)
resolve_class_for_instance_access :: proc(
	snap: ^cache.Snapshot,
	chain: []string,
	offset: int,
	symbol_table: ^symbols.SymbolTable = nil,
) -> ^symbols.Symbol {
	if len(chain) == 0 {
		return nil
	}

	table := symbol_table if symbol_table != nil else snap.symbol_table
	if table == nil {
		return nil
	}

	// Get the type of the first variable in the chain
	var_name := chain[0]
	var_type := lookup_variable_type(snap, var_name, offset, symbol_table)
	if var_type == nil {
		return nil
	}

	// Resolve to the underlying class type
	class_name := resolve_to_class_name(var_type, table)
	if class_name == "" {
		return nil
	}

	// Look up the class symbol
	if class_sym, found := table.symbols[class_name]; found {
		if class_sym.kind == .Class || class_sym.kind == .Interface {
			return &table.symbols[class_name]
		}
	}

	return nil
}

// Resolve type to underlying class name (handles REF TO, Named types)
resolve_to_class_name :: proc(type_info: ^symbols.Type, table: ^symbols.SymbolTable) -> string {
	if type_info == nil {
		return ""
	}

	#partial switch type_info.kind {
	case .Reference:
		// REF TO class_name - get the target type
		if type_info.target_type != nil {
			return resolve_to_class_name(type_info.target_type, table)
		}
	case .Named:
		// Check if this named type is a class
		if table != nil {
			if sym, found := table.symbols[type_info.name]; found {
				if sym.kind == .Class || sym.kind == .Interface {
					return type_info.name
				}
				// If it's a typedef, resolve further
				if sym.kind == .TypeDef && sym.type_info != nil {
					return resolve_to_class_name(sym.type_info, table)
				}
			}
		}
		return type_info.name
	}

	return ""
}

class_static_members_to_completion_items :: proc(
	class_sym: ^symbols.Symbol,
) -> [dynamic]CompletionItem {
	items := make([dynamic]CompletionItem, context.temp_allocator)

	if class_sym == nil || class_sym.child_scope == nil {
		return items
	}

	for _, sym in class_sym.child_scope.symbols {
		if sym.visibility != .Public {
			continue
		}
		if !sym.is_static {
			continue
		}
		if sym.kind != .Field && sym.kind != .Method {
			continue
		}

		item := symbol_to_completion_item(sym)

		if sym.kind == .Field {
			if sym.type_info != nil {
				item.detail = fmt.tprintf("CLASS-DATA %s", symbols.format_type(sym.type_info))
			} else {
				item.detail = "CLASS-DATA"
			}
		} else if sym.kind == .Method {
			item.detail = "CLASS-METHODS"
		}

		append(&items, item)
	}

	return items
}

class_instance_members_to_completion_items :: proc(
	class_sym: ^symbols.Symbol,
) -> [dynamic]CompletionItem {
	items := make([dynamic]CompletionItem, context.temp_allocator)

	if class_sym == nil || class_sym.child_scope == nil {
		return items
	}

	for _, sym in class_sym.child_scope.symbols {
		if sym.visibility != .Public {
			continue
		}
		if sym.kind != .Field && sym.kind != .Method {
			continue
		}
		item := symbol_to_completion_item(sym)
		append(&items, item)
	}

	return items
}

is_ident_char :: proc(c: u8) -> bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
}

// Access kind for member access completion
Access_Kind :: enum {
	None, // No access pattern detected
	Structure, // Structure component access (-)
	Instance, // Instance member access (->)
	Static, // Static member access (=>)
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
	case .Control:
		kind = .Variable
		detail = "CONTROLS"
	}

	return CompletionItem {
		label = sym.name,
		kind = kind,
		detail = detail if len(detail) > 0 else nil,
	}
}
