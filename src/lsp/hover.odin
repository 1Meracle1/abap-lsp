package lsp

import "../cache"
import "core:encoding/json"
import "core:fmt"
import "core:strings"

import "../lang/ast"
import "../lang/lexer"
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

	// Get the effective symbol table (merged from project if available)
	symbol_table := cache.get_effective_symbol_table(srv.storage, hover_params.textDocument.uri)
	if symbol_table == nil {
		symbol_table = snap.symbol_table
	}

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

	if member_hover, ok := lookup_class_member_hover_at_offset(snap, offset); ok {
		hover_text = member_hover
	} else if param_hover, ok := lookup_method_param_hover_at_offset(snap, offset); ok {
		hover_text = param_hover
	} else {
		#partial switch n in node.derived {
		case ^ast.Ident:
			if sym, ok := lookup_symbol_at_offset(snap, n.name, offset, symbol_table); ok {
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
			case .Field:
				if sym.visibility != .None {
					hover_text = format_class_field_signature(sym)
				} else {
					type_str := symbols.format_type(sym.type_info)
					hover_text = fmt.tprintf(
						"(var) %s: %s",
						cache.xml_encode(sym.name, context.temp_allocator),
						type_str,
					)
				}
			case .FieldSymbol:
				type_str := symbols.format_type(sym.type_info)
				hover_text = fmt.tprintf(
					"(field-symbol) %s: %s",
					cache.xml_encode(sym.name, context.temp_allocator),
					type_str,
				)
			case .Constant:
				type_str := symbols.format_type(sym.type_info)
				hover_text = fmt.tprintf("(constant) %s: %s", sym.name, type_str)
			case .Parameter:
				type_str := symbols.format_type(sym.type_info)
				hover_text = fmt.tprintf("(parameter) %s: %s", sym.name, type_str)
			case .Control:
				hover_text = fmt.tprintf("(control) %s", sym.name)
			case:
				type_str := symbols.format_type(sym.type_info)
				hover_text = fmt.tprintf(
					"(var) %s: %s",
					cache.xml_encode(sym.name, context.temp_allocator),
					type_str,
				)
			}
			} else if field_name, field_type, ok := lookup_selector_field_at_offset(
				snap,
				offset,
				symbol_table,
			); ok {
				hover_text = fmt.tprintf(
					"%s: %s",
					cache.xml_encode(field_name, context.temp_allocator),
					symbols.format_type(field_type),
				)
			} else {
				hover_text = fmt.tprintf(
					"(unknown) %s",
					cache.xml_encode(n.name, context.temp_allocator),
				)
			}

	case ^ast.Basic_Lit:
		if n.tok.kind == .Ident &&
		   strings.to_upper(n.tok.lit, context.temp_allocator) == "NULL" {
			hover_text =
				"(Open SQL keyword) NULL — SQL null value; used in IS NULL and IS NOT NULL conditions."
		}

	case ^ast.New_Expr:
		if n.is_inferred {
			hover_text = "NEW #( ) - creates instance with inferred type"
		} else if n.type_expr != nil {
			if type_ident, ok := n.type_expr.derived_expr.(^ast.Ident); ok {
				if sym, found := lookup_symbol_at_offset(
					snap,
					type_ident.name,
					offset,
					symbol_table,
				); found {
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
		method_name := get_call_method_name(n)
		if method_name != "" {
			hover_text = fmt.tprintf("(method call) %s( )", method_name)
		} else {
			hover_text = "(method call)"
		}

	case ^ast.Selector_Expr:
		if n.field != nil {
			field_name := ast.selector_field_ident_name(n)
			if field_name != "" {
				if sym, ok := lookup_symbol_at_offset(snap, field_name, offset, symbol_table); ok {
					type_str := symbols.format_type(sym.type_info)
					hover_text = fmt.tprintf("%s: %s", sym.name, type_str)
				} else if field_name, field_type, ok := lookup_selector_field_at_offset(
					snap,
					offset,
					symbol_table,
				); ok {
					hover_text = fmt.tprintf("%s: %s", field_name, symbols.format_type(field_type))
				}
			} else if field_name, field_type, ok := lookup_selector_field_at_offset(
				snap,
				offset,
				symbol_table,
			); ok {
				hover_text = fmt.tprintf("%s: %s", field_name, symbols.format_type(field_type))
			}
		}

	case ^ast.Substring_Expr:
		if n.offset != nil || n.length != nil || n.length_is_star {
			hover_text = "(substring expression) dobj+off(len)"
		} else {
			hover_text = "(substring expression) dobj(len)"
		}

	case ^ast.Named_Arg:
		if n.name != nil {
			hover_text = fmt.tprintf("(parameter) %s", n.name.name)
		}

	case ^ast.For_Expr:
		if n.var_name != nil {
			hover_text = fmt.tprintf(
				"(for expression) FOR %s IN ... - iterates over internal table",
				n.var_name.name,
			)
		} else {
			hover_text = "(for expression) FOR ... IN ... - iterates over internal table"
		}

	case ^ast.Constructor_Expr:
		keyword_upper := strings.to_upper(n.keyword.lit, context.temp_allocator)
		if n.is_inferred {
			hover_text = fmt.tprintf(
				"(%s expression) %s #( ) - constructor with inferred type",
				keyword_upper,
				keyword_upper,
			)
		} else if n.type_expr != nil {
			if type_ident, ok := n.type_expr.derived_expr.(^ast.Ident); ok {
				hover_text = fmt.tprintf(
					"(%s expression) %s %s( ) - constructor",
					keyword_upper,
					keyword_upper,
					type_ident.name,
				)
			} else {
				hover_text = fmt.tprintf(
					"(%s expression) %s type( ) - constructor",
					keyword_upper,
					keyword_upper,
				)
			}
		} else {
			hover_text = fmt.tprintf("(%s expression) constructor", keyword_upper)
		}

	case ^ast.String_Template_Expr:
		// Check if there are any formatting options in embedded expressions
		has_format_options := false
		for part in n.parts {
			if part.is_expr && len(part.format_options) > 0 {
				has_format_options = true
				break
			}
		}
		if has_format_options {
			hover_text = "(string template) |...{ expr FORMAT = VALUE }...|"
		} else {
			hover_text = "(string template) |...|"
		}

	case ^ast.Binary_Expr:
		hover_text = format_binary_expr_hover(n)

	case ^ast.Paren_Expr:
		hover_text = "(parenthesized expression)"

	case ^ast.Message_Stmt:
		hover_text = "(statement) MESSAGE - displays a message to the user"

	case ^ast.Write_Stmt:
		hover_text = "(statement) WRITE - writes data to the current output list"

	case ^ast.Unassign_Stmt:
		hover_text = "(statement) UNASSIGN - resets a field symbol (removes memory area assignment)"

	case ^ast.Assign_Field_Symbol_Stmt:
		if n.is_component {
			hover_text = "(statement) ASSIGN COMPONENT ... OF STRUCTURE ... TO - assigns a structure component to a field symbol"
		} else if n.is_table_field {
			hover_text = "(statement) ASSIGN TABLE FIELD - dynamically assigns a table work area to a field symbol"
		} else if n.is_dynamic {
			hover_text = "(statement) ASSIGN ( ... ) TO - dynamically assigns a data object to a field symbol"
		} else if n.offset != nil || n.length != nil || n.length_is_star {
			hover_text = "(statement) ASSIGN dobj+off(len) TO - assigns a subfield to a field symbol"
		} else {
			hover_text = "(statement) ASSIGN ... TO - assigns a data object to a field symbol"
		}

	case ^ast.Insert_Stmt:
		switch n.kind {
		case .Into_Table:
			hover_text = "(statement) INSERT ... INTO TABLE - inserts data into an internal table"
		case .Into_Itab:
			hover_text = "(statement) INSERT ... INTO ... [INDEX] - inserts a line into an internal table (optional position)"
		case .Initial_Line_Into_Itab:
			hover_text =
				"(statement) INSERT INITIAL LINE INTO [TABLE] ... [INDEX] [ASSIGNING] - inserts an initial line into an internal table"
		case .Into_Db:
			hover_text = "(statement) INSERT INTO ... VALUES - inserts data into a database table"
		case .From_Wa:
			hover_text = "(statement) INSERT ... FROM - inserts data from a work area into a database table"
		case .From_Table:
			hover_text = "(statement) INSERT ... FROM TABLE - inserts data from an internal table into a database table"
		case .Lines_Of_Into_Table:
			hover_text =
				"(statement) INSERT LINES OF ... INTO TABLE - inserts multiple lines from one internal table into another"
		case .Lines_Of_Into_Itab:
			hover_text =
				"(statement) INSERT LINES OF ... INTO ... [INDEX] - inserts multiple lines from an internal table (optional position)"
		}

	case ^ast.Modify_From_Stmt:
		hover_text = "(statement) MODIFY ... FROM - changes or adds a row in a database table from a work area"

	case ^ast.Append_Stmt:
		switch n.kind {
		case .Simple:
			hover_text = "(statement) APPEND ... TO - appends a line to an internal table"
		case .Initial_Line:
			hover_text = "(statement) APPEND INITIAL LINE TO - appends an initial line to an internal table"
		case .Lines_Of:
			hover_text =
				"(statement) APPEND LINES OF ... [FROM ... TO ...] TO - appends lines (or a line range) from one internal table to another"
		}

	case ^ast.Delete_Stmt:
		switch n.kind {
		case .Where:
			hover_text = "(statement) DELETE ... WHERE - deletes lines from an internal table that satisfy a condition"
		case .Index:
			hover_text = "(statement) DELETE ... INDEX - deletes a line from an internal table by index"
		case .Adjacent_Duplicates:
			hover_text = "(statement) DELETE ADJACENT DUPLICATES - deletes adjacent duplicate entries"
		case .Table_From:
			hover_text = "(statement) DELETE TABLE ... FROM - deletes a table line matching a work area"
		case .Db_From_Table:
			hover_text =
				"(statement) DELETE ... FROM TABLE - deletes database table rows matching lines of an internal table"
		}

	case ^ast.Split_Stmt:
		if n.table_target != nil {
			hover_text = "(statement) SPLIT ... INTO TABLE - splits a data object into an internal table"
		} else {
			hover_text = "(statement) SPLIT ... INTO - splits a data object into multiple target fields"
		}

	case ^ast.Concatenate_Stmt:
		if n.separator != nil && n.respecting_blanks {
			hover_text = "(statement) CONCATENATE ... INTO ... RESPECTING BLANKS SEPARATED BY - join with separator, keep source spaces"
		} else if n.respecting_blanks {
			hover_text = "(statement) CONCATENATE ... INTO ... RESPECTING BLANKS - joins values preserving trailing spaces in sources"
		} else if n.separator != nil {
			hover_text = "(statement) CONCATENATE ... INTO ... SEPARATED BY - joins values with a separator"
		} else {
			hover_text = "(statement) CONCATENATE ... INTO - joins values into a target field"
		}

	case ^ast.Replace_Stmt:
		switch n.scope {
		case .Simple:
			hover_text = "(statement) REPLACE ... WITH ... [INTO|IN ... WITH] - replaces text in a string"
		case .All_Occurrences:
			if n.is_regex {
				hover_text = "(statement) REPLACE ALL OCCURRENCES OF REGEX - replaces all regex matches"
			} else {
				hover_text = "(statement) REPLACE ALL OCCURRENCES OF - replaces all occurrences in a string"
			}
		case .First_Occurrence:
			if n.is_regex {
				hover_text = "(statement) REPLACE FIRST OCCURRENCE OF REGEX - replaces the first regex match"
			} else {
				hover_text = "(statement) REPLACE FIRST OCCURRENCE OF - replaces the first occurrence in a string"
			}
		}

	case ^ast.Field_Symbol_Decl:
		if n.ident != nil {
			hover_text = fmt.tprintf("(field-symbol declaration) `%s`", n.ident.name)
		}

	case ^ast.Field_Symbol_Chain_Decl:
		hover_text = "(chained FIELD-SYMBOLS declaration)"

	case ^ast.Controls_Decl:
		if n.ident != nil {
			kind_str := n.kind == .Tableview ? "TABLEVIEW" : "TABSTRIP"
			hover_text = fmt.tprintf("(control declaration) %s TYPE %s", n.ident.name, kind_str)
		}

	case ^ast.Controls_Chain_Decl:
		hover_text = "(chained CONTROLS declaration)"

	case ^ast.Data_Struct_Decl:
		if n.ident != nil {
			hover_text = fmt.tprintf("(data structure) %s", n.ident.name)
		}

	case ^ast.Do_Stmt:
		if n.times != nil {
			hover_text = "(statement) DO n TIMES ... ENDDO — counted loop"
		} else {
			hover_text = "(statement) DO ... ENDDO — loop until EXIT or error"
		}

	case ^ast.Continue_Stmt:
		hover_text = "(statement) CONTINUE — skip to next iteration of the current loop"

	case ^ast.Exit_Stmt:
		hover_text = "(statement) EXIT — leave the current loop (or processing block)"

	case ^ast.Loop_Stmt:
		switch n.kind {
		case .At:
			if n.transporting_no_fields {
				hover_text = "(statement) LOOP AT ... TRANSPORTING NO FIELDS - iterates over internal table checking conditions"
			} else if n.group_by != nil {
				hover_text = "(statement) LOOP AT ... GROUP BY - iterates over internal table with grouping"
			} else if n.assigning_target != nil {
				hover_text = "(statement) LOOP AT ... ASSIGNING - iterates assigning each line to a field symbol"
			} else if n.into_target != nil {
				hover_text = "(statement) LOOP AT ... INTO - iterates copying each line into a work area"
			} else {
				hover_text = "(statement) LOOP AT - iterates over an internal table"
			}
		case .At_Screen:
			hover_text = "(statement) LOOP AT SCREEN - iterates over screen elements for modification"
		case .At_Group:
			hover_text = "(statement) LOOP AT GROUP - iterates over members of a group created by GROUP BY"
		}

	case ^ast.Loop_At_Control_Stmt:
		switch n.kind {
		case .First:
			hover_text = "(statement) AT FIRST ... ENDAT - first iteration of LOOP AT"
		case .Last:
			hover_text = "(statement) AT LAST ... ENDAT - last iteration of LOOP AT"
		case .New:
			hover_text = "(statement) AT NEW ... ENDAT - start of group when control field changes"
		case .End_Of:
			hover_text = "(statement) AT END OF ... ENDAT - end of group for control field"
		}

	case ^ast.Read_Table_Stmt:
		switch n.kind {
		case .With_Key:
			if n.transporting_no_fields {
				hover_text = "(statement) READ TABLE ... WITH KEY ... TRANSPORTING NO FIELDS - checks if entry exists in table"
			} else if n.assigning_target != nil {
				hover_text = "(statement) READ TABLE ... WITH KEY ... ASSIGNING - reads entry by key and assigns to field symbol"
			} else if n.into_target != nil {
				hover_text = "(statement) READ TABLE ... WITH KEY ... INTO - reads entry by key into work area"
			} else {
				hover_text = "(statement) READ TABLE ... WITH KEY - reads entry from internal table by key"
			}
		case .With_Table_Key:
			if n.transporting_no_fields {
				hover_text = "(statement) READ TABLE ... WITH TABLE KEY ... TRANSPORTING NO FIELDS - checks if entry exists in table"
			} else if n.assigning_target != nil {
				hover_text = "(statement) READ TABLE ... WITH TABLE KEY ... ASSIGNING - reads entry by key and assigns to field symbol"
			} else if n.into_target != nil {
				hover_text = "(statement) READ TABLE ... WITH TABLE KEY ... INTO - reads entry by key into work area"
			} else {
				hover_text = "(statement) READ TABLE ... WITH TABLE KEY - reads entry from internal table by key"
			}
		case .Index:
			if n.assigning_target != nil {
				hover_text = "(statement) READ TABLE ... INDEX ... ASSIGNING - reads entry by index and assigns to field symbol"
			} else if n.into_target != nil {
				hover_text = "(statement) READ TABLE ... INDEX ... INTO - reads entry by index into work area"
			} else {
				hover_text = "(statement) READ TABLE ... INDEX - reads entry from internal table by index"
			}
		}

	case ^ast.Describe_Table_Stmt:
		hover_text = "(statement) DESCRIBE TABLE ... LINES - gets the number of lines in an internal table"

	case ^ast.Open_Cursor_Stmt:
		hover_text = "(statement) OPEN CURSOR ... FOR SELECT — opens a database cursor over an Open SQL result set"

	case ^ast.Fetch_Cursor_Stmt:
		hover_text = "(statement) FETCH NEXT CURSOR ... INTO ... — reads the next rows from an open database cursor"

	case ^ast.Select_Stmt:
		if n.is_single {
			if n.into_kind == .Table {
				hover_text = "(statement) SELECT SINGLE ... INTO TABLE - selects a single row into a table (unusual)"
			} else {
				hover_text = "(statement) SELECT SINGLE - selects a single row from a database table"
			}
		} else if n.into_kind == .Table {
			if len(n.joins) > 0 {
				hover_text = "(statement) SELECT ... JOIN ... INTO TABLE - joins tables and selects into an internal table"
			} else if n.for_all_entries != nil {
				hover_text = "(statement) SELECT ... FOR ALL ENTRIES - selects rows based on entries in an internal table"
			} else if len(n.group_by) > 0 {
				hover_text = "(statement) SELECT ... GROUP BY ... INTO TABLE - groups and aggregates data into an internal table"
			} else {
				hover_text = "(statement) SELECT ... INTO TABLE - selects multiple rows into an internal table"
			}
		} else if len(n.body) > 0 {
			hover_text = "(statement) SELECT ... ENDSELECT - iterates over selected database rows"
		} else {
			hover_text = "(statement) SELECT - reads data from a database table"
		}

	case ^ast.Table_Type:
		table_kind_str := ""
		switch n.kind {
		case .Standard:
			table_kind_str = "STANDARD TABLE"
		case .Sorted:
			table_kind_str = "SORTED TABLE"
		case .Hashed:
			table_kind_str = "HASHED TABLE"
		case .Any:
			table_kind_str = "TABLE"
		}
		hover_text = fmt.tprintf("(type) %s OF ...", table_kind_str)

	case ^ast.Ref_Type:
		hover_text = "(type) REF TO - reference type"

	case ^ast.Line_Type:
		hover_text = "(type) LINE OF - line type of internal table"

	case ^ast.Range_Type:
		hover_text = "(type) RANGE OF - selection table / ranges type"

	case ^ast.Call_Function_Stmt:
		if n.func_name != nil {
			if lit, ok := n.func_name.derived_expr.(^ast.Basic_Lit); ok {
				hover_text = fmt.tprintf(
					"(statement) CALL FUNCTION %s - calls a function module",
					lit.tok.lit,
				)
			} else {
				hover_text = "(statement) CALL FUNCTION - calls a function module"
			}
		} else {
			hover_text = "(statement) CALL FUNCTION - calls a function module"
		}

	case ^ast.Call_Badi_Stmt:
		hover_text = "(statement) CALL BADI - calls a BAdI implementation method"

	case ^ast.Call_System_Stmt:
		if n.module != nil {
			if lit, ok := n.module.derived_expr.(^ast.Basic_Lit); ok {
				hover_text = fmt.tprintf(
					"(statement) CALL %s - system/kernel module",
					lit.tok.lit,
				)
			} else {
				hover_text = "(statement) CALL - system/kernel module"
			}
		} else {
			hover_text = "(statement) CALL - system/kernel module"
		}

	case ^ast.Call_Transaction_Stmt:
		hover_text = "(statement) CALL TRANSACTION - runs a transaction (dialog or batch via USING bdcdata / MODE)"

	case ^ast.Call_Transformation_Stmt:
		hover_text = "(statement) CALL TRANSFORMATION - runs an XSLT or simple transformation on/XML data"

	case ^ast.Call_Function_Param:
		if n.name != nil {
			param_kind_str := ""
			switch n.kind {
			case .Exporting:
				param_kind_str = "EXPORTING"
			case .Importing:
				param_kind_str = "IMPORTING"
			case .Tables:
				param_kind_str = "TABLES"
			case .Changing:
				param_kind_str = "CHANGING"
			case .Receiving:
				param_kind_str = "RECEIVING"
			case .Exceptions:
				param_kind_str = "EXCEPTIONS"
			}
			if n.kind == .Exceptions && n.is_others {
				hover_text = fmt.tprintf(
					"(keyword) %s OTHERS — catch-all exception; `sy-subrc` is set to the given value when no listed exception is raised",
					param_kind_str,
				)
			} else {
				hover_text = fmt.tprintf("(parameter) %s %s", param_kind_str, n.name.name)
			}
		}

		case:
		// For other nodes, maybe just show the type of node?
		// or nothing
		}
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

lookup_class_member_hover_at_offset :: proc(snap: ^cache.Snapshot, offset: int) -> (string, bool) {
	if snap == nil {
		return "", false
	}

	class_def := ast.find_enclosing_class_def(snap.ast, offset)
	if class_def == nil {
		return "", false
	}

	for section in class_def.sections {
		for method_stmt in section.methods {
			if hover_text, ok := class_method_hover_in_stmt(method_stmt, section.access, snap.text, offset); ok {
				return hover_text, true
			}
		}

		for data_stmt in section.data {
			if hover_text, ok := class_data_hover_in_stmt(data_stmt, section.access, snap.text, offset); ok {
				return hover_text, true
			}
		}
	}

	return "", false
}

class_method_hover_in_stmt :: proc(
	stmt: ^ast.Stmt,
	access: ast.Access_Modifier,
	text: string,
	offset: int,
) -> (
	string,
	bool,
) {
	if stmt == nil {
		return "", false
	}

	#partial switch decl in stmt.derived_stmt {
	case ^ast.Method_Decl:
		if decl.ident != nil && range_contains_offset(decl.ident.range, offset) {
			return format_class_method_decl_signature(decl, access, text), true
		}
	case ^ast.Method_Chain_Decl:
		for child in decl.decls {
			if child != nil && child.ident != nil && range_contains_offset(child.ident.range, offset) {
				return format_class_method_decl_signature(child, access, text), true
			}
		}
	}

	return "", false
}

class_data_hover_in_stmt :: proc(
	stmt: ^ast.Stmt,
	access: ast.Access_Modifier,
	text: string,
	offset: int,
) -> (
	string,
	bool,
) {
	if stmt == nil {
		return "", false
	}

	#partial switch decl in stmt.derived_stmt {
	case ^ast.Attr_Decl:
		if decl.ident != nil && range_contains_offset(decl.ident.range, offset) {
			return format_class_attr_decl_signature(decl, access, text, stmt.range), true
		}
	case ^ast.Data_Typed_Chain_Decl:
		is_static := class_data_stmt_is_static(stmt, text)
		for part in decl.parts {
			if part == nil {
				continue
			}
			#partial switch child in part.derived_stmt {
			case ^ast.Data_Typed_Decl:
				child_ident, ok := child.ident.derived_expr.(^ast.Ident)
				if ok && range_contains_offset(child_ident.range, offset) {
					return format_class_data_chain_decl_signature(
						child_ident,
						child.typed,
						access,
						is_static,
						text,
					),
					true
				}
			case ^ast.Data_Struct_Decl:
				if child.ident != nil && range_contains_offset(child.ident.range, offset) {
					return fmt.tprintf("(data structure) %s", child.ident.name), true
				}
				for comp in child.components {
					td, comp_typed := comp.derived_stmt.(^ast.Data_Typed_Decl)
					if !comp_typed {
						continue
					}
					if td.ident == nil {
						continue
					}
					cident, iok := td.ident.derived_expr.(^ast.Ident)
					if iok && range_contains_offset(cident.range, offset) {
						return format_class_data_chain_decl_signature(
							cident,
							td.typed,
							access,
							is_static,
							text,
						),
						true
					}
				}
			}
		}
	}

	return "", false
}

lookup_method_param_hover_at_offset :: proc(snap: ^cache.Snapshot, offset: int) -> (string, bool) {
	if snap == nil {
		return "", false
	}

	if class_def := ast.find_enclosing_class_def(snap.ast, offset); class_def != nil {
		for section in class_def.sections {
			for method_stmt in section.methods {
				if hover_text, ok := method_param_hover_in_stmt(method_stmt, snap.text, offset); ok {
					return hover_text, true
				}
			}
		}
	}

	if iface := ast.find_enclosing_interface(snap.ast, offset); iface != nil {
		for method_stmt in iface.methods {
			if hover_text, ok := method_param_hover_in_stmt(method_stmt, snap.text, offset); ok {
				return hover_text, true
			}
		}
	}

	return "", false
}

method_param_hover_in_stmt :: proc(stmt: ^ast.Stmt, text: string, offset: int) -> (string, bool) {
	if stmt == nil {
		return "", false
	}

	#partial switch decl in stmt.derived_stmt {
	case ^ast.Method_Decl:
		for param in decl.params {
			if param != nil && param.ident != nil && range_contains_offset(param.ident.range, offset) {
				return format_method_param_signature(param, text), true
			}
		}
	case ^ast.Method_Chain_Decl:
		for child in decl.decls {
			if child == nil {
				continue
			}
			for param in child.params {
				if param != nil && param.ident != nil && range_contains_offset(param.ident.range, offset) {
					return format_method_param_signature(param, text), true
				}
			}
		}
	}

	return "", false
}

range_contains_offset :: proc(range: lexer.TextRange, offset: int) -> bool {
	return offset >= range.start && offset <= range.end
}

slice_range_text :: proc(text: string, range: lexer.TextRange) -> string {
	start := range.start
	if start < 0 {
		start = 0
	}
	if start > len(text) {
		start = len(text)
	}

	end := range.end
	if end < start {
		end = start
	}
	if end > len(text) {
		end = len(text)
	}

	return text[start:end]
}

slice_statement_text_from_start :: proc(text: string, start: int) -> string {
	stmt_start := start
	if stmt_start < 0 {
		stmt_start = 0
	}
	if stmt_start >= len(text) {
		return ""
	}

	end := stmt_start
	for end < len(text) {
		ch := text[end]
		end += 1
		if ch == '.' {
			break
		}
	}

	return strings.trim_space(text[stmt_start:end])
}

method_param_kind_to_string :: proc(kind: ast.Method_Param_Kind) -> string {
	switch kind {
	case .Importing:
		return "IMPORTING"
	case .Exporting:
		return "EXPORTING"
	case .Changing:
		return "CHANGING"
	case .Returning:
		return "RETURNING"
	}
	return "PARAMETER"
}

method_param_has_ref_marker :: proc(text: string, param: ^ast.Method_Param) -> bool {
	if param == nil || param.ident == nil {
		return false
	}
	pos := param.ident.range.start - 1
	if pos < 0 || pos >= len(text) {
		return false
	}
	return text[pos] == '!'
}

class_data_stmt_is_static :: proc(stmt: ^ast.Stmt, text: string) -> bool {
	if stmt == nil {
		return false
	}

	stmt_text := slice_range_text(text, stmt.range)
	stmt_upper := strings.to_upper(stmt_text, context.temp_allocator)
	return strings.has_prefix(stmt_upper, "CLASS-DATA")
}

access_modifier_to_string :: proc(access: ast.Access_Modifier) -> string {
	switch access {
	case .Public:
		return "PUBLIC"
	case .Protected:
		return "PROTECTED"
	case .Private:
		return "PRIVATE"
	}
	return ""
}

visibility_to_string :: proc(visibility: symbols.Visibility) -> string {
	switch visibility {
	case .None:
		return ""
	case .Public:
		return "PUBLIC"
	case .Protected:
		return "PROTECTED"
	case .Private:
		return "PRIVATE"
	}
	return ""
}

write_member_section_header :: proc(
	b: ^strings.Builder,
	section_name: string,
) {
	if section_name == "" {
		return
	}

	strings.write_string(b, section_name)
	strings.write_string(b, " SECTION.\n")
}

format_class_attr_decl_signature :: proc(
	attr: ^ast.Attr_Decl,
	access: ast.Access_Modifier,
	text: string,
	stmt_range: lexer.TextRange,
) -> string {
	if attr == nil || attr.ident == nil {
		return ""
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, "```abap\n")
	write_member_section_header(&b, access_modifier_to_string(access))

	if attr.is_class {
		strings.write_string(&b, "CLASS-DATA ")
	} else {
		strings.write_string(&b, "DATA ")
	}
	strings.write_string(&b, attr.ident.name)
	if attr.typed != nil {
		strings.write_string(&b, " TYPE ")
		strings.write_string(&b, slice_range_text(text, attr.typed.range))
	}
	attr_text := strings.to_upper(slice_statement_text_from_start(text, stmt_range.start), context.temp_allocator)
	if attr.is_read_only || strings.contains(attr_text, "READ-ONLY") {
		strings.write_string(&b, " READ-ONLY")
	}
	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

format_method_param_signature :: proc(param: ^ast.Method_Param, text: string) -> string {
	if param == nil || param.ident == nil {
		return ""
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, "```abap\n")
	strings.write_string(&b, method_param_kind_to_string(param.kind))
	strings.write_string(&b, " ")

	if param.kind == .Returning {
		strings.write_string(&b, "VALUE(")
		strings.write_string(&b, param.ident.name)
		strings.write_string(&b, ")")
	} else {
		if method_param_has_ref_marker(text, param) {
			strings.write_string(&b, "!")
		}
		strings.write_string(&b, param.ident.name)
	}

	if param.typed != nil {
		strings.write_string(&b, " TYPE ")
		strings.write_string(&b, slice_range_text(text, param.typed.range))
	}
	if param.likes != nil {
		strings.write_string(&b, " LIKE ")
		strings.write_string(&b, slice_range_text(text, param.likes.range))
	}
	if param.optional {
		strings.write_string(&b, " OPTIONAL")
	}
	if param.default != nil {
		strings.write_string(&b, " DEFAULT ")
		strings.write_string(&b, slice_range_text(text, param.default.range))
	}

	strings.write_string(&b, "\n```")
	return strings.to_string(b)
}

format_class_data_chain_decl_signature :: proc(
	ident: ^ast.Ident,
	typed: ^ast.Expr,
	access: ast.Access_Modifier,
	is_static: bool,
	text: string,
) -> string {
	if ident == nil {
		return ""
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, "```abap\n")
	write_member_section_header(&b, access_modifier_to_string(access))

	if is_static {
		strings.write_string(&b, "CLASS-DATA ")
	} else {
		strings.write_string(&b, "DATA ")
	}
	strings.write_string(&b, ident.name)
	if typed != nil {
		strings.write_string(&b, " TYPE ")
		strings.write_string(&b, slice_range_text(text, typed.range))
	}
	strings.write_string(&b, "\n```")

	return strings.to_string(b)
}

append_method_param_group :: proc(
	b: ^strings.Builder,
	text: string,
	label: string,
	params: [dynamic]^ast.Method_Param,
	kind: ast.Method_Param_Kind,
) {
	group_count := 0
	for param in params {
		if param != nil && param.kind == kind {
			group_count += 1
		}
	}
	if group_count == 0 {
		return
	}

	strings.write_string(b, "\n  ")
	strings.write_string(b, label)
	for param in params {
		if param == nil || param.kind != kind || param.ident == nil {
			continue
		}
		strings.write_string(b, "\n    ")
		if kind == .Returning {
			strings.write_string(b, "VALUE(")
			strings.write_string(b, param.ident.name)
			strings.write_string(b, ")")
		} else {
			strings.write_string(b, param.ident.name)
		}
		if param.typed != nil {
			strings.write_string(b, " TYPE ")
			strings.write_string(b, slice_range_text(text, param.typed.range))
		}
		if param.likes != nil {
			strings.write_string(b, " LIKE ")
			strings.write_string(b, slice_range_text(text, param.likes.range))
		}
		if param.optional {
			strings.write_string(b, " OPTIONAL")
		}
		if param.default != nil {
			strings.write_string(b, " DEFAULT ")
			strings.write_string(b, slice_range_text(text, param.default.range))
		}
	}
}

format_class_method_decl_signature :: proc(
	method: ^ast.Method_Decl,
	access: ast.Access_Modifier,
	text: string,
) -> string {
	if method == nil || method.ident == nil {
		return ""
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, "```abap\n")
	write_member_section_header(&b, access_modifier_to_string(access))

	method_text := slice_statement_text_from_start(text, method.range.start)
	method_text_upper := strings.to_upper(method_text, context.temp_allocator)
	if .Class in method.flags &&
	   !strings.has_prefix(method_text_upper, "CLASS-METHODS") &&
	   strings.has_prefix(method_text_upper, "METHODS") {
		method_text = strings.concatenate({"CLASS-", method_text}, context.temp_allocator)
	}
	if method_text != "" {
		strings.write_string(&b, method_text)
	} else {
		if .Class in method.flags {
			strings.write_string(&b, "CLASS-METHODS ")
		} else {
			strings.write_string(&b, "METHODS ")
		}
		strings.write_string(&b, method.ident.name)
		if .Abstract in method.flags {
			strings.write_string(&b, " ABSTRACT")
		}
		if .Final in method.flags {
			strings.write_string(&b, " FINAL")
		}
		if .Redefinition in method.flags {
			strings.write_string(&b, " REDEFINITION")
		}
		if .Testing in method.flags {
			strings.write_string(&b, " FOR TESTING")
		}
	}

	strings.write_string(&b, "\n```")
	return strings.to_string(b)
}

format_class_field_signature :: proc(sym: symbols.Symbol) -> string {
	if sym.kind != .Field {
		return sym.name
	}

	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, "```abap\n")
	write_member_section_header(&b, visibility_to_string(sym.visibility))

	if sym.is_static {
		strings.write_string(&b, "CLASS-DATA ")
	} else {
		strings.write_string(&b, "DATA ")
	}
	strings.write_string(&b, sym.name)
	if sym.type_info != nil && sym.type_info.kind != .Unknown {
		strings.write_string(&b, " TYPE ")
		strings.write_string(&b, symbols.format_type(sym.type_info))
	}

	strings.write_string(&b, "\n```")
	return strings.to_string(b)
}

lookup_selector_field_at_offset :: proc(
	snap: ^cache.Snapshot,
	offset: int,
	symbol_table: ^symbols.SymbolTable = nil,
) -> (
	string,
	^symbols.Type,
	bool,
) {
	chain := parse_selector_chain_at_offset(snap.text, offset)
	if len(chain) < 2 {
		return "", nil, false
	}

	current_type := lookup_variable_type(snap, chain[0], offset, symbol_table)
	if current_type == nil {
		return "", nil, false
	}
	current_type = resolve_to_struct_type(snap, current_type, symbol_table)
	if current_type == nil {
		return "", nil, false
	}

	for i := 1; i < len(chain); i += 1 {
		field_name := chain[i]
		field_type: ^symbols.Type = nil

		for field in current_type.fields {
			if strings.to_lower(field.name, context.temp_allocator) == field_name {
				field_type = field.type_info
				break
			}
		}

		if field_type == nil {
			return "", nil, false
		}
		if i == len(chain)-1 {
			return field_name, field_type, true
		}

		current_type = resolve_to_struct_type(snap, field_type, symbol_table)
		if current_type == nil {
			return "", nil, false
		}
	}

	return "", nil, false
}

parse_selector_chain_at_offset :: proc(text: string, offset: int) -> [dynamic]string {
	chain := make([dynamic]string, context.temp_allocator)

	if offset < 0 || offset > len(text) || len(text) == 0 {
		return chain
	}

	pos := offset
	if pos < len(text) && is_ident_char(text[pos]) {
		// Keep current position when cursor is on an identifier character.
	} else if pos > 0 && is_ident_char(text[pos-1]) {
		pos -= 1
	} else {
		return chain
	}

	ident_start := pos
	for ident_start > 0 && is_ident_char(text[ident_start-1]) {
		ident_start -= 1
	}

	ident_end := pos + 1
	for ident_end < len(text) && is_ident_char(text[ident_end]) {
		ident_end += 1
	}

	append(&chain, strings.to_lower(text[ident_start:ident_end], context.temp_allocator))

	pos = ident_start - 1

	for pos >= 0 {
		for pos >= 0 && (text[pos] == ' ' || text[pos] == '\t') {
			pos -= 1
		}
		if pos < 0 || text[pos] != '-' {
			break
		}
		if pos + 1 < len(text) && text[pos+1] == '>' {
			break
		}

		pos -= 1
		for pos >= 0 && (text[pos] == ' ' || text[pos] == '\t') {
			pos -= 1
		}
		if pos < 0 || !is_ident_char(text[pos]) {
			break
		}

		ident_end = pos + 1
		for pos >= 0 && is_ident_char(text[pos]) {
			pos -= 1
		}
		ident_start = pos + 1

		inject_at(
			&chain,
			0,
			strings.to_lower(text[ident_start:ident_end], context.temp_allocator),
		)
	}

	return chain
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
	symbol_table: ^symbols.SymbolTable = nil,
) -> (
	symbols.Symbol,
	bool,
) {
	// Use provided symbol table or fall back to snapshot's own table
	table := symbol_table if symbol_table != nil else snap.symbol_table
	if table == nil {
		return {}, false
	}

	if enclosing_form := ast.find_enclosing_form(snap.ast, offset); enclosing_form != nil {
		form_name := enclosing_form.ident.name
		if form_sym, ok := lookup_symbol_in_scope(table, form_name); ok {
			// Look up in the form's local scope first
			if form_sym.child_scope != nil {
				if sym, found := lookup_symbol_in_scope(form_sym.child_scope, name); found {
					return sym, true
				}
			}
		}
	}

	// CLASS ... IMPLEMENTATION method bodies: parameters and locals sit on the method
	// symbol's child scope; class attributes and components live on the class scope.
	if method_impl := ast.find_enclosing_method_impl(snap.ast, offset); method_impl != nil {
		if class_impl := ast.find_enclosing_class_impl(snap.ast, offset); class_impl != nil &&
		   class_impl.ident != nil {
			class_name := class_impl.ident.name
			if class_sym, ok := lookup_symbol_in_scope(table, class_name); ok &&
			   class_sym.child_scope != nil {
				method_key := strings.to_lower(
					symbols.decl_name_from_expr(method_impl.ident),
					context.temp_allocator,
				)
				if method_key != "" {
					if method_sym, mok := class_sym.child_scope.symbols[method_key]; mok &&
					   method_sym.child_scope != nil {
						if sym, found := lookup_symbol_in_scope(method_sym.child_scope, name); found {
							return sym, true
						}
					}
				}
				if sym, found := lookup_symbol_in_scope(class_sym.child_scope, name); found {
					return sym, true
				}
			}
		}
	}

	if enclosing_class := ast.find_enclosing_class_def(snap.ast, offset); enclosing_class != nil {
		class_name := enclosing_class.ident.name
		if class_sym, ok := lookup_symbol_in_scope(table, class_name); ok {
			if class_sym.child_scope != nil {
				if sym, found := lookup_symbol_in_scope(class_sym.child_scope, name); found {
					return sym, true
				}
			}
		}
	}

	if enclosing_iface := ast.find_enclosing_interface(snap.ast, offset); enclosing_iface != nil {
		iface_name := enclosing_iface.ident.name
		if iface_sym, ok := lookup_symbol_in_scope(table, iface_name); ok {
			if iface_sym.child_scope != nil {
				if sym, found := lookup_symbol_in_scope(iface_sym.child_scope, name); found {
					return sym, true
				}
			}
		}
	}

	if enclosing_module := ast.find_enclosing_module(snap.ast, offset); enclosing_module != nil {
		module_name := enclosing_module.ident.name
		if module_sym, ok := lookup_symbol_in_scope(table, module_name); ok {
			if module_sym.child_scope != nil {
				if sym, found := lookup_symbol_in_scope(module_sym.child_scope, name); found {
					return sym, true
				}
			}
		}
	}

	return lookup_symbol_in_scope(table, name)
}

lookup_symbol_in_scope :: proc(table: ^symbols.SymbolTable, name: string) -> (symbols.Symbol, bool) {
	if table == nil {
		return {}, false
	}

	key := strings.to_lower(name, context.temp_allocator)
	return table.symbols[key]
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
	write_member_section_header(&b, visibility_to_string(sym.visibility))
	if sym.is_static {
		strings.write_string(&b, "CLASS-METHODS ")
	} else {
		strings.write_string(&b, "METHODS ")
	}
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
		return ast.selector_field_ident_name(e)
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
