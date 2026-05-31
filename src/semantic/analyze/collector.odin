package abap_frontend_semantic_analyze

import "src:ast"
import "src:parser"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Collector :: struct {
	root:                                   ^ast.File,
	allocator:                              mem.Allocator,
	unit:                                   ^Unit_Analysis,
	forward_type_symbols:                   map[Symbol_Id]bool,
	current_scope:                          Scope_Id,
	loop_source_stack:                      [dynamic]Field_Access,
	structured_groups:                      [dynamic]Structured_Group_Frame,
}

Structured_Group_Frame :: struct {
	name:   string,
	scope:  Scope_Id,
	symbol: Symbol_Id,
	fields: [dynamic]Structure_Field_Data,
}

Decl_Info :: struct {
	kind:            ast.Decl_Clause_Kind,
	flags:           ast.Decl_Clause_Flags,
	depth:           int,
	name:            string,
	range:           tokenizer.Range,
	paren_length:    ^ast.Paren_Length_Clause,
	length_clauses:  []ast.Length_Clause,
	type_clause:     ^ast.Data_Type_Clause,
	value_clause:    ^ast.Value_Clause,
	default_clause:  ^ast.Default_Clause,
	occurs:          ^ast.Expr,
	include_ref:     ^ast.Expr,
	as_name:         string,
	renaming_suffix: string,
	read_only:       bool,
	checkbox_type:   bool,
}

collect_unit :: proc(
	unit_id: Unit_Id,
	uri, source: string,
	parsed: parser.Parsed_File,
	allocator: mem.Allocator,
	mode := Source_Mode.Full,
) -> Unit_Analysis {
	root_range := tokenizer.text_range(0, len(source))
	if parsed.root != nil {
		root_range = parsed.root.range
	}

	unit := unit_analysis_make(unit_id, uri, root_range, allocator)
	unit.source = source
	unit.source_mode = mode
	if mode == .Full {
		for e in parsed.errors {
			append(
				&unit.diagnostics,
				Diagnostic {
					kind = .Syntax_Error,
					range = e.range,
					message = strings.clone(e.message, allocator),
				},
			)
		}
	}
	c := Collector {
		root                                   = parsed.root,
		allocator                              = allocator,
		unit                                   = &unit,
		forward_type_symbols                   = make(map[Symbol_Id]bool, 16, allocator),
		current_scope                          = unit.root_scope,
		loop_source_stack                      = make([dynamic]Field_Access, 0, 4, allocator),
		structured_groups                      = make([dynamic]Structured_Group_Frame, 0, 2, allocator),
	}

	if c.root != nil {
		for stmt in c.root.stmts {
			if c.unit.source_mode == .Dependency_Interface {
				walk_dependency_interface_stmt(&c, stmt, c.unit.root_scope)
			} else {
				walk_stmt(&c, stmt, c.unit.root_scope)
			}
		}
	}
	collect_provided_names(&c)
	return unit
}

push_scope :: proc(
	c: ^Collector,
	kind: Scope_Kind,
	range: tokenizer.Range,
	owner := INVALID_SYMBOL_ID,
) -> Scope_Id {
	id := Scope_Id(u32(len(c.unit.scopes)))
	scope := Scope_Data {
		id           = id,
		kind         = kind,
		range        = range,
		parent       = c.current_scope,
		owner        = owner,
		declarations = make([dynamic]Symbol_Id, 0, 8, c.allocator),
		declarations_by_name = make(map[Scope_Declaration_Key]Symbol_Id, 0, c.allocator),
		children     = make([dynamic]Scope_Id, 0, 4, c.allocator),
	}
	append(&c.unit.scopes, scope)
	if c.current_scope != INVALID_SCOPE_ID {
		append(&c.unit.scopes[scope_id_index(c.current_scope)].children, id)
	}
	c.current_scope = id
	return id
}

pop_scope :: proc(c: ^Collector) {
	if c.current_scope == c.unit.root_scope {
		return
	}
	parent := c.unit.scopes[scope_id_index(c.current_scope)].parent
	if parent != INVALID_SCOPE_ID {
		c.current_scope = parent
	}
}

declare_collected_symbol :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	decl_range: tokenizer.Range,
	structure := INVALID_STRUCTURE_ID,
	declared_type := Field_Type_Ref_Data{},
	has_declared_type := false,
	type_clause_display := "",
	value_clause_display := "",
	type_clause_form := ast.Data_Type_Form{},
	has_type_clause_form := false,
	type_clause_table_has_of := false,
	skip_duplicate_check := false,
	source_decl := Decl_Info{},
	has_source_decl := false,
	type_id := UNKNOWN_TYPE_ID,
	owner := INVALID_SYMBOL_ID,
) -> Symbol_Id {
	canonical := canonical_name(name, c.allocator)
	if !skip_duplicate_check {
		check_duplicate_or_shadow(c, scope, canonical, kind, decl_range)
	}

	id := Symbol_Id(u32(len(c.unit.symbols)))
	decl_info := INVALID_DECL_INFO_ID
	if !symbol_kind_is_builtin(kind) {
		clause_kind := ast.Decl_Clause_Kind.Normal
		clause_flags := ast.Decl_Clause_Flags{}
		type_clause: ^ast.Data_Type_Clause
		value_clause: ^ast.Value_Clause
		default_clause: ^ast.Default_Clause
		if has_source_decl {
			clause_kind = source_decl.kind
			clause_flags = source_decl.flags
			type_clause = source_decl.type_clause
			value_clause = source_decl.value_clause
			default_clause = source_decl.default_clause
		}
		decl_info = push_decl_info(
			&c.unit.decl_infos,
			id,
			scope,
			canonical,
			kind,
			decl_range,
			clause_kind,
			clause_flags,
			type_clause,
			value_clause,
			default_clause,
		)
		if owner != INVALID_SYMBOL_ID {
			c.unit.decl_infos[decl_info_id_index(decl_info)].owner = owner
		}
	}
	append(
		&c.unit.symbols,
		Symbol_Data {
			id = id,
			name = canonical,
			kind = kind,
			owner = owner,
			scope = scope,
			decl_info = decl_info,
			type_id = type_id,
			decl_range = decl_range,
			structure = structure,
			declared_type = declared_type,
			has_declared_type = has_declared_type,
			type_clause_display = strings.clone(type_clause_display, c.allocator) if type_clause_display != "" else "",
			value_clause_display = strings.clone(value_clause_display, c.allocator) if value_clause_display != "" else "",
			type_clause_form = type_clause_form,
			has_type_clause_form = has_type_clause_form,
			type_clause_table_has_of = type_clause_table_has_of,
		},
	)
	scope_record_declaration(c.unit, scope, id)
	return id
}

add_reference :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	namespace: Namespace,
	kind: Reference_Kind,
	range: tokenizer.Range,
	type_is_ref := false,
	type_has_path := false,
	type_first_selector := ast.Selector_Op.Dash,
	type_clause_form := ast.Data_Type_Form{},
	has_type_clause_form := false,
) {
	id := Reference_Id(u32(len(c.unit.references)))
	append(
		&c.unit.references,
		Reference_Data {
			id = id,
			name = canonical_name(name, c.allocator),
			namespace = namespace,
			kind = kind,
			scope = scope,
			range = range,
			type_is_ref = type_is_ref,
			type_has_path = type_has_path,
			type_first_selector = type_first_selector,
			type_clause_form = type_clause_form,
			has_type_clause_form = has_type_clause_form,
		},
	)
}

check_duplicate_or_shadow :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	range: tokenizer.Range,
) {
	if symbol_kind_is_builtin(kind) {
		return
	}
	if scope_has_symbol(c, scope, name, kind) {
		add_diagnostic(c, .Duplicate_Declaration, range, "duplicate declaration")
		return
	}
	parent := c.unit.scopes[scope_id_index(scope)].parent
	for parent != INVALID_SCOPE_ID {
		if scope_has_symbol(c, parent, name, kind) {
			add_diagnostic(c, .Shadowed_Symbol, range, "declaration shadows outer symbol")
			return
		}
		parent = c.unit.scopes[scope_id_index(parent)].parent
	}
}

scope_has_symbol :: proc(c: ^Collector, scope: Scope_Id, name: string, kind: Symbol_Kind) -> bool {
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if symbol_kind_occupies(kind, namespace) {
			if scope_has_declared_declaration(c.unit, scope, namespace, name) {
				return true
			}
		}
	}
	return false
}

find_symbol_in_scope :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
) -> (
	Symbol_Id,
	bool,
) {
	canonical := canonical_name(name, c.allocator)
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if !symbol_kind_occupies(kind, namespace) {
			continue
		}
		if id, ok := scope_lookup_declaration(c.unit, scope, namespace, canonical); ok {
			return id, true
		}
	}
	return INVALID_SYMBOL_ID, false
}

find_same_kind_symbol_in_scope :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
) -> (
	Symbol_Id,
	bool,
) {
	id, ok := find_symbol_in_scope(c, scope, name, kind)
	if !ok || c.unit.symbols[symbol_id_index(id)].kind != kind {
		return INVALID_SYMBOL_ID, false
	}
	return id, true
}

symbol_is_forward_type :: proc(c: ^Collector, id: Symbol_Id) -> bool {
	if is_forward, ok := c.forward_type_symbols[id]; ok {
		return is_forward
	}
	return false
}

add_diagnostic :: proc(
	c: ^Collector,
	kind: Diagnostic_Kind,
	range: tokenizer.Range,
	message: string,
) {
	append(
		&c.unit.diagnostics,
		Diagnostic{kind = kind, range = range, message = strings.clone(message, c.allocator)},
	)
}

walk_dependency_interface_stmt :: proc(c: ^Collector, stmt: ^ast.Stmt, scope: Scope_Id) {
	if stmt == nil {
		return
	}
	#partial switch _ in stmt.derived_stmt {
	case ^ast.Report_Stmt,
	     ^ast.Function_Pool_Decl,
	     ^ast.Data_Decl,
	     ^ast.Data_Chained_Decl,
	     ^ast.Types_Decl,
	     ^ast.Constants_Decl,
	     ^ast.Field_Symbols_Decl,
	     ^ast.Statics_Decl,
	     ^ast.Tables_Decl,
	     ^ast.Ranges_Decl,
	     ^ast.Parameters_Decl,
	     ^ast.Select_Options_Decl,
	     ^ast.Controls_Decl,
	     ^ast.Class_Data_Decl,
	     ^ast.Type_Pools_Decl,
	     ^ast.Form_Decl,
	     ^ast.Function_Decl,
	     ^ast.Class_Decl,
	     ^ast.Interface_Decl:
		walk_stmt(c, stmt, scope)
	case:
	}
}

walk_stmt :: proc(c: ^Collector, stmt: ^ast.Stmt, scope: Scope_Id) {
	if stmt == nil {
		return
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Decl:
		infos := make([dynamic]Decl_Info, 0, 1, c.allocator)
		append(&infos, data_decl_info(n))
		collect_decl_infos(c, scope, infos[:], .Variable)
	case ^ast.Data_Chained_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.decls), c.allocator)
		for clause in n.decls {
			append(&infos, data_branch_info(clause, n.range))
		}
		collect_decl_infos(c, scope, infos[:], .Variable)
	case ^ast.Data_Inline_Decl:
		if symbol_id := declare_name_if_present(c, scope, n.name, .Variable, n.range);
		   symbol_id != INVALID_SYMBOL_ID {
			add_syntax_operand(
				c.unit,
				scope,
				n.range,
				.Variable,
				unknown_type_fact(),
				symbol = Symbol_Handle{unit = c.unit.unit_id, symbol = symbol_id},
				has_symbol = true,
				assignable = true,
			)
		}
		collect_inline_data_stmt_facts(c, n, scope)
	case ^ast.Types_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.types), c.allocator)
		for clause in n.types {
			append(&infos, types_clause_info(clause, n.range))
		}
		collect_decl_infos(c, scope, infos[:], .Type_Def)
	case ^ast.Constants_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.constants), c.allocator)
		for clause in n.constants {
			append(&infos, constants_clause_info(clause, n.range))
		}
		collect_decl_infos(c, scope, infos[:], .Constant)
	case ^ast.Field_Symbols_Decl:
		for clause in n.field_symbols {
			declare_typed_symbol(c, scope, clause.name, .Field_Symbol, n.range, clause.type_clause)
		}
	case ^ast.Statics_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.statics), c.allocator)
		for clause in n.statics {
			append(&infos, statics_clause_info(clause, n.range))
		}
		collect_decl_infos(c, scope, infos[:], .Variable)
	case ^ast.Tables_Decl:
		for clause in n.tables {
			declare_tables_clause(c, scope, clause, n.range)
		}
	case ^ast.Ranges_Decl:
		for clause in n.ranges {
			declare_range_like_clause(c, scope, clause.name, clause.for_clause, n.range)
		}
	case ^ast.Parameters_Decl:
		for clause in n.parameters {
			declare_parameter_clause(c, scope, clause, n.range)
		}
	case ^ast.Select_Options_Decl:
		for clause in n.options {
			declare_select_option_clause(c, scope, clause, n.range)
		}
	case ^ast.Controls_Decl:
		for clause in n.controls {
			declare_typed_symbol(c, scope, clause.name, .Control, n.range, clause.type_clause)
		}
	case ^ast.Class_Data_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.decls), c.allocator)
		for clause in n.decls {
			append(&infos, class_data_clause_info(clause, n.range))
		}
		collect_decl_infos(c, scope, infos[:], .Variable)
	case ^ast.Type_Pools_Decl:
	// TYPE-POOLS loads type pools but does not introduce identifier references.
	case ^ast.Function_Pool_Decl:
		walk_function_pool_decl(c, n, scope)
	case ^ast.Include_Stmt:
		walk_include_stmt(c, n, scope)
	case ^ast.Report_Stmt:
		walk_report_stmt(c, n, scope)
		collect_report_stmt_refs(c, n, scope)
	case ^ast.Class_Decl:
		walk_class_decl(c, n, scope)
	case ^ast.Interface_Decl:
		walk_interface_decl(c, n, scope)
	case ^ast.Method_Decl:
		walk_method_decl(c, n, scope)
	case ^ast.Form_Decl:
		walk_form_decl(c, n, scope)
	case ^ast.Function_Decl:
		walk_function_decl(c, n, scope)
	case ^ast.Module_Decl:
		walk_named_block(c, n.name, .Module, .Module, n.range, n.body, scope)
	case ^ast.Event_Block_Stmt:
		walk_body_in_scope(c, .Event_Block, n.range, n.body)
	case ^ast.Oop_Simple_Stmt:
		walk_oop_simple_stmt(c, n, scope)
	case ^ast.Assign_Stmt:
		collect_assignment_stmt_facts(c, n.range, n.lhs, n.rhs, scope, false)
	case ^ast.Downcast_Assign_Stmt:
		collect_assignment_stmt_facts(c, n.range, n.lhs, n.rhs, scope, false)
	case ^ast.Expr_Stmt:
		collect_expr_refs(c, n.expr, scope)
	case ^ast.Clear_Stmt:
		collect_clear_stmt_facts(c, n, scope)
	case ^ast.Refresh_Stmt:
		collect_refresh_stmt_facts(c, n, scope)
	case ^ast.Free_Stmt:
		collect_free_stmt_facts(c, n, scope)
	case ^ast.Unassign_Stmt:
		collect_unassign_stmt_facts(c, n, scope)
	case ^ast.Move_Stmt:
		collect_move_stmt_facts(c, n, scope)
	case ^ast.Move_Corresponding_Stmt:
		collect_move_corresponding_stmt_facts(c, n, scope)
	case ^ast.Add_Stmt:
		collect_add_stmt_facts(c, n, scope)
	case ^ast.Subtract_Stmt:
		collect_subtract_stmt_facts(c, n, scope)
	case ^ast.Multiply_Stmt:
		collect_multiply_stmt_facts(c, n, scope)
	case ^ast.Divide_Stmt:
		collect_divide_stmt_facts(c, n, scope)
	case ^ast.Compute_Stmt:
		collect_compute_stmt_facts(c, n, scope)
	case ^ast.Concatenate_Stmt:
		collect_concatenate_stmt_facts(c, n, scope)
	case ^ast.Split_Stmt:
		collect_split_stmt_facts(c, n, scope)
	case ^ast.Condense_Stmt:
		collect_expr_refs(c, n.target, scope)
	case ^ast.Replace_Stmt:
		collect_replace_stmt_facts(c, n, scope)
	case ^ast.Translate_Stmt:
		collect_translate_stmt_facts(c, n, scope)
	case ^ast.Shift_Stmt:
		collect_shift_stmt_facts(c, n, scope)
	case ^ast.Find_Stmt:
		collect_find_stmt_facts(c, n, scope)
	case ^ast.Search_Stmt:
		collect_search_stmt_facts(c, n, scope)
	case ^ast.Perform_Stmt:
		collect_perform_stmt_facts(c, n, scope)
	case ^ast.Call_Stmt:
		collect_call_stmt_facts(c, n, scope)
	case ^ast.Submit_Stmt:
		collect_submit_stmt_facts(c, n, scope)
	case ^ast.Message_Stmt:
		collect_message_stmt_facts(c, n, scope)
	case ^ast.Write_Stmt:
		collect_write_stmt_facts(c, n, scope)
	case ^ast.Write_To_Stmt:
		collect_write_to_stmt_facts(c, n, scope)
	case ^ast.Assert_Stmt:
		collect_expr_refs(c, n.condition, scope)
	case ^ast.Check_Stmt:
		collect_expr_refs(c, n.condition, scope)
	case ^ast.Flow_Stmt:
		collect_flow_stmt_facts(c, n, scope)
	case ^ast.Transaction_Stmt:
	case ^ast.Describe_Stmt:
		collect_describe_stmt_facts(c, n, scope)
	case ^ast.Runtime_Stmt:
		collect_runtime_stmt_facts(c, n, scope)
	case ^ast.Set_Handler_Stmt:
		collect_set_handler_stmt_facts(c, n, scope)
	case ^ast.Import_Stmt:
		collect_import_stmt_facts(c, n, scope)
	case ^ast.Export_Stmt:
		collect_export_stmt_facts(c, n, scope)
	case ^ast.Bit_Stmt:
		collect_bit_stmt_facts(c, n, scope)
	case ^ast.Locale_Stmt:
		collect_locale_stmt_facts(c, n, scope)
	case ^ast.Set_Cursor_Stmt:
		collect_expr_refs(c, n.field, scope)
		collect_expr_refs(c, n.offset, scope)
		collect_expr_refs(c, n.line, scope)
		collect_expr_refs(c, n.column, scope)
	case ^ast.Receive_Results_Stmt:
		collect_receive_results_stmt_facts(c, n, scope)
	case ^ast.Raise_Stmt:
		collect_raise_stmt_facts(c, n, scope)
	case ^ast.Authority_Check_Stmt:
		collect_authority_check_stmt_facts(c, n, scope)
	case ^ast.Field_Groups_Stmt:
		collect_expr_list_refs(c, n.groups[:], scope)
	case ^ast.Insert_Dummy_Stmt:
		collect_expr_refs(c, n.target, scope)
	case ^ast.Field_Stmt:
		collect_expr_list_refs(c, n.operands[:], scope)
	case ^ast.Assign_Field_Stmt:
		collect_assign_field_stmt_facts(c, n, scope)
	case ^ast.Create_Object_Stmt:
		collect_create_object_stmt_facts(c, n, scope)
	case ^ast.Create_Data_Stmt:
		collect_create_data_stmt_facts(c, n, scope)
	case ^ast.Text_Transform_Stmt:
		collect_expr_list_refs(c, n.operands[:], scope)
	case ^ast.Wait_Stmt:
		collect_expr_refs(c, n.condition, scope)
		collect_expr_refs(c, n.duration, scope)
	case ^ast.Convert_Time_Stamp_Stmt:
		collect_convert_time_stamp_stmt_facts(c, n, scope)
	case ^ast.List_Control_Stmt:
		collect_expr_list_refs(c, n.operands[:], scope)
	case ^ast.Line_Stmt:
		collect_line_stmt_facts(c, n, scope)
	case ^ast.Macro_Call_Stmt:
		collect_expr_list_refs(c, n.args[:], scope)
	case ^ast.Selection_Screen_Stmt:
		collect_selection_screen_stmt_facts(c, n, scope)
	case ^ast.If_Stmt:
		then_scope := walk_body_in_scope(c, .If_Branch, n.range, n.body)
		collect_expr_refs(c, n.condition, then_scope)
		for clause in n.elseif_clauses {
			branch_scope := walk_body_in_scope(c, .Elseif_Branch, clause.range, clause.body)
			collect_expr_refs(c, clause.condition, branch_scope)
		}
		if n.else_clause != nil {
			_ = walk_body_in_scope(
				c,
				.Else_Branch,
				n.else_clause.range,
				n.else_clause.body,
			)
		}
	case ^ast.Case_Stmt:
		collect_expr_refs(c, n.expr, scope)
		for clause in n.whens {
			when_scope := walk_body_in_scope(c, .When_Branch, clause.range, clause.body)
			collect_expr_list_refs(c, clause.operands[:], when_scope)
		}
		walk_stmt_list(c, n.recovery, scope)
	case ^ast.While_Stmt:
		loop_scope := walk_body_in_scope(c, .While_Block, n.range, n.body)
		collect_expr_refs(c, n.condition, loop_scope)
	case ^ast.Do_Stmt:
		loop_scope := walk_body_in_scope(c, .Do_Block, n.range, n.body)
		collect_expr_refs(c, n.count, loop_scope)
	case ^ast.Loop_Stmt:
		collect_loop_stmt_facts(c, n, scope)
	case ^ast.At_Stmt:
		collect_at_stmt_facts(c, n, scope)
	case ^ast.Try_Stmt:
		body_scope := walk_body_in_scope(c, .Try_Block, n.range, n.body)
		for clause in n.catches {
			_ = walk_catch_clause_facts(c, clause, body_scope)
		}
		if n.cleanup != nil {
			_ = walk_body_in_scope(c, .Cleanup_Clause, n.cleanup.range, n.cleanup.body)
		}
	case ^ast.Read_Table_Stmt:
		collect_read_table_stmt_facts(c, n, scope)
	case ^ast.Select_Stmt:
		collect_select_stmt_facts(c, n, scope)
	case ^ast.Open_Cursor_Stmt:
		collect_open_cursor_stmt_facts(c, n, scope)
	case ^ast.Fetch_Stmt:
		collect_fetch_stmt_facts(c, n, scope)
	case ^ast.Close_Cursor_Stmt:
		collect_expr_refs(c, n.handle, scope)
	case ^ast.Insert_Stmt:
		collect_insert_stmt_facts(c, n, scope)
	case ^ast.Append_Stmt:
		collect_append_stmt_facts(c, n, scope)
	case ^ast.Modify_Stmt:
		collect_modify_stmt_facts(c, n, scope)
	case ^ast.Sort_Stmt:
		collect_sort_stmt_facts(c, n, scope)
	case ^ast.Update_Stmt:
		collect_update_stmt_facts(c, n, scope)
	case ^ast.Delete_Stmt:
		collect_delete_stmt_facts(c, n, scope)
	case ^ast.Dataset_Stmt:
		collect_dataset_stmt_facts(c, n, scope)
	case ^ast.Textpool_Stmt:
		collect_textpool_stmt_facts(c, n, scope)
	case ^ast.Generate_Stmt:
		collect_generate_stmt_facts(c, n, scope)
	case ^ast.Exec_Sql_Stmt:
	}
}

collect_selection_screen_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Selection_Screen_Stmt,
	scope: Scope_Id,
) {
	declare_name_if_present(c, scope, stmt.title_name, .Variable, stmt.title_range)
	declare_name_if_present(c, scope, stmt.comment_name, .Variable, stmt.comment_range)
	if stmt.field_name != "" {
		add_reference(c, scope, stmt.field_name, .Value, .Identifier, stmt.field_range)
	}
}

declare_name_if_present :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	range: tokenizer.Range,
	owner := INVALID_SYMBOL_ID,
) -> Symbol_Id {
	if name == "" {
		return INVALID_SYMBOL_ID
	}
	return declare_collected_symbol(c, scope, name, kind, range, owner = owner)
}

set_entity_owner :: proc(c: ^Collector, id: Entity_Id, owner: Entity_Id) {
	if s := symbol(c.unit, id); s != nil {
		s.owner = owner
	}
	if info := entity_decl_info(c.unit, id); info != nil {
		info.owner = owner
	}
}

set_entity_signature :: proc(c: ^Collector, id: Entity_Id, signature: string) {
	if info := entity_decl_info(c.unit, id); info != nil && signature != "" {
		info.signature = strings.clone(signature, c.allocator)
	}
}

set_entity_signature_scope :: proc(c: ^Collector, id: Entity_Id, scope: Scope_Id) {
	if info := entity_decl_info(c.unit, id); info != nil {
		info.signature_scope = scope
	}
}

set_entity_body_scope :: proc(c: ^Collector, id: Entity_Id, scope: Scope_Id) {
	if info := entity_decl_info(c.unit, id); info != nil {
		info.body_scope = scope
	}
}

set_member_decl_info :: proc(
	c: ^Collector,
	id: Entity_Id,
	class_symbol: Entity_Id,
	visibility: Visibility,
	kind: Class_Member_Kind,
	flags: Decl_Info_Flags,
) {
	set_entity_owner(c, id, class_symbol)
	if info := entity_decl_info(c.unit, id); info != nil {
		info.visibility = visibility
		info.member_kind = kind
		info.flags += flags
	}
}

set_parameter_decl_info :: proc(
	c: ^Collector,
	id: Entity_Id,
	owner: Entity_Id,
	section: Decl_Parameter_Section,
	passing: Decl_Parameter_Passing,
	flags := Decl_Info_Flags{},
) {
	set_entity_owner(c, id, owner)
	if info := entity_decl_info(c.unit, id); info != nil {
		info.parameter_section = section
		info.parameter_passing = passing
		info.flags += flags
	}
}

append_signature_parameter :: proc(
	c: ^Collector,
	owner: Entity_Id,
	symbol: Entity_Id,
	name: string,
	range: tokenizer.Range,
	section: Decl_Parameter_Section,
	passing: Decl_Parameter_Passing,
	type_id: Type_Id,
	declared_type: Field_Type_Ref_Data,
	has_declared_type: bool,
	type_clause_display: string,
	type_clause_form: ast.Data_Type_Form,
	has_type_clause_form: bool,
	type_clause_table_has_of: bool,
	flags := Decl_Info_Flags{},
) {
	if info := entity_decl_info(c.unit, owner); info != nil {
		param_flags := flags
		if has_declared_type {
			param_flags += {.Has_Declared_Type}
		}
		append(
			&info.signature_parameters,
			Decl_Signature_Parameter_Data {
				symbol = symbol,
				name = canonical_name(name, c.allocator),
				range = range,
				section = section,
				passing = passing,
				type_id = type_id,
				declared_type = declared_type,
				type_clause_display = strings.clone(type_clause_display, c.allocator) if type_clause_display != "" else "",
				type_clause_form = type_clause_form,
				has_type_clause_form = has_type_clause_form,
				type_clause_table_has_of = type_clause_table_has_of,
				flags = param_flags,
			},
		)
	}
}

append_signature_exception :: proc(
	c: ^Collector,
	owner: Entity_Id,
	name: string,
	range: tokenizer.Range,
) {
	if info := entity_decl_info(c.unit, owner); info != nil {
		append(
			&info.signature_exceptions,
			Decl_Signature_Exception_Data {
				name = canonical_name(name, c.allocator),
				range = range,
			},
		)
	}
}

set_entity_event_signature :: proc(
	c: ^Collector,
	id: Entity_Id,
	name: string,
	range: tokenizer.Range,
	source_type: Field_Type_Ref_Data,
) {
	if info := entity_decl_info(c.unit, id); info != nil {
		info.event_name = strings.clone(name, c.allocator)
		info.event_range = range
		info.event_source_type = source_type
	}
}

data_decl_info :: proc(n: ^ast.Data_Decl) -> Decl_Info {
	return Decl_Info {
		kind = n.kind,
		flags = n.flags,
		name = n.name,
		range = n.range,
		paren_length = n.paren_length,
		length_clauses = n.length_clauses[:],
		type_clause = n.type_clause,
		value_clause = n.value_clause,
		occurs = n.occurs,
		include_ref = n.include_ref,
		as_name = n.as_name,
		renaming_suffix = n.renaming_suffix,
		read_only = n.read_only,
	}
}

data_branch_info :: proc(n: ast.Data_Chained_Branch, range: tokenizer.Range) -> Decl_Info {
	return Decl_Info {
		kind = n.kind,
		flags = n.flags,
		depth = n.depth,
		name = n.name,
		range = range,
		paren_length = n.paren_length,
		length_clauses = n.length_clauses[:],
		type_clause = n.type_clause,
		value_clause = n.value_clause,
		occurs = n.occurs,
		include_ref = n.include_ref,
		as_name = n.as_name,
		renaming_suffix = n.renaming_suffix,
		read_only = n.read_only,
	}
}

types_clause_info :: proc(n: ast.Types_Clause, range: tokenizer.Range) -> Decl_Info {
	return Decl_Info {
		kind = n.kind,
		flags = n.flags,
		depth = n.depth,
		name = n.name,
		range = range,
		paren_length = n.paren_length,
		length_clauses = n.length_clauses[:],
		type_clause = n.type_clause,
		occurs = n.occurs,
		include_ref = n.include_ref,
		as_name = n.as_name,
		renaming_suffix = n.renaming_suffix,
	}
}

constants_clause_info :: proc(n: ast.Constants_Clause, range: tokenizer.Range) -> Decl_Info {
	return Decl_Info {
		kind = n.kind,
		flags = n.flags,
		depth = n.depth,
		name = n.name,
		range = range,
		paren_length = n.paren_length,
		length_clauses = n.length_clauses[:],
		type_clause = n.type_clause,
		value_clause = n.value_clause,
		occurs = n.occurs,
		include_ref = n.include_ref,
		as_name = n.as_name,
		renaming_suffix = n.renaming_suffix,
	}
}

statics_clause_info :: proc(n: ast.Statics_Clause, range: tokenizer.Range) -> Decl_Info {
	return Decl_Info {
		kind = n.kind,
		flags = n.flags,
		depth = n.depth,
		name = n.name,
		range = range,
		paren_length = n.paren_length,
		length_clauses = n.length_clauses[:],
		type_clause = n.type_clause,
		value_clause = n.value_clause,
		occurs = n.occurs,
		include_ref = n.include_ref,
		as_name = n.as_name,
		renaming_suffix = n.renaming_suffix,
	}
}

class_data_clause_info :: proc(n: ast.Class_Data_Clause, range: tokenizer.Range) -> Decl_Info {
	return Decl_Info {
		kind = n.kind,
		flags = n.flags,
		depth = n.depth,
		name = n.name,
		range = range,
		paren_length = n.paren_length,
		length_clauses = n.length_clauses[:],
		type_clause = n.type_clause,
		value_clause = n.value_clause,
		occurs = n.occurs,
		include_ref = n.include_ref,
		as_name = n.as_name,
		renaming_suffix = n.renaming_suffix,
		read_only = n.read_only,
	}
}

collect_decl_infos :: proc(c: ^Collector, scope: Scope_Id, infos: []Decl_Info, kind: Symbol_Kind) {
	for i in 0 ..< len(infos) {
		info := infos[i]
		if collect_open_structured_group_info(c, scope, info) {
			continue
		}
		if info.depth != 0 {
			continue
		}
		switch info.kind {
		case .Begin_Group:
			if .Common_Part_Delimiter in info.flags {
				continue
			}
			if structured_group_has_matching_end(infos, i) {
				structure_id := structure_from_group(c, scope, infos, i)
				_ = declare_collected_symbol(
					c,
					scope,
					info.name,
					kind,
					info.range,
					structure_id,
					source_decl = info,
					has_source_decl = true,
				)
				add_reference(
					c,
					scope,
					info.name,
					.Type if kind == .Type_Def else .Value,
					.Structured_Decl_End,
					info.range,
				)
			} else {
				start_open_structured_group(c, scope, info, kind)
			}
		case .Normal:
			declare_info_symbol(c, scope, info, kind)
		case .Include_Type, .Include_Structure:
			if type_ref, ok := type_ref_from_expr(c, info.include_ref, .Type if info.kind == .Include_Type else .Value);
			   ok {
				add_type_reference(c, scope, type_ref, info.range, .Structure, info.kind == .Include_Structure)
			}
		case .End_Group:
		}
		collect_decl_info_facts(c, scope, info)
	}
}

structured_group_has_matching_end :: proc(infos: []Decl_Info, start: int) -> bool {
	depth := infos[start].depth
	for i := start + 1; i < len(infos); i += 1 {
		if infos[i].kind == .End_Group && infos[i].depth == depth {
			return true
		}
	}
	return false
}

start_open_structured_group :: proc(
	c: ^Collector,
	scope: Scope_Id,
	info: Decl_Info,
	kind: Symbol_Kind,
) {
	if info.name == "" {
		return
	}
	symbol_id := declare_collected_symbol(
		c,
		scope,
		info.name,
		kind,
		info.range,
		INVALID_STRUCTURE_ID,
		source_decl = info,
		has_source_decl = true,
	)
	add_reference(
		c,
		scope,
		info.name,
		.Type if kind == .Type_Def else .Value,
		.Structured_Decl_End,
		info.range,
	)
	append(
		&c.structured_groups,
		Structured_Group_Frame {
			name = canonical_name(info.name, c.allocator),
			scope = scope,
			symbol = symbol_id,
			fields = make([dynamic]Structure_Field_Data, 0, 4, c.allocator),
		},
	)
}

collect_open_structured_group_info :: proc(
	c: ^Collector,
	scope: Scope_Id,
	info: Decl_Info,
) -> bool {
	index := active_structured_group_index(c, scope)
	if index < 0 {
		return false
	}
	switch info.kind {
	case .Normal:
		if field, ok := structure_field_from_info(c, scope, info); ok {
			append(&c.structured_groups[index].fields, field)
		}
	case .Include_Type, .Include_Structure:
		extend_structure_from_include(c, scope, &c.structured_groups[index].fields, info)
	case .End_Group:
		finish_open_structured_group(c, index)
	case .Begin_Group:
		return false
	}
	return true
}

active_structured_group_index :: proc(c: ^Collector, scope: Scope_Id) -> int {
	if len(c.structured_groups) == 0 {
		return -1
	}
	index := len(c.structured_groups) - 1
	return index if c.structured_groups[index].scope == scope else -1
}

finish_open_structured_group :: proc(c: ^Collector, index: int) {
	frame := c.structured_groups[index]
	structure_id := push_collected_structure(c, frame.name, frame.fields, frame.scope)
	if frame.symbol != INVALID_SYMBOL_ID {
		s := &c.unit.symbols[symbol_id_index(frame.symbol)]
		s.structure = structure_id
	}
	resize(&c.structured_groups, index)
}

declare_info_symbol :: proc(
	c: ^Collector,
	scope: Scope_Id,
	info: Decl_Info,
	kind: Symbol_Kind,
) -> Symbol_Id {
	if info.name == "" {
		return INVALID_SYMBOL_ID
	}
	declared_type, has_type := type_ref_from_clause(c, info.type_clause)
	if info.checkbox_type {
		declared_type = builtin_type_ref("abap_bool")
		has_type = true
	}
	type_display := type_clause_display(c, info.type_clause)
	type_form, has_type_form := type_clause_form_from_ast(info.type_clause)
	apply_occurs_table_form(info, &type_form, &has_type_form)
	type_table_has_of := type_clause_table_has_of_from_ast(info.type_clause)
	value_display := value_clause_display(c, info.value_clause)
	structure_id := INVALID_STRUCTURE_ID
	if has_type {
		if info.type_clause != nil && info.type_clause.form == .Range_Of {
			structure_id = push_range_structure(c, scope, info.name, declared_type)
		}
		add_type_reference(c, scope, declared_type, info.range, type_form, has_type_form)
	}
	return declare_collected_symbol(
		c,
		scope,
		info.name,
		kind,
		info.range,
		structure_id,
		declared_type,
		has_type,
		type_display,
		value_display,
		type_clause_form = type_form,
		has_type_clause_form = has_type_form,
		type_clause_table_has_of = type_table_has_of,
		source_decl = info,
		has_source_decl = true,
	)
}

declare_typed_symbol :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	range: tokenizer.Range,
	type_clause: ^ast.Data_Type_Clause,
	value_display := "",
) -> Symbol_Id {
	if name == "" {
		return INVALID_SYMBOL_ID
	}
	declared_type, has_type := type_ref_from_clause(c, type_clause)
	type_display := type_clause_display(c, type_clause)
	type_form, has_type_form := type_clause_form_from_ast(type_clause)
	type_table_has_of := type_clause_table_has_of_from_ast(type_clause)
	structure_id := INVALID_STRUCTURE_ID
	if has_type {
		add_type_reference(c, scope, declared_type, range, type_form, has_type_form)
	}
	return declare_collected_symbol(
		c,
		scope,
		name,
		kind,
		range,
		structure_id,
		declared_type,
		has_type,
		type_display,
		value_display,
		type_clause_form = type_form,
		has_type_clause_form = has_type_form,
		type_clause_table_has_of = type_table_has_of,
		source_decl = Decl_Info{kind = .Normal, name = name, range = range, type_clause = type_clause},
		has_source_decl = type_clause != nil,
	)
}

type_clause_form_from_ast :: proc(clause: ^ast.Data_Type_Clause) -> (ast.Data_Type_Form, bool) {
	if clause == nil {
		return {}, false
	}
	return clause.form, true
}

type_clause_table_has_of_from_ast :: #force_inline proc "contextless" (clause: ^ast.Data_Type_Clause) -> bool {
	return clause != nil && clause.table_has_of
}

apply_occurs_table_form :: proc(
	info: Decl_Info,
	type_form: ^ast.Data_Type_Form,
	has_type_form: ^bool,
) {
	if info.occurs == nil || !has_type_form^ {
		return
	}
	#partial switch type_form^ {
	case .Like:
		type_form^ = .Like_Table
	case .Type:
		type_form^ = .Standard_Table
	}
}

type_form_is_table_category :: proc "contextless" (form: ast.Data_Type_Form) -> bool {
	#partial switch form {
	case .Any_Table,
	     .Table,
	     .Like_Table,
	     .Index_Table,
	     .Standard_Table,
	     .Sorted_Table,
	     .Hashed_Table,
	     .Like_Standard_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table:
		return true
	}
	return false
}

declare_tables_clause :: proc(
	c: ^Collector,
	scope: Scope_Id,
	clause: ast.Tables_Clause,
	range: tokenizer.Range,
) {
	if clause.name == "" {
		return
	}
	name := canonical_name(clause.name, c.allocator)
	declared_type := Field_Type_Ref_Data {
		namespace = .Type,
		base_name = name,
	}
	add_reference(c, scope, name, .Type, .Type_Ref, range)
	_ = declare_collected_symbol(
		c,
		scope,
		name,
		.Variable,
		range,
		INVALID_STRUCTURE_ID,
		declared_type,
		true,
		name,
	)
	append(&c.unit.table_work_areas, Table_Work_Area_Data{name = name, scope = scope, range = range})
}

declare_range_like_clause :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	for_clause: ^ast.For_Clause,
	range: tokenizer.Range,
) {
	if name == "" {
		return
	}
	low_high, ok := for_clause_type_ref(c, for_clause)
	if !ok {
		low_high = builtin_type_ref("c")
	}
	if ok {
		add_type_reference(c, scope, low_high, range)
	}
	structure_id := push_range_structure(c, scope, name, low_high)
	display :=
		concat2(c, "RANGE OF ", expr_display(c, for_clause.expr)) if for_clause != nil else "RANGE OF c"
	_ = declare_collected_symbol(
		c,
		scope,
		name,
		.Variable,
		range,
		structure_id,
		Field_Type_Ref_Data{},
		false,
		display,
	)
}

declare_parameter_clause :: proc(
	c: ^Collector,
	scope: Scope_Id,
	clause: ast.Parameters_Clause,
	range: tokenizer.Range,
) {
	info := Decl_Info {
		kind           = .Normal,
		name           = clause.name,
		range          = range,
		paren_length   = clause.paren_length,
		length_clauses = clause.length_clauses[:],
		type_clause    = clause.type_clause,
		default_clause = clause.default_clause,
		checkbox_type  = .As_Checkbox in clause.flags,
	}
	value_display := default_clause_display(c, clause.default_clause)
	symbol_id := declare_info_symbol(c, scope, info, .Variable)
	if symbol_id != INVALID_SYMBOL_ID && value_display != "" {
		c.unit.symbols[symbol_id_index(symbol_id)].value_clause_display = value_display
	}
	collect_decl_info_facts(c, scope, info)
	if clause.memory_id != nil {collect_expr_refs(c, clause.memory_id.id, scope)}
	if clause.matchcode_object != nil {collect_expr_refs(c, clause.matchcode_object.object, scope)}
	if clause.visible_length != nil {collect_expr_refs(c, clause.visible_length.length, scope)}
}

declare_select_option_clause :: proc(
	c: ^Collector,
	scope: Scope_Id,
	clause: ast.Select_Options_Clause,
	range: tokenizer.Range,
) {
	declare_range_like_clause(c, scope, clause.name, clause.for_clause, range)
	if clause.default_clause != nil {collect_expr_refs(c, clause.default_clause.expr, scope)}
	if clause.to_clause != nil {collect_expr_refs(c, clause.to_clause.expr, scope)}
	if clause.memory_id != nil {collect_expr_refs(c, clause.memory_id.id, scope)}
	if clause.matchcode_object != nil {collect_expr_refs(c, clause.matchcode_object.object, scope)}
	if clause.visible_length != nil {collect_expr_refs(c, clause.visible_length.length, scope)}
}

structure_from_group :: proc(
	c: ^Collector,
	scope: Scope_Id,
	infos: []Decl_Info,
	start: int,
) -> Structure_Id {
	fields := make([dynamic]Structure_Field_Data, 0, 4, c.allocator)
	depth := infos[start].depth
	i := start + 1
	for i < len(infos) {
		info := infos[i]
		if info.kind == .End_Group && info.depth == depth {
			break
		}
		if info.depth != depth + 1 {
			i += 1
			continue
		}
		switch info.kind {
		case .Normal:
			if field, ok := structure_field_from_info(c, scope, info); ok {
				append(&fields, field)
			}
		case .Begin_Group:
			nested := structure_from_group(c, scope, infos, i)
			flags := Structure_Field_Flags{.Has_Decl_Range}
			append(
				&fields,
				Structure_Field_Data {
					name = canonical_name(info.name, c.allocator),
					decl_range = info.range,
					decl_unit = c.unit.unit_id,
					type_id = type_structure(c.unit, nested),
					structure = nested,
					flags = flags,
				},
			)
		case .Include_Type, .Include_Structure:
			extend_structure_from_include(c, scope, &fields, info)
		case .End_Group:
		}
		i += 1
	}
	return push_collected_structure(c, infos[start].name, fields, scope)
}

structure_field_from_info :: proc(
	c: ^Collector,
	scope: Scope_Id,
	info: Decl_Info,
) -> (
	Structure_Field_Data,
	bool,
) {
	if info.name == "" {
		return {}, false
	}
	type_ref, has_type := type_ref_from_clause(c, info.type_clause)
	type_form, has_type_form := type_clause_form_from_ast(info.type_clause)
	apply_occurs_table_form(info, &type_form, &has_type_form)
	structure_id := INVALID_STRUCTURE_ID
	if has_type {
		add_type_reference(c, scope, type_ref, info.range, type_form, has_type_form)
	}
	type_id := type_structure(c.unit, structure_id) if structure_id != INVALID_STRUCTURE_ID else UNKNOWN_TYPE_ID
	flags := Structure_Field_Flags{.Has_Decl_Range}
	if has_type {
		flags += {.Has_Type_Ref}
	}
	return Structure_Field_Data {
			name = canonical_name(info.name, c.allocator),
			decl_range = info.range,
			decl_unit = c.unit.unit_id,
			type_id = type_id,
			structure = structure_id,
			type_ref = type_ref,
			type_clause_form = type_form,
			has_type_clause_form = has_type_form,
			value_clause_display = value_clause_display(c, info.value_clause),
			flags = flags,
		},
		true
}

extend_structure_from_include :: proc(
	c: ^Collector,
	scope: Scope_Id,
	fields: ^[dynamic]Structure_Field_Data,
	info: Decl_Info,
) {
	type_ref, ok := type_ref_from_expr(
		c,
		info.include_ref,
		.Type if info.kind == .Include_Type else .Value,
	)
	if !ok {
		return
	}
	add_type_reference(c, scope, type_ref, info.range, .Structure, info.kind == .Include_Structure)
	if field, field_ok := include_type_component_field(c, scope, info, type_ref); field_ok {
		append(fields, field)
		return
	}
	if info.as_name != "" {
		flags := Structure_Field_Flags{.Has_Type_Ref}
		append(
			fields,
			Structure_Field_Data {
				name = canonical_name(info.as_name, c.allocator),
				decl_unit = c.unit.unit_id,
				structure = INVALID_STRUCTURE_ID,
				type_ref = type_ref,
				type_clause_form = .Structure,
				has_type_clause_form = true,
				flags = flags,
			},
		)
		return
	}
	append(
		fields,
		Structure_Field_Data {
			decl_range = info.range,
			decl_unit = c.unit.unit_id,
			structure = INVALID_STRUCTURE_ID,
			type_ref = type_ref,
			type_clause_form = .Structure,
			has_type_clause_form = true,
			include_renaming_suffix = strings.clone(info.renaming_suffix, c.allocator) if info.renaming_suffix != "" else "",
			flags = {.Has_Type_Ref, .Is_Include},
		},
	)
}

include_type_component_field :: proc(
	c: ^Collector,
	scope: Scope_Id,
	info: Decl_Info,
	type_ref: Field_Type_Ref_Data,
) -> (Structure_Field_Data, bool) {
	if info.kind != .Include_Type ||
	   info.as_name != "" ||
	   info.renaming_suffix != "" ||
	   info.occurs != nil {
		return {}, false
	}
	is_field := info.value_clause != nil
	if !is_field && type_ref.base_name != "" && len(type_ref.field_path) == 0 {
		symbol_id, ok := lookup_symbol_in_scope_chain(c, scope, type_ref.base_name, type_ref.namespace)
		if !ok && type_ref.namespace == .Type {
			symbol_id, ok = lookup_symbol_in_scope_chain(c, scope, type_ref.base_name, .Value)
		}
		is_field = ok && c.unit.symbols[symbol_id_index(symbol_id)].structure == INVALID_STRUCTURE_ID
	}
	if !is_field {
		return {}, false
	}
	flags := Structure_Field_Flags{.Has_Decl_Range, .Has_Type_Ref}
	return Structure_Field_Data {
		name = "include",
		decl_range = info.range,
		decl_unit = c.unit.unit_id,
		structure = INVALID_STRUCTURE_ID,
		type_ref = type_ref,
		type_clause_form = .Structure,
		has_type_clause_form = true,
		value_clause_display = value_clause_display(c, info.value_clause),
		flags = flags,
	}, true
}

push_collected_structure :: proc(
	c: ^Collector,
	name: string,
	fields: [dynamic]Structure_Field_Data,
	scope := INVALID_SCOPE_ID,
) -> Structure_Id {
	id := Structure_Id(u32(len(c.unit.structures)))
	append(
		&c.unit.structures,
		Structure_Data {
			id = id,
			origin_unit = c.unit.unit_id,
			origin_structure = id,
			name = canonical_name(name, c.allocator),
			scope = scope,
			fields = fields,
		},
	)
	_ = type_structure(c.unit, id)
	return id
}

push_range_structure :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	low_high: Field_Type_Ref_Data,
) -> Structure_Id {
	fields := make([dynamic]Structure_Field_Data, 0, 4, c.allocator)
	sign_type := builtin_type_ref("ddsign")
	option_type := builtin_type_ref("ddoption")
	append(&fields, range_field(c, scope, "sign", sign_type))
	append(&fields, range_field(c, scope, "option", option_type))
	append(&fields, range_field(c, scope, "low", low_high))
	append(&fields, range_field(c, scope, "high", low_high))
	return push_collected_structure(
		c,
		concat3(c, "<range:", canonical_name(name, c.allocator), ">"),
		fields,
		scope,
	)
}

range_field :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	type_ref: Field_Type_Ref_Data,
) -> Structure_Field_Data {
	return Structure_Field_Data {
		name = strings.clone(name, c.allocator),
		decl_unit = c.unit.unit_id,
		structure = INVALID_STRUCTURE_ID,
		type_ref = type_ref,
		flags = {.Has_Type_Ref},
	}
}

type_ref_from_clause :: proc(
	c: ^Collector,
	clause: ^ast.Data_Type_Clause,
) -> (
	Field_Type_Ref_Data,
	bool,
) {
	if clause == nil {
		return {}, false
	}
	ns := Namespace.Type
	is_ref := clause.form == .Ref_To || type_ref_expr_is_ref(clause.type_ref)
	#partial switch clause.form {
	case .Like,
	     .Structure,
	     .Like_Line_Of,
	     .Like_Table,
	     .Like_Standard_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table:
		ns = .Value
	case:
		ns = .Type
	}
	if clause.type_ref == nil {
		return {}, false
	}
	return type_ref_from_expr(c, clause.type_ref, ns, is_ref)
}

type_ref_expr_is_ref :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	if n, ok := expr.derived_expr.(^ast.Type_Ref_Expr); ok {
		return n.is_ref
	}
	return false
}

for_clause_type_ref :: proc(
	c: ^Collector,
	clause: ^ast.For_Clause,
) -> (
	Field_Type_Ref_Data,
	bool,
) {
	if clause == nil || clause.expr == nil {
		return {}, false
	}
	return type_ref_from_expr(c, clause.expr, .Value)
}

type_ref_from_expr :: proc(
	c: ^Collector,
	expr: ^ast.Expr,
	namespace: Namespace,
	is_ref := false,
) -> (
	Field_Type_Ref_Data,
	bool,
) {
	if expr == nil {
		return {}, false
	}
	if type_ref, ok := type_ref_from_ast_expr(c, expr, namespace, is_ref); ok {
		return type_ref, true
	}
	text := expr_display(c, expr)
	text = strings.trim_space(text)
	if text == "" {
		return {}, false
	}
	// Legacy non-selector fallback only; declaration-addition boundaries belong to parser AST.
	return Field_Type_Ref_Data {
			namespace = namespace,
			is_ref = is_ref,
			base_name = canonical_name(text, c.allocator),
			base_range = expr.range,
		},
		true
}

type_ref_from_ast_expr :: proc(
	c: ^Collector,
	expr: ^ast.Expr,
	namespace: Namespace,
	is_ref: bool,
) -> (
	Field_Type_Ref_Data,
	bool,
) {
	if expr == nil {
		return {}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		return type_ref_from_type_ref_expr(c, n, namespace, is_ref)
	case ^ast.Ident_Expr:
		return Field_Type_Ref_Data {
				namespace = namespace,
				is_ref = is_ref,
				base_name = canonical_name(n.name, c.allocator),
				base_range = n.range,
			},
			n.name != ""
	case ^ast.Selector_Expr:
		type_ref, ok := type_ref_from_ast_expr(c, n.base, namespace, is_ref)
		if !ok {
			return {}, false
		}
		name, range, name_ok := expr_name(n.field)
		if !name_ok {
			return {}, false
		}
		if len(type_ref.field_path) == 0 {
			type_ref.field_path = make([dynamic]string, 0, 2, c.allocator)
			type_ref.field_ranges = make([dynamic]tokenizer.Range, 0, 2, c.allocator)
			type_ref.field_derefs = make([dynamic]bool, 0, 2, c.allocator)
			type_ref.field_selectors = make([dynamic]ast.Selector_Op, 0, 2, c.allocator)
		}
		append(&type_ref.field_path, canonical_name(name, c.allocator))
		append(&type_ref.field_ranges, range)
		append(&type_ref.field_derefs, n.op == .Arrow && name == "*")
		append(&type_ref.field_selectors, n.op)
		return type_ref, true
	}
	return {}, false
}

type_ref_from_type_ref_expr :: proc(
	c: ^Collector,
	expr: ^ast.Type_Ref_Expr,
	namespace: Namespace,
	is_ref: bool,
) -> (
	Field_Type_Ref_Data,
	bool,
) {
	base := expr.base_name
	base_range := expr.base_range
	if base == "" && expr.name != "" {
		base = expr.name
		base_range = expr.range
	}
	if base == "" {
		return {}, false
	}
	ns := namespace
	if len(expr.path) > 0 &&
	   (expr.path[0].selector == .Fat_Arrow || expr.path[0].selector == .Tilde) {
		ns = .Type
	}
	field_path := make([dynamic]string, 0, len(expr.path), c.allocator)
	field_ranges := make([dynamic]tokenizer.Range, 0, len(expr.path), c.allocator)
	field_derefs := make([dynamic]bool, 0, len(expr.path), c.allocator)
	field_selectors := make([dynamic]ast.Selector_Op, 0, len(expr.path), c.allocator)
	for segment in expr.path {
		append(&field_path, canonical_name(segment.name, c.allocator))
		append(&field_ranges, segment.range)
		append(&field_derefs, segment.selector == .Arrow && segment.name == "*")
		append(&field_selectors, segment.selector)
	}
	return Field_Type_Ref_Data {
			namespace = ns,
			is_ref = is_ref,
			base_name = canonical_name(base, c.allocator),
			base_range = base_range,
			field_path = field_path,
			field_ranges = field_ranges,
			field_derefs = field_derefs,
			field_selectors = field_selectors,
		},
		true
}

type_clause_display :: proc(c: ^Collector, clause: ^ast.Data_Type_Clause) -> string {
	if clause == nil {
		return ""
	}
	ref := type_ref_display(c, clause.type_ref)
	display := ""
	#partial switch clause.form {
	case .Ref_To:
		display = concat2(c, "REF TO ", ref)
	case .Like_Line_Of, .Type_Line_Of:
		display = concat2(c, "LINE OF ", ref)
	case .Any_Table:
		display = strings.clone("ANY TABLE", c.allocator) if !clause.table_has_of && clause.type_ref == nil else concat2(c, "ANY TABLE OF ", ref)
	case .Table, .Like_Table:
		display = strings.clone("TABLE", c.allocator) if !clause.table_has_of && clause.type_ref == nil else concat2(c, "TABLE OF ", ref)
	case .Index_Table:
		display = strings.clone("INDEX TABLE", c.allocator) if !clause.table_has_of && clause.type_ref == nil else concat2(c, "INDEX TABLE OF ", ref)
	case .Standard_Table, .Like_Standard_Table:
		display = strings.clone("STANDARD TABLE", c.allocator) if !clause.table_has_of && clause.type_ref == nil else concat2(c, "STANDARD TABLE OF ", ref)
	case .Sorted_Table, .Like_Sorted_Table:
		display = strings.clone("SORTED TABLE", c.allocator) if !clause.table_has_of && clause.type_ref == nil else concat2(c, "SORTED TABLE OF ", ref)
	case .Hashed_Table, .Like_Hashed_Table:
		display = strings.clone("HASHED TABLE", c.allocator) if !clause.table_has_of && clause.type_ref == nil else concat2(c, "HASHED TABLE OF ", ref)
	case .Range_Of:
		display = concat2(c, "RANGE OF ", ref)
	case:
		display = ref
	}
	if clause.initial_size != nil {
		return concat3(c, display, " INITIAL SIZE ", expr_display(c, clause.initial_size))
	}
	return display
}

type_ref_display :: proc(c: ^Collector, expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		if n.text != "" {
			return strings.clone(n.text, c.allocator)
		}
		return strings.clone(n.name, c.allocator)
	case ^ast.Ident_Expr:
		return strings.clone(n.name, c.allocator)
	case ^ast.Selector_Expr:
		return ast.print_node(n, c.allocator)
	case ^ast.Interface_Qualified_Selector_Expr:
		return ast.print_node(n, c.allocator)
	}
	return expr_display(c, expr)
}

value_clause_display :: proc(c: ^Collector, clause: ^ast.Value_Clause) -> string {
	if clause == nil {
		return ""
	}
	if clause.is_initial {
		return strings.clone("IS INITIAL", c.allocator)
	}
	return expr_display(c, clause.expr)
}

default_clause_display :: proc(c: ^Collector, clause: ^ast.Default_Clause) -> string {
	if clause == nil {
		return ""
	}
	return expr_display(c, clause.expr)
}

expr_display :: proc(c: ^Collector, expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	if expr.range.start >= 0 &&
	   expr.range.end <= len(c.unit.source) &&
	   expr.range.start < expr.range.end {
		return strings.clone(c.unit.source[expr.range.start:expr.range.end], c.allocator)
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		if n.text != "" {
			return strings.clone(n.text, c.allocator)
		}
		return strings.clone(n.name, c.allocator)
	case ^ast.Ident_Expr:
		return strings.clone(n.name, c.allocator)
	case ^ast.Literal_Expr:
		return strings.clone(n.value, c.allocator)
	}
	return ""
}

add_type_reference :: proc(
	c: ^Collector,
	scope: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	range: tokenizer.Range,
	type_clause_form := ast.Data_Type_Form{},
	has_type_clause_form := false,
) {
	if type_ref.base_name == "" {
		return
	}
	base_range := type_ref.base_range
	if base_range.start >= base_range.end {
		base_range = range
	}
	add_reference(
		c,
		scope,
		type_ref.base_name,
		type_ref.namespace,
		.Type_Ref,
		base_range,
		type_ref.is_ref,
		len(type_ref.field_path) > 0,
		type_ref_path_selector(type_ref, 0),
		type_clause_form,
		has_type_clause_form,
	)
	if len(type_ref.field_path) > 0 {
		segments := make([dynamic]Field_Access_Segment, 0, len(type_ref.field_path), c.allocator)
		for name, i in type_ref.field_path {
			segment_range := range
			if i < len(type_ref.field_ranges) && type_ref.field_ranges[i].start < type_ref.field_ranges[i].end {
				segment_range = type_ref.field_ranges[i]
			}
			append(
				&segments,
				Field_Access_Segment {
					name = name,
					range = segment_range,
					selector = type_ref_path_selector(type_ref, i),
					deref = i < len(type_ref.field_derefs) && type_ref.field_derefs[i],
				},
			)
		}
		append(
			&c.unit.field_accesses,
			Field_Access {
				scope = scope,
				base_namespace = type_ref.namespace,
				base_name = type_ref.base_name,
				base_range = base_range,
				field_path = segments,
				in_type_position = true,
			},
		)
	}
}

type_ref_path_selector :: #force_inline proc(
	type_ref: Field_Type_Ref_Data,
	index: int,
) -> ast.Selector_Op {
	return type_ref.field_selectors[index] if index < len(type_ref.field_selectors) else .Dash
}

lookup_symbol_in_scope_chain :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	namespace: Namespace,
) -> (
	Symbol_Id,
	bool,
) {
	current := scope
	for current != INVALID_SCOPE_ID {
		scope_idx := scope_id_index(current)
		if scope_idx < 0 || scope_idx >= len(c.unit.scopes) {
			break
		}
		if id, ok := scope_lookup_declaration(c.unit, current, namespace, name); ok {
			return id, true
		}
		current = c.unit.scopes[scope_idx].parent
	}
	return INVALID_SYMBOL_ID, false
}

concat2 :: proc(c: ^Collector, a, b: string) -> string {
	out := strings.builder_make(c.allocator)
	strings.write_string(&out, a)
	strings.write_string(&out, b)
	return strings.to_string(out)
}

concat3 :: proc(c: ^Collector, a, b, d: string) -> string {
	out := strings.builder_make(c.allocator)
	strings.write_string(&out, a)
	strings.write_string(&out, b)
	strings.write_string(&out, d)
	return strings.to_string(out)
}

walk_include_stmt :: proc(c: ^Collector, stmt: ^ast.Include_Stmt, scope: Scope_Id) {
	for include_name in stmt.names {
		name := canonical_name(include_name.name, c.allocator)
		_ = declare_collected_symbol(c, scope, name, .Include, include_name.range)
		append(
			&c.unit.include_edges,
			Include_Edge{name = name, range = include_name.range, target = INVALID_UNIT_ID, if_found = stmt.if_found},
		)
		add_reference(c, scope, name, .Value, .Include, include_name.range)
	}
}

walk_report_stmt :: proc(c: ^Collector, stmt: ^ast.Report_Stmt, scope: Scope_Id) {
	if stmt.kind != .Report && stmt.kind != .Program {
		return
	}
	name, range, ok := expr_name(stmt.name)
	if ok {
		_ = declare_collected_symbol(c, scope, name, .Report, range)
	}
}

walk_named_block :: proc(
	c: ^Collector,
	name: string,
	symbol_kind: Symbol_Kind,
	scope_kind: Scope_Kind,
	range: tokenizer.Range,
	body: [dynamic]^ast.Stmt,
	parent_scope: Scope_Id,
) {
	owner := INVALID_SYMBOL_ID
	if name != "" {
		owner = declare_collected_symbol(c, parent_scope, name, symbol_kind, range)
	}
	walk_body_in_scope(c, scope_kind, range, body, owner)
}

walk_class_decl :: proc(c: ^Collector, stmt: ^ast.Class_Decl, scope: Scope_Id) {
	if c.unit.source_mode == .Dependency_Interface && .Implementation in stmt.flags {
		return
	}
	owner := INVALID_SYMBOL_ID
	owner_is_forward := false
	declared_owner := false
	if stmt.name != "" && (.Implementation in stmt.flags || .Bodyless in stmt.flags) {
		if existing, ok := find_same_kind_symbol_in_scope(c, scope, stmt.name, .Class); ok {
			owner = existing
			owner_is_forward = symbol_is_forward_type(c, owner)
		}
	} else if stmt.name != "" {
		if existing, ok := find_same_kind_symbol_in_scope(c, scope, stmt.name, .Class);
		   ok && symbol_is_forward_type(c, existing) {
			owner = existing
			owner_is_forward = true
		}
	}
	if owner == INVALID_SYMBOL_ID && stmt.name != "" {
		owner = declare_collected_symbol(c, scope, stmt.name, .Class, stmt.header_range)
		declared_owner = true
	}
	if .Bodyless in stmt.flags && owner != INVALID_SYMBOL_ID && (declared_owner || owner_is_forward) {
		c.forward_type_symbols[owner] = true
	}
	if .Implementation in stmt.flags && stmt.name != "" {
		add_reference(c, scope, stmt.name, .Type, .Type_Ref, stmt.header_range)
	}
	if !(.Implementation in stmt.flags) && !(.Bodyless in stmt.flags) && owner != INVALID_SYMBOL_ID {
		c.forward_type_symbols[owner] = false
		add_class_definition(c, owner, .Abstract in stmt.flags)
		for friend in stmt.friends {
			if friend.name != "" {
				append(
					&c.unit.class_friends,
					Class_Friend_Data {
						class_symbol = owner,
						friend_name = canonical_name(friend.name, c.allocator),
						range = friend.range,
					},
				)
			}
		}
		if stmt.superclass_name != "" {
			superclass := canonical_name(stmt.superclass_name, c.allocator)
			append(
				&c.unit.class_inheritance,
				Class_Inheritance_Data{class_symbol = owner, superclass_name = superclass},
			)
			add_reference(c, scope, superclass, .Type, .Type_Ref, stmt.superclass_range)
		}
	}
	previous := c.current_scope
	c.current_scope = scope
	class_scope := push_scope(c, .Class, stmt.range, owner)
	if !(.Implementation in stmt.flags) && !(.Bodyless in stmt.flags) {
		set_entity_body_scope(c, owner, class_scope)
		walk_class_body(c, stmt.body, class_scope, owner, .Private)
	} else {
		walk_stmt_list(c, stmt.body, class_scope)
	}
	c.current_scope = class_scope
	pop_scope(c)
	c.current_scope = previous
}

walk_interface_decl :: proc(c: ^Collector, stmt: ^ast.Interface_Decl, scope: Scope_Id) {
	owner := INVALID_SYMBOL_ID
	owner_is_forward := false
	declared_owner := false
	if stmt.name != "" && stmt.is_bodyless {
		if existing, ok := find_same_kind_symbol_in_scope(c, scope, stmt.name, .Interface); ok {
			owner = existing
			owner_is_forward = symbol_is_forward_type(c, owner)
		}
	} else if stmt.name != "" {
		if existing, ok := find_same_kind_symbol_in_scope(c, scope, stmt.name, .Interface);
		   ok && symbol_is_forward_type(c, existing) {
			owner = existing
			owner_is_forward = true
		}
	}
	if owner == INVALID_SYMBOL_ID && stmt.name != "" {
		owner = declare_collected_symbol(c, scope, stmt.name, .Interface, stmt.header_range)
		declared_owner = true
	}
	if stmt.is_bodyless && owner != INVALID_SYMBOL_ID && (declared_owner || owner_is_forward) {
		c.forward_type_symbols[owner] = true
	}
	if !stmt.is_bodyless && owner != INVALID_SYMBOL_ID {
		c.forward_type_symbols[owner] = false
	}
	previous := c.current_scope
	c.current_scope = scope
	interface_scope := push_scope(c, .Interface, stmt.range, owner)
	if !stmt.is_bodyless {
		set_entity_body_scope(c, owner, interface_scope)
		walk_class_body(c, stmt.body, interface_scope, owner, .Public)
	} else {
		walk_stmt_list(c, stmt.body, interface_scope)
	}
	c.current_scope = interface_scope
	pop_scope(c)
	c.current_scope = previous
}

walk_class_body :: proc(
	c: ^Collector,
	body: [dynamic]^ast.Stmt,
	scope: Scope_Id,
	owner: Symbol_Id,
	default_visibility: Visibility,
) {
	visibility := default_visibility
	previous := c.current_scope
	c.current_scope = scope
	for child in body {
		if child == nil {
			continue
		}
		if oop, ok := child.derived_stmt.(^ast.Oop_Simple_Stmt); ok {
			if oop.kind == .Class_Section {
				switch oop.visibility {
				case .Public:
					visibility = .Public
				case .Protected:
					visibility = .Protected
				case .Private:
					visibility = .Private
				case .Unspecified:
				}
			} else {
				if c.unit.source_mode == .Dependency_Interface && visibility == .Private {
					continue
				}
				collect_class_oop_stmt(c, oop, scope, owner, visibility)
			}
			continue
		}
		if c.unit.source_mode == .Dependency_Interface && visibility == .Private {
			continue
		}
		walk_stmt(c, child, scope)
		collect_class_attribute_stmt(c, child, scope, owner, visibility)
	}
	c.current_scope = previous
}

walk_method_decl :: proc(c: ^Collector, stmt: ^ast.Method_Decl, scope: Scope_Id) {
	class_owner, has_class_owner := enclosing_owner(c, scope, .Class)
	owner_entity := class_owner if has_class_owner else INVALID_SYMBOL_ID
	owner := declare_name_if_present(c, scope, stmt.name, .Method, stmt.header_range, owner_entity)
	add_method_interface_qualifier_reference(c, stmt.qualifier, scope, stmt.qualifier_range)
	previous := c.current_scope
	c.current_scope = scope
	method_scope := push_scope(c, .Method, stmt.range, owner)
	set_entity_signature(c, owner, stmt.header_text)
	set_entity_body_scope(c, owner, method_scope)
	if has_class_owner {
		method_name := stmt.name
		if method_name == "" {
			method_name = stmt.member_name
		}
		member_symbol, _ := class_definition_member(c.unit, class_owner, .Routine, method_name)
		note_method_implementation(c, member_symbol, stmt.header_range)
		member_info := entity_decl_info(c.unit, member_symbol)
		declare_method_scope_params(c, member_info, method_scope, owner)
		if member_info == nil || !(.Is_Static in member_info.flags) {
			_ = declare_collected_symbol(
				c,
				method_scope,
				"me",
				.Variable,
				stmt.header_range,
				INVALID_STRUCTURE_ID,
				Field_Type_Ref_Data {
					namespace = .Type,
					is_ref = true,
					base_name = c.unit.symbols[symbol_id_index(class_owner)].name,
				},
				true,
				concat2(c, "REF TO ", c.unit.symbols[symbol_id_index(class_owner)].name),
				owner = owner,
			)
		}
	}
	if c.unit.source_mode != .Dependency_Interface {
		collect_amdp_using_refs(c, stmt, method_scope)
		walk_stmt_list(c, stmt.body, method_scope)
	}
	c.current_scope = method_scope
	pop_scope(c)
	c.current_scope = previous
}

walk_form_decl :: proc(c: ^Collector, stmt: ^ast.Form_Decl, scope: Scope_Id) {
	owner := declare_name_if_present(c, scope, stmt.name, .Form, stmt.header_range)
	previous := c.current_scope
	c.current_scope = scope
	form_scope := push_scope(c, .Form, stmt.range, owner)
	set_entity_signature(c, owner, stmt.header_text)
	set_entity_signature_scope(c, owner, form_scope)
	set_entity_body_scope(c, owner, form_scope)
	form_parameters_from_ast(c, stmt.form_parameters[:], form_scope, owner)
	if c.unit.source_mode != .Dependency_Interface {
		walk_stmt_list(c, stmt.body, form_scope)
	}
	c.current_scope = form_scope
	pop_scope(c)
	c.current_scope = previous
}

walk_function_decl :: proc(c: ^Collector, stmt: ^ast.Function_Decl, scope: Scope_Id) {
	owner := declare_name_if_present(c, scope, stmt.name, .Module, stmt.header_range)
	previous := c.current_scope
	c.current_scope = scope
	function_scope := push_scope(c, .Module, stmt.range, owner)
	set_entity_signature(c, owner, stmt.header_text)
	set_entity_signature_scope(c, owner, function_scope)
	set_entity_body_scope(c, owner, function_scope)
	function_parameters_from_ast(c, stmt, function_scope, owner)
	if c.unit.source_mode != .Dependency_Interface {
		walk_stmt_list(c, stmt.body, function_scope)
	}
	c.current_scope = function_scope
	pop_scope(c)
	c.current_scope = previous
}

walk_body_in_scope :: proc(
	c: ^Collector,
	kind: Scope_Kind,
	range: tokenizer.Range,
	body: [dynamic]^ast.Stmt,
	owner := INVALID_SYMBOL_ID,
) -> Scope_Id {
	_ = push_scope(c, kind, range, owner)
	child_scope := c.current_scope
	walk_stmt_list(c, body, c.current_scope)
	pop_scope(c)
	return child_scope
}

walk_stmt_list :: proc(c: ^Collector, body: [dynamic]^ast.Stmt, scope: Scope_Id) {
	previous := c.current_scope
	c.current_scope = scope
	for child in body {
		walk_stmt(c, child, scope)
	}
	c.current_scope = previous
}

walk_oop_simple_stmt :: proc(c: ^Collector, stmt: ^ast.Oop_Simple_Stmt, scope: Scope_Id) {
	kind := Symbol_Kind.Method
	#partial switch stmt.kind {
	case .Methods, .Class_Methods:
		kind = .Method
	case .Events, .Class_Events:
		kind = .Event
	case:
		return
	}
	for member in stmt.members {
		name := member.name
		if kind == .Method {
			name = oop_method_symbol_name(member)
		}
		declare_name_if_present(c, scope, name, kind, stmt.range)
	}
}

add_class_definition :: proc(c: ^Collector, owner: Symbol_Id, is_abstract: bool) {
	if owner == INVALID_SYMBOL_ID {
		return
	}
	if is_abstract {
		if info := entity_decl_info(c.unit, owner); info != nil {
			info.flags += {.Is_Abstract}
		}
	}
	for &definition in c.unit.class_definitions {
		if definition.class_symbol == owner {
			if is_abstract {
				definition.is_abstract = true
			}
			return
		}
	}
	append(
		&c.unit.class_definitions,
		Class_Definition_Data{class_symbol = owner, is_abstract = is_abstract},
	)
}

collect_class_attribute_stmt :: proc(
	c: ^Collector,
	stmt: ^ast.Stmt,
	scope: Scope_Id,
	class_symbol: Symbol_Id,
	visibility: Visibility,
) {
	if class_symbol == INVALID_SYMBOL_ID {
		return
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Decl:
		infos := make([dynamic]Decl_Info, 0, 1, c.allocator)
		append(&infos, data_decl_info(n))
		collect_class_attribute_infos(c, scope, class_symbol, visibility, false, .Variable, infos[:], n.range)
	case ^ast.Data_Chained_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.decls), c.allocator)
		for clause in n.decls {
			append(&infos, data_branch_info(clause, n.range))
		}
		collect_class_attribute_infos(c, scope, class_symbol, visibility, false, .Variable, infos[:], n.range)
	case ^ast.Class_Data_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.decls), c.allocator)
		for clause in n.decls {
			append(&infos, class_data_clause_info(clause, n.range))
		}
		collect_class_attribute_infos(c, scope, class_symbol, visibility, true, .Variable, infos[:], n.range)
	case ^ast.Statics_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.statics), c.allocator)
		for clause in n.statics {
			append(&infos, statics_clause_info(clause, n.range))
		}
		collect_class_attribute_infos(c, scope, class_symbol, visibility, true, .Variable, infos[:], n.range)
	case ^ast.Constants_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.constants), c.allocator)
		for clause in n.constants {
			append(&infos, constants_clause_info(clause, n.range))
		}
		collect_class_attribute_infos(c, scope, class_symbol, visibility, true, .Constant, infos[:], n.range)
	}
}

collect_class_attribute_infos :: proc(
	c: ^Collector,
	scope: Scope_Id,
	class_symbol: Symbol_Id,
	visibility: Visibility,
	is_static: bool,
	symbol_kind: Symbol_Kind,
	infos: []Decl_Info,
	signature_range: tokenizer.Range,
) {
	for i in 0 ..< len(infos) {
		info := infos[i]
		if info.depth != 0 {
			continue
		}
		if info.kind != .Normal && info.kind != .Begin_Group {
			continue
		}
		if info.name == "" || .Common_Part_Delimiter in info.flags {
			continue
		}
		name := canonical_name(info.name, c.allocator)
		member_symbol := INVALID_SYMBOL_ID
		structure_id := INVALID_STRUCTURE_ID
		type_id := UNKNOWN_TYPE_ID
		if existing, ok := scope_lookup_declaration(c.unit, scope, .Value, name); ok {
			member_symbol = existing
			if s := symbol(c.unit, existing); s != nil {
				structure_id = s.structure
				type_id = s.type_id
			}
		} else {
			if info.kind == .Begin_Group {
				structure_id = structure_from_group(c, scope, infos, i)
				type_id = type_structure(c.unit, structure_id)
			}
		}
		decl_flags := Decl_Info_Flags{}
		if is_static {
			decl_flags += {.Is_Static}
		}
		set_member_decl_info(c, member_symbol, class_symbol, visibility, .Attribute, decl_flags)
		set_entity_signature(c, member_symbol, source_text(c, signature_range))
		if s := symbol(c.unit, member_symbol); s != nil {
			s.structure = structure_id
			s.type_id = type_id
		}
	}
}

collect_class_oop_stmt :: proc(
	c: ^Collector,
	stmt: ^ast.Oop_Simple_Stmt,
	scope: Scope_Id,
	class_symbol: Symbol_Id,
	visibility: Visibility,
) {
	#partial switch stmt.kind {
	case .Methods, .Class_Methods:
		is_static := stmt.kind == .Class_Methods
		for member in stmt.members {
			add_method_interface_qualifier_reference(c, member.qualifier, scope, member.qualifier_range)
			name := oop_method_symbol_name(member)
			member_symbol := declare_name_if_present(c, scope, name, .Method, stmt.range, class_symbol)
			parameters := method_parameters_from_signatures(c, scope, member.signatures[:])
			exceptions := method_exceptions_from_signatures(c, member.signatures[:])
			for param in parameters {
				if .Has_Declared_Type in param.flags {
					add_type_reference(c, scope, param.declared_type, param.range, param.type_clause_form, param.has_type_clause_form)
				}
			}
			decl_flags := Decl_Info_Flags{}
			if is_static {
				decl_flags += {.Is_Static}
			}
			if .Redefinition in member.flags {
				decl_flags += {.Is_Redefinition}
			}
			event_name := ""
			event_range := tokenizer.Range{}
			event_source_type := Field_Type_Ref_Data{}
			if member.event_handler.source_type != nil {
				if type_ref, type_ok := type_ref_from_expr(c, member.event_handler.source_type, .Type);
				   type_ok {
					event_name = canonical_name(member.event_handler.event_name, c.allocator)
					event_range = member.event_handler.event_range
					event_source_type = type_ref
					decl_flags += {.For_Event}
					add_type_reference(c, scope, type_ref, member.event_handler.source_type.range)
				}
			}
			set_member_decl_info(c, member_symbol, class_symbol, visibility, .Method, decl_flags)
			if event_name != "" {
				set_entity_event_signature(c, member_symbol, event_name, event_range, event_source_type)
			}
			set_entity_signature(c, member_symbol, stmt.text)
			sig_scope := declare_signature_scope_params(c, scope, stmt.range, member_symbol, parameters[:])
			set_entity_signature_scope(c, member_symbol, sig_scope)
			for exception in exceptions {
				append_signature_exception(c, member_symbol, exception.name, exception.range)
			}
		}
	case .Events, .Class_Events:
		is_static := stmt.kind == .Class_Events
		for member in stmt.members {
			member_symbol := declare_name_if_present(c, scope, member.name, .Event, stmt.range, class_symbol)
			parameters := event_parameters_from_signatures(c, scope, member.signatures[:])
			for param in parameters {
				if .Has_Declared_Type in param.flags {
					add_type_reference(c, scope, param.declared_type, param.range, param.type_clause_form, param.has_type_clause_form)
				}
			}
			decl_flags := Decl_Info_Flags{}
			if is_static {
				decl_flags += {.Is_Static}
			}
			set_member_decl_info(c, member_symbol, class_symbol, visibility, .Event, decl_flags)
			set_entity_signature(c, member_symbol, stmt.text)
			sig_scope := declare_signature_scope_params(c, scope, stmt.range, member_symbol, parameters[:])
			set_entity_signature_scope(c, member_symbol, sig_scope)
		}
	case .Interfaces:
		for member in stmt.members {
			name := canonical_name(member.name, c.allocator)
			append(
				&c.unit.implemented_interfaces,
				Implemented_Interface_Data {
					owner_symbol = class_symbol,
					interface_name = name,
					range = stmt.range,
				},
			)
			add_reference(c, scope, name, .Type, .Interface_Use, member.range)
		}
	case .Aliases:
		if len(stmt.aliases) > 0 {
			for alias in stmt.aliases {
				collect_member_alias(c, alias.name, alias.target, stmt.range, scope, class_symbol, visibility)
			}
		} else {
			for member in stmt.members {
				for sig in member.signatures {
					if sig.kind == .For && len(sig.values) > 0 {
						collect_member_alias(c, member.name, sig.values[0], stmt.range, scope, class_symbol, visibility)
						break
					}
				}
			}
		}
	case:
	}
}

collect_member_alias :: proc(
	c: ^Collector,
	name: string,
	target: ^ast.Expr,
	range: tokenizer.Range,
	scope: Scope_Id,
	class_symbol: Symbol_Id,
	visibility: Visibility,
) {
	target_ref, ok := type_ref_from_expr(c, target, .Type)
	if !ok || target_ref.base_name == "" {
		return
	}
	add_type_reference(c, scope, target_ref, range)
	target_member := ""
	if len(target_ref.field_path) > 0 {
		target_member = target_ref.field_path[0]
	}
	alias_symbol := declare_collected_symbol(
		c,
		scope,
		name,
		.Alias,
		range,
		skip_duplicate_check = true,
		owner = class_symbol,
	)
	set_entity_signature(c, alias_symbol, source_text(c, range))
	if info := entity_decl_info(c.unit, alias_symbol); info != nil {
		info.visibility = visibility
		info.alias_target_interface_name = target_ref.base_name
		info.alias_target_member_name = target_member
	}
	append(
		&c.unit.member_aliases,
		Member_Alias_Data {
			symbol = alias_symbol,
			owner_symbol = class_symbol,
			alias_name = canonical_name(name, c.allocator),
			target_interface_name = target_ref.base_name,
			target_member_name = target_member,
			range = range,
		},
	)
}

declare_signature_scope_params :: proc(
	c: ^Collector,
	parent_scope: Scope_Id,
	range: tokenizer.Range,
	owner: Entity_Id,
	parameters: []Class_Member_Parameter_Data,
) -> Scope_Id {
	if len(parameters) == 0 {
		return INVALID_SCOPE_ID
	}
	previous := c.current_scope
	c.current_scope = parent_scope
	sig_scope := push_scope(c, .Signature, range, owner)
	for i in 0 ..< len(parameters) {
		param := parameters[i]
		has_type := .Has_Declared_Type in param.flags
		parameters[i].symbol = declare_collected_symbol(
			c,
			sig_scope,
			param.name,
			.Parameter,
			param.range,
			INVALID_STRUCTURE_ID,
			param.declared_type,
			has_type,
			param.type_clause_display,
			type_clause_form = param.type_clause_form,
			has_type_clause_form = param.has_type_clause_form,
			type_clause_table_has_of = param.type_clause_table_has_of,
			type_id = param.type_id,
			owner = owner,
		)
		flags := Decl_Info_Flags{}
		if has_type {
			flags += {.Has_Declared_Type}
		}
		if .Is_Optional in param.flags {
			flags += {.Is_Optional}
		}
		set_parameter_decl_info(
			c,
			parameters[i].symbol,
			owner,
			decl_method_section(param.section),
			decl_passing(param.passing),
			flags,
		)
		append_signature_parameter(
			c,
			owner,
			parameters[i].symbol,
			param.name,
			param.range,
			decl_method_section(param.section),
			decl_passing(param.passing),
			param.type_id,
			param.declared_type,
			has_type,
			param.type_clause_display,
			param.type_clause_form,
			param.has_type_clause_form,
			param.type_clause_table_has_of,
			flags,
		)
	}
	c.current_scope = sig_scope
	pop_scope(c)
	c.current_scope = previous
	return sig_scope
}

method_parameters_from_signatures :: proc(
	c: ^Collector,
	scope: Scope_Id,
	signatures: []ast.Oop_Signature_Clause,
) -> [dynamic]Class_Member_Parameter_Data {
	parameters := make([dynamic]Class_Member_Parameter_Data, 0, 2, c.allocator)
	for sig in signatures {
		section, ok := method_section_from_oop(sig.kind)
		if !ok {
			continue
		}
		for param in sig.parameters {
			append(&parameters, class_member_parameter_from_oop(c, scope, param, section))
		}
	}
	return parameters
}

method_exceptions_from_signatures :: proc(
	c: ^Collector,
	signatures: []ast.Oop_Signature_Clause,
) -> [dynamic]Function_Module_Exception_Data {
	exceptions := make([dynamic]Function_Module_Exception_Data, 0, 1, c.allocator)
	for sig in signatures {
		if sig.kind != .Exceptions {
			continue
		}
		for value in sig.values {
			if value == nil {
				continue
			}
			if raw, ok := value.derived_expr.(^ast.Type_Ref_Expr); ok && len(raw.raw_refs) > 0 {
				for ref in raw.raw_refs {
					append(&exceptions, Function_Module_Exception_Data {
						name  = canonical_name(ref.name, c.allocator),
						range = ref.range,
					})
				}
				continue
			}
			if type_ref, ok := type_ref_from_expr(c, value, .Value); ok {
				append(&exceptions, Function_Module_Exception_Data {
					name  = type_ref.base_name,
					range = type_ref.base_range,
				})
			}
		}
	}
	return exceptions
}

event_parameters_from_signatures :: proc(
	c: ^Collector,
	scope: Scope_Id,
	signatures: []ast.Oop_Signature_Clause,
) -> [dynamic]Class_Member_Parameter_Data {
	parameters := make([dynamic]Class_Member_Parameter_Data, 0, 2, c.allocator)
	for sig in signatures {
		if sig.kind != .Exporting {
			continue
		}
		for param in sig.parameters {
			append(&parameters, class_member_parameter_from_oop(c, scope, param, .Exporting))
		}
	}
	return parameters
}

method_section_from_oop :: proc(kind: ast.Oop_Signature_Kind) -> (Method_Parameter_Section, bool) {
	#partial switch kind {
	case .Importing:
		return .Importing, true
	case .Exporting:
		return .Exporting, true
	case .Changing:
		return .Changing, true
	case .Receiving:
		return .Receiving, true
	case .Returning:
		return .Returning, true
	}
	return .Importing, false
}

class_member_parameter_from_oop :: proc(
	c: ^Collector,
	scope: Scope_Id,
	clause: ast.Oop_Parameter_Clause,
	section: Method_Parameter_Section,
) -> Class_Member_Parameter_Data {
	param := Class_Member_Parameter_Data {
		symbol  = INVALID_SYMBOL_ID,
		section = section,
		name    = canonical_name(clause.name, c.allocator),
		range   = clause.range,
		passing = parameter_passing_from_ast(clause.passing),
	}
	if clause.type_clause != nil {
		if type_ref, has_type := type_ref_from_clause(c, clause.type_clause); has_type {
			param.declared_type = type_ref
			param.flags += {.Has_Declared_Type}
		}
		param.type_clause_display = type_clause_display(c, clause.type_clause)
		if type_form, ok := type_clause_form_from_ast(clause.type_clause); ok {
			param.type_clause_form = type_form
			param.has_type_clause_form = true
		}
		param.type_clause_table_has_of = type_clause_table_has_of_from_ast(clause.type_clause)
	}
	if clause.optional {
		param.flags += {.Is_Optional}
	}
	return param
}

Header_Token :: struct {
	text:  string,
	range: tokenizer.Range,
	kind:  tokenizer.Token_Kind,
}

header_tokens :: proc(c: ^Collector, text: string, base: int) -> [dynamic]Header_Token {
	result := tokenizer.tokenize(text, c.allocator)
	tokens := make([dynamic]Header_Token, 0, len(result.tokens), c.allocator)
	for tok in result.tokens {
		if tok.kind == .Eof {
			continue
		}
		append(
			&tokens,
			Header_Token {
				text = tokenizer.token_lexeme(tok, text),
				range = tokenizer.text_range(base + tok.range.start, base + tok.range.end),
				kind = tok.kind,
			},
		)
	}
	return tokens
}

form_parameters_from_ast :: proc(
	c: ^Collector,
	clauses: []ast.Form_Parameter_Clause,
	scope: Scope_Id,
	owner: Entity_Id,
) {
	for clause in clauses {
		declared_type, has_type := type_ref_from_clause(c, clause.type_clause)
		display := type_clause_display(c, clause.type_clause)
		type_form, has_type_form := type_clause_form_from_ast(clause.type_clause)
		type_table_has_of := type_clause_table_has_of_from_ast(clause.type_clause)
		if clause.section == .Tables &&
		   display != "" &&
		   !(has_type_form && type_form_is_table_category(type_form)) {
			display = concat2(c, "STANDARD TABLE OF ", display)
		}
		structure_id := INVALID_STRUCTURE_ID
		if has_type {
			add_type_reference(c, scope, declared_type, clause.range, type_form, has_type_form)
		}
		symbol_id := declare_collected_symbol(
			c,
			scope,
			clause.name,
			.Parameter,
			clause.range,
			structure_id,
			declared_type,
			has_type,
			display,
			type_clause_form = type_form,
			has_type_clause_form = has_type_form,
			type_clause_table_has_of = type_table_has_of,
			owner = owner,
		)
		section := decl_form_section_from_ast(clause.section)
		passing := decl_passing_from_ast(clause.passing)
		decl_flags := Decl_Info_Flags{.Has_Declared_Type} if has_type else Decl_Info_Flags{}
		set_parameter_decl_info(c, symbol_id, owner, section, passing, decl_flags)
		append_signature_parameter(
			c,
			owner,
			symbol_id,
			clause.name,
			clause.range,
			section,
			passing,
			UNKNOWN_TYPE_ID,
			declared_type,
			has_type,
			display,
			type_form,
			has_type_form,
			type_table_has_of,
			decl_flags,
		)
	}
}

function_parameters_from_ast :: proc(
	c: ^Collector,
	stmt: ^ast.Function_Decl,
	scope: Scope_Id,
	owner: Entity_Id,
) {
	for clause, i in stmt.function_parameters {
		symbol_id := INVALID_SYMBOL_ID
		section := function_parameter_section_from_ast(clause.section)
		name := canonical_name(clause.name, c.allocator)
		range := clause.range
		passing := parameter_passing_from_ast(clause.passing)
		type_clause_display := type_clause_display(c, clause.type_clause)
		type_form, has_type_form := type_clause_form_from_ast(clause.type_clause)
		type_table_has_of := type_clause_table_has_of_from_ast(clause.type_clause)
		ref_type_form := type_form
		if clause.section == .Tables &&
		   type_clause_display != "" &&
		   !(has_type_form && type_form_is_table_category(type_form)) {
			type_clause_display = concat2(c, "STANDARD TABLE OF ", type_clause_display)
			if has_type_form && type_form == .Like {
				ref_type_form = .Structure
			}
		}
		declared_type := Field_Type_Ref_Data{}
		has_declared_type := false
		if type_ref, has_type := type_ref_from_clause(c, clause.type_clause); has_type {
			declared_type = type_ref
			has_declared_type = true
			add_type_reference(c, scope, type_ref, range, ref_type_form, has_type_form)
		}
		if function_parameter_needs_symbol(stmt.function_parameters[:], i, name) {
			symbol_id = declare_collected_symbol(
				c,
				scope,
				name,
				.Parameter,
				range,
				INVALID_STRUCTURE_ID,
				declared_type,
				has_declared_type,
				type_clause_display,
				type_clause_form = type_form,
				has_type_clause_form = has_type_form,
				type_clause_table_has_of = type_table_has_of,
				owner = owner,
			)
		} else if existing, ok := scope_lookup_declaration(c.unit, scope, .Value, name); ok {
			symbol_id = existing
		}
		decl_flags := Decl_Info_Flags{}
		if symbol_id != INVALID_SYMBOL_ID {
			if has_declared_type {
				decl_flags += {.Has_Declared_Type}
			}
			if .Is_Optional in clause.flags {
				decl_flags += {.Is_Optional}
			}
			if .Has_Default_Value in clause.flags {
				decl_flags += {.Has_Default_Value}
			}
			set_parameter_decl_info(
				c,
				symbol_id,
				owner,
				decl_function_section(section),
				decl_passing(passing),
				decl_flags,
			)
			append_signature_parameter(
				c,
				owner,
				symbol_id,
				name,
				range,
				decl_function_section(section),
				decl_passing(passing),
				UNKNOWN_TYPE_ID,
				declared_type,
				has_declared_type,
				type_clause_display,
				ref_type_form,
				has_type_form,
				type_table_has_of,
				decl_flags,
			)
		}
	}
	for exception, i in stmt.exceptions {
		name := canonical_name(exception.name, c.allocator)
		append_signature_exception(c, owner, name, exception.range)
		_ = declare_collected_symbol(
			c,
			scope,
			name,
			.Exception,
			exception.range,
			skip_duplicate_check = function_exception_reuses_parameter_name(
				stmt.function_parameters[:],
				stmt.exceptions[:],
				i,
				name,
			),
			owner = owner,
		)
	}
}

function_parameter_needs_symbol :: proc(
	clauses: []ast.Function_Parameter_Clause,
	index: int,
	name: string,
) -> bool {
	previous := 0
	has_import_export_pair := false
	for i in 0 ..< index {
		if !strings.equal_fold(clauses[i].name, name) {
			continue
		}
		previous += 1
		has_import_export_pair = has_import_export_pair ||
		                          function_parameter_import_export_pair(clauses[i].section, clauses[index].section)
	}
	return previous != 1 || !has_import_export_pair
}

function_parameter_import_export_pair :: #force_inline proc "contextless" (
	left, right: ast.Function_Parameter_Section,
) -> bool {
	return (left == .Importing && right == .Exporting) ||
	       (left == .Exporting && right == .Importing)
}

function_exception_reuses_parameter_name :: proc(
	parameters: []ast.Function_Parameter_Clause,
	exceptions: []ast.Function_Exception_Clause,
	index: int,
	name: string,
) -> bool {
	matching_params := 0
	for param in parameters {
		if strings.equal_fold(param.name, name) {
			matching_params += 1
		}
	}
	if matching_params != 1 {
		return false
	}
	for i in 0 ..< index {
		if strings.equal_fold(exceptions[i].name, name) {
			return false
		}
	}
	return true
}

decl_method_section :: proc(section: Method_Parameter_Section) -> Decl_Parameter_Section {
	switch section {
	case .Importing:
		return .Method_Importing
	case .Exporting:
		return .Method_Exporting
	case .Changing:
		return .Method_Changing
	case .Receiving:
		return .Method_Receiving
	case .Returning:
		return .Method_Returning
	}
	return .None
}

decl_form_section_from_ast :: proc(section: ast.Form_Parameter_Section) -> Decl_Parameter_Section {
	switch section {
	case .Tables:
		return .Form_Tables
	case .Using:
		return .Form_Using
	case .Changing:
		return .Form_Changing
	}
	return .None
}

decl_function_section :: proc(section: Function_Module_Parameter_Section) -> Decl_Parameter_Section {
	switch section {
	case .Importing:
		return .Function_Importing
	case .Exporting:
		return .Function_Exporting
	case .Changing:
		return .Function_Changing
	case .Tables:
		return .Function_Tables
	}
	return .None
}

decl_passing :: proc(passing: Parameter_Passing_Kind) -> Decl_Parameter_Passing {
	switch passing {
	case .Direct:
		return .Direct
	case .Value:
		return .Value
	case .Reference:
		return .Reference
	}
	return .None
}

decl_passing_from_ast :: proc(passing: ast.Parameter_Passing_Kind) -> Decl_Parameter_Passing {
	switch passing {
	case .Direct:
		return .Direct
	case .Value:
		return .Value
	case .Reference:
		return .Reference
	}
	return .None
}

parameter_passing_from_ast :: proc(passing: ast.Parameter_Passing_Kind) -> Parameter_Passing_Kind {
	switch passing {
	case .Direct:
		return .Direct
	case .Value:
		return .Value
	case .Reference:
		return .Reference
	}
	return .Direct
}

function_parameter_section_from_ast :: proc(
	section: ast.Function_Parameter_Section,
) -> Function_Module_Parameter_Section {
	switch section {
	case .Importing:
		return .Importing
	case .Exporting:
		return .Exporting
	case .Changing:
		return .Changing
	case .Tables:
		return .Tables
	}
	return .Importing
}

token_eq :: proc(token: Header_Token, expected: string) -> bool {
	return strings.equal_fold(token.text, expected)
}

token_ident_like :: proc(token: Header_Token) -> bool {
	return token.kind == .Ident || token.kind == .Number
}

source_text :: proc(c: ^Collector, range: tokenizer.Range) -> string {
	if range.start < 0 || range.end > len(c.unit.source) || range.start >= range.end {
		return ""
	}
	return strings.clone(c.unit.source[range.start:range.end], c.allocator)
}

enclosing_owner :: proc(c: ^Collector, scope: Scope_Id, kind: Scope_Kind) -> (Symbol_Id, bool) {
	current := scope
	for current != INVALID_SCOPE_ID {
		s := c.unit.scopes[scope_id_index(current)]
		if s.kind == kind && s.owner != INVALID_SYMBOL_ID {
			return s.owner, true
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

method_member_name :: proc(name: string) -> string {
	if _, member_name, ok := qualified_method_parts(name); ok {
		return member_name
	}
	return name
}

oop_method_symbol_name :: proc(member: ast.Oop_Member_Clause) -> string {
	if member.qualifier != "" {
		return member.name
	}
	if member.member_name != "" {
		return member.member_name
	}
	return method_member_name(member.name)
}

qualified_method_parts :: proc(name: string) -> (string, string, bool) {
	for i := len(name) - 1; i >= 0; i -= 1 {
		if name[i] == '~' {
			return name[:i], name[i + 1:], i > 0 && i + 1 < len(name)
		}
	}
	return "", "", false
}

add_method_interface_qualifier_reference :: proc(
	c: ^Collector,
	interface_name: string,
	scope: Scope_Id,
	range: tokenizer.Range,
) {
	if interface_name != "" {
		add_reference(c, scope, interface_name, .Type, .Interface_Use, range)
	}
}

note_method_implementation :: proc(
	c: ^Collector,
	member_symbol: Symbol_Id,
	range: tokenizer.Range,
) {
	if member_symbol == INVALID_SYMBOL_ID {
		return
	}
	if info := entity_decl_info(c.unit, member_symbol); info != nil {
		info.implementation_unit = c.unit.unit_id
		info.implementation_range = range
		info.flags += {.Has_Implementation}
	}
}

declare_method_scope_params :: proc(
	c: ^Collector,
	info: ^Decl_Info_Data,
	method_scope: Scope_Id,
	owner: Entity_Id,
) {
	if info == nil {
		return
	}
	for param in info.signature_parameters {
		has_type := .Has_Declared_Type in param.flags
		symbol_id := declare_collected_symbol(
			c,
			method_scope,
			param.name,
			.Parameter,
			param.range,
			INVALID_STRUCTURE_ID,
			param.declared_type,
			has_type,
			param.type_clause_display,
			type_clause_form = param.type_clause_form,
			has_type_clause_form = param.has_type_clause_form,
			type_clause_table_has_of = param.type_clause_table_has_of,
			type_id = param.type_id,
			owner = owner,
		)
		set_parameter_decl_info(
			c,
			symbol_id,
			owner,
			param.section,
			param.passing,
			param.flags,
		)
	}
	for exception in info.signature_exceptions {
		_ = declare_collected_symbol(c, method_scope, exception.name, .Exception, exception.range, owner = owner)
	}
}

collect_amdp_using_refs :: proc(c: ^Collector, stmt: ^ast.Method_Decl, scope: Scope_Id) {
	if !stmt.is_amdp {
		return
	}
	tokens := header_tokens(c, stmt.header_text, stmt.header_range.start)
	using_index := -1
	for i in 0 ..< len(tokens) {
		if token_eq(tokens[i], "USING") {
			using_index = i
			break
		}
	}
	if using_index < 0 {
		return
	}
	for i in using_index + 1 ..< len(tokens) {
		if token_ident_like(tokens[i]) {
			add_reference(c, scope, tokens[i].text, .Type, .Type_Ref, tokens[i].range)
		}
	}
}

expr_name :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Type_Ref_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Literal_Expr:
		return n.value, n.range, n.value != ""
	}
	return "", tokenizer.Range{}, false
}

collect_provided_names :: proc(c: ^Collector) {
	for s in c.unit.symbols {
		if s.scope == c.unit.root_scope &&
		   !symbol_kind_is_builtin(s.kind) &&
		   (s.kind == .Class || s.kind == .Interface || s.kind == .Report || s.kind == .Type_Def) {
			add_provided_name(c, s.name)
		}
	}
	stem := uri_file_stem(c.unit.uri)
	if stem != "" {
		add_provided_name(c, stem)
	}
}

add_provided_name :: proc(c: ^Collector, name: string) {
	canonical := canonical_name(name, c.allocator)
	for existing in c.unit.provided_names {
		if existing == canonical {
			return
		}
	}
	append(&c.unit.provided_names, canonical)
}

uri_file_stem :: proc(uri: string) -> string {
	start := 0
	for i in 0 ..< len(uri) {
		if uri[i] == '/' || uri[i] == '\\' {
			start = i + 1
		}
	}
	end := len(uri)
	for i in start ..< len(uri) {
		if uri[i] == '.' {
			end = i
		}
	}
	if end <= start {
		return ""
	}
	return uri[start:end]
}

canonical_name :: #force_inline proc(name: string, allocator: mem.Allocator) -> string {
	return strings.to_lower(name, allocator)
}
