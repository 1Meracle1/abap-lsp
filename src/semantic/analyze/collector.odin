package abap_frontend_semantic_analyze

import "src:ast"
import "src:parser"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Collector :: struct {
	source:                                 string,
	mode:                                   Source_Mode,
	uri:                                    string,
	unit_id:                                Unit_Id,
	root:                                   ^ast.File,
	allocator:                              mem.Allocator,
	scope_symbols:                          map[Scope_Index_Key]Symbol_Id,
	declared_scope_symbols:                 map[Scope_Index_Key]Symbol_Id,
	forward_type_symbols:                   map[Symbol_Id]bool,
	root_scope:                             Scope_Id,
	current_scope:                          Scope_Id,
	scopes:                                 [dynamic]Scope_Data,
	symbols:                                [dynamic]Symbol_Data,
	structures:                             [dynamic]Structure_Data,
	references:                             [dynamic]Reference_Data,
	message_default_class:                  Message_Class_Use_Data,
	has_message_default_class:              bool,
	message_uses:                           [dynamic]Message_Use_Data,
	message_class_entries:                  [dynamic]Message_Class_Entry_Data,
	diagnostics:                            [dynamic]Diagnostic,
	include_edges:                          [dynamic]Include_Edge,
	table_work_areas:                       [dynamic]Table_Work_Area_Data,
	selection_screen_report_type_positions: [dynamic]tokenizer.Range,
	field_accesses:                         [dynamic]Field_Access,
	loop_where_field_contexts:              [dynamic]Loop_Where_Field_Context,
	loop_at_field_contexts:                 [dynamic]Loop_At_Field_Context,
	constructor_for_bindings:               [dynamic]Constructor_For_Binding_Data,
	class_members:                          [dynamic]Class_Member_Data,
	class_definitions:                      [dynamic]Class_Definition_Data,
	class_inheritance:                      [dynamic]Class_Inheritance_Data,
	implemented_interfaces:                 [dynamic]Implemented_Interface_Data,
	member_aliases:                         [dynamic]Member_Alias_Data,
	form_routines:                          [dynamic]Form_Routine_Data,
	function_modules:                       [dynamic]Function_Module_Data,
	named_arguments:                        [dynamic]Named_Argument_Access,
	call_sites:                             [dynamic]Call_Site_Data,
	assignment_sites:                       [dynamic]Assignment_Site_Data,
	concatenate_lines_of_sites:             [dynamic]Concatenate_Lines_Of_Site_Data,
	expression_facts:                       [dynamic]Expression_Fact_Data,
	value_flow_edges:                       [dynamic]Value_Flow_Edge_Data,
	perform_calls:                          [dynamic]Perform_Call_Data,
	find_sites:                             [dynamic]Find_Site_Data,
	system_field_updates:                   [dynamic]System_Field_Update_Data,
	routine_sites:                          [dynamic]Routine_Site_Data,
	internal_table_orders:                  [dynamic]Internal_Table_Order_Data,
	read_table_binary_searches:             [dynamic]Read_Table_Binary_Search_Data,
	field_symbol_state_checks:              [dynamic]Field_Symbol_State_Check_Data,
	value_state_checks:                     [dynamic]Value_State_Check_Data,
	routine_control_regions:                [dynamic]Routine_Control_Region_Data,
	sql_queries:                            [dynamic]Sql_Query_Data,
	sql_sources:                            [dynamic]Sql_Source_Data,
	sql_dynamic_fragments:                  [dynamic]Sql_Dynamic_Fragment_Data,
	sql_projections:                        [dynamic]Sql_Projection_Data,
	sql_name_refs:                          [dynamic]Sql_Name_Ref_Data,
	sql_predicates:                         [dynamic]Sql_Predicate_Data,
	sql_targets:                            [dynamic]Sql_Target_Data,
	create_data_type_handles:               [dynamic]Create_Data_Type_Handle_Site_Data,
	provided_names:                         [dynamic]string,
	loop_source_stack:                      [dynamic]Field_Access,
	structured_groups:                      [dynamic]Structured_Group_Frame,
	unit:                                   Unit_Analysis,
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
		source                                 = source,
		mode                                   = mode,
		uri                                    = unit.uri,
		unit_id                                = unit_id,
		root                                   = parsed.root,
		allocator                              = allocator,
		scope_symbols                          = make(map[Scope_Index_Key]Symbol_Id, len(unit.symbols) * 2 + 64, allocator),
		declared_scope_symbols                 = make(map[Scope_Index_Key]Symbol_Id, len(unit.symbols), allocator),
		forward_type_symbols                   = make(map[Symbol_Id]bool, 16, allocator),
		root_scope                             = unit.root_scope,
		current_scope                          = unit.root_scope,
		scopes                                 = unit.scopes,
		symbols                                = unit.symbols,
		structures                             = unit.structures,
		references                             = unit.references,
		message_default_class                  = unit.message_default_class,
		has_message_default_class              = unit.has_message_default_class,
		message_uses                           = unit.message_uses,
		message_class_entries                  = unit.message_class_entries,
		diagnostics                            = unit.diagnostics,
		include_edges                          = unit.include_edges,
		table_work_areas                       = unit.table_work_areas,
		selection_screen_report_type_positions = unit.selection_screen_report_type_positions,
		field_accesses                         = unit.field_accesses,
		loop_where_field_contexts              = unit.loop_where_field_contexts,
		loop_at_field_contexts                 = unit.loop_at_field_contexts,
		constructor_for_bindings               = unit.constructor_for_bindings,
		class_members                          = unit.class_members,
		class_definitions                      = unit.class_definitions,
		class_inheritance                      = unit.class_inheritance,
		implemented_interfaces                 = unit.implemented_interfaces,
		member_aliases                         = unit.member_aliases,
		form_routines                          = unit.form_routines,
		function_modules                       = unit.function_modules,
		named_arguments                        = unit.named_arguments,
		call_sites                             = unit.call_sites,
		assignment_sites                       = unit.assignment_sites,
		concatenate_lines_of_sites             = unit.concatenate_lines_of_sites,
		expression_facts                       = unit.expression_facts,
		value_flow_edges                       = unit.value_flow_edges,
		perform_calls                          = unit.perform_calls,
		find_sites                             = unit.find_sites,
		system_field_updates                   = unit.system_field_updates,
		routine_sites                          = unit.routine_sites,
		internal_table_orders                  = unit.internal_table_orders,
		read_table_binary_searches             = unit.read_table_binary_searches,
		field_symbol_state_checks              = unit.field_symbol_state_checks,
		value_state_checks                     = unit.value_state_checks,
		routine_control_regions                = unit.routine_control_regions,
		sql_queries                            = unit.sql_queries,
		sql_sources                            = unit.sql_sources,
		sql_dynamic_fragments                  = unit.sql_dynamic_fragments,
		sql_projections                        = unit.sql_projections,
		sql_name_refs                          = unit.sql_name_refs,
		sql_predicates                         = unit.sql_predicates,
		sql_targets                            = unit.sql_targets,
		create_data_type_handles               = unit.create_data_type_handles,
		provided_names                         = unit.provided_names,
		loop_source_stack                      = make([dynamic]Field_Access, 0, 4, allocator),
		structured_groups                      = make([dynamic]Structured_Group_Frame, 0, 2, allocator),
		unit                                   = unit,
	}
	seed_collector_scope_symbols(&c)

	if c.root != nil {
		for stmt in c.root.stmts {
			if c.mode == .Dependency_Interface {
				walk_dependency_interface_stmt(&c, stmt, c.root_scope)
			} else {
				walk_stmt(&c, stmt, c.root_scope)
			}
		}
	}
	collect_provided_names(&c)
	return finish_collector(&c)
}

finish_collector :: proc(c: ^Collector) -> Unit_Analysis {
	c.unit.scopes = c.scopes
	c.unit.symbols = c.symbols
	c.unit.structures = c.structures
	c.unit.references = c.references
	c.unit.message_default_class = c.message_default_class
	c.unit.has_message_default_class = c.has_message_default_class
	c.unit.message_uses = c.message_uses
	c.unit.message_class_entries = c.message_class_entries
	c.unit.diagnostics = c.diagnostics
	c.unit.include_edges = c.include_edges
	c.unit.table_work_areas = c.table_work_areas
	c.unit.selection_screen_report_type_positions = c.selection_screen_report_type_positions
	c.unit.field_accesses = c.field_accesses
	c.unit.loop_where_field_contexts = c.loop_where_field_contexts
	c.unit.loop_at_field_contexts = c.loop_at_field_contexts
	c.unit.constructor_for_bindings = c.constructor_for_bindings
	c.unit.class_members = c.class_members
	c.unit.class_definitions = c.class_definitions
	c.unit.class_inheritance = c.class_inheritance
	c.unit.implemented_interfaces = c.implemented_interfaces
	c.unit.member_aliases = c.member_aliases
	c.unit.form_routines = c.form_routines
	c.unit.function_modules = c.function_modules
	c.unit.named_arguments = c.named_arguments
	c.unit.call_sites = c.call_sites
	c.unit.assignment_sites = c.assignment_sites
	c.unit.concatenate_lines_of_sites = c.concatenate_lines_of_sites
	c.unit.expression_facts = c.expression_facts
	c.unit.value_flow_edges = c.value_flow_edges
	c.unit.perform_calls = c.perform_calls
	c.unit.find_sites = c.find_sites
	c.unit.system_field_updates = c.system_field_updates
	c.unit.routine_sites = c.routine_sites
	c.unit.internal_table_orders = c.internal_table_orders
	c.unit.read_table_binary_searches = c.read_table_binary_searches
	c.unit.field_symbol_state_checks = c.field_symbol_state_checks
	c.unit.value_state_checks = c.value_state_checks
	c.unit.routine_control_regions = c.routine_control_regions
	c.unit.sql_queries = c.sql_queries
	c.unit.sql_sources = c.sql_sources
	c.unit.sql_dynamic_fragments = c.sql_dynamic_fragments
	c.unit.sql_projections = c.sql_projections
	c.unit.sql_name_refs = c.sql_name_refs
	c.unit.sql_predicates = c.sql_predicates
	c.unit.sql_targets = c.sql_targets
	c.unit.create_data_type_handles = c.create_data_type_handles
	c.unit.provided_names = c.provided_names
	return c.unit
}

push_scope :: proc(
	c: ^Collector,
	kind: Scope_Kind,
	range: tokenizer.Range,
	owner := INVALID_SYMBOL_ID,
) -> Scope_Id {
	id := Scope_Id(u32(len(c.scopes)))
	scope := Scope_Data {
		id           = id,
		kind         = kind,
		range        = range,
		parent       = c.current_scope,
		owner        = owner,
		declarations = make([dynamic]Symbol_Id, 0, 8, c.allocator),
		children     = make([dynamic]Scope_Id, 0, 4, c.allocator),
	}
	append(&c.scopes, scope)
	if c.current_scope != INVALID_SCOPE_ID {
		append(&c.scopes[scope_id_index(c.current_scope)].children, id)
	}
	c.current_scope = id
	return id
}

pop_scope :: proc(c: ^Collector) {
	if c.current_scope == c.root_scope {
		return
	}
	parent := c.scopes[scope_id_index(c.current_scope)].parent
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
) -> Symbol_Id {
	canonical := canonical_name(name, c.allocator)
	check_duplicate_or_shadow(c, scope, canonical, kind, decl_range)

	id := Symbol_Id(u32(len(c.symbols)))
	append(
		&c.symbols,
		Symbol_Data {
			id = id,
			name = canonical,
			kind = kind,
			scope = scope,
			decl_range = decl_range,
			structure = structure,
			declared_type = declared_type,
			has_declared_type = has_declared_type,
			type_clause_display = strings.clone(type_clause_display, c.allocator) if type_clause_display != "" else "",
			value_clause_display = strings.clone(value_clause_display, c.allocator) if value_clause_display != "" else "",
			type_clause_form = type_clause_form,
			has_type_clause_form = has_type_clause_form,
		},
	)
	append(&c.scopes[scope_id_index(scope)].declarations, id)
	index_collected_symbol(c, scope, canonical, kind, id)
	return id
}

seed_collector_scope_symbols :: proc(c: ^Collector) {
	for symbol in c.symbols {
		index_collected_symbol(c, symbol.scope, symbol.name, symbol.kind, symbol.id)
	}
}

index_collected_symbol :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	id: Symbol_Id,
) {
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if !symbol_kind_occupies(kind, namespace) {
			continue
		}
		key := Scope_Index_Key{scope = scope, namespace = namespace, name = name}
		if _, exists := c.scope_symbols[key]; !exists {
			c.scope_symbols[key] = id
		}
		if !symbol_kind_is_builtin(kind) {
			if _, exists := c.declared_scope_symbols[key]; !exists {
				c.declared_scope_symbols[key] = id
			}
		}
	}
}

add_reference :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	namespace: Namespace,
	kind: Reference_Kind,
	range: tokenizer.Range,
	type_is_ref := false,
) {
	id := Reference_Id(u32(len(c.references)))
	append(
		&c.references,
		Reference_Data {
			id = id,
			name = canonical_name(name, c.allocator),
			namespace = namespace,
			kind = kind,
			scope = scope,
			range = range,
			type_is_ref = type_is_ref,
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
	parent := c.scopes[scope_id_index(scope)].parent
	for parent != INVALID_SCOPE_ID {
		if scope_has_symbol(c, parent, name, kind) {
			add_diagnostic(c, .Shadowed_Symbol, range, "declaration shadows outer symbol")
			return
		}
		parent = c.scopes[scope_id_index(parent)].parent
	}
}

scope_has_symbol :: proc(c: ^Collector, scope: Scope_Id, name: string, kind: Symbol_Kind) -> bool {
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if symbol_kind_occupies(kind, namespace) {
			if _, ok := c.declared_scope_symbols[Scope_Index_Key{scope = scope, namespace = namespace, name = name}]; ok {
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
		if id, ok := c.scope_symbols[Scope_Index_Key{scope = scope, namespace = namespace, name = canonical}]; ok {
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
	if !ok || c.symbols[symbol_id_index(id)].kind != kind {
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
		&c.diagnostics,
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
		declare_name_if_present(c, scope, n.name, .Variable, n.range)
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
		add_routine_site(c, scope, n.range, .Unknown_Effect)
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
		add_routine_site(c, scope, n.range, .Unknown_Effect)
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
		add_routine_site(c, scope, n.range, .Unknown_Effect)
	case ^ast.Wait_Stmt:
		collect_expr_refs(c, n.condition, scope)
		collect_expr_refs(c, n.duration, scope)
		add_routine_site(c, scope, n.range, .Unknown_Effect)
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
		elseif_scopes := make([dynamic]Scope_Id, 0, len(n.elseif_clauses), c.allocator)
		for clause in n.elseif_clauses {
			branch_scope := walk_body_in_scope(c, .Elseif_Branch, clause.range, clause.body)
			collect_expr_refs(c, clause.condition, branch_scope)
			append(&elseif_scopes, branch_scope)
		}
		else_scope := INVALID_SCOPE_ID
		if n.else_clause != nil {
			else_scope = walk_body_in_scope(
				c,
				.Else_Branch,
				n.else_clause.range,
				n.else_clause.body,
			)
		}
		add_if_region(c, scope, n.range, then_scope, elseif_scopes, else_scope)
	case ^ast.Case_Stmt:
		collect_expr_refs(c, n.expr, scope)
		when_scopes := make([dynamic]Scope_Id, 0, len(n.whens), c.allocator)
		has_others := false
		for clause in n.whens {
			when_scope := walk_body_in_scope(c, .When_Branch, clause.range, clause.body)
			collect_expr_list_refs(c, clause.operands[:], when_scope)
			append(&when_scopes, when_scope)
			has_others = has_others || clause.is_others
		}
		walk_stmt_list(c, n.recovery, scope)
		add_case_region(c, scope, n.range, when_scopes, has_others)
	case ^ast.While_Stmt:
		loop_scope := walk_body_in_scope(c, .While_Block, n.range, n.body)
		collect_expr_refs(c, n.condition, loop_scope)
		add_system_field_update(c, scope, n.range, .While, "index")
		add_loop_region(c, scope, n.range, .While, loop_scope)
	case ^ast.Do_Stmt:
		loop_scope := walk_body_in_scope(c, .Do_Block, n.range, n.body)
		collect_expr_refs(c, n.count, loop_scope)
		add_system_field_update(c, scope, n.range, .Do, "index")
		add_loop_region(c, scope, n.range, .Do, loop_scope)
	case ^ast.Loop_Stmt:
		collect_loop_stmt_facts(c, n, scope)
	case ^ast.At_Stmt:
		collect_at_stmt_facts(c, n, scope)
	case ^ast.Try_Stmt:
		body_scope := walk_body_in_scope(c, .Try_Block, n.range, n.body)
		catch_scopes := make([dynamic]Scope_Id, 0, len(n.catches), c.allocator)
		for clause in n.catches {
			catch_scope := walk_catch_clause_facts(c, clause, body_scope)
			append(&catch_scopes, catch_scope)
		}
		cleanup_scope := INVALID_SCOPE_ID
		if n.cleanup != nil {
			cleanup_scope = walk_body_in_scope(c, .Cleanup_Clause, n.cleanup.range, n.cleanup.body)
		}
		add_try_region(c, scope, n.range, body_scope, catch_scopes, cleanup_scope)
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
		add_routine_site(c, scope, n.range, .Unknown_Effect)
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
		add_routine_site(c, scope, n.range, .Unknown_Effect)
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
) -> Symbol_Id {
	if name == "" {
		return INVALID_SYMBOL_ID
	}
	return declare_collected_symbol(c, scope, name, kind, range)
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
				_ = declare_collected_symbol(c, scope, info.name, kind, info.range, structure_id)
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
				add_type_reference(c, scope, type_ref, info.range)
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
	symbol_id := declare_collected_symbol(c, scope, info.name, kind, info.range, INVALID_STRUCTURE_ID)
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
	structure_id := push_collected_structure(c, frame.name, frame.fields)
	if frame.symbol != INVALID_SYMBOL_ID {
		c.symbols[symbol_id_index(frame.symbol)].structure = structure_id
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
	value_display := value_clause_display(c, info.value_clause)
	structure_id := INVALID_STRUCTURE_ID
	if has_type {
		if info.type_clause != nil && info.type_clause.form == .Range_Of {
			structure_id = push_range_structure(c, scope, info.name, declared_type)
		} else if resolved, ok := resolve_field_type_ref(c, scope, declared_type); ok {
			structure_id = resolved
		}
		add_type_reference(c, scope, declared_type, info.range)
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
	structure_id := INVALID_STRUCTURE_ID
	if has_type {
		if resolved, ok := resolve_field_type_ref(c, scope, declared_type); ok {
			structure_id = resolved
		}
		add_type_reference(c, scope, declared_type, range)
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
	)
}

type_clause_form_from_ast :: proc(clause: ^ast.Data_Type_Clause) -> (ast.Data_Type_Form, bool) {
	if clause == nil {
		return {}, false
	}
	return clause.form, true
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
	append(&c.table_work_areas, Table_Work_Area_Data{name = name, scope = scope, range = range})
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
		c.symbols[symbol_id_index(symbol_id)].value_clause_display = value_display
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
					decl_unit = c.unit_id,
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
	return push_collected_structure(c, infos[start].name, fields)
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
	structure_id := INVALID_STRUCTURE_ID
	if has_type {
		if resolved, ok := resolve_field_type_ref(c, scope, type_ref); ok {
			structure_id = resolved
		}
		add_type_reference(c, scope, type_ref, info.range)
	}
	flags := Structure_Field_Flags{.Has_Decl_Range}
	if has_type {
		flags += {.Has_Type_Ref}
	}
	return Structure_Field_Data {
			name = canonical_name(info.name, c.allocator),
			decl_range = info.range,
			decl_unit = c.unit_id,
			structure = structure_id,
			type_ref = type_ref,
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
	add_type_reference(c, scope, type_ref, info.range)
	resolved := INVALID_STRUCTURE_ID
	if found, found_ok := resolve_field_type_ref(c, scope, type_ref); found_ok {
		resolved = found
		for source in c.structures {
			if source.id == found {
				for field in source.fields {
					next := field
					if info.renaming_suffix != "" {
						next.name = concat2(c, field.name, info.renaming_suffix)
					}
					append(fields, next)
				}
				break
			}
		}
	}
	if info.as_name != "" {
		flags := Structure_Field_Flags{.Has_Type_Ref}
		append(
			fields,
			Structure_Field_Data {
				name = canonical_name(info.as_name, c.allocator),
				decl_unit = c.unit_id,
				structure = resolved,
				type_ref = type_ref,
				flags = flags,
			},
		)
	}
}

push_collected_structure :: proc(
	c: ^Collector,
	name: string,
	fields: [dynamic]Structure_Field_Data,
) -> Structure_Id {
	id := Structure_Id(u32(len(c.structures)))
	append(
		&c.structures,
		Structure_Data {
			id = id,
			origin_unit = c.unit_id,
			origin_structure = id,
			name = canonical_name(name, c.allocator),
			fields = fields,
		},
	)
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
	)
}

range_field :: proc(
	c: ^Collector,
	scope: Scope_Id,
	name: string,
	type_ref: Field_Type_Ref_Data,
) -> Structure_Field_Data {
	structure_id := INVALID_STRUCTURE_ID
	if resolved, ok := resolve_field_type_ref(c, scope, type_ref); ok {
		structure_id = resolved
	}
	return Structure_Field_Data {
		name = strings.clone(name, c.allocator),
		decl_unit = c.unit_id,
		structure = structure_id,
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
		}
		append(&type_ref.field_path, canonical_name(name, c.allocator))
		append(&type_ref.field_ranges, range)
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
	for segment in expr.path {
		append(&field_path, canonical_name(segment.name, c.allocator))
		append(&field_ranges, segment.range)
	}
	return Field_Type_Ref_Data {
			namespace = ns,
			is_ref = is_ref,
			base_name = canonical_name(base, c.allocator),
			base_range = base_range,
			field_path = field_path,
			field_ranges = field_ranges,
		},
		true
}

type_clause_display :: proc(c: ^Collector, clause: ^ast.Data_Type_Clause) -> string {
	if clause == nil {
		return ""
	}
	ref := type_ref_display(c, clause.type_ref)
	#partial switch clause.form {
	case .Ref_To:
		return concat2(c, "REF TO ", ref)
	case .Like_Line_Of, .Type_Line_Of:
		return concat2(c, "LINE OF ", ref)
	case .Any_Table:
		return strings.clone("ANY TABLE", c.allocator) if clause.type_ref == nil else concat2(c, "ANY TABLE OF ", ref)
	case .Table, .Like_Table:
		return concat2(c, "TABLE OF ", ref)
	case .Index_Table:
		return strings.clone("INDEX TABLE", c.allocator) if clause.type_ref == nil else concat2(c, "INDEX TABLE OF ", ref)
	case .Standard_Table, .Like_Standard_Table:
		return concat2(c, "STANDARD TABLE OF ", ref)
	case .Sorted_Table, .Like_Sorted_Table:
		return concat2(c, "SORTED TABLE OF ", ref)
	case .Hashed_Table, .Like_Hashed_Table:
		return concat2(c, "HASHED TABLE OF ", ref)
	case .Range_Of:
		return concat2(c, "RANGE OF ", ref)
	case:
		return ref
	}
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
	   expr.range.end <= len(c.source) &&
	   expr.range.start < expr.range.end {
		return strings.clone(c.source[expr.range.start:expr.range.end], c.allocator)
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
) {
	if type_ref.base_name == "" {
		return
	}
	base_range := type_ref.base_range
	if base_range.start >= base_range.end {
		base_range = range
	}
	add_reference(c, scope, type_ref.base_name, type_ref.namespace, .Type_Ref, base_range, type_ref.is_ref)
	if len(type_ref.field_path) > 0 {
		segments := make([dynamic]Field_Access_Segment, 0, len(type_ref.field_path), c.allocator)
		for name, i in type_ref.field_path {
			segment_range := range
			if i < len(type_ref.field_ranges) && type_ref.field_ranges[i].start < type_ref.field_ranges[i].end {
				segment_range = type_ref.field_ranges[i]
			}
			append(&segments, Field_Access_Segment{name = name, range = segment_range})
		}
		append(
			&c.field_accesses,
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

resolve_field_type_ref :: proc(
	c: ^Collector,
	scope: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (
	Structure_Id,
	bool,
) {
	if type_ref.base_name == "" {
		return INVALID_STRUCTURE_ID, false
	}
	symbol_id, ok := lookup_symbol_in_scope_chain(c, scope, type_ref.base_name, type_ref.namespace)
	if !ok && type_ref.namespace == .Type {
		symbol_id, ok = lookup_symbol_in_scope_chain(c, scope, type_ref.base_name, .Value)
	}
	if !ok {
		return INVALID_STRUCTURE_ID, false
	}
	s := c.symbols[symbol_id_index(symbol_id)]
	if s.structure == INVALID_STRUCTURE_ID {
		return INVALID_STRUCTURE_ID, false
	}
	return resolve_structure_path(c, s.structure, type_ref.field_path[:])
}

resolve_structure_path :: proc(
	c: ^Collector,
	id: Structure_Id,
	path: []string,
) -> (
	Structure_Id,
	bool,
) {
	current := id
	for segment in path {
		st := find_collected_structure(c, current)
		if st == nil {
			return INVALID_STRUCTURE_ID, false
		}
		found := false
		next := INVALID_STRUCTURE_ID
		for field in st.fields {
			if strings.equal_fold(field.name, segment) &&
			   field.structure != INVALID_STRUCTURE_ID {
				found = true
				next = field.structure
				break
			}
		}
		if !found {
			return INVALID_STRUCTURE_ID, false
		}
		current = next
	}
	return current, true
}

find_collected_structure :: proc(c: ^Collector, id: Structure_Id) -> ^Structure_Data {
	for &st in c.structures {
		if st.id == id {
			return &st
		}
	}
	return nil
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
		if scope_idx < 0 || scope_idx >= len(c.scopes) {
			break
		}
		if id, ok := c.scope_symbols[Scope_Index_Key{scope = current, namespace = namespace, name = name}]; ok {
			return id, true
		}
		current = c.scopes[scope_idx].parent
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
			&c.include_edges,
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
	if c.mode == .Dependency_Interface && .Implementation in stmt.flags {
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
		if stmt.superclass_name != "" {
			superclass := canonical_name(stmt.superclass_name, c.allocator)
			append(
				&c.class_inheritance,
				Class_Inheritance_Data{class_symbol = owner, superclass_name = superclass},
			)
			add_reference(c, scope, superclass, .Type, .Type_Ref, stmt.superclass_range)
		}
	}
	previous := c.current_scope
	c.current_scope = scope
	class_scope := push_scope(c, .Class, stmt.range, owner)
	if !(.Implementation in stmt.flags) && !(.Bodyless in stmt.flags) {
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
				if c.mode == .Dependency_Interface && visibility == .Private {
					continue
				}
				collect_class_oop_stmt(c, oop, scope, owner, visibility)
			}
			continue
		}
		if c.mode == .Dependency_Interface && visibility == .Private {
			continue
		}
		collect_class_attribute_stmt(c, child, scope, owner, visibility)
		walk_stmt(c, child, scope)
	}
	c.current_scope = previous
}

walk_method_decl :: proc(c: ^Collector, stmt: ^ast.Method_Decl, scope: Scope_Id) {
	owner := declare_name_if_present(c, scope, stmt.name, .Method, stmt.header_range)
	add_method_interface_qualifier_reference(c, stmt.qualifier, scope, stmt.qualifier_range)
	previous := c.current_scope
	c.current_scope = scope
	method_scope := push_scope(c, .Method, stmt.range, owner)
	if class_owner, ok := enclosing_owner(c, scope, .Class); ok {
		method_name := stmt.member_name
		if method_name == "" {
			method_name = method_member_name(stmt.name)
		}
		note_method_implementation(c, class_owner, method_name, stmt.header_range)
		declare_method_scope_params(c, class_owner, method_name, method_scope)
		member := class_member(c, class_owner, method_name)
		if member == nil || !(.Is_Static in member.flags) {
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
					base_name = c.symbols[symbol_id_index(class_owner)].name,
				},
				true,
				concat2(c, "REF TO ", c.symbols[symbol_id_index(class_owner)].name),
			)
		}
	}
	if c.mode != .Dependency_Interface {
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
	parameters := form_parameters_from_ast(c, stmt.form_parameters[:], form_scope)
	append(
		&c.form_routines,
		Form_Routine_Data {
			symbol = owner,
			signature = strings.clone(stmt.header_text, c.allocator),
			parameters = parameters,
		},
	)
	if c.mode != .Dependency_Interface {
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
	parameters, exceptions := function_parameters_from_ast(c, stmt, function_scope)
	append(
		&c.function_modules,
		Function_Module_Data {
			symbol = owner,
			signature = strings.clone(stmt.header_text, c.allocator),
			parameters = parameters,
			exceptions = exceptions,
		},
	)
	if c.mode != .Dependency_Interface {
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
			name = member.member_name
			if name == "" {
				name = method_member_name(member.name)
			}
		}
		declare_name_if_present(c, scope, name, kind, stmt.range)
	}
}

add_class_definition :: proc(c: ^Collector, owner: Symbol_Id, is_abstract: bool) {
	if owner == INVALID_SYMBOL_ID {
		return
	}
	for &definition in c.class_definitions {
		if definition.class_symbol == owner {
			if is_abstract {
				definition.is_abstract = true
			}
			return
		}
	}
	append(
		&c.class_definitions,
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
		collect_class_attribute_infos(c, scope, class_symbol, visibility, false, infos[:], n.range)
	case ^ast.Data_Chained_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.decls), c.allocator)
		for clause in n.decls {
			append(&infos, data_branch_info(clause, n.range))
		}
		collect_class_attribute_infos(c, scope, class_symbol, visibility, false, infos[:], n.range)
	case ^ast.Class_Data_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.decls), c.allocator)
		for clause in n.decls {
			append(&infos, class_data_clause_info(clause, n.range))
		}
		collect_class_attribute_infos(c, scope, class_symbol, visibility, true, infos[:], n.range)
	case ^ast.Statics_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.statics), c.allocator)
		for clause in n.statics {
			append(&infos, statics_clause_info(clause, n.range))
		}
		collect_class_attribute_infos(c, scope, class_symbol, visibility, true, infos[:], n.range)
	case ^ast.Constants_Decl:
		infos := make([dynamic]Decl_Info, 0, len(n.constants), c.allocator)
		for clause in n.constants {
			append(&infos, constants_clause_info(clause, n.range))
		}
		collect_class_attribute_infos(c, scope, class_symbol, visibility, true, infos[:], n.range)
	}
}

collect_class_attribute_infos :: proc(
	c: ^Collector,
	scope: Scope_Id,
	class_symbol: Symbol_Id,
	visibility: Visibility,
	is_static: bool,
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
		structure_id := INVALID_STRUCTURE_ID
		if info.kind == .Begin_Group {
			structure_id = structure_from_group(c, scope, infos, i)
		} else if type_ref, ok := type_ref_from_clause(c, info.type_clause); ok {
			if resolved, resolved_ok := resolve_field_type_ref(c, scope, type_ref); resolved_ok {
				structure_id = resolved
			}
		}
		flags := Class_Member_Flags{}
		if is_static {
			flags += {.Is_Static}
		}
		append(
			&c.class_members,
			Class_Member_Data {
				class_symbol = class_symbol,
				name = canonical_name(info.name, c.allocator),
				kind = .Attribute,
				visibility = visibility,
				decl_range = info.range,
				signature = source_text(c, signature_range),
				parameters = make([dynamic]Class_Member_Parameter_Data, 0, 0, c.allocator),
				structure = structure_id,
				flags = flags,
			},
		)
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
			name := member.member_name
			if name == "" {
				name = method_member_name(member.name)
			}
			declare_name_if_present(c, scope, name, .Method, stmt.range)
			parameters := method_parameters_from_signatures(c, member.signatures[:])
			exceptions := method_exceptions_from_signatures(c, member.signatures[:])
			for param in parameters {
				if .Has_Declared_Type in param.flags {
					add_type_reference(c, scope, param.declared_type, param.range)
				}
			}
			flags := Class_Member_Flags{}
			if is_static {
				flags += {.Is_Static}
			}
			if .Redefinition in member.flags {
				flags += {.Is_Redefinition}
			}
			append(
				&c.class_members,
				Class_Member_Data {
					class_symbol = class_symbol,
					name = canonical_name(name, c.allocator),
					kind = .Method,
					visibility = visibility,
					decl_range = stmt.range,
					signature = strings.clone(stmt.text, c.allocator),
					parameters = parameters,
					exceptions = exceptions,
					structure = INVALID_STRUCTURE_ID,
					flags = flags,
				},
			)
			declare_signature_scope_params(c, scope, stmt.range, parameters[:])
		}
	case .Events, .Class_Events:
		is_static := stmt.kind == .Class_Events
		for member in stmt.members {
			declare_name_if_present(c, scope, member.name, .Event, stmt.range)
			parameters := event_parameters_from_signatures(c, member.signatures[:])
			for param in parameters {
				if .Has_Declared_Type in param.flags {
					add_type_reference(c, scope, param.declared_type, param.range)
				}
			}
			flags := Class_Member_Flags{}
			if is_static {
				flags += {.Is_Static}
			}
			append(
				&c.class_members,
				Class_Member_Data {
					class_symbol = class_symbol,
					name = canonical_name(member.name, c.allocator),
					kind = .Event,
					visibility = visibility,
					decl_range = stmt.range,
					signature = strings.clone(stmt.text, c.allocator),
					parameters = parameters,
					structure = INVALID_STRUCTURE_ID,
					flags = flags,
				},
			)
		}
	case .Interfaces:
		for member in stmt.members {
			name := canonical_name(member.name, c.allocator)
			append(
				&c.implemented_interfaces,
				Implemented_Interface_Data {
					owner_symbol = class_symbol,
					interface_name = name,
					range = stmt.range,
				},
			)
			add_reference(c, scope, name, .Type, .Interface_Use, member.range)
		}
	case .Aliases:
		for member in stmt.members {
			target_ref := Field_Type_Ref_Data{}
			for sig in member.signatures {
				if sig.kind == .For && len(sig.values) > 0 {
					target_ref, _ = type_ref_from_expr(c, sig.values[0], .Type)
					break
				}
			}
			if target_ref.base_name == "" {
				continue
			}
			target_member := ""
			if len(target_ref.field_path) > 0 {
				target_member = target_ref.field_path[0]
			}
			append(
				&c.member_aliases,
				Member_Alias_Data {
					owner_symbol = class_symbol,
					alias_name = canonical_name(member.name, c.allocator),
					target_interface_name = target_ref.base_name,
					target_member_name = target_member,
					range = stmt.range,
				},
			)
		}
	case:
	}
}

declare_signature_scope_params :: proc(
	c: ^Collector,
	parent_scope: Scope_Id,
	range: tokenizer.Range,
	parameters: []Class_Member_Parameter_Data,
) {
	if len(parameters) == 0 {
		return
	}
	previous := c.current_scope
	c.current_scope = parent_scope
	sig_scope := push_scope(c, .Signature, range)
	for param in parameters {
		has_type := .Has_Declared_Type in param.flags
		_ = declare_collected_symbol(
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
		)
	}
	c.current_scope = sig_scope
	pop_scope(c)
	c.current_scope = previous
}

method_parameters_from_signatures :: proc(
	c: ^Collector,
	signatures: []ast.Oop_Signature_Clause,
) -> [dynamic]Class_Member_Parameter_Data {
	parameters := make([dynamic]Class_Member_Parameter_Data, 0, 2, c.allocator)
	for sig in signatures {
		section, ok := method_section_from_oop(sig.kind)
		if !ok {
			continue
		}
		for param in sig.parameters {
			append(&parameters, class_member_parameter_from_oop(c, param, section))
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
	signatures: []ast.Oop_Signature_Clause,
) -> [dynamic]Class_Member_Parameter_Data {
	parameters := make([dynamic]Class_Member_Parameter_Data, 0, 2, c.allocator)
	for sig in signatures {
		if sig.kind != .Exporting {
			continue
		}
		for param in sig.parameters {
			append(&parameters, class_member_parameter_from_oop(c, param, .Exporting))
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
	clause: ast.Oop_Parameter_Clause,
	section: Method_Parameter_Section,
) -> Class_Member_Parameter_Data {
	param := Class_Member_Parameter_Data {
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
) -> [dynamic]Form_Parameter_Data {
	parameters := make([dynamic]Form_Parameter_Data, 0, 2, c.allocator)
	for clause in clauses {
		declared_type, has_type := type_ref_from_clause(c, clause.type_clause)
		display := type_clause_display(c, clause.type_clause)
		type_form, has_type_form := type_clause_form_from_ast(clause.type_clause)
		if clause.section == .Tables &&
		   display != "" &&
		   !ascii_contains_ignore_case(display, "TABLE OF") {
			display = concat2(c, "STANDARD TABLE OF ", display)
		}
		structure_id := INVALID_STRUCTURE_ID
		if has_type {
			if resolved, resolved_ok := resolve_field_type_ref(c, scope, declared_type);
			   resolved_ok {
				structure_id = resolved
			}
			add_type_reference(c, scope, declared_type, clause.range)
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
		)
		append(
			&parameters,
			Form_Parameter_Data {
				symbol = symbol_id,
				section = form_parameter_section_from_ast(clause.section),
				passing = form_parameter_passing_from_ast(clause.passing),
			},
		)
	}
	return parameters
}

function_parameters_from_ast :: proc(
	c: ^Collector,
	stmt: ^ast.Function_Decl,
	scope: Scope_Id,
) -> (
	[dynamic]Function_Module_Parameter_Data,
	[dynamic]Function_Module_Exception_Data,
) {
	parameters := make([dynamic]Function_Module_Parameter_Data, 0, 2, c.allocator)
	exceptions := make([dynamic]Function_Module_Exception_Data, 0, 1, c.allocator)
	for clause in stmt.function_parameters {
		param := Function_Module_Parameter_Data {
			section = function_parameter_section_from_ast(clause.section),
			name    = canonical_name(clause.name, c.allocator),
			range   = clause.range,
			passing = parameter_passing_from_ast(clause.passing),
		}
		param.type_clause_display = type_clause_display(c, clause.type_clause)
		if type_ref, has_type := type_ref_from_clause(c, clause.type_clause); has_type {
			param.declared_type = type_ref
			param.flags += {.Has_Declared_Type}
			add_type_reference(c, scope, type_ref, param.range)
		}
		type_form, has_type_form := type_clause_form_from_ast(clause.type_clause)
		if .Is_Optional in clause.flags {
			param.flags += {.Is_Optional}
		}
		if .Has_Default_Value in clause.flags {
			param.flags += {.Has_Default_Value}
		}
		_ = declare_collected_symbol(
			c,
			scope,
			param.name,
			.Parameter,
			param.range,
			INVALID_STRUCTURE_ID,
			param.declared_type,
			.Has_Declared_Type in param.flags,
			param.type_clause_display,
			type_clause_form = type_form,
			has_type_clause_form = has_type_form,
		)
		append(&parameters, param)
	}
	for exception in stmt.exceptions {
		name := canonical_name(exception.name, c.allocator)
		append(
			&exceptions,
			Function_Module_Exception_Data {
				name = name,
				range = exception.range,
			},
		)
		_ = declare_collected_symbol(c, scope, name, .Exception, exception.range)
	}
	return parameters, exceptions
}

form_parameter_section_from_ast :: proc(section: ast.Form_Parameter_Section) -> Form_Parameter_Section {
	switch section {
	case .Tables:
		return .Tables
	case .Using:
		return .Using
	case .Changing:
		return .Changing
	}
	return .Using
}

form_parameter_passing_from_ast :: proc(passing: ast.Parameter_Passing_Kind) -> Form_Parameter_Passing_Kind {
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
	if range.start < 0 || range.end > len(c.source) || range.start >= range.end {
		return ""
	}
	return strings.clone(c.source[range.start:range.end], c.allocator)
}

enclosing_owner :: proc(c: ^Collector, scope: Scope_Id, kind: Scope_Kind) -> (Symbol_Id, bool) {
	current := scope
	for current != INVALID_SCOPE_ID {
		s := c.scopes[scope_id_index(current)]
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

class_member :: proc(c: ^Collector, class_symbol: Symbol_Id, name: string) -> ^Class_Member_Data {
	for &member in c.class_members {
		if member.class_symbol == class_symbol && strings.equal_fold(member.name, name) {
			return &member
		}
	}
	return nil
}

note_method_implementation :: proc(
	c: ^Collector,
	class_symbol: Symbol_Id,
	name: string,
	range: tokenizer.Range,
) {
	member := class_member(c, class_symbol, name)
	if member == nil {
		return
	}
	member.implementation_range = range
	member.implementation = Class_Member_Implementation_Data {
		unit  = c.unit_id,
		range = range,
	}
	member.flags += {.Has_Implementation_Range, .Has_Implementation}
}

declare_method_scope_params :: proc(
	c: ^Collector,
	class_symbol: Symbol_Id,
	name: string,
	method_scope: Scope_Id,
) {
	member := class_member(c, class_symbol, name)
	if member == nil {
		return
	}
	for param in member.parameters {
		has_type := .Has_Declared_Type in param.flags
		_ = declare_collected_symbol(
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
		)
	}
	for exception in member.exceptions {
		_ = declare_collected_symbol(c, method_scope, exception.name, .Exception, exception.range)
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
	for s in c.symbols {
		if s.scope == c.root_scope &&
		   !symbol_kind_is_builtin(s.kind) &&
		   (s.kind == .Class || s.kind == .Interface || s.kind == .Report || s.kind == .Type_Def) {
			add_provided_name(c, s.name)
		}
	}
	stem := uri_file_stem(c.uri)
	if stem != "" {
		add_provided_name(c, stem)
	}
}

add_provided_name :: proc(c: ^Collector, name: string) {
	canonical := canonical_name(name, c.allocator)
	for existing in c.provided_names {
		if existing == canonical {
			return
		}
	}
	append(&c.provided_names, canonical)
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

ascii_contains_ignore_case :: proc(haystack, needle: string) -> bool {
	if needle == "" {
		return true
	}
	if len(needle) > len(haystack) {
		return false
	}
	for i in 0 ..= len(haystack) - len(needle) {
		if strings.equal_fold(haystack[i:i + len(needle)], needle) {
			return true
		}
	}
	return false
}
