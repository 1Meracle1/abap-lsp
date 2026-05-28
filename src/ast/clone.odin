package abap_frontend_ast

import "src:tokenizer"

import "base:intrinsics"
import "core:mem"

new :: proc($T: typeid, range: tokenizer.Range, allocator: mem.Allocator) -> ^T {
	n, _ := mem.new(T, allocator)
	n.range = range
	set_derived(n)
	return n
}

set_derived :: #force_inline proc(node: ^$T) {
	node.derived = node
	when intrinsics.type_has_field(T, "derived_expr") {
		node.derived_expr = node
	}
	when intrinsics.type_has_field(T, "derived_stmt") {
		node.derived_stmt = node
	}
}

clone :: proc {
	clone_node,
	clone_expr,
	clone_stmt,
	clone_decl,
}

clone_expr :: proc(node: ^Expr, allocator: mem.Allocator) -> ^Expr {
	return cast(^Expr)clone_node(node, allocator)
}

clone_stmt :: proc(node: ^Stmt, allocator: mem.Allocator) -> ^Stmt {
	return cast(^Stmt)clone_node(node, allocator)
}

clone_decl :: proc(node: ^Decl, allocator: mem.Allocator) -> ^Decl {
	return cast(^Decl)clone_node(node, allocator)
}

clone_node :: proc(node: ^Node, allocator: mem.Allocator) -> ^Node {
	if node == nil {
		return nil
	}
	switch n in node.derived {
	case ^File:
		r := clone_shallow(n, allocator)
		r.allocator = allocator
		r.stmts = clone_stmt_list(n.stmts, allocator)
		return r
	case ^Bad_Expr:
		return clone_shallow(n, allocator)
	case ^Char_String_Template_Expr:
		r := clone_shallow(n, allocator)
		r.parts = clone_expr_list(n.parts, allocator)
		return r
	case ^Template_Literal_Expr:
		return clone_shallow(n, allocator)
	case ^Template_Interpolation_Expr:
		r := clone_shallow(n, allocator)
		r.expr = clone(n.expr, allocator)
		r.format_specs = clone_expr_list(n.format_specs, allocator)
		return r
	case ^Template_Expr:
		r := clone_shallow(n, allocator)
		r.expr = clone(n.expr, allocator)
		return r
	case ^Template_Format_Spec_Expr:
		r := clone_shallow(n, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Binary_Expr:
		r := clone_shallow(n, allocator)
		r.left = clone(n.left, allocator)
		r.right = clone(n.right, allocator)
		return r
	case ^Unary_Expr:
		r := clone_shallow(n, allocator)
		r.expr = clone(n.expr, allocator)
		return r
	case ^Paren_Expr:
		r := clone_shallow(n, allocator)
		r.expr = clone(n.expr, allocator)
		return r
	case ^Ident_Expr:
		return clone_shallow(n, allocator)
	case ^Literal_Expr:
		return clone_shallow(n, allocator)
	case ^Type_Ref_Expr:
		r := clone_shallow(n, allocator)
		r.path = clone_type_ref_path(n.path, allocator)
		r.key = clone_type_ref_key_clause(n.key, allocator)
		r.keys = clone_type_ref_key_clauses(n.keys, allocator)
		r.raw_decls = clone_raw_operand_decls(n.raw_decls, allocator)
		r.raw_refs = clone_raw_operand_refs(n.raw_refs, allocator)
		return r
	case ^Dynamic_Call_Method_Target_Expr:
		r := clone_shallow(n, allocator)
		r.base = clone(n.base, allocator)
		r.method = clone(n.method, allocator)
		return r
	case ^Host_Expr:
		r := clone_shallow(n, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Table_Expr:
		r := clone_shallow(n, allocator)
		r.table = clone(n.table, allocator)
		r.selectors = clone_expr_list(n.selectors, allocator)
		return r
	case ^Selector_Expr:
		r := clone_shallow(n, allocator)
		r.base = clone(n.base, allocator)
		r.field = clone(n.field, allocator)
		return r
	case ^Substring_Expr:
		r := clone_shallow(n, allocator)
		r.base = clone(n.base, allocator)
		r.offset = clone(n.offset, allocator)
		r.length = clone(n.length, allocator)
		return r
	case ^Call_Expr:
		r := clone_shallow(n, allocator)
		r.callee = clone(n.callee, allocator)
		r.args = clone(n.args, allocator)
		return r
	case ^Call_Arg_List_Expr:
		r := clone_shallow(n, allocator)
		r.args = clone_expr_list(n.args, allocator)
		return r
	case ^Call_Arg_Section_Expr:
		r := clone_shallow(n, allocator)
		r.args = clone_expr_list(n.args, allocator)
		return r
	case ^Call_Named_Arg_Expr:
		r := clone_shallow(n, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Call_Positional_Arg_Expr:
		r := clone_shallow(n, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Constructor_Expr:
		r := clone_shallow(n, allocator)
		r.type_ref = clone(n.type_ref, allocator)
		r.args = clone_expr_list(n.args, allocator)
		return r
	case ^Is_Predicate_Expr:
		r := clone_shallow(n, allocator)
		r.subject = clone(n.subject, allocator)
		return r
	case ^Instance_Of_Predicate_Expr:
		r := clone_shallow(n, allocator)
		r.subject = clone(n.subject, allocator)
		r.type_ref = clone(n.type_ref, allocator)
		return r
	case ^Between_Expr:
		r := clone_shallow(n, allocator)
		r.subject = clone(n.subject, allocator)
		r.low = clone(n.low, allocator)
		r.high = clone(n.high, allocator)
		return r
	case ^Sql_Case_When_Expr:
		r := clone_shallow(n, allocator)
		r.condition = clone(n.condition, allocator)
		r.result = clone(n.result, allocator)
		return r
	case ^Sql_Case_Expr:
		r := clone_shallow(n, allocator)
		r.operand = clone(n.operand, allocator)
		r.whens = clone_expr_list(n.whens, allocator)
		r.else_expr = clone(n.else_expr, allocator)
		return r
	case ^Let_Expr:
		r := clone_shallow(n, allocator)
		r.bindings = clone_expr_list(n.bindings, allocator)
		r.body = clone_expr_list(n.body, allocator)
		return r
	case ^Constructor_Let_Binding_Expr:
		r := clone_shallow(n, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Constructor_When_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.condition = clone(n.condition, allocator)
		r.result = clone(n.result, allocator)
		return r
	case ^Constructor_Else_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.result = clone(n.result, allocator)
		return r
	case ^Constructor_For_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.init = clone(n.init, allocator)
		r.then_expr = clone(n.then_expr, allocator)
		r.condition = clone(n.condition, allocator)
		r.source = clone(n.source, allocator)
		r.where_clause = clone(n.where_clause, allocator)
		r.body = clone_expr_list(n.body, allocator)
		return r
	case ^Constructor_Where_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.condition = clone(n.condition, allocator)
		return r
	case ^Constructor_Init_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.assignments = clone_expr_list(n.assignments, allocator)
		return r
	case ^Constructor_Next_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.assignments = clone_expr_list(n.assignments, allocator)
		return r
	case ^Constructor_Named_Assignment_Expr:
		r := clone_shallow(n, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Constructor_Base_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Constructor_Lines_Of_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.source = clone(n.source, allocator)
		r.from = clone(n.from, allocator)
		r.to = clone(n.to, allocator)
		return r
	case ^Constructor_Optional_Expr:
		r := clone_shallow(n, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Constructor_Corresponding_Mapping_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.assignments = clone_expr_list(n.assignments, allocator)
		return r
	case ^Constructor_Corresponding_Mapping_Assignment_Expr:
		r := clone_shallow(n, allocator)
		r.source = clone(n.source, allocator)
		r.default_value = clone(n.default_value, allocator)
		r.mapping = clone(n.mapping, allocator)
		r.except = clone(n.except, allocator)
		return r
	case ^Constructor_Corresponding_Except_Clause_Expr:
		r := clone_shallow(n, allocator)
		r.names = clone_expr_list(n.names, allocator)
		return r
	case ^Data_Inline_Name_Expr:
		return clone_shallow(n, allocator)
	case ^Field_Symbol_Inline_Name_Expr:
		return clone_shallow(n, allocator)
	case ^Data_Decl:
		r := clone_shallow(n, allocator)
		r.paren_length = clone_paren_length_clause(n.paren_length, allocator)
		r.length_clauses = clone_length_clauses(n.length_clauses, allocator)
		r.type_clause = clone_type_clause(n.type_clause, allocator)
		r.value_clause = clone_value_clause(n.value_clause, allocator)
		r.occurs = clone(n.occurs, allocator)
		r.include_ref = clone(n.include_ref, allocator)
		return r
	case ^Data_Chained_Decl:
		r := clone_shallow(n, allocator)
		r.decls = clone_data_chained_branches(n.decls, allocator)
		return r
	case ^Data_Inline_Decl:
		r := clone_shallow(n, allocator)
		r.expr = clone(n.expr, allocator)
		return r
	case ^Types_Decl:
		r := clone_shallow(n, allocator)
		r.types = clone_types_clauses(n.types, allocator)
		return r
	case ^Constants_Decl:
		r := clone_shallow(n, allocator)
		r.constants = clone_constants_clauses(n.constants, allocator)
		return r
	case ^Field_Symbols_Decl:
		r := clone_shallow(n, allocator)
		r.field_symbols = clone_field_symbols_clauses(n.field_symbols, allocator)
		return r
	case ^Statics_Decl:
		r := clone_shallow(n, allocator)
		r.statics = clone_statics_clauses(n.statics, allocator)
		return r
	case ^Tables_Decl:
		r := clone_shallow(n, allocator)
		r.tables = clone_tables_clauses(n.tables, allocator)
		return r
	case ^Ranges_Decl:
		r := clone_shallow(n, allocator)
		r.ranges = clone_ranges_clauses(n.ranges, allocator)
		return r
	case ^Parameters_Decl:
		r := clone_shallow(n, allocator)
		r.parameters = clone_parameters_clauses(n.parameters, allocator)
		return r
	case ^Select_Options_Decl:
		r := clone_shallow(n, allocator)
		r.options = clone_select_options_clauses(n.options, allocator)
		return r
	case ^Controls_Decl:
		r := clone_shallow(n, allocator)
		r.controls = clone_controls_clauses(n.controls, allocator)
		return r
	case ^Class_Data_Decl:
		r := clone_shallow(n, allocator)
		r.decls = clone_class_data_clauses(n.decls, allocator)
		return r
	case ^Type_Pools_Decl:
		r := clone_shallow(n, allocator)
		r.pools = clone_string_list(n.pools, allocator)
		return r
	case ^Function_Pool_Decl:
		return clone_shallow(n, allocator)
	case ^Include_Stmt:
		r := clone_shallow(n, allocator)
		r.names = clone_include_names(n.names, allocator)
		return r
	case ^Assign_Stmt:
		r := clone_shallow(n, allocator)
		r.lhs = clone(n.lhs, allocator)
		r.rhs = clone(n.rhs, allocator)
		return r
	case ^Downcast_Assign_Stmt:
		r := clone_shallow(n, allocator)
		r.lhs = clone(n.lhs, allocator)
		r.rhs = clone(n.rhs, allocator)
		return r
	case ^Expr_Stmt:
		r := clone_shallow(n, allocator)
		r.expr = clone(n.expr, allocator)
		return r
	case ^Clear_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_clear_operands(n.operands, allocator)
		return r
	case ^Refresh_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_refresh_operands(n.operands, allocator)
		return r
	case ^Free_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_free_operands(n.operands, allocator)
		r.memory_id = clone(n.memory_id, allocator)
		return r
	case ^Unassign_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_unassign_operands(n.operands, allocator)
		return r
	case ^Move_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_move_entries(n.entries, allocator)
		return r
	case ^Move_Corresponding_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_move_entries(n.entries, allocator)
		return r
	case ^Add_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_add_entries(n.entries, allocator)
		return r
	case ^Subtract_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_subtract_entries(n.entries, allocator)
		return r
	case ^Multiply_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_multiply_entries(n.entries, allocator)
		return r
	case ^Divide_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_divide_entries(n.entries, allocator)
		return r
	case ^Compute_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_compute_entries(n.entries, allocator)
		return r
	case ^Concatenate_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_concatenate_entries(n.entries, allocator)
		return r
	case ^Split_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_split_entries(n.entries, allocator)
		return r
	case ^Condense_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		return r
	case ^Replace_Stmt:
		r := clone_shallow(n, allocator)
		r.pattern = clone(n.pattern, allocator)
		r.target = clone(n.target, allocator)
		r.replacement = clone(n.replacement, allocator)
		r.section_offset = clone(n.section_offset, allocator)
		r.section_length = clone(n.section_length, allocator)
		return r
	case ^Translate_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.operand = clone(n.operand, allocator)
		return r
	case ^Shift_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.places = clone(n.places, allocator)
		r.delete_pattern = clone(n.delete_pattern, allocator)
		return r
	case ^Find_Stmt:
		r := clone_shallow(n, allocator)
		r.pattern = clone(n.pattern, allocator)
		r.target = clone(n.target, allocator)
		r.section_offset = clone(n.section_offset, allocator)
		r.section_length = clone(n.section_length, allocator)
		r.match_offset = clone(n.match_offset, allocator)
		r.match_length = clone(n.match_length, allocator)
		r.match_count = clone(n.match_count, allocator)
		r.results = clone(n.results, allocator)
		r.submatches = clone_expr_list(n.submatches, allocator)
		return r
	case ^Search_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.pattern = clone(n.pattern, allocator)
		r.starting_at = clone(n.starting_at, allocator)
		r.ending_at = clone(n.ending_at, allocator)
		return r
	case ^Perform_Stmt:
		r := clone_shallow(n, allocator)
		r.form = clone(n.form, allocator)
		r.program = clone(n.program, allocator)
		r.tables = clone_expr_list(n.tables, allocator)
		r.using_args = clone_expr_list(n.using_args, allocator)
		r.changing = clone_expr_list(n.changing, allocator)
		return r
	case ^Call_Stmt:
		r := clone_shallow(n, allocator)
		r.call = clone(n.call, allocator)
		r.target = clone(n.target, allocator)
		r.arg_sections = clone_call_stmt_arg_sections(n.arg_sections, allocator)
		r.named_args = clone_call_stmt_named_args(n.named_args, allocator)
		r.transaction_operands = clone_expr_list(n.transaction_operands, allocator)
		r.transformation_args = clone_call_transformation_args(n.transformation_args, allocator)
		return r
	case ^Submit_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.options = clone_submit_options(n.options, allocator)
		return r
	case ^Message_Stmt:
		r := clone_shallow(n, allocator)
		r.head = clone_message_head(n.head, allocator)
		r.with_args = clone_expr_list(n.with_args, allocator)
		r.into = clone(n.into, allocator)
		r.display_like = clone(n.display_like, allocator)
		r.raising = clone(n.raising, allocator)
		return r
	case ^Write_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_write_operands(n.operands, allocator)
		return r
	case ^Write_To_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_write_to_entries(n.entries, allocator)
		return r
	case ^Assert_Stmt:
		r := clone_shallow(n, allocator)
		r.condition = clone(n.condition, allocator)
		return r
	case ^Check_Stmt:
		r := clone_shallow(n, allocator)
		r.condition = clone(n.condition, allocator)
		return r
	case ^Flow_Stmt:
		return clone_shallow(n, allocator)
	case ^Transaction_Stmt:
		return clone_shallow(n, allocator)
	case ^Describe_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_describe_entries(n.entries, allocator)
		return r
	case ^Runtime_Stmt:
		r := clone_shallow(n, allocator)
		r.id = clone(n.id, allocator)
		r.field = clone(n.field, allocator)
		r.target = clone(n.target, allocator)
		r.value = clone(n.value, allocator)
		r.line = clone(n.line, allocator)
		r.offset = clone(n.offset, allocator)
		r.excluding = clone_expr_list(n.excluding, allocator)
		r.operands = clone_expr_list(n.operands, allocator)
		return r
	case ^Set_Handler_Stmt:
		r := clone_shallow(n, allocator)
		r.handlers = clone_expr_list(n.handlers, allocator)
		r.sender = clone(n.sender, allocator)
		r.activation = clone(n.activation, allocator)
		return r
	case ^Import_Stmt:
		r := clone_shallow(n, allocator)
		r.medium = clone_data_cluster_medium(n.medium, allocator)
		r.parameters = clone_data_cluster_parameters(n.parameters, allocator)
		return r
	case ^Export_Stmt:
		r := clone_shallow(n, allocator)
		r.medium = clone_data_cluster_medium(n.medium, allocator)
		r.parameters = clone_data_cluster_parameters(n.parameters, allocator)
		return r
	case ^Bit_Stmt:
		r := clone_shallow(n, allocator)
		r.position = clone(n.position, allocator)
		r.source = clone(n.source, allocator)
		r.target = clone(n.target, allocator)
		r.value = clone(n.value, allocator)
		return r
	case ^Locale_Stmt:
		r := clone_shallow(n, allocator)
		r.language = clone(n.language, allocator)
		r.country = clone(n.country, allocator)
		r.modifier = clone(n.modifier, allocator)
		return r
	case ^Set_Cursor_Stmt:
		r := clone_shallow(n, allocator)
		r.field = clone(n.field, allocator)
		r.offset = clone(n.offset, allocator)
		r.line = clone(n.line, allocator)
		r.column = clone(n.column, allocator)
		return r
	case ^Receive_Results_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.arg_sections = clone_call_stmt_arg_sections(n.arg_sections, allocator)
		r.named_args = clone_call_stmt_named_args(n.named_args, allocator)
		return r
	case ^Raise_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.operands = clone_expr_list(n.operands, allocator)
		return r
	case ^Authority_Check_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_expr_list(n.operands, allocator)
		r.object = clone(n.object, allocator)
		r.ids = clone_authority_check_ids(n.ids, allocator)
		return r
	case ^Field_Groups_Stmt:
		r := clone_shallow(n, allocator)
		r.groups = clone_expr_list(n.groups, allocator)
		return r
	case ^Insert_Dummy_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		return r
	case ^Field_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_expr_list(n.operands, allocator)
		return r
	case ^Assign_Field_Stmt:
		r := clone_shallow(n, allocator)
		r.source = clone(n.source, allocator)
		r.component = clone(n.component, allocator)
		r.structure = clone(n.structure, allocator)
		r.target = clone(n.target, allocator)
		r.casting_type = clone(n.casting_type, allocator)
		r.casting_decimals = clone(n.casting_decimals, allocator)
		return r
	case ^Create_Object_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.type_ref = clone(n.type_ref, allocator)
		r.type_clause = clone_type_clause(n.type_clause, allocator)
		r.type_dynamic_expr = clone(n.type_dynamic_expr, allocator)
		r.operands = clone_expr_list(n.operands, allocator)
		return r
	case ^Create_Data_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.type_ref = clone(n.type_ref, allocator)
		r.type_clause = clone_type_clause(n.type_clause, allocator)
		r.type_dynamic_expr = clone(n.type_dynamic_expr, allocator)
		r.type_handle = clone(n.type_handle, allocator)
		r.operands = clone_expr_list(n.operands, allocator)
		return r
	case ^Text_Transform_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_expr_list(n.operands, allocator)
		return r
	case ^Wait_Stmt:
		r := clone_shallow(n, allocator)
		r.condition = clone(n.condition, allocator)
		r.duration = clone(n.duration, allocator)
		return r
	case ^Convert_Time_Stamp_Stmt:
		r := clone_shallow(n, allocator)
		r.time_stamp = clone(n.time_stamp, allocator)
		r.time_zone = clone(n.time_zone, allocator)
		r.date = clone(n.date, allocator)
		r.time = clone(n.time, allocator)
		return r
	case ^List_Control_Stmt:
		r := clone_shallow(n, allocator)
		r.operands = clone_expr_list(n.operands, allocator)
		return r
	case ^Line_Stmt:
		r := clone_shallow(n, allocator)
		r.line = clone(n.line, allocator)
		r.index = clone(n.index, allocator)
		r.into = clone(n.into, allocator)
		r.fields = clone_line_fields(n.fields, allocator)
		return r
	case ^Macro_Def_Stmt:
		return clone_shallow(n, allocator)
	case ^Macro_Call_Stmt:
		r := clone_shallow(n, allocator)
		r.args = clone_expr_list(n.args, allocator)
		return r
	case ^Selection_Screen_Stmt:
		return clone_shallow(n, allocator)
	case ^Oop_Simple_Stmt:
		r := clone_shallow(n, allocator)
		r.members = clone_oop_members(n.members, allocator)
		return r
	case ^Oop_Load_Stmt:
		return clone_shallow(n, allocator)
	case ^If_Stmt:
		r := clone_shallow(n, allocator)
		r.condition = clone(n.condition, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		r.elseif_clauses = clone_elseif_clause_list(n.elseif_clauses, allocator)
		r.else_clause = clone_else_clause(n.else_clause, allocator)
		return r
	case ^Case_Stmt:
		r := clone_shallow(n, allocator)
		r.expr = clone(n.expr, allocator)
		r.whens = clone_when_clause_list(n.whens, allocator)
		r.recovery = clone_stmt_list(n.recovery, allocator)
		return r
	case ^While_Stmt:
		r := clone_shallow(n, allocator)
		r.condition = clone(n.condition, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Do_Stmt:
		r := clone_shallow(n, allocator)
		r.count = clone(n.count, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Loop_Stmt:
		r := clone_shallow(n, allocator)
		r.source = clone(n.source, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^At_Stmt:
		r := clone_shallow(n, allocator)
		r.expr = clone(n.expr, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Try_Stmt:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		r.catches = clone_catch_clause_list(n.catches, allocator)
		r.cleanup = clone_cleanup_clause(n.cleanup, allocator)
		return r
	case ^Class_Decl:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		r.friends = clone_class_friends(n.friends, allocator)
		return r
	case ^Interface_Decl:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Method_Decl:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Form_Decl:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		r.form_parameters = clone_form_parameters(n.form_parameters, allocator)
		return r
	case ^Function_Decl:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		r.function_parameters = clone_function_parameters(n.function_parameters, allocator)
		r.exceptions = clone_function_exceptions(n.exceptions, allocator)
		return r
	case ^Module_Decl:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Event_Block_Stmt:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Enhancement_Stmt:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Enhancement_Section_Stmt:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Test_Seam_Stmt:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Test_Injection_Stmt:
		r := clone_shallow(n, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Select_Stmt:
		r := clone_shallow(n, allocator)
		r.with = clone_select_with(n.with, allocator)
		r.query = clone_select_query(n.query, allocator)
		r.body = clone_stmt_list(n.body, allocator)
		return r
	case ^Open_Cursor_Stmt:
		r := clone_shallow(n, allocator)
		r.handle = clone(n.handle, allocator)
		r.query = clone_select_query(n.query, allocator)
		return r
	case ^Fetch_Stmt:
		r := clone_shallow(n, allocator)
		r.handle = clone(n.handle, allocator)
		r.result = clone_select_result(n.result, allocator)
		r.package_size = clone(n.package_size, allocator)
		return r
	case ^Close_Cursor_Stmt:
		r := clone_shallow(n, allocator)
		r.handle = clone(n.handle, allocator)
		return r
	case ^Insert_Stmt:
		r := clone_shallow(n, allocator)
		r.source = clone(n.source, allocator)
		r.target = clone(n.target, allocator)
		r.index = clone(n.index, allocator)
		r.assigning = clone(n.assigning, allocator)
		r.reference_into = clone(n.reference_into, allocator)
		r.assignments = clone_sql_assignments(n.assignments, allocator)
		return r
	case ^Append_Stmt:
		r := clone_shallow(n, allocator)
		r.source = clone(n.source, allocator)
		r.target = clone(n.target, allocator)
		r.assigning = clone(n.assigning, allocator)
		r.reference_into = clone(n.reference_into, allocator)
		return r
	case ^Modify_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.source = clone(n.source, allocator)
		r.index = clone(n.index, allocator)
		r.where_cond = clone(n.where_cond, allocator)
		r.transporting = clone_expr_list(n.transporting, allocator)
		return r
	case ^Sort_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.fields = clone_sort_fields(n.fields, allocator)
		return r
	case ^Update_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.source = clone(n.source, allocator)
		r.assignments = clone_sql_assignments(n.assignments, allocator)
		r.where_cond = clone(n.where_cond, allocator)
		return r
	case ^Delete_Stmt:
		r := clone_shallow(n, allocator)
		r.target = clone(n.target, allocator)
		r.source = clone(n.source, allocator)
		r.index = clone(n.index, allocator)
		r.where_cond = clone(n.where_cond, allocator)
		r.using_key = clone(n.using_key, allocator)
		r.comparing = clone_delete_comparing_clauses(n.comparing, allocator)
		return r
	case ^Read_Table_Stmt:
		r := clone_shallow(n, allocator)
		r.entries = clone_read_table_entries(n.entries, allocator)
		return r
	case ^Dataset_Stmt:
		r := clone_shallow(n, allocator)
		r.dataset = clone(n.dataset, allocator)
		r.source = clone(n.source, allocator)
		r.target = clone(n.target, allocator)
		r.position = clone(n.position, allocator)
		r.message = clone(n.message, allocator)
		r.maximum_length = clone(n.maximum_length, allocator)
		r.actual_length = clone(n.actual_length, allocator)
		r.length = clone(n.length, allocator)
		r.attributes = clone(n.attributes, allocator)
		return r
	case ^Report_Stmt:
		r := clone_shallow(n, allocator)
		r.name = clone(n.name, allocator)
		r.source = clone(n.source, allocator)
		r.line_size = clone(n.line_size, allocator)
		r.line_count = clone(n.line_count, allocator)
		return r
	case ^Textpool_Stmt:
		r := clone_shallow(n, allocator)
		r.program = clone(n.program, allocator)
		r.table = clone(n.table, allocator)
		r.language = clone(n.language, allocator)
		return r
	case ^Exec_Sql_Stmt:
		return clone_shallow(n, allocator)
	case ^Generate_Stmt:
		r := clone_shallow(n, allocator)
		r.source = clone(n.source, allocator)
		r.name = clone(n.name, allocator)
		r.program = clone(n.program, allocator)
		r.dynpro = clone(n.dynpro, allocator)
		r.message = clone(n.message, allocator)
		r.line = clone(n.line, allocator)
		r.word = clone(n.word, allocator)
		r.offset = clone(n.offset, allocator)
		return r
	case ^Invalid_Stmt:
		return clone_shallow(n, allocator)
	}

	return nil
}

clone_shallow :: proc(src: ^$T, allocator: mem.Allocator) -> ^T {
	dst, _ := mem.new(T, allocator)
	dst^ = src^
	set_derived(dst)
	when intrinsics.type_has_field(T, "leading_comments") {
		dst.leading_comments = clone_string_list(src.leading_comments, allocator)
	}
	return dst
}

clone_expr_list :: proc(list: [dynamic]^Expr, allocator: mem.Allocator) -> [dynamic]^Expr {
	res := make([dynamic]^Expr, 0, len(list), allocator)
	for x in list {
		append(&res, clone(x, allocator))
	}
	return res
}

clone_stmt_list :: proc(list: [dynamic]^Stmt, allocator: mem.Allocator) -> [dynamic]^Stmt {
	res := make([dynamic]^Stmt, 0, len(list), allocator)
	for x in list {
		append(&res, clone(x, allocator))
	}
	return res
}

clone_string_list :: proc(list: [dynamic]string, allocator: mem.Allocator) -> [dynamic]string {
	res := make([dynamic]string, 0, len(list), allocator)
	for x in list {
		append(&res, x)
	}
	return res
}

clone_type_ref_path :: proc(list: [dynamic]Type_Ref_Path_Segment, allocator: mem.Allocator) -> [dynamic]Type_Ref_Path_Segment {
	res := make([dynamic]Type_Ref_Path_Segment, 0, len(list), allocator)
	for x in list {
		append(&res, x)
	}
	return res
}

clone_raw_operand_decls :: proc(list: [dynamic]Raw_Operand_Inline_Decl, allocator: mem.Allocator) -> [dynamic]Raw_Operand_Inline_Decl {
	res := make([dynamic]Raw_Operand_Inline_Decl, 0, len(list), allocator)
	for x in list {
		append(&res, x)
	}
	return res
}

clone_raw_operand_path :: proc(list: [dynamic]Raw_Operand_Path_Segment, allocator: mem.Allocator) -> [dynamic]Raw_Operand_Path_Segment {
	res := make([dynamic]Raw_Operand_Path_Segment, 0, len(list), allocator)
	for x in list {
		append(&res, x)
	}
	return res
}

clone_raw_operand_refs :: proc(list: [dynamic]Raw_Operand_Ref, allocator: mem.Allocator) -> [dynamic]Raw_Operand_Ref {
	res := make([dynamic]Raw_Operand_Ref, 0, len(list), allocator)
	for x in list {
		ref := x
		ref.path = clone_raw_operand_path(x.path, allocator)
		append(&res, ref)
	}
	return res
}

clone_include_names :: proc(list: [dynamic]Include_Name, allocator: mem.Allocator) -> [dynamic]Include_Name {
	res := make([dynamic]Include_Name, 0, len(list), allocator)
	for x in list {
		append(&res, x)
	}
	return res
}

clone_call_stmt_arg_sections :: proc(list: [dynamic]Call_Stmt_Arg_Section, allocator: mem.Allocator) -> [dynamic]Call_Stmt_Arg_Section {
	res := make([dynamic]Call_Stmt_Arg_Section, 0, len(list), allocator)
	for x in list {
		append(&res, x)
	}
	return res
}

clone_call_stmt_named_args :: proc(list: [dynamic]Call_Stmt_Named_Arg, allocator: mem.Allocator) -> [dynamic]Call_Stmt_Named_Arg {
	res := make([dynamic]Call_Stmt_Named_Arg, 0, len(list), allocator)
	for x in list {
		arg := x
		arg.value = clone(x.value, allocator)
		arg.raw_decls = clone_raw_operand_decls(x.raw_decls, allocator)
		arg.raw_refs = clone_raw_operand_refs(x.raw_refs, allocator)
		append(&res, arg)
	}
	return res
}

clone_call_transformation_args :: proc(list: [dynamic]Call_Transformation_Arg, allocator: mem.Allocator) -> [dynamic]Call_Transformation_Arg {
	res := make([dynamic]Call_Transformation_Arg, 0, len(list), allocator)
	for x in list {
		arg := x
		arg.value = clone(x.value, allocator)
		append(&res, arg)
	}
	return res
}

clone_class_friends :: proc(list: [dynamic]Class_Friend_Clause, allocator: mem.Allocator) -> [dynamic]Class_Friend_Clause {
	res := make([dynamic]Class_Friend_Clause, 0, len(list), allocator)
	for friend in list {
		append(&res, friend)
	}
	return res
}

clone_type_ref_key_clause :: proc(clause: ^Type_Ref_Key_Clause, allocator: mem.Allocator) -> ^Type_Ref_Key_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Type_Ref_Key_Clause, allocator)
	res.kind = clause.kind
	res.default_key = clause.default_key
	res.sorted = clause.sorted
	res.hashed = clause.hashed
	res.name = clause.name
	res.components = clone_string_list(clause.components, allocator)
	return res
}

clone_type_ref_key_clauses :: proc(list: [dynamic]^Type_Ref_Key_Clause, allocator: mem.Allocator) -> [dynamic]^Type_Ref_Key_Clause {
	res := make([dynamic]^Type_Ref_Key_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, clone_type_ref_key_clause(clause, allocator))
	}
	return res
}

clone_line_fields :: proc(list: [dynamic]Line_Field_Value_Clause, allocator: mem.Allocator) -> [dynamic]Line_Field_Value_Clause {
	res := make([dynamic]Line_Field_Value_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Line_Field_Value_Clause{field = clone(clause.field, allocator), target = clone(clause.target, allocator)})
	}
	return res
}

clone_sort_fields :: proc(list: [dynamic]Sort_Field_Clause, allocator: mem.Allocator) -> [dynamic]Sort_Field_Clause {
	res := make([dynamic]Sort_Field_Clause, 0, len(list), allocator)
	for clause in list {
		next := clause
		next.expr = clone(clause.expr, allocator)
		append(&res, next)
	}
	return res
}

clone_oop_signatures :: proc(list: [dynamic]Oop_Signature_Clause, allocator: mem.Allocator) -> [dynamic]Oop_Signature_Clause {
	res := make([dynamic]Oop_Signature_Clause, 0, len(list), allocator)
	for clause in list {
		append(
			&res,
			Oop_Signature_Clause {
				kind = clause.kind,
				values = clone_expr_list(clause.values, allocator),
				parameters = clone_oop_parameters(clause.parameters, allocator),
			},
		)
	}
	return res
}

clone_oop_parameters :: proc(list: [dynamic]Oop_Parameter_Clause, allocator: mem.Allocator) -> [dynamic]Oop_Parameter_Clause {
	res := make([dynamic]Oop_Parameter_Clause, 0, len(list), allocator)
	for clause in list {
		append(
			&res,
			Oop_Parameter_Clause {
				name = clause.name,
				range = clause.range,
				passing = clause.passing,
				type_clause = clone_type_clause(clause.type_clause, allocator),
				optional = clause.optional,
			},
		)
	}
	return res
}

clone_form_parameters :: proc(list: [dynamic]Form_Parameter_Clause, allocator: mem.Allocator) -> [dynamic]Form_Parameter_Clause {
	res := make([dynamic]Form_Parameter_Clause, 0, len(list), allocator)
	for clause in list {
		append(
			&res,
			Form_Parameter_Clause {
				section = clause.section,
				name = clause.name,
				range = clause.range,
				passing = clause.passing,
				type_clause = clone_type_clause(clause.type_clause, allocator),
			},
		)
	}
	return res
}

clone_function_parameters :: proc(list: [dynamic]Function_Parameter_Clause, allocator: mem.Allocator) -> [dynamic]Function_Parameter_Clause {
	res := make([dynamic]Function_Parameter_Clause, 0, len(list), allocator)
	for clause in list {
		append(
			&res,
			Function_Parameter_Clause {
				section = clause.section,
				name = clause.name,
				range = clause.range,
				passing = clause.passing,
				type_clause = clone_type_clause(clause.type_clause, allocator),
				flags = clause.flags,
			},
		)
	}
	return res
}

clone_function_exceptions :: proc(list: [dynamic]Function_Exception_Clause, allocator: mem.Allocator) -> [dynamic]Function_Exception_Clause {
	res := make([dynamic]Function_Exception_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, clause)
	}
	return res
}

clone_oop_members :: proc(list: [dynamic]Oop_Member_Clause, allocator: mem.Allocator) -> [dynamic]Oop_Member_Clause {
	res := make([dynamic]Oop_Member_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Oop_Member_Clause{name = clause.name, signatures = clone_oop_signatures(clause.signatures, allocator)})
	}
	return res
}

clone_authority_check_ids :: proc(list: [dynamic]Authority_Check_ID_Clause, allocator: mem.Allocator) -> [dynamic]Authority_Check_ID_Clause {
	res := make([dynamic]Authority_Check_ID_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Authority_Check_ID_Clause{id = clone(clause.id, allocator), field = clone(clause.field, allocator)})
	}
	return res
}

clone_clear_operands :: proc(list: [dynamic]Clear_Operand_Clause, allocator: mem.Allocator) -> [dynamic]Clear_Operand_Clause {
	res := make([dynamic]Clear_Operand_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Clear_Operand_Clause{target = clone(clause.target, allocator), mode = clause.mode, value = clone(clause.value, allocator)})
	}
	return res
}

clone_refresh_operands :: proc(list: [dynamic]Refresh_Operand_Clause, allocator: mem.Allocator) -> [dynamic]Refresh_Operand_Clause {
	res := make([dynamic]Refresh_Operand_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Refresh_Operand_Clause{target = clone(clause.target, allocator), table = clause.table})
	}
	return res
}

clone_free_operands :: proc(list: [dynamic]Free_Operand_Clause, allocator: mem.Allocator) -> [dynamic]Free_Operand_Clause {
	res := make([dynamic]Free_Operand_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Free_Operand_Clause{target = clone(clause.target, allocator), object = clause.object})
	}
	return res
}

clone_unassign_operands :: proc(list: [dynamic]Unassign_Operand_Clause, allocator: mem.Allocator) -> [dynamic]Unassign_Operand_Clause {
	res := make([dynamic]Unassign_Operand_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Unassign_Operand_Clause{target = clone(clause.target, allocator)})
	}
	return res
}

clone_move_entries :: proc(list: [dynamic]Move_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Move_Entry_Clause {
	res := make([dynamic]Move_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Move_Entry_Clause{source = clone(clause.source, allocator), target = clone(clause.target, allocator)})
	}
	return res
}

clone_add_entries :: proc(list: [dynamic]Add_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Add_Entry_Clause {
	res := make([dynamic]Add_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Add_Entry_Clause{source = clone(clause.source, allocator), target = clone(clause.target, allocator), result = clone(clause.result, allocator)})
	}
	return res
}

clone_subtract_entries :: proc(list: [dynamic]Subtract_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Subtract_Entry_Clause {
	res := make([dynamic]Subtract_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Subtract_Entry_Clause{source = clone(clause.source, allocator), target = clone(clause.target, allocator), result = clone(clause.result, allocator)})
	}
	return res
}

clone_multiply_entries :: proc(list: [dynamic]Multiply_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Multiply_Entry_Clause {
	res := make([dynamic]Multiply_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Multiply_Entry_Clause{target = clone(clause.target, allocator), source = clone(clause.source, allocator), result = clone(clause.result, allocator)})
	}
	return res
}

clone_divide_entries :: proc(list: [dynamic]Divide_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Divide_Entry_Clause {
	res := make([dynamic]Divide_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Divide_Entry_Clause{form = clause.form, source = clone(clause.source, allocator), target = clone(clause.target, allocator), result = clone(clause.result, allocator)})
	}
	return res
}

clone_compute_entries :: proc(list: [dynamic]Compute_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Compute_Entry_Clause {
	res := make([dynamic]Compute_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Compute_Entry_Clause{exact = clause.exact, target = clone(clause.target, allocator), source = clone(clause.source, allocator)})
	}
	return res
}

clone_concatenate_entries :: proc(list: [dynamic]Concatenate_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Concatenate_Entry_Clause {
	res := make([dynamic]Concatenate_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Concatenate_Entry_Clause {
			sources           = clone_expr_list(clause.sources, allocator),
			lines_of          = clause.lines_of,
			target            = clone(clause.target, allocator),
			separator         = clone(clause.separator, allocator),
			respecting_blanks = clause.respecting_blanks,
		})
	}
	return res
}

clone_split_entries :: proc(list: [dynamic]Split_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Split_Entry_Clause {
	res := make([dynamic]Split_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Split_Entry_Clause {
			source     = clone(clause.source, allocator),
			separator  = clone(clause.separator, allocator),
			targets    = clone_expr_list(clause.targets, allocator),
			into_table = clause.into_table,
		})
	}
	return res
}

clone_submit_options :: proc(list: [dynamic]Submit_Option_Clause, allocator: mem.Allocator) -> [dynamic]Submit_Option_Clause {
	res := make([dynamic]Submit_Option_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Submit_Option_Clause {
			kind       = clause.kind,
			name       = clause.name,
			operator   = clause.operator,
			value      = clone(clause.value, allocator),
			high_value = clone(clause.high_value, allocator),
		})
	}
	return res
}

clone_select_result :: proc(clause: ^Select_Result_Clause, allocator: mem.Allocator) -> ^Select_Result_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Select_Result_Clause, allocator)
	res.range = clause.range
	res.kind = clause.kind
	res.target = clone(clause.target, allocator)
	res.table = clause.table
	res.corresponding_fields = clause.corresponding_fields
	return res
}

clone_select_query :: proc(clause: Select_Query_Clause, allocator: mem.Allocator) -> Select_Query_Clause {
	return Select_Query_Clause {
		single          = clause.single,
		is_distinct     = clause.is_distinct,
		projections     = clone_expr_list(clause.projections, allocator),
		projection_clauses = clone_select_projections(clause.projection_clauses, allocator),
		source          = clone(clause.source, allocator),
		source_clause   = clone_select_source(clause.source_clause, allocator),
		result          = clone_select_result(clause.result, allocator),
		where_cond      = clone(clause.where_cond, allocator),
		dynamic_where   = clause.dynamic_where,
		for_all_entries = clone(clause.for_all_entries, allocator),
		package_size    = clone(clause.package_size, allocator),
		up_to_rows      = clone(clause.up_to_rows, allocator),
		set_ops         = clone_select_set_ops(clause.set_ops, allocator),
		projection_clause      = clause.projection_clause,
		from_clause            = clause.from_clause,
		into_clause            = clause.into_clause,
		where_clause           = clause.where_clause,
		group_by_clause        = clause.group_by_clause,
		having_clause          = clause.having_clause,
		order_by_clause        = clause.order_by_clause,
		order_by_primary_key   = clause.order_by_primary_key,
		order_by_fields        = clone_string_list(clause.order_by_fields, allocator),
		for_all_entries_clause = clause.for_all_entries_clause,
		for_update_clause      = clause.for_update_clause,
		up_to_clause           = clause.up_to_clause,
		package_size_clause    = clause.package_size_clause,
		offset_clause          = clause.offset_clause,
		abap_options_clause    = clause.abap_options_clause,
		set_operator_clause    = clause.set_operator_clause,
	}
}

clone_select_with :: proc(clause: ^Select_With_Clause, allocator: mem.Allocator) -> ^Select_With_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Select_With_Clause, allocator)
	res^ = clause^
	res.entries = clone_select_ctes(clause.entries, allocator)
	return res
}

clone_select_ctes :: proc(list: [dynamic]Select_Cte_Clause, allocator: mem.Allocator) -> [dynamic]Select_Cte_Clause {
	res := make([dynamic]Select_Cte_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Select_Cte_Clause{name = clause.name, query = clone_select_query(clause.query, allocator)})
	}
	return res
}

clone_select_projections :: proc(list: [dynamic]Select_Projection_Clause, allocator: mem.Allocator) -> [dynamic]Select_Projection_Clause {
	res := make([dynamic]Select_Projection_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Select_Projection_Clause{value = clone(clause.value, allocator), alias = clause.alias, range = clause.range})
	}
	return res
}

clone_select_source :: proc(clause: ^Select_Source_Clause, allocator: mem.Allocator) -> ^Select_Source_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Select_Source_Clause, allocator)
	res.range = clause.range
	res.source = clone(clause.source, allocator)
	res.alias = clause.alias
	res.dynamic_source = clause.dynamic_source
	res.joins = clone_select_joins(clause.joins, allocator)
	return res
}

clone_select_joins :: proc(list: [dynamic]Select_Join_Clause, allocator: mem.Allocator) -> [dynamic]Select_Join_Clause {
	res := make([dynamic]Select_Join_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Select_Join_Clause{kind = clause.kind, source = clone(clause.source, allocator), alias = clause.alias, on = clone(clause.on, allocator)})
	}
	return res
}

clone_select_set_ops :: proc(list: [dynamic]Select_Set_Clause, allocator: mem.Allocator) -> [dynamic]Select_Set_Clause {
	res := make([dynamic]Select_Set_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Select_Set_Clause{kind = clause.kind, all = clause.all, query = clone_select_query(clause.query, allocator)})
	}
	return res
}

clone_read_table_entries :: proc(list: [dynamic]Read_Table_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Read_Table_Entry_Clause {
	res := make([dynamic]Read_Table_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Read_Table_Entry_Clause {
			table                  = clone(clause.table, allocator),
			into                   = clone(clause.into, allocator),
			assigning              = clone(clause.assigning, allocator),
			reference_into         = clone(clause.reference_into, allocator),
			key_kind               = clause.key_kind,
			key_name               = clause.key_name,
			key_values             = clone_read_table_key_values(clause.key_values, allocator),
			index                  = clone(clause.index, allocator),
			using_key              = clone(clause.using_key, allocator),
			transporting_no_fields = clause.transporting_no_fields,
			binary_search          = clause.binary_search,
			binary_search_clause   = clause.binary_search_clause,
			comparing              = clone_expr_list(clause.comparing, allocator),
		})
	}
	return res
}

clone_delete_comparing_clauses :: proc(list: [dynamic]Delete_Comparing_Clause, allocator: mem.Allocator) -> [dynamic]Delete_Comparing_Clause {
	res := make([dynamic]Delete_Comparing_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Delete_Comparing_Clause{all_fields = clause.all_fields, expr = clone(clause.expr, allocator), name = clause.name, range = clause.range})
	}
	return res
}

clone_read_table_key_values :: proc(list: [dynamic]Read_Table_Key_Value_Clause, allocator: mem.Allocator) -> [dynamic]Read_Table_Key_Value_Clause {
	res := make([dynamic]Read_Table_Key_Value_Clause, 0, len(list), allocator)
	for clause in list {
		path := make([dynamic]Read_Table_Key_Name_Segment, 0, len(clause.path), allocator)
		for segment in clause.path {
			append(&path, segment)
		}
		append(&res, Read_Table_Key_Value_Clause{name = clause.name, name_range = clause.name_range, path = path, dynamic_name = clone(clause.dynamic_name, allocator), is_dynamic = clause.is_dynamic, table_line = clause.table_line, value = clone(clause.value, allocator)})
	}
	return res
}

clone_sql_assignments :: proc(list: [dynamic]Sql_Assignment_Clause, allocator: mem.Allocator) -> [dynamic]Sql_Assignment_Clause {
	res := make([dynamic]Sql_Assignment_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Sql_Assignment_Clause{
			name = clone(clause.name, allocator),
			value = clone(clause.value, allocator),
			column_name = clause.column_name,
			column_range = clause.column_range,
		})
	}
	return res
}

clone_message_head :: proc(clause: ^Message_Head_Clause, allocator: mem.Allocator) -> ^Message_Head_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Message_Head_Clause, allocator)
	res.code = clone(clause.code, allocator)
	res.id = clone(clause.id, allocator)
	res.msg_type = clone(clause.msg_type, allocator)
	res.number = clone(clause.number, allocator)
	res.compact_class_name = clause.compact_class_name
	res.compact_class_range = clause.compact_class_range
	res.has_compact_class = clause.has_compact_class
	return res
}

clone_write_operands :: proc(list: [dynamic]Write_Operand_Clause, allocator: mem.Allocator) -> [dynamic]Write_Operand_Clause {
	res := make([dynamic]Write_Operand_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Write_Operand_Clause {
			value      = clone(clause.value, allocator),
			line_break = clause.line_break,
			position   = clone(clause.position, allocator),
			length     = clone(clause.length, allocator),
		})
	}
	return res
}

clone_write_to_entries :: proc(list: [dynamic]Write_To_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Write_To_Entry_Clause {
	res := make([dynamic]Write_To_Entry_Clause, 0, len(list), allocator)
	for entry in list {
		append(&res, Write_To_Entry_Clause {
			source = clone(entry.source, allocator),
			target = clone(entry.target, allocator),
		})
	}
	return res
}

clone_describe_entries :: proc(list: [dynamic]Describe_Entry_Clause, allocator: mem.Allocator) -> [dynamic]Describe_Entry_Clause {
	res := make([dynamic]Describe_Entry_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Describe_Entry_Clause {
			source = clone(clause.source, allocator),
			target = clone(clause.target, allocator),
			table  = clause.table,
		})
	}
	return res
}

clone_data_cluster_medium :: proc(
	medium: Data_Cluster_Medium_Clause,
	allocator: mem.Allocator,
) -> Data_Cluster_Medium_Clause {
	return Data_Cluster_Medium_Clause {
		kind        = medium.kind,
		object      = clone(medium.object, allocator),
		dbtab       = medium.dbtab,
		dbtab_range = medium.dbtab_range,
		area        = medium.area,
		area_range  = medium.area_range,
		work_area   = clone(medium.work_area, allocator),
		client      = clone(medium.client, allocator),
		id          = clone(medium.id, allocator),
	}
}

clone_data_cluster_parameters :: proc(
	list: [dynamic]Data_Cluster_Parameter_Clause,
	allocator: mem.Allocator,
) -> [dynamic]Data_Cluster_Parameter_Clause {
	res := make([dynamic]Data_Cluster_Parameter_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Data_Cluster_Parameter_Clause {
			name       = clause.name,
			name_range = clause.name_range,
			value      = clone(clause.value, allocator),
		})
	}
	return res
}

clone_data_chained_branches :: proc(
	list: [dynamic]Data_Chained_Branch,
	allocator: mem.Allocator,
) -> [dynamic]Data_Chained_Branch {
	res := make([dynamic]Data_Chained_Branch, 0, len(list), allocator)
	for branch in list {
		append(
			&res,
			Data_Chained_Branch {
				kind            = branch.kind,
				flags           = branch.flags,
				depth           = branch.depth,
				name            = branch.name,
				paren_length    = clone_paren_length_clause(branch.paren_length, allocator),
				length_clauses  = clone_length_clauses(branch.length_clauses, allocator),
				type_clause     = clone_type_clause(branch.type_clause, allocator),
				value_clause    = clone_value_clause(branch.value_clause, allocator),
				occurs          = clone(branch.occurs, allocator),
				include_ref     = clone(branch.include_ref, allocator),
				as_name         = branch.as_name,
				renaming_suffix = branch.renaming_suffix,
				read_only       = branch.read_only,
			},
		)
	}
	return res
}

clone_elseif_clause_list :: proc(
	list: [dynamic]^Elseif_Clause,
	allocator: mem.Allocator,
) -> [dynamic]^Elseif_Clause {
	res := make([dynamic]^Elseif_Clause, 0, len(list), allocator)
	for item in list {
		append(&res, clone_elseif_clause(item, allocator))
	}
	return res
}

clone_elseif_clause :: proc(clause: ^Elseif_Clause, allocator: mem.Allocator) -> ^Elseif_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Elseif_Clause, allocator)
	res.range = clause.range
	res.condition = clone(clause.condition, allocator)
	res.body = clone_stmt_list(clause.body, allocator)
	return res
}

clone_else_clause :: proc(clause: ^Else_Clause, allocator: mem.Allocator) -> ^Else_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Else_Clause, allocator)
	res.range = clause.range
	res.body = clone_stmt_list(clause.body, allocator)
	return res
}

clone_when_clause_list :: proc(
	list: [dynamic]^When_Clause,
	allocator: mem.Allocator,
) -> [dynamic]^When_Clause {
	res := make([dynamic]^When_Clause, 0, len(list), allocator)
	for item in list {
		append(&res, clone_when_clause(item, allocator))
	}
	return res
}

clone_when_clause :: proc(clause: ^When_Clause, allocator: mem.Allocator) -> ^When_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(When_Clause, allocator)
	res.range = clause.range
	res.operands = clone_expr_list(clause.operands, allocator)
	res.is_others = clause.is_others
	res.body = clone_stmt_list(clause.body, allocator)
	return res
}

clone_catch_clause_list :: proc(
	list: [dynamic]^Catch_Clause,
	allocator: mem.Allocator,
) -> [dynamic]^Catch_Clause {
	res := make([dynamic]^Catch_Clause, 0, len(list), allocator)
	for item in list {
		append(&res, clone_catch_clause(item, allocator))
	}
	return res
}

clone_catch_clause :: proc(clause: ^Catch_Clause, allocator: mem.Allocator) -> ^Catch_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Catch_Clause, allocator)
	res.range = clause.range
	res.exceptions = clone_expr_list(clause.exceptions, allocator)
	res.into = clone(clause.into, allocator)
	res.body = clone_stmt_list(clause.body, allocator)
	return res
}

clone_cleanup_clause :: proc(clause: ^Cleanup_Clause, allocator: mem.Allocator) -> ^Cleanup_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Cleanup_Clause, allocator)
	res.range = clause.range
	res.body = clone_stmt_list(clause.body, allocator)
	return res
}

clone_types_clauses :: proc(list: [dynamic]Types_Clause, allocator: mem.Allocator) -> [dynamic]Types_Clause {
	res := make([dynamic]Types_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Types_Clause {
			kind           = clause.kind,
			flags          = clause.flags,
			depth          = clause.depth,
			name           = clause.name,
			paren_length   = clone_paren_length_clause(clause.paren_length, allocator),
			length_clauses = clone_length_clauses(clause.length_clauses, allocator),
			type_clause    = clone_type_clause(clause.type_clause, allocator),
			occurs         = clone(clause.occurs, allocator),
			include_ref    = clone(clause.include_ref, allocator),
			as_name        = clause.as_name,
			renaming_suffix = clause.renaming_suffix,
		})
	}
	return res
}

clone_constants_clauses :: proc(list: [dynamic]Constants_Clause, allocator: mem.Allocator) -> [dynamic]Constants_Clause {
	res := make([dynamic]Constants_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Constants_Clause {
			kind           = clause.kind,
			flags          = clause.flags,
			depth          = clause.depth,
			name           = clause.name,
			paren_length   = clone_paren_length_clause(clause.paren_length, allocator),
			length_clauses = clone_length_clauses(clause.length_clauses, allocator),
			type_clause    = clone_type_clause(clause.type_clause, allocator),
			value_clause   = clone_value_clause(clause.value_clause, allocator),
			occurs         = clone(clause.occurs, allocator),
			include_ref    = clone(clause.include_ref, allocator),
			as_name        = clause.as_name,
			renaming_suffix = clause.renaming_suffix,
		})
	}
	return res
}

clone_field_symbols_clauses :: proc(list: [dynamic]Field_Symbols_Clause, allocator: mem.Allocator) -> [dynamic]Field_Symbols_Clause {
	res := make([dynamic]Field_Symbols_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Field_Symbols_Clause{clause.name, clone_type_clause(clause.type_clause, allocator)})
	}
	return res
}

clone_statics_clauses :: proc(list: [dynamic]Statics_Clause, allocator: mem.Allocator) -> [dynamic]Statics_Clause {
	res := make([dynamic]Statics_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Statics_Clause {
			kind           = clause.kind,
			flags          = clause.flags,
			depth          = clause.depth,
			name           = clause.name,
			paren_length   = clone_paren_length_clause(clause.paren_length, allocator),
			length_clauses = clone_length_clauses(clause.length_clauses, allocator),
			type_clause    = clone_type_clause(clause.type_clause, allocator),
			value_clause   = clone_value_clause(clause.value_clause, allocator),
			occurs         = clone(clause.occurs, allocator),
			include_ref    = clone(clause.include_ref, allocator),
			as_name        = clause.as_name,
			renaming_suffix = clause.renaming_suffix,
		})
	}
	return res
}

clone_tables_clauses :: proc(list: [dynamic]Tables_Clause, allocator: mem.Allocator) -> [dynamic]Tables_Clause {
	res := make([dynamic]Tables_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, clause)
	}
	return res
}

clone_ranges_clauses :: proc(list: [dynamic]Ranges_Clause, allocator: mem.Allocator) -> [dynamic]Ranges_Clause {
	res := make([dynamic]Ranges_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Ranges_Clause{clause.name, clone_for_clause(clause.for_clause, allocator)})
	}
	return res
}

clone_parameters_clauses :: proc(list: [dynamic]Parameters_Clause, allocator: mem.Allocator) -> [dynamic]Parameters_Clause {
	res := make([dynamic]Parameters_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Parameters_Clause {
			name              = clause.name,
			paren_length      = clone_paren_length_clause(clause.paren_length, allocator),
			length_clauses    = clone_length_clauses(clause.length_clauses, allocator),
			type_clause       = clone_type_clause(clause.type_clause, allocator),
			default_clause    = clone_default_clause(clause.default_clause, allocator),
			flags             = clause.flags,
			radiobutton_group = clone_radiobutton_group_clause(clause.radiobutton_group, allocator),
			user_command      = clone_user_command_clause(clause.user_command, allocator),
			modif_id          = clone_modif_id_clause(clause.modif_id, allocator),
			memory_id         = clone_memory_id_clause(clause.memory_id, allocator),
			matchcode_object  = clone_matchcode_object_clause(clause.matchcode_object, allocator),
			visible_length   = clone_visible_length_clause(clause.visible_length, allocator),
		})
	}
	return res
}

clone_select_options_clauses :: proc(list: [dynamic]Select_Options_Clause, allocator: mem.Allocator) -> [dynamic]Select_Options_Clause {
	res := make([dynamic]Select_Options_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Select_Options_Clause {
			name             = clause.name,
			for_clause       = clone_for_clause(clause.for_clause, allocator),
			default_clause   = clone_default_clause(clause.default_clause, allocator),
			to_clause        = clone_to_clause(clause.to_clause, allocator),
			option_clause    = clone_option_clause(clause.option_clause, allocator),
			sign_clause      = clone_sign_clause(clause.sign_clause, allocator),
			flags            = clause.flags,
			modif_id         = clone_modif_id_clause(clause.modif_id, allocator),
			memory_id        = clone_memory_id_clause(clause.memory_id, allocator),
			matchcode_object = clone_matchcode_object_clause(clause.matchcode_object, allocator),
			visible_length  = clone_visible_length_clause(clause.visible_length, allocator),
			help_request    = clone_selection_request_clause(clause.help_request, allocator),
			value_request   = clone_selection_request_clause(clause.value_request, allocator),
		})
	}
	return res
}

clone_controls_clauses :: proc(list: [dynamic]Controls_Clause, allocator: mem.Allocator) -> [dynamic]Controls_Clause {
	res := make([dynamic]Controls_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Controls_Clause{clause.name, clone_type_clause(clause.type_clause, allocator), clone_using_screen_clause(clause.using_screen, allocator)})
	}
	return res
}

clone_class_data_clauses :: proc(list: [dynamic]Class_Data_Clause, allocator: mem.Allocator) -> [dynamic]Class_Data_Clause {
	res := make([dynamic]Class_Data_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Class_Data_Clause {
			kind           = clause.kind,
			flags          = clause.flags,
			depth          = clause.depth,
			name           = clause.name,
			paren_length   = clone_paren_length_clause(clause.paren_length, allocator),
			length_clauses = clone_length_clauses(clause.length_clauses, allocator),
			type_clause    = clone_type_clause(clause.type_clause, allocator),
			value_clause   = clone_value_clause(clause.value_clause, allocator),
			occurs         = clone(clause.occurs, allocator),
			include_ref    = clone(clause.include_ref, allocator),
			as_name        = clause.as_name,
			renaming_suffix = clause.renaming_suffix,
			read_only      = clause.read_only,
		})
	}
	return res
}

clone_type_clause :: proc(clause: ^Data_Type_Clause, allocator: mem.Allocator) -> ^Data_Type_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Data_Type_Clause, allocator)
	res.form = clause.form
	res.table_has_of = clause.table_has_of
	res.type_ref = clone(clause.type_ref, allocator)
	return res
}

clone_paren_length_clause :: proc(clause: ^Paren_Length_Clause, allocator: mem.Allocator) -> ^Paren_Length_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Paren_Length_Clause, allocator)
	res.expr = clone(clause.expr, allocator)
	return res
}

clone_length_clauses :: proc(list: [dynamic]Length_Clause, allocator: mem.Allocator) -> [dynamic]Length_Clause {
	res := make([dynamic]Length_Clause, 0, len(list), allocator)
	for clause in list {
		append(&res, Length_Clause{kind = clause.kind, expr = clone(clause.expr, allocator)})
	}
	return res
}

clone_value_clause :: proc(clause: ^Value_Clause, allocator: mem.Allocator) -> ^Value_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Value_Clause, allocator)
	res.expr = clone(clause.expr, allocator)
	res.is_initial = clause.is_initial
	return res
}

clone_default_clause :: proc(clause: ^Default_Clause, allocator: mem.Allocator) -> ^Default_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Default_Clause, allocator)
	res.expr = clone(clause.expr, allocator)
	return res
}

clone_for_clause :: proc(clause: ^For_Clause, allocator: mem.Allocator) -> ^For_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(For_Clause, allocator)
	res.expr = clone(clause.expr, allocator)
	return res
}

clone_using_screen_clause :: proc(clause: ^Using_Screen_Clause, allocator: mem.Allocator) -> ^Using_Screen_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Using_Screen_Clause, allocator)
	res.screen = clone(clause.screen, allocator)
	return res
}

clone_to_clause :: proc(clause: ^To_Clause, allocator: mem.Allocator) -> ^To_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(To_Clause, allocator)
	res.expr = clone(clause.expr, allocator)
	return res
}

clone_option_clause :: proc(clause: ^Option_Clause, allocator: mem.Allocator) -> ^Option_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Option_Clause, allocator)
	res.option = clause.option
	return res
}

clone_sign_clause :: proc(clause: ^Sign_Clause, allocator: mem.Allocator) -> ^Sign_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Sign_Clause, allocator)
	res.sign = clause.sign
	return res
}

clone_radiobutton_group_clause :: proc(clause: ^Radiobutton_Group_Clause, allocator: mem.Allocator) -> ^Radiobutton_Group_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Radiobutton_Group_Clause, allocator)
	res.group = clause.group
	return res
}

clone_user_command_clause :: proc(clause: ^User_Command_Clause, allocator: mem.Allocator) -> ^User_Command_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(User_Command_Clause, allocator)
	res.command = clause.command
	return res
}

clone_modif_id_clause :: proc(clause: ^Modif_Id_Clause, allocator: mem.Allocator) -> ^Modif_Id_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Modif_Id_Clause, allocator)
	res.id = clause.id
	return res
}

clone_memory_id_clause :: proc(clause: ^Memory_Id_Clause, allocator: mem.Allocator) -> ^Memory_Id_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Memory_Id_Clause, allocator)
	res.id = clone(clause.id, allocator)
	return res
}

clone_matchcode_object_clause :: proc(clause: ^Matchcode_Object_Clause, allocator: mem.Allocator) -> ^Matchcode_Object_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Matchcode_Object_Clause, allocator)
	res.object = clone(clause.object, allocator)
	return res
}

clone_visible_length_clause :: proc(clause: ^Visible_Length_Clause, allocator: mem.Allocator) -> ^Visible_Length_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Visible_Length_Clause, allocator)
	res.length = clone(clause.length, allocator)
	return res
}

clone_selection_request_clause :: proc(clause: ^Selection_Request_Clause, allocator: mem.Allocator) -> ^Selection_Request_Clause {
	if clause == nil {
		return nil
	}
	res, _ := mem.new(Selection_Request_Clause, allocator)
	res.kind = clause.kind
	res.target = clause.target
	return res
}
