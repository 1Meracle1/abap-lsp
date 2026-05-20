package abap_frontend_ast

import "core:mem"
import "core:strings"

Print_Options :: struct {
	newline: string,
	indent:  string,
}

DEFAULT_PRINT_OPTIONS :: Print_Options {
	newline = "\n",
	indent  = "    ",
}

Printer :: struct {
	out:          ^strings.Builder,
	options:      Print_Options,
	indent_level: int,
}

print_node :: proc(node: ^Node, allocator: mem.Allocator, options := DEFAULT_PRINT_OPTIONS) -> string {
	out := strings.builder_make(allocator)
	write_node(&out, node, options)
	return strings.to_string(out)
}

write_node :: proc(out: ^strings.Builder, node: ^Node, options := DEFAULT_PRINT_OPTIONS) {
	p := Printer{out = out, options = options}
	emit_node(&p, node)
}

emit :: proc(p: ^Printer, text: string) {
	strings.write_string(p.out, text)
}

emit_space :: proc(p: ^Printer) {
	emit(p, " ")
}

emit_newline :: proc(p: ^Printer) {
	emit(p, p.options.newline)
	for _ in 0 ..< p.indent_level {
		emit(p, p.options.indent)
	}
}

emit_node :: proc(p: ^Printer, node: ^Node) {
	if node == nil {
		return
	}
	#partial switch n in node.derived {
	case ^File:
		emit_file(p, n)
	case ^Bad_Expr:
		emit(p, "?")
	case ^Char_String_Template_Expr:
		emit_template(p, n)
	case ^Template_Literal_Expr:
		emit(p, n.literal)
	case ^Template_Interpolation_Expr:
		emit_template_interpolation(p, n)
	case ^Template_Expr:
		emit_node(p, n.expr)
	case ^Template_Format_Spec_Expr:
		emit(p, n.name)
		emit(p, " = ")
		emit_node(p, n.value)
	case ^Binary_Expr:
		emit_node(p, n.left)
		emit_space(p)
		emit(p, binary_op_text(n.op))
		emit_space(p)
		emit_node(p, n.right)
	case ^Unary_Expr:
		op := unary_op_text(n.op)
		emit(p, op)
		if n.op == .Not {
			emit_space(p)
		}
		emit_node(p, n.expr)
	case ^Paren_Expr:
		emit(p, "( ")
		emit_node(p, n.expr)
		emit(p, " )")
	case ^Ident_Expr:
		emit(p, n.name)
	case ^Literal_Expr:
		emit(p, n.value)
	case ^Table_Expr:
		emit_node(p, n.table)
		emit(p, "[")
		if len(n.selectors) > 0 {
			emit_space(p)
			emit_expr_list(p, n.selectors, " ")
			emit_space(p)
		}
		emit(p, "]")
	case ^Selector_Expr:
		emit_node(p, n.base)
		emit(p, selector_op_text(n.op))
		emit_node(p, n.field)
	case ^Substring_Expr:
		emit_node(p, n.base)
		if n.offset != nil {
			emit(p, "+")
			emit_node(p, n.offset)
		}
		if n.length != nil {
			emit(p, "(")
			emit_node(p, n.length)
			emit(p, ")")
		}
	case ^Call_Expr:
		emit_node(p, n.callee)
		emit_node(p, n.args)
	case ^Call_Arg_List_Expr:
		emit(p, "( ")
		emit_expr_list(p, n.args, " ")
		emit(p, " )")
	case ^Call_Arg_Section_Expr:
		emit(p, n.name)
		if len(n.args) > 0 {
			emit_space(p)
			emit_expr_list(p, n.args, " ")
		}
	case ^Call_Named_Arg_Expr:
		emit(p, n.name)
		emit(p, " = ")
		emit_node(p, n.value)
	case ^Call_Positional_Arg_Expr:
		emit_node(p, n.value)
	case ^Constructor_Expr:
		emit(p, constructor_kind_text(n.kind))
		emit_space(p)
		emit_node(p, n.type_ref)
		emit(p, "( ")
		emit_expr_list(p, n.args, " ")
		emit(p, " )")
	case ^Data_Inline_Name_Expr:
		emit(p, "DATA(")
		emit(p, n.name)
		emit(p, ")")
	case ^Field_Symbol_Inline_Name_Expr:
		emit(p, "FIELD-SYMBOL(")
		emit(p, n.name)
		emit(p, ")")
	case ^Data_Decl:
		emit_data_decl(p, n)
	case ^Data_Chained_Decl:
		emit_data_chained_decl(p, n)
	case ^Data_Inline_Decl:
		emit(p, "DATA(")
		emit(p, n.name)
		emit(p, ") = ")
		emit_node(p, n.expr)
		emit(p, ".")
	case ^Types_Decl:
		emit_types_decl(p, n)
	case ^Constants_Decl:
		emit_constants_decl(p, n)
	case ^Field_Symbols_Decl:
		emit_field_symbols_decl(p, n)
	case ^Statics_Decl:
		emit_statics_decl(p, n)
	case ^Tables_Decl:
		emit_tables_decl(p, n)
	case ^Ranges_Decl:
		emit_ranges_decl(p, n)
	case ^Parameters_Decl:
		emit_parameters_decl(p, n)
	case ^Select_Options_Decl:
		emit_select_options_decl(p, n)
	case ^Controls_Decl:
		emit_controls_decl(p, n)
	case ^Class_Data_Decl:
		emit_class_data_decl(p, n)
	case ^Assign_Stmt:
		emit_node(p, n.lhs)
		emit(p, " = ")
		emit_node(p, n.rhs)
		emit(p, ".")
	case ^Downcast_Assign_Stmt:
		emit_node(p, n.lhs)
		emit(p, " ?= ")
		emit_node(p, n.rhs)
		emit(p, ".")
	case ^Expr_Stmt:
		emit_node(p, n.expr)
		emit(p, ".")
	case ^Clear_Stmt:
		emit_clear_stmt(p, n)
	case ^Refresh_Stmt:
		emit_refresh_stmt(p, n)
	case ^Free_Stmt:
		emit_free_stmt(p, n)
	case ^Unassign_Stmt:
		emit_unassign_stmt(p, n)
	case ^Move_Stmt:
		emit_move_stmt(p, n)
	case ^Add_Stmt:
		emit_add_stmt(p, n)
	case ^Subtract_Stmt:
		emit_subtract_stmt(p, n)
	case ^Multiply_Stmt:
		emit_multiply_stmt(p, n)
	case ^Divide_Stmt:
		emit_divide_stmt(p, n)
	case ^Compute_Stmt:
		emit_compute_stmt(p, n)
	case ^Concatenate_Stmt:
		emit_concatenate_stmt(p, n)
	case ^Split_Stmt:
		emit_split_stmt(p, n)
	case ^Condense_Stmt:
		emit(p, "CONDENSE ")
		emit_node(p, n.target)
		if n.no_gaps {
			emit(p, " NO-GAPS")
		}
		emit(p, ".")
	case ^Replace_Stmt:
		emit_replace_stmt(p, n)
	case ^Translate_Stmt:
		emit_translate_stmt(p, n)
	case ^Shift_Stmt:
		emit_shift_stmt(p, n)
	case ^Find_Stmt:
		emit_find_stmt(p, n)
	case ^Search_Stmt:
		emit_search_stmt(p, n)
	case ^Perform_Stmt:
		emit_perform_stmt(p, n)
	case ^Call_Stmt:
		emit_call_stmt(p, n)
	case ^Submit_Stmt:
		emit_submit_stmt(p, n)
	case ^Message_Stmt:
		emit_message_stmt(p, n)
	case ^Write_Stmt:
		emit_write_stmt(p, n)
	case ^If_Stmt:
		emit_if_stmt(p, n)
	case ^Case_Stmt:
		emit_case_stmt(p, n)
	case ^While_Stmt:
		emit(p, "WHILE ")
		emit_node(p, n.condition)
		emit_block(p, n.body, "ENDWHILE")
	case ^Do_Stmt:
		emit(p, "DO")
		if n.count != nil {
			emit_space(p)
			emit_node(p, n.count)
			emit(p, " TIMES")
		}
		emit_block(p, n.body, "ENDDO")
	case ^Loop_Stmt:
		emit(p, "LOOP AT ")
		emit_node(p, n.source)
		emit_block(p, n.body, "ENDLOOP")
	case ^At_Stmt:
		emit(p, "AT ")
		emit(p, n.kind)
		if n.expr != nil {
			emit_space(p)
			emit_node(p, n.expr)
		}
		emit_block(p, n.body, "ENDAT")
	case ^Try_Stmt:
		emit_try_stmt(p, n)
	case ^Class_Decl:
		emit_named_block(p, "CLASS", n.name, n.body, "ENDCLASS")
	case ^Interface_Decl:
		emit_named_block(p, "INTERFACE", n.name, n.body, "ENDINTERFACE")
	case ^Method_Decl:
		emit_named_block(p, "METHOD", n.name, n.body, "ENDMETHOD")
	case ^Form_Decl:
		emit_named_block(p, "FORM", n.name, n.body, "ENDFORM")
	case ^Function_Decl:
		emit_named_block(p, "FUNCTION", n.name, n.body, "ENDFUNCTION")
	case ^Module_Decl:
		emit_named_block(p, "MODULE", n.name, n.body, "ENDMODULE")
	case ^Event_Block_Stmt:
		emit_named_block(p, n.kind, "", n.body, "")
	case ^Enhancement_Stmt:
		emit_named_block(p, "ENHANCEMENT", n.name, n.body, "ENDENHANCEMENT")
	case ^Enhancement_Section_Stmt:
		emit_named_block(p, "ENHANCEMENT-SECTION", n.name, n.body, "END-ENHANCEMENT-SECTION")
	case ^Test_Seam_Stmt:
		emit_named_block(p, "TEST-SEAM", n.name, n.body, "END-TEST-SEAM")
	case ^Test_Injection_Stmt:
		emit_named_block(p, "TEST-INJECTION", n.name, n.body, "END-TEST-INJECTION")
	case ^Invalid_Stmt:
		emit(p, "?")
	case:
		emit(p, "?")
	}
}

emit_file :: proc(p: ^Printer, file: ^File) {
	for stmt, i in file.stmts {
		if i > 0 {
			emit_newline(p)
		}
		emit_node(p, stmt)
	}
}

emit_expr_list :: proc(p: ^Printer, list: [dynamic]^Expr, separator: string) {
	for expr, i in list {
		if i > 0 {
			emit(p, separator)
		}
		emit_node(p, expr)
	}
}

emit_stmt_list :: proc(p: ^Printer, list: [dynamic]^Stmt) {
	for stmt, i in list {
		if i > 0 {
			emit_newline(p)
		}
		emit_node(p, stmt)
	}
}

emit_block :: proc(p: ^Printer, body: [dynamic]^Stmt, end_keyword: string) {
	emit(p, ".")
	p.indent_level += 1
	if len(body) > 0 {
		emit_newline(p)
		emit_stmt_list(p, body)
	}
	p.indent_level -= 1
	emit_newline(p)
	emit(p, end_keyword)
	emit(p, ".")
}

emit_template :: proc(p: ^Printer, expr: ^Char_String_Template_Expr) {
	emit(p, "|")
	for part in expr.parts {
		emit_node(p, part)
	}
	emit(p, "|")
}

emit_template_interpolation :: proc(p: ^Printer, expr: ^Template_Interpolation_Expr) {
	emit(p, "{ ")
	emit_node(p, expr.expr)
	for spec in expr.format_specs {
		emit_space(p)
		emit_node(p, spec)
	}
	emit(p, " }")
}

emit_data_decl :: proc(p: ^Printer, decl: ^Data_Decl) {
	emit(p, "DATA ")
	emit(p, decl.name)
	emit_type_clause(p, decl.type_clause)
	emit(p, ".")
}

emit_data_chained_decl :: proc(p: ^Printer, decl: ^Data_Chained_Decl) {
	emit(p, "DATA: ")
	for branch, i in decl.decls {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, branch.name)
		emit_type_clause(p, branch.type_clause)
	}
	emit(p, ".")
}

emit_types_decl :: proc(p: ^Printer, decl: ^Types_Decl) {
	emit(p, "TYPES")
	emit(p, ": " if len(decl.types) > 1 else " ")
	for clause, i in decl.types {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_paren_length(p, clause.paren_length)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
	}
	emit(p, ".")
}

emit_constants_decl :: proc(p: ^Printer, decl: ^Constants_Decl) {
	emit(p, "CONSTANTS")
	emit(p, ": " if len(decl.constants) > 1 else " ")
	for clause, i in decl.constants {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_paren_length(p, clause.paren_length)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
		emit_value_clause(p, clause.value_clause)
	}
	emit(p, ".")
}

emit_field_symbols_decl :: proc(p: ^Printer, decl: ^Field_Symbols_Decl) {
	emit(p, "FIELD-SYMBOLS")
	emit(p, ": " if len(decl.field_symbols) > 1 else " ")
	for clause, i in decl.field_symbols {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_type_clause(p, clause.type_clause)
	}
	emit(p, ".")
}

emit_statics_decl :: proc(p: ^Printer, decl: ^Statics_Decl) {
	emit(p, "STATICS")
	emit(p, ": " if len(decl.statics) > 1 else " ")
	for clause, i in decl.statics {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_paren_length(p, clause.paren_length)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
		emit_value_clause(p, clause.value_clause)
	}
	emit(p, ".")
}

emit_tables_decl :: proc(p: ^Printer, decl: ^Tables_Decl) {
	emit(p, "TABLES")
	emit(p, ": " if len(decl.tables) > 1 else " ")
	for clause, i in decl.tables {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
	}
	emit(p, ".")
}

emit_ranges_decl :: proc(p: ^Printer, decl: ^Ranges_Decl) {
	emit(p, "RANGES")
	emit(p, ": " if len(decl.ranges) > 1 else " ")
	for clause, i in decl.ranges {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		if clause.for_clause != nil {
			emit(p, " FOR ")
			emit_node(p, clause.for_clause.expr)
		}
	}
	emit(p, ".")
}

emit_parameters_decl :: proc(p: ^Printer, decl: ^Parameters_Decl) {
	emit(p, "PARAMETERS")
	emit(p, ": " if len(decl.parameters) > 1 else " ")
	for clause, i in decl.parameters {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_paren_length(p, clause.paren_length)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
		emit_default_clause(p, clause.default_clause)
		emit_parameter_additions(p, clause)
	}
	emit(p, ".")
}

emit_select_options_decl :: proc(p: ^Printer, decl: ^Select_Options_Decl) {
	emit(p, "SELECT-OPTIONS")
	emit(p, ": " if len(decl.options) > 1 else " ")
	for clause, i in decl.options {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		if clause.for_clause != nil {
			emit(p, " FOR ")
			emit_node(p, clause.for_clause.expr)
		}
		emit_default_clause(p, clause.default_clause)
		if clause.to_clause != nil {
			emit(p, " TO ")
			emit_node(p, clause.to_clause.expr)
		}
		if clause.option_clause != nil {
			emit(p, " OPTION ")
			emit(p, clause.option_clause.option)
		}
		if clause.sign_clause != nil {
			emit(p, " SIGN ")
			emit(p, clause.sign_clause.sign)
		}
		emit_select_option_additions(p, clause)
	}
	emit(p, ".")
}

emit_controls_decl :: proc(p: ^Printer, decl: ^Controls_Decl) {
	emit(p, "CONTROLS")
	emit(p, ": " if len(decl.controls) > 1 else " ")
	for clause, i in decl.controls {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_type_clause(p, clause.type_clause)
		if clause.using_screen != nil {
			emit(p, " USING SCREEN ")
			emit_node(p, clause.using_screen.screen)
		}
	}
	emit(p, ".")
}

emit_class_data_decl :: proc(p: ^Printer, decl: ^Class_Data_Decl) {
	emit(p, "CLASS-DATA")
	emit(p, ": " if len(decl.decls) > 1 else " ")
	for clause, i in decl.decls {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_paren_length(p, clause.paren_length)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
		emit_value_clause(p, clause.value_clause)
	}
	emit(p, ".")
}

emit_type_clause :: proc(p: ^Printer, clause: ^Data_Type_Clause) {
	if clause == nil {
		return
	}
	emit_space(p)
	switch clause.form {
	case .Type:
		emit(p, "TYPE")
	case .Like:
		emit(p, "LIKE")
	case .Ref_To:
		emit(p, "TYPE REF TO")
	case .Like_Line_Of:
		emit(p, "LIKE LINE OF")
	case .Standard_Table:
		emit(p, "TYPE STANDARD TABLE")
	case .Sorted_Table:
		emit(p, "TYPE SORTED TABLE")
	case .Hashed_Table:
		emit(p, "TYPE HASHED TABLE")
	}
	if clause.type_ref != nil {
		if clause.form == .Standard_Table || clause.form == .Sorted_Table || clause.form == .Hashed_Table {
			emit(p, " OF")
		}
		emit_space(p)
		emit_node(p, clause.type_ref)
	}
}

emit_paren_length :: proc(p: ^Printer, clause: ^Paren_Length_Clause) {
	if clause != nil {
		emit(p, "(")
		emit_node(p, clause.expr)
		emit(p, ")")
	}
}

emit_length_clauses :: proc(p: ^Printer, clauses: [dynamic]Length_Clause) {
	for clause in clauses {
		emit_space(p)
		emit(p, "DECIMALS" if clause.kind == .Decimals else "LENGTH")
		emit_space(p)
		emit_node(p, clause.expr)
	}
}

emit_value_clause :: proc(p: ^Printer, clause: ^Value_Clause) {
	if clause != nil {
		emit(p, " VALUE ")
		emit_node(p, clause.expr)
	}
}

emit_default_clause :: proc(p: ^Printer, clause: ^Default_Clause) {
	if clause != nil {
		emit(p, " DEFAULT ")
		emit_node(p, clause.expr)
	}
}

emit_parameter_additions :: proc(p: ^Printer, clause: Parameters_Clause) {
	if .As_Checkbox in clause.flags {emit(p, " AS CHECKBOX")}
	if .Lower_Case in clause.flags {emit(p, " LOWER CASE")}
	if .Obligatory in clause.flags {emit(p, " OBLIGATORY")}
	if .No_Display in clause.flags {emit(p, " NO-DISPLAY")}
	if .Value_Check in clause.flags {emit(p, " VALUE CHECK")}
	if .Help_Request in clause.flags {emit(p, " HELP-REQUEST")}
	if .Value_Request in clause.flags {emit(p, " VALUE-REQUEST")}
	if clause.radiobutton_group != nil {
		emit(p, " RADIOBUTTON GROUP ")
		emit(p, clause.radiobutton_group.group)
	}
	if clause.user_command != nil {
		emit(p, " USER-COMMAND ")
		emit(p, clause.user_command.command)
	}
	if clause.modif_id != nil {
		emit(p, " MODIF ID ")
		emit(p, clause.modif_id.id)
	}
	if clause.memory_id != nil {
		emit(p, " MEMORY ID ")
		emit_node(p, clause.memory_id.id)
	}
	if clause.matchcode_object != nil {
		emit(p, " MATCHCODE OBJECT ")
		emit_node(p, clause.matchcode_object.object)
	}
	if clause.visible_length != nil {
		emit(p, " VISIBLE LENGTH ")
		emit_node(p, clause.visible_length.length)
	}
}

emit_select_option_additions :: proc(p: ^Printer, clause: Select_Options_Clause) {
	if .Lower_Case in clause.flags {emit(p, " LOWER CASE")}
	if .Obligatory in clause.flags {emit(p, " OBLIGATORY")}
	if .No_Display in clause.flags {emit(p, " NO-DISPLAY")}
	if .No_Extension in clause.flags {emit(p, " NO-EXTENSION")}
	if .No_Intervals in clause.flags {emit(p, " NO INTERVALS")}
	if .No_Database_Selection in clause.flags {emit(p, " NO DATABASE SELECTION")}
	if clause.modif_id != nil {
		emit(p, " MODIF ID ")
		emit(p, clause.modif_id.id)
	}
	if clause.memory_id != nil {
		emit(p, " MEMORY ID ")
		emit_node(p, clause.memory_id.id)
	}
	if clause.matchcode_object != nil {
		emit(p, " MATCHCODE OBJECT ")
		emit_node(p, clause.matchcode_object.object)
	}
	if clause.visible_length != nil {
		emit(p, " VISIBLE LENGTH ")
		emit_node(p, clause.visible_length.length)
	}
	if clause.help_request != nil {
		emit(p, " HELP-REQUEST FOR ")
		emit(p, clause.help_request.target)
	}
	if clause.value_request != nil {
		emit(p, " VALUE-REQUEST FOR ")
		emit(p, clause.value_request.target)
	}
}

emit_clear_stmt :: proc(p: ^Printer, stmt: ^Clear_Stmt) {
	emit(p, "CLEAR")
	emit(p, ": " if len(stmt.operands) > 1 else " ")
	for clause, i in stmt.operands {
		if i > 0 {emit(p, ", ")}
		emit_node(p, clause.target)
		if clause.mode == .With_Value {
			emit(p, " WITH ")
			emit_node(p, clause.value)
		} else if clause.mode == .Initial {
			emit(p, " INITIAL")
		}
	}
	emit(p, ".")
}

emit_refresh_stmt :: proc(p: ^Printer, stmt: ^Refresh_Stmt) {
	emit(p, "REFRESH")
	emit(p, ": " if len(stmt.operands) > 1 else " ")
	for clause, i in stmt.operands {
		if i > 0 {emit(p, ", ")}
		if clause.table {emit(p, "TABLE ")}
		emit_node(p, clause.target)
	}
	emit(p, ".")
}

emit_free_stmt :: proc(p: ^Printer, stmt: ^Free_Stmt) {
	emit(p, "FREE ")
	if stmt.memory {
		emit(p, "MEMORY")
		if stmt.memory_id != nil {
			emit(p, " ID ")
			emit_node(p, stmt.memory_id)
		}
		emit(p, ".")
		return
	}
	if len(stmt.operands) > 1 {
		emit(p, ": ")
	}
	for clause, i in stmt.operands {
		if i > 0 {emit(p, ", ")}
		if clause.object {emit(p, "OBJECT ")}
		emit_node(p, clause.target)
	}
	emit(p, ".")
}

emit_unassign_stmt :: proc(p: ^Printer, stmt: ^Unassign_Stmt) {
	emit(p, "UNASSIGN")
	emit(p, ": " if len(stmt.operands) > 1 else " ")
	for clause, i in stmt.operands {
		if i > 0 {emit(p, ", ")}
		emit_node(p, clause.target)
	}
	emit(p, ".")
}

emit_move_stmt :: proc(p: ^Printer, stmt: ^Move_Stmt) {
	emit(p, "MOVE")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {emit(p, ", ")}
		emit_node(p, entry.source)
		emit(p, " TO ")
		emit_node(p, entry.target)
	}
	emit(p, ".")
}

emit_add_stmt :: proc(p: ^Printer, stmt: ^Add_Stmt) {
	emit(p, "ADD")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {emit(p, ", ")}
		emit_node(p, entry.source)
		emit(p, " TO ")
		emit_node(p, entry.target)
		if entry.result != nil {
			emit(p, " GIVING ")
			emit_node(p, entry.result)
		}
	}
	emit(p, ".")
}

emit_subtract_stmt :: proc(p: ^Printer, stmt: ^Subtract_Stmt) {
	emit(p, "SUBTRACT")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {emit(p, ", ")}
		emit_node(p, entry.source)
		emit(p, " FROM ")
		emit_node(p, entry.target)
		if entry.result != nil {
			emit(p, " GIVING ")
			emit_node(p, entry.result)
		}
	}
	emit(p, ".")
}

emit_multiply_stmt :: proc(p: ^Printer, stmt: ^Multiply_Stmt) {
	emit(p, "MULTIPLY")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {emit(p, ", ")}
		emit_node(p, entry.target)
		emit(p, " BY ")
		emit_node(p, entry.source)
		if entry.result != nil {
			emit(p, " GIVING ")
			emit_node(p, entry.result)
		}
	}
	emit(p, ".")
}

emit_divide_stmt :: proc(p: ^Printer, stmt: ^Divide_Stmt) {
	emit(p, "DIVIDE")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {emit(p, ", ")}
		if entry.form == .Into {
			emit_node(p, entry.source)
			emit(p, " INTO ")
			emit_node(p, entry.target)
		} else {
			emit_node(p, entry.target)
			emit(p, " BY ")
			emit_node(p, entry.source)
		}
		if entry.result != nil {
			emit(p, " GIVING ")
			emit_node(p, entry.result)
		}
	}
	emit(p, ".")
}

emit_compute_stmt :: proc(p: ^Printer, stmt: ^Compute_Stmt) {
	emit(p, "COMPUTE")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {emit(p, ", ")}
		if entry.exact {emit(p, "EXACT ")}
		emit_node(p, entry.target)
		emit(p, " = ")
		emit_node(p, entry.source)
	}
	emit(p, ".")
}

emit_concatenate_stmt :: proc(p: ^Printer, stmt: ^Concatenate_Stmt) {
	emit(p, "CONCATENATE")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {emit(p, ", ")}
		if entry.lines_of {emit(p, "LINES OF ")}
		emit_expr_list(p, entry.sources, " ")
		emit(p, " INTO ")
		emit_node(p, entry.target)
		if entry.separator != nil {
			emit(p, " SEPARATED BY ")
			emit_node(p, entry.separator)
		}
		if entry.respecting_blanks {
			emit(p, " RESPECTING BLANKS")
		}
	}
	emit(p, ".")
}

emit_split_stmt :: proc(p: ^Printer, stmt: ^Split_Stmt) {
	emit(p, "SPLIT")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {emit(p, ", ")}
		emit_node(p, entry.source)
		emit(p, " AT ")
		emit_node(p, entry.separator)
		emit(p, " INTO ")
		if entry.into_table {emit(p, "TABLE ")}
		emit_expr_list(p, entry.targets, " ")
	}
	emit(p, ".")
}

emit_replace_stmt :: proc(p: ^Printer, stmt: ^Replace_Stmt) {
	emit(p, "REPLACE ")
	if stmt.occurrence == .First {
		emit(p, "FIRST OCCURRENCE OF ")
	} else if stmt.occurrence == .All {
		emit(p, "ALL OCCURRENCES OF ")
	}
	if stmt.regex {emit(p, "REGEX ")}
	emit_node(p, stmt.pattern)
	if stmt.target != nil {
		emit(p, " IN ")
		if stmt.in_table {emit(p, "TABLE ")}
		emit_node(p, stmt.target)
	}
	if stmt.replacement != nil {
		emit(p, " WITH ")
		emit_node(p, stmt.replacement)
	}
	emit(p, ".")
}

emit_translate_stmt :: proc(p: ^Printer, stmt: ^Translate_Stmt) {
	emit(p, "TRANSLATE ")
	emit_node(p, stmt.target)
	switch stmt.form {
	case .Using:
		emit(p, " USING ")
		emit_node(p, stmt.operand)
	case .To_Upper:
		emit(p, " TO UPPER CASE")
	case .To_Lower:
		emit(p, " TO LOWER CASE")
	case .To_Code_Page:
		emit(p, " TO CODE PAGE ")
		emit_node(p, stmt.operand)
	case .From_Code_Page:
		emit(p, " FROM CODE PAGE ")
		emit_node(p, stmt.operand)
	case .To_Number_Format:
		emit(p, " TO NUMBER FORMAT ")
		emit_node(p, stmt.operand)
	case .From_Number_Format:
		emit(p, " FROM NUMBER FORMAT ")
		emit_node(p, stmt.operand)
	case .Default:
	}
	emit(p, ".")
}

emit_shift_stmt :: proc(p: ^Printer, stmt: ^Shift_Stmt) {
	emit(p, "SHIFT ")
	emit_node(p, stmt.target)
	if stmt.direction == .Left {
		emit(p, " LEFT")
	} else if stmt.direction == .Right {
		emit(p, " RIGHT")
	}
	if stmt.places != nil {
		emit(p, " BY ")
		emit_node(p, stmt.places)
		emit(p, " PLACES")
	}
	if stmt.circular {emit(p, " CIRCULAR")}
	if stmt.delete_direction != .None {
		emit(p, " DELETING ")
		emit(p, "LEADING" if stmt.delete_direction == .Leading else "TRAILING")
		emit_space(p)
		emit_node(p, stmt.delete_pattern)
	}
	emit(p, ".")
}

emit_find_stmt :: proc(p: ^Printer, stmt: ^Find_Stmt) {
	emit(p, "FIND ")
	if stmt.occurrence == .First {
		emit(p, "FIRST OCCURRENCE OF ")
	} else if stmt.occurrence == .All {
		emit(p, "ALL OCCURRENCES OF ")
	}
	if stmt.regex {emit(p, "REGEX ")}
	emit_node(p, stmt.pattern)
	if stmt.target != nil {
		emit(p, " IN ")
		emit_node(p, stmt.target)
	}
	if stmt.match_offset != nil {
		emit(p, " MATCH OFFSET ")
		emit_node(p, stmt.match_offset)
	}
	if stmt.match_length != nil {
		emit(p, " MATCH LENGTH ")
		emit_node(p, stmt.match_length)
	}
	if stmt.results != nil {
		emit(p, " RESULTS ")
		emit_node(p, stmt.results)
	}
	if len(stmt.submatches) > 0 {
		emit(p, " SUBMATCHES ")
		emit_expr_list(p, stmt.submatches, " ")
	}
	emit(p, ".")
}

emit_search_stmt :: proc(p: ^Printer, stmt: ^Search_Stmt) {
	emit(p, "SEARCH ")
	emit_node(p, stmt.target)
	if stmt.pattern != nil {
		emit(p, " FOR ")
		emit_node(p, stmt.pattern)
	}
	if stmt.starting_at != nil {
		emit(p, " STARTING AT ")
		emit_node(p, stmt.starting_at)
	}
	if stmt.ending_at != nil {
		emit(p, " ENDING AT ")
		emit_node(p, stmt.ending_at)
	}
	if stmt.abbreviated {emit(p, " ABBREVIATED")}
	emit(p, ".")
}

emit_perform_stmt :: proc(p: ^Printer, stmt: ^Perform_Stmt) {
	emit(p, "PERFORM ")
	emit_node(p, stmt.form)
	if stmt.program != nil {
		emit(p, " IN PROGRAM ")
		emit_node(p, stmt.program)
	}
	if len(stmt.tables) > 0 {
		emit(p, " TABLES ")
		emit_expr_list(p, stmt.tables, " ")
	}
	if len(stmt.using_args) > 0 {
		emit(p, " USING ")
		emit_expr_list(p, stmt.using_args, " ")
	}
	if len(stmt.changing) > 0 {
		emit(p, " CHANGING ")
		emit_expr_list(p, stmt.changing, " ")
	}
	if stmt.if_found {emit(p, " IF FOUND")}
	emit(p, ".")
}

emit_call_stmt :: proc(p: ^Printer, stmt: ^Call_Stmt) {
	if stmt.kind == .Direct {
		emit_node(p, stmt.call)
		emit(p, ".")
		return
	}
	emit(p, "CALL ")
	emit(p, call_kind_text(stmt.kind))
	if stmt.target != nil {
		emit_space(p)
		emit_node(p, stmt.target)
	}
	emit(p, ".")
}

emit_submit_stmt :: proc(p: ^Printer, stmt: ^Submit_Stmt) {
	emit(p, "SUBMIT ")
	emit_node(p, stmt.target)
	for option in stmt.options {
		emit_submit_option(p, option)
	}
	if stmt.via_selection_screen {emit(p, " VIA SELECTION-SCREEN")}
	if stmt.exporting_list_to_memory {emit(p, " EXPORTING LIST TO MEMORY")}
	if stmt.to_sap_spool {emit(p, " TO SAP-SPOOL")}
	if stmt.without_spool_dynpro {emit(p, " WITHOUT SPOOL DYNPRO")}
	if stmt.and_return {emit(p, " AND RETURN")}
	emit(p, ".")
}

emit_submit_option :: proc(p: ^Printer, option: Submit_Option_Clause) {
	switch option.kind {
	case .Using_Selection_Screen:
		emit(p, " USING SELECTION-SCREEN ")
		emit_node(p, option.value)
	case .Using_Selection_Set:
		emit(p, " USING SELECTION-SET ")
		emit_node(p, option.value)
	case .With_Selection_Table:
		emit(p, " WITH SELECTION-TABLE ")
		emit_node(p, option.value)
	case .With_Free_Selections:
		emit(p, " WITH FREE SELECTIONS ")
		emit_node(p, option.value)
	case .With_Parameter:
		emit(p, " WITH ")
		emit(p, option.name)
		if option.operator != .None {
			emit_space(p)
			emit(p, submit_operator_text(option.operator))
			emit_space(p)
			emit_node(p, option.value)
		}
	case .Line_Size:
		emit(p, " LINE-SIZE ")
		emit_node(p, option.value)
	case .Line_Count:
		emit(p, " LINE-COUNT ")
		emit_node(p, option.value)
	case .User:
		emit(p, " USER ")
		emit_node(p, option.value)
	case .Via_Job:
		emit(p, " VIA JOB ")
		emit_node(p, option.value)
	case .Number:
		emit(p, " NUMBER ")
		emit_node(p, option.value)
	case .Language:
		emit(p, " LANGUAGE ")
		emit_node(p, option.value)
	case .Using_Selection_Sets_Of_Program:
	}
}

emit_message_stmt :: proc(p: ^Printer, stmt: ^Message_Stmt) {
	emit(p, "MESSAGE")
	emit_message_head(p, stmt.head)
	if len(stmt.with_args) > 0 {
		emit(p, " WITH ")
		emit_expr_list(p, stmt.with_args, " ")
	}
	if stmt.into != nil {
		emit(p, " INTO ")
		emit_node(p, stmt.into)
	}
	if stmt.display_like != nil {
		emit(p, " DISPLAY LIKE ")
		emit_node(p, stmt.display_like)
	}
	if stmt.raising != nil {
		emit(p, " RAISING ")
		emit_node(p, stmt.raising)
	}
	emit(p, ".")
}

emit_message_head :: proc(p: ^Printer, head: ^Message_Head_Clause) {
	if head == nil {return}
	if head.id != nil {
		emit(p, " ID ")
		emit_node(p, head.id)
		if head.msg_type != nil {
			emit(p, " TYPE ")
			emit_node(p, head.msg_type)
		}
		if head.number != nil {
			emit(p, " NUMBER ")
			emit_node(p, head.number)
		}
		return
	}
	if head.code != nil {
		emit_space(p)
		emit_node(p, head.code)
	}
	if head.msg_type != nil {
		emit(p, " TYPE ")
		emit_node(p, head.msg_type)
	}
}

emit_write_stmt :: proc(p: ^Printer, stmt: ^Write_Stmt) {
	emit(p, "WRITE")
	if len(stmt.operands) > 0 {emit_space(p)}
	for clause, i in stmt.operands {
		if i > 0 {emit_space(p)}
		if clause.line_break {emit(p, "/")}
		if clause.position != nil {
			if !clause.line_break {emit(p, "AT ")}
			emit_node(p, clause.position)
		}
		if clause.length != nil {
			emit(p, "(")
			emit_node(p, clause.length)
			emit(p, ")")
		}
		if clause.value != nil {
			if clause.line_break || clause.position != nil || clause.length != nil {emit_space(p)}
			emit_node(p, clause.value)
		}
	}
	emit(p, ".")
}

emit_if_stmt :: proc(p: ^Printer, stmt: ^If_Stmt) {
	emit(p, "IF ")
	emit_node(p, stmt.condition)
	emit(p, ".")
	p.indent_level += 1
	if len(stmt.body) > 0 {
		emit_newline(p)
		emit_stmt_list(p, stmt.body)
	}
	p.indent_level -= 1
	for clause in stmt.elseif_clauses {
		emit_newline(p)
		emit(p, "ELSEIF ")
		emit_node(p, clause.condition)
		emit(p, ".")
		p.indent_level += 1
		if len(clause.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, clause.body)
		}
		p.indent_level -= 1
	}
	if stmt.else_clause != nil {
		emit_newline(p)
		emit(p, "ELSE.")
		p.indent_level += 1
		if len(stmt.else_clause.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, stmt.else_clause.body)
		}
		p.indent_level -= 1
	}
	emit_newline(p)
	emit(p, "ENDIF.")
}

emit_case_stmt :: proc(p: ^Printer, stmt: ^Case_Stmt) {
	emit(p, "CASE ")
	emit_node(p, stmt.expr)
	emit(p, ".")
	for clause in stmt.whens {
		emit_newline(p)
		emit(p, "WHEN")
		if clause.is_others {
			emit(p, " OTHERS")
		} else if len(clause.operands) > 0 {
			emit_space(p)
			emit_expr_list(p, clause.operands, " OR ")
		}
		emit(p, ".")
		p.indent_level += 1
		if len(clause.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, clause.body)
		}
		p.indent_level -= 1
	}
	emit_newline(p)
	emit(p, "ENDCASE.")
}

emit_try_stmt :: proc(p: ^Printer, stmt: ^Try_Stmt) {
	emit(p, "TRY.")
	p.indent_level += 1
	if len(stmt.body) > 0 {
		emit_newline(p)
		emit_stmt_list(p, stmt.body)
	}
	p.indent_level -= 1
	for clause in stmt.catches {
		emit_newline(p)
		emit(p, "CATCH ")
		emit_expr_list(p, clause.exceptions, " ")
		if clause.into != nil {
			emit(p, " INTO ")
			emit_node(p, clause.into)
		}
		emit(p, ".")
		p.indent_level += 1
		if len(clause.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, clause.body)
		}
		p.indent_level -= 1
	}
	if stmt.cleanup != nil {
		emit_newline(p)
		emit(p, "CLEANUP.")
		p.indent_level += 1
		if len(stmt.cleanup.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, stmt.cleanup.body)
		}
		p.indent_level -= 1
	}
	emit_newline(p)
	emit(p, "ENDTRY.")
}

emit_named_block :: proc(p: ^Printer, start_keyword, name: string, body: [dynamic]^Stmt, end_keyword: string) {
	emit(p, start_keyword)
	if name != "" {
		emit_space(p)
		emit(p, name)
	}
	if end_keyword == "" {
		emit(p, ".")
		p.indent_level += 1
		if len(body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, body)
		}
		p.indent_level -= 1
	} else {
		emit_block(p, body, end_keyword)
	}
}

binary_op_text :: proc(op: Binary_Op) -> string {
	switch op {
	case .Add: return "+"
	case .Subtract: return "-"
	case .Multiply: return "*"
	case .Divide: return "/"
	case .Integer_Divide: return "DIV"
	case .Modulo: return "MOD"
	case .Concatenate: return "&&"
	case .Equal: return "="
	case .Not_Equal: return "<>"
	case .Less: return "<"
	case .Less_Equal: return "<="
	case .Greater: return ">"
	case .Greater_Equal: return ">="
	case .Contains_Only: return "CO"
	case .Contains_Not_Only: return "CN"
	case .Contains_Any: return "CA"
	case .Contains_Not_Any: return "NA"
	case .Contains_String: return "CS"
	case .Contains_No_String: return "NS"
	case .Covers_Pattern: return "CP"
	case .Covers_No_Pattern: return "NP"
	case .In: return "IN"
	case .And: return "AND"
	case .Or: return "OR"
	case .Is: return "IS"
	case .Between: return "BETWEEN"
	}
	return "?"
}

unary_op_text :: proc(op: Unary_Op) -> string {
	switch op {
	case .Minus: return "-"
	case .Plus: return "+"
	case .Not: return "NOT"
	}
	return "?"
}

selector_op_text :: proc(op: Selector_Op) -> string {
	switch op {
	case .Dash: return "-"
	case .Arrow: return "->"
	case .Fat_Arrow: return "=>"
	case .Tilde: return "~"
	}
	return "?"
}

constructor_kind_text :: proc(kind: Constructor_Kind) -> string {
	switch kind {
	case .New: return "NEW"
	case .Value: return "VALUE"
	case .Conv: return "CONV"
	case .Ref: return "REF"
	case .Cast: return "CAST"
	case .Exact: return "EXACT"
	case .Corresponding: return "CORRESPONDING"
	case .Filter: return "FILTER"
	case .Reduce: return "REDUCE"
	case .Switch: return "SWITCH"
	case .Cond: return "COND"
	}
	return "?"
}

call_kind_text :: proc(kind: Call_Kind) -> string {
	switch kind {
	case .Direct: return ""
	case .Method: return "METHOD"
	case .Function: return "FUNCTION"
	case .Customer_Function: return "CUSTOMER-FUNCTION"
	case .Database_Procedure: return "DATABASE PROCEDURE"
	case .Transformation: return "TRANSFORMATION"
	case .Badi: return "BADI"
	case .Screen: return "SCREEN"
	case .Selection_Screen: return "SELECTION-SCREEN"
	case .Transaction: return "TRANSACTION"
	case .Dialog: return "DIALOG"
	case .Subscreen: return "SUBSCREEN"
	}
	return "?"
}

submit_operator_text :: proc(op: Submit_Option_Operator) -> string {
	switch op {
	case .None: return ""
	case .Assign: return "="
	case .Eq: return "EQ"
	case .Ne: return "NE"
	case .Bt: return "BT"
	case .Nb: return "NB"
	case .Cp: return "CP"
	case .Np: return "NP"
	case .Ge: return "GE"
	case .Gt: return "GT"
	case .Le: return "LE"
	case .Lt: return "LT"
	case .Other: return "="
	}
	return "?"
}
