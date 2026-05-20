package abap_frontend_parser

import "../ast"
import "../tokenizer"

import "base:runtime"
import "core:strings"
import "core:testing"

Node_Counts :: struct {
	binary:        int,
	selector:      int,
	table:         int,
	template:      int,
	interpolation: int,
	format_spec:   int,
	constructor:   int,
	data_decl:     int,
	data_inline:   int,
	types_decl:    int,
	constants:     int,
	field_symbols: int,
	statics:       int,
	tables_decl:   int,
	ranges:        int,
	parameters:    int,
	select_options: int,
	controls:      int,
	class_data:    int,
	assign:        int,
	downcast:      int,
	clear:         int,
	refresh:       int,
	free:          int,
	unassign:      int,
	move_stmt:     int,
	add_stmt:      int,
	concatenate:   int,
	perform:       int,
	call_stmt:     int,
	submit:        int,
	message:       int,
	write:         int,
	if_stmt:       int,
	case_stmt:     int,
	while_stmt:    int,
	do_stmt:       int,
	loop_stmt:     int,
	at_stmt:       int,
	try_stmt:      int,
	class_decl:    int,
	interface_decl: int,
	method_decl:   int,
	form_decl:     int,
	function_decl: int,
	module_decl:   int,
	event_block:   int,
	enhancement:   int,
	test_seam:     int,
	test_injection: int,
	select_stmt:   int,
	open_cursor:   int,
	fetch_stmt:    int,
	close_cursor:  int,
	insert_stmt:   int,
	update_stmt:   int,
	delete_stmt:   int,
	read_table:    int,
	dataset_stmt:  int,
	report_stmt:   int,
	textpool_stmt: int,
}

count_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	counts := cast(^Node_Counts)v.data
	#partial switch _ in node.derived {
	case ^ast.Binary_Expr:
		counts.binary += 1
	case ^ast.Selector_Expr:
		counts.selector += 1
	case ^ast.Table_Expr:
		counts.table += 1
	case ^ast.Char_String_Template_Expr:
		counts.template += 1
	case ^ast.Template_Interpolation_Expr:
		counts.interpolation += 1
	case ^ast.Template_Format_Spec_Expr:
		counts.format_spec += 1
	case ^ast.Constructor_Expr:
		counts.constructor += 1
	case ^ast.Data_Decl:
		counts.data_decl += 1
	case ^ast.Data_Inline_Decl:
		counts.data_inline += 1
	case ^ast.Types_Decl:
		counts.types_decl += 1
	case ^ast.Constants_Decl:
		counts.constants += 1
	case ^ast.Field_Symbols_Decl:
		counts.field_symbols += 1
	case ^ast.Statics_Decl:
		counts.statics += 1
	case ^ast.Tables_Decl:
		counts.tables_decl += 1
	case ^ast.Ranges_Decl:
		counts.ranges += 1
	case ^ast.Parameters_Decl:
		counts.parameters += 1
	case ^ast.Select_Options_Decl:
		counts.select_options += 1
	case ^ast.Controls_Decl:
		counts.controls += 1
	case ^ast.Class_Data_Decl:
		counts.class_data += 1
	case ^ast.Assign_Stmt:
		counts.assign += 1
	case ^ast.Downcast_Assign_Stmt:
		counts.downcast += 1
	case ^ast.Clear_Stmt:
		counts.clear += 1
	case ^ast.Refresh_Stmt:
		counts.refresh += 1
	case ^ast.Free_Stmt:
		counts.free += 1
	case ^ast.Unassign_Stmt:
		counts.unassign += 1
	case ^ast.Move_Stmt:
		counts.move_stmt += 1
	case ^ast.Add_Stmt:
		counts.add_stmt += 1
	case ^ast.Concatenate_Stmt:
		counts.concatenate += 1
	case ^ast.Perform_Stmt:
		counts.perform += 1
	case ^ast.Call_Stmt:
		counts.call_stmt += 1
	case ^ast.Submit_Stmt:
		counts.submit += 1
	case ^ast.Message_Stmt:
		counts.message += 1
	case ^ast.Write_Stmt:
		counts.write += 1
	case ^ast.If_Stmt:
		counts.if_stmt += 1
	case ^ast.Case_Stmt:
		counts.case_stmt += 1
	case ^ast.While_Stmt:
		counts.while_stmt += 1
	case ^ast.Do_Stmt:
		counts.do_stmt += 1
	case ^ast.Loop_Stmt:
		counts.loop_stmt += 1
	case ^ast.At_Stmt:
		counts.at_stmt += 1
	case ^ast.Try_Stmt:
		counts.try_stmt += 1
	case ^ast.Class_Decl:
		counts.class_decl += 1
	case ^ast.Interface_Decl:
		counts.interface_decl += 1
	case ^ast.Method_Decl:
		counts.method_decl += 1
	case ^ast.Form_Decl:
		counts.form_decl += 1
	case ^ast.Function_Decl:
		counts.function_decl += 1
	case ^ast.Module_Decl:
		counts.module_decl += 1
	case ^ast.Event_Block_Stmt:
		counts.event_block += 1
	case ^ast.Enhancement_Stmt:
		counts.enhancement += 1
	case ^ast.Test_Seam_Stmt:
		counts.test_seam += 1
	case ^ast.Test_Injection_Stmt:
		counts.test_injection += 1
	case ^ast.Select_Stmt:
		counts.select_stmt += 1
	case ^ast.Open_Cursor_Stmt:
		counts.open_cursor += 1
	case ^ast.Fetch_Stmt:
		counts.fetch_stmt += 1
	case ^ast.Close_Cursor_Stmt:
		counts.close_cursor += 1
	case ^ast.Insert_Stmt:
		counts.insert_stmt += 1
	case ^ast.Update_Stmt:
		counts.update_stmt += 1
	case ^ast.Delete_Stmt:
		counts.delete_stmt += 1
	case ^ast.Read_Table_Stmt:
		counts.read_table += 1
	case ^ast.Dataset_Stmt:
		counts.dataset_stmt += 1
	case ^ast.Report_Stmt:
		counts.report_stmt += 1
	case ^ast.Textpool_Stmt:
		counts.textpool_stmt += 1
	}
	return v
}

count_nodes :: proc(root: ^ast.Node) -> Node_Counts {
	counts := Node_Counts{}
	visitor := ast.Visitor{visit = count_visit, data = rawptr(&counts)}
	ast.walk(&visitor, root)
	return counts
}

test_parser :: proc(source: string) -> Parser {
	alloc := runtime.heap_allocator()
	return init_parser(source, "test.abap", alloc)
}

@(test)
expect_token_mismatch_does_not_advance :: proc(t: ^testing.T) {
	p := test_parser("DATA lv.")

	tok := expect_token(&p, .Period)

	testing.expect_value(t, tok.kind, tokenizer.Token_Kind.Ident)
	testing.expect_value(t, p.index, 0)
	testing.expect(t, at_keyword(&p, "DATA"))
	testing.expect_value(t, len(p.errors), 1)
}

@(test)
expect_token_match_advances :: proc(t: ^testing.T) {
	p := test_parser(".")

	tok := expect_token(&p, .Period)

	testing.expect_value(t, p.index, 1)
	testing.expect_value(t, tok.kind, tokenizer.Token_Kind.Period)
	testing.expect_value(t, len(p.errors), 0)
}

@(test)
top_level_loop_makes_progress_on_unexpected_tokens :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse("@ @ .", "test.abap", alloc)

	testing.expect_value(t, len(parsed.root.stmts), 1)
	_, ok := parsed.root.stmts[0].derived_stmt.(^ast.Invalid_Stmt)
	testing.expect(t, ok)
	testing.expect(t, len(parsed.errors) > 0)
}

@(test)
missing_period_creates_local_diagnostic_and_valid_statement :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse("DATA lv", "test.abap", alloc)

	testing.expect_value(t, len(parsed.root.stmts), 1)
	decl, ok := parsed.root.stmts[0].derived_stmt.(^ast.Data_Decl)
	testing.expect(t, ok)
	testing.expect_value(t, decl.name, "lv")
	testing.expect_value(t, len(parsed.errors), 1)
}

@(test)
missing_period_does_not_swallow_following_simple_statement :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse(`DATA first
DATA second.`, "test.abap", alloc)

	testing.expect_value(t, len(parsed.root.stmts), 2)
	first, first_data := parsed.root.stmts[0].derived_stmt.(^ast.Data_Decl)
	second, second_data := parsed.root.stmts[1].derived_stmt.(^ast.Data_Decl)
	testing.expect(t, first_data)
	testing.expect(t, second_data)
	testing.expect_value(t, first.name, "first")
	testing.expect_value(t, second.name, "second")
	testing.expect_value(t, len(parsed.errors), 1)
}

@(test)
statement_list_stop_keywords_are_not_consumed :: proc(t: ^testing.T) {
	p := test_parser(`DATA lv.
ENDIF.`)
	stops := []string{"ENDIF"}

	stmts := parse_stmt_list_until(&p, stops)

	testing.expect_value(t, len(stmts), 1)
	testing.expect(t, at_keyword(&p, "ENDIF"))
}

@(test)
expression_precedence_keeps_multiply_inside_add :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse("lv = a + b * c.", "test.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	assign, ok := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	testing.expect(t, ok)
	add, add_ok := assign.rhs.derived_expr.(^ast.Binary_Expr)
	testing.expect(t, add_ok)
	testing.expect_value(t, add.op, ast.Binary_Op.Add)
	mul, mul_ok := add.right.derived_expr.(^ast.Binary_Expr)
	testing.expect(t, mul_ok)
	testing.expect_value(t, mul.op, ast.Binary_Op.Multiply)
}

@(test)
selector_chains_build_nested_selector_nodes :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse("lv = lo_obj->mo_child=>gc_value~part-name.", "test.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.selector, 4)
}

@(test)
table_expression_keeps_table_and_selector_shape :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse("lv = itab[ table_line = 'X' ].", "test.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	table, ok := assign.rhs.derived_expr.(^ast.Table_Expr)
	testing.expect(t, ok)
	testing.expect_value(t, len(table.selectors), 1)
	_, is_binary := table.selectors[0].derived_expr.(^ast.Binary_Expr)
	testing.expect(t, is_binary)
}

@(test)
missing_expression_failure_stays_local :: proc(t: ^testing.T) {
	p := test_parser(".")

	expr := parse_expr(&p)

	testing.expect(t, expr == nil)
	testing.expect_value(t, p.index, 0)
	testing.expect_value(t, current_token(&p).kind, tokenizer.Token_Kind.Period)
	testing.expect_value(t, len(p.errors), 1)
}

@(test)
logical_comparison_precedence_keeps_and_inside_or :: proc(t: ^testing.T) {
	p := test_parser("a = 1 OR b = 2 AND c = 3.")

	expr := parse_logical_expr(&p)

	testing.expect(t, expr != nil)
	outer, outer_ok := expr.derived_expr.(^ast.Binary_Expr)
	testing.expect(t, outer_ok)
	testing.expect_value(t, outer.op, ast.Binary_Op.Or)
	right, right_ok := outer.right.derived_expr.(^ast.Binary_Expr)
	testing.expect(t, right_ok)
	testing.expect_value(t, right.op, ast.Binary_Op.And)
	testing.expect_value(t, current_token(&p).kind, tokenizer.Token_Kind.Period)
}

@(test)
basic_string_template_interpolation :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse("rv = |{ lv_amount }|.", "test.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.template, 1)
	testing.expect_value(t, counts.interpolation, 1)
	testing.expect_value(t, counts.format_spec, 0)
}

@(test)
print_node_reconstructs_whole_file_from_ast :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `DATA lv TYPE i.
lv = 1.`
	parsed := parse(source, "roundtrip.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, ast.print_node(parsed.root, alloc), source)
}

@(test)
write_node_prints_individual_statement_without_source :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `DATA lv TYPE i.
lv = a + b.`
	parsed := parse(source, "stmt_source.abap", alloc)
	out := strings.builder_make(alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	ast.write_node(&out, parsed.root.stmts[1])
	testing.expect_value(t, strings.to_string(out), `lv = a + b.`)
}

@(test)
print_node_reconstructs_string_template_nodes :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `rv = |Amount { lv_amount DECIMALS = 2 }|.`
	parsed := parse(source, "template_source.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	template := assign.rhs.derived_expr.(^ast.Char_String_Template_Expr)
	interpolation := template.parts[1].derived_expr.(^ast.Template_Interpolation_Expr)
	testing.expect_value(t, ast.print_node(template, alloc), `|Amount { lv_amount DECIMALS = 2 }|`)
	testing.expect_value(t, ast.print_node(interpolation, alloc), `{ lv_amount DECIMALS = 2 }`)
}

@(test)
print_node_uses_formatting_options :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `IF a = 1. lv = 1. ENDIF.`
	parsed := parse(source, "format_options.abap", alloc)
	options := ast.Print_Options{newline = "\r\n", indent = "  "}

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, ast.print_node(parsed.root, alloc, options), "IF a = 1.\r\n  lv = 1.\r\nENDIF.")
}

@(test)
template_interpolation_extracts_decimals_and_width_specs :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse("rv = |Amount { lv_amount DECIMALS = 2 WIDTH = 12 }|.", "test.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.interpolation, 1)
	testing.expect_value(t, counts.format_spec, 2)
}

@(test)
template_interpolation_accepts_multiline_value_constructor_optional :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `rv = | { VALUE #( mt_trn[ bizttype = 60 ]-docnum
       OPTIONAL ) ALPHA = OUT } |.`
	parsed := parse(source, "test.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.interpolation, 1)
	testing.expect_value(t, counts.constructor, 1)
	testing.expect_value(t, counts.format_spec, 1)
}

@(test)
constructor_expr_keeps_kind_enum :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	parsed := parse("rv = VALUE #( 1 ).", "constructor_kind.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	constructor := assign.rhs.derived_expr.(^ast.Constructor_Expr)
	testing.expect_value(t, constructor.kind, ast.Constructor_Kind.Value)
}

@(test)
statement_batch_declarations :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `DATA lv TYPE i.
DATA(lv_inline) = 1.
TYPES ty_i TYPE i.
CONSTANTS c_i TYPE i VALUE 1.
FIELD-SYMBOLS <fs> TYPE any.
STATICS st TYPE i.
TABLES mara.
RANGES r_matnr FOR mara-matnr.
PARAMETERS p_count TYPE i DEFAULT 1.
SELECT-OPTIONS s_matnr FOR mara-matnr.
CONTROLS tc TYPE TABLEVIEW USING SCREEN 100.
CLASS-DATA gv TYPE i.`
	parsed := parse(source, "decls.abap", alloc)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.data_decl, 1)
	testing.expect_value(t, counts.data_inline, 1)
	testing.expect_value(t, counts.types_decl, 1)
	testing.expect_value(t, counts.constants, 1)
	testing.expect_value(t, counts.field_symbols, 1)
	testing.expect_value(t, counts.statics, 1)
	testing.expect_value(t, counts.tables_decl, 1)
	testing.expect_value(t, counts.ranges, 1)
	testing.expect_value(t, counts.parameters, 1)
	testing.expect_value(t, counts.select_options, 1)
	testing.expect_value(t, counts.controls, 1)
	testing.expect_value(t, counts.class_data, 1)
}

@(test)
declaration_nodes_keep_concrete_clause_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `TYPES ty_i TYPE i.
CONSTANTS c_i TYPE i VALUE 1.
RANGES r_matnr FOR mara-matnr.
PARAMETERS p_count TYPE i DEFAULT 1.
SELECT-OPTIONS s_matnr FOR mara-matnr.
CONTROLS tc TYPE TABLEVIEW USING SCREEN 100.
CLASS-DATA gv TYPE i VALUE 0.`
	parsed := parse(source, "decl_fields.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	constants := parsed.root.stmts[1].derived_stmt.(^ast.Constants_Decl)
	ranges := parsed.root.stmts[2].derived_stmt.(^ast.Ranges_Decl)
	parameters := parsed.root.stmts[3].derived_stmt.(^ast.Parameters_Decl)
	options := parsed.root.stmts[4].derived_stmt.(^ast.Select_Options_Decl)
	controls := parsed.root.stmts[5].derived_stmt.(^ast.Controls_Decl)
	class_data := parsed.root.stmts[6].derived_stmt.(^ast.Class_Data_Decl)

	testing.expect_value(t, len(types.types), 1)
	testing.expect_value(t, types.types[0].name, "ty_i")
	testing.expect(t, types.types[0].type_clause != nil)
	testing.expect_value(t, types.types[0].type_clause.form, ast.Data_Type_Form.Type)
	testing.expect_value(t, len(constants.constants), 1)
	testing.expect(t, constants.constants[0].value_clause != nil)
	testing.expect(t, ranges.ranges[0].for_clause != nil)
	testing.expect(t, parameters.parameters[0].default_clause != nil)
	testing.expect(t, options.options[0].for_clause != nil)
	testing.expect(t, controls.controls[0].using_screen != nil)
	testing.expect(t, class_data.decls[0].value_clause != nil)
}

@(test)
declaration_additions_keep_concrete_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `CONSTANTS lcv_max(14) TYPE p DECIMALS 7 VALUE '0.9999999'.
FIELD-SYMBOLS <line> LIKE LINE OF itab.
FIELD-SYMBOLS <lt_records> TYPE STANDARD TABLE.
PARAMETERS p_flag AS CHECKBOX DEFAULT 'X' MODIF ID md.
PARAMETERS p_mode RADIOBUTTON GROUP g01 USER-COMMAND upd LOWER CASE OBLIGATORY.
SELECT-OPTIONS s_matnr FOR mara-matnr NO-DISPLAY VISIBLE LENGTH 20 DEFAULT 'A' TO 'Z' OPTION BT SIGN I MATCHCODE OBJECT /sttp/h_loc_gln MEMORY ID gln MODIF ID grp.`
	parsed := parse(source, "decl_additions.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	constants := parsed.root.stmts[0].derived_stmt.(^ast.Constants_Decl)
	field_line := parsed.root.stmts[1].derived_stmt.(^ast.Field_Symbols_Decl)
	field_table := parsed.root.stmts[2].derived_stmt.(^ast.Field_Symbols_Decl)
	checkbox := parsed.root.stmts[3].derived_stmt.(^ast.Parameters_Decl)
	radio := parsed.root.stmts[4].derived_stmt.(^ast.Parameters_Decl)
	options := parsed.root.stmts[5].derived_stmt.(^ast.Select_Options_Decl)

	testing.expect(t, constants.constants[0].paren_length != nil)
	testing.expect_value(t, len(constants.constants[0].length_clauses), 1)
	testing.expect_value(t, constants.constants[0].length_clauses[0].kind, ast.Length_Clause_Kind.Decimals)
	testing.expect_value(t, field_line.field_symbols[0].type_clause.form, ast.Data_Type_Form.Like_Line_Of)
	testing.expect_value(t, field_table.field_symbols[0].type_clause.form, ast.Data_Type_Form.Standard_Table)
	testing.expect(t, .As_Checkbox in checkbox.parameters[0].flags)
	testing.expect(t, checkbox.parameters[0].default_clause != nil)
	testing.expect_value(t, checkbox.parameters[0].modif_id.id, "md")
	testing.expect_value(t, radio.parameters[0].radiobutton_group.group, "g01")
	testing.expect_value(t, radio.parameters[0].user_command.command, "upd")
	testing.expect(t, .Lower_Case in radio.parameters[0].flags)
	testing.expect(t, .Obligatory in radio.parameters[0].flags)
	testing.expect(t, .No_Display in options.options[0].flags)
	testing.expect(t, options.options[0].visible_length != nil)
	testing.expect(t, options.options[0].to_clause != nil)
	testing.expect_value(t, options.options[0].option_clause.option, "BT")
	testing.expect_value(t, options.options[0].sign_clause.sign, "I")
	testing.expect(t, options.options[0].matchcode_object != nil)
	testing.expect(t, options.options[0].memory_id != nil)
	testing.expect_value(t, options.options[0].modif_id.id, "grp")
}

@(test)
statement_batch_assignments_and_simple_statements :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `lv = 1.
lr ?= lo_ref.
CLEAR lv.
REFRESH lt_tab.
FREE lt_tab.
UNASSIGN <fs>.
MOVE a TO b.
ADD 1 TO lv.
CONCATENATE a b INTO c.
PERFORM frm.
CALL METHOD lo->run.
SUBMIT zrep.
MESSAGE 'x' TYPE 'I'.
WRITE lv.
lo->run( ).`
	parsed := parse(source, "simple.abap", alloc)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.assign, 1)
	testing.expect_value(t, counts.downcast, 1)
	testing.expect_value(t, counts.clear, 1)
	testing.expect_value(t, counts.refresh, 1)
	testing.expect_value(t, counts.free, 1)
	testing.expect_value(t, counts.unassign, 1)
	testing.expect_value(t, counts.move_stmt, 1)
	testing.expect_value(t, counts.add_stmt, 1)
	testing.expect_value(t, counts.concatenate, 1)
	testing.expect_value(t, counts.perform, 1)
	testing.expect_value(t, counts.call_stmt, 2)
	testing.expect_value(t, counts.submit, 1)
	testing.expect_value(t, counts.message, 1)
	testing.expect_value(t, counts.write, 1)
}

@(test)
simple_resource_and_arithmetic_statements_keep_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `CLEAR: lv_a WITH 'X', lv_b.
REFRESH TABLE lt_tab.
FREE MEMORY ID lv_id.
UNASSIGN <fs>.
MOVE src TO dst.
ADD 1 TO lv_sum GIVING lv_total.
SUBTRACT 1 FROM lv_sum.
MULTIPLY lv_sum BY factor.
DIVIDE lv_sum BY factor GIVING lv_div.
COMPUTE EXACT lv_sum = a + b.`
	parsed := parse(source, "simple_fields.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	clear := parsed.root.stmts[0].derived_stmt.(^ast.Clear_Stmt)
	refresh := parsed.root.stmts[1].derived_stmt.(^ast.Refresh_Stmt)
	free := parsed.root.stmts[2].derived_stmt.(^ast.Free_Stmt)
	unassign := parsed.root.stmts[3].derived_stmt.(^ast.Unassign_Stmt)
	move_stmt := parsed.root.stmts[4].derived_stmt.(^ast.Move_Stmt)
	add := parsed.root.stmts[5].derived_stmt.(^ast.Add_Stmt)
	subtract := parsed.root.stmts[6].derived_stmt.(^ast.Subtract_Stmt)
	multiply := parsed.root.stmts[7].derived_stmt.(^ast.Multiply_Stmt)
	divide := parsed.root.stmts[8].derived_stmt.(^ast.Divide_Stmt)
	compute := parsed.root.stmts[9].derived_stmt.(^ast.Compute_Stmt)

	testing.expect_value(t, len(clear.operands), 2)
	testing.expect_value(t, clear.operands[0].mode, ast.Clear_Mode.With_Value)
	testing.expect(t, clear.operands[0].value != nil)
	testing.expect(t, refresh.operands[0].table)
	testing.expect(t, free.memory)
	testing.expect(t, free.memory_id != nil)
	testing.expect_value(t, len(unassign.operands), 1)
	testing.expect(t, move_stmt.entries[0].source != nil)
	testing.expect(t, move_stmt.entries[0].target != nil)
	testing.expect(t, add.entries[0].result != nil)
	testing.expect(t, subtract.entries[0].target != nil)
	testing.expect(t, multiply.entries[0].source != nil)
	testing.expect_value(t, divide.entries[0].form, ast.Divide_Form.By)
	testing.expect(t, divide.entries[0].result != nil)
	testing.expect(t, compute.entries[0].exact)
	_, sum_ok := compute.entries[0].source.derived_expr.(^ast.Binary_Expr)
	testing.expect(t, sum_ok)
}

@(test)
simple_text_and_flow_statements_keep_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `CONCATENATE a b INTO c SEPARATED BY sep RESPECTING BLANKS.
SPLIT text AT sep INTO left right.
CONDENSE text NO-GAPS.
REPLACE FIRST OCCURRENCE OF 'a' IN text WITH 'b'.
TRANSLATE text TO UPPER CASE.
SHIFT text RIGHT BY 2 PLACES.
FIND FIRST OCCURRENCE OF 'a' IN text MATCH OFFSET off RESULTS res.
SEARCH text FOR pattern STARTING AT first ENDING AT last ABBREVIATED.
PERFORM frm IN PROGRAM prog USING arg CHANGING out IF FOUND.
CALL FUNCTION 'Z_FM'.
SUBMIT zrep WITH p = v AND RETURN.
MESSAGE '001' TYPE 'I' WITH a b INTO msg.
WRITE /10(5) text.`
	parsed := parse(source, "simple_text_flow.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	concat := parsed.root.stmts[0].derived_stmt.(^ast.Concatenate_Stmt)
	split := parsed.root.stmts[1].derived_stmt.(^ast.Split_Stmt)
	condense := parsed.root.stmts[2].derived_stmt.(^ast.Condense_Stmt)
	replace := parsed.root.stmts[3].derived_stmt.(^ast.Replace_Stmt)
	translate := parsed.root.stmts[4].derived_stmt.(^ast.Translate_Stmt)
	shift := parsed.root.stmts[5].derived_stmt.(^ast.Shift_Stmt)
	find := parsed.root.stmts[6].derived_stmt.(^ast.Find_Stmt)
	search := parsed.root.stmts[7].derived_stmt.(^ast.Search_Stmt)
	perform := parsed.root.stmts[8].derived_stmt.(^ast.Perform_Stmt)
	call_stmt := parsed.root.stmts[9].derived_stmt.(^ast.Call_Stmt)
	submit := parsed.root.stmts[10].derived_stmt.(^ast.Submit_Stmt)
	message := parsed.root.stmts[11].derived_stmt.(^ast.Message_Stmt)
	write := parsed.root.stmts[12].derived_stmt.(^ast.Write_Stmt)

	testing.expect_value(t, len(concat.entries[0].sources), 2)
	testing.expect(t, concat.entries[0].separator != nil)
	testing.expect(t, concat.entries[0].respecting_blanks)
	testing.expect_value(t, len(split.entries[0].targets), 2)
	testing.expect(t, condense.no_gaps)
	testing.expect_value(t, replace.occurrence, ast.Replace_Occurrence.First)
	testing.expect(t, replace.replacement != nil)
	testing.expect_value(t, translate.form, ast.Translate_Form.To_Upper)
	testing.expect_value(t, shift.direction, ast.Shift_Direction.Right)
	testing.expect(t, shift.places != nil)
	testing.expect_value(t, find.occurrence, ast.Find_Occurrence.First)
	testing.expect(t, find.match_offset != nil)
	testing.expect(t, find.results != nil)
	testing.expect(t, search.starting_at != nil)
	testing.expect(t, search.ending_at != nil)
	testing.expect(t, search.abbreviated)
	testing.expect(t, perform.program != nil)
	testing.expect_value(t, len(perform.using_args), 1)
	testing.expect_value(t, len(perform.changing), 1)
	testing.expect(t, perform.if_found)
	testing.expect_value(t, call_stmt.kind, ast.Call_Kind.Function)
	testing.expect(t, call_stmt.target != nil)
	testing.expect(t, submit.and_return)
	testing.expect_value(t, len(submit.options), 1)
	testing.expect_value(t, submit.options[0].operator, ast.Submit_Option_Operator.Assign)
	testing.expect(t, message.head.msg_type != nil)
	testing.expect_value(t, len(message.with_args), 2)
	testing.expect(t, message.into != nil)
	testing.expect(t, write.operands[0].line_break)
	testing.expect(t, write.operands[0].position != nil)
	testing.expect(t, write.operands[0].length != nil)
}

@(test)
statement_batch_control_blocks :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `IF a = 1. WRITE 'a'. ELSEIF a = 2. WRITE 'b'. ELSE. WRITE 'c'. ENDIF.
CASE a. WHEN 1 OR 2. WRITE 'n'. WHEN OTHERS. WRITE 'o'. ENDCASE.
WHILE a > 0. a = a - 1. ENDWHILE.
DO 3 TIMES. WRITE a. ENDDO.
LOOP AT itab INTO wa. AT FIRST. WRITE wa. ENDAT. ENDLOOP.
TRY. WRITE 'x'. CATCH cx_root INTO DATA(lo). WRITE 'y'. CLEANUP. WRITE 'z'. ENDTRY.`
	parsed := parse(source, "control.abap", alloc)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.if_stmt, 1)
	testing.expect_value(t, counts.case_stmt, 1)
	testing.expect_value(t, counts.while_stmt, 1)
	testing.expect_value(t, counts.do_stmt, 1)
	testing.expect_value(t, counts.loop_stmt, 1)
	testing.expect_value(t, counts.at_stmt, 1)
	testing.expect_value(t, counts.try_stmt, 1)

	if_stmt := parsed.root.stmts[0].derived_stmt.(^ast.If_Stmt)
	case_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Case_Stmt)
	try_stmt := parsed.root.stmts[5].derived_stmt.(^ast.Try_Stmt)
	testing.expect_value(t, len(if_stmt.elseif_clauses), 1)
	testing.expect(t, if_stmt.else_clause != nil)
	testing.expect_value(t, len(case_stmt.whens), 2)
	testing.expect_value(t, len(try_stmt.catches), 1)
	testing.expect(t, try_stmt.cleanup != nil)
}

@(test)
statement_batch_structural_blocks :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `CLASS lcl DEFINITION. ENDCLASS.
INTERFACE lif. ENDINTERFACE.
CLASS lcl IMPLEMENTATION. METHOD run. DATA lv TYPE i. ENDMETHOD. ENDCLASS.
FORM frm. WRITE 'f'. ENDFORM.
FUNCTION z_fm. WRITE 'x'. ENDFUNCTION.
MODULE pai INPUT. WRITE 'm'. ENDMODULE.
ENHANCEMENT enh. WRITE 'e'. ENDENHANCEMENT.
TEST-SEAM seam. WRITE 's'. END-TEST-SEAM.
TEST-INJECTION seam. WRITE 'i'. END-TEST-INJECTION.
START-OF-SELECTION. WRITE 'start'.`
	parsed := parse(source, "structural.abap", alloc)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.class_decl, 2)
	testing.expect_value(t, counts.interface_decl, 1)
	testing.expect_value(t, counts.method_decl, 1)
	testing.expect_value(t, counts.form_decl, 1)
	testing.expect_value(t, counts.function_decl, 1)
	testing.expect_value(t, counts.module_decl, 1)
	testing.expect_value(t, counts.enhancement, 1)
	testing.expect_value(t, counts.test_seam, 1)
	testing.expect_value(t, counts.test_injection, 1)
	testing.expect_value(t, counts.event_block, 1)
}

@(test)
statement_batch_open_sql_and_data_access :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `SELECT * FROM mara INTO wa. WRITE wa. ENDSELECT.
OPEN CURSOR cv FOR SELECT * FROM mara.
FETCH NEXT CURSOR cv INTO wa.
CLOSE CURSOR cv.
INSERT mara FROM wa.
UPDATE mara FROM wa.
DELETE FROM mara WHERE matnr = lv.
READ TABLE itab INTO wa INDEX 1.
OPEN DATASET file FOR INPUT IN TEXT MODE.
REPORT zrep.
READ REPORT prog INTO lt.
READ TEXTPOOL prog INTO lt LANGUAGE sy-langu.`
	parsed := parse(source, "data_access.abap", alloc)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.select_stmt, 1)
	testing.expect_value(t, counts.open_cursor, 1)
	testing.expect_value(t, counts.fetch_stmt, 1)
	testing.expect_value(t, counts.close_cursor, 1)
	testing.expect_value(t, counts.insert_stmt, 1)
	testing.expect_value(t, counts.update_stmt, 1)
	testing.expect_value(t, counts.delete_stmt, 1)
	testing.expect_value(t, counts.read_table, 1)
	testing.expect_value(t, counts.dataset_stmt, 1)
	testing.expect_value(t, counts.report_stmt, 2)
	testing.expect_value(t, counts.textpool_stmt, 1)
}

@(test)
data_access_statements_keep_concrete_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `READ TABLE itab INTO DATA(row) WITH KEY id = lv_id TRANSPORTING NO FIELDS.
INSERT wa INTO TABLE itab INDEX idx ASSIGNING FIELD-SYMBOL(<row>).
UPDATE mara SET matnr = lv_new WHERE matnr = lv_old.
DELETE ADJACENT DUPLICATES FROM itab COMPARING matnr.`
	parsed := parse(source, "data_access_fields.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	insert := parsed.root.stmts[1].derived_stmt.(^ast.Insert_Stmt)
	update := parsed.root.stmts[2].derived_stmt.(^ast.Update_Stmt)
	delete_stmt := parsed.root.stmts[3].derived_stmt.(^ast.Delete_Stmt)

	testing.expect_value(t, len(read.entries), 1)
	testing.expect(t, read.entries[0].table != nil)
	testing.expect(t, read.entries[0].into != nil)
	testing.expect_value(t, read.entries[0].key_kind, ast.Read_Table_Key_Kind.Key)
	testing.expect_value(t, len(read.entries[0].key_values), 1)
	testing.expect(t, read.entries[0].transporting_no_fields)
	testing.expect_value(t, insert.form, ast.Insert_Form.Internal_Table)
	testing.expect(t, insert.source != nil)
	testing.expect(t, insert.target != nil)
	testing.expect(t, insert.index != nil)
	testing.expect(t, insert.assigning != nil)
	testing.expect_value(t, len(update.assignments), 1)
	testing.expect(t, update.where_cond != nil)
	testing.expect_value(t, delete_stmt.form, ast.Delete_Form.Adjacent_Duplicates)
	testing.expect(t, delete_stmt.target != nil)
	testing.expect_value(t, len(delete_stmt.comparing), 1)
}

@(test)
cursor_dataset_report_and_textpool_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `SELECT SINGLE matnr FROM mara INTO DATA(lv_matnr) WHERE matnr = lv_key.
OPEN CURSOR WITH HOLD cv FOR SELECT matnr FROM mara WHERE matnr = lv_key.
FETCH NEXT CURSOR cv INTO TABLE lt_mara PACKAGE SIZE lv_size.
CLOSE CURSOR cv.
OPEN DATASET file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT AT POSITION pos MESSAGE msg.
READ DATASET file INTO text MAXIMUM LENGTH max ACTUAL LENGTH DATA(len).
TRANSFER text TO file LENGTH len.
REPORT zrep.
READ REPORT prog INTO source.
INSERT TEXTPOOL prog FROM pool LANGUAGE lang.`
	parsed := parse(source, "surface_fields.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	select_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	open_cursor := parsed.root.stmts[1].derived_stmt.(^ast.Open_Cursor_Stmt)
	fetch := parsed.root.stmts[2].derived_stmt.(^ast.Fetch_Stmt)
	close_cursor := parsed.root.stmts[3].derived_stmt.(^ast.Close_Cursor_Stmt)
	open_dataset := parsed.root.stmts[4].derived_stmt.(^ast.Dataset_Stmt)
	read_dataset := parsed.root.stmts[5].derived_stmt.(^ast.Dataset_Stmt)
	transfer := parsed.root.stmts[6].derived_stmt.(^ast.Dataset_Stmt)
	report := parsed.root.stmts[7].derived_stmt.(^ast.Report_Stmt)
	read_report := parsed.root.stmts[8].derived_stmt.(^ast.Report_Stmt)
	textpool := parsed.root.stmts[9].derived_stmt.(^ast.Textpool_Stmt)

	testing.expect(t, select_stmt.query.single)
	testing.expect(t, select_stmt.query.source != nil)
	testing.expect(t, select_stmt.query.result != nil)
	testing.expect(t, select_stmt.query.where_cond != nil)
	testing.expect(t, open_cursor.with_hold)
	testing.expect(t, open_cursor.handle != nil)
	testing.expect(t, open_cursor.query.source != nil)
	testing.expect(t, fetch.handle != nil)
	testing.expect(t, fetch.result != nil)
	testing.expect(t, fetch.result.table)
	testing.expect(t, fetch.package_size != nil)
	testing.expect(t, close_cursor.handle != nil)
	testing.expect_value(t, open_dataset.kind, ast.Dataset_Kind.Open)
	testing.expect_value(t, open_dataset.access, ast.Dataset_Open_Access.Output)
	testing.expect(t, open_dataset.text_mode)
	testing.expect_value(t, open_dataset.encoding, "DEFAULT")
	testing.expect(t, open_dataset.position != nil)
	testing.expect(t, open_dataset.message != nil)
	testing.expect_value(t, read_dataset.kind, ast.Dataset_Kind.Read)
	testing.expect(t, read_dataset.target != nil)
	testing.expect(t, read_dataset.maximum_length != nil)
	testing.expect(t, read_dataset.actual_length != nil)
	testing.expect_value(t, transfer.kind, ast.Dataset_Kind.Transfer)
	testing.expect(t, transfer.source != nil)
	testing.expect(t, transfer.length != nil)
	testing.expect_value(t, report.kind, ast.Report_Kind.Report)
	testing.expect(t, report.name != nil)
	testing.expect_value(t, read_report.kind, ast.Report_Kind.Read_Report)
	testing.expect(t, read_report.source != nil)
	testing.expect_value(t, textpool.kind, ast.Textpool_Kind.Insert)
	testing.expect(t, textpool.table != nil)
	testing.expect(t, textpool.language != nil)
}
