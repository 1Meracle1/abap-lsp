package abap_frontend_parser

import "../ast"

import "core:testing"

@(test)
statement_batch_assignments_and_simple_statements :: proc(t: ^testing.T) {
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
	parsed := parse(source, "simple.abap", context.allocator)
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
keyword_like_complex_lhs_assignments_parse :: proc(t: ^testing.T) {
	source := `interface-unicode = 'X'.
method-alias = seox_true.
method-state = seoc_state_implemented.
method[ id = 1 ] = value.
method+1(2) = value.
method(2) = value.`
	parsed := parse(source, "keyword_selector_assignment.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.assign, 6)
	testing.expect_value(t, counts.invalid_stmt, 0)
}

@(test)
selection_screen_statements_are_not_macro_calls :: proc(t: ^testing.T) {
	source := `SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE sc_title.
SELECTION-SCREEN COMMENT 1(18) sc_url FOR FIELD p_url.
SELECTION-SCREEN END OF SCREEN 1002.`
	parsed := parse(source, "selection_screen.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	begin := parsed.root.stmts[0].derived_stmt.(^ast.Selection_Screen_Stmt)
	comment := parsed.root.stmts[1].derived_stmt.(^ast.Selection_Screen_Stmt)
	_, macro := parsed.root.stmts[1].derived_stmt.(^ast.Macro_Call_Stmt)

	testing.expect(t, !macro)
	testing.expect_value(t, begin.title_name, "sc_title")
	testing.expect_value(t, comment.comment_name, "sc_url")
	testing.expect_value(t, comment.field_name, "p_url")
}

@(test)
selection_screen_block_prints_without_information_loss :: proc(t: ^testing.T) {
	source := `SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE sc_title.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) sc_url FOR FIELD p_url.
PARAMETERS: p_url TYPE string LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) sc_user FOR FIELD p_user.
PARAMETERS: p_user TYPE string LOWER CASE VISIBLE LENGTH 60.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) sc_pass FOR FIELD p_pass.
PARAMETERS: p_pass TYPE c LENGTH 255 LOWER CASE VISIBLE LENGTH 60.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) sc_cmnt FOR FIELD p_cmnt.
PARAMETERS: p_cmnt TYPE c LENGTH 255 LOWER CASE VISIBLE LENGTH 60.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 1002.`
	parsed := parse(source, "selection_screen_roundtrip.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
call_selection_screen_clauses_are_not_target_refs :: proc(t: ^testing.T) {
	source := `CALL SELECTION-SCREEN c_dynnr
  STARTING AT ls_position-start_column ls_position-start_row
  ENDING AT ls_position-end_column ls_position-end_row.`
	parsed := parse(source, "call_selection_screen.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	target := stmt.target.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, stmt.kind, ast.Call_Kind.Selection_Screen)
	testing.expect_value(t, len(target.raw_refs), 1)
	testing.expect_value(t, target.raw_refs[0].name, "c_dynnr")
}

@(test)
missing_rhs_recovery_preserves_following_statement :: proc(t: ^testing.T) {
	assignment := parse("lv_bad = .\nlv_after = 1.", "missing_rhs.abap", context.allocator)
	inline := parse("DATA(lv_bad) = .\nDATA lv_after TYPE i.", "inline_rhs.abap", context.allocator)
	assignment_counts := count_nodes(assignment.root)
	inline_counts := count_nodes(inline.root)

	expect_error_contains(t, assignment, "expected assignment value after '='")
	testing.expect_value(t, assignment_counts.assign, 1)
	testing.expect(t, assignment_counts.invalid_stmt >= 1)

	expect_error_contains(t, inline, "expected expression after '=' in inline DATA declaration")
	testing.expect_value(t, inline_counts.data_decl, 1)
	testing.expect(t, inline_counts.invalid_stmt >= 1)
}

@(test)
method_call_missing_period_leaves_next_statement_token :: proc(t: ^testing.T) {
	parsed := parse(
		"lo_prog->add_statement( lo_item )\nDATA lv_after TYPE i.",
		"method_period.abap",
		context.allocator,
	)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "expected '.' after method call")
	testing.expect_value(t, counts.call_stmt, 0)
	testing.expect_value(t, counts.data_decl, 1)
	testing.expect(t, counts.invalid_stmt >= 1)
}

@(test)
simple_resource_and_arithmetic_statements_keep_fields :: proc(t: ^testing.T) {
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
	parsed := parse(source, "simple_fields.abap", context.allocator)

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
	source := `CONCATENATE a b INTO c SEPARATED BY sep RESPECTING BLANKS.
SPLIT text AT sep INTO left right.
CONDENSE text NO-GAPS.
REPLACE FIRST OCCURRENCE OF 'a' IN text WITH 'b'.
TRANSLATE text TO UPPER CASE.
SHIFT text RIGHT BY 2 PLACES.
FIND FIRST OCCURRENCE OF 'a' IN text MATCH OFFSET off MATCH COUNT cnt RESULTS res.
SEARCH text FOR pattern STARTING AT first ENDING AT last ABBREVIATED.
PERFORM frm IN PROGRAM prog USING arg CHANGING out IF FOUND.
CALL FUNCTION 'Z_FM'.
SUBMIT zrep WITH p = v AND RETURN.
MESSAGE '001' TYPE 'I' WITH a b INTO msg.
WRITE /10(5) text.`
	parsed := parse(source, "simple_text_flow.abap", context.allocator)

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
	testing.expect(t, find.match_count != nil)
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
string_statement_parser_facts_keep_modes :: proc(t: ^testing.T) {
	source := `CONCATENATE LINES OF lt_source INTO lv_text IN BYTE MODE.
FIND ALL OCCURRENCES OF 'A' IN lv_text RESULTS lv_result.`
	parsed := parse(source, "string_stmt_facts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	concat := parsed.root.stmts[0].derived_stmt.(^ast.Concatenate_Stmt)
	find := parsed.root.stmts[1].derived_stmt.(^ast.Find_Stmt)

	testing.expect(t, concat.byte_mode)
	testing.expect_value(t, len(concat.entries), 1)
	testing.expect(t, concat.entries[0].lines_of)
	testing.expect_value(t, find.occurrence, ast.Find_Occurrence.All)
	testing.expect(t, find.results != nil)
	testing.expect_value(t, ast.print_node(concat, context.allocator), "CONCATENATE LINES OF lt_source INTO lv_text IN BYTE MODE.")
}

@(test)
message_heads_keep_compact_class_fact :: proc(t: ^testing.T) {
	source := `MESSAGE e001(zmsg) WITH lv_text DISPLAY LIKE lv_like RAISING cx_msg.
MESSAGE ID zmsg TYPE lv_type NUMBER lv_no.
MESSAGE e001.`
	parsed := parse(source, "message_heads.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	compact := parsed.root.stmts[0].derived_stmt.(^ast.Message_Stmt)
	id_form := parsed.root.stmts[1].derived_stmt.(^ast.Message_Stmt)
	default_form := parsed.root.stmts[2].derived_stmt.(^ast.Message_Stmt)

	testing.expect(t, compact.head.has_compact_class)
	testing.expect_value(t, compact.head.compact_class_name, "zmsg")
	testing.expect_value(
		t,
		source[compact.head.compact_class_range.start:compact.head.compact_class_range.end],
		"zmsg",
	)
	testing.expect(t, compact.head.code != nil)
	testing.expect(t, compact.display_like != nil)
	testing.expect(t, compact.raising != nil)
	testing.expect(t, id_form.head.id != nil)
	testing.expect(t, id_form.head.msg_type != nil)
	testing.expect(t, id_form.head.number != nil)
	testing.expect(t, !id_form.head.has_compact_class)
	testing.expect(t, default_form.head.code != nil)
	testing.expect(t, !default_form.head.has_compact_class)
}

@(test)
simple_runtime_flow_and_macro_statements_keep_nodes :: proc(t: ^testing.T) {
	source := `ASSERT lo_ref IS BOUND.
CHECK lv_ok = abap_true.
RETURN.
CONTINUE.
EXIT.
STOP.
COMMIT WORK AND WAIT.
ROLLBACK WORK.
DESCRIBE TABLE lt_text LINES DATA(lv_lines).
GET RUN TIME FIELD DATA(runtime).
SET HANDLER lo_handler->on_event FOR lo_sender.
LOG-POINT ID zsub FIELDS lv_value.
GET BADI lo_badi.
RAISE EXCEPTION TYPE cx_demo.
RAISE EVENT changed EXPORTING value = lv_value.
AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '03'.
FIELD-GROUPS header.
INSERT DUMMY INTO header.
FIELD value MODULE mod.
OVERLAY text WITH mask.
PACK src TO dst.
UNPACK src TO dst.
WAIT UP TO 1 SECONDS.
SKIP.
ULINE.
NEW-LINE.
NEW-PAGE.
RESERVE 2 LINES.
BACK.
ASSIGN me->(name) TO <fs>.
CREATE OBJECT lo_ref EXPORTING iv_value = lv_value.
DEFINE set_field.
  &1 = &2.
END-OF-DEFINITION.
set_field lv_a lv_b.`
	parsed := parse(source, "simple_more.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.assert_stmt, 1)
	testing.expect_value(t, counts.check_stmt, 1)
	testing.expect_value(t, counts.flow_stmt, 4)
	testing.expect_value(t, counts.transaction_stmt, 2)
	testing.expect_value(t, counts.describe_stmt, 1)
	testing.expect_value(t, counts.runtime_stmt, 4)
	testing.expect_value(t, counts.raise_stmt, 2)
	testing.expect_value(t, counts.authority_check, 1)
	testing.expect_value(t, counts.field_groups, 1)
	testing.expect_value(t, counts.insert_dummy, 1)
	testing.expect_value(t, counts.field_stmt, 1)
	testing.expect_value(t, counts.text_transform, 4)
	testing.expect_value(t, counts.list_control, 6)
	testing.expect_value(t, counts.assign_field, 1)
	testing.expect_value(t, counts.create_object, 1)
	testing.expect_value(t, counts.macro_def, 1)
	testing.expect_value(t, counts.macro_call, 1)
}

@(test)
runtime_get_set_variants_keep_detailed_fields :: proc(t: ^testing.T) {
	source := `GET PARAMETER ID 'ABC' FIELD lv_value.
SET PARAMETER ID 'ABC' FIELD lv_value.
GET CURSOR FIELD lv_field LINE lv_line OFFSET lv_off VALUE lv_value.
SET PF-STATUS lv_status EXCLUDING lt_excluding.
SET SCREEN 100.
SET USER-COMMAND lv_ok.
SET UPDATE TASK LOCAL.
GET TIME STAMP FIELD lv_timestamp.
GET REFERENCE OF ls_data INTO lr_data.`
	parsed := parse(source, "runtime_details.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.runtime_stmt, 9)
	get_parameter := parsed.root.stmts[0].derived_stmt.(^ast.Runtime_Stmt)
	set_parameter := parsed.root.stmts[1].derived_stmt.(^ast.Runtime_Stmt)
	cursor := parsed.root.stmts[2].derived_stmt.(^ast.Runtime_Stmt)
	pf_status := parsed.root.stmts[3].derived_stmt.(^ast.Runtime_Stmt)
	screen := parsed.root.stmts[4].derived_stmt.(^ast.Runtime_Stmt)
	user_command := parsed.root.stmts[5].derived_stmt.(^ast.Runtime_Stmt)
	update_task := parsed.root.stmts[6].derived_stmt.(^ast.Runtime_Stmt)
	time_stamp := parsed.root.stmts[7].derived_stmt.(^ast.Runtime_Stmt)
	reference := parsed.root.stmts[8].derived_stmt.(^ast.Runtime_Stmt)

	testing.expect_value(t, get_parameter.subject, ast.Runtime_Subject.Parameter_ID_Field)
	testing.expect(t, get_parameter.id != nil)
	testing.expect(t, get_parameter.field != nil)
	testing.expect_value(t, set_parameter.subject, ast.Runtime_Subject.Parameter_ID_Field)
	testing.expect_value(t, cursor.subject, ast.Runtime_Subject.Cursor)
	testing.expect(t, cursor.field != nil)
	testing.expect(t, cursor.line != nil)
	testing.expect(t, cursor.offset != nil)
	testing.expect(t, cursor.value != nil)
	testing.expect_value(t, pf_status.subject, ast.Runtime_Subject.PF_Status)
	testing.expect(t, pf_status.target != nil)
	testing.expect_value(t, len(pf_status.excluding), 1)
	testing.expect_value(t, screen.subject, ast.Runtime_Subject.Screen)
	testing.expect_value(t, user_command.subject, ast.Runtime_Subject.User_Command)
	testing.expect_value(t, update_task.subject, ast.Runtime_Subject.Update_Task_Local)
	testing.expect_value(t, time_stamp.subject, ast.Runtime_Subject.Time_Stamp_Field)
	testing.expect(t, time_stamp.target != nil)
	testing.expect_value(t, ast.print_node(time_stamp, context.allocator), "GET TIME STAMP FIELD lv_timestamp.")
	testing.expect_value(t, reference.subject, ast.Runtime_Subject.Reference)
	testing.expect(t, reference.value != nil)
	testing.expect(t, reference.target != nil)
}

@(test)
oop_simple_member_statements_do_not_become_method_calls :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_demo.
    ALIASES set FOR if_demo~set.
    EVENTS changed EXPORTING VALUE(value) TYPE string.
    CLASS-EVENTS static_changed.
    METHODS run IMPORTING iv_value TYPE i.
    CLASS-METHODS create RETURNING VALUE(ro_obj) TYPE REF TO lcl.
ENDCLASS.`
	parsed := parse(source, "oop_simple.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.class_decl, 1)
	testing.expect_value(t, counts.oop_simple, 7)
	testing.expect_value(t, counts.invalid_stmt, 0)

	class_decl := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl)
	interfaces := class_decl.body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	aliases := class_decl.body[2].derived_stmt.(^ast.Oop_Simple_Stmt)
	events := class_decl.body[3].derived_stmt.(^ast.Oop_Simple_Stmt)
	methods := class_decl.body[5].derived_stmt.(^ast.Oop_Simple_Stmt)
	class_methods := class_decl.body[6].derived_stmt.(^ast.Oop_Simple_Stmt)
	testing.expect_value(t, interfaces.members[0].name, "if_demo")
	testing.expect_value(t, aliases.members[0].name, "set")
	testing.expect_value(t, aliases.members[0].signatures[0].kind, ast.Oop_Signature_Kind.For)
	alias_target := aliases.members[0].signatures[0].values[0].derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, alias_target.base_name, "if_demo")
	testing.expect_value(t, alias_target.path[0].name, "set")
	testing.expect_value(t, events.members[0].signatures[0].kind, ast.Oop_Signature_Kind.Exporting)
	testing.expect_value(t, events.members[0].signatures[0].parameters[0].name, "value")
	testing.expect_value(t, len(methods.members), 1)
	testing.expect_value(t, methods.members[0].name, "run")
	testing.expect_value(t, len(methods.members[0].signatures), 1)
	testing.expect_value(t, methods.members[0].signatures[0].kind, ast.Oop_Signature_Kind.Importing)
	testing.expect_value(t, methods.members[0].signatures[0].parameters[0].name, "iv_value")
	testing.expect_value(t, len(class_methods.members), 1)
	testing.expect_value(t, class_methods.members[0].name, "create")
	testing.expect_value(t, class_methods.members[0].signatures[0].kind, ast.Oop_Signature_Kind.Returning)
	testing.expect_value(t, class_methods.members[0].signatures[0].parameters[0].name, "ro_obj")
}

@(test)
oop_qualified_method_member_keeps_component_name :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS if_demo~run REDEFINITION.
ENDCLASS.`
	parsed := parse(source, "oop_qualified_method.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl).body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	testing.expect_value(t, len(methods.members), 1)
	testing.expect_value(t, methods.members[0].name, "if_demo~run")
	testing.expect(t, .Redefinition in methods.members[0].flags)
}

@(test)
oop_signature_parameters_are_concrete_ast_clauses :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING it_source TYPE STANDARD TABLE iv_state TYPE i OPTIONAL iv_date LIKE sy-datum iv_text TYPE string
      RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.`
	parsed := parse(source, "oop_parameters.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl).body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	importing := methods.members[0].signatures[0]
	returning := methods.members[0].signatures[1]
	testing.expect_value(t, len(importing.parameters), 4)
	testing.expect_value(t, importing.parameters[0].name, "it_source")
	testing.expect_value(t, importing.parameters[0].type_clause.form, ast.Data_Type_Form.Standard_Table)
	testing.expect(t, importing.parameters[0].type_clause.type_ref == nil)
	testing.expect_value(t, importing.parameters[1].name, "iv_state")
	testing.expect_value(t, importing.parameters[1].passing, ast.Parameter_Passing_Kind.Direct)
	testing.expect(t, importing.parameters[1].optional)
	testing.expect_value(t, importing.parameters[2].name, "iv_date")
	date_ref := importing.parameters[2].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, date_ref.base_name, "sy")
	testing.expect_value(t, date_ref.path[0].name, "datum")
	testing.expect_value(t, importing.parameters[3].name, "iv_text")
	testing.expect(t, importing.parameters[3].type_clause != nil)
	testing.expect_value(t, returning.parameters[0].name, "rv_ok")
	testing.expect_value(t, returning.parameters[0].passing, ast.Parameter_Passing_Kind.Value)
}

@(test)
oop_signature_accepts_escaped_keyword_parameters :: proc(t: ^testing.T) {
	source := `INTERFACE lif.
  METHODS set_option
    IMPORTING
      !OPTION TYPE I
      !VALUE TYPE ABAP_BOOL DEFAULT ABAP_TRUE.
ENDINTERFACE.`
	parsed := parse(source, "oop_escaped_keyword_parameters.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Interface_Decl).body[0].derived_stmt.(^ast.Oop_Simple_Stmt)
	importing := methods.members[0].signatures[0]
	testing.expect_value(t, len(importing.parameters), 2)
	testing.expect_value(t, importing.parameters[0].name, "OPTION")
	testing.expect_value(t, importing.parameters[0].passing, ast.Parameter_Passing_Kind.Direct)
	option_type := importing.parameters[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, option_type.base_name, "I")
	testing.expect_value(t, importing.parameters[1].name, "VALUE")
	testing.expect_value(t, importing.parameters[1].passing, ast.Parameter_Passing_Kind.Direct)
	value_type := importing.parameters[1].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, value_type.base_name, "ABAP_BOOL")
}

@(test)
oop_section_visibility_is_ast_field :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS pub.
  PROTECTED SECTION.
    METHODS prot.
  PRIVATE SECTION.
    METHODS priv.
ENDCLASS.
INTERFACE lif.
  PUBLIC SECTION.
    METHODS if_pub.
ENDINTERFACE.`
	parsed := parse(source, "oop_visibility.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	class_decl := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl)
	public := class_decl.body[0].derived_stmt.(^ast.Oop_Simple_Stmt)
	protected := class_decl.body[2].derived_stmt.(^ast.Oop_Simple_Stmt)
	private := class_decl.body[4].derived_stmt.(^ast.Oop_Simple_Stmt)
	iface := parsed.root.stmts[1].derived_stmt.(^ast.Interface_Decl)
	if_public := iface.body[0].derived_stmt.(^ast.Oop_Simple_Stmt)

	testing.expect_value(t, public.visibility, ast.Oop_Visibility.Public)
	testing.expect_value(t, protected.visibility, ast.Oop_Visibility.Protected)
	testing.expect_value(t, private.visibility, ast.Oop_Visibility.Private)
	testing.expect_value(t, if_public.visibility, ast.Oop_Visibility.Public)
}

@(test)
multiline_simple_blocks_consume_parameter_assignments :: proc(t: ^testing.T) {
	source := `CALL FUNCTION 'Z_FM'
  EXPORTING
    id = 'NA'
    value = lv_value.
CALL METHOD lo_plugin->('RUN')
  EXPORTING
    iv_value = lv_value.
RAISE EXCEPTION TYPE cx_demo
  EXPORTING
    textid = cx_demo=>id
    value = lv_value.
CREATE OBJECT lo_ref
  EXPORTING
    iv_value = lv_value.
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create IMPORTING iv_name TYPE string
        RETURNING VALUE(ro_obj) TYPE REF TO lcl,
      run IMPORTING iv_value TYPE i.
ENDCLASS.`
	parsed := parse(source, "simple_multiline.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.call_stmt, 2)
	testing.expect_value(t, counts.raise_stmt, 1)
	testing.expect_value(t, counts.create_object, 1)
	testing.expect_value(t, counts.oop_simple, 2)
}

@(test)
raw_call_statements_carry_argument_facts :: proc(t: ^testing.T) {
	source := `CALL FUNCTION 'Z_FM'
  EXPORTING iv_in = lv_in
  IMPORTING ev_out = DATA(lv_out)
  CHANGING cv_any = FIELD-SYMBOL(<fs_any>)
  TABLES ct_rows = lt_rows
  EXCEPTIONS failed = 1.
CALL METHOD lo->run
  EXPORTING iv_dyn = (lv_dynamic)
  RECEIVING rv_result = DATA(lv_result).`
	parsed := parse(source, "raw_call_sections.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	function := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	method := parsed.root.stmts[1].derived_stmt.(^ast.Call_Stmt)
	expected_function := [?]ast.Call_Arg_Section_Kind {
		.Exporting,
		.Importing,
		.Changing,
		.Tables,
		.Exceptions,
	}

	testing.expect_value(t, len(function.arg_sections), len(expected_function))
	testing.expect_value(t, len(function.named_args), len(expected_function))
	for i in 0 ..< len(expected_function) {
		testing.expect_value(t, function.arg_sections[i].kind, expected_function[i])
		testing.expect_value(t, function.named_args[i].section, expected_function[i])
		testing.expect(t, function.named_args[i].has_section)
	}
	testing.expect_value(t, len(method.arg_sections), 2)
	testing.expect_value(t, len(method.named_args), 2)
	testing.expect_value(t, method.arg_sections[0].kind, ast.Call_Arg_Section_Kind.Exporting)
	testing.expect_value(t, method.arg_sections[1].kind, ast.Call_Arg_Section_Kind.Receiving)
	testing.expect_value(t, method.named_args[0].section, ast.Call_Arg_Section_Kind.Exporting)
	testing.expect_value(t, method.named_args[1].section, ast.Call_Arg_Section_Kind.Receiving)
	testing.expect_value(t, source[function.named_args[1].value_range.start:function.named_args[1].value_range.end], "DATA(lv_out)")
	testing.expect_value(t, len(function.named_args[0].raw_refs), 1)
	testing.expect_value(t, function.named_args[0].raw_refs[0].name, "lv_in")
	testing.expect_value(t, len(function.named_args[1].raw_decls), 1)
	testing.expect_value(t, function.named_args[1].raw_decls[0].name, "lv_out")
	testing.expect_value(t, len(function.named_args[2].raw_decls), 1)
	testing.expect_value(t, function.named_args[2].raw_decls[0].kind, ast.Raw_Operand_Inline_Decl_Kind.Field_Symbol)
	testing.expect_value(t, function.named_args[2].raw_decls[0].name, "<fs_any>")
	testing.expect_value(t, len(function.named_args[3].raw_refs), 1)
	testing.expect_value(t, function.named_args[3].raw_refs[0].name, "lt_rows")
	testing.expect_value(t, len(function.named_args[4].raw_refs), 0)
	testing.expect_value(t, len(method.named_args[0].raw_refs), 0)
	testing.expect_value(t, len(method.named_args[1].raw_decls), 1)
	testing.expect_value(t, method.named_args[1].raw_decls[0].name, "lv_result")
}

@(test)
raw_call_method_targets_carry_parser_reference_facts :: proc(t: ^testing.T) {
	source := `CALL METHOD lo_client->run EXPORTING iv_value = lv_value.
CALL METHOD lcl_demo=>class_run.
CALL METHOD lo_client->('RUN') EXPORTING iv_value = lv_value.`
	parsed := parse(source, "raw_call_method_targets.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	instance := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	static := parsed.root.stmts[1].derived_stmt.(^ast.Call_Stmt)
	dynamic_call := parsed.root.stmts[2].derived_stmt.(^ast.Call_Stmt)
	instance_target := instance.target.derived_expr.(^ast.Type_Ref_Expr)
	static_target := static.target.derived_expr.(^ast.Type_Ref_Expr)
	dynamic_target := dynamic_call.target.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect(t, instance_target.raw_operand)
	testing.expect_value(t, instance_target.raw_refs[0].name, "lo_client")
	testing.expect_value(t, instance_target.raw_refs[0].path[0].name, "run")
	testing.expect(t, static_target.raw_refs[0].type_base)
	testing.expect_value(t, static_target.raw_refs[0].name, "lcl_demo")
	testing.expect_value(t, static_target.raw_refs[0].path[0].name, "class_run")
	testing.expect_value(t, len(dynamic_target.raw_refs), 1)
	testing.expect_value(t, dynamic_target.raw_refs[0].name, "lo_client")
	testing.expect_value(t, len(dynamic_target.raw_refs[0].path), 0)
}

@(test)
call_transformation_id_carries_modeled_args :: proc(t: ^testing.T) {
	source := `CALL TRANSFORMATION id
  OPTIONS initial_components = 'suppress'
  SOURCE (lt_stab)
  RESULT XML li_doc.`
	parsed := parse(source, "call_transformation_id.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	target := stmt.target.derived_expr.(^ast.Type_Ref_Expr)
	source_value := stmt.transformation_args[1].value.derived_expr.(^ast.Type_Ref_Expr)
	result_value := stmt.transformation_args[2].value.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, stmt.kind, ast.Call_Kind.Transformation)
	testing.expect_value(t, target.name, "id")
	testing.expect(t, !target.raw_operand)
	testing.expect_value(t, len(stmt.transformation_args), 3)
	testing.expect_value(t, stmt.transformation_args[0].kind, ast.Call_Transformation_Arg_Kind.Options)
	testing.expect_value(t, stmt.transformation_args[0].name, "initial_components")
	testing.expect(t, stmt.transformation_args[0].has_eq)
	testing.expect_value(t, stmt.transformation_args[1].kind, ast.Call_Transformation_Arg_Kind.Source)
	testing.expect_value(t, source_value.raw_refs[0].name, "lt_stab")
	testing.expect_value(t, stmt.transformation_args[2].kind, ast.Call_Transformation_Arg_Kind.Result)
	testing.expect_value(t, stmt.transformation_args[2].name, "XML")
	testing.expect_value(t, result_value.raw_refs[0].name, "li_doc")
}

@(test)
call_transaction_carries_parser_operand_facts :: proc(t: ^testing.T) {
	source := `CALL TRANSACTION tcode WITH AUTHORITY-CHECK USING bdc_tab MODE mode UPDATE upd MESSAGES INTO msg_tab.
CALL TRANSACTION tcode WITHOUT AUTHORITY-CHECK USING bdc_tab OPTIONS FROM opt MESSAGES INTO msg_tab.`
	parsed := parse(source, "call_transaction_facts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	first := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	second := parsed.root.stmts[1].derived_stmt.(^ast.Call_Stmt)
	first_target := first.target.derived_expr.(^ast.Type_Ref_Expr)
	second_target := second.target.derived_expr.(^ast.Type_Ref_Expr)
	expected_first := [?]string{"bdc_tab", "mode", "upd", "msg_tab"}
	expected_second := [?]string{"bdc_tab", "opt", "msg_tab"}

	testing.expect_value(t, first.kind, ast.Call_Kind.Transaction)
	testing.expect_value(t, second.kind, ast.Call_Kind.Transaction)
	testing.expect_value(t, first_target.raw_refs[0].name, "tcode")
	testing.expect_value(t, second_target.raw_refs[0].name, "tcode")
	testing.expect_value(t, len(first.transaction_operands), len(expected_first))
	testing.expect_value(t, len(second.transaction_operands), len(expected_second))
	for value, i in expected_first {
		operand := first.transaction_operands[i].derived_expr.(^ast.Type_Ref_Expr)
		testing.expect_value(t, operand.raw_refs[0].name, value)
	}
	for value, i in expected_second {
		operand := second.transaction_operands[i].derived_expr.(^ast.Type_Ref_Expr)
		testing.expect_value(t, operand.raw_refs[0].name, value)
	}
}

@(test)
direct_call_statement_keeps_parser_modeled_arguments :: proc(t: ^testing.T) {
	source := `run( EXPORTING iv_value = lv_value ).`
	parsed := parse(source, "direct_call_args.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	call := stmt.call.derived_expr.(^ast.Call_Expr)
	args := call.args.derived_expr.(^ast.Call_Arg_List_Expr)
	section := args.args[0].derived_expr.(^ast.Call_Arg_Section_Expr)

	testing.expect_value(t, stmt.kind, ast.Call_Kind.Direct)
	testing.expect_value(t, len(stmt.named_args), 0)
	testing.expect_value(t, section.kind, ast.Call_Arg_Section_Kind.Exporting)
}

@(test)
raw_simple_operands_carry_parser_reference_facts :: proc(t: ^testing.T) {
	source := `RAISE EVENT changed EXPORTING value = ls_row-field other = DATA(lv_raw).
ASSIGN COMPONENT lv_name OF STRUCTURE ls_row TO FIELD-SYMBOL(<fs_raw>).
DATA lv_typed TYPE sy-datum.`
	parsed := parse(source, "raw_operand_facts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	raise := parsed.root.stmts[0].derived_stmt.(^ast.Raise_Stmt)
	raise_target := raise.target.derived_expr.(^ast.Type_Ref_Expr)
	raise_args := raise.operands[0].derived_expr.(^ast.Type_Ref_Expr)
	assign := parsed.root.stmts[1].derived_stmt.(^ast.Assign_Field_Stmt)
	assign_args := assign.operands[0].derived_expr.(^ast.Type_Ref_Expr)
	decl := parsed.root.stmts[2].derived_stmt.(^ast.Data_Decl)
	type_ref := decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect(t, raise_target.raw_operand)
	testing.expect_value(t, len(raise_target.raw_refs), 1)
	testing.expect_value(t, raise_target.raw_refs[0].name, "changed")
	testing.expect_value(t, len(raise_args.raw_decls), 1)
	testing.expect_value(t, raise_args.raw_decls[0].kind, ast.Raw_Operand_Inline_Decl_Kind.Data)
	testing.expect_value(t, raise_args.raw_decls[0].name, "lv_raw")
	testing.expect_value(t, len(raise_args.raw_refs), 1)
	testing.expect_value(t, raise_args.raw_refs[0].name, "ls_row")
	testing.expect_value(t, raise_args.raw_refs[0].path[0].name, "field")
	testing.expect_value(t, len(assign_args.raw_decls), 1)
	testing.expect_value(t, assign_args.raw_decls[0].kind, ast.Raw_Operand_Inline_Decl_Kind.Field_Symbol)
	testing.expect_value(t, assign_args.raw_decls[0].name, "<fs_raw>")
	testing.expect_value(t, len(assign_args.raw_refs), 2)
	testing.expect_value(t, assign_args.raw_refs[0].name, "lv_name")
	testing.expect_value(t, assign_args.raw_refs[1].name, "ls_row")
	testing.expect(t, !type_ref.raw_operand)
	testing.expect_value(t, len(type_ref.raw_refs), 0)
}

@(test)
authority_check_object_keeps_id_fields :: proc(t: ^testing.T) {
	source := `AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.`
	parsed := parse(source, "authority_check.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Authority_Check_Stmt)
	testing.expect(t, stmt.object != nil)
	testing.expect_value(t, len(stmt.ids), 1)
	testing.expect(t, stmt.ids[0].id != nil)
	testing.expect(t, stmt.ids[0].field != nil)
	printed := ast.print_node(parsed.root, context.allocator)
	testing.expect_value(t, printed, "AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.")
}
