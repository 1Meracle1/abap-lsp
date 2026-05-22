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
FIND FIRST OCCURRENCE OF 'a' IN text MATCH OFFSET off RESULTS res.
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
GET REFERENCE OF ls_data INTO lr_data.`
	parsed := parse(source, "runtime_details.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.runtime_stmt, 8)
	get_parameter := parsed.root.stmts[0].derived_stmt.(^ast.Runtime_Stmt)
	set_parameter := parsed.root.stmts[1].derived_stmt.(^ast.Runtime_Stmt)
	cursor := parsed.root.stmts[2].derived_stmt.(^ast.Runtime_Stmt)
	pf_status := parsed.root.stmts[3].derived_stmt.(^ast.Runtime_Stmt)
	screen := parsed.root.stmts[4].derived_stmt.(^ast.Runtime_Stmt)
	user_command := parsed.root.stmts[5].derived_stmt.(^ast.Runtime_Stmt)
	update_task := parsed.root.stmts[6].derived_stmt.(^ast.Runtime_Stmt)
	reference := parsed.root.stmts[7].derived_stmt.(^ast.Runtime_Stmt)

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
oop_signature_parameters_are_concrete_ast_clauses :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_count TYPE i iv_text TYPE string
      RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.`
	parsed := parse(source, "oop_parameters.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl).body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	importing := methods.members[0].signatures[0]
	returning := methods.members[0].signatures[1]
	testing.expect_value(t, len(importing.parameters), 2)
	testing.expect_value(t, importing.parameters[0].name, "iv_count")
	testing.expect_value(t, importing.parameters[1].name, "iv_text")
	testing.expect(t, importing.parameters[1].type_clause != nil)
	testing.expect_value(t, returning.parameters[0].name, "rv_ok")
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
