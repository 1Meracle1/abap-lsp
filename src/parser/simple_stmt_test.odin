package abap_frontend_parser

import "src:ast"

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
assignment_with_trailing_pragma_keeps_lhs_rhs_shape :: proc(t: ^testing.T) {
	parsed := parse(`sy-tcode = 'SE41' ##WRITE_OK.`, "assignment_pragma.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 1)
	assign, ok := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	testing.expect(t, ok)
	lhs, lhs_ok := assign.lhs.derived_expr.(^ast.Selector_Expr)
	testing.expect(t, lhs_ok)
	base := lhs.base.derived_expr.(^ast.Ident_Expr)
	field := lhs.field.derived_expr.(^ast.Ident_Expr)
	rhs := assign.rhs.derived_expr.(^ast.Literal_Expr)
	testing.expect_value(t, base.name, "sy")
	testing.expect_value(t, lhs.op, ast.Selector_Op.Dash)
	testing.expect_value(t, field.name, "tcode")
	testing.expect_value(t, rhs.value, "'SE41'")
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
	testing.expect_value(t, begin.kind, ast.Selection_Screen_Kind.Begin_Screen)
	testing.expect_value(t, begin.screen.text, "1002")
	testing.expect_value(t, begin.title.text, "sc_title")
	testing.expect_value(t, begin.title_name.text, "sc_title")
	testing.expect_value(t, begin.raw_text, "")
	testing.expect_value(t, comment.kind, ast.Selection_Screen_Kind.Comment)
	testing.expect_value(t, comment.position.text, "1")
	testing.expect_value(t, comment.length.text, "18")
	testing.expect_value(t, comment.comment_name.text, "sc_url")
	testing.expect_value(t, comment.field_name.text, "p_url")
	testing.expect_value(t, comment.raw_text, "")
}

@(test)
selection_screen_known_forms_are_structured :: proc(t: ^testing.T) {
	source := `SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE sc_title.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT /1(18) sc_url FOR FIELD p_url MODIF ID mod.
SELECTION-SCREEN PUSHBUTTON /20(10) pb_text USER-COMMAND run MODIF ID md2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 1002.`
	parsed := parse(source, "selection_screen_structured.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	begin_screen := parsed.root.stmts[0].derived_stmt.(^ast.Selection_Screen_Stmt)
	skip := parsed.root.stmts[1].derived_stmt.(^ast.Selection_Screen_Stmt)
	begin_line := parsed.root.stmts[2].derived_stmt.(^ast.Selection_Screen_Stmt)
	comment := parsed.root.stmts[3].derived_stmt.(^ast.Selection_Screen_Stmt)
	pushbutton := parsed.root.stmts[4].derived_stmt.(^ast.Selection_Screen_Stmt)
	end_line := parsed.root.stmts[5].derived_stmt.(^ast.Selection_Screen_Stmt)
	begin_block := parsed.root.stmts[6].derived_stmt.(^ast.Selection_Screen_Stmt)
	end_block := parsed.root.stmts[7].derived_stmt.(^ast.Selection_Screen_Stmt)
	end_screen := parsed.root.stmts[8].derived_stmt.(^ast.Selection_Screen_Stmt)

	testing.expect_value(t, begin_screen.kind, ast.Selection_Screen_Kind.Begin_Screen)
	testing.expect_value(t, begin_screen.screen.text, "1002")
	testing.expect_value(t, begin_screen.title.text, "sc_title")
	testing.expect_value(t, skip.kind, ast.Selection_Screen_Kind.Skip)
	testing.expect_value(t, skip.skip_lines.text, "2")
	testing.expect_value(t, begin_line.kind, ast.Selection_Screen_Kind.Begin_Line)
	testing.expect_value(t, comment.kind, ast.Selection_Screen_Kind.Comment)
	testing.expect(t, comment.line_break)
	testing.expect_value(t, comment.position.text, "1")
	testing.expect_value(t, comment.length.text, "18")
	testing.expect_value(t, comment.comment_name.text, "sc_url")
	testing.expect_value(t, comment.field_name.text, "p_url")
	testing.expect_value(t, comment.modif_id.text, "mod")
	testing.expect_value(t, pushbutton.kind, ast.Selection_Screen_Kind.Pushbutton)
	testing.expect(t, pushbutton.line_break)
	testing.expect_value(t, pushbutton.position.text, "20")
	testing.expect_value(t, pushbutton.length.text, "10")
	testing.expect_value(t, pushbutton.pushbutton_name.text, "pb_text")
	testing.expect_value(t, pushbutton.user_command.text, "run")
	testing.expect_value(t, pushbutton.modif_id.text, "md2")
	testing.expect_value(t, end_line.kind, ast.Selection_Screen_Kind.End_Line)
	testing.expect_value(t, begin_block.kind, ast.Selection_Screen_Kind.Begin_Block)
	testing.expect_value(t, begin_block.block_name.text, "b1")
	testing.expect(t, begin_block.with_frame)
	testing.expect_value(t, begin_block.title.text, "text-001")
	testing.expect_value(t, begin_block.title_name.text, "text")
	testing.expect_value(t, end_block.kind, ast.Selection_Screen_Kind.End_Block)
	testing.expect_value(t, end_block.block_name.text, "b1")
	testing.expect_value(t, end_screen.kind, ast.Selection_Screen_Kind.End_Screen)
	testing.expect_value(t, end_screen.screen.text, "1002")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
selection_screen_comment_names_are_limited_to_eight_characters :: proc(t: ^testing.T) {
	source := `SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE gv_sel_title.
SELECTION-SCREEN COMMENT 1(18) gv_comment.`
	parsed := parse(source, "selection_screen_name_length.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 2)
	expect_error_contains(t, parsed, "comment name can be up to eight characters long")
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"gv_sel_title",
	)
	testing.expect_value(
		t,
		source[parsed.errors[1].range.start:parsed.errors[1].range.end],
		"gv_comment",
	)
}

@(test)
selection_screen_addition_names_have_sap_length_limits :: proc(t: ^testing.T) {
	source := `REPORT zlen_selection.

SELECTION-SCREEN BEGIN OF BLOCK abcdefghijabcdefghija WITH FRAME TITLE abcdefghi.
PARAMETERS p_abcdefg TYPE c RADIOBUTTON GROUP abcde
  USER-COMMAND 123456789012345678901
  MODIF ID abcd
  MEMORY ID abcdefghijabcdefghija.
SELECT-OPTIONS so_abcdef FOR sy-datum MODIF ID abcd MEMORY ID abcdefghijabcdefghija.
SELECTION-SCREEN COMMENT /1(20) abcdefghi FOR FIELD p_abcdefg MODIF ID abcd.
SELECTION-SCREEN PUSHBUTTON /1(10) abcdefghi USER-COMMAND 123456789012345678901 MODIF ID abcd.
SELECTION-SCREEN END OF BLOCK abcdefghijabcdefghija.`
	parsed := parse(source, "selection_screen_addition_name_length.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 18)
	expect_error_contains(t, parsed, "block name can be up to 20 characters long")
	expect_error_contains(t, parsed, "frame title name can be up to eight characters long")
	expect_error_contains(t, parsed, "parameter name can be up to eight characters long")
	expect_error_contains(t, parsed, "radio button group name can be up to four characters long")
	expect_error_contains(t, parsed, "user command can be up to 20 characters long")
	expect_error_contains(t, parsed, "modification id can be up to three characters long")
	expect_error_contains(t, parsed, "memory id can be up to 20 characters long")
	expect_error_contains(t, parsed, "RADIOBUTTON GROUP and MEMORY ID cannot be used together")
	expect_error_contains(t, parsed, "select-option name can be up to eight characters long")
	expect_error_contains(t, parsed, "comment name can be up to eight characters long")
	expect_error_contains(t, parsed, "pushbutton name can be up to eight characters long")
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

Selection_Screen_Walk_Counts :: struct {
	statements:  int,
	comments:    int,
	pushbuttons: int,
	begin_blocks: int,
}

selection_screen_walk_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	counts := cast(^Selection_Screen_Walk_Counts)v.data
	#partial switch n in node.derived {
	case ^ast.Selection_Screen_Stmt:
		counts.statements += 1
		#partial switch n.kind {
		case .Comment:
			counts.comments += 1
		case .Pushbutton:
			counts.pushbuttons += 1
		case .Begin_Block:
			counts.begin_blocks += 1
		}
	}
	return v
}

@(test)
cloned_selection_screen_structured_strings_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `SELECTION-SCREEN COMMENT /1(18) sc_url FOR FIELD p_url MODIF ID mod.
SELECTION-SCREEN PUSHBUTTON /20(10) pb_text USER-COMMAND run MODIF ID md2.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.`
	root := clone_parse_after_source_overwrite(t, source)

	comment := root.stmts[0].derived_stmt.(^ast.Selection_Screen_Stmt)
	pushbutton := root.stmts[1].derived_stmt.(^ast.Selection_Screen_Stmt)
	block := root.stmts[2].derived_stmt.(^ast.Selection_Screen_Stmt)

	testing.expect_value(t, comment.comment_name.text, "sc_url")
	testing.expect_value(t, comment.field_name.text, "p_url")
	testing.expect_value(t, comment.modif_id.text, "mod")
	testing.expect_value(t, pushbutton.pushbutton_name.text, "pb_text")
	testing.expect_value(t, pushbutton.user_command.text, "run")
	testing.expect_value(t, pushbutton.modif_id.text, "md2")
	testing.expect_value(t, block.block_name.text, "b1")
	testing.expect_value(t, block.title.text, "text-001")
	testing.expect_value(t, block.title_name.text, "text")
	testing.expect_value(t, ast.print_node(root, context.allocator), source)
}

@(test)
walk_visits_selection_screen_statements :: proc(t: ^testing.T) {
	source := `SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN COMMENT 1(18) sc_url FOR FIELD p_url.
SELECTION-SCREEN PUSHBUTTON 20(10) pb_text USER-COMMAND run.
SELECTION-SCREEN END OF BLOCK b1.`
	parsed := parse(source, "selection_screen_walk.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := Selection_Screen_Walk_Counts{}
	visitor := ast.Visitor{visit = selection_screen_walk_visit, data = rawptr(&counts)}
	ast.walk(&visitor, parsed.root)

	testing.expect_value(t, counts.statements, 4)
	testing.expect_value(t, counts.comments, 1)
	testing.expect_value(t, counts.pushbuttons, 1)
	testing.expect_value(t, counts.begin_blocks, 1)
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
	testing.expect_value(t, target.raw_refs[0].name.text, "c_dynnr")
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
clear_chain_recovery_keeps_operand_after_missing_comma :: proc(t: ^testing.T) {
	source := `CLEAR:
  lv_var1
  lv_var2.`
	parsed := parse(source, "clear_missing_comma.abap", context.allocator)

	expect_error_contains(t, parsed, "expected ',' between CLEAR operands")
	testing.expect_value(t, len(parsed.root.stmts), 1)

	clear := parsed.root.stmts[0].derived_stmt.(^ast.Clear_Stmt)
	testing.expect_value(t, len(clear.operands), 2)
	first := clear.operands[0].target.derived_expr.(^ast.Ident_Expr)
	second := clear.operands[1].target.derived_expr.(^ast.Ident_Expr)
	testing.expect_value(t, first.name, "lv_var1")
	testing.expect_value(t, second.name, "lv_var2")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), "CLEAR: lv_var1, lv_var2.")
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
split_accepts_multiline_targets :: proc(t: ^testing.T) {
	source := `SPLIT lv_sgln
AT ':'
INTO DATA(lv_part_1)
     DATA(lv_part_2)
     DATA(lv_part_3)
IN CHARACTER MODE.
SPLIT lv_corrected_epc AT 'urn:epc:id:sgtin:' INTO TABLE
     DATA(lt_parts).`
	parsed := parse(source, "split_multiline.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	first := parsed.root.stmts[0].derived_stmt.(^ast.Split_Stmt)
	second := parsed.root.stmts[1].derived_stmt.(^ast.Split_Stmt)
	testing.expect_value(t, len(first.entries[0].targets), 3)
	testing.expect(t, second.entries[0].into_table)
	testing.expect_value(t, len(second.entries[0].targets), 1)
}

@(test)
move_corresponding_carries_statement_form :: proc(t: ^testing.T) {
	parsed := parse("MOVE-CORRESPONDING src TO dst.", "move_corresponding.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Move_Corresponding_Stmt)
	testing.expect_value(t, len(stmt.entries), 1)
	testing.expect_value(t, ast.print_node(stmt, context.allocator), "MOVE-CORRESPONDING src TO dst.")
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
	testing.expect(t, perform.has_program_clause)
	testing.expect_value(t, perform.form_kind, ast.Perform_Form_Kind.Static)
	testing.expect_value(t, perform.program_kind, ast.Perform_Program_Kind.Static)
	testing.expect_value(t, len(perform.using_args), 1)
	testing.expect_value(t, len(perform.changing), 1)
	testing.expect(t, perform.if_found)
	testing.expect_value(t, call_stmt.kind, ast.Call_Kind.Function)
	testing.expect(t, call_stmt.target != nil)
	testing.expect(t, .And_Return in submit.flags)
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
submit_statement_target_shape_is_modeled_and_validated :: proc(t: ^testing.T) {
	parsed := parse(
		`SUBMIT scpr3 AND RETURN.
SUBMIT (lv_report) VIA SELECTION-SCREEN.`,
		"submit_shape.abap",
		context.allocator,
	)

	testing.expect_value(t, len(parsed.errors), 0)
	static_submit := parsed.root.stmts[0].derived_stmt.(^ast.Submit_Stmt)
	dynamic_submit := parsed.root.stmts[1].derived_stmt.(^ast.Submit_Stmt)
	testing.expect_value(t, static_submit.target_kind, ast.Submit_Target_Kind.Static)
	testing.expect_value(t, dynamic_submit.target_kind, ast.Submit_Target_Kind.Dynamic)
	testing.expect(t, .And_Return in static_submit.flags)
	testing.expect(t, .Via_Selection_Screen in dynamic_submit.flags)

	invalid := parse("SUBMIT 1 + 2.", "bad_submit_shape.abap", context.allocator)
	testing.expect(t, len(invalid.errors) > 0)
}

@(test)
perform_statement_shape_is_modeled_and_validated :: proc(t: ^testing.T) {
	source := `PERFORM (lv_form) IN PROGRAM ('RDDU0001') IF FOUND.
PERFORM local_form IN PROGRAM.`
	parsed := parse(source, "perform_shape.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	dynamic_perform := parsed.root.stmts[0].derived_stmt.(^ast.Perform_Stmt)
	omitted := parsed.root.stmts[1].derived_stmt.(^ast.Perform_Stmt)
	testing.expect_value(t, dynamic_perform.form_kind, ast.Perform_Form_Kind.Dynamic)
	testing.expect_value(t, dynamic_perform.program_kind, ast.Perform_Program_Kind.Dynamic)
	testing.expect(t, dynamic_perform.has_program_clause)
	testing.expect(t, dynamic_perform.if_found)
	testing.expect_value(t, omitted.program_kind, ast.Perform_Program_Kind.Omitted)
	testing.expect(t, omitted.has_program_clause)
	testing.expect_value(t, ast.print_node(omitted, context.allocator), "PERFORM local_form IN PROGRAM.")
}

@(test)
perform_in_program_requires_program_before_additions :: proc(t: ^testing.T) {
	parsed := parse("PERFORM local_form IN PROGRAM USING value.", "perform_bad_program.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
}

@(test)
replace_section_keeps_bounds_and_target :: proc(t: ^testing.T) {
	source := `REPLACE SECTION OFFSET 0 LENGTH 1 OF lv_value WITH space.`
	parsed := parse(source, "replace_section.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	replace := parsed.root.stmts[0].derived_stmt.(^ast.Replace_Stmt)
	target := replace.target.derived_expr.(^ast.Ident_Expr)
	length := replace.section_length.derived_expr.(^ast.Literal_Expr)

	testing.expect(t, replace.pattern == nil)
	testing.expect(t, replace.section_offset != nil)
	testing.expect_value(t, length.value, "1")
	testing.expect_value(t, target.name, "lv_value")
	testing.expect_value(t, ast.print_node(replace, context.allocator), source)
}

@(test)
replace_section_requires_of :: proc(t: ^testing.T) {
	parsed := parse(`REPLACE SECTION OFFSET 0 lv_value WITH space.`, "replace_bad_section.abap", context.allocator)

	expect_error_contains(t, parsed, "expected OF after REPLACE SECTION")
}

@(test)
write_to_operands_keep_targets :: proc(t: ^testing.T) {
	source := `WRITE: lv_date TO lv_date_string,
       lv_time TO lv_time_string.`
	parsed := parse(source, "write_to.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	write := parsed.root.stmts[0].derived_stmt.(^ast.Write_To_Stmt)
	testing.expect_value(t, len(write.entries), 2)
	testing.expect(t, write.entries[0].source != nil)
	testing.expect(t, write.entries[0].target != nil)
	testing.expect(t, write.entries[1].source != nil)
	testing.expect(t, write.entries[1].target != nil)
	first_target := write.entries[0].target.derived_expr.(^ast.Ident_Expr)
	second_target := write.entries[1].target.derived_expr.(^ast.Ident_Expr)
	testing.expect_value(t, first_target.name, "lv_date_string")
	testing.expect_value(t, second_target.name, "lv_time_string")

	single := parse(`WRITE lv_date TO lv_date_string.`, "write_to_print.abap", context.allocator)
	testing.expect_value(t, ast.print_node(single.root.stmts[0], context.allocator), "WRITE lv_date TO lv_date_string.")
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
find_in_section_keeps_bounds_and_target :: proc(t: ^testing.T) {
	source := `FIND FIRST OCCURRENCE OF lc_null IN SECTION OFFSET lv_cursor LENGTH lv_len OF iv_data MATCH OFFSET lv_match.`
	parsed := parse(source, "find_in_section.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	find := parsed.root.stmts[0].derived_stmt.(^ast.Find_Stmt)
	target := find.target.derived_expr.(^ast.Ident_Expr)
	offset := find.section_offset.derived_expr.(^ast.Ident_Expr)
	length := find.section_length.derived_expr.(^ast.Ident_Expr)

	testing.expect_value(t, target.name, "iv_data")
	testing.expect_value(t, offset.name, "lv_cursor")
	testing.expect_value(t, length.name, "lv_len")
	testing.expect(t, find.match_offset != nil)
	testing.expect_value(t, ast.print_node(find, context.allocator), source)
}

@(test)
find_in_section_requires_of :: proc(t: ^testing.T) {
	parsed := parse(`FIND 'x' IN SECTION OFFSET off text.`, "find_bad_section.abap", context.allocator)

	expect_error_contains(t, parsed, "expected OF after FIND IN SECTION")
}

@(test)
find_in_table_keeps_target_after_table_keyword :: proc(t: ^testing.T) {
	source := `FIND REGEX 'CLASS\s+(.*)\s+DEFINITION' IN TABLE ct_source SUBMATCHES cv_clsname IGNORING CASE ##REGEX_POSIX.`
	parsed := parse(source, "find_in_table.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	find := parsed.root.stmts[0].derived_stmt.(^ast.Find_Stmt)
	target := find.target.derived_expr.(^ast.Ident_Expr)

	testing.expect(t, find.in_table)
	testing.expect_value(t, target.name, "ct_source")
	testing.expect_value(t, len(find.submatches), 1)
}

@(test)
find_in_table_match_line_keeps_line_target :: proc(t: ^testing.T) {
	source := `FIND REGEX 'x' IN TABLE ct_source MATCH LINE lv_tabix.`
	parsed := parse(source, "find_match_line.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	find := parsed.root.stmts[0].derived_stmt.(^ast.Find_Stmt)
	line := find.match_line.derived_expr.(^ast.Ident_Expr)

	testing.expect(t, find.in_table)
	testing.expect(t, find.match_offset == nil)
	testing.expect_value(t, line.name, "lv_tabix")
	testing.expect_value(t, ast.print_node(find, context.allocator), source)
}

@(test)
find_match_requires_known_addition :: proc(t: ^testing.T) {
	parsed := parse(`FIND 'x' IN text MATCH WORD lv_word.`, "find_bad_match.abap", context.allocator)

	expect_error_contains(t, parsed, "expected OFFSET, LENGTH, LINE, or COUNT after FIND MATCH")
}

@(test)
find_match_line_requires_table :: proc(t: ^testing.T) {
	parsed := parse(`FIND 'x' IN text MATCH LINE lv_line.`, "find_bad_match_line.abap", context.allocator)

	expect_error_contains(t, parsed, "MATCH LINE requires FIND IN TABLE")
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
	testing.expect_value(t, compact.head.compact_class_name.text, "zmsg")
	testing.expect_value(
		t,
		source[compact.head.compact_class_name.range.start:compact.head.compact_class_name.range.end],
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
LEAVE LIST-PROCESSING.
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
	testing.expect_value(t, counts.flow_stmt, 5)
	testing.expect_value(t, counts.transaction_stmt, 2)
	testing.expect_value(t, counts.describe_stmt, 1)
	testing.expect_value(t, counts.runtime_stmt, 3)
	testing.expect_value(t, counts.set_handler, 1)
	testing.expect_value(t, counts.raise_stmt, 2)
	testing.expect_value(t, counts.authority_check, 1)
	testing.expect_value(t, counts.field_groups, 1)
	testing.expect_value(t, counts.insert_dummy, 1)
	testing.expect_value(t, counts.field_stmt, 1)
	testing.expect_value(t, counts.text_transform, 3)
	testing.expect_value(t, counts.wait_stmt, 1)
	testing.expect_value(t, counts.list_control, 6)
	testing.expect_value(t, counts.assign_field, 1)
	testing.expect_value(t, counts.create_object, 1)
	testing.expect_value(t, counts.macro_def, 1)
	testing.expect_value(t, counts.macro_call, 1)
}

@(test)
describe_statements_keep_subjects_and_result_targets :: proc(t: ^testing.T) {
	source := `DESCRIBE TABLE itab LINES lv_lines.
DESCRIBE FIELD lv_value LENGTH lv_length IN CHARACTER MODE.
DESCRIBE FIELD lv_value TYPE lv_type.
DESCRIBE TABLE itab LINES DATA(lv_inline).`
	parsed := parse(source, "describe_targets.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 4)

	table_lines := parsed.root.stmts[0].derived_stmt.(^ast.Describe_Stmt)
	testing.expect_value(t, len(table_lines.entries), 1)
	table_lines_entry := table_lines.entries[0]
	table_lines_source := table_lines_entry.source.derived_expr.(^ast.Ident_Expr)
	table_lines_target := table_lines_entry.target.derived_expr.(^ast.Ident_Expr)
	testing.expect(t, table_lines_entry.table)
	testing.expect(t, !table_lines_entry.field)
	testing.expect_value(t, table_lines_entry.target_kind, ast.Describe_Target_Kind.Lines)
	testing.expect_value(t, table_lines_source.name, "itab")
	testing.expect_value(t, table_lines_target.name, "lv_lines")

	field_length := parsed.root.stmts[1].derived_stmt.(^ast.Describe_Stmt)
	field_length_entry := field_length.entries[0]
	field_length_source := field_length_entry.source.derived_expr.(^ast.Ident_Expr)
	field_length_target := field_length_entry.target.derived_expr.(^ast.Ident_Expr)
	testing.expect(t, !field_length_entry.table)
	testing.expect(t, field_length_entry.field)
	testing.expect_value(t, field_length_entry.target_kind, ast.Describe_Target_Kind.Length)
	testing.expect_value(t, field_length_source.name, "lv_value")
	testing.expect_value(t, field_length_target.name, "lv_length")

	field_type := parsed.root.stmts[2].derived_stmt.(^ast.Describe_Stmt)
	field_type_entry := field_type.entries[0]
	field_type_source := field_type_entry.source.derived_expr.(^ast.Ident_Expr)
	field_type_target := field_type_entry.target.derived_expr.(^ast.Ident_Expr)
	testing.expect(t, !field_type_entry.table)
	testing.expect(t, field_type_entry.field)
	testing.expect_value(t, field_type_entry.target_kind, ast.Describe_Target_Kind.Type)
	testing.expect_value(t, field_type_source.name, "lv_value")
	testing.expect_value(t, field_type_target.name, "lv_type")

	inline_lines := parsed.root.stmts[3].derived_stmt.(^ast.Describe_Stmt)
	inline_lines_entry := inline_lines.entries[0]
	inline_target := inline_lines_entry.target.derived_expr.(^ast.Data_Inline_Name_Expr)
	testing.expect(t, inline_lines_entry.table)
	testing.expect_value(t, inline_lines_entry.target_kind, ast.Describe_Target_Kind.Lines)
	testing.expect_value(t, inline_target.name.text, "lv_inline")
}

@(test)
macro_statements_keep_detailed_body_and_call_args :: proc(t: ^testing.T) {
	source := `DEFINE set_field.
  &1 = &2.
  WRITE &1.
END-OF-DEFINITION.
set_field lv_a 'B'.`
	parsed := parse(source, "macro_stmt.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.macro_def, 1)
	testing.expect_value(t, counts.macro_call, 1)
	testing.expect_value(t, counts.macro_arg_ref, 3)

	macro := parsed.root.stmts[0].derived_stmt.(^ast.Macro_Def_Stmt)
	testing.expect_value(t, macro.name.text, "set_field")
	testing.expect_value(t, source[macro.name.range.start:macro.name.range.end], "set_field")
	testing.expect_value(t, len(macro.body), 2)

	assign := macro.body[0].derived_stmt.(^ast.Assign_Stmt)
	lhs := assign.lhs.derived_expr.(^ast.Macro_Arg_Ref_Expr)
	rhs := assign.rhs.derived_expr.(^ast.Macro_Arg_Ref_Expr)
	testing.expect_value(t, lhs.name.text, "&1")
	testing.expect_value(t, lhs.slot, 1)
	testing.expect_value(t, rhs.name.text, "&2")
	testing.expect_value(t, rhs.slot, 2)

	write := macro.body[1].derived_stmt.(^ast.Write_Stmt)
	write_arg := write.operands[0].value.derived_expr.(^ast.Macro_Arg_Ref_Expr)
	testing.expect_value(t, write_arg.slot, 1)

	call := parsed.root.stmts[1].derived_stmt.(^ast.Macro_Call_Stmt)
	testing.expect_value(t, call.name.text, "set_field")
	testing.expect_value(t, len(call.args), 2)
	testing.expect_value(t, call.args[0].derived_expr.(^ast.Ident_Expr).name, "lv_a")
	testing.expect_value(t, call.args[1].derived_expr.(^ast.Literal_Expr).value, "'B'")
}

@(test)
wait_stmt_keeps_condition_and_duration :: proc(t: ^testing.T) {
	source := `WAIT UP TO 1 SECONDS.
WAIT UNTIL mv_free <> lv_free UP TO 120 SECONDS.`
	parsed := parse(source, "wait_stmt.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.wait_stmt, 2)
	wait_duration := parsed.root.stmts[0].derived_stmt.(^ast.Wait_Stmt)
	wait_until := parsed.root.stmts[1].derived_stmt.(^ast.Wait_Stmt)
	testing.expect(t, wait_duration.condition == nil)
	testing.expect(t, wait_duration.duration != nil)
	testing.expect(t, wait_until.condition != nil)
	testing.expect(t, wait_until.duration != nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
convert_time_stamp_uses_dedicated_stmt :: proc(t: ^testing.T) {
	source := `CONVERT TIME STAMP iv_ts TIME ZONE lc_utc INTO DATE lv_date TIME lv_time.
CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_ts TIME ZONE lc_utc.`
	parsed := parse(source, "convert_time_stamp.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.convert_time_stamp, 2)
	testing.expect_value(t, counts.text_transform, 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Convert_Time_Stamp_Stmt)
	inverse := parsed.root.stmts[1].derived_stmt.(^ast.Convert_Time_Stamp_Stmt)
	testing.expect_value(t, stmt.kind, ast.Convert_Time_Stamp_Kind.Time_Stamp_To_Date_Time)
	testing.expect_value(t, inverse.kind, ast.Convert_Time_Stamp_Kind.Date_Time_To_Time_Stamp)
	testing.expect(t, stmt.time_stamp != nil)
	testing.expect(t, stmt.time_zone != nil)
	testing.expect(t, stmt.date != nil)
	testing.expect(t, stmt.time != nil)
	testing.expect(t, inverse.time_stamp != nil)
	testing.expect(t, inverse.time_zone != nil)
	testing.expect(t, inverse.date != nil)
	testing.expect(t, inverse.time != nil)
	testing.expect(t, stmt.daylight_saving_time == nil)
	testing.expect(t, inverse.daylight_saving_time == nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
convert_time_stamp_accepts_common_optional_forms :: proc(t: ^testing.T) {
	source := `CONVERT DATE lv_date INTO TIME STAMP lv_ts TIME ZONE lc_utc.
CONVERT DATE lv_date TIME lv_time DAYLIGHT SAVING TIME lv_dst INTO TIME STAMP DATA(lv_ts) TIME ZONE lc_utc.
CONVERT TIME STAMP lv_ts TIME ZONE lc_utc INTO DATE DATA(lv_date).
CONVERT TIME STAMP lv_ts TIME ZONE lc_utc INTO TIME DATA(lv_time).
CONVERT TIME STAMP lv_ts TIME ZONE lc_utc INTO DATE lv_date TIME lv_time DAYLIGHT SAVING TIME DATA(lv_dst).`
	parsed := parse(source, "convert_time_stamp_optional.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.convert_time_stamp, 5)
	testing.expect_value(t, counts.text_transform, 0)

	date_only := parsed.root.stmts[0].derived_stmt.(^ast.Convert_Time_Stamp_Stmt)
	date_with_dst := parsed.root.stmts[1].derived_stmt.(^ast.Convert_Time_Stamp_Stmt)
	date_target_only := parsed.root.stmts[2].derived_stmt.(^ast.Convert_Time_Stamp_Stmt)
	time_target_only := parsed.root.stmts[3].derived_stmt.(^ast.Convert_Time_Stamp_Stmt)
	all_targets := parsed.root.stmts[4].derived_stmt.(^ast.Convert_Time_Stamp_Stmt)

	testing.expect(t, date_only.date != nil)
	testing.expect(t, date_only.time == nil)
	testing.expect(t, date_only.daylight_saving_time == nil)
	testing.expect(t, date_with_dst.time != nil)
	testing.expect(t, date_with_dst.daylight_saving_time != nil)
	testing.expect(t, date_with_dst.time_stamp != nil)
	testing.expect(t, date_target_only.date != nil)
	testing.expect(t, date_target_only.time == nil)
	testing.expect(t, time_target_only.date == nil)
	testing.expect(t, time_target_only.time != nil)
	testing.expect(t, all_targets.date != nil)
	testing.expect(t, all_targets.time != nil)
	testing.expect(t, all_targets.daylight_saving_time != nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
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
GET LOCALE LANGUAGE lv_language COUNTRY lv_country MODIFIER lv_modifier.
SET LOCALE LANGUAGE lv_language.
GET REFERENCE OF ls_data INTO lr_data.`
	parsed := parse(source, "runtime_details.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.runtime_stmt, 9)
	testing.expect_value(t, counts.locale_stmt, 2)
	get_parameter := parsed.root.stmts[0].derived_stmt.(^ast.Runtime_Stmt)
	set_parameter := parsed.root.stmts[1].derived_stmt.(^ast.Runtime_Stmt)
	cursor := parsed.root.stmts[2].derived_stmt.(^ast.Runtime_Stmt)
	pf_status := parsed.root.stmts[3].derived_stmt.(^ast.Runtime_Stmt)
	screen := parsed.root.stmts[4].derived_stmt.(^ast.Runtime_Stmt)
	user_command := parsed.root.stmts[5].derived_stmt.(^ast.Runtime_Stmt)
	update_task := parsed.root.stmts[6].derived_stmt.(^ast.Runtime_Stmt)
	time_stamp := parsed.root.stmts[7].derived_stmt.(^ast.Runtime_Stmt)
	locale := parsed.root.stmts[8].derived_stmt.(^ast.Locale_Stmt)
	set_locale := parsed.root.stmts[9].derived_stmt.(^ast.Locale_Stmt)
	reference := parsed.root.stmts[10].derived_stmt.(^ast.Runtime_Stmt)

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
	testing.expect_value(t, locale.kind, ast.Locale_Kind.Get)
	testing.expect(t, locale.language != nil)
	testing.expect(t, locale.country != nil)
	testing.expect(t, locale.modifier != nil)
	testing.expect_value(t, ast.print_node(locale, context.allocator), "GET LOCALE LANGUAGE lv_language COUNTRY lv_country MODIFIER lv_modifier.")
	testing.expect_value(t, set_locale.kind, ast.Locale_Kind.Set)
	testing.expect(t, set_locale.language != nil)
	testing.expect_value(t, ast.print_node(set_locale, context.allocator), "SET LOCALE LANGUAGE lv_language.")
	testing.expect_value(t, reference.subject, ast.Runtime_Subject.Reference)
	testing.expect(t, reference.value != nil)
	testing.expect(t, reference.target != nil)
}

@(test)
set_handler_stmt_keeps_sender_and_activation :: proc(t: ^testing.T) {
	source := `SET HANDLER on_event FOR mi_html_viewer ACTIVATION space.
SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.`
	parsed := parse(source, "set_handler.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.runtime_stmt, 0)
	testing.expect_value(t, counts.set_handler, 2)
	first := parsed.root.stmts[0].derived_stmt.(^ast.Set_Handler_Stmt)
	second := parsed.root.stmts[1].derived_stmt.(^ast.Set_Handler_Stmt)

	testing.expect_value(t, len(first.handlers), 1)
	testing.expect(t, first.sender != nil)
	testing.expect(t, first.activation != nil)
	testing.expect(t, !first.all_instances)
	testing.expect_value(t, ast.print_node(first, context.allocator), "SET HANDLER on_event FOR mi_html_viewer ACTIVATION space.")
	testing.expect_value(t, len(second.handlers), 1)
	testing.expect(t, second.sender == nil)
	testing.expect(t, second.activation != nil)
	testing.expect(t, second.all_instances)
	testing.expect_value(t, ast.print_node(second, context.allocator), "SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.")
}

@(test)
get_set_bit_use_dedicated_stmt :: proc(t: ^testing.T) {
	source := `GET BIT 1 OF lv_x INTO DATA(lv_bit).
SET BIT lv_pos OF lv_x TO lv_bit.`
	parsed := parse(source, "bit_stmt.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.bit_stmt, 2)
	testing.expect_value(t, counts.runtime_stmt, 0)
	get_bit := parsed.root.stmts[0].derived_stmt.(^ast.Bit_Stmt)
	set_bit := parsed.root.stmts[1].derived_stmt.(^ast.Bit_Stmt)
	testing.expect_value(t, get_bit.kind, ast.Bit_Kind.Get)
	testing.expect(t, get_bit.position != nil)
	testing.expect(t, get_bit.source != nil)
	testing.expect(t, get_bit.target != nil)
	testing.expect_value(t, set_bit.kind, ast.Bit_Kind.Set)
	testing.expect(t, set_bit.position != nil)
	testing.expect(t, set_bit.target != nil)
	testing.expect(t, set_bit.value != nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
set_cursor_uses_dedicated_stmt :: proc(t: ^testing.T) {
	source := `SET CURSOR FIELD 'P_PASS'.
SET CURSOR FIELD l_dynpro_field-screenname OFFSET lv_off.
SET CURSOR 2 ls-cline.`
	parsed := parse(source, "set_cursor.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.set_cursor, 3)
	field := parsed.root.stmts[0].derived_stmt.(^ast.Set_Cursor_Stmt)
	field_with_offset := parsed.root.stmts[1].derived_stmt.(^ast.Set_Cursor_Stmt)
	position := parsed.root.stmts[2].derived_stmt.(^ast.Set_Cursor_Stmt)

	testing.expect(t, field.field != nil)
	testing.expect(t, field_with_offset.field != nil)
	testing.expect(t, field_with_offset.offset != nil)
	testing.expect(t, position.line != nil)
	testing.expect(t, position.column != nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
receive_results_from_function_keeps_target_and_arguments :: proc(t: ^testing.T) {
	source := `RECEIVE RESULTS FROM FUNCTION 'Z_DEMO'
  IMPORTING ev_value = DATA(lv_value)
  TABLES et_rows = lt_rows
  EXCEPTIONS failed = 1.`
	parsed := parse(source, "receive_results.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 1)

	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.receive_results, 1)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Receive_Results_Stmt)
	testing.expect(t, stmt.target != nil)
	testing.expect_value(t, len(stmt.arg_sections), 3)
	testing.expect_value(t, len(stmt.named_args), 3)
	testing.expect_value(t, stmt.named_args[0].name.text, "ev_value")
	testing.expect_value(t, stmt.named_args[1].name.text, "et_rows")
	testing.expect_value(t, stmt.named_args[2].name.text, "failed")
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
	testing.expect_value(t, interfaces.members[0].name.text, "if_demo")
	testing.expect_value(t, aliases.members[0].name.text, "set")
	testing.expect_value(t, len(aliases.aliases), 1)
	testing.expect_value(t, aliases.aliases[0].name.text, "set")
	testing.expect_value(t, aliases.aliases[0].target_interface_name.text, "if_demo")
	testing.expect_value(t, aliases.aliases[0].target_member_name.text, "set")
	testing.expect_value(t, aliases.members[0].signatures[0].kind, ast.Oop_Signature_Kind.For)
	alias_target := aliases.members[0].signatures[0].values[0].derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, alias_target.base_name.text, "if_demo")
	testing.expect_value(t, alias_target.path[0].name.text, "set")
	testing.expect_value(t, events.members[0].signatures[0].kind, ast.Oop_Signature_Kind.Exporting)
	testing.expect_value(t, events.members[0].signatures[0].parameters[0].name.text, "value")
	testing.expect_value(t, len(methods.members), 1)
	testing.expect_value(t, methods.members[0].name.text, "run")
	testing.expect_value(t, len(methods.members[0].signatures), 1)
	testing.expect_value(t, methods.members[0].signatures[0].kind, ast.Oop_Signature_Kind.Importing)
	testing.expect_value(t, methods.members[0].signatures[0].parameters[0].name.text, "iv_value")
	testing.expect_value(t, len(class_methods.members), 1)
	testing.expect_value(t, class_methods.members[0].name.text, "create")
	testing.expect_value(t, class_methods.members[0].signatures[0].kind, ast.Oop_Signature_Kind.Returning)
	testing.expect_value(t, class_methods.members[0].signatures[0].parameters[0].name.text, "ro_obj")
}

@(test)
oop_event_handler_keeps_for_event_shape :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS on_changed FOR EVENT changed OF lcl_source IMPORTING ev_object.
ENDCLASS.`
	parsed := parse(source, "oop_event_handler.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	class_decl := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl)
	methods := class_decl.body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	handler := methods.members[0].event_handler

	testing.expect_value(t, handler.event_name.text, "changed")
	testing.expect(t, handler.source_type != nil)
	source_ref := handler.source_type.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, source_ref.base_name.text, "lcl_source")
	testing.expect_value(t, len(methods.members[0].signatures), 1)
	testing.expect_value(t, methods.members[0].signatures[0].kind, ast.Oop_Signature_Kind.Importing)
	testing.expect_value(t, methods.members[0].signatures[0].parameters[0].name.text, "ev_object")
}

@(test)
oop_event_handler_requires_of_source_type :: proc(t: ^testing.T) {
	parsed := parse(
		`CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS on_changed FOR EVENT changed IMPORTING ev_object.
ENDCLASS.`,
		"oop_event_handler_invalid.abap",
		context.allocator,
	)

	expect_error_contains(t, parsed, "expected OF in FOR EVENT method declaration")
}

@(test)
oop_alias_target_must_be_interface_member :: proc(t: ^testing.T) {
	source := `INTERFACE lif.
  ALIASES bad FOR if_demo=>set.
ENDINTERFACE.`
	parsed := parse(source, "oop_alias_invalid.abap", context.allocator)

	expect_error_contains(t, parsed, "ALIASES target must be interface~member")
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
	testing.expect_value(t, methods.members[0].name.text, "if_demo~run")
	testing.expect_value(t, methods.members[0].qualifier.text, "if_demo")
	testing.expect_value(t, methods.members[0].member_name.text, "run")
	testing.expect_value(t, source[methods.members[0].qualifier.range.start:methods.members[0].qualifier.range.end], "if_demo")
	testing.expect_value(t, source[methods.members[0].member_name.range.start:methods.members[0].member_name.range.end], "run")
	testing.expect(t, .Redefinition in methods.members[0].flags)
}

@(test)
oop_qualified_method_requires_redefinition :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS if_demo~run IMPORTING iv_value TYPE i.
ENDCLASS.`
	parsed := parse(source, "oop_qualified_method_without_redefinition.abap", context.allocator)

	expect_error_contains(t, parsed, "qualified method declaration requires REDEFINITION")
}

@(test)
oop_redefinition_rejects_redeclared_signature :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS get_text REDEFINITION RETURNING VALUE(result) TYPE string.
ENDCLASS.`
	parsed := parse(source, "oop_redefinition_signature.abap", context.allocator)

	expect_error_contains(t, parsed, "REDEFINITION method cannot declare a signature")
}

@(test)
oop_signature_parameters_are_concrete_ast_clauses :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING it_source TYPE STANDARD TABLE it_any TYPE ANY TABLE it_index TYPE INDEX TABLE iv_state TYPE i OPTIONAL iv_date LIKE sy-datum iv_text TYPE string
      RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.`
	parsed := parse(source, "oop_parameters.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl).body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	importing := methods.members[0].signatures[0]
	returning := methods.members[0].signatures[1]
	testing.expect_value(t, len(importing.parameters), 6)
	testing.expect_value(t, importing.parameters[0].name.text, "it_source")
	testing.expect_value(t, importing.parameters[0].type_clause.form, ast.Data_Type_Form.Standard_Table)
	testing.expect(t, importing.parameters[0].type_clause.type_ref == nil)
	testing.expect_value(t, importing.parameters[1].name.text, "it_any")
	testing.expect_value(t, importing.parameters[1].type_clause.form, ast.Data_Type_Form.Any_Table)
	testing.expect(t, importing.parameters[1].type_clause.type_ref == nil)
	testing.expect_value(t, importing.parameters[2].name.text, "it_index")
	testing.expect_value(t, importing.parameters[2].type_clause.form, ast.Data_Type_Form.Index_Table)
	testing.expect(t, importing.parameters[2].type_clause.type_ref == nil)
	testing.expect_value(t, importing.parameters[3].name.text, "iv_state")
	testing.expect_value(t, importing.parameters[3].passing, ast.Parameter_Passing_Kind.Direct)
	testing.expect(t, importing.parameters[3].optional)
	testing.expect_value(t, importing.parameters[4].name.text, "iv_date")
	date_ref := importing.parameters[4].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, date_ref.base_name.text, "sy")
	testing.expect_value(t, date_ref.path[0].name.text, "datum")
	testing.expect_value(t, importing.parameters[5].name.text, "iv_text")
	testing.expect(t, importing.parameters[5].type_clause != nil)
	testing.expect_value(t, returning.parameters[0].name.text, "rv_ok")
	testing.expect_value(t, returning.parameters[0].passing, ast.Parameter_Passing_Kind.Value)
}

@(test)
oop_signature_accepts_bare_table_and_rejects_inline_table_definition :: proc(t: ^testing.T) {
	source := `INTERFACE lif.
  METHODS run
    EXPORTING
      !HTML_TABLE TYPE TABLE
      !ROWS TYPE TABLE OF string.
ENDINTERFACE.`
	parsed := parse(source, "oop_table_parameter_shapes.abap", context.allocator)

	expect_error_contains(
		t,
		parsed,
		"complex type definitions are not allowed in parameter sections",
	)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Interface_Decl).body[0].derived_stmt.(^ast.Oop_Simple_Stmt)
	exporting := methods.members[0].signatures[0]
	testing.expect_value(t, len(exporting.parameters), 2)
	testing.expect_value(t, exporting.parameters[0].type_clause.form, ast.Data_Type_Form.Table)
	testing.expect(t, !exporting.parameters[0].type_clause.table_has_of)
	testing.expect(t, exporting.parameters[0].type_clause.type_ref == nil)
	testing.expect_value(t, exporting.parameters[1].type_clause.form, ast.Data_Type_Form.Table)
	testing.expect(t, exporting.parameters[1].type_clause.table_has_of)
	row_ref := exporting.parameters[1].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, row_ref.base_name.text, "string")
}

@(test)
oop_signature_rejects_complex_parameter_type_definition :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
  CLASS-METHODS method_name
    IMPORTING
      !it_dels TYPE STANDARD TABLE OF /sttp/e_docnum WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.`
	parsed := parse(source, "oop_complex_parameter_type.abap", context.allocator)

	expect_error_contains(
		t,
		parsed,
		"complex type definitions are not allowed in parameter sections",
	)
}

@(test)
oop_signature_accepts_escaped_keyword_parameters :: proc(t: ^testing.T) {
	source := `INTERFACE lif.
  METHODS set_option
    IMPORTING
      !OPTION TYPE I
      !VALUE TYPE ABAP_BOOL DEFAULT ABAP_TRUE
      !NEXT TYPE I.
ENDINTERFACE.`
	parsed := parse(source, "oop_escaped_keyword_parameters.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Interface_Decl).body[0].derived_stmt.(^ast.Oop_Simple_Stmt)
	importing := methods.members[0].signatures[0]
	testing.expect_value(t, len(importing.parameters), 3)
	testing.expect_value(t, importing.parameters[0].name.text, "OPTION")
	testing.expect_value(t, importing.parameters[0].passing, ast.Parameter_Passing_Kind.Direct)
	testing.expect(t, importing.parameters[0].escaped)
	option_type := importing.parameters[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, option_type.base_name.text, "I")
	testing.expect_value(t, importing.parameters[1].name.text, "VALUE")
	testing.expect_value(t, importing.parameters[1].passing, ast.Parameter_Passing_Kind.Direct)
	testing.expect(t, importing.parameters[1].escaped)
	testing.expect(t, importing.parameters[1].has_default)
	testing.expect(t, importing.parameters[1].default_expr != nil)
	value_type := importing.parameters[1].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, value_type.base_name.text, "ABAP_BOOL")
	testing.expect_value(t, importing.parameters[2].name.text, "NEXT")
	testing.expect(t, importing.parameters[2].escaped)
	testing.expect_value(
		t,
		ast.print_node(methods, context.allocator),
		"METHODS set_option IMPORTING !OPTION TYPE I !VALUE TYPE ABAP_BOOL DEFAULT ABAP_TRUE !NEXT TYPE I.",
	)
}

@(test)
oop_member_additions_and_preferred_parameter_are_ast_fields :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS choose ABSTRACT IMPORTING !iv_one TYPE string !iv_two TYPE string PREFERRED PARAMETER iv_two.
    METHODS done FINAL.
ENDCLASS.`
	parsed := parse(source, "oop_member_additions.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	class_decl := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl)
	choose := class_decl.body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	done := class_decl.body[2].derived_stmt.(^ast.Oop_Simple_Stmt)
	signature := choose.members[0].signatures[0]

	testing.expect(t, .Abstract in choose.members[0].flags)
	testing.expect(t, .Final in done.members[0].flags)
	testing.expect_value(t, signature.preferred_parameter.text, "iv_two")
	testing.expect_value(t, len(signature.parameters), 2)
	testing.expect(t, signature.parameters[0].escaped)
	testing.expect(t, signature.parameters[1].escaped)
	testing.expect_value(t, source[choose.members[0].range.start:choose.members[0].range.end], "choose ABSTRACT IMPORTING !iv_one TYPE string !iv_two TYPE string PREFERRED PARAMETER iv_two")
	testing.expect_value(
		t,
		ast.print_node(choose, context.allocator),
		"METHODS choose ABSTRACT IMPORTING !iv_one TYPE string !iv_two TYPE string PREFERRED PARAMETER iv_two.",
	)
}

@(test)
oop_signature_accepts_unescaped_value_parameter_name :: proc(t: ^testing.T) {
	source := `INTERFACE lif.
  METHODS run IMPORTING value TYPE numeric.
ENDINTERFACE.`
	parsed := parse(source, "oop_value_parameter.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Interface_Decl).body[0].derived_stmt.(^ast.Oop_Simple_Stmt)
	importing := methods.members[0].signatures[0]
	testing.expect_value(t, len(importing.parameters), 1)
	testing.expect_value(t, importing.parameters[0].name.text, "value")
	testing.expect_value(t, importing.parameters[0].passing, ast.Parameter_Passing_Kind.Direct)
	value_type := importing.parameters[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, value_type.base_name.text, "numeric")
}

@(test)
oop_signature_type_components_can_use_addition_keywords :: proc(t: ^testing.T) {
	source := `INTERFACE lif.
  METHODS run
    IMPORTING
      !IV_LENGTH TYPE IF_FDT_ELEMENT=>LENGTH OPTIONAL
      !IV_DECIMALS TYPE IF_FDT_ELEMENT=>DECIMALS OPTIONAL.
ENDINTERFACE.`
	parsed := parse(source, "oop_keyword_type_components.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	methods := parsed.root.stmts[0].derived_stmt.(^ast.Interface_Decl).body[0].derived_stmt.(^ast.Oop_Simple_Stmt)
	importing := methods.members[0].signatures[0]
	testing.expect_value(t, len(importing.parameters), 2)
	length_ref := importing.parameters[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	decimals_ref := importing.parameters[1].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, length_ref.base_name.text, "IF_FDT_ELEMENT")
	testing.expect_value(t, length_ref.path[0].name.text, "LENGTH")
	testing.expect_value(t, length_ref.path[0].selector, ast.Selector_Op.Fat_Arrow)
	testing.expect_value(t, decimals_ref.base_name.text, "IF_FDT_ELEMENT")
	testing.expect_value(t, decimals_ref.path[0].name.text, "DECIMALS")
	testing.expect(t, importing.parameters[0].optional)
	testing.expect(t, importing.parameters[1].optional)
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
  TABLES ct_rows = lt_rows
  CHANGING cv_any = FIELD-SYMBOL(<fs_any>)
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
		.Tables,
		.Changing,
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
	testing.expect_value(t, function.named_args[0].raw_refs[0].name.text, "lv_in")
	testing.expect_value(t, len(function.named_args[1].raw_decls), 1)
	testing.expect_value(t, function.named_args[1].raw_decls[0].name.text, "lv_out")
	testing.expect_value(t, len(function.named_args[2].raw_refs), 1)
	testing.expect_value(t, function.named_args[2].raw_refs[0].name.text, "lt_rows")
	testing.expect_value(t, len(function.named_args[3].raw_decls), 1)
	testing.expect_value(t, function.named_args[3].raw_decls[0].kind, ast.Raw_Operand_Inline_Decl_Kind.Field_Symbol)
	testing.expect_value(t, function.named_args[3].raw_decls[0].name.text, "<fs_any>")
	testing.expect_value(t, len(function.named_args[4].raw_refs), 0)
	testing.expect_value(t, len(method.named_args[0].raw_refs), 0)
	testing.expect_value(t, len(method.named_args[1].raw_decls), 1)
	testing.expect_value(t, method.named_args[1].raw_decls[0].name.text, "lv_result")
}

@(test)
call_function_argument_values_accept_section_keyword_selectors :: proc(t: ^testing.T) {
	source := `CALL FUNCTION 'Z_FM'
  TABLES tables_parameter = <ls_func>-tables
         changing_parameter = <ls_func>-changing.`
	parsed := parse(source, "call_function_keyword_selectors.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	testing.expect_value(t, len(stmt.named_args), 2)
	if len(stmt.named_args) != 2 {
		return
	}

	first := stmt.named_args[0]
	second := stmt.named_args[1]
	testing.expect_value(t, source[first.value_range.start:first.value_range.end], "<ls_func>-tables")
	testing.expect_value(t, source[second.value_range.start:second.value_range.end], "<ls_func>-changing")
	testing.expect_value(t, len(first.raw_refs), 1)
	testing.expect_value(t, len(second.raw_refs), 1)
	testing.expect_value(t, first.raw_refs[0].name.text, "<ls_func>")
	testing.expect_value(t, second.raw_refs[0].name.text, "<ls_func>")
	testing.expect_value(t, first.raw_refs[0].path[0].name.text, "tables")
	testing.expect_value(t, second.raw_refs[0].path[0].name.text, "changing")
}

@(test)
call_function_accepts_parameter_and_exception_tables :: proc(t: ^testing.T) {
	source := `CALL FUNCTION lv_func DESTINATION c_dest
  PARAMETER-TABLE lt_params
  EXCEPTION-TABLE lt_exceptions.`
	parsed := parse(source, "call_function_parameter_table.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	param_table := stmt.function_parameter_table.derived_expr.(^ast.Type_Ref_Expr)
	exception_table := stmt.function_exception_table.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, param_table.source.text, "lt_params")
	testing.expect_value(t, exception_table.source.text, "lt_exceptions")
	testing.expect_value(t, len(stmt.arg_sections), 0)
	testing.expect_value(t, len(stmt.named_args), 0)
}

@(test)
call_function_models_destination_sections_and_exception_messages :: proc(t: ^testing.T) {
	source := `CALL FUNCTION lv_func DESTINATION c_dest
  EXPORTING iv_value = lv_value
  IMPORTING ev_value = DATA(lv_out)
  TABLES ct_rows = lt_rows
  CHANGING cv_value = lv_value
  EXCEPTIONS
    system_failure = 1 MESSAGE lv_msg
    communication_failure = 2 MESSAGE lv_msg
    OTHERS = 3.`
	parsed := parse(source, "call_function_destination.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	target := stmt.target.derived_expr.(^ast.Ident_Expr)
	destination := stmt.function_destination.derived_expr.(^ast.Ident_Expr)
	testing.expect_value(t, target.name, "lv_func")
	testing.expect_value(t, stmt.function_execution, ast.Call_Function_Execution_Kind.Destination)
	testing.expect_value(t, destination.name, "c_dest")
	testing.expect_value(t, len(stmt.arg_sections), 5)
	testing.expect_value(t, stmt.arg_sections[0].kind, ast.Call_Arg_Section_Kind.Exporting)
	testing.expect_value(t, stmt.arg_sections[1].kind, ast.Call_Arg_Section_Kind.Importing)
	testing.expect_value(t, stmt.arg_sections[2].kind, ast.Call_Arg_Section_Kind.Tables)
	testing.expect_value(t, stmt.arg_sections[3].kind, ast.Call_Arg_Section_Kind.Changing)
	testing.expect_value(t, stmt.arg_sections[4].kind, ast.Call_Arg_Section_Kind.Exceptions)
	testing.expect_value(t, len(stmt.named_args), 7)
	testing.expect_value(t, stmt.named_args[4].name.text, "system_failure")
	testing.expect(t, stmt.named_args[4].message != nil)
	testing.expect_value(t, stmt.named_args[5].name.text, "communication_failure")
	testing.expect(t, stmt.named_args[5].message != nil)
	testing.expect_value(t, stmt.named_args[6].name.text, "OTHERS")
	testing.expect(t, stmt.named_args[6].message == nil)
}

@(test)
call_function_accepts_async_destination_additions :: proc(t: ^testing.T) {
	source := `CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
  DESTINATION 'NONE'
  STARTING NEW TASK 'ABAPGIT'
  EXPORTING tcode = iv_tcode.
CALL FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
  STARTING NEW TASK lv_task
  DESTINATION IN GROUP mv_group
  CALLING on_end_of_task ON END OF TASK
  EXPORTING is_tadir = is_tadir.`
	parsed := parse(source, "call_function_async_destination.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	first := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	first_destination := first.function_destination.derived_expr.(^ast.Literal_Expr)
	first_task := first.function_task.derived_expr.(^ast.Literal_Expr)
	testing.expect_value(t, first.function_execution, ast.Call_Function_Execution_Kind.Starting_New_Task)
	testing.expect_value(t, first.function_destination_in_group, false)
	testing.expect_value(t, first_destination.value, "'NONE'")
	testing.expect_value(t, first_task.value, "'ABAPGIT'")

	second := parsed.root.stmts[1].derived_stmt.(^ast.Call_Stmt)
	second_destination := second.function_destination.derived_expr.(^ast.Ident_Expr)
	second_task := second.function_task.derived_expr.(^ast.Ident_Expr)
	handler := second.function_end_task_handler.derived_expr.(^ast.Ident_Expr)
	testing.expect_value(t, second.function_execution, ast.Call_Function_Execution_Kind.Starting_New_Task)
	testing.expect_value(t, second.function_destination_in_group, true)
	testing.expect_value(t, second_destination.name, "mv_group")
	testing.expect_value(t, second_task.name, "lv_task")
	testing.expect_value(t, second.function_end_task_handler_kind, ast.Call_Function_End_Task_Handler_Kind.Calling)
	testing.expect_value(t, handler.name, "on_end_of_task")
}

@(test)
call_function_rejects_invalid_parameter_list_forms :: proc(t: ^testing.T) {
	parsed := parse(
		`CALL FUNCTION 'A' iv_value = lv_value.
CALL FUNCTION 'B' RECEIVING rv_value = lv_value.
CALL FUNCTION 'C' EXPORTING.
CALL FUNCTION 'D' CHANGING cv_value = lv_value TABLES ct_rows = lt_rows.
CALL FUNCTION 'E' EXPORTING iv_value = lv_value EXPORTING iv_other = lv_value.
CALL FUNCTION 'F' EXPORTING iv_value = lv_value iv_value = lv_other.
CALL FUNCTION 'G' UNKNOWN.`,
		"call_function_bad_parameter_lists.abap",
		context.allocator,
	)

	expect_error_contains(t, parsed, "parameter assignment requires a parameter section")
	expect_error_contains(t, parsed, "RECEIVING is not allowed in CALL FUNCTION parameter list")
	expect_error_contains(t, parsed, "expected parameter assignment after CALL FUNCTION section")
	expect_error_contains(t, parsed, "CALL FUNCTION parameter sections are out of order")
	expect_error_contains(t, parsed, "duplicate CALL FUNCTION parameter section")
	expect_error_contains(t, parsed, "duplicate CALL FUNCTION parameter")
	expect_error_contains(t, parsed, "unexpected CALL FUNCTION addition")
}

@(test)
call_method_args_keep_value_exprs_and_keyword_names :: proc(t: ^testing.T) {
	source := `CALL METHOD lo->run
  EXPORTING tables = get_field_rules( )
  CHANGING cv_value = lv_value.
CALL METHOD /sttp/cl_dm_query=>query_objectdata_item(
  EXPORTING iv_objcode = lv_objcode
  CHANGING co_messages = lo_messages
).`
	parsed := parse(source, "call_method_value_exprs.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	parenthesized := parsed.root.stmts[1].derived_stmt.(^ast.Call_Stmt)
	testing.expect_value(t, len(stmt.arg_sections), 2)
	testing.expect_value(t, len(stmt.named_args), 2)
	testing.expect_value(t, stmt.arg_sections[0].kind, ast.Call_Arg_Section_Kind.Exporting)
	testing.expect_value(t, stmt.arg_sections[1].kind, ast.Call_Arg_Section_Kind.Changing)
	testing.expect_value(t, stmt.named_args[0].section, ast.Call_Arg_Section_Kind.Exporting)
	testing.expect_value(t, stmt.named_args[0].name.text, "tables")
	_, is_call := stmt.named_args[0].value.derived_expr.(^ast.Call_Expr)
	testing.expect(t, is_call)
	testing.expect(t, stmt.named_args[1].value != nil)
	testing.expect_value(t, ast.print_node(parenthesized.target, context.allocator), "/sttp/cl_dm_query=>query_objectdata_item")
	testing.expect_value(t, len(parenthesized.named_args), 2)
	testing.expect_value(t, parenthesized.named_args[0].name.text, "iv_objcode")
	testing.expect(t, parenthesized.named_args[0].value != nil)
	testing.expect_value(t, parenthesized.named_args[1].name.text, "co_messages")
	testing.expect(t, parenthesized.named_args[1].value != nil)
}

@(test)
raw_call_method_targets_carry_parser_reference_facts :: proc(t: ^testing.T) {
	source := `CALL METHOD lo_client->run EXPORTING iv_value = lv_value.
CALL METHOD lcl_demo=>class_run.
CALL METHOD lo_client->('RUN') EXPORTING iv_value = lv_value.
CALL METHOD <ls_extension_mapper_pair>-file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT').
CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in.
CALL METHOD (lv_class)=>create.
CALL METHOD (lv_class)=>if_demo~create_instance.`
	parsed := parse(source, "raw_call_method_targets.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	instance := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	static := parsed.root.stmts[1].derived_stmt.(^ast.Call_Stmt)
	dynamic_call := parsed.root.stmts[2].derived_stmt.(^ast.Call_Stmt)
	dynamic_component_call := parsed.root.stmts[3].derived_stmt.(^ast.Call_Stmt)
	dynamic_static_literal := parsed.root.stmts[4].derived_stmt.(^ast.Call_Stmt)
	dynamic_static_variable := parsed.root.stmts[5].derived_stmt.(^ast.Call_Stmt)
	dynamic_static_qualified := parsed.root.stmts[6].derived_stmt.(^ast.Call_Stmt)
	instance_target := instance.target.derived_expr.(^ast.Type_Ref_Expr)
	static_target := static.target.derived_expr.(^ast.Type_Ref_Expr)
	dynamic_target := dynamic_call.target.derived_expr.(^ast.Dynamic_Call_Method_Target_Expr)
	dynamic_component_target := dynamic_component_call.target.derived_expr.(^ast.Dynamic_Call_Method_Target_Expr)
	dynamic_static_literal_target := dynamic_static_literal.target.derived_expr.(^ast.Dynamic_Call_Method_Target_Expr)
	dynamic_static_variable_target := dynamic_static_variable.target.derived_expr.(^ast.Dynamic_Call_Method_Target_Expr)
	dynamic_static_qualified_target := dynamic_static_qualified.target.derived_expr.(^ast.Dynamic_Call_Method_Target_Expr)

	testing.expect(t, instance_target.raw_operand)
	testing.expect_value(t, instance_target.raw_refs[0].name.text, "lo_client")
	testing.expect_value(t, instance_target.raw_refs[0].path[0].name.text, "run")
	testing.expect(t, static_target.raw_refs[0].type_base)
	testing.expect_value(t, static_target.raw_refs[0].name.text, "lcl_demo")
	testing.expect_value(t, static_target.raw_refs[0].path[0].name.text, "class_run")
	testing.expect(t, !dynamic_target.base_dynamic)
	testing.expect(t, dynamic_target.method_dynamic)
	testing.expect_value(t, ast.print_node(dynamic_target, context.allocator), "lo_client->('RUN')")
	_, component_receiver_ok := dynamic_component_target.base.derived_expr.(^ast.Selector_Expr)
	testing.expect(t, component_receiver_ok)
	testing.expect(t, !dynamic_component_target.base_dynamic)
	testing.expect(t, dynamic_component_target.method_dynamic)
	testing.expect_value(t, ast.print_node(dynamic_component_target, context.allocator), "<ls_extension_mapper_pair>-file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')")
	testing.expect(t, dynamic_static_literal_target.base_dynamic)
	testing.expect(t, !dynamic_static_literal_target.method_dynamic)
	testing.expect_value(t, ast.print_node(dynamic_static_literal_target, context.allocator), "('CL_ABAP_CONV_CODEPAGE')=>create_in")
	testing.expect(t, dynamic_static_variable_target.base_dynamic)
	testing.expect(t, !dynamic_static_variable_target.method_dynamic)
	testing.expect_value(t, ast.print_node(dynamic_static_variable_target, context.allocator), "(lv_class)=>create")
	testing.expect(t, dynamic_static_qualified_target.base_dynamic)
	testing.expect(t, !dynamic_static_qualified_target.method_dynamic)
	testing.expect_value(t, ast.print_node(dynamic_static_qualified_target, context.allocator), "(lv_class)=>if_demo~create_instance")
}

@(test)
ole_call_method_targets_carry_value_parts :: proc(t: ^testing.T) {
	source := `CALL METHOD OF lv_excel 'Workbooks' = lv_wrkbks.
CALL METHOD OF lv_excel 'Cells' = lv_cell EXPORTING #1 = lv_row #2 = lv_col.
CALL METHOD OF lv_wrkbk 'SaveAs' EXPORTING #1 = x_file.
CALL METHOD OF lv_excel 'Quit'.`
	parsed := parse(source, "ole_call_method_targets.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	workbooks := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	cells := parsed.root.stmts[1].derived_stmt.(^ast.Call_Stmt)
	save_as := parsed.root.stmts[2].derived_stmt.(^ast.Call_Stmt)
	quit := parsed.root.stmts[3].derived_stmt.(^ast.Call_Stmt)
	workbooks_target := workbooks.target.derived_expr.(^ast.Ole_Call_Method_Target_Expr)
	cells_target := cells.target.derived_expr.(^ast.Ole_Call_Method_Target_Expr)
	save_as_target := save_as.target.derived_expr.(^ast.Ole_Call_Method_Target_Expr)
	quit_target := quit.target.derived_expr.(^ast.Ole_Call_Method_Target_Expr)

	testing.expect_value(t, ast.print_node(workbooks_target, context.allocator), "OF lv_excel 'Workbooks' = lv_wrkbks")
	testing.expect_value(t, ast.print_node(cells_target.object, context.allocator), "lv_excel")
	testing.expect_value(t, ast.print_node(cells_target.member, context.allocator), "'Cells'")
	testing.expect_value(t, ast.print_node(cells_target.result, context.allocator), "lv_cell")
	testing.expect_value(t, len(cells.named_args), 2)
	testing.expect_value(t, cells.named_args[0].name.text, "#1")
	testing.expect_value(t, cells.named_args[1].name.text, "#2")
	testing.expect(t, save_as_target.result == nil)
	testing.expect_value(t, len(save_as.named_args), 1)
	testing.expect_value(t, save_as.named_args[0].name.text, "#1")
	testing.expect(t, quit_target.result == nil)
}

@(test)
call_method_target_stops_before_positional_parenthesized_args :: proc(t: ^testing.T) {
	source := `CALL METHOD lo_send_mail->set_document( lo_document ).`
	parsed := parse(source, "call_method_positional_arg.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	testing.expect_value(t, ast.print_node(stmt.target, context.allocator), "lo_send_mail->set_document")
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
	testing.expect_value(t, target.name.text, "id")
	testing.expect(t, !target.raw_operand)
	testing.expect_value(t, len(stmt.transformation_args), 3)
	testing.expect_value(t, stmt.transformation_args[0].kind, ast.Call_Transformation_Arg_Kind.Options)
	testing.expect_value(t, stmt.transformation_args[0].name.text, "initial_components")
	testing.expect(t, stmt.transformation_args[0].has_eq)
	testing.expect_value(t, stmt.transformation_args[1].kind, ast.Call_Transformation_Arg_Kind.Source)
	testing.expect_value(t, source_value.raw_refs[0].name.text, "lt_stab")
	testing.expect_value(t, stmt.transformation_args[2].kind, ast.Call_Transformation_Arg_Kind.Result)
	testing.expect_value(t, stmt.transformation_args[2].name.text, "XML")
	testing.expect_value(t, result_value.raw_refs[0].name.text, "li_doc")
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
	testing.expect_value(t, first_target.raw_refs[0].name.text, "tcode")
	testing.expect_value(t, second_target.raw_refs[0].name.text, "tcode")
	testing.expect_value(t, len(first.transaction_operands), len(expected_first))
	testing.expect_value(t, len(second.transaction_operands), len(expected_second))
	for value, i in expected_first {
		operand := first.transaction_operands[i].derived_expr.(^ast.Type_Ref_Expr)
		testing.expect_value(t, operand.raw_refs[0].name.text, value)
	}
	for value, i in expected_second {
		operand := second.transaction_operands[i].derived_expr.(^ast.Type_Ref_Expr)
		testing.expect_value(t, operand.raw_refs[0].name.text, value)
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
constructor_chain_statement_is_not_macro_call :: proc(t: ^testing.T) {
	source := `NEW lcl_dep( )->consume( ).`
	parsed := parse(source, "constructor_chain_stmt.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Expr_Stmt)
	_, is_call := stmt.expr.derived_expr.(^ast.Call_Expr)
	testing.expect(t, is_call)
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
	assign_component := assign.component.derived_expr.(^ast.Type_Ref_Expr)
	assign_structure := assign.structure.derived_expr.(^ast.Type_Ref_Expr)
	assign_target := assign.target.derived_expr.(^ast.Type_Ref_Expr)
	decl := single_data_branch(parsed.root.stmts[2])
	type_ref := decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect(t, raise_target.raw_operand)
	testing.expect_value(t, len(raise_target.raw_refs), 1)
	testing.expect_value(t, raise_target.raw_refs[0].name.text, "changed")
	testing.expect_value(t, len(raise_args.raw_decls), 1)
	testing.expect_value(t, raise_args.raw_decls[0].kind, ast.Raw_Operand_Inline_Decl_Kind.Data)
	testing.expect_value(t, raise_args.raw_decls[0].name.text, "lv_raw")
	testing.expect_value(t, len(raise_args.raw_refs), 1)
	testing.expect_value(t, raise_args.raw_refs[0].name.text, "ls_row")
	testing.expect_value(t, raise_args.raw_refs[0].path[0].name.text, "field")
	testing.expect_value(t, len(assign_component.raw_refs), 1)
	testing.expect_value(t, assign_component.raw_refs[0].name.text, "lv_name")
	testing.expect_value(t, len(assign_structure.raw_refs), 1)
	testing.expect_value(t, assign_structure.raw_refs[0].name.text, "ls_row")
	testing.expect_value(t, len(assign_target.raw_decls), 1)
	testing.expect_value(t, assign_target.raw_decls[0].kind, ast.Raw_Operand_Inline_Decl_Kind.Field_Symbol)
	testing.expect_value(t, assign_target.raw_decls[0].name.text, "<fs_raw>")
	testing.expect(t, !type_ref.raw_operand)
	testing.expect_value(t, len(type_ref.raw_refs), 0)
}

@(test)
raw_assign_deref_path_keeps_arrow_selector :: proc(t: ^testing.T) {
	parsed := parse(`ASSIGN lr_data->* TO <fs>.`, "assign_deref.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Field_Stmt)
	operand := assign.source.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect(t, operand.raw_operand)
	testing.expect_value(t, operand.raw_refs[0].name.text, "lr_data")
	testing.expect_value(t, operand.raw_refs[0].path[0].name.text, "*")
	testing.expect_value(t, operand.raw_refs[0].path[0].selector, ast.Selector_Op.Arrow)
}

@(test)
raw_assign_dynamic_path_marks_reference_source :: proc(t: ^testing.T) {
	parsed := parse(`ASSIGN lo_object->('PARAMS') TO <lo_params>.`, "assign_dynamic_path.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Field_Stmt)
	operand := assign.source.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect(t, operand.raw_operand)
	testing.expect_value(t, len(operand.raw_refs), 1)
	testing.expect_value(t, operand.raw_refs[0].name.text, "lo_object")
	testing.expect(t, operand.raw_refs[0].dynamic_path)
	testing.expect_value(t, len(operand.raw_refs[0].path), 0)
}

@(test)
raw_assign_casting_does_not_reference_clause_keyword :: proc(t: ^testing.T) {
	parsed := parse(`ASSIGN lv_x TO <lv_y> CASTING.`, "assign_casting.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Field_Stmt)
	source := assign.source.derived_expr.(^ast.Type_Ref_Expr)
	target := assign.target.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect(t, assign.casting)
	testing.expect_value(t, len(source.raw_refs), 1)
	testing.expect_value(t, source.raw_refs[0].name.text, "lv_x")
	testing.expect_value(t, len(target.raw_refs), 1)
	testing.expect_value(t, target.raw_refs[0].name.text, "<lv_y>")
}

@(test)
raw_assign_allows_range_addition :: proc(t: ^testing.T) {
	parsed := parse(`ASSIGN wa_snap-flist(1600) TO <buffer> RANGE wa_snap.`, "assign_range.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Field_Stmt)
	testing.expect(t, assign.source != nil)
	testing.expect(t, assign.target != nil)
}

@(test)
assign_casting_addition_is_modeled_and_printed :: proc(t: ^testing.T) {
	source := `ASSIGN lv_x TO <lv_y> CASTING TYPE i DECIMALS lv_dec.`
	parsed := parse(source, "assign_casting_type.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Field_Stmt)

	testing.expect(t, assign.casting)
	testing.expect(t, assign.casting_type != nil)
	testing.expect(t, assign.casting_decimals != nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
assign_rejects_addition_keywords_outside_casting_position :: proc(t: ^testing.T) {
	without_casting := parse(`ASSIGN lv_x TO <lv_y> TYPE i.`, "assign_bad_type.abap", context.allocator)
	before_to := parse(`ASSIGN lv_x CASTING TO <lv_y>.`, "assign_bad_casting.abap", context.allocator)

	expect_error_contains(t, without_casting, "unexpected ASSIGN addition")
	expect_error_contains(t, before_to, "expected TO in ASSIGN statement")
}

@(test)
memory_transfer_statements_model_entries :: proc(t: ^testing.T) {
	source := `IMPORT variscreens = lt_variscreens FROM MEMORY ID '%_SCRNR_%'.
EXPORT variscreens = lt_variscreens TO MEMORY ID '%_SCRNR_%'.`
	parsed := parse(source, "import_memory.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	import_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Import_Stmt)
	export_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Export_Stmt)
	value := import_stmt.parameters[0].value.derived_expr.(^ast.Ident_Expr)
	memory_id := import_stmt.medium.id.derived_expr.(^ast.Literal_Expr)

	testing.expect_value(t, len(import_stmt.parameters), 1)
	testing.expect_value(t, import_stmt.parameters[0].name.text, "variscreens")
	testing.expect_value(t, import_stmt.medium.kind, ast.Data_Cluster_Medium_Kind.Memory_ID)
	testing.expect_value(t, len(export_stmt.parameters), 1)
	testing.expect_value(t, export_stmt.parameters[0].name.text, "variscreens")
	testing.expect_value(t, export_stmt.medium.kind, ast.Data_Cluster_Medium_Kind.Memory_ID)
	testing.expect_value(t, value.name, "lt_variscreens")
	testing.expect_value(t, memory_id.value, "'%_SCRNR_%'")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
memory_transfer_statements_accept_keyword_like_value_name :: proc(t: ^testing.T) {
	source := `EXPORT name = value TO MEMORY ID 'id'.
IMPORT name = value FROM MEMORY ID 'id'.`
	parsed := parse(source, "import_export_memory_keyword_like_value.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	export_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Export_Stmt)
	import_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Import_Stmt)
	export_value := export_stmt.parameters[0].value.derived_expr.(^ast.Ident_Expr)
	import_value := import_stmt.parameters[0].value.derived_expr.(^ast.Ident_Expr)
	export_id := export_stmt.medium.id.derived_expr.(^ast.Literal_Expr)
	import_id := import_stmt.medium.id.derived_expr.(^ast.Literal_Expr)

	testing.expect_value(t, len(export_stmt.parameters), 1)
	testing.expect_value(t, export_stmt.parameters[0].name.text, "name")
	testing.expect_value(t, export_stmt.medium.kind, ast.Data_Cluster_Medium_Kind.Memory_ID)
	testing.expect_value(t, export_value.name, "value")
	testing.expect_value(t, export_id.value, "'id'")
	testing.expect_value(t, len(import_stmt.parameters), 1)
	testing.expect_value(t, import_stmt.parameters[0].name.text, "name")
	testing.expect_value(t, import_stmt.medium.kind, ast.Data_Cluster_Medium_Kind.Memory_ID)
	testing.expect_value(t, import_value.name, "value")
	testing.expect_value(t, import_id.value, "'id'")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
export_to_memory_accepts_multiline_parameters_without_commas :: proc(t: ^testing.T) {
	source := `EXPORT scpr3_display_only = lv_display_only
       scpr3_bcset_id     = lv_bcset_id
  TO MEMORY ID 'SCPR3_PARAMETER'.`
	parsed := parse(source, "export_memory_multiline.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	export_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Export_Stmt)
	testing.expect_value(t, len(export_stmt.parameters), 2)
	testing.expect_value(t, export_stmt.parameters[0].name.text, "scpr3_display_only")
	testing.expect_value(t, export_stmt.parameters[1].name.text, "scpr3_bcset_id")
	testing.expect_value(t, export_stmt.medium.kind, ast.Data_Cluster_Medium_Kind.Memory_ID)
}

@(test)
data_cluster_medium_variants_model_entries :: proc(t: ^testing.T) {
	source := `IMPORT row = ls_row FROM DATA BUFFER lv_xstr.
EXPORT row = ls_row TO DATA BUFFER lv_xstr.
IMPORT row = ls_row FROM INTERNAL TABLE lt_cluster.
EXPORT row = ls_row TO INTERNAL TABLE lt_cluster.
IMPORT row = ls_row FROM DATABASE demo_indx_blob(sc) TO ls_indx CLIENT lv_client ID lv_id.
EXPORT row = ls_row TO DATABASE demo_indx_blob(sc) FROM ls_indx CLIENT lv_client ID lv_id.
IMPORT row = ls_row FROM SHARED MEMORY demo_indx_blob(sc) TO ls_indx CLIENT lv_client ID lv_id.
EXPORT row = ls_row TO SHARED BUFFER demo_indx_blob(sc) FROM ls_indx CLIENT lv_client ID lv_id.`
	parsed := parse(source, "data_cluster_media.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	import_buffer := parsed.root.stmts[0].derived_stmt.(^ast.Import_Stmt)
	export_buffer := parsed.root.stmts[1].derived_stmt.(^ast.Export_Stmt)
	import_table := parsed.root.stmts[2].derived_stmt.(^ast.Import_Stmt)
	export_table := parsed.root.stmts[3].derived_stmt.(^ast.Export_Stmt)
	import_database := parsed.root.stmts[4].derived_stmt.(^ast.Import_Stmt)
	export_database := parsed.root.stmts[5].derived_stmt.(^ast.Export_Stmt)
	import_shared := parsed.root.stmts[6].derived_stmt.(^ast.Import_Stmt)
	export_shared := parsed.root.stmts[7].derived_stmt.(^ast.Export_Stmt)

	testing.expect_value(t, import_buffer.medium.kind, ast.Data_Cluster_Medium_Kind.Data_Buffer)
	testing.expect_value(t, export_buffer.medium.kind, ast.Data_Cluster_Medium_Kind.Data_Buffer)
	testing.expect_value(t, import_table.medium.kind, ast.Data_Cluster_Medium_Kind.Internal_Table)
	testing.expect_value(t, export_table.medium.kind, ast.Data_Cluster_Medium_Kind.Internal_Table)
	testing.expect_value(t, import_database.medium.kind, ast.Data_Cluster_Medium_Kind.Database)
	testing.expect_value(t, export_database.medium.kind, ast.Data_Cluster_Medium_Kind.Database)
	testing.expect_value(t, import_shared.medium.kind, ast.Data_Cluster_Medium_Kind.Shared_Memory)
	testing.expect_value(t, export_shared.medium.kind, ast.Data_Cluster_Medium_Kind.Shared_Buffer)
	testing.expect_value(t, import_database.medium.dbtab.text, "demo_indx_blob")
	testing.expect_value(t, import_database.medium.area.text, "sc")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
data_cluster_parameter_keyword_forms_model_entries :: proc(t: ^testing.T) {
	source := `IMPORT row TO ls_row FROM MEMORY ID lv_id.
EXPORT row FROM ls_row TO MEMORY ID lv_id.`
	parsed := parse(source, "data_cluster_parameter_keywords.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	import_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Import_Stmt)
	export_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Export_Stmt)
	import_value := import_stmt.parameters[0].value.derived_expr.(^ast.Ident_Expr)
	export_value := export_stmt.parameters[0].value.derived_expr.(^ast.Ident_Expr)

	testing.expect_value(t, import_stmt.parameters[0].name.text, "row")
	testing.expect_value(t, export_stmt.parameters[0].name.text, "row")
	testing.expect_value(t, import_value.name, "ls_row")
	testing.expect_value(t, export_value.name, "ls_row")
}

@(test)
data_cluster_database_medium_accepts_doc_example_order :: proc(t: ^testing.T) {
	source := `IMPORT row = ls_row FROM DATABASE demo_indx_blob(sc) ID lv_id TO ls_indx.`
	parsed := parse(source, "data_cluster_database_order.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Import_Stmt)
	testing.expect_value(t, stmt.medium.kind, ast.Data_Cluster_Medium_Kind.Database)
	testing.expect(t, stmt.medium.id != nil)
	testing.expect(t, stmt.medium.work_area != nil)
}

@(test)
create_object_models_target_and_type_clause :: proc(t: ^testing.T) {
	source := `CREATE OBJECT ri_html TYPE zcl_abapgit_html.
CREATE OBJECT ri_dyn TYPE (lv_class).`
	parsed := parse(source, "create_object_type_ref.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	static_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Create_Object_Stmt)
	dynamic_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Create_Object_Stmt)
	static_target := static_stmt.target.derived_expr.(^ast.Type_Ref_Expr)
	static_type := static_stmt.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	dynamic_type := dynamic_stmt.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, len(static_target.raw_refs), 1)
	testing.expect_value(t, static_target.raw_refs[0].name.text, "ri_html")
	testing.expect(t, !static_stmt.type_dynamic)
	testing.expect(t, !static_type.raw_operand)
	testing.expect_value(t, ast.print_node(static_stmt.type_ref, context.allocator), "zcl_abapgit_html")
	testing.expect(t, dynamic_stmt.type_dynamic)
	testing.expect(t, dynamic_type.raw_operand)
	testing.expect_value(t, len(dynamic_type.raw_refs), 1)
	testing.expect_value(t, dynamic_type.raw_refs[0].name.text, "lv_class")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
create_object_models_exporting_as_call_args :: proc(t: ^testing.T) {
	source := `CREATE OBJECT ro_generic
  EXPORTING
    io_field_rules = get_field_rules( )
    tables         = ms_item.`
	parsed := parse(source, "create_object_exporting.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Create_Object_Stmt)
	section := stmt.operands[0].derived_expr.(^ast.Call_Arg_Section_Expr)
	arg := section.args[0].derived_expr.(^ast.Call_Named_Arg_Expr)

	testing.expect_value(t, section.kind, ast.Call_Arg_Section_Kind.Exporting)
	testing.expect_value(t, arg.name.text, "io_field_rules")
	testing.expect_value(t, section.args[1].derived_expr.(^ast.Call_Named_Arg_Expr).name.text, "tables")
	_, ok := arg.value.derived_expr.(^ast.Call_Expr)
	testing.expect(t, ok)
}

@(test)
create_object_exporting_rejects_stray_section_tokens :: proc(t: ^testing.T) {
	source := `CREATE OBJECT ro_generic
  EXPORTING
    io_field_rules = get_field_rules( )1
    is_item        = 1ms_item.`
	parsed := parse(source, "create_object_bad_exporting.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
}

@(test)
create_data_ref_to_dynamic_type_tracks_only_runtime_name_expr :: proc(t: ^testing.T) {
	source := `CREATE DATA lr_lit TYPE REF TO ('CL_W3_API_XML3').
CREATE DATA lr_var TYPE REF TO (lv_class).`
	parsed := parse(source, "create_data_dynamic_type_ref.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	lit_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Create_Data_Stmt)
	var_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Create_Data_Stmt)
	lit_type := lit_stmt.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	var_type := var_stmt.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect(t, lit_stmt.type_dynamic)
	testing.expect(t, var_stmt.type_dynamic)
	testing.expect(t, lit_type.raw_operand)
	testing.expect(t, var_type.is_ref)
	testing.expect(t, lit_type.is_ref)
	testing.expect_value(t, len(lit_type.raw_refs), 0)
	testing.expect_value(t, len(var_type.raw_refs), 1)
	testing.expect_value(t, var_type.raw_refs[0].name.text, "lv_class")
}

@(test)
create_data_table_dynamic_type_tracks_runtime_name_expr :: proc(t: ^testing.T) {
	source := `CREATE DATA lr_field TYPE STANDARD TABLE OF (<ls_table>-tobj_name).
CREATE DATA lr_var TYPE STANDARD TABLE OF (lv_primary).`
	parsed := parse(source, "create_data_dynamic_table_type.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	field_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Create_Data_Stmt)
	var_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Create_Data_Stmt)
	field_type := field_stmt.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	field_dynamic := field_stmt.type_dynamic_expr.derived_expr.(^ast.Type_Ref_Expr)
	var_type := var_stmt.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, field_stmt.type_clause.form, ast.Data_Type_Form.Standard_Table)
	testing.expect(t, field_stmt.type_dynamic)
	testing.expect(t, field_type.raw_operand)
	testing.expect_value(t, len(field_type.raw_refs), 1)
	testing.expect_value(t, field_type.raw_refs[0].name.text, "<ls_table>")
	testing.expect_value(t, field_type.raw_refs[0].path[0].name.text, "tobj_name")
	testing.expect_value(t, len(field_dynamic.raw_refs), 1)
	testing.expect_value(t, var_stmt.type_clause.form, ast.Data_Type_Form.Standard_Table)
	testing.expect_value(t, len(var_type.raw_refs), 1)
	testing.expect_value(t, var_type.raw_refs[0].name.text, "lv_primary")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
create_data_type_handle_tracks_value_operand :: proc(t: ^testing.T) {
	source := `CREATE DATA rr_data TYPE HANDLE lo_table.`
	parsed := parse(source, "create_data_type_handle.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Create_Data_Stmt)
	handle := stmt.type_handle.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect(t, stmt.type_ref == nil)
	testing.expect(t, stmt.type_clause == nil)
	testing.expect(t, handle.raw_operand)
	testing.expect_value(t, len(handle.raw_refs), 1)
	testing.expect_value(t, handle.raw_refs[0].name.text, "lo_table")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
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
