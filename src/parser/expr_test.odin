package abap_frontend_parser

import "src:ast"
import "src:tokenizer"

import "core:strings"
import "core:testing"

@(test)
expression_precedence_keeps_multiply_inside_add :: proc(t: ^testing.T) {
	parsed := parse("lv = a + b * c.", "test.abap", context.allocator)

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
parenthesized_mod_arithmetic_keeps_closing_delimiters_balanced :: proc(t: ^testing.T) {
	source := `lv2 = ( 10
      - ( ( ( 3
              * ( lv1+1(1)
                + lv1+3(1)
                + lv1+5(1) ) )
            + ( lv1(1)
              + lv1+2(1)
              + lv1+4(1) ) ) MOD 10 ) ) MOD 10.`
	parsed := parse(source, "check_digit.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
}

@(test)
selector_chains_build_nested_selector_nodes :: proc(t: ^testing.T) {
	parsed := parse("lv = lo_obj->mo_child=>gc_value~part-name.", "test.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.selector, 2)
	testing.expect_value(t, counts.interface_qualified_selector, 1)
}

@(test)
interface_qualified_selectors_keep_receiver_interface_and_member :: proc(t: ^testing.T) {
	parsed := parse("lv = ix_error->if_t100_message~t100key-msgid.", "test.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	msgid := assign.rhs.derived_expr.(^ast.Selector_Expr)
	t100key := msgid.base.derived_expr.(^ast.Interface_Qualified_Selector_Expr)
	receiver := t100key.receiver.derived_expr.(^ast.Ident_Expr)
	iface := t100key.interface.derived_expr.(^ast.Ident_Expr)
	member := t100key.member.derived_expr.(^ast.Ident_Expr)

	testing.expect_value(t, receiver.name, "ix_error")
	testing.expect_value(t, t100key.receiver_op, ast.Selector_Op.Arrow)
	testing.expect_value(t, iface.name, "if_t100_message")
	testing.expect_value(t, member.name, "t100key")
	testing.expect_value(t, ast.print_node(assign.rhs, context.allocator), "ix_error->if_t100_message~t100key-msgid")
}

@(test)
interface_qualified_selector_requires_identifier_shape :: proc(t: ^testing.T) {
	parsed := parse("lv = ix_error->if_t100_message~*.", "test.abap", context.allocator)

	expect_error_contains(t, parsed, "interface-qualified selector must be receiver->interface~member")
}

@(test)
table_expression_keeps_table_and_selector_shape :: proc(t: ^testing.T) {
	parsed := parse("lv = itab[ table_line = 'X' ].", "test.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	table, ok := assign.rhs.derived_expr.(^ast.Table_Expr)
	testing.expect(t, ok)
	testing.expect_value(t, len(table.selectors), 1)
	_, is_binary := table.selectors[0].derived_expr.(^ast.Binary_Expr)
	testing.expect(t, is_binary)
}

@(test)
substring_offset_length_keeps_length_out_of_offset :: proc(t: ^testing.T) {
	source := `lv_a = lv_val+0(1).
lv_b = lv_val+lv_last(1).
lv_c = im_response_string+ls_match-offset(ls_match-length).
lv_d = lv_val+4(*).
lv_e = lv_val+lv_last.`
	parsed := parse(source, "substring_offsets.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)

	first := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt).rhs.derived_expr.(^ast.Substring_Expr)
	_, first_offset := first.offset.derived_expr.(^ast.Literal_Expr)
	_, first_length := first.length.derived_expr.(^ast.Literal_Expr)
	testing.expect(t, first_offset)
	testing.expect(t, first_length)

	second := parsed.root.stmts[1].derived_stmt.(^ast.Assign_Stmt).rhs.derived_expr.(^ast.Substring_Expr)
	_, second_offset := second.offset.derived_expr.(^ast.Ident_Expr)
	_, second_length := second.length.derived_expr.(^ast.Literal_Expr)
	testing.expect(t, second_offset)
	testing.expect(t, second_length)

	third := parsed.root.stmts[2].derived_stmt.(^ast.Assign_Stmt).rhs.derived_expr.(^ast.Substring_Expr)
	_, third_offset := third.offset.derived_expr.(^ast.Selector_Expr)
	_, third_length := third.length.derived_expr.(^ast.Selector_Expr)
	testing.expect(t, third_offset)
	testing.expect(t, third_length)

	fourth := parsed.root.stmts[3].derived_stmt.(^ast.Assign_Stmt).rhs.derived_expr.(^ast.Substring_Expr)
	_, fourth_offset := fourth.offset.derived_expr.(^ast.Literal_Expr)
	fourth_length, fourth_length_ok := fourth.length.derived_expr.(^ast.Literal_Expr)
	testing.expect(t, fourth_offset)
	testing.expect(t, fourth_length_ok)
	testing.expect_value(t, fourth_length.value, "*")

	fifth := parsed.root.stmts[4].derived_stmt.(^ast.Assign_Stmt).rhs.derived_expr.(^ast.Substring_Expr)
	_, fifth_offset := fifth.offset.derived_expr.(^ast.Ident_Expr)
	testing.expect(t, fifth_offset)
	testing.expect(t, fifth.length == nil)
}

@(test)
call_argument_sections_carry_parser_kinds :: proc(t: ^testing.T) {
	parsed := parse(
		`rv = foo( EXPORTING iv_in = lv_in IMPORTING ev_out = lv_out CHANGING cv_any = lv_any TABLES ct_rows = lt_rows RECEIVING rv_result = lv_result EXCEPTIONS failed = 1 ).`,
		"call_sections.abap",
		context.allocator,
	)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	call := assign.rhs.derived_expr.(^ast.Call_Expr)
	args := call.args.derived_expr.(^ast.Call_Arg_List_Expr)
	expected := [?]ast.Call_Arg_Section_Kind {
		.Exporting,
		.Importing,
		.Changing,
		.Tables,
		.Receiving,
		.Exceptions,
	}
	testing.expect_value(t, len(args.args), len(expected))
	for i in 0 ..< len(expected) {
		section := args.args[i].derived_expr.(^ast.Call_Arg_Section_Expr)
		testing.expect_value(t, section.kind, expected[i])
		testing.expect_value(t, len(section.args), 1)
	}
}

@(test)
call_argument_value_stops_before_operator_named_arg :: proc(t: ^testing.T) {
	source := `rv = lo_obj->add_message(
  EXPORTING
    msgguid = iv_msgguid
    ns = ls_pers_qmsg-queue_ns
  EXCEPTIONS
    already_exist = 1
    lock_failed = 2 ).`
	parsed := parse(source, "call_ns_arg.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	call := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt).rhs.derived_expr.(^ast.Call_Expr)
	args := call.args.derived_expr.(^ast.Call_Arg_List_Expr)
	exporting := args.args[0].derived_expr.(^ast.Call_Arg_Section_Expr)
	exceptions := args.args[1].derived_expr.(^ast.Call_Arg_Section_Expr)
	testing.expect_value(t, len(exporting.args), 2)
	testing.expect_value(t, len(exceptions.args), 2)
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
	parsed := parse("rv = |{ lv_amount }|.", "test.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.template, 1)
	testing.expect_value(t, counts.interpolation, 1)
	testing.expect_value(t, counts.format_spec, 0)
}

@(test)
print_node_reconstructs_whole_file_from_ast :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
lv = 1.`
	parsed := parse(source, "roundtrip.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
write_node_prints_individual_statement_without_source :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
lv = a + b.`
	parsed := parse(source, "stmt_source.abap", context.allocator)
	out := strings.builder_make(context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	ast.write_node(&out, parsed.root.stmts[1])
	testing.expect_value(t, strings.to_string(out), `lv = a + b.`)
}

@(test)
print_node_reconstructs_string_template_nodes :: proc(t: ^testing.T) {
	source := `rv = |Amount { lv_amount DECIMALS = 2 }|.`
	parsed := parse(source, "template_source.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	template := assign.rhs.derived_expr.(^ast.Char_String_Template_Expr)
	interpolation := template.parts[1].derived_expr.(^ast.Template_Interpolation_Expr)
	format_spec := interpolation.format_specs[0].derived_expr.(^ast.Template_Format_Spec_Expr)
	option, has_option := format_spec.option.?
	testing.expect_value(t, ast.print_node(template, context.allocator), `|Amount { lv_amount DECIMALS = 2 }|`)
	testing.expect_value(t, ast.print_node(interpolation, context.allocator), `{ lv_amount DECIMALS = 2 }`)
	testing.expect(t, has_option)
	testing.expect_value(t, option, ast.Template_Format_Option.Decimals)
}

@(test)
string_template_format_specs_keep_name_tokens :: proc(t: ^testing.T) {
	source := `DATA(lv_text) = |Value { lv_value WIDTH = 5 ALIGN = LEFT }|.`
	parsed := parse(source, "template_format_tokens.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.format_spec, 2)

	decl := parsed.root.stmts[0].derived_stmt.(^ast.Data_Inline_Decl)
	template := decl.expr.derived_expr.(^ast.Char_String_Template_Expr)
	interpolation := template.parts[1].derived_expr.(^ast.Template_Interpolation_Expr)
	width := interpolation.format_specs[0].derived_expr.(^ast.Template_Format_Spec_Expr)
	align := interpolation.format_specs[1].derived_expr.(^ast.Template_Format_Spec_Expr)
	width_start := strings.index(source, "WIDTH")
	align_start := strings.index(source, "ALIGN")

	testing.expect_value(t, width.name.text, "WIDTH")
	testing.expect_value(t, width.name.range, tokenizer.text_range(width_start, width_start + len("WIDTH")))
	testing.expect_value(t, source[width.name.range.start:width.name.range.end], "WIDTH")
	testing.expect_value(t, align.name.text, "ALIGN")
	testing.expect_value(t, align.name.range, tokenizer.text_range(align_start, align_start + len("ALIGN")))
	testing.expect_value(t, source[align.name.range.start:align.name.range.end], "ALIGN")
	testing.expect_value(t, ast.print_node(template, context.allocator), `|Value { lv_value WIDTH = 5 ALIGN = LEFT }|`)
	testing.expect_value(t, ast.print_node(interpolation, context.allocator), `{ lv_value WIDTH = 5 ALIGN = LEFT }`)
}

@(test)
template_interpolation_extracts_decimals_and_width_specs :: proc(t: ^testing.T) {
	parsed := parse("rv = |Amount { lv_amount DECIMALS = 2 WIDTH = 12 }|.", "test.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.interpolation, 1)
	testing.expect_value(t, counts.format_spec, 2)
}

@(test)
template_interpolation_accepts_multiline_value_constructor_optional :: proc(t: ^testing.T) {
	source := `rv = | { VALUE #( mt_trn[ bizttype = 60 ]-docnum
       OPTIONAL ) ALPHA = OUT } |.`
	parsed := parse(source, "test.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.interpolation, 1)
	testing.expect_value(t, counts.constructor, 1)
	testing.expect_value(t, counts.format_spec, 1)
}

@(test)
constructor_expr_keeps_kind_enum :: proc(t: ^testing.T) {
	parsed := parse("rv = VALUE #( 1 ).", "constructor_kind.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	assign := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	constructor := assign.rhs.derived_expr.(^ast.Constructor_Expr)
	testing.expect_value(t, constructor.kind, ast.Constructor_Kind.Value)
}

@(test)
logical_predicates_build_dedicated_nodes :: proc(t: ^testing.T) {
	source := `IF oref IS INSTANCE OF cl_foo OR lv BETWEEN 1 AND max_v.
ENDIF.`
	parsed := parse(source, "predicates.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.instance_of, 1)
	testing.expect_value(t, counts.between_expr, 1)
}

@(test)
line_exists_and_is_not_initial_parse_in_logical_condition :: proc(t: ^testing.T) {
	source := `IF NOT line_exists( lt_rep_evt[ rule_type = lc_rs_comm ] ) AND lt_obj_comm IS NOT INITIAL.
ENDIF.`
	parsed := parse(source, "line_exists.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.is_predicate, 1)
	testing.expect_value(t, counts.table, 1)
}

@(test)
value_constructor_builds_base_for_and_assignment_clauses :: proc(t: ^testing.T) {
	source := `DATA(lt_new) = VALUE #( BASE lt_base FOR ls_obj IN mt_obj_ids ( objid = ls_obj-objid ) ).`
	parsed := parse(source, "value_for.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor, 1)
	testing.expect_value(t, counts.constructor_base, 1)
	testing.expect_value(t, counts.constructor_for, 1)
	testing.expect_value(t, counts.constructor_named, 1)
}

@(test)
value_constructor_allows_for_in_group_clause :: proc(t: ^testing.T) {
	source := `DATA(lt_new) = VALUE #( FOR ls_obj IN GROUP lg_obj ( objid = ls_obj-objid ) ).`
	parsed := parse(source, "value_for_group.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor_for, 1)
	testing.expect_value(t, counts.constructor_for_group, 1)
}

@(test)
value_constructor_allows_component_path_assignment_names :: proc(t: ^testing.T) {
	source := `lt_decode = VALUE #( ( obj_code-code_char = |{ '(00)' }{ is_resp_stru-kod } | code_type = 'C' ) ).`
	parsed := parse(source, "value_component_path.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor_named, 2)
}

@(test)
reduce_constructor_builds_init_for_and_next_clauses :: proc(t: ^testing.T) {
	source := `DATA(lv_rep) = REDUCE i( INIT x = 0 FOR wa IN lt_rep NEXT x = x + wa ).`
	parsed := parse(source, "reduce.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor_init, 1)
	testing.expect_value(t, counts.constructor_for, 1)
	testing.expect_value(t, counts.constructor_next, 1)
	testing.expect_value(t, counts.constructor_named, 2)
}

@(test)
reduce_constructor_allows_for_until_without_then :: proc(t: ^testing.T) {
	source := "DATA(lv_res1) = REDUCE string( INIT text = `Count up:`\n" +
	          "                               FOR n = 1 UNTIL n > 10\n" +
	          "                               NEXT text = text && | { n }| )."
	parsed := parse(source, "reduce_for_until.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	decl := parsed.root.stmts[0].derived_stmt.(^ast.Data_Inline_Decl)
	constructor := decl.expr.derived_expr.(^ast.Constructor_Expr)
	for_clause := constructor.args[1].derived_expr.(^ast.Constructor_For_Clause_Expr)

	testing.expect_value(t, for_clause.kind, ast.Constructor_For_Kind.For_Then_Until)
	testing.expect(t, for_clause.then_expr == nil)
	testing.expect_value(t, len(for_clause.body), 1)
	_, next_ok := for_clause.body[0].derived_expr.(^ast.Constructor_Next_Clause_Expr)
	testing.expect(t, next_ok)

	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor_init, 1)
	testing.expect_value(t, counts.constructor_for, 1)
	testing.expect_value(t, counts.constructor_next, 1)
	testing.expect_value(t, counts.constructor_named, 2)
	testing.expect_value(t, counts.template, 1)
	testing.expect_value(t, counts.interpolation, 1)
}

@(test)
filter_constructor_accepts_using_key_before_where :: proc(t: ^testing.T) {
	source := `DATA(lt_filtered) = FILTER #( lt_rows USING KEY primary_key WHERE id = lv_id ).`
	parsed := parse(source, "filter_using_key.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	decl := parsed.root.stmts[0].derived_stmt.(^ast.Data_Inline_Decl)
	constructor := decl.expr.derived_expr.(^ast.Constructor_Expr)
	testing.expect_value(t, constructor.kind, ast.Constructor_Kind.Filter)
	testing.expect_value(t, len(constructor.args), 2)
	_, source_ok := constructor.args[0].derived_expr.(^ast.Ident_Expr)
	_, where_ok := constructor.args[1].derived_expr.(^ast.Constructor_Where_Clause_Expr)
	testing.expect(t, source_ok)
	testing.expect(t, where_ok)
}

@(test)
cond_constructor_builds_let_when_and_else_clauses :: proc(t: ^testing.T) {
	source := `DATA(lv_text) = COND string( LET t = '120000' IN WHEN sy-timlo < t THEN |AM| ELSE |PM| ).`
	parsed := parse(source, "cond.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.let_expr, 1)
	testing.expect_value(t, counts.constructor_when, 1)
	testing.expect_value(t, counts.constructor_else, 1)
}

@(test)
cond_constructor_allows_let_as_when_result :: proc(t: ^testing.T) {
	source := `lt_ext_pda_data = COND #(
  WHEN is_response-success = 'false'
  THEN LET ls_extpda1 = VALUE zattp_rs_extpda1( status = TEXT-005 )
           lv_update_pda1 = update_zattp_rs_extpda1( is_rs_extpda1 = ls_extpda1 )
       IN VALUE #( ( ext_pda_fail_flag = abap_true extpda1 = VALUE zattp_tt_rs_extpda1( ( ls_extpda1 ) ) ) )
  ELSE VALUE #( ) ).`
	parsed := parse(source, "cond_then_let.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.let_expr, 1)
	testing.expect_value(t, counts.constructor_when, 1)
}

@(test)
filter_constructor_allows_except_in_where :: proc(t: ^testing.T) {
	source := `DATA(lt_obj_dif) = FILTER #( lt_child_obj
  EXCEPT IN lt_rep_rel_obj
  WHERE objid = objid ).`
	parsed := parse(source, "filter_except_in.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor, 1)
	decl := parsed.root.stmts[0].derived_stmt.(^ast.Data_Inline_Decl)
	constructor := decl.expr.derived_expr.(^ast.Constructor_Expr)
	testing.expect_value(t, len(constructor.args), 2)
	_, source_ok := constructor.args[0].derived_expr.(^ast.Ident_Expr)
	except_in := constructor.args[1].derived_expr.(^ast.Constructor_Filter_Except_In_Clause_Expr)
	where_clause := except_in.where_clause.derived_expr.(^ast.Constructor_Where_Clause_Expr)
	testing.expect(t, source_ok)
	testing.expect(t, except_in.source != nil)
	_, condition_parenthesized := where_clause.condition.derived_expr.(^ast.Paren_Expr)
	testing.expect(t, !condition_parenthesized)
}

@(test)
filter_constructor_rejects_parenthesized_except_in_where :: proc(t: ^testing.T) {
	source := `DATA(lt_obj_dif) = FILTER #( lt_child_obj
  EXCEPT IN lt_rep_rel_obj
  WHERE ( objid = objid ) ).`
	parsed := parse(source, "filter_except_in_parenthesized_where.abap", context.allocator)

	expect_error_contains(t, parsed, "FILTER EXCEPT IN WHERE clause does not allow parentheses")
}

@(test)
corresponding_constructor_builds_mapping_and_except_clauses :: proc(t: ^testing.T) {
	source := `DATA(ls_dst) = CORRESPONDING #( ls_src MAPPING dst_field = src_field fallback = DEFAULT lv_fallback ( child = child MAPPING dst_nested = src_nested EXCEPT spare ) EXCEPT unused ).`
	parsed := parse(source, "corresponding.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor_mapping, 2)
	testing.expect_value(t, counts.constructor_mapping_assignment, 4)
	testing.expect_value(t, counts.constructor_except, 2)
}

@(test)
nested_template_and_double_ampersand_parse_as_expressions :: proc(t: ^testing.T) {
	source := `rv_text = |prefix { |{ mv_inner }| } { mo_left->to_string( ) && mv_op }|.`
	parsed := parse(source, "template_concat.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.template, 2)
	testing.expect_value(t, counts.interpolation, 3)
	testing.expect(t, counts.binary >= 1)
}

@(test)
invalid_between_condition_stays_diagnostic_not_success :: proc(t: ^testing.T) {
	source := `IF lv BETWEEN 1 max.
ENDIF.`
	parsed := parse(source, "bad_between.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
	expect_error_contains(t, parsed, "expected keyword")
}
