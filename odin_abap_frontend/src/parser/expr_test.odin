package abap_frontend_parser

import "../ast"
import "../tokenizer"

import "base:runtime"
import "core:strings"
import "core:testing"

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
logical_predicates_build_dedicated_nodes :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `IF oref IS INSTANCE OF cl_foo OR lv BETWEEN 1 AND max_v.
ENDIF.`
	parsed := parse(source, "predicates.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.instance_of, 1)
	testing.expect_value(t, counts.between_expr, 1)
}

@(test)
line_exists_and_is_not_initial_parse_in_logical_condition :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `IF NOT line_exists( lt_rep_evt[ rule_type = lc_rs_comm ] ) AND lt_obj_comm IS NOT INITIAL.
ENDIF.`
	parsed := parse(source, "line_exists.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.is_predicate, 1)
	testing.expect_value(t, counts.table, 1)
}

@(test)
value_constructor_builds_base_for_and_assignment_clauses :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `DATA(lt_new) = VALUE #( BASE lt_base FOR ls_obj IN mt_obj_ids ( objid = ls_obj-objid ) ).`
	parsed := parse(source, "value_for.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor, 1)
	testing.expect_value(t, counts.constructor_base, 1)
	testing.expect_value(t, counts.constructor_for, 1)
	testing.expect_value(t, counts.constructor_named, 1)
}

@(test)
reduce_constructor_builds_init_for_and_next_clauses :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `DATA(lv_rep) = REDUCE i( INIT x = 0 FOR wa IN lt_rep NEXT x = x + wa ).`
	parsed := parse(source, "reduce.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor_init, 1)
	testing.expect_value(t, counts.constructor_for, 1)
	testing.expect_value(t, counts.constructor_next, 1)
	testing.expect_value(t, counts.constructor_named, 2)
}

@(test)
cond_constructor_builds_let_when_and_else_clauses :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `DATA(lv_text) = COND string( LET t = '120000' IN WHEN sy-timlo < t THEN |AM| ELSE |PM| ).`
	parsed := parse(source, "cond.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.let_expr, 1)
	testing.expect_value(t, counts.constructor_when, 1)
	testing.expect_value(t, counts.constructor_else, 1)
}

@(test)
corresponding_constructor_builds_mapping_and_except_clauses :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `DATA(ls_dst) = CORRESPONDING #( ls_src MAPPING dst_field = src_field fallback = DEFAULT lv_fallback ( child = child MAPPING dst_nested = src_nested EXCEPT spare ) EXCEPT unused ).`
	parsed := parse(source, "corresponding.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.constructor_mapping, 2)
	testing.expect_value(t, counts.constructor_mapping_assignment, 4)
	testing.expect_value(t, counts.constructor_except, 2)
}

@(test)
nested_template_and_double_ampersand_parse_as_expressions :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `rv_text = |prefix { |{ mv_inner }| } { mo_left->to_string( ) && mv_op }|.`
	parsed := parse(source, "template_concat.abap", alloc)

	testing.expect_value(t, len(parsed.errors), 0)
	counts := count_nodes(parsed.root)
	testing.expect_value(t, counts.template, 2)
	testing.expect_value(t, counts.interpolation, 3)
	testing.expect(t, counts.binary >= 1)
}

@(test)
invalid_between_condition_stays_diagnostic_not_success :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `IF lv BETWEEN 1 max.
ENDIF.`
	parsed := parse(source, "bad_between.abap", alloc)

	testing.expect(t, len(parsed.errors) > 0)
	expect_error_contains(t, parsed, "expected keyword")
}
