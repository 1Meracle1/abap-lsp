package abap_frontend_vm_runtime

import ir "src:ir"

import "core:strings"
import "core:testing"

test_type_descriptor :: proc(
	name: string,
	family: ir.Runtime_Type_Family,
	text_kind: ir.Runtime_Text_Kind = .None,
	preserves_trailing_blanks: bool = false,
) -> ir.Runtime_Type_Descriptor {
	return ir.Runtime_Type_Descriptor {
		family = family,
		display_name = name,
		elementary = ir.Runtime_Elementary_Descriptor {
			text_kind = text_kind,
			preserves_trailing_blanks = preserves_trailing_blanks,
		},
	}
}

test_reference_descriptor :: proc(
	name: string,
	kind: ir.Runtime_Reference_Kind,
) -> ir.Runtime_Type_Descriptor {
	return ir.Runtime_Type_Descriptor {
		family = .Reference,
		display_name = name,
		reference = ir.Runtime_Reference_Descriptor {
			kind = kind,
			target_name = name,
		},
	}
}

@(test)
context_keeps_values_and_captures_output :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	context_global_write(&ctx, "gv_total", value_integer_make(3))
	sy := context_runtime_read(&ctx, "sy")
	testing.expect(t, context_field_store(&ctx, Field_Request {
		base = sy,
		name = "subrc",
		value = value_integer_make(4),
	}))

	total := context_global_read(&ctx, "gv_total")
	testing.expect_value(t, value_kind(total), Value_Kind.Integer)
	testing.expect_value(t, value_int(total), i64(3))

	subrc, subrc_ok := context_field_load(&ctx, Field_Request{base = sy, name = "subrc"})
	testing.expect(t, subrc_ok)
	testing.expect_value(t, value_kind(subrc), Value_Kind.Integer)
	testing.expect_value(t, value_int(subrc), i64(4))

	values := [?]Value{total, subrc}
	testing.expect(t, context_write(&ctx, values[:]))
	testing.expect_value(t, len(ctx.events), 1)
	if len(ctx.events) > 0 {
		testing.expect_value(t, ctx.events[0].kind, IO_Event_Kind.Write)
		testing.expect_value(t, ctx.events[0].text, "3 4")
	}
}

@(test)
context_io_policy_can_explicitly_deny_all_output :: proc(t: ^testing.T) {
	ctx := context_make(Context_Options{io_policy = IO_Policy{kind = .Deny_All}}, context.allocator)
	defer context_destroy(&ctx)
	value := value_string("denied", context.allocator)
	defer value_destroy(&value)
	testing.expect(t, !context_write(&ctx, []Value{value}))
	testing.expect_value(t, ctx.trap.kind, Trap_Kind.Unsupported)
}

@(test)
sql_without_a_host_service_traps_as_unsupported :: proc(t: ^testing.T) {
	ctx := context_make(allocator = context.allocator)
	defer context_destroy(&ctx)
	_, _, ok := context_sql_select(&ctx)
	testing.expect(t, !ok)
	testing.expect_value(t, ctx.trap.kind, Trap_Kind.Unsupported)
}

@(test)
context_system_values_use_canonical_field_names :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	context_system_write(&ctx, "subrc", value_integer_make(8))
	subrc := context_system_read(&ctx, "subrc")
	testing.expect_value(t, value_kind(subrc), Value_Kind.Integer)
	testing.expect_value(t, value_int(subrc), i64(8))
}

@(test)
context_exception_helpers_match_catch_and_report_unhandled :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	testing.expect(t, context_exception_raise(&ctx, "lcx_error"))
	testing.expect(t, context_exception_matches(&ctx, "lcx_error"))
	testing.expect(t, context_exception_matches(&ctx, "cx_root"))
	testing.expect(t, !context_exception_matches(&ctx, "lcx_other"))

	catch_type := test_reference_descriptor("ref:cx_root", .Class)
	caught, caught_ok := context_exception_catch(&ctx, &catch_type, context.allocator)
	defer value_destroy(&caught)
	testing.expect(t, caught_ok)
	testing.expect_value(t, value_kind(caught), Value_Kind.Object)
	testing.expect(t, !context_exception_matches(&ctx, "cx_root"))

	testing.expect(t, context_exception_raise(&ctx, "lcx_unhandled"))
	testing.expect(t, !context_exception_unhandled(&ctx))
	testing.expect_value(t, ctx.trap.kind, Trap_Kind.Exception)
	testing.expect(t, strings.contains(ctx.trap.message, "lcx_unhandled"))
}

@(test)
literal_conversion_treats_abap_character_types_as_text :: proc(t: ^testing.T) {
	char_type := test_type_descriptor("c", .Text, .Fixed)
	bare := value_from_literal("'I'", &char_type, context.allocator)
	defer value_destroy(&bare)
	testing.expect_value(t, value_kind(bare), Value_Kind.String)
	testing.expect_value(t, value_text(bare), "I")

	sized_char_type := test_type_descriptor("c(10)", .Text, .Fixed)
	sized := value_from_literal("'hello'", &sized_char_type, context.allocator)
	defer value_destroy(&sized)
	testing.expect_value(t, value_kind(sized), Value_Kind.String)
	testing.expect_value(t, value_text(sized), "hello")
}

@(test)
references_read_and_write_cells_and_globals :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	context_global_write(&ctx, "gv_total", value_integer_make(3))
	global_ref := value_alias_global(&ctx, "gv_total", context.allocator)
	defer value_destroy(&global_ref)
	testing.expect_value(t, value_kind(global_ref), Value_Kind.Integer)
	testing.expect_value(t, value_int(global_ref), i64(3))

	global_data := value_reference_data(global_ref)
	testing.expect(t, global_data != nil)
	if global_data != nil {
		testing.expect(t, reference_write(global_data, value_integer_make(4)))
	}
	testing.expect_value(t, value_int(context_global_read(&ctx, "gv_total")), i64(4))

	cell := cell_make(value_string("A", context.allocator), context.allocator)
	defer cell_release(cell)
	cell_ref := value_alias_cell(cell, context.allocator)
	defer value_destroy(&cell_ref)
	testing.expect_value(t, value_text(cell_ref), "A")
	cell_data := value_reference_data(cell_ref)
	testing.expect(t, cell_data != nil)
	if cell_data != nil {
		replacement := value_string("B", context.allocator)
		defer value_destroy(&replacement)
		testing.expect(t, reference_write(cell_data, replacement))
	}
	testing.expect_value(t, value_text(cell.value), "B")
}

@(test)
abap_helpers_cover_current_scalar_semantics :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	integer_type := test_type_descriptor("i", .Integer)
	integer_type.elementary.bits = 32
	integer_type.elementary.signed = true
	sum, sum_ok := abap_numeric_arithmetic(&ctx, .Add, value_integer_make(1), value_integer_make(2), &integer_type)
	testing.expect(t, sum_ok)
	testing.expect_value(t, value_kind(sum), Value_Kind.Integer)
	testing.expect_value(t, value_int(sum), i64(3))

	equal, equal_ok := abap_compare(&ctx, .Equal, sum, value_integer_make(3))
	testing.expect(t, equal_ok)
	testing.expect_value(t, value_kind(equal), Value_Kind.Predicate)
	testing.expect_value(t, value_int(equal), i64(1))
}

@(test)
abap_numeric_arithmetic_preserves_decimal_and_float_values :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)
	decimal_type := ir.Runtime_Type_Descriptor {
		family = .Decimal,
		display_name = "p(8,2)",
		elementary = ir.Runtime_Elementary_Descriptor{length = 8, has_length = true, decimals = 2, has_decimals = true},
	}
	left, left_ok := value_decimal_parse("10.25")
	right, right_ok := value_decimal_parse("2.00")
	testing.expect(t, left_ok && right_ok)
	product, product_ok := abap_numeric_arithmetic(&ctx, .Multiply, left, right, &decimal_type)
	testing.expect(t, product_ok)
	product_text, product_text_ok := value_scalar_text(product, context.allocator)
	defer delete(product_text)
	testing.expect(t, product_text_ok)
	testing.expect_value(t, product_text, "20.50")

	quotient, quotient_ok := abap_numeric_arithmetic(&ctx, .Divide, left, right, &decimal_type)
	testing.expect(t, quotient_ok)
	quotient_text, quotient_text_ok := value_scalar_text(quotient, context.allocator)
	defer delete(quotient_text)
	testing.expect(t, quotient_text_ok)
	testing.expect_value(t, quotient_text, "5.13")

	integer_type := test_type_descriptor("i", .Integer)
	integer_type.elementary.bits = 32
	integer_type.elementary.signed = true
	integer_divide, integer_divide_ok := abap_numeric_arithmetic(&ctx, .Integer_Divide, value_integer_make(11), value_integer_make(4), &integer_type)
	testing.expect(t, integer_divide_ok)
	testing.expect_value(t, value_int(integer_divide), i64(2))
	modulo, modulo_ok := abap_numeric_arithmetic(&ctx, .Modulo, value_integer_make(11), value_integer_make(4), &integer_type)
	testing.expect(t, modulo_ok)
	testing.expect_value(t, value_int(modulo), i64(3))

	float_type := ir.Runtime_Type_Descriptor{family = .Float, display_name = "f"}
	float_sum, float_sum_ok := abap_numeric_arithmetic(&ctx, .Add, value_float_make(1.5), value_integer_make(2), &float_type)
	testing.expect(t, float_sum_ok)
	float_value, float_ok := value_float(float_sum)
	testing.expect(t, float_ok)
	testing.expect_value(t, float_value, 3.5)
}

@(test)
numeric_arithmetic_traps_target_overflow :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)
	int1_type := ir.Runtime_Type_Descriptor {
		family = .Integer,
		display_name = "int1",
		elementary = ir.Runtime_Elementary_Descriptor{bits = 8, signed = false},
	}
	_, integer_ok := abap_numeric_arithmetic(&ctx, .Add, value_integer_make(255), value_integer_make(1), &int1_type)
	testing.expect(t, !integer_ok)
	testing.expect_value(t, ctx.trap.kind, Trap_Kind.Overflow)
	context_trap(&ctx, .None, "")

	packed_type := ir.Runtime_Type_Descriptor {
		family = .Decimal,
		display_name = "p(2,0)",
		elementary = ir.Runtime_Elementary_Descriptor{length = 2, has_length = true, decimals = 0, has_decimals = true},
	}
	_, decimal_ok := abap_numeric_arithmetic(&ctx, .Add, value_decimal_make(999, 0), value_decimal_make(1, 0), &packed_type)
	testing.expect(t, !decimal_ok)
	testing.expect_value(t, ctx.trap.kind, Trap_Kind.Overflow)
}

@(test)
abap_helpers_cover_statement_string_semantics :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	values := [?]Value{
		value_string("A", context.allocator),
		value_string("B", context.allocator),
		value_string("-", context.allocator),
	}
		defer value_destroy(&values[0])
		defer value_destroy(&values[1])
		defer value_destroy(&values[2])
		string_type := test_type_descriptor("string", .Text, .String, true)
		string_types := [?]Type_Descriptor{&string_type, &string_type}
		concatenated, concatenated_ok := abap_concatenate(&ctx, values[:], string_types[:], true, false, context.allocator)
		defer value_destroy(&concatenated)
		testing.expect(t, concatenated_ok)
		testing.expect_value(t, value_text(concatenated), "A-B")

		char_values := [?]Value{
			value_string("A  ", context.allocator),
			value_string("B ", context.allocator),
			value_string("- ", context.allocator),
		}
		defer value_destroy(&char_values[0])
		defer value_destroy(&char_values[1])
		defer value_destroy(&char_values[2])
		char_type := test_type_descriptor("c", .Text, .Fixed, false)
		char_types := [?]Type_Descriptor{&char_type, &char_type}
		trimmed, trimmed_ok := abap_concatenate(&ctx, char_values[:], char_types[:], true, false, context.allocator)
		defer value_destroy(&trimmed)
		testing.expect(t, trimmed_ok)
		testing.expect_value(t, value_text(trimmed), "A- B")

		respected, respected_ok := abap_concatenate(&ctx, char_values[:], char_types[:], true, true, context.allocator)
		defer value_destroy(&respected)
		testing.expect(t, respected_ok)
		testing.expect_value(t, value_text(respected), "A  - B ")

		wide := value_string("  A   B  ", context.allocator)
	defer value_destroy(&wide)
	condensed, condensed_ok := abap_condense(&ctx, wide, false, context.allocator)
	defer value_destroy(&condensed)
	testing.expect(t, condensed_ok)
	testing.expect_value(t, value_text(condensed), "A B")

	no_gaps, no_gaps_ok := abap_condense(&ctx, wide, true, context.allocator)
	defer value_destroy(&no_gaps)
	testing.expect(t, no_gaps_ok)
	testing.expect_value(t, value_text(no_gaps), "AB")

	mixed := value_string("AbC", context.allocator)
	defer value_destroy(&mixed)
	lower, lower_ok := abap_translate(&ctx, .To_Lower, mixed, context.allocator)
	defer value_destroy(&lower)
	testing.expect(t, lower_ok)
	testing.expect_value(t, value_text(lower), "abc")

	split_source := value_string("AA-BB-CC-DD", context.allocator)
	defer value_destroy(&split_source)
	split_separator := value_string("-", context.allocator)
	defer value_destroy(&split_separator)
	split_values, split_ok := abap_split(&ctx, split_source, split_separator, 3, context.allocator)
	testing.expect(t, split_ok)
	testing.expect_value(t, len(split_values), 3)
	if len(split_values) == 3 {
		defer value_destroy(&split_values[0])
		defer value_destroy(&split_values[1])
		defer value_destroy(&split_values[2])
		testing.expect_value(t, value_text(split_values[0]), "AA")
		testing.expect_value(t, value_text(split_values[1]), "BB")
		testing.expect_value(t, value_text(split_values[2]), "CC-DD")
	}

	replace_source := value_string("abap ABAP abap", context.allocator)
	defer value_destroy(&replace_source)
	replace_pattern := value_string("ab", context.allocator)
	defer value_destroy(&replace_pattern)
	replace_value := value_string("##", context.allocator)
	defer value_destroy(&replace_value)
	replaced, replaced_ok := abap_replace(&ctx, .All, replace_source, replace_pattern, replace_value, context.allocator)
	defer value_destroy(&replaced)
	testing.expect(t, replaced_ok)
	testing.expect_value(t, value_text(replaced), "##ap ABAP ##ap")

	shift_source := value_string("hallo", context.allocator)
	defer value_destroy(&shift_source)
	shift_left, shift_left_ok := abap_shift(&ctx, .Left, shift_source, value_integer_make(2), context.allocator)
	defer value_destroy(&shift_left)
	testing.expect(t, shift_left_ok)
	testing.expect_value(t, value_text(shift_left), "llo")
	shift_right, shift_right_ok := abap_shift(&ctx, .Right, shift_source, value_integer_make(3), context.allocator)
	defer value_destroy(&shift_right)
	testing.expect(t, shift_right_ok)
	testing.expect_value(t, value_text(shift_right), "   hallo")

	find_pattern := value_string("alpha", context.allocator)
	defer value_destroy(&find_pattern)
	find_target := value_string("Alpha beta alpha", context.allocator)
	defer value_destroy(&find_target)
	find_result, find_ok := abap_find(&ctx, .All, true, find_pattern, find_target)
	testing.expect(t, find_ok)
	testing.expect_value(t, find_result.subrc, i64(0))
	testing.expect_value(t, find_result.offset, i64(11))
	testing.expect_value(t, find_result.length, i64(5))
	testing.expect_value(t, find_result.count, i64(2))

	search_pattern := value_string("BETA", context.allocator)
	defer value_destroy(&search_pattern)
	search_result, search_ok := abap_search(&ctx, find_target, search_pattern)
	testing.expect(t, search_ok)
	testing.expect_value(t, search_result.subrc, i64(0))
	testing.expect_value(t, search_result.fdpos, i64(6))

	missing_pattern := value_string("missing", context.allocator)
	defer value_destroy(&missing_pattern)
	missing_result, missing_ok := abap_search(&ctx, find_target, missing_pattern)
	testing.expect(t, missing_ok)
	testing.expect_value(t, missing_result.subrc, i64(4))
	testing.expect_value(t, missing_result.fdpos, i64(16))
}

@(test)
object_references_are_heap_allocated_and_reference_counted :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	object_ref_type := test_reference_descriptor("ref:zagent", .Class)
	initial := initial_for_type(&object_ref_type, context.allocator)
	testing.expect_value(t, value_kind(initial), Value_Kind.Initial)

	object, object_ok := abap_construct(&ctx, "new", nil, &object_ref_type)
	defer value_destroy(&object)
	testing.expect(t, object_ok)
	testing.expect_value(t, value_kind(object), Value_Kind.Object)
	object_data := value_object_data(object)
	testing.expect(t, object_data != nil)
	if object_data != nil {
		testing.expect_value(t, object_data.refs, 1)
		testing.expect_value(t, object_data.type_name, "ref:zagent")
	}

	clone := value_clone(object, context.allocator)
	testing.expect_value(t, value_kind(clone), Value_Kind.Object)
	if object_data != nil {
		testing.expect_value(t, object_data.refs, 2)
	}
	value_destroy(&clone)
	if object_data != nil {
		testing.expect_value(t, object_data.refs, 1)
	}

	testing.expect(t, context_field_store(&ctx, Field_Request {
		base = object,
		name = "mv_total",
		value = value_integer_make(7),
	}))
	int_type := test_type_descriptor("i", .Integer)
	total, total_ok := context_field_load(&ctx, Field_Request {
		base = object,
		name = "mv_total",
		result_type = &int_type,
	})
	defer value_destroy(&total)
	testing.expect(t, total_ok)
	testing.expect_value(t, value_kind(total), Value_Kind.Integer)
	testing.expect_value(t, value_int(total), i64(7))
}

@(test)
table_helpers_read_and_mutate_values_directly :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	table := value_table(context.allocator)
	defer value_destroy(&table)
	row := value_integer_make(7)
	values := [?]Value{row, table}
	append_result, append_ok := context_table_mutate(&ctx, Table_Request{operation = .Append, values = values[:]})
	defer value_destroy(&append_result.subrc)
	defer value_destroy(&append_result.tabix)
	testing.expect(t, append_ok)
	testing.expect_value(t, value_int(append_result.subrc), i64(0))
	testing.expect_value(t, value_int(append_result.tabix), i64(1))

	read_values := [?]Value{table, value_integer_make(1)}
	int_type := test_type_descriptor("i", .Integer)
	read_result, read_ok := context_table_read(
		&ctx,
		Table_Request{operation = .Read, values = read_values[:], result_type = &int_type},
	)
	defer value_destroy(&read_result.row)
	defer value_destroy(&read_result.subrc)
	defer value_destroy(&read_result.tabix)
	testing.expect(t, read_ok)
	testing.expect_value(t, value_kind(read_result.row), Value_Kind.Integer)
	testing.expect_value(t, value_int(read_result.row), i64(7))
	testing.expect_value(t, value_kind(read_result.subrc), Value_Kind.Integer)
	testing.expect_value(t, value_int(read_result.subrc), i64(0))
	testing.expect_value(t, value_int(read_result.tabix), i64(1))
}

@(test)
structure_fields_and_table_rows_support_mixed_report_shapes :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	row_type := test_type_descriptor("ty_source", .Structure)
	string_type := test_type_descriptor("string", .Text, .String, true)
	row := initial_for_type(&row_type, context.allocator)
	defer value_destroy(&row)
	first_material := value_string("MAT-001", context.allocator)
	defer value_destroy(&first_material)
	testing.expect(t, context_field_store(&ctx, Field_Request{base = row, name = "matnr", value = first_material}))

	table := value_table(context.allocator)
	defer value_destroy(&table)
	append_values := [?]Value{row, table}
	append_result, append_ok := context_table_mutate(&ctx, Table_Request{operation = .Append, values = append_values[:]})
	defer value_destroy(&append_result.subrc)
	defer value_destroy(&append_result.tabix)
	testing.expect(t, append_ok)

	second_material := value_string("MAT-002", context.allocator)
	defer value_destroy(&second_material)
	testing.expect(t, context_field_store(&ctx, Field_Request{base = row, name = "matnr", value = second_material}))

	read_values := [?]Value{table, value_integer_make(1)}
	read_result, read_ok := context_table_read(
		&ctx,
		Table_Request{operation = .Read, values = read_values[:], result_type = &row_type},
	)
	defer value_destroy(&read_result.row)
	defer value_destroy(&read_result.subrc)
	defer value_destroy(&read_result.tabix)
	testing.expect(t, read_ok)
	testing.expect_value(t, value_kind(read_result.subrc), Value_Kind.Integer)
	testing.expect_value(t, value_int(read_result.subrc), i64(0))
	testing.expect_value(t, value_int(read_result.tabix), i64(1))

	material, material_ok := context_field_load(
		&ctx,
		Field_Request{base = read_result.row, name = "matnr", result_type = &string_type},
	)
	defer value_destroy(&material)
	testing.expect(t, material_ok)
	testing.expect_value(t, value_kind(material), Value_Kind.String)
	testing.expect_value(t, value_text(material), "MAT-001")
}

@(test)
table_helpers_match_components_mutate_and_sort_fields :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	table := value_table(context.allocator)
	defer value_destroy(&table)

	make_row :: proc(id: i64, text: string) -> Value {
		row := value_structure("ty_row", context.allocator)
		structure := value_structure_data(row)
		assert(structure != nil)
		id_value := value_integer_make(id)
		text_value := value_string(text, context.allocator)
		defer value_destroy(&text_value)
		structure_set_field(structure, "id", id_value)
		structure_set_field(structure, "text", text_value)
		return row
	}

	first := make_row(2, "b")
	defer value_destroy(&first)
	second := make_row(1, "a")
	defer value_destroy(&second)
	append_values := [?]Value{first, table}
	append_result, append_ok := context_table_mutate(&ctx, Table_Request{operation = .Append, values = append_values[:]})
	defer value_destroy(&append_result.subrc)
	defer value_destroy(&append_result.tabix)
	testing.expect(t, append_ok)
	append_values[0] = second
	append_result2, append_ok2 := context_table_mutate(&ctx, Table_Request{operation = .Append, values = append_values[:]})
	defer value_destroy(&append_result2.subrc)
	defer value_destroy(&append_result2.tabix)
	testing.expect(t, append_ok2)

	id_path := [?]string{"id"}
	key := value_integer_make(1)
	components := [?]Table_Component{{path = id_path[:], value = key}}
	read_values := [?]Value{table}
	row_type := test_type_descriptor("ty_row", .Structure)
	read_result, read_ok := context_table_read(
		&ctx,
		Table_Request{operation = .Read, values = read_values[:], result_type = &row_type, components = components[:]},
	)
	defer value_destroy(&read_result.row)
	defer value_destroy(&read_result.subrc)
	defer value_destroy(&read_result.tabix)
	testing.expect(t, read_ok)
	testing.expect_value(t, value_int(read_result.subrc), i64(0))
	testing.expect_value(t, value_int(read_result.tabix), i64(2))

	replacement := make_row(1, "z")
	defer value_destroy(&replacement)
	modify_values := [?]Value{replacement, table}
	mod_result, mod_ok := context_table_mutate(
		&ctx,
		Table_Request{operation = .Modify, values = modify_values[:], components = components[:]},
	)
	defer value_destroy(&mod_result.subrc)
	defer value_destroy(&mod_result.tabix)
	testing.expect(t, mod_ok)
	testing.expect_value(t, value_int(mod_result.subrc), i64(0))
	testing.expect_value(t, value_int(mod_result.tabix), i64(2))

	text_path := [?]string{"text"}
	sort_components := [?]Table_Sort_Component{{path = text_path[:], descending = true}}
	sort_values := [?]Value{table}
	sort_result, sort_ok := context_table_mutate(
		&ctx,
		Table_Request{operation = .Sort, values = sort_values[:], sort_components = sort_components[:]},
	)
	defer value_destroy(&sort_result.subrc)
	defer value_destroy(&sort_result.tabix)
	testing.expect(t, sort_ok)
	first_read_values := [?]Value{table, value_integer_make(1)}
	sorted_result, sorted_ok := context_table_read(
		&ctx,
		Table_Request{operation = .Read, values = first_read_values[:], result_type = &row_type},
	)
	defer value_destroy(&sorted_result.row)
	defer value_destroy(&sorted_result.subrc)
	defer value_destroy(&sorted_result.tabix)
	testing.expect(t, sorted_ok)
	string_type := test_type_descriptor("string", .Text, .String, true)
	text, text_ok := context_field_load(&ctx, Field_Request{base = sorted_result.row, name = "text", result_type = &string_type})
	defer value_destroy(&text)
	testing.expect(t, text_ok)
	testing.expect_value(t, value_text(text), "z")
}

@(test)
scalar_text_is_limited_to_scalar_values :: proc(t: ^testing.T) {
	text := value_string("abc", context.allocator)
	defer value_destroy(&text)
	scalar_text, scalar_ok := value_scalar_text(text, context.temp_allocator)
	testing.expect(t, scalar_ok)
	testing.expect_value(t, scalar_text, "abc")

	structure := value_structure("sy", context.allocator)
	defer value_destroy(&structure)
	table := value_table(context.allocator)
	defer value_destroy(&table)
	iterator := value_table_iterator(table, context.allocator)
	defer value_destroy(&iterator)

	values := [?]Value {
		Value_World{},
		structure,
		table,
		iterator,
	}
	for value in values {
		_, ok := value_scalar_text(value, context.temp_allocator)
		testing.expect(t, !ok)
		testing.expect(t, !value_has_scalar_text(value))
	}
}

@(test)
text_operations_reject_non_scalar_values :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	structure := value_structure("sy", context.allocator)
	defer value_destroy(&structure)
	values := [?]Value{structure}
	testing.expect(t, !context_write(&ctx, values[:]))
	testing.expect_value(t, ctx.trap.kind, Trap_Kind.Type)

	string_type := test_type_descriptor("string", .Text, .String, true)
	cast_value, cast_ok := value_cast(structure, &string_type, context.allocator)
	defer value_destroy(&cast_value)
	testing.expect(t, !cast_ok)

	table := value_table(context.allocator)
	defer value_destroy(&table)
	append_result, append_ok := table_append(&ctx, table, structure, {})
	defer value_destroy(&append_result.subrc)
	defer value_destroy(&append_result.tabix)
	testing.expect(t, append_ok)
	sort_result, sort_ok := table_sort(&ctx, table, nil, false, {})
	defer value_destroy(&sort_result.subrc)
	defer value_destroy(&sort_result.tabix)
	testing.expect(t, !sort_ok)
	testing.expect_value(t, ctx.trap.kind, Trap_Kind.Type)
}
