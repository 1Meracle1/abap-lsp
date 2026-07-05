package abap_frontend_runtime

import "core:testing"

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
context_system_values_use_canonical_field_names :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	context_system_write(&ctx, "subrc", value_integer_make(8))
	subrc := context_system_read(&ctx, "subrc")
	testing.expect_value(t, value_kind(subrc), Value_Kind.Integer)
	testing.expect_value(t, value_int(subrc), i64(8))
}

@(test)
abap_helpers_cover_current_scalar_semantics :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	sum, sum_ok := abap_integer_arithmetic(&ctx, .Add, value_integer_make(1), value_integer_make(2))
	testing.expect(t, sum_ok)
	testing.expect_value(t, value_kind(sum), Value_Kind.Integer)
	testing.expect_value(t, value_int(sum), i64(3))

	equal, equal_ok := abap_compare(&ctx, .Equal, sum, value_integer_make(3))
	testing.expect(t, equal_ok)
	testing.expect_value(t, value_kind(equal), Value_Kind.Predicate)
	testing.expect_value(t, value_int(equal), i64(1))
}

@(test)
object_references_are_heap_allocated_and_reference_counted :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	initial := initial_for_type("!ref:lcl_demo", context.allocator)
	testing.expect_value(t, value_kind(initial), Value_Kind.Initial)

	object, object_ok := abap_construct(&ctx, "new", nil, "!ref:lcl_demo")
	defer value_destroy(&object)
	testing.expect(t, object_ok)
	testing.expect_value(t, value_kind(object), Value_Kind.Object)
	object_data := value_object_data(object)
	testing.expect(t, object_data != nil)
	if object_data != nil {
		testing.expect_value(t, object_data.refs, 1)
		testing.expect_value(t, object_data.type_name, "!ref:lcl_demo")
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
	total, total_ok := context_field_load(&ctx, Field_Request {
		base = object,
		name = "mv_total",
		result_type = "!i",
	})
	defer value_destroy(&total)
	testing.expect(t, total_ok)
	testing.expect_value(t, value_kind(total), Value_Kind.Integer)
	testing.expect_value(t, value_int(total), i64(7))
}

@(test)
table_helpers_read_and_mutate_values_without_bytecode :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	table := value_table(context.allocator)
	defer value_destroy(&table)
	row := value_integer_make(7)
	values := [?]Value{row, table}
	testing.expect(t, context_table_mutate(&ctx, Table_Request{operation = .Append, values = values[:]}))

	read_values := [?]Value{table, value_integer_make(1)}
	read, subrc, read_ok := context_table_read(
		&ctx,
		Table_Request{operation = .Read, values = read_values[:], result_type = "!i"},
	)
	defer value_destroy(&read)
	defer value_destroy(&subrc)
	testing.expect(t, read_ok)
	testing.expect_value(t, value_kind(read), Value_Kind.Integer)
	testing.expect_value(t, value_int(read), i64(7))
	testing.expect_value(t, value_kind(subrc), Value_Kind.Integer)
	testing.expect_value(t, value_int(subrc), i64(0))
}

@(test)
structure_fields_and_table_rows_support_mixed_report_shapes :: proc(t: ^testing.T) {
	ctx := context_make({}, context.allocator)
	defer context_destroy(&ctx)

	row := initial_for_type("!ty_source", context.allocator)
	defer value_destroy(&row)
	first_material := value_string("MAT-001", context.allocator)
	defer value_destroy(&first_material)
	testing.expect(t, context_field_store(&ctx, Field_Request{base = row, name = "matnr", value = first_material}))

	table := value_table(context.allocator)
	defer value_destroy(&table)
	append_values := [?]Value{row, table}
	testing.expect(t, context_table_mutate(&ctx, Table_Request{operation = .Append, values = append_values[:]}))

	second_material := value_string("MAT-002", context.allocator)
	defer value_destroy(&second_material)
	testing.expect(t, context_field_store(&ctx, Field_Request{base = row, name = "matnr", value = second_material}))

	read_values := [?]Value{table, value_integer_make(1)}
	read, subrc, read_ok := context_table_read(
		&ctx,
		Table_Request{operation = .Read, values = read_values[:], result_type = "!ty_source"},
	)
	defer value_destroy(&read)
	defer value_destroy(&subrc)
	testing.expect(t, read_ok)
	testing.expect_value(t, value_kind(subrc), Value_Kind.Integer)
	testing.expect_value(t, value_int(subrc), i64(0))

	material, material_ok := context_field_load(
		&ctx,
		Field_Request{base = read, name = "matnr", result_type = "!string"},
	)
	defer value_destroy(&material)
	testing.expect(t, material_ok)
	testing.expect_value(t, value_kind(material), Value_Kind.String)
	testing.expect_value(t, value_text(material), "MAT-001")
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

	cast_value, cast_ok := value_cast(structure, "!string", context.allocator)
	defer value_destroy(&cast_value)
	testing.expect(t, !cast_ok)

	table := value_table(context.allocator)
	defer value_destroy(&table)
	testing.expect(t, table_append(&ctx, table, structure, {}))
	testing.expect(t, !table_sort(&ctx, table, {}))
	testing.expect_value(t, ctx.trap.kind, Trap_Kind.Type)
}
