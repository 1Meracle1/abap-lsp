package abap_frontend_vm_runtime

import "core:mem"
import "core:slice"
import "core:strings"

context_table_read :: proc(
	ctx: ^Context,
	request: Table_Request,
	source: Source_Loc = {},
) -> (Table_Read_Result, bool) {
	if len(request.values) == 0 {
		context_trap(ctx, .Type, "table read requires a table operand", source)
		return {}, false
	}
	table := request.values[0]
	index := request.index
	if index == 0 && len(request.components) == 0 && len(request.values) > 1 {
		if parsed, ok := value_integer(request.values[1]); ok {
			index = int(parsed)
		}
	}
	row, tabix, found := table_read(table, index, request.components, ctx.allocator, source)
	if found {
		return Table_Read_Result {
			row = row,
			subrc = value_integer_make(0),
			tabix = value_integer_make(i64(tabix)),
		}, true
	}
	return Table_Read_Result {
		row = initial_for_type(request.result_type, ctx.allocator),
		subrc = value_integer_make(4),
		tabix = value_integer_make(0),
	}, true
}

context_table_iter :: proc(
	ctx: ^Context,
	request: Table_Request,
	source: Source_Loc = {},
) -> (Value, bool) {
	if len(request.values) == 0 {
		context_trap(ctx, .Type, "table iterator requires a table operand", source)
		return {}, false
	}
	return value_table_iterator(request.values[0], ctx.allocator, request.components), true
}

context_table_next :: proc(
	ctx: ^Context,
	request: Table_Request,
	source: Source_Loc = {},
) -> (Table_Next_Result, bool) {
	if len(request.values) == 0 || value_iterator_data(request.values[0]) == nil {
		context_trap(ctx, .Type, "table next requires a table iterator operand", source)
		return {}, false
	}
	iter := value_iterator_data(request.values[0])
	if iter.table == nil || iter.index < 0 {
		next := value_iterator_advanced_to(request.values[0], iter.index + 1, iter.matched, ctx.allocator)
		return Table_Next_Result {
			has_row = value_predicate(false),
			row = initial_for_type(request.result_type, ctx.allocator),
			next_iter = next,
			subrc = value_integer_make(4),
			tabix = value_integer_make(0),
		}, true
	}
	row_index := table_find_match(iter.table, iter.index, iter.filters[:], source)
	if row_index < 0 {
		next := value_iterator_advanced_to(request.values[0], len(iter.table.rows), iter.matched, ctx.allocator)
		subrc := i64(0) if iter.matched else i64(4)
		return Table_Next_Result {
			has_row = value_predicate(false),
			row = initial_for_type(request.result_type, ctx.allocator),
			next_iter = next,
			subrc = value_integer_make(subrc),
			tabix = value_integer_make(0),
		}, true
	}
	tabix := row_index + 1
	next := value_iterator_advanced_to(request.values[0], row_index + 1, true, ctx.allocator)
	return Table_Next_Result {
		has_row = value_predicate(true),
		row = value_deep_clone(iter.table.rows[row_index], ctx.allocator),
		next_iter = next,
		subrc = value_integer_make(0),
		tabix = value_integer_make(i64(tabix)),
	}, true
}

context_table_mutate :: proc(
	ctx: ^Context,
	request: Table_Request,
	source: Source_Loc = {},
) -> (Table_Mutate_Result, bool) {
	#partial switch request.operation {
	case .Append:
		if len(request.values) < 2 {
			context_trap(ctx, .Type, "APPEND requires row and table operands", source)
			return {}, false
		}
		return table_append(ctx, request.values[1], request.values[0], source)
	case .Insert:
		if len(request.values) < 2 {
			context_trap(ctx, .Type, "INSERT requires row and table operands", source)
			return {}, false
		}
		index := request.index
		if index == 0 && len(request.values) > 2 {
			if parsed, ok := value_integer(request.values[2]); ok {
				index = int(parsed)
			}
		}
		return table_insert(ctx, request.values[1], request.values[0], index, source)
	case .Modify:
		if len(request.values) < 2 {
			context_trap(ctx, .Type, "MODIFY requires row and table operands", source)
			return {}, false
		}
		index := request.index
		if index == 0 && len(request.components) == 0 && len(request.values) > 2 {
			if parsed, ok := value_integer(request.values[2]); ok {
				index = int(parsed)
			}
		}
		return table_modify(ctx, request.values[1], request.values[0], index, request.components, source)
	case .Delete:
		if len(request.values) < 1 {
			context_trap(ctx, .Type, "DELETE requires a table operand", source)
			return {}, false
		}
		index := request.index
		if index == 0 && len(request.components) == 0 && len(request.values) > 1 {
			if parsed, ok := value_integer(request.values[1]); ok {
				index = int(parsed)
			}
		}
		source_row := Value{}
		if len(request.values) > 1 && index == 0 && len(request.components) == 0 {
			source_row = request.values[1]
		}
		return table_delete(ctx, request.values[0], index, request.components, source_row, source)
	case .Sort:
		if len(request.values) < 1 {
			context_trap(ctx, .Type, "SORT requires a table operand", source)
			return {}, false
		}
		return table_sort(ctx, request.values[0], request.sort_components, request.descending, source)
	case:
		context_trap(ctx, .Unsupported, "internal table operation is not implemented", source)
		return {}, false
	}
}

context_sql_select :: proc(
	ctx: ^Context,
	source: Source_Loc = {},
) -> (Value, Value, bool) {
	context_trap(ctx, .Unsupported, "Open SQL requires a host SQL service", source)
	return {}, {}, false
}

context_sql_mutate :: proc(
	ctx: ^Context,
	source: Source_Loc = {},
) -> bool {
	context_trap(ctx, .Unsupported, "Open SQL requires a host SQL service", source)
	return false
}

table_len :: proc "contextless" (value: Value) -> int {
	if table := value_table_data(value); table != nil {
		return len(table.rows)
	}
	return 0
}

table_read :: proc(
	value: Value,
	index: int,
	components: []Table_Component,
	allocator: mem.Allocator,
	source: Source_Loc,
) -> (Value, int, bool) {
	table := value_table_data(value)
	if value_kind(value) == .Initial {
		return {}, 0, false
	}
	if table == nil {
		return {}, 0, false
	}
	row_index := index - 1
	if index == 0 {
		row_index = table_find_match(table, 0, components, source)
	}
	if row_index < 0 || row_index >= len(table.rows) {
		return {}, 0, false
	}
	return value_deep_clone(table.rows[row_index], allocator), row_index + 1, true
}

table_mutate_result :: #force_inline proc "contextless" (subrc, tabix: i64) -> Table_Mutate_Result {
	return Table_Mutate_Result{subrc = value_integer_make(subrc), tabix = value_integer_make(tabix)}
}

table_append :: proc(ctx: ^Context, table: Value, row: Value, source: Source_Loc) -> (Table_Mutate_Result, bool) {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return {}, false
	}
	append(&data.rows, value_deep_clone(row, data.allocator))
	return table_mutate_result(0, i64(len(data.rows))), true
}

table_insert :: proc(ctx: ^Context, table: Value, row: Value, index: int, source: Source_Loc) -> (Table_Mutate_Result, bool) {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return {}, false
	}
	row_index := index - 1
	if index == 0 || row_index >= len(data.rows) {
		append(&data.rows, value_deep_clone(row, data.allocator))
		return table_mutate_result(0, i64(len(data.rows))), true
	}
	if row_index < 0 {
		row_index = 0
	}
	append(&data.rows, Value{})
	for i := len(data.rows) - 1; i > row_index; i -= 1 {
		data.rows[i] = data.rows[i - 1]
	}
	data.rows[row_index] = value_deep_clone(row, data.allocator)
	return table_mutate_result(0, i64(row_index + 1)), true
}

table_modify :: proc(
	ctx: ^Context,
	table: Value,
	row: Value,
	index: int,
	components: []Table_Component,
	source: Source_Loc,
) -> (Table_Mutate_Result, bool) {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return {}, false
	}
	if len(components) > 0 {
		modified := false
		first_tabix := 0
		for row_index in 0 ..< len(data.rows) {
			if !table_row_matches(data.rows[row_index], components, source) {
				continue
			}
			value_destroy(&data.rows[row_index])
			data.rows[row_index] = value_deep_clone(row, data.allocator)
			if first_tabix == 0 {
				first_tabix = row_index + 1
			}
			modified = true
		}
		if modified {
			return table_mutate_result(0, i64(first_tabix)), true
		}
		return table_mutate_result(4, 0), true
	}
	row_index := index - 1
	if index == 0 {
		row_index = 0
	}
	if row_index < 0 || row_index >= len(data.rows) {
		return table_mutate_result(4, 0), true
	}
	value_destroy(&data.rows[row_index])
	data.rows[row_index] = value_deep_clone(row, data.allocator)
	return table_mutate_result(0, i64(row_index + 1)), true
}

table_delete :: proc(
	ctx: ^Context,
	table: Value,
	index: int,
	components: []Table_Component,
	source_row: Value,
	source: Source_Loc,
) -> (Table_Mutate_Result, bool) {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return {}, false
	}
	if len(components) > 0 || value_kind(source_row) != .Initial {
		deleted := false
		first_tabix := 0
		for row_index := len(data.rows) - 1; row_index >= 0; row_index -= 1 {
			matches := table_row_matches(data.rows[row_index], components, source) if len(components) > 0 else table_values_equal(ctx, data.rows[row_index], source_row, source)
			if !matches {
				continue
			}
			value_destroy(&data.rows[row_index])
			ordered_remove(&data.rows, row_index)
			first_tabix = row_index + 1
			deleted = true
			if row_index == 0 {
				break
			}
		}
		if deleted {
			return table_mutate_result(0, i64(first_tabix)), true
		}
		return table_mutate_result(4, 0), true
	}
	row_index := index - 1
	if index == 0 {
		row_index = 0
	}
	if row_index < 0 || row_index >= len(data.rows) {
		return table_mutate_result(4, 0), true
	}
	value_destroy(&data.rows[row_index])
	ordered_remove(&data.rows, row_index)
	return table_mutate_result(0, i64(row_index + 1)), true
}

table_sort :: proc(
	ctx: ^Context,
	table: Value,
	components: []Table_Sort_Component,
	descending: bool,
	source: Source_Loc,
) -> (Table_Mutate_Result, bool) {
	data := value_table_data(table)
	if data == nil {
		context_trap(ctx, .Type, "table operand is not mutable", source)
		return {}, false
	}
	if len(components) == 0 {
		for row in data.rows {
			if !value_has_scalar_text(row) {
				context_trap(ctx, .Type, "SORT requires scalar row values", source)
				return {}, false
			}
		}
		table_sort_descending = descending
		slice.sort_by(data.rows[:], table_row_less)
		return table_mutate_result(0, 0), true
	}
	for row in data.rows {
		for component in components {
			value, ok := table_component_value(row, component.path)
			if !ok || !value_has_scalar_text(value) {
				context_trap(ctx, .Type, "SORT requires scalar component values", source)
				return {}, false
			}
		}
	}
	table_sort_components = components
	slice.sort_by(data.rows[:], table_row_component_less)
	table_sort_components = nil
	return table_mutate_result(0, 0), true
}

table_sort_descending: bool
table_sort_components: []Table_Sort_Component

table_row_less :: proc(left, right: Value) -> bool {
	left_text, left_ok := value_scalar_text(left, context.temp_allocator)
	right_text, right_ok := value_scalar_text(right, context.temp_allocator)
	assert(left_ok && right_ok)
	result := strings.compare(left_text, right_text) < 0
	return !result if table_sort_descending else result
}

table_row_component_less :: proc(left, right: Value) -> bool {
	for component in table_sort_components {
		left_value, left_ok := table_component_value(left, component.path)
		right_value, right_ok := table_component_value(right, component.path)
		assert(left_ok && right_ok)
		left_text, left_text_ok := value_scalar_text(left_value, context.temp_allocator)
		right_text, right_text_ok := value_scalar_text(right_value, context.temp_allocator)
		assert(left_text_ok && right_text_ok)
		cmp := strings.compare(left_text, right_text)
		if cmp == 0 {
			continue
		}
		less := cmp < 0
		return !less if component.descending else less
	}
	return false
}

table_find_match :: proc(table: ^Table_Data, start: int, components: []Table_Component, source: Source_Loc) -> int {
	if table == nil {
		return -1
	}
	first := start
	if first < 0 {
		first = 0
	}
	if len(components) == 0 {
		return first if first < len(table.rows) else -1
	}
	for row_index in first ..< len(table.rows) {
		if table_row_matches(table.rows[row_index], components, source) {
			return row_index
		}
	}
	return -1
}

table_row_matches :: proc(row: Value, components: []Table_Component, source: Source_Loc) -> bool {
	for component in components {
		actual, ok := table_component_value(row, component.path)
		if !ok || !table_values_equal(nil, actual, component.value, source) {
			return false
		}
	}
	return true
}

table_values_equal :: proc(ctx: ^Context, left, right: Value, source: Source_Loc) -> bool {
	if left_int, left_ok := value_integer(left); left_ok {
		if right_int, right_ok := value_integer(right); right_ok {
			return left_int == right_int
		}
	}
	left_text, left_text_ok := value_scalar_text(left, context.temp_allocator)
	right_text, right_text_ok := value_scalar_text(right, context.temp_allocator)
	if !left_text_ok || !right_text_ok {
		if ctx != nil {
			context_trap(ctx, .Type, "table key comparison requires scalar values", source)
		}
		return false
	}
	return left_text == right_text
}

table_component_value :: proc(row: Value, path: []string) -> (Value, bool) {
	if len(path) == 0 || (len(path) == 1 && strings.equal_fold(path[0], "table_line")) {
		return value_borrow_alias(row), true
	}
	current := value_borrow_alias(row)
	for name in path {
		if name == "" {
			return {}, false
		}
		if strings.equal_fold(name, "table_line") {
			current = value_borrow_alias(current)
			continue
		}
		structure := value_structure_data(current)
		if structure == nil {
			return {}, false
		}
		field, ok := table_structure_field_value(structure, name)
		if !ok {
			return {}, false
		}
		current = value_borrow_alias(field)
	}
	return current, true
}

table_structure_field_value :: proc(structure: ^Structure_Data, name: string) -> (Value, bool) {
	if structure == nil {
		return {}, false
	}
	if field, ok := structure.fields[name]; ok {
		return field, true
	}
	for key, field in structure.fields {
		if strings.equal_fold(key, name) {
			return field, true
		}
	}
	return {}, false
}
