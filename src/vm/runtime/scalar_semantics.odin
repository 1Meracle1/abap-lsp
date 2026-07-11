package abap_frontend_vm_runtime

import "core:mem"
import "core:math"
import "core:strings"

import "base:intrinsics"

abap_numeric_arithmetic :: proc(
	ctx: ^Context,
	kind: Arithmetic_Kind,
	left, right: Value,
	result_type: Type_Descriptor,
	source: Source_Loc = {},
) -> (Value, bool) {
	if result_type == nil || !type_is_numeric(result_type) {
		context_trap(ctx, .Type, "numeric arithmetic requires a concrete numeric result type", source)
		return {}, false
	}
	if type_is_float(result_type) || value_kind(left) == .Float || value_kind(right) == .Float {
		left_float, left_ok := value_float(left)
		right_float, right_ok := value_float(right)
		if !left_ok || !right_ok {
			context_trap(ctx, .Type, "numeric arithmetic requires numeric operands", source)
			return {}, false
		}
		if (kind == .Divide || kind == .Integer_Divide || kind == .Modulo) && right_float == 0 {
			context_trap(ctx, .Divide_By_Zero, "division by zero", source)
			return {}, false
		}
		result: f64
		switch kind {
		case .Add: result = left_float + right_float
		case .Subtract: result = left_float - right_float
		case .Multiply: result = left_float * right_float
		case .Divide: result = left_float / right_float
		case .Integer_Divide: return value_integer_make(i64(left_float / right_float)), true
		case .Modulo:
			quotient := i64(left_float / right_float)
			result = left_float - f64(quotient) * right_float
		}
		if math.is_inf(result) || math.is_nan(result) {
			context_trap(ctx, .Overflow, "floating-point arithmetic overflow", source)
			return {}, false
		}
		return value_float_make(result), true
	}
	if type_is_decimal(result_type) || value_kind(left) == .Decimal || value_kind(right) == .Decimal {
		result_scale := type_decimal_places(result_type)
		left_value, left_ok := value_to_decimal(left, result_scale)
		right_value, right_ok := value_to_decimal(right, result_scale)
		if !left_ok || !right_ok {
			context_trap(ctx, .Type, "numeric arithmetic requires numeric operands", source)
			return {}, false
		}
		left_decimal := left_value.(Value_Decimal)
		right_decimal := right_value.(Value_Decimal)
		if (kind == .Divide || kind == .Integer_Divide || kind == .Modulo) && right_decimal.coefficient == 0 {
			context_trap(ctx, .Divide_By_Zero, "division by zero", source)
			return {}, false
		}
		factor, factor_ok := decimal_pow10(result_scale)
		if !factor_ok {
			context_trap(ctx, .Type, "decimal scale is unsupported", source)
			return {}, false
		}
		result_coefficient: i128
		switch kind {
		case .Add:
			result_coefficient, factor_ok = intrinsics.overflow_add(left_decimal.coefficient, right_decimal.coefficient)
		case .Subtract:
			result_coefficient, factor_ok = intrinsics.overflow_sub(left_decimal.coefficient, right_decimal.coefficient)
		case .Multiply:
			product, overflow := intrinsics.overflow_mul(left_decimal.coefficient, right_decimal.coefficient)
			factor_ok = overflow
			result_coefficient = value_decimal_rescale_coefficient(product, result_scale * 2, result_scale)
		case .Divide:
			numerator, overflow := intrinsics.overflow_mul(left_decimal.coefficient, factor)
			factor_ok = overflow
			result_coefficient = decimal_divide_rounded(numerator, right_decimal.coefficient)
		case .Integer_Divide: return value_integer_make(i64(left_decimal.coefficient / right_decimal.coefficient)), true
		case .Modulo: result_coefficient = left_decimal.coefficient % right_decimal.coefficient
		}
		if factor_ok {
			context_trap(ctx, .Overflow, "decimal arithmetic exceeds runtime precision", source)
			return {}, false
		}
		result := Value_Decimal{coefficient = result_coefficient, scale = result_scale}
		if !decimal_fits_type(result, result_type) {
			context_trap(ctx, .Overflow, "decimal result exceeds target field length", source)
			return {}, false
		}
		return result, true
	}
	left_int, left_ok := value_integer(left)
	right_int, right_ok := value_integer(right)
	if !left_ok || !right_ok {
		context_trap(ctx, .Type, "numeric arithmetic requires numeric operands", source)
		return {}, false
	}
	result: i64
	overflow := false
	switch kind {
	case .Add:
		result, overflow = intrinsics.overflow_add(left_int, right_int)
	case .Subtract:
		result, overflow = intrinsics.overflow_sub(left_int, right_int)
	case .Multiply:
		result, overflow = intrinsics.overflow_mul(left_int, right_int)
	case .Divide:
		if right_int == 0 {
			context_trap(ctx, .Divide_By_Zero, "division by zero", source)
			return {}, false
		}
		if left_int == min(i64) && right_int == -1 { overflow = true } else { result = left_int / right_int }
	case .Integer_Divide:
		if right_int == 0 {
			context_trap(ctx, .Divide_By_Zero, "division by zero", source)
			return {}, false
		}
		if left_int == min(i64) && right_int == -1 { overflow = true } else { result = left_int / right_int }
	case .Modulo:
		if right_int == 0 {
			context_trap(ctx, .Divide_By_Zero, "division by zero", source)
			return {}, false
		}
		result = left_int % right_int
	}
	if overflow || !integer_fits_type(result, result_type) {
		context_trap(ctx, .Overflow, "integer arithmetic overflow", source)
		return {}, false
	}
	return value_integer_make(result), true
}

abap_compare :: proc(
	ctx: ^Context,
	kind: Comparison_Kind,
	left, right: Value,
	source: Source_Loc = {},
) -> (Value, bool) {
	cmp: int
	if left_float, left_ok := value_float(left); left_ok && value_kind(left) != .String {
		if right_float, right_ok := value_float(right); right_ok && value_kind(right) != .String {
			cmp = -1 if left_float < right_float else (1 if left_float > right_float else 0)
		} else {
			context_trap(ctx, .Type, "comparison operands have incompatible runtime types", source)
			return {}, false
		}
	} else {
		left_text, left_text_ok := value_scalar_text(left, context.temp_allocator)
		right_text, right_text_ok := value_scalar_text(right, context.temp_allocator)
		if !left_text_ok || !right_text_ok {
			context_trap(ctx, .Type, "comparison requires scalar operands", source)
			return {}, false
		}
		cmp = strings.compare(left_text, right_text)
	}

	result := false
	switch kind {
	case .Equal:
		result = cmp == 0
	case .Not_Equal:
		result = cmp != 0
	case .Less:
		result = cmp < 0
	case .Less_Equal:
		result = cmp <= 0
	case .Greater:
		result = cmp > 0
	case .Greater_Equal:
		result = cmp >= 0
	}
	return value_predicate(result), true
}

abap_string_join :: proc(
	ctx: ^Context,
	values: []Value,
	allocator: mem.Allocator,
	source: Source_Loc = {},
) -> (Value, bool) {
	out := strings.builder_make(context.temp_allocator)
	for value in values {
		if !value_write_scalar_text(&out, value) {
			context_trap(ctx, .Type, "string operation requires scalar text operands", source)
			return {}, false
		}
	}
	return value_string(strings.to_string(out), allocator), true
}

abap_concatenate :: proc(
	ctx: ^Context,
	values: []Value,
	source_types: []Type_Descriptor,
	has_separator: bool,
	respecting_blanks: bool,
	allocator: mem.Allocator,
	source: Source_Loc = {},
) -> (Value, bool) {
	source_count := len(values)
	separator := ""
	if has_separator {
		if len(values) == 0 {
			context_trap(ctx, .Type, "CONCATENATE requires source operands", source)
			return {}, false
		}
		source_count = len(values) - 1
		separator_text, separator_ok := value_scalar_text(values[len(values) - 1], context.temp_allocator)
		if !separator_ok {
			context_trap(ctx, .Type, "CONCATENATE separator must be scalar text", source)
			return {}, false
		}
		separator = separator_text
	}
	if source_count <= 0 {
		context_trap(ctx, .Type, "CONCATENATE requires source operands", source)
		return {}, false
	}
	assert(len(source_types) == source_count, "CONCATENATE source type metadata is incomplete")
	out := strings.builder_make(context.temp_allocator)
	for i in 0 ..< source_count {
		if i > 0 && has_separator {
			strings.write_string(&out, separator)
		}
		text, text_ok := value_scalar_text(values[i], context.temp_allocator)
		if !text_ok {
			context_trap(ctx, .Type, "CONCATENATE requires scalar text operands", source)
			return {}, false
		}
		typ := source_types[i]
		if !respecting_blanks && type_is_fixed_text(typ) {
			text = strings.trim_right(text, " ")
		}
		strings.write_string(&out, text)
	}
	return value_string(strings.to_string(out), allocator), true
}

abap_condense :: proc(
	ctx: ^Context,
	value: Value,
	no_gaps: bool,
	allocator: mem.Allocator,
	source: Source_Loc = {},
) -> (Value, bool) {
	text, text_ok := value_scalar_text(value, context.temp_allocator)
	if !text_ok {
		context_trap(ctx, .Type, "CONDENSE requires a scalar text operand", source)
		return {}, false
	}
	out := strings.builder_make(context.temp_allocator)
	pending_space := false
	wrote := false
	for ch in text {
		if ch == ' ' {
			if !no_gaps && wrote {
				pending_space = true
			}
			continue
		}
		if pending_space {
			strings.write_byte(&out, ' ')
			pending_space = false
		}
		strings.write_rune(&out, ch)
		wrote = true
	}
	return value_string(strings.to_string(out), allocator), true
}

abap_translate :: proc(
	ctx: ^Context,
	kind: Translate_Kind,
	value: Value,
	allocator: mem.Allocator,
	source: Source_Loc = {},
) -> (Value, bool) {
	text, text_ok := value_scalar_text(value, context.temp_allocator)
	if !text_ok {
		context_trap(ctx, .Type, "TRANSLATE requires a scalar text operand", source)
		return {}, false
	}
	switch kind {
	case .To_Upper:
		return value_string(strings.to_upper(text, context.temp_allocator), allocator), true
	case .To_Lower:
		return value_string(strings.to_lower(text, context.temp_allocator), allocator), true
	}
	return value_string(text, allocator), true
}

abap_split :: proc(
	ctx: ^Context,
	value: Value,
	separator: Value,
	result_count: int,
	allocator: mem.Allocator,
	source: Source_Loc = {},
) -> ([]Value, bool) {
	text, text_ok := value_scalar_text(value, context.temp_allocator)
	if !text_ok {
		context_trap(ctx, .Type, "SPLIT requires a scalar text source", source)
		return nil, false
	}
	separator_text, separator_ok := value_scalar_text(separator, context.temp_allocator)
	if !separator_ok {
		context_trap(ctx, .Type, "SPLIT separator must be scalar text", source)
		return nil, false
	}
	if separator_text == "" {
		context_trap(ctx, .Type, "SPLIT separator must not be empty", source)
		return nil, false
	}
	if result_count <= 0 {
		context_trap(ctx, .Type, "SPLIT requires target operands", source)
		return nil, false
	}
	results := make([]Value, result_count, context.temp_allocator)
	remaining := text
	for i := 0; i < result_count; i += 1 {
		part := remaining
		if i < result_count - 1 {
			if index := strings.index(remaining, separator_text); index >= 0 {
				part = remaining[:index]
				remaining = remaining[index + len(separator_text):]
			} else {
				remaining = ""
			}
		}
		results[i] = value_string(part, allocator)
	}
	return results, true
}

abap_replace :: proc(
	ctx: ^Context,
	occurrence: Replace_Occurrence,
	value: Value,
	pattern: Value,
	replacement: Value,
	allocator: mem.Allocator,
	source: Source_Loc = {},
) -> (Value, bool) {
	text, text_ok := value_scalar_text(value, context.temp_allocator)
	if !text_ok {
		context_trap(ctx, .Type, "REPLACE requires a scalar text target", source)
		return {}, false
	}
	pattern_text, pattern_ok := value_scalar_text(pattern, context.temp_allocator)
	if !pattern_ok {
		context_trap(ctx, .Type, "REPLACE pattern must be scalar text", source)
		return {}, false
	}
	replacement_text, replacement_ok := value_scalar_text(replacement, context.temp_allocator)
	if !replacement_ok {
		context_trap(ctx, .Type, "REPLACE replacement must be scalar text", source)
		return {}, false
	}
	if pattern_text == "" {
		return value_string(text, allocator), true
	}
	out := strings.builder_make(context.temp_allocator)
	start := 0
	replaced := false
	for start <= len(text) {
		index := strings.index(text[start:], pattern_text)
		if index < 0 {
			break
		}
		match_start := start + index
		strings.write_string(&out, text[start:match_start])
		strings.write_string(&out, replacement_text)
		start = match_start + len(pattern_text)
		replaced = true
		if occurrence == .First {
			break
		}
	}
	if !replaced {
		return value_string(text, allocator), true
	}
	strings.write_string(&out, text[start:])
	return value_string(strings.to_string(out), allocator), true
}

abap_shift :: proc(
	ctx: ^Context,
	direction: Shift_Direction,
	value: Value,
	places: Value,
	allocator: mem.Allocator,
	source: Source_Loc = {},
) -> (Value, bool) {
	text, text_ok := value_scalar_text(value, context.temp_allocator)
	if !text_ok {
		context_trap(ctx, .Type, "SHIFT requires a scalar text target", source)
		return {}, false
	}
	place_count, places_ok := value_integer(places)
	if !places_ok {
		context_trap(ctx, .Type, "SHIFT places must be integer-compatible", source)
		return {}, false
	}
	if place_count <= 0 {
		return value_string(text, allocator), true
	}
	count := int(place_count)
	switch direction {
	case .Left:
		if count >= len(text) {
			return value_string("", allocator), true
		}
		return value_string(text[count:], allocator), true
	case .Right:
		out := strings.builder_make(context.temp_allocator)
		for i := 0; i < count; i += 1 {
			strings.write_byte(&out, ' ')
		}
		strings.write_string(&out, text)
		return value_string(strings.to_string(out), allocator), true
	}
	return value_string(text, allocator), true
}

abap_find :: proc(
	ctx: ^Context,
	occurrence: Find_Occurrence,
	ignoring_case: bool,
	pattern: Value,
	target: Value,
	source: Source_Loc = {},
) -> (Find_Result, bool) {
	pattern_text, pattern_ok := value_scalar_text(pattern, context.temp_allocator)
	if !pattern_ok {
		context_trap(ctx, .Type, "FIND pattern must be scalar text", source)
		return {}, false
	}
	target_text, target_ok := value_scalar_text(target, context.temp_allocator)
	if !target_ok {
		context_trap(ctx, .Type, "FIND target must be scalar text", source)
		return {}, false
	}
	search_pattern := pattern_text
	search_target := target_text
	if ignoring_case {
		search_pattern = strings.to_lower(pattern_text, context.temp_allocator)
		search_target = strings.to_lower(target_text, context.temp_allocator)
	}
	if search_pattern == "" {
		return Find_Result{subrc = 0, offset = 0, length = 0, count = 1}, true
	}
	result := Find_Result{subrc = 4, offset = 0, length = 0, count = 0}
	start := 0
	for start <= len(search_target) {
		index := strings.index(search_target[start:], search_pattern)
		if index < 0 {
			break
		}
		match_start := start + index
		result.subrc = 0
		result.offset = i64(match_start)
		result.length = i64(len(search_pattern))
		result.count += 1
		if occurrence == .First {
			break
		}
		start = match_start + len(search_pattern)
	}
	return result, true
}

abap_search :: proc(
	ctx: ^Context,
	target: Value,
	pattern: Value,
	source: Source_Loc = {},
) -> (Search_Result, bool) {
	target_text, target_ok := value_scalar_text(target, context.temp_allocator)
	if !target_ok {
		context_trap(ctx, .Type, "SEARCH target must be scalar text", source)
		return {}, false
	}
	pattern_text, pattern_ok := value_scalar_text(pattern, context.temp_allocator)
	if !pattern_ok {
		context_trap(ctx, .Type, "SEARCH pattern must be scalar text", source)
		return {}, false
	}
	if strings.contains(pattern_text, "*") || strings.contains(pattern_text, ".") {
		context_trap(ctx, .Unsupported, "SEARCH special pattern semantics are not implemented", source)
		return {}, false
	}
	search_target := strings.to_lower(target_text, context.temp_allocator)
	search_pattern := strings.to_lower(pattern_text, context.temp_allocator)
	if search_pattern == "" {
		return Search_Result{subrc = 0, fdpos = 0}, true
	}
	index := strings.index(search_target, search_pattern)
	if index < 0 {
		return Search_Result{subrc = 4, fdpos = i64(len(target_text))}, true
	}
	return Search_Result{subrc = 0, fdpos = i64(index)}, true
}

abap_construct :: proc(
	ctx: ^Context,
	callee_name: string,
	values: []Value,
	result_type: Type_Descriptor,
	reference_target_type: Type_Descriptor = nil,
	source: Source_Loc = {},
) -> (Value, bool) {
	switch callee_name {
	case "", "value":
		if len(values) > 0 {
			return value_storage_clone(values[0], ctx.allocator), true
		}
		return initial_for_type(result_type, ctx.allocator), true
	case "ref":
		if len(values) != 1 {
			context_trap(ctx, .Type, "REF constructor requires one addressable operand", source)
			return {}, false
		}
		reference, reference_ok := value_data_reference_from_alias(values[0], ctx.allocator)
		if !reference_ok {
			context_trap(ctx, .Unsupported, "REF constructor requires a deterministic addressable operand", source)
			return {}, false
		}
		return reference, true
	case "new", "create_data":
		if type_is_data_reference(result_type) {
			cell_value := initial_for_type(reference_target_type, ctx.allocator)
			if len(values) > 0 {
				value_destroy(&cell_value)
				cell_value = value_storage_clone(values[0], ctx.allocator)
			}
			defer value_destroy(&cell_value)
			cell := cell_make(cell_value, ctx.allocator)
			defer cell_release(cell)
			return value_data_reference_cell(cell, ctx.allocator), true
		}
		if type_is_object_reference(result_type) {
			return value_object(type_display_name(result_type), ctx.allocator), true
		}
		return value_structure(type_display_name(result_type), ctx.allocator), true
	case "conv", "exact", "cast":
		if len(values) == 1 {
			value, ok := value_cast(values[0], result_type, ctx.allocator)
			if !ok {
				context_trap(ctx, .Type, "constructor cast failed", source)
				return {}, false
			}
			return value, true
		}
		context_trap(ctx, .Unsupported, "constructor cast value runtime semantics are not implemented", source)
		return {}, false
	case:
		context_trap(ctx, .Unsupported, "constructor runtime semantics are not implemented", source)
		return {}, false
	}
}
