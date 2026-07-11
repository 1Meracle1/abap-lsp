package abap_frontend_vm_runtime

import "core:strings"

context_call :: proc(
	ctx: ^Context,
	request: Call_Request,
	source: Source_Loc = {},
) -> (Value, bool) {
	name := request.callee_name
	switch name {
	case "boolc":
		return value_string("X" if len(request.values) > 0 && value_truthy(request.values[0]) else "", ctx.allocator), true
	case "abs":
		if len(request.values) == 0 {
			return value_integer_make(0), true
		}
		value, ok := value_integer(request.values[0])
		if !ok {
			context_trap(ctx, .Type, "ABS requires an integer-compatible operand", source)
			return {}, false
		}
		if value < 0 {
			value = -value
		}
		return value_integer_make(value), true
	case "sign":
		if len(request.values) == 0 {
			return value_integer_make(0), true
		}
		value, ok := value_integer(request.values[0])
		if !ok {
			context_trap(ctx, .Type, "SIGN requires an integer-compatible operand", source)
			return {}, false
		}
		return value_integer_make(-1 if value < 0 else (1 if value > 0 else 0)), true
	case "strlen", "charlen", "numofchar", "dbmaxlen", "xstrlen":
		if len(request.values) == 0 {
			return value_integer_make(0), true
		}
		text, text_ok := value_scalar_text(request.values[0], context.temp_allocator)
		if !text_ok {
			context_trap(ctx, .Type, "string length builtin requires a scalar text operand", source)
			return {}, false
		}
		return value_integer_make(i64(len(text))), true
	case "lines":
		if len(request.values) == 0 {
			return value_integer_make(0), true
		}
		return value_integer_make(i64(table_len(request.values[0]))), true
	case "nmax", "nmin":
		return builtin_extremum(ctx, name, request.values, source)
	case "to_lower":
		if len(request.values) == 0 {
			return value_string("", ctx.allocator), true
		}
		text, text_ok := value_scalar_text(request.values[0], context.temp_allocator)
		if !text_ok {
			context_trap(ctx, .Type, "TO_LOWER requires a scalar text operand", source)
			return {}, false
		}
		return value_string(strings.to_lower(text, context.temp_allocator), ctx.allocator), true
	case "to_upper":
		if len(request.values) == 0 {
			return value_string("", ctx.allocator), true
		}
		text, text_ok := value_scalar_text(request.values[0], context.temp_allocator)
		if !text_ok {
			context_trap(ctx, .Type, "TO_UPPER requires a scalar text operand", source)
			return {}, false
		}
		return value_string(strings.to_upper(text, context.temp_allocator), ctx.allocator), true
	}
	context_trap(ctx, .Unsupported, "ABAP call dispatch is not implemented", source)
	return {}, false
}

builtin_extremum :: proc(
	ctx: ^Context,
	name: string,
	values: []Value,
	source: Source_Loc,
) -> (Value, bool) {
	if len(values) == 0 {
		return value_integer_make(0), true
	}
	best, best_ok := value_integer(values[0])
	if !best_ok {
		context_trap(ctx, .Type, "numeric builtin requires integer-compatible operands", source)
		return {}, false
	}
	for value in values[1:] {
		current, current_ok := value_integer(value)
		if !current_ok {
			context_trap(ctx, .Type, "numeric builtin requires integer-compatible operands", source)
			return {}, false
		}
		if (name == "nmax" && current > best) || (name == "nmin" && current < best) {
			best = current
		}
	}
	return value_integer_make(best), true
}
