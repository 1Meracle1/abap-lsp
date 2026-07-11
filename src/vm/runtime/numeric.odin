package abap_frontend_vm_runtime

import "core:mem"
import "core:strconv"
import "core:strings"

import "base:intrinsics"

decimal_pow10 :: proc "contextless" (places: i32) -> (i128, bool) {
	if places < 0 || places > 38 {
		return 0, false
	}
	value := i128(1)
	for _ in 0 ..< places {
		next, overflow := intrinsics.overflow_mul(value, i128(10))
		if overflow { return 0, false }
		value = next
	}
	return value, true
}

integer_fits_type :: proc(value: i64, typ: Type_Descriptor) -> bool {
	assert(typ != nil)
	if typ.family == .Numeric { return true }
	assert(typ.family == .Integer)
	assert(typ.elementary.bits > 0 && typ.elementary.bits <= 64)
	bits := typ.elementary.bits
	if bits == 64 {
		return typ.elementary.signed || value >= 0
	}
	if typ.elementary.signed {
		limit := i64(1) << (bits - 1)
		return value >= -limit && value < limit
	}
	return value >= 0 && u64(value) < (u64(1) << bits)
}

decimal_fits_type :: proc(value: Value_Decimal, typ: Type_Descriptor) -> bool {
	assert(typ != nil)
	if typ.family == .Numeric { return true }
	assert(typ.family == .Decimal)
	assert(typ.elementary.has_length)
	assert(typ.elementary.length >= 1 && typ.elementary.length <= 16)
	// An ABAP packed field of n bytes carries 2*n-1 decimal digits.
	digits := value_decimal_text(Value_Decimal{coefficient = value.coefficient, scale = 0}, context.temp_allocator)
	if len(digits) > 0 && digits[0] == '-' { digits = digits[1:] }
	return len(digits) <= typ.elementary.length * 2 - 1
}

value_decimal_rescale_coefficient :: proc "contextless" (coefficient: i128, from_scale, to_scale: i32) -> i128 {
	if from_scale == to_scale {
		return coefficient
	}
	if from_scale < to_scale {
		factor, ok := decimal_pow10(to_scale - from_scale)
		return coefficient * factor if ok else coefficient
	}
	factor, ok := decimal_pow10(from_scale - to_scale)
	if !ok || factor == 0 {
		return coefficient
	}
	quotient := coefficient / factor
	remainder := coefficient % factor
	if remainder < 0 {
		remainder = -remainder
	}
	if remainder * 2 >= factor {
		quotient += -1 if coefficient < 0 else 1
	}
	return quotient
}

decimal_divide_rounded :: proc "contextless" (numerator, denominator: i128) -> i128 {
	quotient := numerator / denominator
	remainder := numerator % denominator
	if remainder < 0 { remainder = -remainder }
	abs_denominator := denominator
	if abs_denominator < 0 { abs_denominator = -abs_denominator }
	if remainder * 2 >= abs_denominator {
		quotient += -1 if (numerator < 0) != (denominator < 0) else 1
	}
	return quotient
}

value_decimal_parse :: proc(input: string) -> (Value, bool) {
	text := strings.trim_space(input)
	if len(text) == 0 {
		return {}, false
	}
	negative := false
	i := 0
	if text[0] == '+' || text[0] == '-' {
		negative = text[0] == '-'
		i = 1
	}
	coefficient := i128(0)
	scale := i32(0)
	seen_digit := false
	seen_dot := false
	for ; i < len(text); i += 1 {
		ch := text[i]
		if ch == '.' && !seen_dot {
			seen_dot = true
			continue
		}
		if ch < '0' || ch > '9' {
			return {}, false
		}
		seen_digit = true
		coefficient = coefficient * 10 + i128(ch - '0')
		if seen_dot {
			scale += 1
		}
	}
	if !seen_digit {
		return {}, false
	}
	if negative {
		coefficient = -coefficient
	}
	return value_decimal_make(coefficient, scale), true
}

value_decimal_text :: proc(value: Value_Decimal, allocator: mem.Allocator) -> string {
	coefficient := value.coefficient
	negative := coefficient < 0
	if negative {
		coefficient = -coefficient
	}
	buf: [64]byte
	end := len(buf)
	if coefficient == 0 {
		end -= 1
		buf[end] = '0'
	} else {
		for coefficient > 0 {
			end -= 1
			buf[end] = byte(coefficient % 10) + '0'
			coefficient /= 10
		}
	}
	digits := string(buf[end:])
	out := strings.builder_make(allocator)
	if negative {
		strings.write_byte(&out, '-')
	}
	if value.scale <= 0 {
		strings.write_string(&out, digits)
		return strings.to_string(out)
	}
	if len(digits) <= int(value.scale) {
		strings.write_string(&out, "0.")
		for _ in 0 ..< int(value.scale) - len(digits) {
			strings.write_byte(&out, '0')
		}
		strings.write_string(&out, digits)
		return strings.to_string(out)
	}
	point := len(digits) - int(value.scale)
	strings.write_string(&out, digits[:point])
	strings.write_byte(&out, '.')
	strings.write_string(&out, digits[point:])
	return strings.to_string(out)
}

value_float :: proc(value: Value) -> (f64, bool) {
	resolved := value_borrow_alias(value)
	#partial switch v in resolved {
	case Value_Integer:
		return f64(v), true
	case Value_Decimal:
		factor, ok := decimal_pow10(v.scale)
		return f64(v.coefficient) / f64(factor), ok
	case Value_Float:
		return f64(v), true
	case Value_Initial:
		return 0, true
	case Value_String:
		return strconv.parse_f64(strings.trim_space(string(v)))
	case Value_Predicate:
		return 1 if bool(v) else 0, true
	}
	return 0, false
}

value_to_decimal :: proc(value: Value, scale: i32) -> (Value, bool) {
	resolved := value_borrow_alias(value)
	#partial switch v in resolved {
	case Value_Decimal:
		return value_decimal_make(value_decimal_rescale_coefficient(v.coefficient, v.scale, scale), scale), true
	case Value_Integer:
		factor, ok := decimal_pow10(scale)
		return value_decimal_make(i128(v) * factor, scale), ok
	case Value_Float:
		factor, ok := decimal_pow10(scale)
		if !ok {
			return {}, false
		}
		scaled := f64(v) * f64(factor)
		coefficient := i128(scaled + (0.5 if scaled >= 0 else -0.5))
		return value_decimal_make(coefficient, scale), true
	case Value_Initial:
		return value_decimal_make(0, scale), true
	case Value_String:
		parsed, ok := value_decimal_parse(string(v))
		if !ok {
			return {}, false
		}
		return value_to_decimal(parsed, scale)
	case Value_Predicate:
		factor, ok := decimal_pow10(scale)
		return value_decimal_make(factor if bool(v) else 0, scale), ok
	}
	return {}, false
}
