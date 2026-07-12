package abap_frontend_vm_runtime

import "core:mem"
import "core:strconv"
import "core:strings"

value_kind :: proc "contextless" (value: Value) -> Value_Kind {
	#partial switch _ in value {
	case Value_Initial:
		return .Initial
	case Value_World:
		return .World
	case Value_Integer:
		return .Integer
	case Value_Decimal:
		return .Decimal
	case Value_Float:
		return .Float
	case Value_String:
		return .String
	case Value_Structure:
		return .Structure
	case Value_Object:
		return .Object
	case Value_Predicate:
		return .Predicate
	case Value_Table:
		return .Table
	case Value_Table_Iterator:
		return .Table_Iterator
	case Value_Reference:
		if reference := value_reference_data(value); reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_kind(reference_borrow(reference))
		}
		return .Reference
	}
	return .Initial
}

value_int :: proc "contextless" (value: Value) -> i64 {
	#partial switch v in value {
	case Value_Integer:
		return i64(v)
	case Value_Predicate:
		return 1 if bool(v) else 0
	case Value_Reference:
		reference := (^Reference_Data)(v)
		if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_int(reference_borrow(reference))
		}
	}
	return 0
}

value_text :: proc "contextless" (value: Value) -> string {
	#partial switch v in value {
	case Value_String:
		return string(v)
	case Value_Structure:
		if v.name != "" {
			return v.name
		}
		if v.data != nil {
			return v.data.name
		}
	case Value_Reference:
		reference := (^Reference_Data)(v)
		if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_text(reference_borrow(reference))
		}
	}
	return ""
}

value_structure_data :: proc "contextless" (value: Value) -> ^Structure_Data {
	resolved := value_borrow_alias(value)
	#partial switch v in resolved {
	case Value_Structure:
		return v.data
	}
	return nil
}

value_object_data :: proc "contextless" (value: Value) -> ^Object_Data {
	resolved := value_borrow_alias(value)
	#partial switch v in resolved {
	case Value_Object:
		return (^Object_Data)(v)
	}
	return nil
}

value_table_data :: proc "contextless" (value: Value) -> ^Table_Data {
	resolved := value_borrow_alias(value)
	#partial switch v in resolved {
	case Value_Table:
		return (^Table_Data)(v)
	}
	return nil
}

value_iterator_data :: proc "contextless" (value: Value) -> ^Table_Iterator_Data {
	resolved := value_borrow_alias(value)
	#partial switch v in resolved {
	case Value_Table_Iterator:
		return (^Table_Iterator_Data)(v)
	}
	return nil
}

value_from_literal :: proc(literal: string, typ: Type_Descriptor, allocator: mem.Allocator) -> Value {
	assert(typ != nil, "runtime literal materialization requires a type descriptor")
	switch typ.family {
	case .Integer:
		parsed, ok := strconv.parse_int(strings.trim_space(literal), 10)
		assert(ok, "integer runtime literals must be valid")
		return value_integer_make(i64(parsed))
	case .Decimal:
		value, ok := value_decimal_parse(strings.trim_space(literal))
		assert(ok, "decimal runtime literals must be valid")
		return value
	case .Float:
		parsed, ok := strconv.parse_f64(strings.trim_space(literal))
		assert(ok, "floating-point runtime literals must be valid")
		return value_float_make(parsed)
	case .Predicate:
		return value_predicate(literal != "" && literal != "0" && literal != " ")
	case .Text, .Date, .Time:
		text := literal
		if len(literal) >= 2 &&
		   ((literal[0] == '\'' && literal[len(literal) - 1] == '\'') ||
		    (literal[0] == '`' && literal[len(literal) - 1] == '`')) {
			text = literal[1:len(literal) - 1]
		}
		return value_string(text, allocator)
	case .Unknown, .Void, .World, .Numeric, .Bytes, .Structure, .Table,
	     .Table_Iterator, .Reference, .Object, .Interface, .Exception, .Routine:
		assert(false, "runtime type family cannot be materialized from a literal")
	}
	unreachable()
}

value_cast :: proc(value: Value, typ: Type_Descriptor, allocator: mem.Allocator) -> (Value, bool) {
	assert(typ != nil, "runtime casts require a target type descriptor")
	resolved := value
	if value_is_alias_reference(value) {
		resolved = value_borrow_alias(value)
	}
	switch typ.family {
	case .Integer:
		int_value, ok := value_integer(resolved)
		return value_integer_make(int_value), ok && integer_fits_type(int_value, typ)
	case .Decimal:
		decimal, ok := value_to_decimal(resolved, type_decimal_places(typ))
		if !ok { return {}, false }
		return decimal, decimal_fits_type(decimal.(Value_Decimal), typ)
	case .Float:
		float_value, ok := value_float(resolved)
		return value_float_make(float_value), ok
	case .Predicate:
		return value_predicate(value_truthy(resolved)), true
	case .Text, .Date, .Time:
		text, text_ok := value_scalar_text(resolved, context.temp_allocator)
		if !text_ok {
			return {}, false
		}
		return value_string(text, allocator), true
	case .Reference:
		kind := value_kind(resolved)
		if kind == .Initial || kind == .Object || kind == .Reference {
			return value_clone(resolved, allocator), true
		}
		return {}, false
	case .Numeric:
		kind := value_kind(resolved)
		if kind == .Integer || kind == .Decimal || kind == .Float {
			return value_clone(resolved, allocator), true
		}
		return {}, false
	case .Structure:
		if value_kind(resolved) == .Structure {
			return value_clone(resolved, allocator), true
		}
		return {}, false
	case .Table:
		if value_kind(resolved) == .Table {
			return value_clone(resolved, allocator), true
		}
		return {}, false
	case .Unknown, .Void, .World, .Bytes, .Table_Iterator,
	     .Object, .Interface, .Exception, .Routine:
		assert(false, "runtime type family cannot be a cast target")
	}
	unreachable()
}

initial_for_type :: proc(typ: Type_Descriptor, allocator: mem.Allocator) -> Value {
	assert(typ != nil, "runtime value initialization requires a type descriptor")
	switch typ.family {
	case .Integer:
		return value_integer_make(0)
	case .Decimal:
		return value_decimal_make(0, type_decimal_places(typ))
	case .Float:
		return value_float_make(0)
	case .Predicate:
		return value_predicate(false)
	case .Text, .Date, .Time:
		return value_string("", allocator)
	case .Table:
		return value_table(allocator)
	case .Reference:
		// An unbound reference is intentionally represented by Value_Initial.
		return value_initial()
	case .Structure:
		return value_structure(typ.display_name, allocator)
	case .Unknown:
		assert(false, "unknown runtime types cannot be initialized as values")
	case .Void:
		assert(false, "void runtime types cannot be initialized as values")
	case .World:
		assert(false, "world runtime types cannot be initialized as values")
	case .Numeric:
		assert(false, "generic numeric runtime types cannot be initialized as values")
	case .Bytes:
		assert(false, "byte runtime types cannot be initialized as values")
	case .Table_Iterator:
		assert(false, "table iterator runtime types cannot be initialized as values")
	case .Object:
		// Objects enter runtime through object references and construction.
		assert(false, "object runtime types cannot be initialized directly")
	case .Interface:
		assert(false, "interface runtime types cannot be initialized directly")
	case .Exception:
		assert(false, "exception runtime types cannot be initialized directly")
	case .Routine:
		assert(false, "routine runtime types cannot be initialized as values")
	}
	unreachable()
}

type_is_numeric :: #force_inline proc "contextless" (typ: Type_Descriptor) -> bool {
	return typ != nil && (typ.family == .Numeric || typ.family == .Integer || typ.family == .Decimal || typ.family == .Float)
}

type_decimal_places :: #force_inline proc "contextless" (typ: Type_Descriptor) -> i32 {
	if typ != nil && typ.elementary.has_decimals {
		return i32(typ.elementary.decimals)
	}
	return 0
}

type_is_text :: #force_inline proc "contextless" (typ: Type_Descriptor) -> bool {
	return typ != nil && (typ.family == .Text || typ.family == .Date || typ.family == .Time)
}

type_is_fixed_text :: #force_inline proc "contextless" (typ: Type_Descriptor) -> bool {
	return typ != nil && type_is_text(typ) && !typ.elementary.preserves_trailing_blanks
}

type_is_data_reference :: #force_inline proc "contextless" (typ: Type_Descriptor) -> bool {
	return typ != nil && typ.family == .Reference && typ.reference.kind == .Data
}

type_is_object_reference :: #force_inline proc "contextless" (typ: Type_Descriptor) -> bool {
	return typ != nil &&
	       typ.family == .Reference &&
	       (typ.reference.kind == .Object ||
	        typ.reference.kind == .Class ||
	        typ.reference.kind == .Interface ||
	        typ.reference.kind == .Exception)
}

value_integer_make :: #force_inline proc "contextless" (value: i64) -> Value {
	return Value_Integer(value)
}

value_decimal_make :: #force_inline proc "contextless" (coefficient: i128, scale: i32) -> Value {
	return Value_Decimal{coefficient = coefficient, scale = scale}
}

value_float_make :: #force_inline proc "contextless" (value: f64) -> Value {
	return Value_Float(value)
}

value_predicate :: #force_inline proc "contextless" (value: bool) -> Value {
	return Value_Predicate(value)
}

value_string :: #force_inline proc(value: string, allocator: mem.Allocator) -> Value {
	return Value_String(strings.clone(value, allocator))
}

value_structure :: #force_inline proc(name: string, allocator: mem.Allocator) -> Value {
	structure := new(Structure_Data, allocator)
	structure^ = Structure_Data {
		allocator = allocator,
		refs = 1,
		name = strings.clone(name, allocator),
		fields = make(map[string]Value, 8, allocator),
	}
	return Value_Structure{data = structure}
}

value_object :: proc(type_name: string, allocator: mem.Allocator) -> Value {
	object := new(Object_Data, allocator)
	object^ = Object_Data {
		allocator = allocator,
		refs = 1,
		type_name = strings.clone(type_name, allocator),
		fields = make(map[string]Value, 8, allocator),
	}
	return Value_Object(object)
}

value_table :: proc(allocator: mem.Allocator) -> Value {
	table := new(Table_Data, allocator)
	table^ = Table_Data {
		allocator = allocator,
		refs = 1,
		rows = make([dynamic]Value, 0, 4, allocator),
	}
	return Value_Table(table)
}

value_table_iterator :: proc(
	table_value: Value,
	allocator: mem.Allocator = context.allocator,
	filters: []Table_Component = nil,
) -> Value {
	table := value_table_data(table_value)
	table_retain(table)
	iterator := new(Table_Iterator_Data, allocator)
	iterator^ = Table_Iterator_Data {
		allocator = allocator,
		refs = 1,
		table = table,
		filters = make([dynamic]Table_Component, 0, len(filters), allocator),
	}
	for filter in filters {
		append(&iterator.filters, table_component_clone(filter, allocator))
	}
	return Value_Table_Iterator(iterator)
}

value_iterator_advanced :: proc(iterator_value: Value, allocator: mem.Allocator) -> Value {
	iterator := value_iterator_data(iterator_value)
	if iterator == nil {
		return value_initial()
	}
	return value_iterator_advanced_to(iterator_value, iterator.index + 1, iterator.matched, allocator)
}

value_iterator_advanced_to :: proc(
	iterator_value: Value,
	index: int,
	matched: bool,
	allocator: mem.Allocator = context.allocator,
) -> Value {
	iterator := value_iterator_data(iterator_value)
	if iterator == nil {
		return value_initial()
	}
	table_retain(iterator.table)
	next := new(Table_Iterator_Data, allocator)
	next^ = Table_Iterator_Data {
		allocator = allocator,
		refs = 1,
		table = iterator.table,
		index = index,
		matched = matched,
		filters = make([dynamic]Table_Component, 0, len(iterator.filters), allocator),
	}
	for filter in iterator.filters {
		append(&next.filters, table_component_clone(filter, allocator))
	}
	return Value_Table_Iterator(next)
}

value_clone :: proc(value: Value, allocator: mem.Allocator) -> Value {
	#partial switch v in value {
	case Value_String:
		return Value_String(strings.clone(string(v), allocator))
	case Value_Structure:
		structure_retain(v.data)
		return value
	case Value_Object:
		object_retain((^Object_Data)(v))
		return value
	case Value_Table:
		table_retain((^Table_Data)(v))
		return value
	case Value_Table_Iterator:
		iterator_retain((^Table_Iterator_Data)(v))
		return value
	case Value_Reference:
		reference_retain((^Reference_Data)(v))
		return value
	case Value_Initial, Value_World, Value_Integer, Value_Decimal, Value_Float, Value_Predicate:
		return value
	}
	return value_initial()
}

value_deep_clone :: proc(value: Value, allocator: mem.Allocator) -> Value {
	#partial switch v in value {
	case Value_Structure:
		if v.data == nil {
			return value_initial()
		}
		out := value_structure(v.data.name, allocator)
		out_structure := value_structure_data(out)
		assert(out_structure != nil)
		for key, field in v.data.fields {
			field_clone := value_deep_clone(field, allocator)
			structure_set_field(out_structure, key, field_clone)
			value_destroy(&field_clone)
		}
		return out
	case Value_Table:
		table := (^Table_Data)(v)
		out := value_table(allocator)
		out_table := value_table_data(out)
		assert(out_table != nil)
		if table == nil {
			return out
		}
		for row in table.rows {
			append(&out_table.rows, value_deep_clone(row, allocator))
		}
		return out
	case Value_Table_Iterator:
		return value_clone(value, allocator)
	case Value_Object:
		return value_clone(value, allocator)
	case Value_Reference:
		reference := (^Reference_Data)(v)
		if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_deep_clone(reference_borrow(reference), allocator)
		}
		return value_clone(value, allocator)
	case Value_String, Value_Initial, Value_World, Value_Integer, Value_Decimal, Value_Float, Value_Predicate:
		return value_clone(value, allocator)
	}
	return value_initial()
}

value_destroy :: proc(value: ^Value) {
	#partial switch v in value^ {
	case Value_String:
		delete(string(v))
	case Value_Structure:
		structure_release(v.data)
	case Value_Object:
		object_release((^Object_Data)(v))
	case Value_Table:
		table_release((^Table_Data)(v))
	case Value_Table_Iterator:
		iterator_release((^Table_Iterator_Data)(v))
	case Value_Reference:
		reference_release((^Reference_Data)(v))
	}
	value^ = {}
}

value_integer :: proc(value: Value) -> (i64, bool) {
	#partial switch v in value {
	case Value_Integer:
		return i64(v), true
	case Value_Decimal:
		coefficient := value_decimal_rescale_coefficient(v.coefficient, v.scale, 0)
		return i64(coefficient), coefficient >= i128(min(i64)) && coefficient <= i128(max(i64))
	case Value_Float:
		return i64(v), f64(i64(v)) == f64(v)
	case Value_Predicate:
		return 1 if bool(v) else 0, true
	case Value_Initial:
		return 0, true
	case Value_String:
		parsed, ok := strconv.parse_int(strings.trim_space(string(v)), 10)
		return i64(parsed), ok
	case Value_Reference:
		reference := (^Reference_Data)(v)
		if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_integer(reference_borrow(reference))
		}
		return 0, false
	case Value_World, Value_Structure, Value_Object, Value_Table, Value_Table_Iterator:
		return 0, false
	}
	return 0, true
}

value_truthy :: proc(value: Value) -> bool {
	#partial switch v in value {
	case Value_Initial:
		return false
	case Value_World:
		return true
	case Value_Structure:
		return true
	case Value_Object:
		return (^Object_Data)(v) != nil
	case Value_Table:
		table := (^Table_Data)(v)
		return table != nil && len(table.rows) > 0
	case Value_Table_Iterator:
		iterator := (^Table_Iterator_Data)(v)
		return iterator != nil && iterator.table != nil && iterator.index < len(iterator.table.rows)
	case Value_Reference:
		reference := (^Reference_Data)(v)
		if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_truthy(reference_borrow(reference))
		}
		return reference != nil && reference.target_kind != .None
	case Value_Integer:
		return i64(v) != 0
	case Value_Decimal:
		return v.coefficient != 0
	case Value_Float:
		return f64(v) != 0
	case Value_Predicate:
		return bool(v)
	case Value_String:
		return strings.trim_space(string(v)) != ""
	}
	return false
}

value_is_initial :: proc "contextless" (value: Value) -> bool {
	#partial switch v in value {
	case Value_Initial:
		return true
	case Value_World:
		return false
	case Value_Structure:
		return false
	case Value_Object:
		return (^Object_Data)(v) == nil
	case Value_Table:
		table := (^Table_Data)(v)
		return table == nil || len(table.rows) == 0
	case Value_Table_Iterator:
		iterator := (^Table_Iterator_Data)(v)
		return iterator == nil ||
		       iterator.table == nil ||
		       iterator.index >= len(iterator.table.rows)
	case Value_Reference:
		reference := (^Reference_Data)(v)
		if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_is_initial(reference_borrow(reference))
		}
		return reference == nil || reference.target_kind == .None
	case Value_Integer:
		return i64(v) == 0
	case Value_Decimal:
		return v.coefficient == 0
	case Value_Float:
		return f64(v) == 0
	case Value_Predicate:
		return !bool(v)
	case Value_String:
		return string(v) == ""
	}
	return true
}

value_has_scalar_text :: proc "contextless" (value: Value) -> bool {
	#partial switch _ in value {
	case Value_Initial, Value_Integer, Value_Decimal, Value_Float, Value_Predicate, Value_String:
		return true
	case Value_Reference:
		if reference := value_reference_data(value); reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_has_scalar_text(reference_borrow(reference))
		}
		return false
	case Value_World, Value_Structure, Value_Object, Value_Table, Value_Table_Iterator:
		return false
	}
	return true
}

value_scalar_text :: proc(value: Value, allocator: mem.Allocator) -> (string, bool) {
	#partial switch v in value {
	case Value_Initial:
		return "", true
	case Value_Integer:
		buf: [32]byte
		return strings.clone(strconv.write_int(buf[:], i64(v), 10), allocator), true
	case Value_Decimal:
		return value_decimal_text(v, allocator), true
	case Value_Float:
		buf: [128]byte
		text := strconv.write_float(buf[:], f64(v), 'g', -1, 64)
		if len(text) > 0 && text[0] == '+' {
			text = text[1:]
		}
		return strings.clone(text, allocator), true
	case Value_Predicate:
		return "X" if bool(v) else "", true
	case Value_String:
		return string(v), true
	case Value_Reference:
		reference := (^Reference_Data)(v)
		if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_scalar_text(reference_borrow(reference), allocator)
		}
		return "", false
	case Value_World, Value_Structure, Value_Object, Value_Table, Value_Table_Iterator:
		return "", false
	}
	return "", true
}

value_write_scalar_text :: proc(out: ^strings.Builder, value: Value) -> bool {
	#partial switch v in value {
	case Value_Initial:
		return true
	case Value_Integer:
		buf: [32]byte
		strings.write_string(out, strconv.write_int(buf[:], i64(v), 10))
		return true
	case Value_Decimal:
		text := value_decimal_text(v, context.temp_allocator)
		strings.write_string(out, text)
		return true
	case Value_Float:
		buf: [128]byte
		text := strconv.write_float(buf[:], f64(v), 'g', -1, 64)
		if len(text) > 0 && text[0] == '+' {
			text = text[1:]
		}
		strings.write_string(out, text)
		return true
	case Value_Predicate:
		if bool(v) {
			strings.write_byte(out, 'X')
		}
		return true
	case Value_String:
		strings.write_string(out, string(v))
		return true
	case Value_Reference:
		reference := (^Reference_Data)(v)
		if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
			return value_write_scalar_text(out, reference_borrow(reference))
		}
		return false
	case Value_World, Value_Structure, Value_Object, Value_Table, Value_Table_Iterator:
		return false
	}
	return true
}

structure_is_system :: #force_inline proc "contextless" (value: Value) -> bool {
	#partial switch v in value {
	case Value_Structure:
		return v.name == "sy" ||
		       v.name == "syst" ||
		       (v.data != nil && (v.data.name == "sy" || v.data.name == "syst"))
	}
	return false
}
