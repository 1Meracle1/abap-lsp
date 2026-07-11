package abap_frontend_vm_runtime

import "core:mem"
import "core:strings"

cell_make :: proc(value: Value, allocator: mem.Allocator = context.allocator) -> ^Cell_Data {
	cell := new(Cell_Data, allocator)
	cell^ = Cell_Data {
		allocator = allocator,
		refs = 1,
		value = value_clone(value, allocator),
	}
	return cell
}

cell_retain :: #force_inline proc "contextless" (cell: ^Cell_Data) {
	if cell != nil {
		cell.refs += 1
	}
}

cell_release :: proc(cell: ^Cell_Data) {
	if cell == nil {
		return
	}
	assert(cell.refs > 0)
	cell.refs -= 1
	if cell.refs > 0 {
		return
	}
	value_destroy(&cell.value)
	mem.free(rawptr(cell), cell.allocator)
}

cell_write :: proc(cell: ^Cell_Data, value: Value) {
	assert(cell != nil)
	new_value := value_storage_clone(value, cell.allocator)
	value_destroy(&cell.value)
	cell.value = new_value
}

cell_bind :: proc(cell: ^Cell_Data, value: Value) {
	assert(cell != nil)
	new_value := value_clone(value, cell.allocator)
	value_destroy(&cell.value)
	cell.value = new_value
}

reference_retain :: #force_inline proc "contextless" (reference: ^Reference_Data) {
	if reference != nil {
		reference.refs += 1
	}
}

reference_release :: proc(reference: ^Reference_Data) {
	if reference == nil {
		return
	}
	assert(reference.refs > 0)
	reference.refs -= 1
	if reference.refs > 0 {
		return
	}
	cell_release(reference.cell)
	delete(reference.name, reference.allocator)
	value_destroy(&reference.base)
	delete(reference.field_name, reference.allocator)
	mem.free(rawptr(reference), reference.allocator)
}

value_alias_cell :: proc(cell: ^Cell_Data, allocator: mem.Allocator = context.allocator) -> Value {
	return value_reference(.Alias, .Cell, allocator, cell = cell)
}

value_data_reference_cell :: proc(cell: ^Cell_Data, allocator: mem.Allocator = context.allocator) -> Value {
	return value_reference(.Data, .Cell, allocator, cell = cell)
}

value_alias_global :: proc(ctx: ^Context, name: string, allocator: mem.Allocator = context.allocator) -> Value {
	return value_reference(.Alias, .Global, allocator, ctx = ctx, name = name)
}

value_reference_unassigned :: proc(mode: Reference_Mode, allocator: mem.Allocator = context.allocator) -> Value {
	return value_reference(mode, .None, allocator)
}

value_field_symbol_binding_from_alias :: proc(value: Value, allocator: mem.Allocator = context.allocator) -> (Value, bool) {
	reference := value_reference_data(value)
	if reference == nil || reference.mode != .Alias || reference.target_kind == .None {
		return {}, false
	}
	return value_reference_from_existing(reference, .Binding, allocator), true
}

value_data_reference_from_alias :: proc(value: Value, allocator: mem.Allocator = context.allocator) -> (Value, bool) {
	reference := value_reference_data(value)
	if reference == nil || reference.mode != .Alias || reference.target_kind == .None {
		return {}, false
	}
	return value_reference_from_existing(reference, .Data, allocator), true
}

value_alias_from_data_reference :: proc(value: Value, allocator: mem.Allocator = context.allocator) -> (Value, bool) {
	reference := value_reference_data(value)
	if reference == nil || reference.mode != .Data || reference.target_kind == .None {
		return {}, false
	}
	return value_reference_from_existing(reference, .Alias, allocator), true
}

value_reference_field :: proc(
	base: Value,
	field_name: string,
	result_type: Type_Descriptor,
	mode: Reference_Mode,
	allocator: mem.Allocator = context.allocator,
) -> Value {
	reference := new(Reference_Data, allocator)
	reference^ = Reference_Data {
		allocator = allocator,
		refs = 1,
		mode = mode,
		target_kind = .Field,
		base = value_clone(base, allocator),
		field_name = strings.clone(field_name, allocator),
		result_type = result_type,
	}
	return Value_Reference(reference)
}

value_reference :: proc(
	mode: Reference_Mode,
	target_kind: Reference_Target_Kind,
	allocator: mem.Allocator,
	cell: ^Cell_Data = nil,
	ctx: ^Context = nil,
	name: string = "",
) -> Value {
	reference := new(Reference_Data, allocator)
	reference^ = Reference_Data {
		allocator = allocator,
		refs = 1,
		mode = mode,
		target_kind = target_kind,
		cell = cell,
		ctx = ctx,
		name = strings.clone(name, allocator),
	}
	cell_retain(cell)
	return Value_Reference(reference)
}

value_reference_from_existing :: proc(
	source: ^Reference_Data,
	mode: Reference_Mode,
	allocator: mem.Allocator,
) -> Value {
	assert(source != nil)
	reference := new(Reference_Data, allocator)
	reference^ = Reference_Data {
		allocator = allocator,
		refs = 1,
		mode = mode,
		target_kind = source.target_kind,
		cell = source.cell,
		ctx = source.ctx,
		name = strings.clone(source.name, allocator),
		base = value_clone(source.base, allocator),
		field_name = strings.clone(source.field_name, allocator),
		result_type = source.result_type,
	}
	cell_retain(reference.cell)
	return Value_Reference(reference)
}

map_set_value :: proc(
	values: ^map[string]Value,
	name: string,
	value: Value,
	allocator: mem.Allocator,
) {
	new_value := value_storage_clone(value, allocator)
	if existing, ok := values^[name]; ok {
		value_destroy(&existing)
		values^[name] = new_value
		return
	}
	owned_key := strings.clone(name, allocator)
	values^[owned_key] = new_value
}

map_set_value_raw :: proc(
	values: ^map[string]Value,
	name: string,
	value: Value,
	allocator: mem.Allocator,
) {
	new_value := value_clone(value, allocator)
	if existing, ok := values^[name]; ok {
		value_destroy(&existing)
		values^[name] = new_value
		return
	}
	owned_key := strings.clone(name, allocator)
	values^[owned_key] = new_value
}

map_value_or_initial :: #force_inline proc "contextless" (values: map[string]Value, name: string) -> Value {
	if value, ok := values[name]; ok {
		return value
	}
	return value_initial()
}

value_initial :: #force_inline proc "contextless" () -> Value {
	return Value_Initial{}
}

value_reference_data :: proc "contextless" (value: Value) -> ^Reference_Data {
	#partial switch v in value {
	case Value_Reference:
		return (^Reference_Data)(v)
	}
	return nil
}

value_reference_mode :: proc "contextless" (value: Value) -> (Reference_Mode, bool) {
	if reference := value_reference_data(value); reference != nil {
		return reference.mode, true
	}
	return .Alias, false
}

value_is_alias_reference :: proc "contextless" (value: Value) -> bool {
	reference := value_reference_data(value)
	return reference != nil && (reference.mode == .Alias || reference.mode == .Binding)
}

value_is_data_reference :: proc "contextless" (value: Value) -> bool {
	reference := value_reference_data(value)
	return reference != nil && reference.mode == .Data
}

reference_borrow :: proc "contextless" (reference: ^Reference_Data, depth := 0) -> Value {
	if reference == nil || depth > 32 {
		return value_initial()
	}
	switch reference.target_kind {
	case .None:
		return value_initial()
	case .Cell:
		if reference.cell == nil {
			return value_initial()
		}
		return reference.cell.value
	case .Global:
		if reference.ctx == nil {
			return value_initial()
		}
		return context_global_read(reference.ctx, reference.name)
	case .Field:
		base := value_borrow_alias(reference.base, depth + 1)
		if structure := value_structure_data(base); structure != nil {
			if value, ok := structure.fields[reference.field_name]; ok {
				return value
			}
			return value_initial()
		}
		if object := value_object_data(base); object != nil {
			if value, ok := object.fields[reference.field_name]; ok {
				return value
			}
			return value_initial()
		}
	}
	return value_initial()
}

value_borrow_alias :: proc "contextless" (value: Value, depth := 0) -> Value {
	reference := value_reference_data(value)
	if reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
		return reference_borrow(reference, depth + 1)
	}
	return value
}

value_read_alias :: proc(value: Value, allocator: mem.Allocator = context.allocator) -> Value {
	if reference := value_reference_data(value); reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
		return value_clone(reference_borrow(reference), allocator)
	}
	return value_clone(value, allocator)
}

value_storage_clone :: proc(value: Value, allocator: mem.Allocator = context.allocator) -> Value {
	if reference := value_reference_data(value); reference != nil && (reference.mode == .Alias || reference.mode == .Binding) {
		return value_clone(reference_borrow(reference), allocator)
	}
	return value_clone(value, allocator)
}

reference_write :: proc(
	reference: ^Reference_Data,
	value: Value,
	source: Source_Loc = {},
) -> bool {
	if reference == nil || reference.mode != .Alias {
		return false
	}
	switch reference.target_kind {
	case .None:
		return false
	case .Cell:
		if reference.cell == nil {
			return false
		}
		cell_write(reference.cell, value)
		return true
	case .Global:
		if reference.ctx == nil {
			return false
		}
		context_global_write(reference.ctx, reference.name, value)
		return true
	case .Field:
		return reference_write_field(reference, value)
	}
	return false
}

reference_write_field :: proc(
	reference: ^Reference_Data,
	value: Value,
) -> bool {
	base := value_borrow_alias(reference.base)
	if structure := value_structure_data(base); structure != nil {
		structure_set_field(structure, reference.field_name, value)
		return true
	}
	if object := value_object_data(base); object != nil {
		object_set_field(object, reference.field_name, value)
		return true
	}
	return false
}

reference_materialize_field :: proc(
	reference: ^Reference_Data,
	result_type: Type_Descriptor,
) -> bool {
	if reference == nil || reference.mode != .Alias || reference.target_kind != .Field {
		return true
	}
	base := value_borrow_alias(reference.base)
	if structure := value_structure_data(base); structure != nil {
		if _, ok := structure.fields[reference.field_name]; !ok {
			initial := initial_for_type(result_type, structure.allocator)
			structure_set_field(structure, reference.field_name, initial)
			value_destroy(&initial)
		}
		return true
	}
	if object := value_object_data(base); object != nil {
		if _, ok := object.fields[reference.field_name]; !ok {
			initial := initial_for_type(result_type, object.allocator)
			object_set_field(object, reference.field_name, initial)
			value_destroy(&initial)
		}
		return true
	}
	return false
}
