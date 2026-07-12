package abap_frontend_vm

import runtime "src:vm/runtime"

exec_alloca :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	initial := runtime.initial_for_type(
		result_reference_target_descriptor(vm.module, frame.function, instruction, 1),
		vm.allocator,
	)
	defer runtime.value_destroy(&initial)
	cell := runtime.cell_make(initial, vm.allocator)
	address := runtime.value_alias_cell(cell, vm.allocator)
	runtime.cell_release(cell)
	defer runtime.value_destroy(&address)
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	set_result(vm, frame, instruction, 1, address)
	frame.ip += 1
}

exec_addr_of :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	slot, ok := slot_debug(frame.function, instruction.payload)
	if !ok {
		vm_trap(vm, .Invalid_Instruction, "slot index is invalid", instruction.source)
		return
	}
	assert(slot.type != nil, "runtime slots require type descriptors")
	value := runtime.Value{}
	value_owned := false
	slot_index := int(instruction.payload)
	#partial switch slot.kind {
	case .Global:
		if slot.is_field_symbol {
			value = runtime.context_global_read(&vm.runtime_context, slot.name)
		} else {
			if runtime.value_kind(runtime.context_global_read(&vm.runtime_context, slot.name)) == .Initial &&
			   slot.type.family != .Numeric {
				initial := runtime.initial_for_type(slot.type, vm.allocator)
				defer runtime.value_destroy(&initial)
				if runtime.value_kind(initial) != .Initial {
					runtime.context_global_write(&vm.runtime_context, slot.name, initial)
				}
			}
			value = runtime.value_alias_global(&vm.runtime_context, slot.name, vm.allocator)
			value_owned = true
		}
	case .Runtime:
		value = runtime.context_runtime_read(&vm.runtime_context, slot.name)
	case:
		cell := frame_slot_cell(frame, slot_index)
		if cell == nil {
			vm_trap(vm, .Invalid_Instruction, "slot cell is invalid", instruction.source)
			return
		}
		if slot.is_field_symbol {
			value = cell.value
		} else {
			// Generic numeric parameters and results have no concrete initial
			// representation. Calls populate them before use or assignment.
			if runtime.value_kind(cell.value) == .Initial && slot.type.family != .Numeric {
				initial := runtime.initial_for_type(slot.type, vm.allocator)
				defer runtime.value_destroy(&initial)
				if runtime.value_kind(initial) != .Initial {
					runtime.cell_write(cell, initial)
				}
			}
			value = runtime.value_alias_cell(cell, vm.allocator)
			value_owned = true
		}
	}
	set_result(vm, frame, instruction, 0, value)
	if value_owned {
		runtime.value_destroy(&value)
	}
	frame.ip += 1
}

exec_deref :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	value, ok := runtime.value_alias_from_data_reference(get_operand(frame, instruction, 0), vm.allocator)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_trap(vm, .Type, "deref operand is not a data reference", instruction.source)
		return
	}
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

exec_field_addr :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	field, ok := field_debug(frame.function, instruction.payload)
	if !ok {
		vm_trap(vm, .Invalid_Instruction, "field index is invalid", instruction.source)
		return
	}
	request := runtime.Field_Request {
		base = get_operand(frame, instruction, 0),
		name = field.name,
		result_type = field.result_type,
	}
	value, value_ok := runtime.context_field_load(
		&vm.runtime_context,
		request,
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&value)
	if !value_ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

exec_load :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 1))
	frame.ip += 1
}

exec_store :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	if instruction.address_kind != .Slot {
		exec_store_address(vm, frame, instruction)
		return
	}
	slot, ok := slot_debug(frame.function, instruction.payload)
	if !ok {
		vm_trap(vm, .Invalid_Instruction, "slot index is invalid", instruction.source)
		return
	}
	value := get_operand(frame, instruction, 2)
	slot_index := int(instruction.payload)
	#partial switch slot.kind {
	case .Global:
		if slot.is_field_symbol {
			if !store_global_field_symbol(vm, slot, value, instruction.source) {
				return
			}
		} else {
			runtime.context_global_write(&vm.runtime_context, slot.name, value)
		}
	case .Runtime:
		if !runtime.context_runtime_write(
			&vm.runtime_context,
			slot.name,
			value,
			instruction_runtime_source(instruction),
		) {
			vm_sync_runtime_trap(vm)
			return
		}
	case:
		cell := frame_slot_cell(frame, slot_index)
		if cell == nil {
			vm_trap(vm, .Invalid_Instruction, "slot cell is invalid", instruction.source)
			return
		}
		if slot.is_field_symbol {
			if !store_cell_field_symbol(vm, cell, value, instruction.source) {
				return
			}
		} else {
			runtime.cell_write(cell, value)
		}
	}
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	frame.ip += 1
}

exec_store_address :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	address := get_operand(frame, instruction, 1)
	value := get_operand(frame, instruction, 2)
	reference := runtime.value_reference_data(address)
	if reference == nil || !runtime.reference_write(reference, value, instruction_runtime_source(instruction)) {
		vm_trap(vm, .Type, "store address is not writable", instruction.source)
		return
	}
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	frame.ip += 1
}

exec_struct_init :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	typ := result_type_descriptor(frame.function, instruction, 0)
	value := runtime.initial_for_type(typ, vm.allocator)
	defer runtime.value_destroy(&value)
	structure := runtime.value_structure_data(value)
	if structure == nil || typ == nil {
		vm_trap(vm, .Type, "struct_init result is not a structure", instruction.source)
		return
	}
	for operand_index in 0 ..< int(instruction.operand_count) {
		if operand_index >= len(typ.structure.fields) {
			vm_trap(vm, .Invalid_Instruction, "struct_init has more operands than fields", instruction.source)
			return
		}
		field := typ.structure.fields[operand_index]
		runtime.structure_set_field(structure, field.name, get_operand(frame, instruction, operand_index))
	}
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

exec_extract_value :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	field, ok := field_debug(frame.function, instruction.payload)
	if !ok {
		vm_trap(vm, .Invalid_Instruction, "field index is invalid", instruction.source)
		return
	}
	request := runtime.Field_Request {
		base = get_operand(frame, instruction, 0),
		name = field.name,
		result_type = field.result_type,
	}
	value, value_ok := runtime.context_field_load(
		&vm.runtime_context,
		request,
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&value)
	if !value_ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

frame_slot_cell :: #force_inline proc "contextless" (frame: ^Frame, slot_index: int) -> ^runtime.Cell_Data {
	if slot_index < 0 || slot_index >= len(frame.slot_cells) {
		return nil
	}
	return frame.slot_cells[slot_index]
}

store_cell_field_symbol :: proc(
	vm: ^VM,
	cell: ^runtime.Cell_Data,
	value: runtime.Value,
	source: runtime.Source_Loc,
) -> bool {
	if reference := runtime.value_reference_data(value); reference != nil && reference.mode == .Binding {
		bound := runtime.value_reference_from_existing(reference, .Alias, cell.allocator)
		defer runtime.value_destroy(&bound)
		runtime.cell_bind(cell, bound)
		return true
	}
	current := runtime.value_reference_data(cell.value)
	if current == nil || current.mode != .Alias || current.target_kind == .None {
		vm_trap(vm, .Type, "field symbol is not assigned", source)
		return false
	}
	if !runtime.reference_write(current, value, source) {
		vm_trap(vm, .Type, "field symbol target is not writable", source)
		return false
	}
	return true
}

store_global_field_symbol :: proc(
	vm: ^VM,
	slot: Slot_Debug,
	value: runtime.Value,
	source: runtime.Source_Loc,
) -> bool {
	if reference := runtime.value_reference_data(value); reference != nil && reference.mode == .Binding {
		bound := runtime.value_reference_from_existing(reference, .Alias, vm.allocator)
		defer runtime.value_destroy(&bound)
		runtime.context_global_bind(&vm.runtime_context, slot.name, bound)
		return true
	}
	current_value := runtime.context_global_read(&vm.runtime_context, slot.name)
	current := runtime.value_reference_data(current_value)
	if current == nil || current.mode != .Alias || current.target_kind == .None {
		vm_trap(vm, .Type, "field symbol is not assigned", source)
		return false
	}
	if !runtime.reference_write(current, value, source) {
		vm_trap(vm, .Type, "field symbol target is not writable", source)
		return false
	}
	return true
}
