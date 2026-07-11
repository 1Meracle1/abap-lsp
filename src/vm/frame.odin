package abap_frontend_vm

import ir "src:ir"
import runtime "src:vm/runtime"

import "core:mem"

Frame :: struct {
	function_id:      ir.Function_Id,
	function:         ^Prepared_Function,
	ip:               u32,
	call_source:      runtime.Source_Loc,
	allocator:        mem.Allocator,
	registers:        []runtime.Value,
	slots:            []runtime.Value,
	slot_cells:       []^runtime.Cell_Data,
	return_registers: []Register,
}

vm_push_function :: proc(
	vm: ^VM,
	id: ir.Function_Id,
	args: []runtime.Value,
	return_registers: []Register,
	source: runtime.Source_Loc,
) -> bool {
	if id == ir.INVALID_FUNCTION_ID || int(id) >= len(vm.module.functions) {
		vm_trap(vm, .Invalid_Function, "call target function is invalid", source)
		return false
	}
	function := &vm.module.functions[int(id)]
	frame := Frame {
		function_id = id,
		function = function,
		call_source = source,
		allocator = vm.allocator,
		registers = make([]runtime.Value, function.frame_slot_count, vm.allocator),
		slots = make([]runtime.Value, len(function.slots), vm.allocator),
		slot_cells = make([]^runtime.Cell_Data, len(function.slots), vm.allocator),
		return_registers = return_registers,
	}
	for slot_index in 0 ..< len(frame.slot_cells) {
		frame.slot_cells[slot_index] = runtime.cell_make(runtime.value_initial(), frame.allocator)
	}
	for arg, i in args {
		if i >= len(function.block_params) {
			break
		}
		set_register(&frame, function.block_params[i], arg)
	}
	if len(args) == 0 && len(function.block_params) > 0 {
		set_register(&frame, function.block_params[0], vm.runtime_context.world)
	}
	frame_bind_slots(&frame, args)
	append(&vm.frames, frame)
	return true
}

frame_bind_slots :: proc(frame: ^Frame, args: []runtime.Value) {
	arg_index := 0
	if len(args) > 0 && runtime.value_kind(args[0]) == runtime.Value_Kind.World {
		arg_index = 1
	}
	for slot, slot_index in frame.function.slots {
		#partial switch slot.kind {
		case .Instance, .Parameter:
			if arg_index >= len(args) {
				return
			}
			if cell := frame_slot_cell(frame, slot_index); cell != nil {
				runtime.cell_write(cell, args[arg_index])
			}
			arg_index += 1
		}
	}
}

frame_output_value :: proc "contextless" (
	frame: ^Frame,
	index: int,
	count: int,
) -> (runtime.Value, bool) {
	if index < 0 || count <= 0 || index >= count {
		return {}, false
	}
	param_count := 0
	for slot in frame.function.slots {
		if slot.kind == .Parameter {
			param_count += 1
		}
	}
	start := param_count - count
	if start < 0 {
		return {}, false
	}
	param_index := 0
	for slot, slot_index in frame.function.slots {
		if slot.kind != .Parameter {
			continue
		}
		if param_index == start + index {
			if cell := frame_slot_cell(frame, slot_index); cell != nil {
				return cell.value, true
			}
			return {}, false
		}
		param_index += 1
	}
	return {}, false
}

slot_debug :: #force_inline proc "contextless" (function: ^Prepared_Function, payload: u32) -> (Slot_Debug, bool) {
	if int(payload) >= len(function.slots) {
		return {}, false
	}
	return function.slots[int(payload)], true
}

field_debug :: #force_inline proc "contextless" (function: ^Prepared_Function, payload: u32) -> (Field_Ref, bool) {
	if int(payload) >= len(function.fields) {
		return {}, false
	}
	return function.fields[int(payload)], true
}

get_operand :: #force_inline proc "contextless" (
	frame: ^Frame,
	instruction: Prepared_Instruction,
	index: int,
) -> runtime.Value {
	if index < 0 || index >= int(instruction.operand_count) {
		return {}
	}
	register := frame.function.operand_registers[int(instruction.operand_start) + index]
	return register_value(frame, register)
}

set_operand :: proc(frame: ^Frame, instruction: Prepared_Instruction, index: int, value: runtime.Value) {
	if index < 0 || index >= int(instruction.operand_count) {
		return
	}
	register := frame.function.operand_registers[int(instruction.operand_start) + index]
	set_register(frame, register, value)
}

set_result :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
	index: int,
	value: runtime.Value,
) {
	if index < 0 || index >= int(instruction.result_count) {
		vm_trap(vm, .Invalid_Instruction, "result register index is invalid", instruction.source)
		return
	}
	register := frame.function.result_registers[int(instruction.result_start) + index]
	set_register(frame, register, value)
}

register_value :: #force_inline proc "contextless" (frame: ^Frame, register: Register) -> runtime.Value {
	if register == INVALID_REGISTER || int(register) >= len(frame.registers) {
		return {}
	}
	return frame.registers[int(register)]
}

set_register :: proc(frame: ^Frame, register: Register, value: runtime.Value) {
	if register == INVALID_REGISTER || int(register) >= len(frame.registers) {
		return
	}
	new_value := runtime.value_clone(value, frame.allocator)
	runtime.value_destroy(&frame.registers[int(register)])
	frame.registers[int(register)] = new_value
}

result_type_name :: #force_inline proc "contextless" (
	function: ^Prepared_Function,
	instruction: Prepared_Instruction,
	index: int,
) -> string {
	if index < 0 || index >= int(instruction.result_count) {
		return ""
	}
	register := function.result_registers[int(instruction.result_start) + index]
	if register == INVALID_REGISTER || int(register) >= len(function.values) {
		return ""
	}
	return function.values[int(register)].type_name
}

result_type_descriptor :: #force_inline proc "contextless" (
	function: ^Prepared_Function,
	instruction: Prepared_Instruction,
	index: int,
) -> runtime.Type_Descriptor {
	if index < 0 || index >= int(instruction.result_count) {
		return nil
	}
	register := function.result_registers[int(instruction.result_start) + index]
	if register == INVALID_REGISTER || int(register) >= len(function.values) {
		return nil
	}
	return function.values[int(register)].type
}

result_reference_target_descriptor :: proc "contextless" (
	module: ^Prepared_Module,
	function: ^Prepared_Function,
	instruction: Prepared_Instruction,
	index: int,
) -> runtime.Type_Descriptor {
	descriptor := result_type_descriptor(function, instruction, index)
	if descriptor == nil || descriptor.family != .Reference || module == nil {
		return nil
	}
	target := descriptor.reference.target_type
	if target == ir.INVALID_TYPE_ID || int(target) < 0 || int(target) >= len(module.type_descriptors) {
		return nil
	}
	return &module.type_descriptors[int(target)]
}

exec_call :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	target := instruction.call_target
	args := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
	for i in 0 ..< int(instruction.operand_count) {
		args[i] = get_operand(frame, instruction, i)
	}
	return_registers := make([]Register, instruction.result_count, vm.allocator)
	for i in 0 ..< int(instruction.result_count) {
		return_registers[i] = frame.function.result_registers[int(instruction.result_start) + i]
	}
	frame.ip += 1
	vm_push_function(vm, target, args[:], return_registers, instruction.source)
}

exec_invoke :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	if instruction.call_target != ir.INVALID_FUNCTION_ID {
		exec_invoke_function_call(vm, frame, instruction, instruction.call_target)
		return
	}
	if target, ok := invoke_intrinsic_function_target(instruction); ok {
		exec_invoke_function_call(vm, frame, instruction, target)
		return
	}

	frame_index := len(vm.frames) - 1
	frame_count := len(vm.frames)
	if vm.options.dispatcher.dispatch != nil {
		switch dispatch_external_intrinsic(vm, frame, instruction) {
		case .Ok:
			if vm.state != .Trapped && len(vm.frames) == frame_count {
				exec_invoke_transfer(vm, &vm.frames[frame_index], instruction)
			}
			return
		case .Trap:
			return
		case .Unsupported:
		}
	}

	dispatch_intrinsic(vm, frame, instruction)
	if vm.state != .Trapped && len(vm.frames) == frame_count {
		exec_invoke_transfer(vm, &vm.frames[frame_index], instruction)
	}
}

exec_invoke_function_call :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
	target: ir.Function_Id,
) {
	args := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
	for i in 0 ..< int(instruction.operand_count) {
		args[i] = get_operand(frame, instruction, i)
	}
	return_registers := call_return_registers(frame, instruction, vm.allocator)
	vm_push_function(vm, target, args[:], return_registers, instruction.source)
}

invoke_intrinsic_function_target :: proc "contextless" (instruction: Prepared_Instruction) -> (ir.Function_Id, bool) {
	#partial switch instruction.intrinsic_op {
	case .Call_Routine, .Call_Method:
		payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_Call_Payload)
		if payload_ok && payload.has_call_function_target {
			return payload.call_function_target, true
		}
	}
	return ir.INVALID_FUNCTION_ID, false
}

exec_invoke_transfer :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	if runtime.context_trapped(&vm.runtime_context) {
		vm_sync_runtime_trap(vm)
		return
	}
	edge_index := 0
	if vm.runtime_context.exception.raised {
		edge_index = 1
	}
	exec_branch(vm, frame, instruction, edge_index)
}

exec_branch :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction, relative_edge_index: int) {
	if relative_edge_index < 0 || relative_edge_index >= int(instruction.edge_count) {
		vm_trap(vm, .Invalid_Instruction, "branch edge index is invalid", instruction.source)
		return
	}
	edge_index := int(instruction.edge_start) + relative_edge_index
	if edge_index < 0 || edge_index >= len(frame.function.edges) {
		vm_trap(vm, .Invalid_Instruction, "branch edge is invalid", instruction.source)
		return
	}
	edge := frame.function.edges[edge_index]
	values := make([]runtime.Value, edge.arg_count, context.temp_allocator)
	for i in 0 ..< int(edge.arg_count) {
		reg := frame.function.edge_registers[int(edge.arg_start) + i]
		values[i] = register_value(frame, reg)
	}
	for i in 0 ..< int(edge.param_count) {
		if i >= len(values) {
			vm_trap(vm, .Invalid_Instruction, "branch argument count does not match target parameters", instruction.source)
			return
		}
		reg := frame.function.edge_registers[int(edge.param_start) + i]
		set_register(frame, reg, values[i])
	}
	frame.ip = u32(edge.target)
}

exec_switch :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	selector := get_operand(frame, instruction, 0)
	for i in 1 ..< int(instruction.edge_count) {
		edge := frame.function.edges[int(instruction.edge_start) + i]
		if edge.case_value == INVALID_REGISTER {
			vm_trap(vm, .Invalid_Instruction, "switch case is missing its value", instruction.source)
			return
		}
		matched, ok := runtime.abap_compare(
			&vm.runtime_context,
			.Equal,
			selector,
			register_value(frame, edge.case_value),
			instruction_runtime_source(instruction),
		)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		is_match := runtime.value_truthy(matched)
		runtime.value_destroy(&matched)
		if is_match {
			exec_branch(vm, frame, instruction, i)
			return
		}
	}
	exec_branch(vm, frame, instruction, 0)
}

exec_return :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	values := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
	for i in 0 ..< int(instruction.operand_count) {
		values[i] = get_operand(frame, instruction, i)
	}
	completed := pop(&vm.frames)
	defer frame_destroy(&completed)

	if len(vm.frames) == 0 {
		return
	}
	caller := &vm.frames[len(vm.frames) - 1]
	output_count := len(completed.return_registers) - len(values)
	for reg, i in completed.return_registers {
		value := runtime.Value{}
		if i < len(values) {
			value = values[i]
		} else {
			output, output_ok := frame_output_value(&completed, i - len(values), output_count)
			if !output_ok {
				vm_trap(vm, .Invalid_Instruction, "return value count does not match call results", instruction.source)
				return
			}
			value = output
		}
		set_register(caller, reg, value)
	}
	if int(caller.ip) < len(caller.function.instructions) {
		caller_instruction := caller.function.instructions[int(caller.ip)]
		if caller_instruction.opcode == .Invoke {
			exec_invoke_transfer(vm, caller, caller_instruction)
		}
	}
}

exec_trap :: proc(vm: ^VM, instruction: Prepared_Instruction) {
	message := instruction.trap_message
	if message == "" {
		message = "IR trap"
	}
	vm_trap(vm, .Unsupported, message, instruction.source)
}

frame_destroy :: proc(frame: ^Frame) {
	for &value in frame.registers {
		runtime.value_destroy(&value)
	}
	delete(frame.registers)
	for &value in frame.slots {
		runtime.value_destroy(&value)
	}
	delete(frame.slots)
	for cell in frame.slot_cells {
		runtime.cell_release(cell)
	}
	delete(frame.slot_cells)
	delete(frame.return_registers)
	frame^ = {}
}
