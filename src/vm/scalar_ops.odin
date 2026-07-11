package abap_frontend_vm

import runtime "src:vm/runtime"

exec_const :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	if int(instruction.payload) >= len(frame.function.constants) {
		vm_trap(vm, .Invalid_Instruction, "constant index is invalid", instruction.source)
		return
	}
	literal := frame.function.constants[int(instruction.payload)]
	typ := result_type_descriptor(frame.function, instruction, 0)
	value := runtime.value_from_literal(literal, typ, vm.allocator)
	defer runtime.value_destroy(&value)
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

exec_initial :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	value := runtime.initial_for_type(result_type_descriptor(frame.function, instruction, 0), vm.allocator)
	defer runtime.value_destroy(&value)
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

exec_null_ref :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	set_result(vm, frame, instruction, 0, runtime.value_initial())
	frame.ip += 1
}

exec_integer_binary :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	left, left_ok := runtime.value_integer(get_operand(frame, instruction, 0))
	right, right_ok := runtime.value_integer(get_operand(frame, instruction, 1))
	if !left_ok || !right_ok {
		vm_trap(vm, .Type, "integer core operation requires integer operands", instruction.source)
		return
	}
	result: i64
	#partial switch instruction.opcode {
	case .Add:
		result = left + right
	case .Sub:
		result = left - right
	case .Mul:
		result = left * right
	case .Div:
		if right == 0 {
			vm_trap(vm, .Divide_By_Zero, "division by zero", instruction.source)
			return
		}
		result = left / right
	case .Mod:
		if right == 0 {
			vm_trap(vm, .Divide_By_Zero, "modulo by zero", instruction.source)
			return
		}
		result = left % right
	case:
		vm_trap(vm, .Invalid_Instruction, "integer core operation opcode is invalid", instruction.source)
		return
	}
	set_result(vm, frame, instruction, 0, runtime.value_integer_make(result))
	frame.ip += 1
}

exec_integer_negate :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	value, ok := runtime.value_integer(get_operand(frame, instruction, 0))
	if !ok {
		vm_trap(vm, .Type, "integer negate requires an integer operand", instruction.source)
		return
	}
	set_result(vm, frame, instruction, 0, runtime.value_integer_make(-value))
	frame.ip += 1
}

exec_logical_binary :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	left := runtime.value_truthy(get_operand(frame, instruction, 0))
	right := runtime.value_truthy(get_operand(frame, instruction, 1))
	result: bool
	#partial switch instruction.opcode {
	case .And:
		result = left && right
	case .Or:
		result = left || right
	case .Xor:
		result = left != right
	case:
		vm_trap(vm, .Invalid_Instruction, "logical core operation opcode is invalid", instruction.source)
		return
	}
	set_logical_result(vm, frame, instruction, result)
	frame.ip += 1
}

exec_logical_not :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	set_logical_result(vm, frame, instruction, !runtime.value_truthy(get_operand(frame, instruction, 0)))
	frame.ip += 1
}

set_logical_result :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction, result: bool) {
	typ := result_type_descriptor(frame.function, instruction, 0)
	if runtime.type_is_integer(typ) && !runtime.type_is_predicate(typ) {
		set_result(vm, frame, instruction, 0, runtime.value_integer_make(1 if result else 0))
		return
	}
	set_result(vm, frame, instruction, 0, runtime.value_predicate(result))
}

exec_cmp :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	comparison: runtime.Comparison_Kind
	#partial switch instruction.compare_predicate {
	case .EQ:
		comparison = .Equal
	case .NE:
		comparison = .Not_Equal
	case .LT:
		comparison = .Less
	case .LE:
		comparison = .Less_Equal
	case .GT:
		comparison = .Greater
	case .GE:
		comparison = .Greater_Equal
	}
	result, ok := runtime.abap_compare(
		&vm.runtime_context,
		comparison,
		get_operand(frame, instruction, 0),
		get_operand(frame, instruction, 1),
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&result)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, result)
	frame.ip += 1
}

exec_select :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	value_index := 1 if runtime.value_truthy(get_operand(frame, instruction, 0)) else 2
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, value_index))
	frame.ip += 1
}

exec_cast :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	typ := result_type_descriptor(frame.function, instruction, 0)
	value, ok := runtime.value_cast(get_operand(frame, instruction, 0), typ, vm.allocator)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_trap(vm, .Type, "runtime cast failed", instruction.source)
		return
	}
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}
