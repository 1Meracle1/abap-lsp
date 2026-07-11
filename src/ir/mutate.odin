package abap_frontend_ir

use_detach :: proc(function: ^Function, use_id: Use_Id) {
	assert(use_id != INVALID_USE_ID && int(use_id) < len(function.uses))
	use := &function.uses[int(use_id)]
	old_value := use.value
	assert(old_value != INVALID_VALUE_ID && int(old_value) < len(function.values))
	value := &function.values[int(old_value)]
	if use.prev_for_value != INVALID_USE_ID {
		function.uses[int(use.prev_for_value)].next_for_value = use.next_for_value
	} else {
		value.first_use = use.next_for_value
	}
	if use.next_for_value != INVALID_USE_ID {
		function.uses[int(use.next_for_value)].prev_for_value = use.prev_for_value
	}
	value.use_count -= 1
	use.prev_for_value = INVALID_USE_ID
	use.next_for_value = INVALID_USE_ID
}

use_attach :: proc(function: ^Function, use_id: Use_Id, value_id: Value_Id) {
	assert(use_id != INVALID_USE_ID && int(use_id) < len(function.uses))
	assert(value_id != INVALID_VALUE_ID && int(value_id) < len(function.values))
	use := &function.uses[int(use_id)]
	value := &function.values[int(value_id)]
	use.value = value_id
	use.prev_for_value = INVALID_USE_ID
	use.next_for_value = value.first_use
	if value.first_use != INVALID_USE_ID {
		function.uses[int(value.first_use)].prev_for_value = use_id
	}
	value.first_use = use_id
	value.use_count += 1
}

replace_use_value :: proc(function: ^Function, use_id: Use_Id, new_value: Value_Id) -> bool {
	if function == nil ||
	   use_id == INVALID_USE_ID ||
	   int(use_id) >= len(function.uses) ||
	   new_value == INVALID_VALUE_ID ||
	   int(new_value) >= len(function.values) {
		return false
	}
	old_value := function.uses[int(use_id)].value
	if old_value == new_value {
		return true
	}
	use_detach(function, use_id)
	use_attach(function, use_id, new_value)
	function_set_user_operand_value(function, function.uses[int(use_id)].user, function.uses[int(use_id)].operand_index, new_value)
	function.mutation_generation += 1
	return true
}

instruction_set_operand :: proc(
	function: ^Function,
	instruction: Instruction_Id,
	operand_index: u32,
	new_value: Value_Id,
) -> bool {
	use_id, ok := function_instruction_operand_use(function, instruction, operand_index)
	if !ok {
		return false
	}
	return replace_use_value(function, use_id, new_value)
}

replace_all_uses :: proc(function: ^Function, old_value, new_value: Value_Id) -> int {
	if function == nil ||
	   old_value == INVALID_VALUE_ID ||
	   new_value == INVALID_VALUE_ID ||
	   int(old_value) >= len(function.values) ||
	   int(new_value) >= len(function.values) ||
	   old_value == new_value {
		return 0
	}
	replaced := 0
	use := function.values[int(old_value)].first_use
	for use != INVALID_USE_ID {
		next := function.uses[int(use)].next_for_value
		if replace_use_value(function, use, new_value) {
			replaced += 1
		}
		use = next
	}
	return replaced
}

function_instruction_operand_use :: proc(
	function: ^Function,
	instruction: Instruction_Id,
	operand_index: u32,
) -> (Use_Id, bool) {
	if function == nil || instruction == INVALID_INSTRUCTION_ID {
		return INVALID_USE_ID, false
	}
	if op, ok := function_op_record(function, Op_Id(instruction)); ok {
		if int(operand_index) >= len(op.operand_uses) {
			return INVALID_USE_ID, false
		}
		return op.operand_uses[int(operand_index)], true
	}
	return INVALID_USE_ID, false
}

function_set_user_operand_value :: proc(
	function: ^Function,
	instruction: Instruction_Id,
	operand_index: u32,
	value: Value_Id,
) -> bool {
	if op, ok := function_op_record(function, Op_Id(instruction)); ok {
		if int(operand_index) >= len(op.operands) {
			return false
		}
		op.operands[int(operand_index)] = value
		instruction_set_successor_operand(op, operand_index, value)
		return true
	}
	return false
}

instruction_set_successor_operand :: proc(instruction: ^Instruction, operand_index: u32, value: Value_Id) {
	for &edge in instruction.successors {
		start := int(edge.operand_start)
		end := start + int(edge.operand_count)
		index := int(operand_index)
		if instruction.opcode == .Switch && edge.kind == .Switch_Case && index == start - 1 {
			edge.case_value = value
		}
		if index >= start && index < end {
			edge.args[index - start] = value
		}
	}
}
