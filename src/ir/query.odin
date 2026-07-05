package abap_frontend_ir

Value_Definition :: struct {
	kind:              Value_Kind,
	block:             Block_Id,
	block_param_index: u32,
	op:                Op_Id,
	op_location:       Op_Location,
	result_index:      u32,
}

module_function_record :: proc(module: ^Module, id: Function_Id) -> (^Function, bool) {
	if module == nil || id == INVALID_FUNCTION_ID || int(id) >= len(module.functions) {
		return nil, false
	}
	return &module.functions[int(id)], true
}

module_type_record :: proc(module: ^Module, id: Type_Id) -> (^Type, bool) {
	if module == nil || id == INVALID_TYPE_ID || int(id) >= len(module.types) {
		return nil, false
	}
	return &module.types[int(id)], true
}

function_block_record :: proc(function: ^Function, id: Block_Id) -> (^Block, bool) {
	if function == nil || id == INVALID_BLOCK_ID || int(id) >= len(function.blocks) {
		return nil, false
	}
	return &function.blocks[int(id)], true
}

function_op_location :: proc(function: ^Function, id: Op_Id) -> (Op_Location, bool) {
	if function == nil || id == INVALID_OP_ID || int(id) >= len(function.op_locations) {
		return {}, false
	}
	loc := function.op_locations[int(id)]
	if loc.block == INVALID_BLOCK_ID || int(loc.block) >= len(function.blocks) {
		return {}, false
	}
	block := &function.blocks[int(loc.block)]
	if int(loc.index) >= len(block.ops) {
		return {}, false
	}
	if block.ops[int(loc.index)].id != id {
		return {}, false
	}
	return loc, true
}

function_op_record :: proc(function: ^Function, id: Op_Id) -> (^Op, bool) {
	loc, ok := function_op_location(function, id)
	if !ok {
		return nil, false
	}
	return &function.blocks[int(loc.block)].ops[int(loc.index)], true
}

function_value_record :: proc(function: ^Function, id: Value_Id) -> (^Value, bool) {
	if function == nil || id == INVALID_VALUE_ID || int(id) >= len(function.values) {
		return nil, false
	}
	return &function.values[int(id)], true
}

function_slot_record :: proc(function: ^Function, id: Slot_Id) -> (^Slot, bool) {
	if function == nil || id == INVALID_SLOT_ID || int(id) >= len(function.slots) {
		return nil, false
	}
	return &function.slots[int(id)], true
}

function_value_definition :: proc(function: ^Function, id: Value_Id) -> (Value_Definition, bool) {
	value, ok := function_value_record(function, id)
	if !ok {
		return {}, false
	}
	if value.block == INVALID_BLOCK_ID || int(value.block) >= len(function.blocks) {
		return {}, false
	}
	switch value.kind {
	case .Block_Param:
		block := &function.blocks[int(value.block)]
		for param, i in block.params {
			if param.value == id {
				return Value_Definition {
						kind = value.kind,
						block = value.block,
						block_param_index = u32(i),
						op = INVALID_OP_ID,
					},
					true
			}
		}
	case .Op_Result:
		loc, loc_ok := function_op_location(function, value.op)
		if !loc_ok {
			return {}, false
		}
		if value.block != loc.block {
			return {}, false
		}
		op := &function.blocks[int(loc.block)].ops[int(loc.index)]
		if int(value.result_index) >= len(op.results) || op.results[int(value.result_index)] != id {
			return {}, false
		}
		return Value_Definition {
				kind = value.kind,
				block = value.block,
				op = value.op,
				op_location = loc,
				result_index = value.result_index,
			},
			true
	}
	return {}, false
}

function_block_source :: proc(function: ^Function, id: Block_Id) -> (Source_Loc, bool) {
	block, ok := function_block_record(function, id)
	if !ok {
		return {}, false
	}
	return block.source, true
}

function_op_source :: proc(function: ^Function, id: Op_Id) -> (Source_Loc, bool) {
	op, ok := function_op_record(function, id)
	if !ok {
		return {}, false
	}
	return op.source, true
}

function_value_source :: proc(function: ^Function, id: Value_Id) -> (Source_Loc, bool) {
	def, ok := function_value_definition(function, id)
	if !ok {
		return {}, false
	}
	switch def.kind {
	case .Block_Param:
		block, block_ok := function_block_record(function, def.block)
		if !block_ok {
			return {}, false
		}
		return block.source, true
	case .Op_Result:
		op, op_ok := function_op_record(function, def.op)
		if !op_ok {
			return {}, false
		}
		return op.source, true
	}
	return {}, false
}

function_slot_source :: proc(function: ^Function, id: Slot_Id) -> (Source_Loc, bool) {
	slot, ok := function_slot_record(function, id)
	if !ok {
		return {}, false
	}
	return slot.source, true
}
