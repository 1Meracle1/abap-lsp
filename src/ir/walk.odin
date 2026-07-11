package abap_frontend_ir

Walk_Visitor :: struct {
	visit_function:   proc(visitor: ^Walk_Visitor, module: ^Module, function_id: Function_Id, function: ^Function) -> bool,
	visit_block:      proc(visitor: ^Walk_Visitor, module: ^Module, function_id: Function_Id, function: ^Function, block_id: Block_Id, block: ^Block) -> bool,
	visit_op:         proc(visitor: ^Walk_Visitor, module: ^Module, function_id: Function_Id, function: ^Function, block_id: Block_Id, block: ^Block, op: ^Op) -> bool,
	visit_terminator: proc(visitor: ^Walk_Visitor, module: ^Module, function_id: Function_Id, function: ^Function, block_id: Block_Id, block: ^Block, term: ^Instruction) -> bool,
	data:             rawptr,
}

walk_module :: proc(visitor: ^Walk_Visitor, module: ^Module) -> bool {
	for _, i in module.functions {
		if !walk_function(visitor, module, Function_Id(i)) {
			return false
		}
	}
	return true
}

walk_function :: proc(visitor: ^Walk_Visitor, module: ^Module, function_id: Function_Id) -> bool {
	function := function_ptr(module, function_id)
	if visitor.visit_function != nil && !visitor.visit_function(visitor, module, function_id, function) {
		return false
	}
	for _, i in function.blocks {
		if !walk_block(visitor, module, function_id, Block_Id(i)) {
			return false
		}
	}
	return true
}

walk_block :: proc(
	visitor: ^Walk_Visitor,
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
) -> bool {
	function := function_ptr(module, function_id)
	block := block_ptr(function, block_id)
	if visitor.visit_block != nil && !visitor.visit_block(visitor, module, function_id, function, block_id, block) {
		return false
	}
	for instruction in block.instructions {
		op := op_ptr(function, Op_Id(instruction))
		if visitor.visit_op != nil &&
		   !visitor.visit_op(visitor, module, function_id, function, block_id, block, op) {
			return false
		}
	}
	if visitor.visit_terminator != nil && block.terminator != INVALID_INSTRUCTION_ID {
		term := op_ptr(function, Op_Id(block.terminator))
		if !visitor.visit_terminator(visitor, module, function_id, function, block_id, block, term) {
			return false
		}
	}
	return true
}
