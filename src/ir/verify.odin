package abap_frontend_ir

import "core:mem"

Verify_Diagnostic_Kind :: enum {
	Invalid_Function,
	Invalid_Block,
	Invalid_Value,
	Invalid_Type,
	Missing_Terminator,
	Bad_Terminator_Args,
	Bad_Return_Args,
	Bad_World_Chain,
	Dominance_Error,
	Bad_Op_Signature,
	Bad_Use_List,
	Bad_Intrinsic,
	Bad_Memory_Alias,
	Bad_Exception_Edge,
}

Verify_Diagnostic :: struct {
	kind:     Verify_Diagnostic_Kind,
	function: Function_Id,
	block:    Block_Id,
	op:       Op_Id,
	value:    Value_Id,
	message:  string,
	source:   Source_Loc,
}

Verify_Result :: struct {
	ok:          bool,
	diagnostics: [dynamic]Verify_Diagnostic,
}

Verify_Options :: struct {
	allow_legacy_top_level_may_throw_propagation: bool,
}

verify_result_destroy :: proc(result: ^Verify_Result) {
	delete(result.diagnostics)
	result^ = {}
}

verify_module :: proc(module: ^Module, allocator: mem.Allocator = context.allocator) -> Verify_Result {
	return verify_module_with_options(
		module,
		Verify_Options{},
		allocator,
	)
}

verify_module_with_options :: proc(
	module: ^Module,
	options: Verify_Options,
	allocator: mem.Allocator = context.allocator,
) -> Verify_Result {
	result := Verify_Result {
		ok = true,
		diagnostics = make([dynamic]Verify_Diagnostic, 0, 8, allocator),
	}
	for entry in module.entries {
		if entry == INVALID_FUNCTION_ID || int(entry) >= len(module.functions) {
			verify_add(&result, .Invalid_Function, "module entry has invalid function target")
			continue
		}
		function := function_ptr(module, entry)
		if function.role != .Report_Entry {
			verify_add(&result, .Invalid_Function, "module entry must target a report entry function", entry, source = function.source)
		}
	}
	verify_module_tables(module, &result)
	for _, i in module.functions {
		verify_function(module, Function_Id(i), options, &result)
	}
	result.ok = len(result.diagnostics) == 0
	return result
}

verify_function :: proc(module: ^Module, function_id: Function_Id, options: Verify_Options, result: ^Verify_Result) {
	function := function_ptr(module, function_id)
	if function.entry == INVALID_BLOCK_ID || int(function.entry) >= len(function.blocks) {
		verify_add(result, .Invalid_Function, "function has no valid entry block", function_id)
		return
	}
	if len(function.blocks) == 0 {
		verify_add(result, .Invalid_Function, "function has no blocks", function_id)
		return
	}

	verify_function_shape(module, function, function_id, result)

	dom := verify_compute_dominators(function, context.temp_allocator)
	defer delete(dom, context.temp_allocator)

	for block, block_index in function.blocks {
		block_id := Block_Id(block_index)
		if block.terminator == INVALID_INSTRUCTION_ID {
			verify_add(result, .Missing_Terminator, "block is missing terminator", function_id, block_id, source = block.source)
		}
		verify_block_ops(module, function, function_id, block_id, dom, options, result)
		verify_terminator(module, function, function_id, block_id, dom, options, result)
		verify_world_chain(function, function_id, block_id, result)
	}
}

verify_function_shape :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	result: ^Verify_Result,
) {
	for typ in function.return_types {
		if !verify_type_valid(module, typ) {
			verify_add(result, .Invalid_Type, "function return type is invalid", function_id)
		}
	}
	for typ in function.signature.params {
		if !verify_type_valid(module, typ) {
			verify_add(result, .Invalid_Type, "function parameter type is invalid", function_id)
		}
	}
	for typ in function.signature.results {
		if !verify_type_valid(module, typ) {
			verify_add(result, .Invalid_Type, "function signature result type is invalid", function_id)
		}
	}
	verify_function_signature_entry(module, function, function_id, result)
	verify_function_signature_results(module, function, function_id, result)

	for slot in function.slots {
		if !verify_type_valid(module, slot.type) {
			verify_add(result, .Invalid_Type, "slot has invalid type", function_id, source = slot.source)
		}
	}

	for value, value_index in function.values {
		verify_value_record_shape(module, function, function_id, Value_Id(value_index), value, result)
	}

	for instruction, instruction_index in function.instructions {
		if instruction.id != Instruction_Id(instruction_index) {
			verify_add(result, .Invalid_Function, "instruction id does not match instruction table index", function_id, instruction.parent, Op_Id(instruction_index), source = instruction.source)
		}
	}

	for loc, op_index in function.op_locations {
		if loc.block == INVALID_BLOCK_ID {
			continue
		}
		verify_op_location_record(function, function_id, Op_Id(op_index), loc, result)
	}

	for block, block_index in function.blocks {
		block_id := Block_Id(block_index)
		if block.id != block_id {
			verify_add(result, .Invalid_Block, "block id does not match block index", function_id, block_id, source = block.source)
		}
		for arg in block.args {
			if !verify_value_id_valid(function, arg) {
				verify_add(result, .Invalid_Value, "block parameter references invalid value", function_id, block_id, value = arg, source = block.source)
				continue
			}
			record := value_ptr(function, arg)
			if record.kind != .Block_Param || record.block != block_id {
				verify_add(result, .Invalid_Value, "block parameter value does not point back to block", function_id, block_id, value = arg, source = block.source)
			}
		}
	}
	verify_use_lists(function, function_id, result)
}

verify_function_signature_entry :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	result: ^Verify_Result,
) {
	if function.entry == INVALID_BLOCK_ID || int(function.entry) >= len(function.blocks) {
		return
	}
	entry := block_ptr(function, function.entry)
	if len(entry.args) != len(function.signature.params) {
		verify_add(result, .Invalid_Function, "entry block argument count must match function signature parameters", function_id, function.entry, source = entry.source)
		return
	}
	for arg, i in entry.args {
		if !verify_value_id_valid(function, arg) {
			verify_add(result, .Invalid_Value, "entry block argument references invalid value", function_id, function.entry, value = arg, source = entry.source)
			continue
		}
		arg_type := value_type(function, arg)
		expected := function.signature.params[i]
		if verify_type_valid(module, arg_type) && verify_type_valid(module, expected) && arg_type != expected {
			verify_add(result, .Invalid_Function, "entry block argument type must match function signature parameter", function_id, function.entry, value = arg, source = entry.source)
		}
	}
}

verify_function_signature_results :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	result: ^Verify_Result,
) {
	if len(function.return_types) != len(function.signature.results) {
		verify_add(result, .Invalid_Function, "function return types must match function signature results", function_id, source = function.source)
		return
	}
	for typ, i in function.return_types {
		expected := function.signature.results[i]
		if verify_type_valid(module, typ) && verify_type_valid(module, expected) && typ != expected {
			verify_add(result, .Invalid_Function, "function return type must match function signature result", function_id, source = function.source)
		}
	}
}

verify_value_record_shape :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	value_id: Value_Id,
	value: Value,
	result: ^Verify_Result,
) {
	if !verify_type_valid(module, value.type) {
		verify_add(result, .Invalid_Type, "value has invalid type", function_id, value = value_id)
	}
	if !verify_block_id_valid(function, value.block) {
		verify_add(result, .Invalid_Value, "value definition block is invalid", function_id, value = value_id)
		return
	}

	#partial switch value.kind {
	case .Block_Param:
		if value.op != INVALID_OP_ID {
			verify_add(result, .Invalid_Value, "block parameter value must not reference an operation", function_id, value.block, value.op, value_id)
		}
		block := block_ptr(function, value.block)
		found := false
		for arg in block.args {
			if arg == value_id {
				found = true
				break
			}
		}
		if !found {
			verify_add(result, .Invalid_Value, "block parameter value is not listed by defining block", function_id, value.block, value = value_id, source = block.source)
		}
	case .Op_Result:
		if !verify_op_id_valid(function, value.op) {
			verify_add(result, .Invalid_Value, "operation result value references invalid operation", function_id, value.block, value.op, value_id)
			return
		}
		loc := function.op_locations[int(value.op)]
		if !verify_op_location_points_to_op(function, value.op, loc) {
			verify_add(result, .Invalid_Value, "operation result value references invalid operation location", function_id, value.block, value.op, value_id)
			return
		}
		if value.block != loc.block {
			verify_add(result, .Invalid_Value, "operation result value block does not match operation block", function_id, value.block, value.op, value_id)
			return
		}
		op := &function.instructions[int(value.op)]
		if int(value.result_index) >= len(op.results) || op.results[int(value.result_index)] != value_id {
			verify_add(result, .Invalid_Value, "operation result value is not owned by defining operation", function_id, value.block, value.op, value_id, op.source)
		}
	}
}

verify_op_location_record :: proc(
	function: ^Function,
	function_id: Function_Id,
	op_id: Op_Id,
	loc: Op_Location,
	result: ^Verify_Result,
) {
	if loc.block == INVALID_BLOCK_ID || int(loc.block) >= len(function.blocks) {
		verify_add(result, .Invalid_Function, "operation location references invalid block", function_id, op = op_id)
		return
	}
	block := block_ptr(function, loc.block)
	if int(op_id) < 0 || int(op_id) >= len(function.instructions) {
		verify_add(result, .Invalid_Function, "operation location references invalid instruction", function_id, loc.block, op_id, source = block.source)
		return
	}
	if int(loc.index) < len(block.instructions) {
		if block.instructions[int(loc.index)] != Instruction_Id(op_id) {
			verify_add(result, .Invalid_Function, "operation location target has mismatched instruction id", function_id, loc.block, op_id, source = block.source)
		}
	} else if int(loc.index) == len(block.instructions) {
		if block.terminator != Instruction_Id(op_id) {
			verify_add(result, .Invalid_Function, "operation location target has mismatched terminator id", function_id, loc.block, op_id, source = block.source)
		}
	} else {
		verify_add(result, .Invalid_Function, "operation location references invalid block instruction index", function_id, loc.block, op_id, source = block.source)
		return
	}
	op := &function.instructions[int(op_id)]
	if op.id != Instruction_Id(op_id) {
		verify_add(result, .Invalid_Function, "operation location target has mismatched operation id", function_id, loc.block, op_id, source = op.source)
	}
}

verify_op_location_points_to_op :: #force_inline proc "contextless" (
	function: ^Function,
	op_id: Op_Id,
	loc: Op_Location,
) -> bool {
	if loc.block == INVALID_BLOCK_ID || int(loc.block) >= len(function.blocks) {
		return false
	}
	block := &function.blocks[int(loc.block)]
	if int(op_id) < 0 || int(op_id) >= len(function.instructions) {
		return false
	}
	if int(loc.index) < len(block.instructions) {
		return block.instructions[int(loc.index)] == Instruction_Id(op_id)
	}
	if int(loc.index) == len(block.instructions) {
		return block.terminator == Instruction_Id(op_id)
	}
	return false
}

verify_block_ops :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	dom: []bool,
	options: Verify_Options,
	result: ^Verify_Result,
) {
	block := block_ptr(function, block_id)
	for instruction, op_index in block.instructions {
		if instruction == INVALID_INSTRUCTION_ID || int(instruction) >= len(function.instructions) {
			verify_add(result, .Invalid_Function, "block instruction list references invalid instruction", function_id, block_id, Op_Id(instruction), source = block.source)
			continue
		}
		op := function.instructions[int(instruction)]
		if int(op.id) >= len(function.op_locations) {
			verify_add(result, .Invalid_Function, "operation id has no location", function_id, block_id, op.id, source = op.source)
		} else {
			loc := function.op_locations[int(op.id)]
			if loc.block != block_id || loc.index != u32(op_index) {
				verify_add(result, .Invalid_Function, "operation location does not point back to operation", function_id, block_id, op.id, source = op.source)
			}
		}
		verify_instruction_use_records(function, function_id, block_id, op, result)
		verify_canonical_instruction(module, function, function_id, block_id, op, false, options, result)
		call_operand_count := verify_call_operand_count(op)
		for operand_index in 0 ..< call_operand_count {
			verify_value_use(module, function, function_id, block_id, u32(op_index), op.operands[operand_index], dom, result, op.id, op.source)
		}
		if op.opcode == .Invoke {
			for edge in op.successors {
				for arg in edge.args {
					verify_value_use(module, function, function_id, block_id, u32(op_index + 1), arg, dom, result, op.id, op.source)
				}
			}
		}
		for value, result_index in op.results {
			if value == INVALID_VALUE_ID || int(value) >= len(function.values) {
				verify_add(result, .Invalid_Value, "operation result has invalid value id", function_id, block_id, op.id, value, op.source)
				continue
			}
			record := value_ptr(function, value)
			if record.kind != .Op_Result || record.block != block_id || record.op != op.id || record.result_index != u32(result_index) {
				verify_add(result, .Invalid_Value, "operation result value does not point back to operation", function_id, block_id, op.id, value, op.source)
			}
			if !verify_type_valid(module, record.type) {
				verify_add(result, .Invalid_Type, "operation result has invalid type", function_id, block_id, op.id, value, op.source)
			}
		}
	}
}

verify_terminator :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	dom: []bool,
	options: Verify_Options,
	result: ^Verify_Result,
) {
	block := block_ptr(function, block_id)
	if block.terminator == INVALID_INSTRUCTION_ID {
		return
	}
	if int(block.terminator) >= len(function.instructions) {
		verify_add(result, .Invalid_Function, "block terminator references invalid instruction", function_id, block_id, Op_Id(block.terminator), source = block.source)
		return
	}
	use_index := u32(len(block.instructions))
	term := function.instructions[int(block.terminator)]
	if int(term.id) >= len(function.op_locations) {
		verify_add(result, .Invalid_Function, "terminator id has no location", function_id, block_id, term.id, source = term.source)
	} else {
		loc := function.op_locations[int(term.id)]
		if loc.block != block_id || loc.index != use_index {
			verify_add(result, .Invalid_Function, "terminator location does not point back to block terminator", function_id, block_id, term.id, source = term.source)
		}
	}
	verify_instruction_use_records(function, function_id, block_id, term, result)
	verify_canonical_instruction(module, function, function_id, block_id, term, true, options, result)
	#partial switch term.opcode {
	case .Br:
		if verify_terminator_successor(function_id, block_id, term, 0, .Normal, 0, result) {
			edge := term.successors[0]
			verify_branch_target(module, function, function_id, block_id, edge.target, edge.args[:], result, term.source)
			for arg in edge.args {
				verify_value_use(module, function, function_id, block_id, use_index, arg, dom, result, term.id, term.source)
			}
		}
	case .Cond_Br:
		condition := term.operands[0] if len(term.operands) > 0 else INVALID_VALUE_ID
		verify_value_use(module, function, function_id, block_id, use_index, condition, dom, result, term.id, term.source)
		condition_type, condition_type_ok := verify_value_type_lookup(function, condition)
		if condition_type_ok && verify_type_valid(module, condition_type) && condition_type != BUILTIN_TYPE_PREDICATE {
			verify_add(result, .Bad_Terminator_Args, "conditional branch condition must be predicate", function_id, block_id, term.id, condition, term.source)
		}
		if verify_terminator_successor(function_id, block_id, term, 0, .True, 1, result) {
			edge := term.successors[0]
			verify_branch_target(module, function, function_id, block_id, edge.target, edge.args[:], result, term.source)
			for arg in edge.args {
				verify_value_use(module, function, function_id, block_id, use_index, arg, dom, result, term.id, term.source)
			}
		}
		if len(term.successors) > 0 {
			operand_start := 1 + int(term.successors[0].operand_count)
			if verify_terminator_successor(function_id, block_id, term, 1, .False, operand_start, result) {
				edge := term.successors[1]
				verify_branch_target(module, function, function_id, block_id, edge.target, edge.args[:], result, term.source)
				for arg in edge.args {
					verify_value_use(module, function, function_id, block_id, use_index, arg, dom, result, term.id, term.source)
				}
			}
		}
	case .Return:
		if len(function.return_types) != len(term.operands) {
			verify_add(result, .Bad_Return_Args, "return value count does not match function return types", function_id, block_id, source = term.source)
		} else {
			for value, i in term.operands {
				verify_value_use(module, function, function_id, block_id, use_index, value, dom, result, term.id, term.source)
				value_typ, value_type_ok := verify_value_type_lookup(function, value)
				return_type := function.return_types[i]
				if value_type_ok && verify_type_valid(module, value_typ) && verify_type_valid(module, return_type) && value_typ != return_type {
					verify_add(result, .Bad_Return_Args, "return value type does not match function return type", function_id, block_id, value = value, source = term.source)
				}
			}
		}
	case .Switch:
		if len(term.operands) == 0 || len(term.successors) == 0 {
			return
		}
		selector := term.operands[0]
		verify_value_use(module, function, function_id, block_id, use_index, selector, dom, result, term.id, term.source)
		selector_type, selector_type_ok := verify_value_type_lookup(function, selector)
		expected_operand_start := 1
		if verify_terminator_successor(function_id, block_id, term, 0, .Normal, expected_operand_start, result) {
			edge := term.successors[0]
			if edge.case_value != INVALID_VALUE_ID {
				verify_add(result, .Bad_Terminator_Args, "switch default edge must not have a case value", function_id, block_id, term.id, edge.case_value, term.source)
			}
			verify_branch_target(module, function, function_id, block_id, edge.target, edge.args[:], result, term.source)
			for arg in edge.args {
				verify_value_use(module, function, function_id, block_id, use_index, arg, dom, result, term.id, term.source)
			}
			expected_operand_start += len(edge.args)
		}
		for edge, i in term.successors[1:] {
			expected_operand_start += 1
			if verify_terminator_successor(function_id, block_id, term, i + 1, .Switch_Case, expected_operand_start, result) {
				if edge.case_value == INVALID_VALUE_ID || term.operands[expected_operand_start - 1] != edge.case_value {
					verify_add(result, .Bad_Terminator_Args, "switch case edge must carry its case value immediately before branch arguments", function_id, block_id, term.id, source = term.source)
				} else {
					verify_value_use(module, function, function_id, block_id, use_index, edge.case_value, dom, result, term.id, term.source)
					case_type, case_type_ok := verify_value_type_lookup(function, edge.case_value)
					if selector_type_ok && case_type_ok && selector_type != case_type {
						verify_add(result, .Bad_Terminator_Args, "switch case value type must match selector type", function_id, block_id, term.id, edge.case_value, term.source)
					}
				}
				verify_branch_target(module, function, function_id, block_id, edge.target, edge.args[:], result, term.source)
				for arg in edge.args {
					verify_value_use(module, function, function_id, block_id, use_index, arg, dom, result, term.id, term.source)
				}
				expected_operand_start += len(edge.args)
			}
		}
	case .Unreachable:
	case:
		verify_add(result, .Bad_Terminator_Args, "block terminator must use a control-flow opcode", function_id, block_id, term.id, source = term.source)
	}
}

verify_terminator_successor :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	term: Instruction,
	index: int,
	kind: Edge_Kind,
	operand_start: int,
	result: ^Verify_Result,
) -> bool {
	if index >= len(term.successors) {
		verify_add(result, .Bad_Terminator_Args, "terminator is missing canonical successor edge", function_id, block_id, source = term.source)
		return false
	}
	edge := term.successors[index]
	if edge.kind != kind {
		verify_add(result, .Bad_Terminator_Args, "terminator successor kind must match branch kind", function_id, block_id, source = term.source)
	}
	if int(edge.operand_start) != operand_start {
		verify_add(result, .Bad_Terminator_Args, "terminator successor operand slice must mirror branch args", function_id, block_id, source = term.source)
	}
	if int(edge.operand_count) != len(edge.args) {
		verify_add(result, .Bad_Terminator_Args, "terminator successor args must mirror branch args", function_id, block_id, source = term.source)
		return false
	}
	if operand_start + len(edge.args) > len(term.operands) {
		verify_add(result, .Bad_Terminator_Args, "terminator successor operand slice is outside operands", function_id, block_id, source = term.source)
		return false
	}
	for arg, i in edge.args {
		if edge.args[i] != arg || term.operands[operand_start + i] != arg {
			verify_add(result, .Bad_Terminator_Args, "terminator successor args must mirror operand slice", function_id, block_id, value = arg, source = term.source)
		}
	}
	return true
}

verify_module_tables :: proc(module: ^Module, result: ^Verify_Result) {
	if module.target.pointer_bits == 0 {
		verify_add(result, .Invalid_Type, "target pointer width must be non-zero")
	}
	if module.target.default_integer_bits == 0 {
		verify_add(result, .Invalid_Type, "target default integer width must be non-zero")
	}
	if module.target.string_encoding == .Unknown {
		verify_add(result, .Invalid_Type, "target string encoding must be known")
	}
	for typ, i in module.types {
		if typ.id != Type_Id(i) {
			verify_add(result, .Invalid_Type, "type id does not match type table index", value = Value_Id(i))
		}
		verify_type_runtime_descriptor(module, Type_Id(i), typ, result)
		#partial switch data in typ.data {
		case Integer_Type_Data:
			if data.bits == 0 {
				verify_add(result, .Invalid_Type, "integer type must have non-zero bit width", value = Value_Id(i))
			}
		case Struct_Type_Data:
			for field in data.fields {
				if !verify_type_valid(module, field.type) {
					verify_add(result, .Invalid_Type, "struct field type is invalid", value = Value_Id(i))
				}
			}
		case Table_Type_Data:
			if !verify_type_valid(module, data.row_type) {
				verify_add(result, .Invalid_Type, "table row type is invalid", value = Value_Id(i))
			}
		case Reference_Type_Data:
			if !verify_type_valid(module, data.pointee) {
				verify_add(result, .Invalid_Type, "reference pointee type is invalid", value = Value_Id(i))
			}
		}
	}
	for constant, i in module.constants {
		if !verify_type_valid(module, constant.type) {
			verify_add(result, .Invalid_Type, "constant has invalid type", value = Value_Id(i))
		}
	}
	for global, i in module.globals {
		if !verify_type_valid(module, global.type) {
			verify_add(result, .Invalid_Type, "global has invalid type", value = Value_Id(i))
		}
	}
	for intrinsic, i in module.intrinsics {
		if intrinsic.name == "" {
			verify_add(result, .Bad_Intrinsic, "intrinsic declaration must have a stable name", op = Op_Id(i))
		}
		if intrinsic.op == .Unknown {
			verify_add(result, .Bad_Intrinsic, "intrinsic declaration must have supported operation", op = Op_Id(i))
		}
		if intrinsic.op != .Unknown {
			expected_name := intrinsic_name_for_op(intrinsic.op)
			if intrinsic.name != expected_name {
				verify_add(result, .Bad_Intrinsic, "intrinsic declaration name must match operation", op = Op_Id(i))
			}
			expected_family := intrinsic_family_for_op(intrinsic.op)
			if intrinsic.family != expected_family {
				verify_add(result, .Bad_Intrinsic, "intrinsic declaration family must match operation", op = Op_Id(i))
			}
		}
		if intrinsic.effects != intrinsic.signature.effects {
			verify_add(result, .Bad_Intrinsic, "intrinsic declaration effects must match signature effects", op = Op_Id(i))
		}
		if intrinsic.signature.can_throw != (.May_Throw in intrinsic.effects) {
			verify_add(result, .Bad_Intrinsic, "intrinsic can_throw flag must match may-throw effect", op = Op_Id(i))
		}
		if intrinsic.signature.can_trap != (.May_Trap in intrinsic.effects) {
			verify_add(result, .Bad_Intrinsic, "intrinsic can_trap flag must match may-trap effect", op = Op_Id(i))
		}
		for typ in intrinsic.signature.params {
			if !verify_type_valid(module, typ) {
				verify_add(result, .Bad_Intrinsic, "intrinsic parameter type is invalid", op = Op_Id(i))
			}
		}
		for typ in intrinsic.signature.results {
			if !verify_type_valid(module, typ) {
				verify_add(result, .Bad_Intrinsic, "intrinsic result type is invalid", op = Op_Id(i))
			}
		}
	}
	for scope, i in module.effect_scopes {
		if scope.name == "" {
			verify_add(result, .Bad_Memory_Alias, "effect scope must have stable name", op = Op_Id(i))
		}
		if scope.type != INVALID_TYPE_ID && !verify_type_valid(module, scope.type) {
			verify_add(result, .Bad_Memory_Alias, "effect scope type is invalid", op = Op_Id(i))
		}
	}
	for alias, i in module.alias_classes {
		if alias.name == "" {
			verify_add(result, .Bad_Memory_Alias, "alias class must have stable name", op = Op_Id(i))
		}
		if alias.parent != INVALID_ALIAS_CLASS_ID && int(alias.parent) >= len(module.alias_classes) {
			verify_add(result, .Bad_Memory_Alias, "alias class parent is invalid", op = Op_Id(i))
		}
		for disjoint in alias.disjoint {
			if disjoint == INVALID_ALIAS_CLASS_ID || int(disjoint) >= len(module.alias_classes) {
				verify_add(result, .Bad_Memory_Alias, "alias class disjoint target is invalid", op = Op_Id(i))
			}
		}
	}
}

verify_type_runtime_descriptor :: proc(
	module: ^Module,
	type_id: Type_Id,
	typ: Type,
	result: ^Verify_Result,
) {
	if typ.runtime.display_name == "" {
		verify_add(result, .Invalid_Type, "runtime type descriptor must carry display name", value = Value_Id(type_id))
	}
	#partial switch typ.runtime.family {
	case .Integer:
		if typ.runtime.elementary.bits == 0 {
			verify_add(result, .Invalid_Type, "integer runtime descriptor must carry bit width", value = Value_Id(type_id))
		}
	case .Decimal:
		if !typ.runtime.elementary.has_length || typ.runtime.elementary.length < 1 || typ.runtime.elementary.length > 16 {
			verify_add(result, .Invalid_Type, "packed decimal runtime descriptor must carry a length from 1 through 16", value = Value_Id(type_id))
		}
	case .Text:
		if typ.runtime.elementary.text_kind == .None {
			verify_add(result, .Invalid_Type, "text runtime descriptor must carry text kind", value = Value_Id(type_id))
		}
	case .Structure:
		for field in typ.runtime.structure.fields {
			if field.name == "" {
				verify_add(result, .Invalid_Type, "structure runtime field must carry name", value = Value_Id(type_id))
			}
			if !verify_type_valid(module, field.type) {
				verify_add(result, .Invalid_Type, "structure runtime field type is invalid", value = Value_Id(type_id))
			}
		}
	case .Table:
		if !verify_type_valid(module, typ.runtime.table.row_type) {
			verify_add(result, .Invalid_Type, "table runtime descriptor row type is invalid", value = Value_Id(type_id))
		}
		if typ.runtime.table.category == .Unknown {
			verify_add(result, .Invalid_Type, "table runtime descriptor must carry table category", value = Value_Id(type_id))
		}
		verify_table_key_descriptor(module, type_id, typ.runtime.table.primary_key, result, primary = true)
		for key in typ.runtime.table.secondary_keys {
			verify_table_key_descriptor(module, type_id, key, result)
		}
	case .Reference:
		if typ.runtime.reference.kind == .Unknown {
			verify_add(result, .Invalid_Type, "reference runtime descriptor must carry reference kind", value = Value_Id(type_id))
		}
		if typ.runtime.reference.target_type != INVALID_TYPE_ID &&
		   !verify_type_valid(module, typ.runtime.reference.target_type) {
			verify_add(result, .Invalid_Type, "reference runtime descriptor target type is invalid", value = Value_Id(type_id))
		}
	}
}

verify_table_key_descriptor :: proc(
	module: ^Module,
	type_id: Type_Id,
	key: Runtime_Table_Key_Descriptor,
	result: ^Verify_Result,
	primary: bool = false,
) {
	if key.name == "" && len(key.components) == 0 {
		return
	}
	if !primary && key.name == "" {
		verify_add(result, .Invalid_Type, "secondary table key runtime descriptor must carry name", value = Value_Id(type_id))
	}
	for component in key.components {
		if component.name == "" || len(component.path) == 0 {
			verify_add(result, .Invalid_Type, "table key component runtime descriptor must carry path", value = Value_Id(type_id))
		}
		if component.type != INVALID_TYPE_ID && !verify_type_valid(module, component.type) {
			verify_add(result, .Invalid_Type, "table key component runtime descriptor type is invalid", value = Value_Id(type_id))
		}
	}
}

verify_use_lists :: proc(function: ^Function, function_id: Function_Id, result: ^Verify_Result) {
	seen := make([]u32, len(function.uses), context.temp_allocator)
	defer delete(seen, context.temp_allocator)
	for value, value_index in function.values {
		count: u32
		prev := INVALID_USE_ID
		use := value.first_use
		for use != INVALID_USE_ID {
			if int(use) >= len(function.uses) {
				verify_add(result, .Bad_Use_List, "value use list points outside use table", function_id, value = Value_Id(value_index))
				break
			}
				record := function.uses[int(use)]
				if seen[int(use)] != 0 {
					verify_add(result, .Bad_Use_List, "value use list contains a cycle", function_id, op = Op_Id(record.user), value = Value_Id(value_index))
					break
				}
			if record.value != Value_Id(value_index) {
				verify_add(result, .Bad_Use_List, "use list node points at a different value", function_id, op = Op_Id(record.user), value = Value_Id(value_index))
			}
			if record.prev_for_value != prev {
				verify_add(result, .Bad_Use_List, "use list prev link is inconsistent", function_id, op = Op_Id(record.user), value = Value_Id(value_index))
			}
			seen[int(use)] += 1
			count += 1
			prev = use
			use = record.next_for_value
		}
		if count != value.use_count {
			verify_add(result, .Bad_Use_List, "value use count does not match linked list", function_id, value = Value_Id(value_index))
		}
	}
	for use, use_index in function.uses {
		if use.id != Use_Id(use_index) {
			verify_add(result, .Bad_Use_List, "use id does not match use index", function_id, op = Op_Id(use.user), value = use.value)
		}
		if !verify_value_id_valid(function, use.value) {
			verify_add(result, .Bad_Use_List, "use references invalid value", function_id, op = Op_Id(use.user), value = use.value)
		}
		if seen[use_index] != 1 {
			verify_add(result, .Bad_Use_List, "use must appear in exactly one value use list", function_id, op = Op_Id(use.user), value = use.value)
		}
		if !verify_user_operand_matches(function, use) {
			verify_add(result, .Bad_Use_List, "use does not match its user operand slot", function_id, op = Op_Id(use.user), value = use.value)
		}
	}
}

verify_user_operand_matches :: proc(function: ^Function, use: Use) -> bool {
	if op, ok := function_op_record(function, Op_Id(use.user)); ok {
		if int(use.operand_index) >= len(op.operands) || int(use.operand_index) >= len(op.operand_uses) {
			return false
		}
		return op.operands[int(use.operand_index)] == use.value &&
		       op.operand_uses[int(use.operand_index)] == use.id
	}
	return false
}

verify_instruction_use_records :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if len(op.operands) != len(op.operand_uses) {
		verify_add(result, .Bad_Use_List, "instruction operand and use counts differ", function_id, block_id, op.id, source = op.source)
		return
	}
	for use, i in op.operand_uses {
		if use == INVALID_USE_ID || int(use) >= len(function.uses) {
			verify_add(result, .Bad_Use_List, "instruction operand has invalid use id", function_id, block_id, op.id, source = op.source)
			continue
		}
		record := function.uses[int(use)]
		if record.user != op.id || record.operand_index != u32(i) || record.value != op.operands[i] {
			verify_add(result, .Bad_Use_List, "instruction operand use record does not match operand", function_id, block_id, op.id, op.operands[i], op.source)
		}
	}
}

verify_canonical_instruction :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	is_terminator: bool,
	options: Verify_Options,
	result: ^Verify_Result,
) {
	if op.parent != block_id {
		verify_add(result, .Invalid_Block, "instruction parent block does not match owning block", function_id, block_id, op.id, source = op.source)
	}
	if op.id == INVALID_OP_ID || u32(op.id) >= function.next_instruction_id || int(op.id) >= len(function.instructions) {
		verify_add(result, .Invalid_Function, "instruction id is outside function instruction id range", function_id, block_id, op.id, source = op.source)
	}
	if op.intrinsic != INVALID_INTRINSIC_ID && op.opcode != .Intrinsic && op.opcode != .Invoke {
		verify_add(result, .Bad_Intrinsic, "only intrinsic or invoke opcode may reference intrinsic declaration", function_id, block_id, op.id, source = op.source)
	}
	verify_instruction_successor_policy(module, function, function_id, block_id, op, is_terminator, result)
	verify_world_value_positions(function, function_id, block_id, op, is_terminator, result)
	verify_core_effect_policy(function, function_id, block_id, op, result)
	verify_may_throw_policy(module, function_id, block_id, op, options, result)
	switch op.opcode {
	case .Const:
		if len(op.operands) != 0 || len(op.results) != 1 {
			verify_add(result, .Bad_Op_Signature, "const instruction must have one result and no operands", function_id, block_id, op.id, source = op.source)
		}
		verify_const_attrs(module, function, function_id, block_id, op, result)
	case .Initial:
		verify_op_arity(result, function_id, block_id, op, 0, 0, 1, 1)
	case .Null_Ref:
		verify_op_arity(result, function_id, block_id, op, 0, 0, 1, 1)
		verify_result_reference_like(module, function, function_id, block_id, op, 0, "null_ref result must be reference-like", result)
	case .Global_Addr:
		verify_op_arity(result, function_id, block_id, op, 0, 0, 1, 1)
		verify_result_reference_like(module, function, function_id, block_id, op, 0, "global_addr result must be reference-like", result)
	case .Function_Addr:
		verify_op_arity(result, function_id, block_id, op, 0, 0, 1, 1)
		verify_result_reference_like(module, function, function_id, block_id, op, 0, "function_addr result must be callable reference-like", result)
	case .Add, .Sub, .Mul, .Div, .Mod:
		if len(op.operands) != 2 || len(op.results) != 1 {
			verify_add(result, .Bad_Op_Signature, "integer binary instruction must have two operands and one result", function_id, block_id, op.id, source = op.source)
		}
		verify_same_operand_result_types(function, function_id, block_id, op, result)
		verify_integer_operands_and_results(module, function, function_id, block_id, op, result)
	case .And, .Or, .Xor:
		if len(op.operands) != 2 || len(op.results) != 1 {
			verify_add(result, .Bad_Op_Signature, "logical instruction must have two operands and one result", function_id, block_id, op.id, source = op.source)
		}
		verify_same_operand_result_types(function, function_id, block_id, op, result)
		verify_logical_operands_and_results(module, function, function_id, block_id, op, result)
	case .Neg:
		if len(op.operands) != 1 || len(op.results) != 1 {
			verify_add(result, .Bad_Op_Signature, "integer unary instruction must have one operand and one result", function_id, block_id, op.id, source = op.source)
		}
		verify_same_operand_result_types(function, function_id, block_id, op, result)
		verify_integer_operands_and_results(module, function, function_id, block_id, op, result)
	case .Not:
		if len(op.operands) != 1 || len(op.results) != 1 {
			verify_add(result, .Bad_Op_Signature, "logical not instruction must have one operand and one result", function_id, block_id, op.id, source = op.source)
		}
		verify_same_operand_result_types(function, function_id, block_id, op, result)
		verify_logical_operands_and_results(module, function, function_id, block_id, op, result)
	case .Cmp:
		if len(op.operands) != 2 || len(op.results) != 1 || !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_PREDICATE) {
			verify_add(result, .Bad_Op_Signature, "compare instruction must produce predicate from two operands", function_id, block_id, op.id, source = op.source)
		}
		if _, ok := op.attrs.(Compare_Attrs); !ok {
			verify_add(result, .Bad_Op_Signature, "compare instruction must carry compare attrs", function_id, block_id, op.id, source = op.source)
		}
		verify_operand_types_match(function, function_id, block_id, op, 0, 1, "compare operand types must match", result)
	case .Select:
		verify_op_arity(result, function_id, block_id, op, 3, 3, 1, 1)
		verify_operand_predicate(module, function, function_id, block_id, op, 0, "select condition must be predicate", result)
		verify_operand_types_match(function, function_id, block_id, op, 1, 2, "select value operand types must match", result)
		verify_operand_result_types_match(function, function_id, block_id, op, 1, 0, "select result type must match selected values", result)
	case .Cast, .Int_Extend, .Int_Truncate, .Ref_Cast, .Addr_Cast:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		if op.opcode == .Int_Extend || op.opcode == .Int_Truncate {
			verify_integer_operands_and_results(module, function, function_id, block_id, op, result)
		}
		if op.opcode == .Ref_Cast || op.opcode == .Addr_Cast {
			verify_operand_reference_like(module, function, function_id, block_id, op, 0, "reference/address cast operand must be reference-like", result)
			verify_result_reference_like(module, function, function_id, block_id, op, 0, "reference/address cast result must be reference-like", result)
		}
	case .Alloca:
		verify_op_arity(result, function_id, block_id, op, 0, 1, 1, 1)
		verify_result_reference_like(module, function, function_id, block_id, op, 0, "alloca result must be reference-like", result)
	case .Addr_Of:
		verify_op_arity(result, function_id, block_id, op, 0, 0, 1, 1)
		verify_slot_address_attrs(function, function_id, block_id, op, result)
		verify_result_reference_like(module, function, function_id, block_id, op, 0, "addr_of result must be reference-like", result)
	case .Deref:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		verify_operand_reference_like(module, function, function_id, block_id, op, 0, "deref operand must be reference-like", result)
		verify_operand_data_reference(module, function, function_id, block_id, op, 0, "deref operand must be a data reference", result)
	case .Field_Addr:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		verify_field_projection_attrs(function, function_id, block_id, op, result)
		verify_operand_address_base(module, function, function_id, block_id, op, 0, "field_addr base must be aggregate or reference-like", result)
		verify_result_reference_like(module, function, function_id, block_id, op, 0, "field_addr result must be reference-like", result)
	case .Index_Addr, .Table_Row_Addr:
		verify_op_arity(result, function_id, block_id, op, 2, -1, 1, 1)
		verify_result_reference_like(module, function, function_id, block_id, op, 0, "indexed address result must be reference-like", result)
	case .Intrinsic:
		verify_intrinsic_call(module, function, function_id, block_id, op, result)
	case .Load:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_load_store_address_operands(module, function, function_id, block_id, op, result)
		verify_load_result_matches_address(module, function, function_id, block_id, op, result)
	case .Store:
		verify_op_arity(result, function_id, block_id, op, 3, 3, 1, 1)
		verify_load_store_address_operands(module, function, function_id, block_id, op, result)
		verify_store_value_matches_address(module, function, function_id, block_id, op, result)
	case .Struct_Init:
		verify_op_arity(result, function_id, block_id, op, 0, -1, 1, 1)
		verify_result_aggregate_like(module, function, function_id, block_id, op, 0, "struct_init result must be aggregate-like", result)
	case .Extract_Value:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		verify_field_projection_attrs(function, function_id, block_id, op, result)
	case .Insert_Value:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_operand_result_types_match(function, function_id, block_id, op, 0, 0, "insert_value result type must match aggregate operand", result)
	case .Call:
		verify_direct_call_effects(function_id, block_id, op, result)
		attrs, attrs_ok := verify_call_attrs(module, function_id, block_id, op, result)
		verify_call_signature(module, function, function_id, block_id, op, attrs, attrs_ok, result)
	case .Invoke:
		if op.intrinsic != INVALID_INTRINSIC_ID {
			verify_intrinsic_call(module, function, function_id, block_id, op, result)
		} else {
			verify_direct_call_effects(function_id, block_id, op, result)
			attrs, attrs_ok := verify_call_attrs(module, function_id, block_id, op, result)
			verify_call_signature(module, function, function_id, block_id, op, attrs, attrs_ok, result)
		}
		verify_invoke_successors(module, function, function_id, block_id, op, result)
	case .Br:
		if !is_terminator {
			verify_add(result, .Bad_Op_Signature, "control-flow opcode must be represented by the block terminator", function_id, block_id, op.id, source = op.source)
		}
		if len(op.results) != 0 || len(op.successors) != 1 {
			verify_add(result, .Bad_Terminator_Args, "branch terminator must have one successor and no results", function_id, block_id, op.id, source = op.source)
		}
	case .Cond_Br:
		if !is_terminator {
			verify_add(result, .Bad_Op_Signature, "control-flow opcode must be represented by the block terminator", function_id, block_id, op.id, source = op.source)
		}
		if len(op.operands) < 1 || len(op.results) != 0 || len(op.successors) != 2 {
			verify_add(result, .Bad_Terminator_Args, "conditional branch terminator must have a condition, two successors, and no results", function_id, block_id, op.id, source = op.source)
		}
	case .Switch:
		if !is_terminator {
			verify_add(result, .Bad_Op_Signature, "control-flow opcode must be represented by the block terminator", function_id, block_id, op.id, source = op.source)
		}
		if len(op.operands) < 1 || len(op.results) != 0 || len(op.successors) < 1 {
			verify_add(result, .Bad_Terminator_Args, "switch terminator must have a selector, default successor, and no results", function_id, block_id, op.id, source = op.source)
		}
	case .Return:
		if !is_terminator {
			verify_add(result, .Bad_Op_Signature, "control-flow opcode must be represented by the block terminator", function_id, block_id, op.id, source = op.source)
		}
		if len(op.results) != 0 || len(op.successors) != 0 {
			verify_add(result, .Bad_Terminator_Args, "return terminator must not have results or successors", function_id, block_id, op.id, source = op.source)
		}
	case .Unreachable:
		if !is_terminator {
			verify_add(result, .Bad_Op_Signature, "control-flow opcode must be represented by the block terminator", function_id, block_id, op.id, source = op.source)
		}
		if len(op.operands) != 0 || len(op.results) != 0 || len(op.successors) != 0 {
			verify_add(result, .Bad_Terminator_Args, "unreachable terminator must not have operands, results, or successors", function_id, block_id, op.id, source = op.source)
		}
	case .Trap:
		verify_op_arity(result, function_id, block_id, op, 0, -1, 0, 0)
		if !(.May_Trap in op.effects) {
			verify_add(result, .Bad_Op_Signature, "trap instruction must carry may-trap effect", function_id, block_id, op.id, source = op.source)
		}
		if attrs, ok := op.attrs.(Trap_Attrs); !ok || attrs.message == "" {
			verify_add(result, .Bad_Op_Signature, "trap instruction must carry message attrs", function_id, block_id, op.id, source = op.source)
		}
		if !verify_source_loc_has_provenance(op.source) {
			verify_add(result, .Bad_Op_Signature, "trap instruction must carry source provenance", function_id, block_id, op.id, source = op.source)
		}
	case .Debug_Value:
		verify_op_arity(result, function_id, block_id, op, 1, -1, 0, 0)
		if op.effects != {} {
			verify_add(result, .Bad_Op_Signature, "debug_value instruction must not carry effects", function_id, block_id, op.id, source = op.source)
		}
	case .Unsupported:
		if !(.Unsupported in op.effects) {
			verify_add(result, .Bad_Op_Signature, "unsupported instruction must carry unsupported effect", function_id, block_id, op.id, source = op.source)
		}
		if attrs, ok := op.attrs.(Unsupported_Attrs); !ok || attrs.message == "" {
			verify_add(result, .Bad_Op_Signature, "unsupported instruction must carry message attrs", function_id, block_id, op.id, source = op.source)
		}
		if !verify_source_loc_has_provenance(op.source) {
			verify_add(result, .Bad_Op_Signature, "unsupported instruction must carry source provenance", function_id, block_id, op.id, source = op.source)
		}
	}
	verify_memory_requirements(function_id, block_id, op, result)
	for access in op.memory {
		verify_memory_access(module, function_id, block_id, op, access, result)
	}
}

verify_world_value_positions :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	is_terminator: bool,
	result: ^Verify_Result,
) {
	effect_carrier := op.effects != {} || op.opcode == .Intrinsic || op.opcode == .Invoke
	for operand, i in op.operands {
		if !verify_value_has_type(function, operand, BUILTIN_TYPE_WORLD) {
			continue
		}
		allowed := effect_carrier && i == 0
		if is_terminator && op.opcode == .Return {
			allowed = i == 0
		}
		for edge in op.successors {
			start := int(edge.operand_start)
			if i >= start && i < start + int(edge.operand_count) {
				allowed = true
				break
			}
		}
		if !allowed {
			verify_add(result, .Bad_World_Chain, "world token appears in a non-effect operand position", function_id, block_id, op.id, operand, op.source)
		}
	}
	for value, i in op.results {
		if verify_value_has_type(function, value, BUILTIN_TYPE_WORLD) && !(effect_carrier && i == 0) {
			verify_add(result, .Bad_World_Chain, "world token appears in a non-effect result position", function_id, block_id, op.id, value, op.source)
		}
	}
}

verify_const_attrs :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	constant, ok := op.attrs.(Constant_Id)
	if !ok || constant == INVALID_CONSTANT_ID || int(constant) >= len(module.constants) {
		verify_add(result, .Bad_Op_Signature, "const instruction must reference valid constant attrs", function_id, block_id, op.id, source = op.source)
		return
	}
	if len(op.results) != 1 || !verify_value_id_valid(function, op.results[0]) {
		return
	}
	result_type := value_type(function, op.results[0])
	constant_type := module.constants[int(constant)].type
	if verify_type_valid(module, result_type) && verify_type_valid(module, constant_type) && result_type != constant_type {
		verify_add(result, .Bad_Op_Signature, "const result type must match constant type", function_id, block_id, op.id, op.results[0], op.source)
	}
}

verify_storage_types_compatible :: proc(module: ^Module, storage, value: Type_Id) -> bool {
	if verify_types_compatible(storage, value) {
		return true
	}
	storage_kind, storage_ok := verify_type_kind(module, storage)
	value_kind, value_ok := verify_type_kind(module, value)
	if !storage_ok || !value_ok {
		return false
	}
	if storage_kind == .Semantic || value_kind == .Semantic {
		return true
	}
	return verify_type_text_like(storage_kind) && verify_type_text_like(value_kind)
}

verify_type_text_like :: proc "contextless" (kind: Type_Kind) -> bool {
	#partial switch kind {
	case .String, .Char, .Numc, .Unknown:
		return true
	case:
		return false
	}
}

verify_instruction_successor_policy :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	is_terminator: bool,
	result: ^Verify_Result,
) {
	if len(op.successors) == 0 {
		return
	}
	successors_allowed := op.opcode == .Invoke ||
	                      is_terminator && (op.opcode == .Br || op.opcode == .Cond_Br || op.opcode == .Switch)
	if !successors_allowed {
		verify_add(result, .Bad_Exception_Edge, "only invoke or branch terminators may carry canonical successor edges", function_id, block_id, op.id, source = op.source)
	}
	for edge in op.successors {
		if edge.kind == .Exception || edge.kind == .Cleanup {
			if op.opcode != .Invoke {
				verify_add(result, .Bad_Exception_Edge, "exception edges are only valid on invoke instructions", function_id, block_id, op.id, source = op.source)
			}
		}
		verify_instruction_successor_edge(module, function, function_id, block_id, op, edge, result)
	}
}

verify_instruction_successor_edge :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	edge: Successor_Edge,
	result: ^Verify_Result,
) {
	if edge.target == INVALID_BLOCK_ID || int(edge.target) >= len(function.blocks) {
		verify_add(result, .Invalid_Block, "instruction successor target is invalid", function_id, block_id, op.id, source = op.source)
		return
	}
	if int(edge.operand_start) > len(op.operands) || int(edge.operand_count) > len(op.operands) - int(edge.operand_start) {
		verify_add(result, .Bad_Exception_Edge, "instruction successor operand slice is outside operands", function_id, block_id, op.id, source = op.source)
		return
	}
	if int(edge.operand_count) != len(edge.args) {
		verify_add(result, .Bad_Exception_Edge, "instruction successor operand count must match edge args", function_id, block_id, op.id, source = op.source)
		return
	}
	for arg, i in edge.args {
		if op.operands[int(edge.operand_start) + i] != arg {
			verify_add(result, .Bad_Exception_Edge, "instruction successor edge args must mirror operand slice", function_id, block_id, op.id, arg, op.source)
		}
	}
	verify_branch_target(module, function, function_id, block_id, edge.target, edge.args[:], result, op.source)
}

verify_core_effect_policy :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if op.opcode == .Intrinsic || op.effects == {} {
		return
	}
	allowed := verify_core_allowed_effects(op.opcode)
	if !verify_effects_subset(op.effects, allowed) {
		verify_add(result, .Bad_Op_Signature, "core opcode must not carry unsupported effects", function_id, block_id, op.id, source = op.source)
		return
	}
	if len(op.operands) == 0 || !verify_value_has_type(function, op.operands[0], BUILTIN_TYPE_WORLD) {
		verify_add(result, .Bad_Op_Signature, "effectful core instruction must take world as first operand", function_id, block_id, op.id, source = op.source)
	}
	if verify_core_effect_writes_world(op) &&
	   (len(op.results) == 0 || !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_WORLD)) {
		verify_add(result, .Bad_Op_Signature, "effectful core instruction must produce world as first result", function_id, block_id, op.id, source = op.source)
	}
}

verify_core_allowed_effects :: proc "contextless" (opcode: Opcode) -> Effect_Set {
	#partial switch opcode {
	case .Load:
		return {.Read_Local, .Read_Global, .Read_System, .Read_Table, .SQL, .May_Trap}
	case .Store:
		return {
			.Read_Local,
			.Write_Local,
			.Read_Global,
			.Write_Global,
			.Read_System,
			.Write_System,
			.Read_Table,
			.Write_Table,
			.SQL,
			.May_Trap,
		}
	case .Alloca:
		return {.Write_Local, .May_Trap}
	case .Call, .Invoke:
		return {
			.Read_Local,
			.Write_Local,
			.Read_Global,
			.Write_Global,
			.Read_System,
			.Write_System,
			.Read_Table,
			.Write_Table,
			.SQL,
			.IO,
			.May_Trap,
			.May_Throw,
			.Calls_IR,
		}
	case .Trap:
		return {.May_Trap}
	case .Unsupported:
		return {.May_Trap, .Unsupported}
	}
	return {}
}

verify_effects_subset :: proc "contextless" (effects, allowed: Effect_Set) -> bool {
	if .Read_Local in effects && !(.Read_Local in allowed) {
		return false
	}
	if .Write_Local in effects && !(.Write_Local in allowed) {
		return false
	}
	if .Read_Global in effects && !(.Read_Global in allowed) {
		return false
	}
	if .Write_Global in effects && !(.Write_Global in allowed) {
		return false
	}
	if .Read_System in effects && !(.Read_System in allowed) {
		return false
	}
	if .Write_System in effects && !(.Write_System in allowed) {
		return false
	}
	if .Read_Table in effects && !(.Read_Table in allowed) {
		return false
	}
	if .Write_Table in effects && !(.Write_Table in allowed) {
		return false
	}
	if .SQL in effects && !(.SQL in allowed) {
		return false
	}
	if .IO in effects && !(.IO in allowed) {
		return false
	}
	if .May_Trap in effects && !(.May_Trap in allowed) {
		return false
	}
	if .May_Throw in effects && !(.May_Throw in allowed) {
		return false
	}
	if .Calls_IR in effects && !(.Calls_IR in allowed) {
		return false
	}
	if .Calls_Host in effects && !(.Calls_Host in allowed) {
		return false
	}
	if .Unsupported in effects && !(.Unsupported in allowed) {
		return false
	}
	return true
}

verify_core_effect_writes_world :: proc "contextless" (op: Op) -> bool {
	#partial switch op.opcode {
	case .Store, .Alloca, .Call, .Invoke, .Unsupported:
		return true
	}
	return false
}

verify_may_throw_policy :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	options: Verify_Options,
	result: ^Verify_Result,
) {
	if !(.May_Throw in op.effects) {
		return
	}
	if op.opcode == .Invoke {
		return
	}
	if options.allow_legacy_top_level_may_throw_propagation &&
	   verify_legacy_top_level_exception_propagation_allowed(module, op) {
		return
	}
	verify_add(result, .Bad_Exception_Edge, "may-throw operation must use invoke with valid exception edges", function_id, block_id, op.id, source = op.source)
}

verify_legacy_top_level_exception_propagation_allowed :: proc(module: ^Module, op: Op) -> bool {
	#partial switch op.opcode {
	case .Call:
		if !(.Calls_IR in op.effects) || .Calls_Host in op.effects {
			return false
		}
		attrs, attrs_ok := op.attrs.(Call_Attrs)
		return attrs_ok && attrs.target != INVALID_FUNCTION_ID && int(attrs.target) < len(module.functions)
	case .Intrinsic:
		if op.intrinsic == INVALID_INTRINSIC_ID || int(op.intrinsic) >= len(module.intrinsics) {
			return false
		}
		intrinsic := module.intrinsics[int(op.intrinsic)]
		#partial switch intrinsic.op {
		case .ABAP_Exception_Raise,
		     .ABAP_Exception_Match,
		     .ABAP_Exception_Catch,
		     .ABAP_Exception_Unhandled,
		     .Call_Routine,
		     .Call_Method:
			return true
		case:
			return false
		}
	}
	return false
}

verify_operand_type :: proc(function: ^Function, op: Op, index: int) -> (Type_Id, bool) {
	if index < 0 || index >= len(op.operands) {
		return INVALID_TYPE_ID, false
	}
	return verify_value_type_lookup(function, op.operands[index])
}

verify_result_type :: proc(function: ^Function, op: Op, index: int) -> (Type_Id, bool) {
	if index < 0 || index >= len(op.results) {
		return INVALID_TYPE_ID, false
	}
	return verify_value_type_lookup(function, op.results[index])
}

verify_types_compatible :: proc "contextless" (a, b: Type_Id) -> bool {
	return a == b || a == BUILTIN_TYPE_UNKNOWN || b == BUILTIN_TYPE_UNKNOWN
}

verify_type_kind :: proc(module: ^Module, typ: Type_Id) -> (Type_Kind, bool) {
	if !verify_type_valid(module, typ) {
		return .Unknown, false
	}
	return module.types[int(typ)].kind, true
}

verify_type_integer_like :: proc(module: ^Module, typ: Type_Id) -> bool {
	if !verify_type_valid(module, typ) {
		return false
	}
	descriptor := module.types[int(typ)].runtime
	#partial switch descriptor.family {
	case .Integer:
		return true
	}
	kind := module.types[int(typ)].kind
	return kind == .Integer
}

verify_type_numeric_like :: proc(module: ^Module, typ: Type_Id) -> bool {
	if !verify_type_valid(module, typ) {
		return false
	}
	#partial switch module.types[int(typ)].runtime.family {
	case .Numeric, .Integer, .Decimal, .Float:
		return true
	}
	kind := module.types[int(typ)].kind
	return kind == .Integer || kind == .Decimal || kind == .Float
}

verify_type_logical_like :: proc(module: ^Module, typ: Type_Id) -> bool {
	if !verify_type_valid(module, typ) {
		return false
	}
	descriptor := module.types[int(typ)].runtime
	#partial switch descriptor.family {
	case .Predicate, .Integer, .Unknown:
		return true
	}
	kind := module.types[int(typ)].kind
	return kind == .Predicate || kind == .Integer || kind == .Unknown || kind == .Semantic
}

verify_type_reference_like :: proc(module: ^Module, typ: Type_Id) -> bool {
	if !verify_type_valid(module, typ) {
		return false
	}
	descriptor := module.types[int(typ)].runtime
	#partial switch descriptor.family {
	case .Reference, .Object, .Interface, .Exception, .Routine, .Unknown:
		return true
	}
	kind := module.types[int(typ)].kind
	return kind == .Reference ||
	       kind == .Pointer ||
	       kind == .Object ||
	       kind == .Interface ||
	       kind == .Exception ||
	       kind == .Routine ||
	       kind == .Unknown ||
	       kind == .Semantic
}

verify_type_aggregate_like :: proc(module: ^Module, typ: Type_Id) -> bool {
	if !verify_type_valid(module, typ) {
		return false
	}
	descriptor := module.types[int(typ)].runtime
	#partial switch descriptor.family {
	case .Structure, .Table, .Unknown:
		return true
	}
	kind := module.types[int(typ)].kind
	return kind == .Structure || kind == .Struct || kind == .Table || kind == .Unknown || kind == .Semantic
}

verify_integer_operands_and_results :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	for operand in op.operands {
		typ, ok := verify_value_type_lookup(function, operand)
		if ok && !verify_type_integer_like(module, typ) {
			verify_add(result, .Bad_Op_Signature, "integer operation operand must have integer type", function_id, block_id, op.id, operand, op.source)
		}
	}
	for value in op.results {
		typ, ok := verify_value_type_lookup(function, value)
		if ok && !verify_type_integer_like(module, typ) {
			verify_add(result, .Bad_Op_Signature, "integer operation result must have integer type", function_id, block_id, op.id, value, op.source)
		}
	}
}

verify_numeric_operands_and_results :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	for operand in op.operands {
		typ, ok := verify_value_type_lookup(function, operand)
		if ok && !verify_type_numeric_like(module, typ) {
			verify_add(result, .Bad_Op_Signature, "numeric operation operand must have numeric type", function_id, block_id, op.id, operand, op.source)
		}
	}
	for value in op.results {
		typ, ok := verify_value_type_lookup(function, value)
		if ok && !verify_type_numeric_like(module, typ) {
			verify_add(result, .Bad_Op_Signature, "numeric operation result must have numeric type", function_id, block_id, op.id, value, op.source)
		}
	}
}

verify_logical_operands_and_results :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	for operand in op.operands {
		typ, ok := verify_value_type_lookup(function, operand)
		if ok && !verify_type_logical_like(module, typ) {
			verify_add(result, .Bad_Op_Signature, "logical operation operand must have predicate or integer type", function_id, block_id, op.id, operand, op.source)
		}
	}
	for value in op.results {
		typ, ok := verify_value_type_lookup(function, value)
		if ok && !verify_type_logical_like(module, typ) {
			verify_add(result, .Bad_Op_Signature, "logical operation result must have predicate or integer type", function_id, block_id, op.id, value, op.source)
		}
	}
}

verify_operand_predicate :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	index: int,
	message: string,
	result: ^Verify_Result,
) {
	typ, ok := verify_operand_type(function, op, index)
	if ok && verify_type_valid(module, typ) && typ != BUILTIN_TYPE_PREDICATE && typ != BUILTIN_TYPE_UNKNOWN {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.operands[index], op.source)
	}
}

verify_operand_reference_like :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	index: int,
	message: string,
	result: ^Verify_Result,
) {
	typ, ok := verify_operand_type(function, op, index)
	if ok && !verify_type_reference_like(module, typ) {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.operands[index], op.source)
	}
}

verify_operand_data_reference :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	index: int,
	message: string,
	result: ^Verify_Result,
) {
	typ, ok := verify_operand_type(function, op, index)
	if !ok || !verify_type_valid(module, typ) {
		return
	}
	if !verify_type_reference_like(module, typ) {
		return
	}
	descriptor := module.types[int(typ)].runtime
	if descriptor.family == .Unknown {
		return
	}
	if descriptor.family != .Reference {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.operands[index], op.source)
		return
	}
	if descriptor.reference.kind != .Data && descriptor.reference.kind != .Unknown {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.operands[index], op.source)
	}
}

verify_result_reference_like :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	index: int,
	message: string,
	result: ^Verify_Result,
) {
	typ, ok := verify_result_type(function, op, index)
	if ok && !verify_type_reference_like(module, typ) {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.results[index], op.source)
	}
}

verify_result_aggregate_like :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	index: int,
	message: string,
	result: ^Verify_Result,
) {
	typ, ok := verify_result_type(function, op, index)
	if ok && !verify_type_aggregate_like(module, typ) {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.results[index], op.source)
	}
}

verify_operand_types_match :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	left, right: int,
	message: string,
	result: ^Verify_Result,
) {
	left_type, left_ok := verify_operand_type(function, op, left)
	right_type, right_ok := verify_operand_type(function, op, right)
	if left_ok && right_ok && !verify_types_compatible(left_type, right_type) {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.operands[right], op.source)
	}
}

verify_operand_result_types_match :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	operand_index, result_index: int,
	message: string,
	result: ^Verify_Result,
) {
	operand_type, operand_ok := verify_operand_type(function, op, operand_index)
	result_type, result_ok := verify_result_type(function, op, result_index)
	if operand_ok && result_ok && !verify_types_compatible(operand_type, result_type) {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.results[result_index], op.source)
	}
}

verify_field_projection_attrs :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if projection, ok := op.attrs.(Projection_Id); ok {
		if projection == INVALID_PROJECTION_ID || int(projection) >= len(function.projections) {
			verify_add(result, .Bad_Op_Signature, "field operation must reference valid projection attrs", function_id, block_id, op.id, source = op.source)
			return
		}
		path := projection_ptr(function, projection)
		if len(path.segments) == 0 {
			verify_add(result, .Bad_Op_Signature, "field operation projection path must not be empty", function_id, block_id, op.id, source = op.source)
			return
		}
		for segment in path.segments {
			if segment.name == "" {
				verify_add(result, .Bad_Op_Signature, "projection segment must have a field name", function_id, block_id, op.id, source = op.source)
			}
		}
		return
	}
	verify_add(result, .Bad_Op_Signature, "field operation must carry projection attrs", function_id, block_id, op.id, source = op.source)
}

verify_slot_address_attrs :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	attrs, ok := op.attrs.(Slot_Address_Attrs)
	if !ok {
		verify_add(result, .Bad_Op_Signature, "addr_of instruction must carry slot address attrs", function_id, block_id, op.id, source = op.source)
		return
	}
	if attrs.slot == INVALID_SLOT_ID || int(attrs.slot) >= len(function.slots) {
		verify_add(result, .Bad_Op_Signature, "slot address instruction must reference a valid slot", function_id, block_id, op.id, source = op.source)
	}
}

verify_operand_address_base :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	operand_index: int,
	message: string,
	result: ^Verify_Result,
) {
	typ, ok := verify_operand_type(function, op, operand_index)
	if !ok {
		return
	}
	if !verify_type_reference_like(module, typ) && !verify_type_aggregate_like(module, typ) {
		verify_add(result, .Bad_Op_Signature, message, function_id, block_id, op.id, op.operands[operand_index], op.source)
	}
}

verify_load_result_matches_address :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if len(op.results) != 1 {
		return
	}
	result_type, result_ok := verify_result_type(function, op, 0)
	if !result_ok {
		return
	}
	for access in op.memory {
		if access.kind == .Read || access.kind == .Read_Write {
			if verify_type_valid(module, access.type) && verify_type_valid(module, result_type) && !verify_storage_types_compatible(module, access.type, result_type) {
				verify_add(result, .Bad_Op_Signature, "load result type must match memory access type", function_id, block_id, op.id, op.results[0], op.source)
			}
			return
		}
	}
}

verify_store_value_matches_address :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if len(op.results) > 0 && !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_WORLD) {
		verify_add(result, .Bad_Op_Signature, "store instruction must produce world as first result", function_id, block_id, op.id, op.results[0], op.source)
	}
	if len(op.operands) <= 2 {
		return
	}
	value_type_id, value_ok := verify_operand_type(function, op, 2)
	if !value_ok {
		return
	}
	for access in op.memory {
		if access.kind == .Write || access.kind == .Read_Write {
			if verify_type_valid(module, access.type) && verify_type_valid(module, value_type_id) && !verify_storage_types_compatible(module, access.type, value_type_id) {
				verify_add(result, .Bad_Op_Signature, "store value type must match memory access type", function_id, block_id, op.id, op.operands[2], op.source)
			}
			return
		}
	}
}

verify_call_signature :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	attrs: Call_Attrs,
	attrs_ok: bool,
	result: ^Verify_Result,
) {
	if !attrs_ok || attrs.target == INVALID_FUNCTION_ID || int(attrs.target) >= len(module.functions) {
		return
	}
	target := &module.functions[int(attrs.target)]
	if target.entry == INVALID_BLOCK_ID ||
	   int(target.entry) >= len(target.blocks) ||
	   len(block_ptr(target, target.entry).args) != len(target.signature.params) ||
	   len(target.return_types) != len(target.signature.results) {
		verify_add(result, .Bad_Op_Signature, "call target signature is missing or inconsistent", function_id, block_id, op.id, source = op.source)
		return
	}
	call_operand_count := verify_call_operand_count(op)
	if call_operand_count != len(target.signature.params) {
		verify_add(result, .Bad_Op_Signature, "call operand count must match callee signature parameters", function_id, block_id, op.id, source = op.source)
	} else {
		for expected, i in target.signature.params {
			actual, actual_ok := verify_operand_type(function, op, i)
			if actual_ok && verify_type_valid(module, expected) && verify_type_valid(module, actual) && !verify_types_compatible(expected, actual) {
				verify_add(result, .Bad_Op_Signature, "call operand type must match callee signature parameter", function_id, block_id, op.id, op.operands[i], op.source)
			}
		}
	}
	if len(op.results) != len(target.signature.results) {
		verify_add(result, .Bad_Op_Signature, "call result count must match callee signature results", function_id, block_id, op.id, source = op.source)
	} else {
		for expected, i in target.signature.results {
			actual, actual_ok := verify_result_type(function, op, i)
			if actual_ok && verify_type_valid(module, expected) && verify_type_valid(module, actual) && !verify_types_compatible(expected, actual) {
				verify_add(result, .Bad_Op_Signature, "call result type must match callee signature result", function_id, block_id, op.id, op.results[i], op.source)
			}
		}
	}
}

verify_call_operand_count :: proc "contextless" (op: Op) -> int {
	count := len(op.operands)
	if op.opcode == .Invoke {
		for edge in op.successors {
			if int(edge.operand_start) < count {
				count = int(edge.operand_start)
			}
		}
	}
	return count
}

verify_direct_call_effects :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if !(.Calls_IR in op.effects) {
		verify_add(result, .Bad_Op_Signature, "direct call instruction must carry calls-ir effect", function_id, block_id, op.id, source = op.source)
	}
}

verify_invoke_successors :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	normal_count := 0
	exception_count := 0
	for edge in op.successors {
		#partial switch edge.kind {
		case .Normal:
			normal_count += 1
		case .Exception:
			exception_count += 1
		case:
			verify_add(result, .Bad_Exception_Edge, "invoke may only have normal and exception successors", function_id, block_id, op.id, source = op.source)
		}
		verify_instruction_successor_edge(module, function, function_id, block_id, op, edge, result)
	}
	if normal_count != 1 {
		verify_add(result, .Bad_Exception_Edge, "invoke must have exactly one normal successor", function_id, block_id, op.id, source = op.source)
	}
	if exception_count != 1 {
		verify_add(result, .Bad_Exception_Edge, "invoke must have exactly one exception successor", function_id, block_id, op.id, source = op.source)
	}
	if !(.May_Throw in op.effects) {
		verify_add(result, .Bad_Exception_Edge, "invoke must carry may-throw effect", function_id, block_id, op.id, source = op.source)
	}
}

verify_memory_requirements :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	requires_memory := op.opcode == .Load ||
	                   op.opcode == .Store ||
	                   op.opcode == .Alloca ||
	                   .Read_Local in op.effects ||
	                   .Write_Local in op.effects ||
	                   .Read_Global in op.effects ||
	                   .Write_Global in op.effects ||
	                   .Read_System in op.effects ||
	                   .Write_System in op.effects ||
	                   .Read_Table in op.effects ||
	                   .Write_Table in op.effects ||
	                   .SQL in op.effects
	if requires_memory && len(op.memory) == 0 {
		verify_add(result, .Bad_Memory_Alias, "memory-effect instruction must carry memory metadata", function_id, block_id, op.id, source = op.source)
	}
	if op.opcode == .Load && !verify_memory_has_kind(op.memory[:], .Read) {
		verify_add(result, .Bad_Memory_Alias, "load instruction must carry read memory metadata", function_id, block_id, op.id, source = op.source)
	}
	if op.opcode == .Store && !verify_memory_has_kind(op.memory[:], .Write) {
		verify_add(result, .Bad_Memory_Alias, "store instruction must carry write memory metadata", function_id, block_id, op.id, source = op.source)
	}
}

verify_memory_has_kind :: proc "contextless" (memory: []Memory_Access, kind: Memory_Access_Kind) -> bool {
	for access in memory {
		if access.kind == kind || access.kind == .Read_Write {
			return true
		}
	}
	return false
}

verify_load_store_address_operands :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if len(op.operands) < 2 {
		return
	}
	if !verify_value_has_type(function, op.operands[0], BUILTIN_TYPE_WORLD) {
		verify_add(result, .Bad_Op_Signature, "memory instruction must take world as first operand", function_id, block_id, op.id, op.operands[0], op.source)
	}
	address_type, address_ok := verify_operand_type(function, op, 1)
	if address_ok && !verify_type_reference_like(module, address_type) {
		verify_add(result, .Bad_Op_Signature, "memory instruction address operand must be reference-like", function_id, block_id, op.id, op.operands[1], op.source)
	}
}

verify_same_operand_result_types :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if len(op.operands) == 0 || len(op.results) == 0 {
		return
	}
	first, first_ok := verify_operand_type(function, op, 0)
	if !first_ok {
		return
	}
	for operand in op.operands[1:] {
		typ, typ_ok := verify_value_type_lookup(function, operand)
		if !typ_ok {
			continue
		}
		if typ != first && typ != BUILTIN_TYPE_UNKNOWN && first != BUILTIN_TYPE_UNKNOWN {
			verify_add(result, .Bad_Op_Signature, "binary operand types must match", function_id, block_id, op.id, operand, op.source)
		}
	}
	result_type, result_type_ok := verify_result_type(function, op, 0)
	if !result_type_ok {
		return
	}
	if result_type != first && result_type != BUILTIN_TYPE_UNKNOWN && first != BUILTIN_TYPE_UNKNOWN {
		verify_add(result, .Bad_Op_Signature, "binary result type must match operands", function_id, block_id, op.id, op.results[0], op.source)
	}
}

verify_intrinsic_call :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if op.intrinsic == INVALID_INTRINSIC_ID || int(op.intrinsic) >= len(module.intrinsics) {
		verify_add(result, .Bad_Intrinsic, "intrinsic instruction references invalid intrinsic declaration", function_id, block_id, op.id, source = op.source)
		return
	}
	if attrs, ok := op.attrs.(Intrinsic_Call_Attrs); !ok || attrs.intrinsic != op.intrinsic {
		verify_add(result, .Bad_Intrinsic, "intrinsic instruction attrs must mirror intrinsic declaration id", function_id, block_id, op.id, source = op.source)
	}
	intrinsic := module.intrinsics[int(op.intrinsic)]
	if intrinsic.effects != op.effects {
		verify_add(result, .Bad_Intrinsic, "intrinsic instruction effects must match declaration", function_id, block_id, op.id, source = op.source)
	}
	if intrinsic.signature.effects != op.effects {
		verify_add(result, .Bad_Intrinsic, "intrinsic instruction effects must match signature effects", function_id, block_id, op.id, source = op.source)
	}
	if intrinsic.signature.can_throw != (.May_Throw in op.effects) {
		verify_add(result, .Bad_Intrinsic, "intrinsic call can_throw flag must match may-throw effect", function_id, block_id, op.id, source = op.source)
	}
	if intrinsic.signature.can_trap != (.May_Trap in op.effects) {
		verify_add(result, .Bad_Intrinsic, "intrinsic call can_trap flag must match may-trap effect", function_id, block_id, op.id, source = op.source)
	}
	call_operand_count := verify_call_operand_count(op)
	if len(intrinsic.signature.params) != call_operand_count {
		verify_add(result, .Bad_Intrinsic, "intrinsic operand count must match signature", function_id, block_id, op.id, source = op.source)
	} else {
		for expected, i in intrinsic.signature.params {
			actual := value_type(function, op.operands[i])
			if expected != actual && expected != BUILTIN_TYPE_UNKNOWN && actual != BUILTIN_TYPE_UNKNOWN {
				verify_add(result, .Bad_Intrinsic, "intrinsic operand type must match signature", function_id, block_id, op.id, op.operands[i], op.source)
			}
		}
	}
	if len(intrinsic.signature.results) != len(op.results) {
		verify_add(result, .Bad_Intrinsic, "intrinsic result count must match signature", function_id, block_id, op.id, source = op.source)
	} else {
		for expected, i in intrinsic.signature.results {
			actual := value_type(function, op.results[i])
			if expected != actual && expected != BUILTIN_TYPE_UNKNOWN && actual != BUILTIN_TYPE_UNKNOWN {
				verify_add(result, .Bad_Intrinsic, "intrinsic result type must match signature", function_id, block_id, op.id, op.results[i], op.source)
			}
		}
	}
	verify_intrinsic_payload(module, function, function_id, block_id, op, intrinsic, result)
}

verify_intrinsic_payload :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
) {
	#partial switch intrinsic.op {
	case .ABAP_Move:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
	case .ABAP_Add,
	     .ABAP_Subtract,
	     .ABAP_Multiply,
	     .ABAP_Divide,
	     .ABAP_Integer_Divide,
	     .ABAP_Modulo:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_numeric_operands_and_results(module, function, function_id, block_id, op, result)
	case .ABAP_Equal,
	     .ABAP_Not_Equal,
	     .ABAP_Less,
	     .ABAP_Less_Equal,
	     .ABAP_Greater,
	     .ABAP_Greater_Equal:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		if len(op.results) == 1 && !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_PREDICATE) {
			verify_add(result, .Bad_Intrinsic, "ABAP comparison intrinsic must produce predicate", function_id, block_id, op.id, op.results[0], op.source)
		}
	case .ABAP_And,
	     .ABAP_Or:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		if len(op.results) == 1 && !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_PREDICATE) {
			verify_add(result, .Bad_Intrinsic, "ABAP boolean intrinsic must produce predicate", function_id, block_id, op.id, op.results[0], op.source)
		}
	case .ABAP_Not,
	     .ABAP_Is_Initial:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		if len(op.results) == 1 && !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_PREDICATE) {
			verify_add(result, .Bad_Intrinsic, "ABAP predicate intrinsic must produce predicate", function_id, block_id, op.id, op.results[0], op.source)
		}
	case .ABAP_String_Concat:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
	case .ABAP_String_Template:
		verify_op_arity(result, function_id, block_id, op, 0, -1, 1, 1)
	case .ABAP_Construct:
		verify_op_arity(result, function_id, block_id, op, 0, -1, 1, 1)
		verify_intrinsic_call_payload(function_id, block_id, op, intrinsic, result, require_name = intrinsic.op == .ABAP_Construct)
	case .ABAP_Concatenate,
	     .ABAP_Condense,
	     .ABAP_Translate,
	     .ABAP_Replace,
	     .ABAP_Shift,
	     .ABAP_Find:
		verify_intrinsic_string_payload(function_id, block_id, op, intrinsic, result)
	case .ABAP_Split:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, -1)
	case .ABAP_Search:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 2, 2)
	case .ABAP_Exception_Raise:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_exception_payload(function_id, block_id, op, intrinsic, result, true)
	case .ABAP_Exception_Match:
		verify_intrinsic_world_read_signature(function, function_id, block_id, op, result)
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		if len(op.results) == 1 && !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_PREDICATE) {
			verify_add(result, .Bad_Intrinsic, "exception match intrinsic must produce predicate", function_id, block_id, op.id, op.results[0], op.source)
		}
		verify_intrinsic_exception_payload(function_id, block_id, op, intrinsic, result, true)
	case .ABAP_Exception_Catch:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 2)
	case .ABAP_Exception_Unhandled:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		if !(.May_Trap in op.effects) {
			verify_add(result, .Bad_Intrinsic, "exception unhandled intrinsic must be marked may-trap", function_id, block_id, op.id, source = op.source)
		}
	case .ABAP_Message:
		verify_intrinsic_message_payload(function_id, block_id, op, intrinsic, result)
	case .ABAP_Write:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_op_arity(result, function_id, block_id, op, 1, -1, 1, 1)
	case .ABAP_Clear:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_op_arity(result, function_id, block_id, op, 1, 2, 2, 2)
	case .ABAP_Refresh,
	     .ABAP_Free,
	     .ABAP_Unassign:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_op_arity(result, function_id, block_id, op, 1, 1, 2, 2)
	case .ABAP_Assign_Field:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_op_arity(result, function_id, block_id, op, 2, 3, 2, 2)
	case .Call_Builtin:
		verify_op_arity(result, function_id, block_id, op, 0, -1, 1, 1)
		verify_intrinsic_call_payload(function_id, block_id, op, intrinsic, result, .Builtin)
	case .Call_Routine:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_call_payload(function_id, block_id, op, intrinsic, result)
	case .Call_Method:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_call_payload(function_id, block_id, op, intrinsic, result, .Method)
	case .Table_Iter:
		verify_op_arity(result, function_id, block_id, op, 2, -1, 2, 2)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_table_payload(module, function, function_id, block_id, op, intrinsic, result)
		verify_table_iter_signature(function, function_id, block_id, op, result)
	case .Table_Next:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 5, 5)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_table_payload(module, function, function_id, block_id, op, intrinsic, result)
		verify_table_next_signature(function, function_id, block_id, op, result)
	case .Table_Read:
		verify_op_arity(result, function_id, block_id, op, 2, -1, 4, 4)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_table_payload(module, function, function_id, block_id, op, intrinsic, result, require_result = true)
	case .Table_Append,
	     .Table_Insert,
	     .Table_Modify,
	     .Table_Delete,
	     .Table_Sort:
		verify_op_arity(result, function_id, block_id, op, 2, -1, 3, 3)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_table_payload(module, function, function_id, block_id, op, intrinsic, result)
	case .SQL_Select:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 3, 3)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_sql_payload(module, function_id, block_id, op, intrinsic, result, true)
	case .SQL_Open_Cursor,
	     .SQL_Fetch:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_sql_payload(module, function_id, block_id, op, intrinsic, result, intrinsic.op != .SQL_Fetch, allow_unknown_source = intrinsic.op == .SQL_Fetch)
	case .SQL_Close_Cursor:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
	case .SQL_Insert,
	     .SQL_Update,
	     .SQL_Delete,
	     .SQL_Modify:
		verify_op_arity(result, function_id, block_id, op, 1, -1, 1, 1)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_sql_payload(module, function_id, block_id, op, intrinsic, result, false)
	case .System_Read:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		verify_intrinsic_world_read_signature(function, function_id, block_id, op, result)
		verify_intrinsic_system_payload(function_id, block_id, op, intrinsic, result)
	case .System_Write:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_system_payload(function_id, block_id, op, intrinsic, result)
	case .Host_Call:
		verify_intrinsic_world_signature(function, function_id, block_id, op, result, true)
		verify_intrinsic_host_payload(function_id, block_id, op, intrinsic, result)
	case .Unsupported:
		if !(.Unsupported in op.effects) {
			verify_add(result, .Bad_Intrinsic, "unsupported intrinsic must carry unsupported effect", function_id, block_id, op.id, source = op.source)
		}
		verify_intrinsic_unsupported_payload(function_id, block_id, op, intrinsic, result)
	case .Unknown:
		verify_add(result, .Bad_Intrinsic, "intrinsic call must reference supported intrinsic operation", function_id, block_id, op.id, source = op.source)
	}
}

verify_intrinsic_world_signature :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
	writes: bool,
) {
	verify_intrinsic_world_read_signature(function, function_id, block_id, op, result)
	if writes {
		if len(op.results) == 0 || !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_WORLD) {
			verify_add(result, .Bad_Intrinsic, "effectful intrinsic must produce world as first result", function_id, block_id, op.id, source = op.source)
		}
	}
}

verify_intrinsic_world_read_signature :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if len(op.operands) == 0 || !verify_value_has_type(function, op.operands[0], BUILTIN_TYPE_WORLD) {
		verify_add(result, .Bad_Intrinsic, "effectful intrinsic must take world as first operand", function_id, block_id, op.id, source = op.source)
	}
}

verify_intrinsic_call_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
	expected_kind: Abap_Call_Kind = .Unknown,
	require_name: bool = true,
) {
	payload, payload_ok := intrinsic.payload.(Intrinsic_Call_Payload)
	if !payload_ok {
		verify_add(result, .Bad_Intrinsic, "call intrinsic must carry call payload", function_id, block_id, op.id, source = op.source)
		return
	}
	if require_name && payload.callee_name == "" {
		verify_add(result, .Bad_Intrinsic, "call intrinsic must carry callee name", function_id, block_id, op.id, source = op.source)
	}
	if payload.call_kind == .Unknown && intrinsic.op != .ABAP_Construct {
		verify_add(result, .Bad_Intrinsic, "call intrinsic must carry call kind", function_id, block_id, op.id, source = op.source)
	}
	if expected_kind != .Unknown && payload.call_kind != expected_kind {
		verify_add(result, .Bad_Intrinsic, "call intrinsic kind does not match declaration", function_id, block_id, op.id, source = op.source)
	}
	if payload.has_call_function_target && payload.call_function_target == INVALID_FUNCTION_ID {
		verify_add(result, .Bad_Intrinsic, "call intrinsic target flag must carry function target", function_id, block_id, op.id, source = op.source)
	}
}

verify_intrinsic_exception_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
	require_name: bool,
) {
	payload, payload_ok := intrinsic.payload.(Intrinsic_Exception_Payload)
	if !payload_ok {
		verify_add(result, .Bad_Intrinsic, "exception intrinsic must carry exception payload", function_id, block_id, op.id, source = op.source)
		return
	}
	if require_name && payload.exception_name == "" {
		verify_add(result, .Bad_Intrinsic, "exception intrinsic must carry exception name", function_id, block_id, op.id, source = op.source)
	}
}

verify_intrinsic_string_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
) {
	payload, payload_ok := intrinsic.payload.(Intrinsic_String_Payload)
	if !payload_ok {
		verify_add(result, .Bad_Intrinsic, "string intrinsic must carry string payload", function_id, block_id, op.id, source = op.source)
		return
	}
	#partial switch intrinsic.op {
	case .ABAP_Concatenate:
		verify_op_arity(result, function_id, block_id, op, 1, -1, 1, 1)
		if payload.has_separator && len(op.operands) < 2 {
			verify_add(result, .Bad_Intrinsic, "concatenate intrinsic with separator must carry source and separator operands", function_id, block_id, op.id, source = op.source)
		}
	case .ABAP_Condense:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
	case .ABAP_Translate:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		if payload.translate_mode == .Unknown {
			verify_add(result, .Bad_Intrinsic, "translate intrinsic must carry supported translate mode", function_id, block_id, op.id, source = op.source)
		}
	case .ABAP_Split:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, -1)
	case .ABAP_Replace:
		verify_op_arity(result, function_id, block_id, op, 3, 3, 1, 1)
		if payload.replace_occurrence == .Unknown {
			verify_add(result, .Bad_Intrinsic, "replace intrinsic must carry supported occurrence mode", function_id, block_id, op.id, source = op.source)
		}
	case .ABAP_Shift:
		verify_op_arity(result, function_id, block_id, op, 1, 2, 1, 1)
		if payload.shift_direction == .Unknown {
			verify_add(result, .Bad_Intrinsic, "shift intrinsic must carry supported direction", function_id, block_id, op.id, source = op.source)
		}
	case .ABAP_Find:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 4, 4)
		if payload.find_occurrence == .Unknown {
			verify_add(result, .Bad_Intrinsic, "find intrinsic must carry supported occurrence mode", function_id, block_id, op.id, source = op.source)
		}
	case .ABAP_Search:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 2, 2)
	}
}

verify_intrinsic_message_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
) {
	verify_op_arity(result, function_id, block_id, op, 1, -1, 1, 2)
	payload, payload_ok := intrinsic.payload.(Intrinsic_Message_Payload)
	if !payload_ok {
		verify_add(result, .Bad_Intrinsic, "message intrinsic must carry message payload", function_id, block_id, op.id, source = op.source)
		return
	}
	if payload.form == .Unknown {
		verify_add(result, .Bad_Intrinsic, "message intrinsic must carry message form", function_id, block_id, op.id, source = op.source)
	}
	if payload.head_operands < 0 || payload.arg_count < 0 {
		verify_add(result, .Bad_Intrinsic, "message intrinsic operand counts must not be negative", function_id, block_id, op.id, source = op.source)
	}
	addition_operands := 0
	if payload.display_like_operand {
		addition_operands += 1
	}
	if payload.raising_operand {
		addition_operands += 1
	}
	expected_operands := 1 + payload.head_operands + payload.arg_count + addition_operands
	if len(op.operands) != expected_operands {
		verify_add(result, .Bad_Intrinsic, "message intrinsic operand count does not match payload", function_id, block_id, op.id, source = op.source)
	}
	expected_results := 2 if payload.has_into else 1
	if len(op.results) != expected_results {
		verify_add(result, .Bad_Intrinsic, "message intrinsic result count does not match INTO payload", function_id, block_id, op.id, source = op.source)
	}
	if payload.has_display_like && payload.display_like == "" && !payload.display_like_operand {
		verify_add(result, .Bad_Intrinsic, "message DISPLAY LIKE must carry static text or dynamic operand", function_id, block_id, op.id, source = op.source)
	}
	if payload.has_raising && payload.raising == "" && !payload.raising_operand {
		verify_add(result, .Bad_Intrinsic, "message RAISING must carry static text or dynamic operand", function_id, block_id, op.id, source = op.source)
	}
}

verify_intrinsic_table_payload :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
	require_result: bool = false,
) {
	payload, payload_ok := intrinsic.payload.(Intrinsic_Table_Payload)
	if !payload_ok {
		verify_add(result, .Bad_Intrinsic, "table intrinsic must carry table payload", function_id, block_id, op.id, source = op.source)
		return
	}
	if payload.access == .Unknown {
		verify_add(result, .Bad_Intrinsic, "table intrinsic must carry access mode", function_id, block_id, op.id, source = op.source)
	}
	if payload.row_type == BUILTIN_TYPE_VOID || !verify_type_valid(module, payload.row_type) {
		verify_add(result, .Bad_Intrinsic, "table intrinsic must carry valid row type", function_id, block_id, op.id, source = op.source)
	}
	if payload.key_kind == .Named && payload.key_name == "" {
		verify_add(result, .Bad_Intrinsic, "table intrinsic with named key must carry key name", function_id, block_id, op.id, source = op.source)
	}
	if payload.component_count < 0 {
		verify_add(result, .Bad_Intrinsic, "table intrinsic component count must not be negative", function_id, block_id, op.id, source = op.source)
	}
	if require_result && payload.result_kind == .None {
		verify_add(result, .Bad_Intrinsic, "table read intrinsic must carry result mode", function_id, block_id, op.id, source = op.source)
	}
	verify_table_payload_operand(module, function, function_id, block_id, op, intrinsic, payload, result)
}

verify_table_payload_operand :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	payload: Intrinsic_Table_Payload,
	result: ^Verify_Result,
) {
	table_operand_index := -1
	#partial switch intrinsic.op {
	case .Table_Iter, .Table_Read:
		table_operand_index = 1
	case .Table_Append, .Table_Insert, .Table_Modify:
		table_operand_index = 2
	case .Table_Delete, .Table_Sort:
		table_operand_index = 1
	}
	if table_operand_index < 0 || table_operand_index >= len(op.operands) {
		return
	}
	table_type, table_type_ok := verify_operand_type(function, op, table_operand_index)
	if !table_type_ok || !verify_type_valid(module, table_type) {
		return
	}
	descriptor := module.types[int(table_type)].runtime
	if descriptor.family == .Unknown {
		return
	}
	if descriptor.family != .Table {
		verify_add(result, .Bad_Intrinsic, "table intrinsic table operand must have table runtime descriptor", function_id, block_id, op.id, op.operands[table_operand_index], op.source)
		return
	}
	if verify_type_valid(module, descriptor.table.row_type) &&
	   verify_type_valid(module, payload.row_type) &&
	   !verify_types_compatible(descriptor.table.row_type, payload.row_type) {
		verify_add(result, .Bad_Intrinsic, "table intrinsic row type must match table descriptor row type", function_id, block_id, op.id, op.operands[table_operand_index], op.source)
	}
}

verify_intrinsic_sql_payload :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
	require_projection: bool,
	allow_unknown_source: bool = false,
) {
	payload, payload_ok := intrinsic.payload.(Intrinsic_SQL_Payload)
	if !payload_ok {
		verify_add(result, .Bad_Intrinsic, "SQL intrinsic must carry SQL payload", function_id, block_id, op.id, source = op.source)
		return
	}
	if payload.source_kind == .Unknown && !allow_unknown_source {
		verify_add(result, .Bad_Intrinsic, "SQL intrinsic must carry source kind", function_id, block_id, op.id, source = op.source)
	}
	if payload.source_kind == .Dynamic || payload.source_kind == .Unresolved {
		verify_add(result, .Bad_Intrinsic, "resolved SQL intrinsic must not carry dynamic or unresolved source", function_id, block_id, op.id, source = op.source)
	}
	if payload.source_kind == .Resolved && payload.source_name == "" {
		verify_add(result, .Bad_Intrinsic, "resolved SQL intrinsic must carry source name", function_id, block_id, op.id, source = op.source)
	}
	if payload.row_type == BUILTIN_TYPE_VOID || !verify_type_valid(module, payload.row_type) {
		verify_add(result, .Bad_Intrinsic, "SQL intrinsic must carry valid row type", function_id, block_id, op.id, source = op.source)
	}
	if payload.scalar_type != BUILTIN_TYPE_VOID && !verify_type_valid(module, payload.scalar_type) {
		verify_add(result, .Bad_Intrinsic, "SQL intrinsic scalar type must be valid when present", function_id, block_id, op.id, source = op.source)
	}
	if payload.projection_count < 0 || payload.source_count < 0 {
		verify_add(result, .Bad_Intrinsic, "SQL intrinsic counts must not be negative", function_id, block_id, op.id, source = op.source)
	}
	if require_projection && payload.projection_count == 0 {
		verify_add(result, .Bad_Intrinsic, "SQL query intrinsic must carry projection count", function_id, block_id, op.id, source = op.source)
	}
	if payload.assignment_count < 0 {
		verify_add(result, .Bad_Intrinsic, "SQL mutation assignment count must not be negative", function_id, block_id, op.id, source = op.source)
	}
}

verify_intrinsic_system_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
) {
	payload, payload_ok := intrinsic.payload.(Intrinsic_System_Field_Payload)
	if !payload_ok || payload.system_field == "" {
		verify_add(result, .Bad_Intrinsic, "system-field intrinsic must name a system field", function_id, block_id, op.id, source = op.source)
	}
}

verify_intrinsic_host_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
) {
	payload, payload_ok := intrinsic.payload.(Intrinsic_Host_Payload)
	if !payload_ok || payload.symbol_name == "" {
		verify_add(result, .Bad_Intrinsic, "host intrinsic must name host symbol", function_id, block_id, op.id, source = op.source)
	}
}

verify_intrinsic_unsupported_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	intrinsic: Intrinsic,
	result: ^Verify_Result,
) {
	payload, payload_ok := intrinsic.payload.(Intrinsic_Unsupported_Payload)
	if !payload_ok || payload.message == "" {
		verify_add(result, .Bad_Intrinsic, "unsupported intrinsic must carry message", function_id, block_id, op.id, source = op.source)
	}
	if !verify_source_loc_has_provenance(op.source) {
		verify_add(result, .Bad_Intrinsic, "unsupported intrinsic must carry source provenance", function_id, block_id, op.id, source = op.source)
	}
}

verify_call_attrs :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) -> (
	Call_Attrs,
	bool,
) {
	attrs, ok := op.attrs.(Call_Attrs)
	if !ok {
		verify_add(result, .Bad_Op_Signature, "call instruction must carry call attrs", function_id, block_id, op.id, source = op.source)
		return {}, false
	}
	if attrs.target == INVALID_FUNCTION_ID || int(attrs.target) >= len(module.functions) {
		verify_add(result, .Bad_Op_Signature, "call target is invalid", function_id, block_id, op.id, source = op.source)
		return attrs, false
	}
	return attrs, true
}

verify_memory_access :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	access: Memory_Access,
	result: ^Verify_Result,
) {
	if !verify_type_valid(module, access.type) {
		verify_add(result, .Bad_Memory_Alias, "memory access type is invalid", function_id, block_id, op.id, source = op.source)
	}
	requires_alias_scope := access.kind == .Read || access.kind == .Write || access.kind == .Read_Write
	if requires_alias_scope && access.alias_class == INVALID_ALIAS_CLASS_ID {
		verify_add(result, .Bad_Memory_Alias, "memory access must carry alias class", function_id, block_id, op.id, source = op.source)
	} else if access.alias_class != INVALID_ALIAS_CLASS_ID && int(access.alias_class) >= len(module.alias_classes) {
		verify_add(result, .Bad_Memory_Alias, "memory access alias class is invalid", function_id, block_id, op.id, source = op.source)
	}
	if requires_alias_scope && access.scope == INVALID_EFFECT_SCOPE_ID {
		verify_add(result, .Bad_Memory_Alias, "memory access must carry effect scope", function_id, block_id, op.id, source = op.source)
	} else if access.scope != INVALID_EFFECT_SCOPE_ID && int(access.scope) >= len(module.effect_scopes) {
		verify_add(result, .Bad_Memory_Alias, "memory access effect scope is invalid", function_id, block_id, op.id, source = op.source)
	}
	if op.opcode == .Load || op.opcode == .Store {
		if access.address_operand == INVALID_OPERAND_INDEX || int(access.address_operand) >= len(op.operands) {
			verify_add(result, .Bad_Memory_Alias, "memory access address operand is invalid", function_id, block_id, op.id, source = op.source)
		} else {
			address := op.operands[int(access.address_operand)]
			function := function_ptr(module, function_id)
			if address == INVALID_VALUE_ID || int(address) >= len(function.values) {
				verify_add(result, .Bad_Memory_Alias, "memory access address operand value is invalid", function_id, block_id, op.id, source = op.source)
			} else {
				verify_memory_address_descriptor_matches_access(module, function, function_id, block_id, op, address, access, result)
			}
		}
	}
	if op.opcode == .Store {
		if access.value_operand == INVALID_OPERAND_INDEX || int(access.value_operand) >= len(op.operands) {
			verify_add(result, .Bad_Memory_Alias, "memory access value operand is invalid", function_id, block_id, op.id, source = op.source)
		}
	} else if access.value_operand != INVALID_OPERAND_INDEX && int(access.value_operand) >= len(op.operands) {
		verify_add(result, .Bad_Memory_Alias, "memory access value operand is invalid", function_id, block_id, op.id, source = op.source)
	}
}

verify_memory_address_descriptor_matches_access :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	address: Value_Id,
	access: Memory_Access,
	result: ^Verify_Result,
) {
	address_type := value_type(function, address)
	if !verify_type_valid(module, address_type) || !verify_type_valid(module, access.type) {
		return
	}
	descriptor := module.types[int(address_type)].runtime
	if descriptor.family != .Reference ||
	   descriptor.reference.kind == .Unknown ||
	   descriptor.reference.target_type == INVALID_TYPE_ID {
		return
	}
	if verify_type_valid(module, descriptor.reference.target_type) &&
	   !verify_storage_types_compatible(module, descriptor.reference.target_type, access.type) {
		verify_add(result, .Bad_Memory_Alias, "memory access type must match address descriptor target type", function_id, block_id, op.id, address, op.source)
	}
}

verify_table_iter_signature :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if len(op.results) < 2 {
		return
	}
	if !verify_value_has_type(function, op.results[1], BUILTIN_TYPE_TABLE_ITERATOR) {
		verify_add(result, .Bad_Op_Signature, "table iterator operation must produce table iterator", function_id, block_id, op.id, op.results[1], op.source)
	}
}

verify_table_next_signature :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if len(op.operands) < 2 {
		return
	}
	if !verify_value_has_type(function, op.operands[1], BUILTIN_TYPE_TABLE_ITERATOR) {
		verify_add(result, .Bad_Op_Signature, "table next operation must consume table iterator", function_id, block_id, op.id, op.operands[1], op.source)
	}
}

verify_op_arity :: proc(
	result: ^Verify_Result,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	operand_min: int,
	operand_max: int,
	result_min: int,
	result_max: int,
) {
	operand_count := verify_call_operand_count(op)
	if operand_count < operand_min || (operand_max >= 0 && operand_count > operand_max) {
		verify_add(result, .Bad_Op_Signature, "operation operand count does not match operation kind", function_id, block_id, op.id, source = op.source)
	}
	if len(op.results) < result_min || (result_max >= 0 && len(op.results) > result_max) {
		verify_add(result, .Bad_Op_Signature, "operation result count does not match operation kind", function_id, block_id, op.id, source = op.source)
	}
}

verify_branch_target :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	from_block: Block_Id,
	target: Block_Id,
	args: []Value_Id,
	result: ^Verify_Result,
	source: Source_Loc,
) {
	if target == INVALID_BLOCK_ID || int(target) >= len(function.blocks) {
		verify_add(result, .Invalid_Block, "branch target is invalid", function_id, from_block, source = source)
		return
	}
	target_block := block_ptr(function, target)
	if len(target_block.args) != len(args) {
		verify_add(result, .Bad_Terminator_Args, "branch argument count does not match target block parameters", function_id, from_block, source = source)
		return
	}
	for arg, i in args {
		if arg == INVALID_VALUE_ID || int(arg) >= len(function.values) {
			continue
		}
		param := target_block.args[i]
		if !verify_value_id_valid(function, param) {
			verify_add(result, .Invalid_Value, "branch target block parameter references invalid value", function_id, from_block, value = param, source = source)
			continue
		}
		arg_type, arg_type_ok := verify_value_type_lookup(function, arg)
		param_type, param_type_ok := verify_value_type_lookup(function, param)
		if arg_type_ok && param_type_ok && verify_type_valid(module, arg_type) && verify_type_valid(module, param_type) && arg_type != param_type {
			verify_add(result, .Bad_Terminator_Args, "branch argument type does not match target block parameter", function_id, from_block, value = arg, source = source)
		}
	}
}

verify_value_use :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	use_block: Block_Id,
	use_op_index: u32,
	value: Value_Id,
	dom: []bool,
	result: ^Verify_Result,
	op: Op_Id = INVALID_OP_ID,
	source: Source_Loc = {},
) {
	if value == INVALID_VALUE_ID || int(value) >= len(function.values) {
		verify_add(result, .Invalid_Value, "use references invalid value", function_id, use_block, op, value, source)
		return
	}
	record := value_ptr(function, value)
	if !verify_type_valid(module, record.type) {
		verify_add(result, .Invalid_Type, "use references value with invalid type", function_id, use_block, op, value, source)
	}
	if !verify_block_id_valid(function, record.block) {
		verify_add(result, .Invalid_Value, "use references value with invalid definition block", function_id, use_block, op, value, source)
		return
	}
	if record.kind == .Op_Result && !verify_op_id_valid(function, record.op) {
		verify_add(result, .Invalid_Value, "use references value with invalid defining operation", function_id, use_block, op, value, source)
		return
	}
	if !verify_value_dominates(function, value, use_block, use_op_index, dom) {
		verify_add(result, .Dominance_Error, "value does not dominate use", function_id, use_block, op, value, source)
	}
}

verify_type_valid :: #force_inline proc "contextless" (module: ^Module, typ: Type_Id) -> bool {
	return typ != INVALID_TYPE_ID && int(typ) >= 0 && int(typ) < len(module.types)
}

verify_block_id_valid :: #force_inline proc "contextless" (function: ^Function, block: Block_Id) -> bool {
	return block != INVALID_BLOCK_ID && int(block) >= 0 && int(block) < len(function.blocks)
}

verify_value_id_valid :: #force_inline proc "contextless" (function: ^Function, value: Value_Id) -> bool {
	return value != INVALID_VALUE_ID && int(value) >= 0 && int(value) < len(function.values)
}

verify_op_id_valid :: #force_inline proc "contextless" (function: ^Function, op: Op_Id) -> bool {
	if op == INVALID_OP_ID || int(op) < 0 || int(op) >= len(function.instructions) || int(op) >= len(function.op_locations) {
		return false
	}
	loc := function.op_locations[int(op)]
	return loc.block != INVALID_BLOCK_ID &&
	       int(loc.block) >= 0 &&
	       int(loc.block) < len(function.blocks) &&
	       int(loc.index) >= 0 &&
	       (int(loc.index) < len(function.blocks[int(loc.block)].instructions) &&
	        function.blocks[int(loc.block)].instructions[int(loc.index)] == Instruction_Id(op) ||
	        int(loc.index) == len(function.blocks[int(loc.block)].instructions) &&
	        function.blocks[int(loc.block)].terminator == Instruction_Id(op))
}

verify_value_type_lookup :: #force_inline proc "contextless" (function: ^Function, value: Value_Id) -> (Type_Id, bool) {
	if !verify_value_id_valid(function, value) {
		return INVALID_TYPE_ID, false
	}
	return function.values[int(value)].type, true
}

verify_value_has_type :: #force_inline proc "contextless" (function: ^Function, value: Value_Id, typ: Type_Id) -> bool {
	value_typ, ok := verify_value_type_lookup(function, value)
	return ok && value_typ == typ
}

verify_source_loc_has_provenance :: #force_inline proc "contextless" (source: Source_Loc) -> bool {
	return source.file != nil || source.node != nil || source.range.end > source.range.start
}

verify_value_dominates :: proc(
	function: ^Function,
	value: Value_Id,
	use_block: Block_Id,
	use_op_index: u32,
	dom: []bool,
) -> bool {
	record := value_ptr(function, value)
	if !verify_block_id_valid(function, record.block) {
		return false
	}
	if record.block == use_block {
		if record.kind == .Block_Param {
			return true
		}
		if !verify_op_id_valid(function, record.op) {
			return false
		}
		loc := function.op_locations[int(record.op)]
		if !verify_op_location_points_to_op(function, record.op, loc) {
			return false
		}
		return loc.index < use_op_index
	}
	return verify_dom_get(dom, len(function.blocks), record.block, use_block)
}

verify_value_op_index :: proc(function: ^Function, value: Value_Id) -> u32 {
	record := value_ptr(function, value)
	if record.op == INVALID_OP_ID {
		return 0
	}
	return function.op_locations[int(record.op)].index
}

verify_world_chain :: proc(function: ^Function, function_id: Function_Id, block_id: Block_Id, result: ^Verify_Result) {
	block := block_ptr(function, block_id)
	current := verify_block_world_param(function, block_id)
	for instruction in block.instructions {
		if instruction == INVALID_INSTRUCTION_ID || int(instruction) >= len(function.instructions) {
			continue
		}
		op := function.instructions[int(instruction)]
		reads_world := len(op.operands) > 0 && verify_value_has_type(function, op.operands[0], BUILTIN_TYPE_WORLD)
		writes_world := len(op.results) > 0 && verify_value_has_type(function, op.results[0], BUILTIN_TYPE_WORLD)
		if reads_world {
			if current == INVALID_VALUE_ID {
				verify_add(result, .Bad_World_Chain, "world-reading operation appears in block without world parameter", function_id, block_id, op.id, source = op.source)
			} else if len(op.operands) == 0 || op.operands[0] != current {
				verify_add(result, .Bad_World_Chain, "effect operation does not consume the current world token", function_id, block_id, op.id, source = op.source)
			}
		}
		if writes_world {
			current = op.results[0]
		}
		if op.opcode == .Invoke {
			for edge in op.successors {
				verify_world_branch_arg(function, function_id, block_id, edge.target, edge.args[:], current, result, op.source)
			}
		}
	}
	if block.terminator == INVALID_INSTRUCTION_ID || int(block.terminator) >= len(function.instructions) {
		return
	}
	term := function.instructions[int(block.terminator)]
	#partial switch term.opcode {
	case .Br:
		if len(term.successors) > 0 {
			verify_world_branch_arg(function, function_id, block_id, term.successors[0].target, term.successors[0].args[:], current, result, term.source)
		}
	case .Cond_Br:
		if len(term.successors) > 0 {
			verify_world_branch_arg(function, function_id, block_id, term.successors[0].target, term.successors[0].args[:], current, result, term.source)
		}
		if len(term.successors) > 1 {
			verify_world_branch_arg(function, function_id, block_id, term.successors[1].target, term.successors[1].args[:], current, result, term.source)
		}
	case .Switch:
		for edge in term.successors {
			verify_world_branch_arg(function, function_id, block_id, edge.target, edge.args[:], current, result, term.source)
		}
	case .Return:
		if len(term.operands) > 0 && verify_value_has_type(function, term.operands[0], BUILTIN_TYPE_WORLD) {
			if current == INVALID_VALUE_ID {
				verify_add(result, .Bad_World_Chain, "return from block without current world token", function_id, block_id, source = term.source)
			} else if term.operands[0] != current {
				verify_add(result, .Bad_World_Chain, "return does not use current world token", function_id, block_id, source = term.source)
			}
		}
	}
}

verify_world_branch_arg :: proc(
	function: ^Function,
	function_id: Function_Id,
	from_block: Block_Id,
	target: Block_Id,
	args: []Value_Id,
	current: Value_Id,
	result: ^Verify_Result,
	source: Source_Loc,
) {
	if target == INVALID_BLOCK_ID || int(target) >= len(function.blocks) || current == INVALID_VALUE_ID {
		if target != INVALID_BLOCK_ID && int(target) < len(function.blocks) {
			target_block := block_ptr(function, target)
			for arg in target_block.args {
				if verify_value_has_type(function, arg, BUILTIN_TYPE_WORLD) {
					verify_add(result, .Bad_World_Chain, "branch from block without current world token targets world block", function_id, from_block, source = source)
					return
				}
			}
		}
		return
	}
	target_block := block_ptr(function, target)
	world_arg_index := -1
	for arg, i in target_block.args {
		if verify_value_has_type(function, arg, BUILTIN_TYPE_WORLD) {
			world_arg_index = i
			break
		}
	}
	if world_arg_index < 0 {
		return
	}
	if world_arg_index >= len(args) || args[world_arg_index] != current {
		verify_add(result, .Bad_World_Chain, "branch does not pass current world token to target block", function_id, from_block, source = source)
	}
}

verify_block_world_param :: proc(function: ^Function, block_id: Block_Id) -> Value_Id {
	block := block_ptr(function, block_id)
	for arg in block.args {
		if verify_value_has_type(function, arg, BUILTIN_TYPE_WORLD) {
			return arg
		}
	}
	return INVALID_VALUE_ID
}

verify_compute_dominators :: proc(function: ^Function, allocator: mem.Allocator) -> []bool {
	n := len(function.blocks)
	dom := make([]bool, n * n, allocator)
	preds := make([dynamic][dynamic]Block_Id, n, allocator)
	defer {
		for &pred in preds {
			delete(pred)
		}
		delete(preds)
	}
	for i in 0 ..< n {
		preds[i] = make([dynamic]Block_Id, 0, 2, allocator)
	}
	for block, i in function.blocks {
		from := Block_Id(i)
		for instruction in block.instructions {
			if instruction == INVALID_INSTRUCTION_ID || int(instruction) >= len(function.instructions) {
				continue
			}
			op := function.instructions[int(instruction)]
			if op.opcode != .Invoke {
				continue
			}
			for edge in op.successors {
				if edge.target != INVALID_BLOCK_ID && int(edge.target) < n {
					append(&preds[int(edge.target)], from)
				}
			}
		}
		if block.terminator != INVALID_INSTRUCTION_ID && int(block.terminator) < len(function.instructions) {
			term := function.instructions[int(block.terminator)]
			for edge in term.successors {
				if edge.target != INVALID_BLOCK_ID && int(edge.target) < n {
					append(&preds[int(edge.target)], from)
				}
			}
		}
	}

	entry := int(function.entry)
	for b in 0 ..< n {
		for d in 0 ..< n {
			verify_dom_set(dom, n, Block_Id(d), Block_Id(b), b != entry)
		}
		verify_dom_set(dom, n, Block_Id(b), Block_Id(b), true)
	}
	for d in 0 ..< n {
		verify_dom_set(dom, n, Block_Id(d), function.entry, d == entry)
	}

	changed := true
	for changed {
		changed = false
		for b in 0 ..< n {
			if b == entry {
				continue
			}
			new_values := make([]bool, n, allocator)
			for d in 0 ..< n {
				new_values[d] = len(preds[b]) > 0
			}
			for pred in preds[b] {
				for d in 0 ..< n {
					new_values[d] = new_values[d] && verify_dom_get(dom, n, Block_Id(d), pred)
				}
			}
			if len(preds[b]) == 0 {
				for d in 0 ..< n {
					new_values[d] = false
				}
			}
			new_values[b] = true
			for d in 0 ..< n {
				old := verify_dom_get(dom, n, Block_Id(d), Block_Id(b))
				if old != new_values[d] {
					verify_dom_set(dom, n, Block_Id(d), Block_Id(b), new_values[d])
					changed = true
				}
			}
			delete(new_values, allocator)
		}
	}
	return dom
}

verify_dom_get :: #force_inline proc "contextless" (dom: []bool, count: int, dominator, block: Block_Id) -> bool {
	return dom[int(block) * count + int(dominator)]
}

verify_dom_set :: #force_inline proc "contextless" (dom: []bool, count: int, dominator, block: Block_Id, value: bool) {
	dom[int(block) * count + int(dominator)] = value
}

verify_add :: proc(
	result: ^Verify_Result,
	kind: Verify_Diagnostic_Kind,
	message: string,
	function: Function_Id = INVALID_FUNCTION_ID,
	block: Block_Id = INVALID_BLOCK_ID,
	op: Op_Id = INVALID_OP_ID,
	value: Value_Id = INVALID_VALUE_ID,
	source: Source_Loc = {},
) {
	append(
		&result.diagnostics,
		Verify_Diagnostic {
			kind = kind,
			function = function,
			block = block,
			op = op,
			value = value,
			message = message,
			source = source,
		},
	)
	result.ok = false
}
