package abap_frontend_ir

import semantic "src:semantic"

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

verify_result_destroy :: proc(result: ^Verify_Result) {
	assert(result != nil)
	delete(result.diagnostics)
	result^ = {}
}

verify_module :: proc(module: ^Module, allocator: mem.Allocator = context.allocator) -> Verify_Result {
	result := Verify_Result {
		ok = true,
		diagnostics = make([dynamic]Verify_Diagnostic, 0, 8, allocator),
	}
	assert(module != nil)
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
	for _, i in module.functions {
		verify_function(module, Function_Id(i), &result)
	}
	result.ok = len(result.diagnostics) == 0
	return result
}

verify_function :: proc(module: ^Module, function_id: Function_Id, result: ^Verify_Result) {
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
	defer delete(dom)

	for block, block_index in function.blocks {
		block_id := Block_Id(block_index)
		if block.term.kind == .Invalid {
			verify_add(result, .Missing_Terminator, "block is missing terminator", function_id, block_id, source = block.source)
		}
		verify_block_ops(module, function, function_id, block_id, dom, result)
		verify_terminator(module, function, function_id, block_id, dom, result)
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

	for slot in function.slots {
		if !verify_type_valid(module, slot.type) {
			verify_add(result, .Invalid_Type, "slot has invalid type", function_id, source = slot.source)
		}
	}

	for value, value_index in function.values {
		verify_value_record_shape(module, function, function_id, Value_Id(value_index), value, result)
	}

	for loc, op_index in function.op_locations {
		verify_op_location_record(function, function_id, Op_Id(op_index), loc, result)
	}

	for block, block_index in function.blocks {
		block_id := Block_Id(block_index)
		for param in block.params {
			if !verify_value_id_valid(function, param.value) {
				verify_add(result, .Invalid_Value, "block parameter references invalid value", function_id, block_id, value = param.value, source = block.source)
				continue
			}
			record := value_ptr(function, param.value)
			if record.kind != .Block_Param || record.block != block_id {
				verify_add(result, .Invalid_Value, "block parameter value does not point back to block", function_id, block_id, value = param.value, source = block.source)
			}
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

	switch value.kind {
	case .Block_Param:
		if value.op != INVALID_OP_ID {
			verify_add(result, .Invalid_Value, "block parameter value must not reference an operation", function_id, value.block, value.op, value_id)
		}
		block := block_ptr(function, value.block)
		found := false
		for param in block.params {
			if param.value == value_id {
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
		op := &function.blocks[int(loc.block)].ops[int(loc.index)]
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
	if int(loc.index) >= len(block.ops) {
		verify_add(result, .Invalid_Function, "operation location references invalid block operation index", function_id, loc.block, op_id, source = block.source)
		return
	}
	op := &block.ops[int(loc.index)]
	if op.id != op_id {
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
	if int(loc.index) >= len(block.ops) {
		return false
	}
	return block.ops[int(loc.index)].id == op_id
}

verify_block_ops :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	dom: []bool,
	result: ^Verify_Result,
) {
	block := block_ptr(function, block_id)
	for op, op_index in block.ops {
		if int(op.id) >= len(function.op_locations) {
			verify_add(result, .Invalid_Function, "operation id has no location", function_id, block_id, op.id, source = op.source)
		} else {
			loc := function.op_locations[int(op.id)]
			if loc.block != block_id || loc.index != u32(op_index) {
				verify_add(result, .Invalid_Function, "operation location does not point back to operation", function_id, block_id, op.id, source = op.source)
			}
		}
		verify_op_record_type(module, function, function_id, block_id, op, result)
		verify_op_signature(module, function, function_id, block_id, op, result)
		for operand in op.operands {
			verify_value_use(module, function, function_id, block_id, u32(op_index), operand, dom, result, op.id, op.source)
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
		if .Reads_World in op.flags {
			if len(op.operands) == 0 || !verify_value_has_type(function, op.operands[0], BUILTIN_TYPE_WORLD) {
				verify_add(result, .Bad_World_Chain, "world-reading operation must take world as first operand", function_id, block_id, op.id, source = op.source)
			}
		}
		if .Writes_World in op.flags {
			if !(.Reads_World in op.flags) {
				verify_add(result, .Bad_World_Chain, "world-writing operation must also read the current world token", function_id, block_id, op.id, source = op.source)
			}
			if len(op.results) == 0 || !verify_value_has_type(function, op.results[0], BUILTIN_TYPE_WORLD) {
				verify_add(result, .Bad_World_Chain, "world-writing operation must produce world as first result", function_id, block_id, op.id, source = op.source)
			}
		}
	}
}

verify_op_record_type :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if !verify_type_valid(module, op.type) {
		verify_add(result, .Invalid_Type, "operation record has invalid type", function_id, block_id, op.id, source = op.source)
		return
	}
	if len(op.results) == 0 {
		if op.type != BUILTIN_TYPE_VOID {
			verify_add(result, .Bad_Op_Signature, "operation without results must have void record type", function_id, block_id, op.id, source = op.source)
		}
		return
	}
	first_type, first_type_ok := verify_value_type_lookup(function, op.results[0])
	if first_type_ok && verify_type_valid(module, first_type) && op.type != first_type {
		verify_add(result, .Bad_Op_Signature, "operation record type must match first result type", function_id, block_id, op.id, op.results[0], op.source)
	}
}

verify_terminator :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	dom: []bool,
	result: ^Verify_Result,
) {
	block := block_ptr(function, block_id)
	use_index := u32(len(block.ops))
	term := &block.term
	#partial switch term.kind {
	case .Invalid:
		return
	case .Branch:
		verify_branch_target(module, function, function_id, block_id, term.target, term.target_args[:], result, term.source)
		for arg in term.target_args {
			verify_value_use(module, function, function_id, block_id, use_index, arg, dom, result, source = term.source)
		}
	case .Cond_Branch:
		verify_value_use(module, function, function_id, block_id, use_index, term.condition, dom, result, source = term.source)
		condition_type, condition_type_ok := verify_value_type_lookup(function, term.condition)
		if condition_type_ok && verify_type_valid(module, condition_type) && condition_type != BUILTIN_TYPE_PREDICATE {
			verify_add(result, .Bad_Terminator_Args, "conditional branch condition must be predicate", function_id, block_id, value = term.condition, source = term.source)
		}
		verify_branch_target(module, function, function_id, block_id, term.true_target, term.true_args[:], result, term.source)
		verify_branch_target(module, function, function_id, block_id, term.false_target, term.false_args[:], result, term.source)
		for arg in term.true_args {
			verify_value_use(module, function, function_id, block_id, use_index, arg, dom, result, source = term.source)
		}
		for arg in term.false_args {
			verify_value_use(module, function, function_id, block_id, use_index, arg, dom, result, source = term.source)
		}
	case .Return:
		if len(function.return_types) != len(term.values) {
			verify_add(result, .Bad_Return_Args, "return value count does not match function return types", function_id, block_id, source = term.source)
		} else {
			for value, i in term.values {
				verify_value_use(module, function, function_id, block_id, use_index, value, dom, result, source = term.source)
				value_typ, value_type_ok := verify_value_type_lookup(function, value)
				return_type := function.return_types[i]
				if value_type_ok && verify_type_valid(module, value_typ) && verify_type_valid(module, return_type) && value_typ != return_type {
					verify_add(result, .Bad_Return_Args, "return value type does not match function return type", function_id, block_id, value = value, source = term.source)
				}
			}
		}
	case .Unreachable:
	}
}

verify_op_signature :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	#partial switch op.kind {
	case .Core_Const:
		verify_op_arity(result, function_id, block_id, op, 0, 0, 1, 1)
	case .Core_Load:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		verify_slot_payload(function, function_id, block_id, op, result)
	case .Core_Store:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_slot_payload(function, function_id, block_id, op, result)
	case .Core_Field_Load:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
	case .Core_Field_Store:
		verify_op_arity(result, function_id, block_id, op, 3, 3, 1, 1)
		case .Core_Cast:
			verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		case .Core_Call:
			verify_op_arity(result, function_id, block_id, op, 1, -1, 1, -1)
			verify_effectful_core_call_signature(module, function_id, block_id, op, result)
		case .Core_Unsupported:
		if !(.Unsupported in op.flags) {
			verify_add(result, .Bad_Op_Signature, "core.unsupported operation must carry unsupported flag", function_id, block_id, op.id, source = op.source)
		}
		if !(.May_Trap in op.flags) {
			verify_add(result, .Bad_Op_Signature, "core.unsupported operation must be marked may-trap", function_id, block_id, op.id, source = op.source)
		}
		if !(.Reads_World in op.flags) || !(.Writes_World in op.flags) {
			verify_add(result, .Bad_Op_Signature, "core.unsupported operation must participate in world threading", function_id, block_id, op.id, source = op.source)
		}
		if op.payload.unsupported_message == "" {
			verify_add(result, .Bad_Op_Signature, "core.unsupported operation must carry unsupported message", function_id, block_id, op.id, source = op.source)
		}
		if !verify_source_loc_has_provenance(op.source) {
			verify_add(result, .Bad_Op_Signature, "core.unsupported operation must carry source provenance", function_id, block_id, op.id, source = op.source)
		}
	case .Abap_Move:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
	case .Abap_Add,
	     .Abap_Subtract,
	     .Abap_Multiply,
	     .Abap_Divide,
	     .Abap_Equal,
	     .Abap_Not_Equal,
	     .Abap_Less,
	     .Abap_Less_Equal,
	     .Abap_Greater,
	     .Abap_Greater_Equal,
	     .Abap_And,
	     .Abap_Or,
	     .Abap_String_Concat:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
	case .Abap_Not,
	     .Abap_Is_Initial:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
	case .Abap_String_Template,
	     .Abap_Construct:
		verify_op_arity(result, function_id, block_id, op, 0, -1, 1, 1)
	case .Abap_Builtin_Call:
		verify_op_arity(result, function_id, block_id, op, 0, -1, 1, 1)
		verify_call_payload(function_id, block_id, op, result, .Builtin)
	case .Abap_Routine_Call:
		verify_effectful_call_signature(function_id, block_id, op, result)
		verify_call_payload(function_id, block_id, op, result)
	case .Abap_Method_Call:
		verify_effectful_call_signature(function_id, block_id, op, result)
		verify_call_payload(function_id, block_id, op, result, .Method)
		verify_method_call_receiver(module, function, function_id, block_id, op, result)
	case .Abap_Message:
		verify_message_signature(function_id, block_id, op, result)
	case .Abap_Write:
		verify_effectful_write_signature(function_id, block_id, op, result)
	case .Abap_Clear:
		verify_op_arity(result, function_id, block_id, op, 1, 2, 2, 2)
	case .Abap_Refresh,
	     .Abap_Free,
	     .Abap_Unassign:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 2, 2)
	case .Abap_Assign_Field:
		verify_op_arity(result, function_id, block_id, op, 2, 3, 2, 2)
	case .Table_Iter:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 2, 2)
		verify_effectful_table_signature(function_id, block_id, op, result)
		verify_table_payload(module, function_id, block_id, op, result)
		verify_table_iter_signature(function, function_id, block_id, op, result)
	case .Table_Next:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 3, 3)
		verify_effectful_table_signature(function_id, block_id, op, result)
		verify_table_payload(module, function_id, block_id, op, result)
		verify_table_next_signature(function, function_id, block_id, op, result)
	case .Table_Read:
		verify_op_arity(result, function_id, block_id, op, 2, -1, 3, 3)
		verify_effectful_table_signature(function_id, block_id, op, result)
		verify_table_payload(module, function_id, block_id, op, result)
		if op.payload.table_result_kind == .None {
			verify_add(result, .Bad_Op_Signature, "table read operation must carry result mode", function_id, block_id, op.id, source = op.source)
		}
	case .Table_Append,
	     .Table_Insert,
	     .Table_Modify:
		verify_op_arity(result, function_id, block_id, op, 2, -1, 1, 1)
		verify_effectful_table_signature(function_id, block_id, op, result)
		verify_table_payload(module, function_id, block_id, op, result)
		if op.payload.table_source_kind == .Unknown {
			verify_add(result, .Bad_Op_Signature, "table mutation operation must carry source mode", function_id, block_id, op.id, source = op.source)
		}
	case .Table_Delete:
		verify_op_arity(result, function_id, block_id, op, 2, -1, 1, 1)
		verify_effectful_table_signature(function_id, block_id, op, result)
		verify_table_payload(module, function_id, block_id, op, result)
	case .Table_Sort:
		verify_op_arity(result, function_id, block_id, op, 2, -1, 1, 1)
		verify_effectful_table_signature(function_id, block_id, op, result)
		verify_table_payload(module, function_id, block_id, op, result)
	case .Sql_Select:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 3, 3)
		verify_effectful_sql_signature(function_id, block_id, op, result)
		verify_sql_query_payload(module, function_id, block_id, op, result)
	case .Sql_Open_Cursor,
	     .Sql_Fetch:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_effectful_sql_signature(function_id, block_id, op, result)
		verify_sql_query_payload(module, function_id, block_id, op, result)
	case .Sql_Close_Cursor:
		verify_op_arity(result, function_id, block_id, op, 2, 2, 1, 1)
		verify_effectful_sql_signature(function_id, block_id, op, result)
	case .Sql_Insert,
	     .Sql_Update,
	     .Sql_Delete,
	     .Sql_Modify:
		verify_op_arity(result, function_id, block_id, op, 1, -1, 1, 1)
		verify_effectful_sql_signature(function_id, block_id, op, result)
		verify_sql_mutation_payload(module, function_id, block_id, op, result)
	case .System_Read:
		verify_op_arity(result, function_id, block_id, op, 1, 1, 1, 1)
		if op.payload.system_field == "" {
			verify_add(result, .Bad_Op_Signature, "system read operation must name a system field", function_id, block_id, op.id, source = op.source)
		}
	case .System_Write:
		verify_op_arity(result, function_id, block_id, op, 1, 2, 1, 1)
		if op.payload.system_field == "" {
			verify_add(result, .Bad_Op_Signature, "system write operation must name a system field", function_id, block_id, op.id, source = op.source)
		}
	}
}

verify_effectful_write_signature :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	verify_op_arity(result, function_id, block_id, op, 1, -1, 1, 1)
	if !(.Reads_World in op.flags) || !(.Writes_World in op.flags) {
		verify_add(result, .Bad_Op_Signature, "write operation must participate in world threading", function_id, block_id, op.id, source = op.source)
	}
}

verify_message_signature :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if !(.Reads_World in op.flags) || !(.Writes_World in op.flags) {
		verify_add(result, .Bad_Op_Signature, "message operation must participate in world threading", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.message_form == .Unknown {
		verify_add(result, .Bad_Op_Signature, "message operation must carry message form", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.message_head_operands < 0 ||
	   op.payload.message_arg_count < 0 {
		verify_add(result, .Bad_Op_Signature, "message operation operand counts must not be negative", function_id, block_id, op.id, source = op.source)
	}
	addition_operands := 0
	if op.payload.message_display_like_operand {
		addition_operands += 1
	}
	if op.payload.message_raising_operand {
		addition_operands += 1
	}
	expected_operands := 1 + op.payload.message_head_operands + op.payload.message_arg_count + addition_operands
	if len(op.operands) != expected_operands {
		verify_add(result, .Bad_Op_Signature, "message operation operand count does not match payload", function_id, block_id, op.id, source = op.source)
	}
	expected_results := 2 if op.payload.message_has_into else 1
	if len(op.results) != expected_results {
		verify_add(result, .Bad_Op_Signature, "message operation result count does not match INTO payload", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.message_has_display_like && op.payload.message_display_like == "" && !op.payload.message_display_like_operand {
		verify_add(result, .Bad_Op_Signature, "message DISPLAY LIKE must carry static text or dynamic operand", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.message_has_raising && op.payload.message_raising == "" && !op.payload.message_raising_operand {
		verify_add(result, .Bad_Op_Signature, "message RAISING must carry static text or dynamic operand", function_id, block_id, op.id, source = op.source)
	}
}

verify_effectful_sql_signature :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if !(.Reads_World in op.flags) || !(.Writes_World in op.flags) {
		verify_add(result, .Bad_Op_Signature, "SQL operation must participate in world threading", function_id, block_id, op.id, source = op.source)
	}
}

verify_sql_query_payload :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if op.kind != .Sql_Fetch && op.payload.sql_query == nil {
		verify_add(result, .Bad_Op_Signature, "SQL query operation must carry query AST", function_id, block_id, op.id, source = op.source)
	}
	if op.kind != .Sql_Fetch {
		verify_sql_source_payload(function_id, block_id, op, result)
	}
	verify_sql_type_payload(module, function_id, block_id, op, result)
	if op.payload.sql_projection_count < 0 || op.payload.sql_source_count < 0 {
		verify_add(result, .Bad_Op_Signature, "SQL query counts must not be negative", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.sql_projection_count == 0 {
		verify_add(result, .Bad_Op_Signature, "SQL query operation must carry projection count", function_id, block_id, op.id, source = op.source)
	}
}

verify_sql_mutation_payload :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	verify_sql_source_payload(function_id, block_id, op, result)
	verify_sql_type_payload(module, function_id, block_id, op, result)
	if op.payload.sql_assignment_count < 0 {
		verify_add(result, .Bad_Op_Signature, "SQL mutation assignment count must not be negative", function_id, block_id, op.id, source = op.source)
	}
}

verify_sql_source_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if op.payload.sql_source_kind == .Unknown {
		verify_add(result, .Bad_Op_Signature, "SQL operation must carry source kind", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.sql_source_kind == .Dynamic || op.payload.sql_source_kind == .Unresolved {
		verify_add(result, .Bad_Op_Signature, "resolved SQL operation must not carry dynamic or unresolved source", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.sql_source_kind == .Resolved && op.payload.sql_source_entity == nil {
		verify_add(result, .Bad_Op_Signature, "resolved SQL source must carry semantic entity", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.sql_source_kind == .Resolved && op.payload.sql_source_name == "" {
		verify_add(result, .Bad_Op_Signature, "resolved SQL source must carry source name", function_id, block_id, op.id, source = op.source)
	}
}

verify_sql_type_payload :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if op.payload.sql_row_type == BUILTIN_TYPE_VOID ||
	   !verify_type_valid(module, op.payload.sql_row_type) {
		verify_add(result, .Bad_Op_Signature, "SQL operation must carry valid row type", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.sql_scalar_type != BUILTIN_TYPE_VOID &&
	   !verify_type_valid(module, op.payload.sql_scalar_type) {
		verify_add(result, .Bad_Op_Signature, "SQL operation scalar type must be valid when present", function_id, block_id, op.id, source = op.source)
	}
}

verify_effectful_call_signature :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	verify_op_arity(result, function_id, block_id, op, 1, -1, 1, -1)
	if !(.Reads_World in op.flags) || !(.Writes_World in op.flags) {
		verify_add(result, .Bad_Op_Signature, "effectful call operation must participate in world threading", function_id, block_id, op.id, source = op.source)
	}
}

verify_effectful_core_call_signature :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if !(.Reads_World in op.flags) || !(.Writes_World in op.flags) {
		verify_add(result, .Bad_Op_Signature, "core.call operation must participate in world threading", function_id, block_id, op.id, source = op.source)
	}
	if !op.payload.has_call_function_target {
		verify_add(result, .Bad_Op_Signature, "core.call operation must carry function target", function_id, block_id, op.id, source = op.source)
		return
	}
	if op.payload.call_function_target == INVALID_FUNCTION_ID ||
	   int(op.payload.call_function_target) >= len(module.functions) {
		verify_add(result, .Bad_Op_Signature, "core.call operation has invalid function target", function_id, block_id, op.id, source = op.source)
		return
	}
}

verify_call_payload :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
	expected_kind: Abap_Call_Kind = .Unknown,
) {
	if op.payload.callee_name == "" {
		verify_add(result, .Bad_Op_Signature, "call operation must carry callee name", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.call_kind == .Unknown {
		verify_add(result, .Bad_Op_Signature, "resolved call operation must carry call kind", function_id, block_id, op.id, source = op.source)
	}
	if expected_kind != .Unknown && op.payload.call_kind != expected_kind {
		verify_add(result, .Bad_Op_Signature, "call operation kind does not match payload call kind", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.call_target == nil {
		verify_add(result, .Bad_Op_Signature, "resolved call operation must carry semantic target", function_id, block_id, op.id, source = op.source)
		return
	}
	if expected_kind == .Builtin && op.payload.call_target.kind != .Builtin {
		verify_add(result, .Bad_Op_Signature, "builtin call target must be a builtin entity", function_id, block_id, op.id, source = op.source)
	}
	if expected_kind == .Method && op.payload.call_target.kind != .Method {
		verify_add(result, .Bad_Op_Signature, "method call target must be a method entity", function_id, block_id, op.id, source = op.source)
	}
	if expected_kind == .Unknown && op.payload.call_target.kind == .Method {
		verify_add(result, .Bad_Op_Signature, "method targets must use method call operation", function_id, block_id, op.id, source = op.source)
	}
}

verify_method_call_receiver :: proc(
	module: ^Module,
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	target := op.payload.call_target
	if !verify_method_target_has_value_receiver(target) {
		return
	}
	if len(op.operands) < 2 {
		verify_add(result, .Bad_Op_Signature, "instance method call must carry receiver operand", function_id, block_id, op.id, source = op.source)
		return
	}
	receiver_type, ok := verify_value_type_lookup(function, op.operands[1])
	if !ok || !verify_type_valid(module, receiver_type) {
		return
	}
	if type_ptr(module, receiver_type).kind != .Reference {
		verify_add(result, .Bad_Op_Signature, "instance method call receiver operand must be a reference", function_id, block_id, op.id, source = op.source)
	}
}

verify_method_target_has_value_receiver :: proc "contextless" (target: ^semantic.Entity) -> bool {
	return target != nil &&
	       target.kind == .Method &&
	       target.owner != nil &&
	       (target.owner.kind == .Class || target.owner.kind == .Interface) &&
	       !(.Static in target.flags)
}

verify_effectful_table_signature :: proc(
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if !(.Reads_World in op.flags) || !(.Writes_World in op.flags) {
		verify_add(result, .Bad_Op_Signature, "table operation must participate in world threading", function_id, block_id, op.id, source = op.source)
	}
}

verify_table_payload :: proc(
	module: ^Module,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if op.payload.table_access == .Unknown {
		verify_add(result, .Bad_Op_Signature, "table operation must carry access mode", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.table_row_type == BUILTIN_TYPE_VOID ||
	   !verify_type_valid(module, op.payload.table_row_type) {
		verify_add(result, .Bad_Op_Signature, "table operation must carry valid row type", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.table_key_kind == .Named && op.payload.table_key_name == "" {
		verify_add(result, .Bad_Op_Signature, "table operation with named key must carry key name", function_id, block_id, op.id, source = op.source)
	}
	if op.payload.table_component_count < 0 {
		verify_add(result, .Bad_Op_Signature, "table operation component count must not be negative", function_id, block_id, op.id, source = op.source)
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
	if len(op.operands) < operand_min || (operand_max >= 0 && len(op.operands) > operand_max) {
		verify_add(result, .Bad_Op_Signature, "operation operand count does not match operation kind", function_id, block_id, op.id, source = op.source)
	}
	if len(op.results) < result_min || (result_max >= 0 && len(op.results) > result_max) {
		verify_add(result, .Bad_Op_Signature, "operation result count does not match operation kind", function_id, block_id, op.id, source = op.source)
	}
}

verify_slot_payload :: proc(
	function: ^Function,
	function_id: Function_Id,
	block_id: Block_Id,
	op: Op,
	result: ^Verify_Result,
) {
	if op.payload.slot == INVALID_SLOT_ID || int(op.payload.slot) >= len(function.slots) {
		verify_add(result, .Bad_Op_Signature, "slot operation must reference a valid slot", function_id, block_id, op.id, source = op.source)
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
	if len(target_block.params) != len(args) {
		verify_add(result, .Bad_Terminator_Args, "branch argument count does not match target block parameters", function_id, from_block, source = source)
		return
	}
	for arg, i in args {
		if arg == INVALID_VALUE_ID || int(arg) >= len(function.values) {
			continue
		}
		param := target_block.params[i].value
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
	return op != INVALID_OP_ID && int(op) >= 0 && int(op) < len(function.op_locations)
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
	for op in block.ops {
		if .Reads_World in op.flags {
			if current == INVALID_VALUE_ID {
				verify_add(result, .Bad_World_Chain, "world-reading operation appears in block without world parameter", function_id, block_id, op.id, source = op.source)
			} else if len(op.operands) == 0 || op.operands[0] != current {
				verify_add(result, .Bad_World_Chain, "effect operation does not consume the current world token", function_id, block_id, op.id, source = op.source)
			}
		}
		if .Writes_World in op.flags && len(op.results) > 0 {
			current = op.results[0] if verify_value_has_type(function, op.results[0], BUILTIN_TYPE_WORLD) else INVALID_VALUE_ID
		}
	}
	#partial switch block.term.kind {
	case .Branch:
		verify_world_branch_arg(function, function_id, block_id, block.term.target, block.term.target_args[:], current, result, block.term.source)
	case .Cond_Branch:
		verify_world_branch_arg(function, function_id, block_id, block.term.true_target, block.term.true_args[:], current, result, block.term.source)
		verify_world_branch_arg(function, function_id, block_id, block.term.false_target, block.term.false_args[:], current, result, block.term.source)
	case .Return:
		if len(block.term.values) > 0 && verify_value_has_type(function, block.term.values[0], BUILTIN_TYPE_WORLD) {
			if current == INVALID_VALUE_ID {
				verify_add(result, .Bad_World_Chain, "return from block without current world token", function_id, block_id, source = block.term.source)
			} else if block.term.values[0] != current {
				verify_add(result, .Bad_World_Chain, "return does not use current world token", function_id, block_id, source = block.term.source)
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
			for param in target_block.params {
				if verify_value_has_type(function, param.value, BUILTIN_TYPE_WORLD) {
					verify_add(result, .Bad_World_Chain, "branch from block without current world token targets world block", function_id, from_block, source = source)
					return
				}
			}
		}
		return
	}
	target_block := block_ptr(function, target)
	world_arg_index := -1
	for param, i in target_block.params {
		if verify_value_has_type(function, param.value, BUILTIN_TYPE_WORLD) {
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
	for param in block.params {
		if verify_value_has_type(function, param.value, BUILTIN_TYPE_WORLD) {
			return param.value
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
		#partial switch block.term.kind {
		case .Branch:
			if block.term.target != INVALID_BLOCK_ID && int(block.term.target) < n {
				append(&preds[int(block.term.target)], from)
			}
		case .Cond_Branch:
			if block.term.true_target != INVALID_BLOCK_ID && int(block.term.true_target) < n {
				append(&preds[int(block.term.true_target)], from)
			}
			if block.term.false_target != INVALID_BLOCK_ID && int(block.term.false_target) < n {
				append(&preds[int(block.term.false_target)], from)
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
			delete(new_values)
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
