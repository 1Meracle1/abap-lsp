package abap_frontend_vm

import ir "src:ir"
import runtime "src:vm/runtime"

dispatch_abap_intrinsic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	#partial switch instruction.intrinsic_op {
	case .ABAP_Move:
		value, ok := runtime.value_cast(get_operand(frame, instruction, 0), result_type_descriptor(frame.function, instruction, 0), vm.allocator)
		defer runtime.value_destroy(&value)
		if !ok {
			vm_trap(vm, .Type, "ABAP move conversion failed", instruction.source)
			return
		}
		set_result(vm, frame, instruction, 0, value)
	case .ABAP_Add, .ABAP_Subtract, .ABAP_Multiply, .ABAP_Divide, .ABAP_Integer_Divide, .ABAP_Modulo:
		dispatch_numeric_arithmetic(vm, frame, instruction, instruction.intrinsic_op)
	case .ABAP_Equal,
	     .ABAP_Not_Equal,
	     .ABAP_Less,
	     .ABAP_Less_Equal,
	     .ABAP_Greater,
	     .ABAP_Greater_Equal:
		dispatch_comparison(vm, frame, instruction, instruction.intrinsic_op)
	case .ABAP_And, .ABAP_Or:
		left := runtime.value_truthy(get_operand(frame, instruction, 0))
		right := runtime.value_truthy(get_operand(frame, instruction, 1))
		set_result(
			vm,
			frame,
			instruction,
			0,
			runtime.value_predicate(left && right if instruction.intrinsic_op == .ABAP_And else left || right),
		)
	case .ABAP_Not:
		set_result(vm, frame, instruction, 0, runtime.value_predicate(!runtime.value_truthy(get_operand(frame, instruction, 0))))
	case .ABAP_Is_Initial:
		set_result(vm, frame, instruction, 0, runtime.value_predicate(runtime.value_is_initial(get_operand(frame, instruction, 0))))
	case .ABAP_String_Concat, .ABAP_String_Template:
		values := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
		for i in 0 ..< int(instruction.operand_count) {
			values[i] = get_operand(frame, instruction, i)
		}
		value, ok := runtime.abap_string_join(
			&vm.runtime_context,
			values[:],
			vm.allocator,
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&value)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_result(vm, frame, instruction, 0, value)
	case .ABAP_Concatenate:
		dispatch_concatenate(vm, frame, instruction)
	case .ABAP_Condense:
		payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_String_Payload)
		if !payload_ok {
			vm_trap(vm, .Invalid_Instruction, "condense intrinsic payload is missing", instruction.source)
			return
		}
		value, ok := runtime.abap_condense(
			&vm.runtime_context,
			get_operand(frame, instruction, 0),
			payload.no_gaps,
			vm.allocator,
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&value)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_result(vm, frame, instruction, 0, value)
	case .ABAP_Translate:
		dispatch_translate(vm, frame, instruction)
	case .ABAP_Split:
		dispatch_split(vm, frame, instruction)
	case .ABAP_Replace:
		dispatch_replace(vm, frame, instruction)
	case .ABAP_Shift:
		dispatch_shift(vm, frame, instruction)
	case .ABAP_Find:
		dispatch_find(vm, frame, instruction)
	case .ABAP_Search:
		dispatch_search(vm, frame, instruction)
	case .ABAP_Construct:
		dispatch_construct(vm, frame, instruction)
	case .ABAP_Exception_Raise:
		payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_Exception_Payload)
		if !payload_ok {
			vm_trap(vm, .Invalid_Instruction, "exception raise intrinsic payload is missing", instruction.source)
			return
		}
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		if !runtime.context_exception_raise(
			&vm.runtime_context,
			payload.exception_name,
			instruction_runtime_source(instruction),
		) {
			vm_sync_runtime_trap(vm)
		}
	case .ABAP_Exception_Match:
		payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_Exception_Payload)
		if !payload_ok {
			vm_trap(vm, .Invalid_Instruction, "exception match intrinsic payload is missing", instruction.source)
			return
		}
		matches := runtime.context_exception_matches(&vm.runtime_context, payload.exception_name)
		set_result(vm, frame, instruction, 0, runtime.value_predicate(matches))
	case .ABAP_Exception_Catch:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		if instruction.result_count > 1 {
			value, ok := runtime.context_exception_catch(
				&vm.runtime_context,
				result_type_descriptor(frame.function, instruction, 1),
				vm.allocator,
			)
			defer runtime.value_destroy(&value)
			if !ok {
				vm_sync_runtime_trap(vm)
				return
			}
			set_result(vm, frame, instruction, 1, value)
		} else {
			value, ok := runtime.context_exception_catch(
				&vm.runtime_context,
				nil,
				vm.allocator,
			)
			defer runtime.value_destroy(&value)
			if !ok {
				vm_sync_runtime_trap(vm)
				return
			}
		}
	case .ABAP_Exception_Unhandled:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		if !runtime.context_exception_unhandled(
			&vm.runtime_context,
			instruction_runtime_source(instruction),
		) {
			vm_sync_runtime_trap(vm)
		}
	case .ABAP_Write:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		value_count := int(instruction.operand_count) - 1
		if value_count < 0 {
			value_count = 0
		}
		values := make([]runtime.Value, value_count, context.temp_allocator)
		for i := 0; i < value_count; i += 1 {
			values[i] = get_operand(frame, instruction, i + 1)
		}
		if !runtime.context_write(
			&vm.runtime_context,
			values[:],
			instruction_runtime_source(instruction),
		) {
			vm_sync_runtime_trap(vm)
		}
	case .ABAP_Message:
		dispatch_message(vm, frame, instruction)
	case .ABAP_Clear, .ABAP_Refresh, .ABAP_Free:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		if instruction.result_count > 1 {
			value := runtime.initial_for_type(result_type_descriptor(frame.function, instruction, 1), vm.allocator)
			defer runtime.value_destroy(&value)
			set_result(vm, frame, instruction, 1, value)
		}
	case .ABAP_Unassign:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		if instruction.result_count > 1 {
			value := runtime.value_reference_unassigned(.Binding, vm.allocator)
			defer runtime.value_destroy(&value)
			set_result(vm, frame, instruction, 1, value)
		}
	case .ABAP_Assign_Field:
		values := intrinsic_values(frame, instruction, 1)
		value, ok := runtime.context_assign_field(
			&vm.runtime_context,
			runtime.Assign_Request{values = values},
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&value)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		set_result(vm, frame, instruction, 1, value)
	case:
		vm_trap(vm, .Unsupported, "ABAP intrinsic is not implemented", instruction.source)
	}
}

dispatch_numeric_arithmetic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
	kind: ir.Intrinsic_Op,
) {
	arithmetic: runtime.Arithmetic_Kind
	#partial switch kind {
	case .ABAP_Add:
		arithmetic = .Add
	case .ABAP_Subtract:
		arithmetic = .Subtract
	case .ABAP_Multiply:
		arithmetic = .Multiply
	case .ABAP_Divide:
		arithmetic = .Divide
	case .ABAP_Integer_Divide:
		arithmetic = .Integer_Divide
	case .ABAP_Modulo:
		arithmetic = .Modulo
	case:
		vm_trap(vm, .Unsupported, "ABAP numeric arithmetic intrinsic is not implemented", instruction.source)
		return
	}
	result, result_ok := runtime.abap_numeric_arithmetic(
		&vm.runtime_context,
		arithmetic,
		get_operand(frame, instruction, 0),
		get_operand(frame, instruction, 1),
		result_type_descriptor(frame.function, instruction, 0),
		instruction_runtime_source(instruction),
	)
	if !result_ok {
		vm_sync_runtime_trap(vm)
		return
	}
	defer runtime.value_destroy(&result)
	set_result(vm, frame, instruction, 0, result)
}

dispatch_comparison :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
	kind: ir.Intrinsic_Op,
) {
	comparison: runtime.Comparison_Kind
	#partial switch kind {
	case .ABAP_Equal:
		comparison = .Equal
	case .ABAP_Not_Equal:
		comparison = .Not_Equal
	case .ABAP_Less:
		comparison = .Less
	case .ABAP_Less_Equal:
		comparison = .Less_Equal
	case .ABAP_Greater:
		comparison = .Greater
	case .ABAP_Greater_Equal:
		comparison = .Greater_Equal
	case:
		vm_trap(vm, .Unsupported, "ABAP comparison intrinsic is not implemented", instruction.source)
		return
	}
	result, result_ok := runtime.abap_compare(
		&vm.runtime_context,
		comparison,
		get_operand(frame, instruction, 0),
		get_operand(frame, instruction, 1),
		instruction_runtime_source(instruction),
	)
	if !result_ok {
		vm_sync_runtime_trap(vm)
		return
	}
	defer runtime.value_destroy(&result)
	set_result(vm, frame, instruction, 0, result)
}

dispatch_concatenate :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_String_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "concatenate intrinsic payload is missing", instruction.source)
		return
	}
	source_count := int(instruction.operand_count)
	if payload.has_separator {
		source_count -= 1
	}
	assert(source_count > 0)
	values := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
	source_types := make([]runtime.Type_Descriptor, source_count, context.temp_allocator)
	for i in 0 ..< int(instruction.operand_count) {
		values[i] = get_operand(frame, instruction, i)
		if i >= source_count {
			continue
		}
		register := frame.function.operand_registers[int(instruction.operand_start) + i]
		assert(register != INVALID_REGISTER && int(register) < len(frame.function.values))
		source_types[i] = frame.function.values[int(register)].type
	}
	value, ok := runtime.abap_concatenate(
		&vm.runtime_context,
		values[:],
		source_types[:],
		payload.has_separator,
		payload.respecting_blanks,
		vm.allocator,
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, value)
}

dispatch_translate :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_String_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "translate intrinsic payload is missing", instruction.source)
		return
	}
	kind: runtime.Translate_Kind
	switch payload.translate_mode {
	case .To_Upper:
		kind = .To_Upper
	case .To_Lower:
		kind = .To_Lower
	case .Unknown:
		vm_trap(vm, .Invalid_Instruction, "translate intrinsic mode is missing", instruction.source)
		return
	}
	value, ok := runtime.abap_translate(
		&vm.runtime_context,
		kind,
		get_operand(frame, instruction, 0),
		vm.allocator,
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, value)
}

dispatch_split :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	values, ok := runtime.abap_split(
		&vm.runtime_context,
		get_operand(frame, instruction, 0),
		get_operand(frame, instruction, 1),
		int(instruction.result_count),
		vm.allocator,
		instruction_runtime_source(instruction),
	)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	for &value, i in values {
		set_result(vm, frame, instruction, i, value)
		runtime.value_destroy(&value)
	}
}

dispatch_replace :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_String_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "replace intrinsic payload is missing", instruction.source)
		return
	}
	occurrence: runtime.Replace_Occurrence
	switch payload.replace_occurrence {
	case .First:
		occurrence = .First
	case .All:
		occurrence = .All
	case .Unknown:
		vm_trap(vm, .Invalid_Instruction, "replace intrinsic occurrence is missing", instruction.source)
		return
	}
	value, ok := runtime.abap_replace(
		&vm.runtime_context,
		occurrence,
		get_operand(frame, instruction, 0),
		get_operand(frame, instruction, 1),
		get_operand(frame, instruction, 2),
		vm.allocator,
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, value)
}

dispatch_shift :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_String_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "shift intrinsic payload is missing", instruction.source)
		return
	}
	direction: runtime.Shift_Direction
	switch payload.shift_direction {
	case .Left:
		direction = .Left
	case .Right:
		direction = .Right
	case .Unknown:
		vm_trap(vm, .Invalid_Instruction, "shift intrinsic direction is missing", instruction.source)
		return
	}
	places := runtime.value_integer_make(1)
	if instruction.operand_count > 1 {
		places = get_operand(frame, instruction, 1)
	}
	value, ok := runtime.abap_shift(
		&vm.runtime_context,
		direction,
		get_operand(frame, instruction, 0),
		places,
		vm.allocator,
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, value)
}

dispatch_find :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_String_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "find intrinsic payload is missing", instruction.source)
		return
	}
	occurrence: runtime.Find_Occurrence
	switch payload.find_occurrence {
	case .First:
		occurrence = .First
	case .All:
		occurrence = .All
	case .Unknown:
		vm_trap(vm, .Invalid_Instruction, "find intrinsic occurrence is missing", instruction.source)
		return
	}
	result, ok := runtime.abap_find(
		&vm.runtime_context,
		occurrence,
		payload.find_ignoring_case,
		get_operand(frame, instruction, 0),
		get_operand(frame, instruction, 1),
		instruction_runtime_source(instruction),
	)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, runtime.value_integer_make(result.subrc))
	set_result(vm, frame, instruction, 1, runtime.value_integer_make(result.offset))
	set_result(vm, frame, instruction, 2, runtime.value_integer_make(result.length))
	set_result(vm, frame, instruction, 3, runtime.value_integer_make(result.count))
}

dispatch_search :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	result, ok := runtime.abap_search(
		&vm.runtime_context,
		get_operand(frame, instruction, 0),
		get_operand(frame, instruction, 1),
		instruction_runtime_source(instruction),
	)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, runtime.value_integer_make(result.subrc))
	set_result(vm, frame, instruction, 1, runtime.value_integer_make(result.fdpos))
}

dispatch_construct :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_Call_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "construct intrinsic payload is missing", instruction.source)
		return
	}
	values := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
	for i in 0 ..< int(instruction.operand_count) {
		values[i] = get_operand(frame, instruction, i)
	}
	value, ok := runtime.abap_construct(
		&vm.runtime_context,
		payload.callee_name,
		values[:],
		result_type_descriptor(frame.function, instruction, 0),
		result_reference_target_descriptor(vm.module, frame.function, instruction, 0),
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, value)
}

dispatch_message :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_Message_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "message intrinsic payload is missing", instruction.source)
		return
	}
	first_arg := 1 + payload.head_operands
	values := make([]runtime.Value, payload.arg_count, context.temp_allocator)
	for i := 0; i < payload.arg_count; i += 1 {
		operand_index := first_arg + i
		if operand_index >= int(instruction.operand_count) {
			values = values[:i]
			break
		}
		values[i] = get_operand(frame, instruction, operand_index)
	}
	text, ok := runtime.context_message(
		&vm.runtime_context,
		runtime.Message_Descriptor {
			message_id = payload.id,
			message_type = payload.msg_type,
			message_number = payload.number,
		},
		values[:],
		instruction_runtime_source(instruction),
	)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	if payload.has_into && instruction.result_count > 1 {
		value := runtime.value_string(text, vm.allocator)
		defer runtime.value_destroy(&value)
		set_result(vm, frame, instruction, 1, value)
	}
}
