package abap_frontend_vm

import ir "src:ir"
import runtime "src:vm/runtime"

import "core:mem"
import "core:strings"

Intrinsic_Dispatch_Result :: enum {
	Ok,
	Trap,
	Unsupported,
}

Intrinsic_Dispatch_Context :: struct {
	intrinsic:    ir.Intrinsic_Op,
	name:         string,
	source:       runtime.Source_Loc,
	operands:     []runtime.Value,
	result_types: []runtime.Type_Descriptor,
	allocator:    mem.Allocator,
	data:         rawptr,
	results:      []runtime.Value,
	result_set:   []bool,
	trap:         runtime.Trap,
}

Intrinsic_Dispatch_Proc :: #type proc(ctx: ^Intrinsic_Dispatch_Context) -> Intrinsic_Dispatch_Result

Intrinsic_Dispatcher :: struct {
	dispatch: Intrinsic_Dispatch_Proc,
	data:     rawptr,
}

exec_intrinsic :: proc(vm: ^VM, frame: ^Frame, instruction: Prepared_Instruction) {
	frame_index := len(vm.frames) - 1
	frame_count := len(vm.frames)
	if vm.options.dispatcher.dispatch != nil {
		switch dispatch_external_intrinsic(vm, frame, instruction) {
		case .Ok:
				if vm.state != .Trapped && len(vm.frames) == frame_count {
					vm.frames[frame_index].ip += 1
				}
				return
		case .Trap:
				return
		case .Unsupported:
		}
	}
	dispatch_intrinsic(vm, frame, instruction)
	if vm.state != .Trapped && len(vm.frames) == frame_count {
		vm.frames[frame_index].ip += 1
	}
}

dispatch_external_intrinsic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) -> Intrinsic_Dispatch_Result {
	operand_count := int(instruction.operand_count)
	result_count := int(instruction.result_count)
	operands := make([]runtime.Value, operand_count, context.temp_allocator)
	result_types := make([]runtime.Type_Descriptor, result_count, context.temp_allocator)
	results := make([]runtime.Value, result_count, context.temp_allocator)
	result_set := make([]bool, result_count, context.temp_allocator)
	for i in 0 ..< operand_count {
		operands[i] = get_operand(frame, instruction, i)
	}
	for i in 0 ..< result_count {
		result_types[i] = result_type_descriptor(frame.function, instruction, i)
	}
	ctx := Intrinsic_Dispatch_Context {
		intrinsic = instruction.intrinsic_op,
		name = instruction.intrinsic_name,
		source = instruction.source,
		operands = operands,
		result_types = result_types,
		allocator = vm.allocator,
		data = vm.options.dispatcher.data,
		results = results,
		result_set = result_set,
	}
	defer {
		for &value in ctx.results {
			runtime.value_destroy(&value)
		}
		delete(ctx.trap.message)
		runtime.source_loc_destroy(&ctx.trap.source)
	}
	dispatch_result := vm.options.dispatcher.dispatch(&ctx)
	switch dispatch_result {
	case .Ok:
		for set, i in ctx.result_set {
			if !set {
				vm_trap(vm, .Invalid_Instruction, "intrinsic dispatcher did not set every result", instruction.source)
				return .Trap
			}
			set_result(vm, frame, instruction, i, ctx.results[i])
		}
		return .Ok
	case .Trap:
		if ctx.trap.kind == .None {
			vm_trap(vm, .Unsupported, "intrinsic dispatcher trapped without a diagnostic", instruction.source)
		} else {
			vm_trap(vm, ctx.trap.kind, ctx.trap.message, ctx.trap.source)
		}
		return .Trap
	case .Unsupported:
		return .Unsupported
	}
	return .Unsupported
}

dispatch_intrinsic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	#partial switch instruction.intrinsic_op {
	case .Call_Builtin, .Call_Routine, .Call_Method:
		dispatch_call_intrinsic(vm, frame, instruction)
	case .System_Read, .System_Write:
		dispatch_system_intrinsic(vm, frame, instruction)
	case .Table_Iter,
	     .Table_Next,
	     .Table_Read,
	     .Table_Append,
	     .Table_Insert,
	     .Table_Modify,
	     .Table_Delete,
	     .Table_Sort:
		dispatch_table_intrinsic(vm, frame, instruction)
	case .SQL_Select,
	     .SQL_Open_Cursor,
	     .SQL_Fetch,
	     .SQL_Close_Cursor,
	     .SQL_Insert,
	     .SQL_Update,
	     .SQL_Delete,
	     .SQL_Modify:
		dispatch_sql_intrinsic(vm, frame, instruction)
	case .Unsupported:
		payload := instruction.intrinsic_payload.(ir.Intrinsic_Unsupported_Payload)
		vm_trap(vm, .Unsupported, payload.message, instruction.source)
	case:
		dispatch_abap_intrinsic(vm, frame, instruction)
	}
}

dispatch_system_intrinsic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_System_Field_Payload)
	if !payload_ok || payload.system_field == "" {
		vm_trap(vm, .Invalid_Instruction, "system intrinsic field name is missing", instruction.source)
		return
	}
	#partial switch instruction.intrinsic_op {
	case .System_Read:
		value := runtime.context_system_read(&vm.runtime_context, payload.system_field)
		defer runtime.value_destroy(&value)
		set_result(vm, frame, instruction, 0, value)
	case .System_Write:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		runtime.context_system_write(&vm.runtime_context, payload.system_field, get_operand(frame, instruction, 1))
	case:
		vm_trap(vm, .Invalid_Instruction, "system intrinsic operation is not implemented", instruction.source)
	}
}

dispatch_call_intrinsic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_Call_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "call intrinsic payload is missing", instruction.source)
		return
	}
	if instruction.intrinsic_op != .Call_Builtin && payload.has_call_function_target {
		target := payload.call_function_target
		args := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
		for i in 0 ..< int(instruction.operand_count) {
			args[i] = get_operand(frame, instruction, i)
		}
		return_registers := call_return_registers(frame, instruction, vm.allocator)
		frame.ip += 1
		vm_push_function(vm, target, args[:], return_registers, instruction.source)
		return
	}
	skip_world := 1 if instruction.intrinsic_op != .Call_Builtin else 0
	values := intrinsic_values(frame, instruction, skip_world)
	result_index := 0
	if skip_world > 0 {
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		result_index = 1
	}
	if result_index >= int(instruction.result_count) && instruction.intrinsic_op != .Call_Builtin {
		_, ok := runtime.context_call(
			&vm.runtime_context,
			runtime.Call_Request {
				callee_name = payload.callee_name,
				values = values,
			},
			instruction_runtime_source(instruction),
		)
		if !ok {
			vm_sync_runtime_trap(vm)
		}
		return
	}
	value, ok := runtime.context_call(
		&vm.runtime_context,
		runtime.Call_Request {
			callee_name = payload.callee_name,
			values = values,
			result_type = result_type_descriptor(frame.function, instruction, result_index),
		},
		instruction_runtime_source(instruction),
	)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, result_index, value)
}

call_return_registers :: proc(
	frame: ^Frame,
	instruction: Prepared_Instruction,
	allocator: mem.Allocator,
) -> []Register {
	return_registers := make([]Register, instruction.result_count, allocator)
	for i in 0 ..< int(instruction.result_count) {
		return_registers[i] = frame.function.result_registers[int(instruction.result_start) + i]
	}
	return return_registers
}

intrinsic_dispatch_set_result :: proc(ctx: ^Intrinsic_Dispatch_Context, index: int, value: runtime.Value) -> bool {
	if ctx == nil || index < 0 || index >= len(ctx.results) {
		return false
	}
	runtime.value_destroy(&ctx.results[index])
	ctx.results[index] = runtime.value_clone(value, ctx.allocator)
	ctx.result_set[index] = true
	return true
}

intrinsic_dispatch_trap :: proc(
	ctx: ^Intrinsic_Dispatch_Context,
	kind: runtime.Trap_Kind,
	message: string,
) {
	if ctx == nil {
		return
	}
	delete(ctx.trap.message)
	runtime.source_loc_destroy(&ctx.trap.source)
	ctx.trap = runtime.Trap {
		kind = kind,
		message = strings.clone(message, ctx.allocator),
		source = runtime.source_loc_clone(ctx.source, ctx.allocator),
	}
}
