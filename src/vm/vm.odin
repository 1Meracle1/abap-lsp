package abap_frontend_vm

import bytecode "src:ir/bytecode"
import ir "src:ir"
import runtime "src:runtime"

import "core:mem"

DEFAULT_STEP_LIMIT :: u64(1_000_000)

VM_State :: enum {
	Ready,
	Running,
	Paused,
	Completed,
	Trapped,
}

Run_Result :: struct {
	status:            runtime.Run_Status,
	trap:              runtime.Trap,
	events:            [dynamic]runtime.IO_Event,
	final_values:      [dynamic]runtime.Named_Value,
	instruction_count: u64,
}

Run_Options :: struct {
	step_limit: u64,
	io_policy:  runtime.IO_Policy,
	dispatcher: Callback_Dispatcher,
}

Callback_Result :: enum {
	Ok,
	Trap,
	Unsupported,
}

Callback_Context :: struct {
	module:      ^bytecode.Module,
	function:    ^bytecode.Function,
	instruction: bytecode.Instruction,
	callback:    ^bytecode.Runtime_Callback,
	data:        rawptr,
}

Callback_Proc :: #type proc(ctx: ^Callback_Context) -> Callback_Result

Callback_Dispatcher :: struct {
	dispatch: Callback_Proc,
	data:     rawptr,
}

Frame :: struct {
	function_id:      bytecode.Function_Id,
	function:         ^bytecode.Function,
	ip:               u32,
	allocator:        mem.Allocator,
	registers:        []runtime.Value,
	slots:            []runtime.Value,
	return_registers: []bytecode.Register,
}

VM :: struct {
	module:            ^bytecode.Module,
	options:           Run_Options,
	state:             VM_State,
	allocator:         mem.Allocator,
	frames:            [dynamic]Frame,
	runtime_context:   runtime.Context,
	instruction_count: u64,
	callback_context:  Callback_Context,
	callback_trap:     runtime.Trap,
	have_callback_trap: bool,
}

run_result_destroy :: proc(result: ^Run_Result) {
	for &event in result.events {
		runtime.io_event_destroy(&event)
	}
	delete(result.events)
	for &value in result.final_values {
		runtime.named_value_destroy(&value)
	}
	delete(result.final_values)
	delete(result.trap.message)
	result^ = {}
}

execute_module :: proc(
	module: ^bytecode.Module,
	options: Run_Options = {},
	allocator: mem.Allocator = context.allocator,
) -> Run_Result {
	vm := vm_make(module, options, allocator)
	defer vm_destroy(&vm)

	vm_run_until_complete(&vm)
	return run_result_from_vm(&vm, allocator)
}

vm_make :: proc(module: ^bytecode.Module, options: Run_Options, allocator: mem.Allocator) -> VM {
	opts := options
	if opts.step_limit == 0 {
		opts.step_limit = DEFAULT_STEP_LIMIT
	}
	if !opts.io_policy.capture_write && !opts.io_policy.capture_message {
		opts.io_policy = runtime.io_policy_captured()
	}
	return VM {
		module = module,
		options = opts,
		state = .Ready,
		allocator = allocator,
		frames = make([dynamic]Frame, 0, 8, allocator),
		runtime_context = runtime.context_make(
			runtime.Context_Options{io_policy = opts.io_policy},
			allocator,
		),
	}
}

vm_destroy :: proc(vm: ^VM) {
	for &frame in vm.frames {
		frame_destroy(&frame)
	}
	delete(vm.frames)
	runtime.context_destroy(&vm.runtime_context)
	if vm.have_callback_trap {
		delete(vm.callback_trap.message)
	}
	vm^ = {}
}

vm_start :: proc(vm: ^VM) -> VM_State {
	if vm.state != .Ready {
		return vm.state
	}
	if vm.module == nil {
		vm_trap(vm, .Invalid_Module, "VM module is nil")
		return vm.state
	}
	if len(vm.module.entries) != 1 {
		vm_trap(vm, .Invalid_Module, "VM requires exactly one executable entry")
		return vm.state
	}
	entry := vm.module.entries[0]
	if !vm_push_function(vm, entry, nil, nil, {}) {
		return vm.state
	}
	vm.state = .Running
	return vm.state
}

vm_run_until_complete :: proc(vm: ^VM) -> VM_State {
	if vm.state == .Ready {
		vm_start(vm)
	}
	for vm.state == .Running {
		vm_step(vm)
	}
	return vm.state
}

vm_is_finished :: #force_inline proc "contextless" (vm: ^VM) -> bool {
	return vm.state == .Completed || vm.state == .Trapped
}

run_result_from_vm :: proc(vm: ^VM, allocator: mem.Allocator = context.allocator) -> Run_Result {
	assert(vm_is_finished(vm))

	status := runtime.Run_Status.Completed
	if vm.state == .Trapped {
		status = .Trapped
	}
	result := Run_Result {
		status = status,
		trap = runtime.trap_clone(vm.runtime_context.trap, allocator),
		events = make([dynamic]runtime.IO_Event, 0, len(vm.runtime_context.events), allocator),
		final_values = make(
			[dynamic]runtime.Named_Value,
			0,
			len(vm.runtime_context.global_values) + len(vm.runtime_context.system_values),
			allocator,
		),
		instruction_count = vm.instruction_count,
	}
	for event in vm.runtime_context.events {
		append(&result.events, runtime.io_event_clone(event, allocator))
	}
	runtime.context_collect_final_values(&vm.runtime_context, &result.final_values, allocator)
	return result
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
	delete(frame.return_registers)
	frame^ = {}
}

vm_step :: proc(vm: ^VM) -> VM_State {
	if vm.state == .Ready {
		vm_start(vm)
	}
	if vm.state != .Running {
		return vm.state
	}
	if vm.options.step_limit > 0 && vm.instruction_count >= vm.options.step_limit {
		vm_trap(vm, .Step_Limit, "runtime step limit exceeded", current_source(vm))
		return vm.state
	}
	if len(vm.frames) == 0 {
		vm.state = .Completed
		return vm.state
	}
	frame := &vm.frames[len(vm.frames) - 1]
	if int(frame.ip) >= len(frame.function.instructions) {
		vm_trap(vm, .Invalid_Instruction, "instruction pointer is outside function", current_source(vm))
		return vm.state
	}
	instruction := frame.function.instructions[int(frame.ip)]
	vm.instruction_count += 1

	switch instruction.op {
	case .Nop:
		frame.ip += 1
	case .Const:
		exec_const(vm, frame, instruction)
	case .Move:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		frame.ip += 1
	case .Load:
		exec_load(vm, frame, instruction)
	case .Store:
		exec_store(vm, frame, instruction)
	case .Field_Load, .Field_Store:
		exec_field_access(vm, frame, instruction)
	case .Cast:
		exec_cast(vm, frame, instruction)
	case .Call:
		exec_call(vm, frame, instruction)
	case .Call_Runtime:
		exec_runtime_callback(vm, frame, instruction)
	case .Branch:
		exec_branch(vm, frame, instruction, 0)
	case .Cond_Branch:
		condition := get_operand(frame, instruction, 0)
		edge_index := 0 if runtime.value_truthy(condition) else 1
		exec_branch(vm, frame, instruction, edge_index)
	case .Return:
		exec_return(vm, frame, instruction)
	case .Unreachable:
		vm_trap(vm, .Invalid_Instruction, "reached unreachable bytecode", instruction.source)
	case .Unsupported:
		vm_trap(vm, .Unsupported, "unsupported bytecode instruction", instruction.source)
	}
	if vm.state != .Trapped && len(vm.frames) == 0 {
		vm.state = .Completed
	}
	return vm.state
}

exec_const :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction) {
	if int(instruction.payload) >= len(frame.function.constants) {
		vm_trap(vm, .Invalid_Instruction, "constant index is invalid", instruction.source)
		return
	}
	literal := frame.function.constants[int(instruction.payload)]
	typ := result_type_name(frame.function, instruction, 0)
	value := runtime.value_from_literal(literal, typ, vm.allocator)
	defer runtime.value_destroy(&value)
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

exec_load :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction) {
	slot, ok := slot_debug(frame.function, instruction.payload)
	if !ok {
		vm_trap(vm, .Invalid_Instruction, "slot index is invalid", instruction.source)
		return
	}
	value := runtime.Value{}
	#partial switch slot.kind {
	case .Global:
		value = runtime.context_global_read(&vm.runtime_context, slot.name)
	case .Runtime:
		value = runtime.context_runtime_read(&vm.runtime_context, slot.name)
	case:
		value = frame.slots[int(instruction.payload)]
	}
	if runtime.value_kind(value) == .Initial {
		initial := runtime.initial_for_type(slot.type_name, vm.allocator)
		if runtime.value_kind(initial) != .Initial {
			defer runtime.value_destroy(&initial)
			value = initial
			#partial switch slot.kind {
			case .Global:
				runtime.context_global_write(&vm.runtime_context, slot.name, initial)
			case .Runtime:
			case:
				slot_index := int(instruction.payload)
				new_value := runtime.value_clone(initial, frame.allocator)
				runtime.value_destroy(&frame.slots[slot_index])
				frame.slots[slot_index] = new_value
			}
		}
	}
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

exec_store :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction) {
	slot, ok := slot_debug(frame.function, instruction.payload)
	if !ok {
		vm_trap(vm, .Invalid_Instruction, "slot index is invalid", instruction.source)
		return
	}
	value := get_operand(frame, instruction, 1)
	#partial switch slot.kind {
	case .Global:
		runtime.context_global_write(&vm.runtime_context, slot.name, value)
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
		slot_index := int(instruction.payload)
		new_value := runtime.value_clone(value, frame.allocator)
		runtime.value_destroy(&frame.slots[slot_index])
		frame.slots[slot_index] = new_value
	}
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	frame.ip += 1
}

exec_field_access :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction) {
	field, ok := field_debug(frame.function, instruction.payload)
	if !ok {
		vm_trap(vm, .Invalid_Instruction, "field index is invalid", instruction.source)
		return
	}
	request := runtime.Field_Request {
		base = get_operand(frame, instruction, 1),
		name = field,
		result_type = result_type_name(frame.function, instruction, 0),
	}
	if instruction.op == .Field_Load {
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
		return
	}
	request.value = get_operand(frame, instruction, 2)
	if !runtime.context_field_store(
		&vm.runtime_context,
		request,
		instruction_runtime_source(instruction),
	) {
		vm_sync_runtime_trap(vm)
		return
	}
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	frame.ip += 1
}

exec_cast :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction) {
	typ := result_type_name(frame.function, instruction, 0)
	value, ok := runtime.value_cast(get_operand(frame, instruction, 0), typ, vm.allocator)
	defer runtime.value_destroy(&value)
	if !ok {
		vm_trap(vm, .Type, "runtime cast failed", instruction.source)
		return
	}
	set_result(vm, frame, instruction, 0, value)
	frame.ip += 1
}

exec_call :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction) {
	target := bytecode.Function_Id(instruction.payload)
	args := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
	for i in 0 ..< int(instruction.operand_count) {
		args[i] = get_operand(frame, instruction, i)
	}
	return_registers := make([]bytecode.Register, instruction.result_count, vm.allocator)
	for i in 0 ..< int(instruction.result_count) {
		return_registers[i] = frame.function.result_registers[int(instruction.result_start) + i]
	}
	frame.ip += 1
	vm_push_function(vm, target, args[:], return_registers, instruction.source)
}

exec_branch :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction, relative_edge_index: int) {
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

exec_return :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction) {
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
}

exec_runtime_callback :: proc(vm: ^VM, frame: ^Frame, instruction: bytecode.Instruction) {
	if int(instruction.payload) >= len(frame.function.runtime_callbacks) {
		vm_trap(vm, .Invalid_Instruction, "runtime callback index is invalid", instruction.source)
		return
	}
	frame_index := len(vm.frames) - 1
	frame_count := len(vm.frames)
	callback := &frame.function.runtime_callbacks[int(instruction.payload)]
	if vm.options.dispatcher.dispatch != nil {
		vm.callback_context = Callback_Context {
			module = vm.module,
			function = frame.function,
			instruction = instruction,
			callback = callback,
			data = vm.options.dispatcher.data,
		}
		switch vm.options.dispatcher.dispatch(&vm.callback_context) {
			case .Ok:
				if vm.state != .Trapped && len(vm.frames) == frame_count {
					vm.frames[frame_index].ip += 1
				}
				return
			case .Trap:
				if vm.have_callback_trap {
					vm.state = .Trapped
					runtime.context_trap(
						&vm.runtime_context,
						vm.callback_trap.kind,
						vm.callback_trap.message,
						vm.callback_trap.source,
					)
				} else {
					vm_trap(vm, .Unsupported, "runtime callback trapped", instruction.source)
				}
			return
		case .Unsupported:
		}
	}
	dispatch_default_callback(vm, frame, instruction, callback)
	if vm.state != .Trapped && len(vm.frames) == frame_count {
		vm.frames[frame_index].ip += 1
	}
}

dispatch_default_callback :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	callback: ^bytecode.Runtime_Callback,
) {
	switch callback.kind {
	case .Abap:
		dispatch_abap_callback(vm, frame, instruction, callback)
	case .Call:
		dispatch_call_callback(vm, frame, instruction, callback)
	case .System_Field:
		dispatch_system_callback(vm, frame, instruction, callback)
	case .Table:
		dispatch_table_callback(vm, frame, instruction, callback)
	case .Sql:
		dispatch_sql_callback(vm, frame, instruction, callback)
	case .Unsupported:
		vm_trap(vm, .Unsupported, callback.payload.unsupported_message, instruction.source)
	}
}

dispatch_system_callback :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	callback: ^bytecode.Runtime_Callback,
) {
	if callback.payload.system_field == "" {
		vm_trap(vm, .Invalid_Instruction, "system callback field name is missing", instruction.source)
		return
	}
	#partial switch callback.op_kind {
	case .System_Read:
		value := runtime.context_system_read(&vm.runtime_context, callback.payload.system_field)
		defer runtime.value_destroy(&value)
		set_result(vm, frame, instruction, 0, value)
	case .System_Write:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		runtime.context_system_write(&vm.runtime_context, callback.payload.system_field, get_operand(frame, instruction, 1))
	case:
		vm_trap(vm, .Invalid_Instruction, "system callback operation is not implemented", instruction.source)
	}
}

dispatch_call_callback :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	callback: ^bytecode.Runtime_Callback,
) {
	if callback.op_kind != .Abap_Builtin_Call && callback.payload.has_call_function_target {
		target := bytecode.Function_Id(u32(callback.payload.call_function_target))
		args := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
		for i in 0 ..< int(instruction.operand_count) {
			args[i] = get_operand(frame, instruction, i)
		}
		return_registers := call_return_registers(frame, instruction, vm.allocator)
		frame.ip += 1
		vm_push_function(vm, target, args[:], return_registers, instruction.source)
		return
	}
	skip_world := 1 if callback.kind == .Call && callback.op_kind != .Abap_Builtin_Call else 0
	values := callback_values(frame, instruction, skip_world)
	result_index := 0
	if skip_world > 0 {
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		result_index = 1
	}
	if result_index >= int(instruction.result_count) && callback.op_kind != .Abap_Builtin_Call {
		_, ok := runtime.context_call(
			&vm.runtime_context,
			runtime.Call_Request {
				callee_name = callback.payload.callee_name,
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
			callee_name = callback.payload.callee_name,
			values = values,
			result_type = result_type_name(frame.function, instruction, result_index),
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
	instruction: bytecode.Instruction,
	allocator: mem.Allocator,
) -> []bytecode.Register {
	return_registers := make([]bytecode.Register, instruction.result_count, allocator)
	for i in 0 ..< int(instruction.result_count) {
		return_registers[i] = frame.function.result_registers[int(instruction.result_start) + i]
	}
	return return_registers
}

dispatch_table_callback :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	callback: ^bytecode.Runtime_Callback,
) {
	operation, operation_ok := table_operation(callback.op_kind)
	if !operation_ok {
		vm_trap(vm, .Unsupported, "ABAP table callback is not implemented", instruction.source)
		return
	}
	values := callback_values(frame, instruction, 1)
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	request := runtime.Table_Request {
		operation = operation,
		values = values,
	}
	#partial switch operation {
	case .Iter:
		request.result_type = result_type_name(frame.function, instruction, 1)
		iter, ok := runtime.context_table_iter(
			&vm.runtime_context,
			request,
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&iter)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_result(vm, frame, instruction, 1, iter)
	case .Next:
		request.result_type = result_type_name(frame.function, instruction, 2)
		has_row, row, next_iter, ok := runtime.context_table_next(
			&vm.runtime_context,
			request,
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&has_row)
		defer runtime.value_destroy(&row)
		defer runtime.value_destroy(&next_iter)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_operand(frame, instruction, 1, next_iter)
		set_result(vm, frame, instruction, 1, has_row)
		set_result(vm, frame, instruction, 2, row)
	case .Read:
		request.result_type = result_type_name(frame.function, instruction, 1)
		row, subrc, ok := runtime.context_table_read(
			&vm.runtime_context,
			request,
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&row)
		defer runtime.value_destroy(&subrc)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_result(vm, frame, instruction, 1, row)
		set_result(vm, frame, instruction, 2, subrc)
	case:
		if !runtime.context_table_mutate(
			&vm.runtime_context,
			request,
			instruction_runtime_source(instruction),
		) {
			vm_sync_runtime_trap(vm)
		}
	}
}

dispatch_sql_callback :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	callback: ^bytecode.Runtime_Callback,
) {
	operation, operation_ok := sql_operation(callback.op_kind)
	if !operation_ok {
		vm_trap(vm, .Unsupported, "Open SQL callback is not implemented", instruction.source)
		return
	}
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	request := runtime.SQL_Request {
		operation = operation,
		result_type = result_type_name(frame.function, instruction, 1),
	}
	if operation == .Select {
		value, subrc, ok := runtime.context_sql_select(
			&vm.runtime_context,
			request,
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&value)
		defer runtime.value_destroy(&subrc)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_result(vm, frame, instruction, 1, value)
		set_result(vm, frame, instruction, 2, subrc)
		return
	}
	if !runtime.context_sql_mutate(
		&vm.runtime_context,
		request,
		instruction_runtime_source(instruction),
	) {
		vm_sync_runtime_trap(vm)
	}
}

table_operation :: proc "contextless" (kind: ir.Op_Kind) -> (runtime.Table_Operation, bool) {
	#partial switch kind {
	case .Table_Iter:
		return .Iter, true
	case .Table_Next:
		return .Next, true
	case .Table_Read:
		return .Read, true
	case .Table_Append:
		return .Append, true
	case .Table_Insert:
		return .Insert, true
	case .Table_Modify:
		return .Modify, true
	case .Table_Delete:
		return .Delete, true
	case .Table_Sort:
		return .Sort, true
	}
	return .Read, false
}

sql_operation :: proc "contextless" (kind: ir.Op_Kind) -> (runtime.SQL_Operation, bool) {
	#partial switch kind {
	case .Sql_Select:
		return .Select, true
	case .Sql_Open_Cursor:
		return .Open_Cursor, true
	case .Sql_Fetch:
		return .Fetch, true
	case .Sql_Close_Cursor:
		return .Close_Cursor, true
	case .Sql_Modify:
		return .Modify, true
	case .Sql_Delete:
		return .Delete, true
	case .Sql_Insert:
		return .Insert, true
	case .Sql_Update:
		return .Update, true
	}
	return .Select, false
}

callback_values :: proc(
	frame: ^Frame,
	instruction: bytecode.Instruction,
	skip: int,
) -> []runtime.Value {
	count := int(instruction.operand_count) - skip
	if count <= 0 {
		return nil
	}
	values := make([]runtime.Value, count, context.temp_allocator)
	for i in 0 ..< count {
		values[i] = get_operand(frame, instruction, i + skip)
	}
	return values
}

dispatch_abap_callback :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	callback: ^bytecode.Runtime_Callback,
) {
	#partial switch callback.op_kind {
	case .Abap_Move:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	case .Abap_Add, .Abap_Subtract, .Abap_Multiply, .Abap_Divide:
		dispatch_integer_arithmetic(vm, frame, instruction, callback.op_kind)
	case .Abap_Equal,
	     .Abap_Not_Equal,
	     .Abap_Less,
	     .Abap_Less_Equal,
	     .Abap_Greater,
	     .Abap_Greater_Equal:
		dispatch_comparison(vm, frame, instruction, callback.op_kind)
	case .Abap_And, .Abap_Or:
		left := runtime.value_truthy(get_operand(frame, instruction, 0))
		right := runtime.value_truthy(get_operand(frame, instruction, 1))
		set_result(
			vm,
			frame,
			instruction,
			0,
			runtime.value_predicate(left && right if callback.op_kind == .Abap_And else left || right),
		)
	case .Abap_Not:
		set_result(vm, frame, instruction, 0, runtime.value_predicate(!runtime.value_truthy(get_operand(frame, instruction, 0))))
	case .Abap_Is_Initial:
		set_result(vm, frame, instruction, 0, runtime.value_predicate(runtime.value_is_initial(get_operand(frame, instruction, 0))))
	case .Abap_String_Concat, .Abap_String_Template:
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
	case .Abap_Construct:
		dispatch_construct(vm, frame, instruction, callback)
	case .Abap_Write:
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
	case .Abap_Message:
		dispatch_message(vm, frame, instruction, callback)
	case .Abap_Clear, .Abap_Refresh, .Abap_Free, .Abap_Unassign:
		set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
		if instruction.result_count > 1 {
			value := runtime.initial_for_type(result_type_name(frame.function, instruction, 1), vm.allocator)
			defer runtime.value_destroy(&value)
			set_result(vm, frame, instruction, 1, value)
		}
	case .Abap_Assign_Field:
		values := callback_values(frame, instruction, 1)
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
		vm_trap(vm, .Unsupported, "ABAP runtime callback is not implemented", instruction.source)
	}
}

dispatch_integer_arithmetic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	kind: ir.Op_Kind,
) {
	arithmetic: runtime.Arithmetic_Kind
	#partial switch kind {
	case .Abap_Add:
		arithmetic = .Add
	case .Abap_Subtract:
		arithmetic = .Subtract
	case .Abap_Multiply:
		arithmetic = .Multiply
	case .Abap_Divide:
		arithmetic = .Divide
	case:
		vm_trap(vm, .Unsupported, "ABAP integer arithmetic callback is not implemented", instruction.source)
		return
	}
	result, result_ok := runtime.abap_integer_arithmetic(
		&vm.runtime_context,
		arithmetic,
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

dispatch_comparison :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	kind: ir.Op_Kind,
) {
	comparison: runtime.Comparison_Kind
	#partial switch kind {
	case .Abap_Equal:
		comparison = .Equal
	case .Abap_Not_Equal:
		comparison = .Not_Equal
	case .Abap_Less:
		comparison = .Less
	case .Abap_Less_Equal:
		comparison = .Less_Equal
	case .Abap_Greater:
		comparison = .Greater
	case .Abap_Greater_Equal:
		comparison = .Greater_Equal
	case:
		vm_trap(vm, .Unsupported, "ABAP comparison callback is not implemented", instruction.source)
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

dispatch_construct :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
	callback: ^bytecode.Runtime_Callback,
) {
	values := make([]runtime.Value, instruction.operand_count, context.temp_allocator)
	for i in 0 ..< int(instruction.operand_count) {
		values[i] = get_operand(frame, instruction, i)
	}
	value, ok := runtime.abap_construct(
		&vm.runtime_context,
		callback.payload.callee_name,
		values[:],
		result_type_name(frame.function, instruction, 0),
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
	instruction: bytecode.Instruction,
	callback: ^bytecode.Runtime_Callback,
) {
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	payload := callback.payload
	first_arg := 1 + payload.message_head_operands
	values := make([]runtime.Value, payload.message_arg_count, context.temp_allocator)
	for i := 0; i < payload.message_arg_count; i += 1 {
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
			message_id = payload.message_id,
			message_type = payload.message_type,
			message_number = payload.message_number,
		},
		values[:],
		instruction_runtime_source(instruction),
	)
	if !ok {
		vm_sync_runtime_trap(vm)
		return
	}
	if callback.payload.message_has_into && instruction.result_count > 1 {
		value := runtime.value_string(text, vm.allocator)
		defer runtime.value_destroy(&value)
		set_result(vm, frame, instruction, 1, value)
	}
}

vm_push_function :: proc(
	vm: ^VM,
	id: bytecode.Function_Id,
	args: []runtime.Value,
	return_registers: []bytecode.Register,
	source: ir.Source_Loc,
) -> bool {
	if id == bytecode.INVALID_FUNCTION_ID || int(id) >= len(vm.module.functions) {
		vm_trap(vm, .Invalid_Function, "call target function is invalid", source)
		return false
	}
	function := &vm.module.functions[int(id)]
	frame := Frame {
		function_id = id,
		function = function,
		allocator = vm.allocator,
		registers = make([]runtime.Value, function.register_count, vm.allocator),
		slots = make([]runtime.Value, len(function.slots), vm.allocator),
		return_registers = return_registers,
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
			runtime.value_destroy(&frame.slots[slot_index])
			frame.slots[slot_index] = runtime.value_clone(args[arg_index], frame.allocator)
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
			return frame.slots[slot_index], true
		}
		param_index += 1
	}
	return {}, false
}

slot_debug :: #force_inline proc "contextless" (function: ^bytecode.Function, payload: u32) -> (bytecode.Slot_Debug, bool) {
	if int(payload) >= len(function.slots) {
		return {}, false
	}
	return function.slots[int(payload)], true
}

field_debug :: #force_inline proc "contextless" (function: ^bytecode.Function, payload: u32) -> (string, bool) {
	if int(payload) >= len(function.fields) {
		return "", false
	}
	return function.fields[int(payload)].name, true
}

get_operand :: #force_inline proc "contextless" (
	frame: ^Frame,
	instruction: bytecode.Instruction,
	index: int,
) -> runtime.Value {
	if index < 0 || index >= int(instruction.operand_count) {
		return {}
	}
	register := frame.function.operand_registers[int(instruction.operand_start) + index]
	return register_value(frame, register)
}

set_operand :: proc(frame: ^Frame, instruction: bytecode.Instruction, index: int, value: runtime.Value) {
	if index < 0 || index >= int(instruction.operand_count) {
		return
	}
	register := frame.function.operand_registers[int(instruction.operand_start) + index]
	set_register(frame, register, value)
}

set_result :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: bytecode.Instruction,
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

register_value :: #force_inline proc "contextless" (frame: ^Frame, register: bytecode.Register) -> runtime.Value {
	if register == bytecode.INVALID_REGISTER || int(register) >= len(frame.registers) {
		return {}
	}
	return frame.registers[int(register)]
}

set_register :: proc(frame: ^Frame, register: bytecode.Register, value: runtime.Value) {
	if register == bytecode.INVALID_REGISTER || int(register) >= len(frame.registers) {
		return
	}
	new_value := runtime.value_clone(value, frame.allocator)
	runtime.value_destroy(&frame.registers[int(register)])
	frame.registers[int(register)] = new_value
}

result_type_name :: #force_inline proc "contextless" (
	function: ^bytecode.Function,
	instruction: bytecode.Instruction,
	index: int,
) -> string {
	if index < 0 || index >= int(instruction.result_count) {
		return ""
	}
	register := function.result_registers[int(instruction.result_start) + index]
	if register == bytecode.INVALID_REGISTER || int(register) >= len(function.values) {
		return ""
	}
	return function.values[int(register)].type_name
}

current_source :: #force_inline proc "contextless" (vm: ^VM) -> ir.Source_Loc {
	if len(vm.frames) == 0 {
		return {}
	}
	frame := &vm.frames[len(vm.frames) - 1]
	if int(frame.ip) < len(frame.function.instructions) {
		return frame.function.instructions[int(frame.ip)].source
	}
	return {}
}

vm_trap :: proc(vm: ^VM, kind: runtime.Trap_Kind, message: string, source: ir.Source_Loc = {}) {
	vm.state = .Trapped
	runtime.context_trap(&vm.runtime_context, kind, message, runtime_source_from_ir(source))
}

vm_sync_runtime_trap :: #force_inline proc "contextless" (vm: ^VM) {
	if runtime.context_trapped(&vm.runtime_context) {
		vm.state = .Trapped
	}
}

runtime_source_from_ir :: #force_inline proc "contextless" (source: ir.Source_Loc) -> runtime.Source_Loc {
	path := ""
	if source.file != nil {
		path = source.file.path
	}
	return runtime.Source_Loc {
		path = path,
		range = runtime.Source_Range {
			start = source.range.start,
			end = source.range.end,
		},
	}
}

instruction_runtime_source :: #force_inline proc "contextless" (instruction: bytecode.Instruction) -> runtime.Source_Loc {
	return runtime_source_from_ir(instruction.source)
}
