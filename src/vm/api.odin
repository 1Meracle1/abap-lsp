package abap_frontend_vm

import ir "src:ir"
import runtime "src:vm/runtime"

import "core:mem"

Run_Status :: enum {
	Completed,
	Trapped,
}

Run_Result :: struct {
	status:            Run_Status,
	trap:              runtime.Trap,
	stack_trace:       [dynamic]Stack_Trace_Frame,
	events:            [dynamic]runtime.IO_Event,
	final_values:      [dynamic]runtime.Named_Value,
	instruction_count: u64,
}

Run_Options :: struct {
	step_limit: u64,
	io_policy:  runtime.IO_Policy,
	dispatcher: Intrinsic_Dispatcher,
}

execute_module :: proc(
	module: ^ir.Module,
	options: Run_Options = {},
	allocator: mem.Allocator = context.allocator,
) -> Run_Result {
	prepared := prepare_module(module, allocator)
	defer prepare_result_destroy(&prepared)
	if !prepared.ok {
		return run_result_trapped(.Invalid_Module, prepared.message, prepared.source, allocator)
	}
	return execute_prepared_module(&prepared.module, options, allocator)
}

execute_prepared_module :: proc(
	module: ^Prepared_Module,
	options: Run_Options = {},
	allocator: mem.Allocator = context.allocator,
) -> Run_Result {
	vm := vm_make_prepared(module, options, allocator)
	defer vm_destroy(&vm)
	vm_run_until_complete(&vm)
	return run_result_from_vm(&vm, allocator)
}

run_result_destroy :: proc(result: ^Run_Result) {
	for &frame in result.stack_trace {
		stack_trace_frame_destroy(&frame)
	}
	delete(result.stack_trace)
	for &event in result.events {
		runtime.io_event_destroy(&event)
	}
	delete(result.events)
	for &value in result.final_values {
		runtime.named_value_destroy(&value)
	}
	delete(result.final_values)
	delete(result.trap.message)
	runtime.source_loc_destroy(&result.trap.source)
	result^ = {}
}

run_result_from_vm :: proc(vm: ^VM, allocator: mem.Allocator = context.allocator) -> Run_Result {
	assert(vm_is_finished(vm))
	context.temp_allocator = vm.scratch_allocator
	scratch := vm_scratch_begin(vm)
	defer vm_scratch_end(scratch)

	status := Run_Status.Completed
	if vm.state == .Trapped {
		status = .Trapped
	}
	result := Run_Result {
		status = status,
		trap = runtime.trap_clone(vm.runtime_context.trap, allocator),
		stack_trace = make([dynamic]Stack_Trace_Frame, 0, len(vm.frames), allocator),
		events = make([dynamic]runtime.IO_Event, 0, len(vm.runtime_context.events), allocator),
		final_values = make(
			[dynamic]runtime.Named_Value,
			0,
			len(vm.runtime_context.global_values) + len(vm.runtime_context.system_values),
			allocator,
		),
		instruction_count = vm.instruction_count,
	}
	if status == .Trapped {
		vm_collect_stack_trace(vm, &result.stack_trace, allocator)
	}
	for event in vm.runtime_context.events {
		append(&result.events, runtime.io_event_clone(event, allocator))
	}
	runtime.context_collect_final_values(&vm.runtime_context, &result.final_values, allocator)
	return result
}
