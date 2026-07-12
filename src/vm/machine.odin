package abap_frontend_vm

import runtime "src:vm/runtime"

import "core:mem"
import virtual "core:mem/virtual"

DEFAULT_STEP_LIMIT :: u64(1_000_000)

VM_State :: enum {
	Ready,
	Running,
	Paused,
	Completed,
	Trapped,
}

VM :: struct {
	module:            ^Prepared_Module,
	options:           Run_Options,
	state:             VM_State,
	allocator:         mem.Allocator,
	scratch_arena:     ^virtual.Arena,
	scratch_allocator: mem.Allocator,
	scratch_active:    bool,
	frames:            [dynamic]Frame,
	runtime_context:   runtime.Context,
	instruction_count: u64,
}

VM_Scratch_Scope :: struct {
	vm:   ^VM,
	temp: virtual.Arena_Temp,
}

vm_make_prepared :: proc(
	module: ^Prepared_Module,
	options: Run_Options,
	allocator: mem.Allocator = context.allocator,
) -> VM {
	return vm_make(module, options, allocator)
}

vm_make :: proc(module: ^Prepared_Module, options: Run_Options, allocator: mem.Allocator) -> VM {
	opts := options
	if opts.step_limit == 0 {
		opts.step_limit = DEFAULT_STEP_LIMIT
	}
	scratch_arena := new(virtual.Arena, allocator)
	assert(scratch_arena != nil)
	arena_err := virtual.arena_init_growing(scratch_arena)
	assert(arena_err == .None)
	return VM {
		module = module,
		options = opts,
		state = .Ready,
		allocator = allocator,
		scratch_arena = scratch_arena,
		scratch_allocator = virtual.arena_allocator(scratch_arena),
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
	if vm.scratch_arena != nil {
		virtual.arena_destroy(vm.scratch_arena)
		free(vm.scratch_arena, vm.allocator)
	}
	vm^ = {}
}

vm_scratch_begin :: proc(vm: ^VM) -> VM_Scratch_Scope {
	assert(!vm.scratch_active)
	scope := VM_Scratch_Scope{vm = vm}
	vm.scratch_active = true
	scope.temp = virtual.arena_temp_begin(vm.scratch_arena)
	return scope
}

vm_scratch_end :: proc(scope: VM_Scratch_Scope) {
	assert(scope.vm.scratch_active)
	virtual.arena_temp_end(scope.temp)
	scope.vm.scratch_active = false
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

vm_step :: proc(vm: ^VM) -> VM_State {
	context.temp_allocator = vm.scratch_allocator
	scratch := vm_scratch_begin(vm)
	defer vm_scratch_end(scratch)

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

	#partial switch instruction.opcode {
	case .Const:
		exec_const(vm, frame, instruction)
	case .Initial:
		exec_initial(vm, frame, instruction)
	case .Null_Ref:
		exec_null_ref(vm, frame, instruction)
	case .Add, .Sub, .Mul, .Div, .Mod:
		exec_integer_binary(vm, frame, instruction)
	case .Neg:
		exec_integer_negate(vm, frame, instruction)
	case .And, .Or, .Xor:
		exec_logical_binary(vm, frame, instruction)
	case .Not:
		exec_logical_not(vm, frame, instruction)
	case .Cmp:
		exec_cmp(vm, frame, instruction)
	case .Select:
		exec_select(vm, frame, instruction)
	case .Alloca:
		exec_alloca(vm, frame, instruction)
	case .Addr_Of:
		exec_addr_of(vm, frame, instruction)
	case .Deref:
		exec_deref(vm, frame, instruction)
	case .Field_Addr:
		exec_field_addr(vm, frame, instruction)
	case .Load:
		exec_load(vm, frame, instruction)
	case .Store:
		exec_store(vm, frame, instruction)
	case .Struct_Init:
		exec_struct_init(vm, frame, instruction)
	case .Extract_Value:
		exec_extract_value(vm, frame, instruction)
	case .Cast, .Int_Extend, .Int_Truncate, .Ref_Cast, .Addr_Cast:
		exec_cast(vm, frame, instruction)
	case .Debug_Value:
		frame.ip += 1
	case .Call:
		exec_call(vm, frame, instruction)
	case .Invoke:
		exec_invoke(vm, frame, instruction)
	case .Intrinsic:
		exec_intrinsic(vm, frame, instruction)
	case .Br:
		exec_branch(vm, frame, instruction, 0)
	case .Cond_Br:
		condition := get_operand(frame, instruction, 0)
		edge_index := 0 if runtime.value_truthy(condition) else 1
		exec_branch(vm, frame, instruction, edge_index)
	case .Switch:
		exec_switch(vm, frame, instruction)
	case .Return:
		exec_return(vm, frame, instruction)
	case .Unreachable:
		vm_trap(vm, .Invalid_Instruction, "reached unreachable IR instruction", instruction.source)
	case .Trap:
		exec_trap(vm, instruction)
	case .Unsupported:
		vm_trap(vm, .Unsupported, "unsupported IR instruction", instruction.source)
	case:
		vm_trap(vm, .Unsupported, "IR instruction is not implemented by VM", instruction.source)
	}
	if vm.state != .Trapped && len(vm.frames) == 0 {
		vm.state = .Completed
	}
	return vm.state
}
