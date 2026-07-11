package abap_frontend_vm

import ir "src:ir"
import runtime "src:vm/runtime"

import "core:mem"
import "core:strings"

Stack_Trace_Frame :: struct {
	name:   string,
	source: runtime.Source_Loc,
}

vm_collect_stack_trace :: proc(
	vm: ^VM,
	out: ^[dynamic]Stack_Trace_Frame,
	allocator: mem.Allocator,
) {
	for offset in 0 ..< len(vm.frames) {
		frame_index := len(vm.frames) - 1 - offset
		frame := &vm.frames[frame_index]
		if frame.function.role == .Report_Entry {
			continue
		}
		source := runtime.Source_Loc{}
		if frame_index == len(vm.frames) - 1 {
			source = vm.runtime_context.trap.source
			if !runtime_source_has_location(source) {
				source = frame_current_source(frame)
			}
		} else {
			source = vm.frames[frame_index + 1].call_source
			if !runtime_source_has_location(source) {
				source = frame_current_source(frame)
			}
		}
		name := frame.function.name
		if name == "" {
			name = "<anonymous>"
		}
		append(
			out,
			Stack_Trace_Frame {
					name = strings.clone(name, allocator),
					source = runtime.source_loc_clone(source, allocator),
			},
		)
	}
}

stack_trace_frame_destroy :: proc(frame: ^Stack_Trace_Frame) {
	delete(frame.name)
	runtime.source_loc_destroy(&frame.source)
	frame^ = {}
}

current_source :: #force_inline proc "contextless" (vm: ^VM) -> runtime.Source_Loc {
	if len(vm.frames) == 0 {
		return {}
	}
	frame := &vm.frames[len(vm.frames) - 1]
	return frame_current_source(frame)
}

frame_current_source :: #force_inline proc "contextless" (frame: ^Frame) -> runtime.Source_Loc {
	if int(frame.ip) < len(frame.function.instructions) {
		return frame.function.instructions[int(frame.ip)].source
	}
	return {}
}

vm_trap :: proc(vm: ^VM, kind: runtime.Trap_Kind, message: string, source: runtime.Source_Loc = {}) {
	vm.state = .Trapped
	runtime.context_trap(&vm.runtime_context, kind, message, source)
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

runtime_source_has_location :: #force_inline proc "contextless" (source: runtime.Source_Loc) -> bool {
	return source.path != "" || source.range.end > source.range.start
}

instruction_runtime_source :: #force_inline proc "contextless" (instruction: Prepared_Instruction) -> runtime.Source_Loc {
	return instruction.source
}
