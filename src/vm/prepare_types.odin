package abap_frontend_vm

import ir "src:ir"
import runtime "src:vm/runtime"

import "core:mem"

Register :: distinct u32

INVALID_REGISTER :: Register(0xffffffff)
UNKNOWN_FIELD_INDEX :: i32(-1)
UNKNOWN_FIELD_BYTE_OFFSET :: i32(-1)

Prepared_Address_Kind :: enum {
	None,
	Slot,
	Field,
}

Prepared_Edge :: struct {
	target:      u32,
	case_value:  Register,
	arg_start:   u32,
	arg_count:   u32,
	param_start: u32,
	param_count: u32,
}

Field_Segment_Ref :: struct {
	name:        string,
	field_index: i32,
	byte_offset: i32,
}

Field_Ref :: struct {
	name:        string,
	result_type: runtime.Type_Descriptor,
	result_type_name: string,
	field_index: i32,
	byte_offset: i32,
	projection:  [dynamic]Field_Segment_Ref,
}

Slot_Debug :: struct {
	kind:            ir.Slot_Kind,
	name:            string,
	type:            runtime.Type_Descriptor,
	type_name:       string,
	is_field_symbol: bool,
}

Value_Debug :: struct {
	name:      string,
	type:      runtime.Type_Descriptor,
	type_name: string,
}

Block_Debug :: struct {
	name:        string,
	param_start: u32,
	param_count: u32,
}

Prepared_Instruction :: struct {
	opcode:            ir.Opcode,
	address_kind:      Prepared_Address_Kind,
	payload:           u32,
	call_target:       ir.Function_Id,
	compare_predicate: ir.Compare_Predicate,
	compare_mode:      ir.Compare_Mode,
	intrinsic_op:      ir.Intrinsic_Op,
	intrinsic_family:  ir.Intrinsic_Family,
	intrinsic_name:    string,
	intrinsic_payload: ir.Intrinsic_Payload,
	trap_message:      string,
	operand_start:    u32,
	operand_count:    u32,
	result_start:     u32,
	result_count:     u32,
	edge_start:       u32,
	edge_count:       u32,
	source:           runtime.Source_Loc,
}

Prepared_Function :: struct {
	name:               string,
	role:               ir.Function_Role,
	frame_slot_count:   u32,
	block_offsets:      [dynamic]u32,
	block_debug:        [dynamic]Block_Debug,
	block_params:       [dynamic]Register,
	constants:          [dynamic]string,
	fields:             [dynamic]Field_Ref,
	slots:              [dynamic]Slot_Debug,
	values:             [dynamic]Value_Debug,
	return_type_names:  [dynamic]string,
	operand_registers:  [dynamic]Register,
	result_registers:   [dynamic]Register,
	edge_registers:     [dynamic]Register,
	edges:              [dynamic]Prepared_Edge,
	instructions:       [dynamic]Prepared_Instruction,
}

Prepared_Module :: struct {
	allocator:        mem.Allocator,
	entries:          [dynamic]ir.Function_Id,
	type_names:       [dynamic]string,
	type_descriptors: [dynamic]ir.Runtime_Type_Descriptor,
	functions:        [dynamic]Prepared_Function,
}

Prepare_Result :: struct {
	module:  Prepared_Module,
	ok:      bool,
	message: string,
	source:  ir.Source_Loc,
}

Unsupported_Search :: struct {
	found:   bool,
	message: string,
	source:  ir.Source_Loc,
}

Prepare_Context :: struct {
	module:       ^Prepared_Module,
	ir_module:    ^ir.Module,
	out_function: ^Prepared_Function,
	ir_function:  ^ir.Function,
}

prepare_result_destroy :: proc(result: ^Prepare_Result) {
	assert(result != nil)
	prepared_module_destroy(&result.module)
	delete(result.message)
	result^ = {}
}

prepared_module_destroy :: proc(module: ^Prepared_Module) {
	assert(module != nil)
	for &function in module.functions {
		prepared_function_destroy(&function, module.allocator)
	}
	for type_name in module.type_names {
		delete(type_name, module.allocator)
	}
	for &descriptor in module.type_descriptors {
		ir.runtime_type_descriptor_destroy(&descriptor, module.allocator)
	}
	delete(module.entries)
	delete(module.type_names)
	delete(module.type_descriptors)
	delete(module.functions)
	module^ = {}
}

prepared_function_destroy :: proc(function: ^Prepared_Function, allocator: mem.Allocator) {
	assert(function != nil)
	delete(function.name, allocator)
	for block in function.block_debug {
		delete(block.name, allocator)
	}
	for constant in function.constants {
		delete(constant, allocator)
	}
	for &field in function.fields {
		field_ref_destroy_owned(&field, allocator)
	}
	for slot in function.slots {
		delete(slot.name, allocator)
		delete(slot.type_name, allocator)
	}
	for value in function.values {
		delete(value.name, allocator)
		delete(value.type_name, allocator)
	}
	for return_type_name in function.return_type_names {
		delete(return_type_name, allocator)
	}
	for &instruction in function.instructions {
		delete(instruction.source.path, allocator)
		delete(instruction.intrinsic_name, allocator)
		ir.intrinsic_payload_destroy(&instruction.intrinsic_payload, allocator)
		delete(instruction.trap_message, allocator)
	}
	delete(function.block_offsets)
	delete(function.block_debug)
	delete(function.block_params)
	delete(function.constants)
	delete(function.fields)
	delete(function.slots)
	delete(function.values)
	delete(function.return_type_names)
	delete(function.operand_registers)
	delete(function.result_registers)
	delete(function.edge_registers)
	delete(function.edges)
	delete(function.instructions)
	function^ = {}
}

prepared_module_entry_count :: proc "contextless" (module: ^Prepared_Module) -> int {
	if module == nil {
		return 0
	}
	return len(module.entries)
}

prepared_module_make :: proc(module: ^ir.Module, allocator: mem.Allocator) -> Prepared_Module {
	out := Prepared_Module {
		allocator = allocator,
		entries = make([dynamic]ir.Function_Id, 0, len(module.entries), allocator),
		type_names = make([dynamic]string, 0, len(module.types), allocator),
		type_descriptors = make([dynamic]ir.Runtime_Type_Descriptor, 0, len(module.types), allocator),
		functions = make([dynamic]Prepared_Function, 0, len(module.functions), allocator),
	}
	for entry in module.entries {
		append(&out.entries, entry)
	}
	for _, i in module.types {
		append(&out.type_names, clone_type_name(module, ir.Type_Id(i), allocator))
		append(&out.type_descriptors, ir.runtime_type_descriptor_clone(module.types[i].runtime, allocator))
	}
	return out
}
