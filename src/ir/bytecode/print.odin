package abap_frontend_ir_bytecode

import ir "src:ir"

import "core:mem"
import "core:strings"

Print_Options :: struct {
	raw:         bool,
	show_source: bool,
}

print_module :: proc(
	module: ^Module,
	allocator: mem.Allocator = context.allocator,
	options: Print_Options = {},
) -> string {
	if options.raw {
		return print_module_raw(module, allocator, options)
	}

	out: strings.Builder
	strings.builder_init(&out, 0, 4096, allocator)
	assert(module != nil)

	for entry in module.entries {
		strings.write_string(&out, "entry @")
		print_function_name(&out, module, entry)
		strings.write_byte(&out, '\n')
	}
	if len(module.entries) > 0 && len(module.functions) > 0 {
		strings.write_byte(&out, '\n')
	}

	for &function, i in module.functions {
		if i > 0 {
			strings.write_byte(&out, '\n')
		}
		print_function(&out, module, &function, Function_Id(i), options)
	}
	return strings.to_string(out)
}

print_module_raw :: proc(
	module: ^Module,
	allocator: mem.Allocator = context.allocator,
	options: Print_Options = {raw = true},
) -> string {
	out: strings.Builder
	strings.builder_init(&out, 0, 4096, allocator)
	assert(module != nil)
	for &function, i in module.functions {
		if i > 0 {
			strings.write_byte(&out, '\n')
		}
		print_function_raw(&out, module, &function, Function_Id(i), options)
	}
	return strings.to_string(out)
}

print_function :: proc(
	out: ^strings.Builder,
	module: ^Module,
	function: ^Function,
	id: Function_Id,
	options: Print_Options,
) {
	strings.write_string(out, "bytecode @")
	if function.name != "" {
		strings.write_string(out, function.name)
	} else {
		strings.write_string(out, "function")
		strings.write_int(out, int(id))
	}
	if function.role != .Unknown {
		strings.write_string(out, " role=")
		strings.write_string(out, ir.function_role_name(function.role))
	}
	strings.write_string(out, " registers=")
	strings.write_int(out, int(function.register_count))
	strings.write_string(out, " -> (")
	for return_type_name, i in function.return_type_names {
		if i > 0 {
			strings.write_string(out, ", ")
		}
		strings.write_string(out, return_type_name)
	}
	strings.write_string(out, ") {\n")

	print_slots_section(out, function)
	print_blocks_section(out, function)
	print_constants_section(out, function)
	print_fields_section(out, function)
	print_runtimes_section(out, module, function)
	print_code_section(out, module, function, options)

	strings.write_string(out, "}\n")
}

print_slots_section :: proc(out: ^strings.Builder, function: ^Function) {
	if len(function.slots) == 0 {
		return
	}
	strings.write_string(out, "  slots:\n")
	for slot, i in function.slots {
		strings.write_string(out, "    %s")
		strings.write_int(out, i)
		strings.write_byte(out, ' ')
		strings.write_string(out, ir.slot_kind_name(slot.kind))
		if slot.name != "" {
			strings.write_byte(out, ' ')
			strings.write_string(out, slot.name)
		}
		strings.write_string(out, " : ")
		strings.write_string(out, slot.type_name)
		strings.write_byte(out, '\n')
	}
}

print_blocks_section :: proc(out: ^strings.Builder, function: ^Function) {
	if len(function.block_offsets) == 0 {
		return
	}
	strings.write_string(out, "  blocks:\n")
	for offset, i in function.block_offsets {
		strings.write_string(out, "    ")
		print_block_ref(out, function, i)
		strings.write_string(out, " = ")
		print_block_offset(out, offset)
		strings.write_byte(out, '(')
		if i < len(function.block_debug) {
			debug := function.block_debug[i]
			print_register_pool_range(
				out,
				function,
				function.block_params[:],
				debug.param_start,
				debug.param_count,
				true,
				true,
			)
		}
		strings.write_string(out, ")\n")
	}
}

print_constants_section :: proc(out: ^strings.Builder, function: ^Function) {
	if len(function.constants) == 0 {
		return
	}
	strings.write_string(out, "  constants:\n")
	for constant, i in function.constants {
		strings.write_string(out, "    #")
		strings.write_int(out, i)
		strings.write_string(out, " raw=")
		print_literal_spelling(out, constant)
		strings.write_byte(out, '\n')
	}
}

print_fields_section :: proc(out: ^strings.Builder, function: ^Function) {
	if len(function.fields) == 0 {
		return
	}
	strings.write_string(out, "  fields:\n")
	for field, i in function.fields {
		strings.write_string(out, "    #")
		strings.write_int(out, i)
		strings.write_string(out, " .")
		strings.write_string(out, field)
		strings.write_byte(out, '\n')
	}
}

print_runtimes_section :: proc(out: ^strings.Builder, module: ^Module, function: ^Function) {
	if len(function.runtime_callbacks) == 0 {
		return
	}
	strings.write_string(out, "  runtimes:\n")
	for callback, i in function.runtime_callbacks {
		strings.write_string(out, "    #")
		strings.write_int(out, i)
		strings.write_byte(out, ' ')
		if callback.name != "" {
			strings.write_string(out, callback.name)
		} else {
			strings.write_string(out, ir.op_kind_name(callback.op_kind))
		}
		strings.write_string(out, " kind=")
		strings.write_string(out, runtime_callback_kind_name(callback.kind))
		strings.write_string(out, " op=")
		strings.write_string(out, ir.op_kind_name(callback.op_kind))
		print_runtime_payload(out, module, callback)
		strings.write_byte(out, '\n')
	}
}

print_code_section :: proc(
	out: ^strings.Builder,
	module: ^Module,
	function: ^Function,
	options: Print_Options,
) {
	strings.write_string(out, "  code:\n")
	for instruction, ip in function.instructions {
		print_block_headers_at_ip(out, function, Block_Offset(ip))
		strings.write_string(out, "    ")
		strings.write_int(out, ip)
		strings.write_string(out, ": ")
		print_instruction(out, module, function, instruction, options)
		strings.write_byte(out, '\n')
	}
}

print_block_headers_at_ip :: proc(
	out: ^strings.Builder,
	function: ^Function,
	offset: Block_Offset,
) {
	for block_offset, i in function.block_offsets {
		if block_offset != offset {
			continue
		}
		strings.write_string(out, "  ")
		print_block_ref(out, function, i)
		strings.write_string(out, " @")
		print_block_offset(out, offset)
		strings.write_byte(out, '(')
		if i < len(function.block_debug) {
			debug := function.block_debug[i]
			print_register_pool_range(
				out,
				function,
				function.block_params[:],
				debug.param_start,
				debug.param_count,
				true,
				true,
			)
		}
		strings.write_string(out, "):\n")
	}
}

print_instruction :: proc(
	out: ^strings.Builder,
	module: ^Module,
	function: ^Function,
	instruction: Instruction,
	options: Print_Options,
) {
	if instruction.result_count > 0 {
		print_register_pool_range(
			out,
			function,
			function.result_registers[:],
			instruction.result_start,
			instruction.result_count,
			true,
			false,
		)
		strings.write_string(out, " = ")
	}

	switch instruction.op {
	case .Const:
		strings.write_string(out, "core.const literal=")
		if int(instruction.payload) < len(function.constants) {
			result_type := instruction_result_type(function, instruction)
			print_literal_value(out, function.constants[int(instruction.payload)], result_type)
		} else {
			strings.write_string(out, "<invalid>")
		}
	case .Load:
		strings.write_string(out, "core.load ")
		print_slot_ref(out, function, instruction.payload)
		print_instruction_operands(out, function, instruction)
	case .Store:
		strings.write_string(out, "core.store ")
		print_slot_ref(out, function, instruction.payload)
		print_instruction_operands(out, function, instruction)
	case .Field_Load:
		strings.write_string(out, "core.field_load ")
		print_field_ref(out, function, instruction.payload)
		print_instruction_operands(out, function, instruction)
	case .Field_Store:
		strings.write_string(out, "core.field_store ")
		print_field_ref(out, function, instruction.payload)
		print_instruction_operands(out, function, instruction)
	case .Cast:
		strings.write_string(out, "core.cast ")
		print_type_id(out, module, ir.Type_Id(instruction.payload))
		print_instruction_operands(out, function, instruction)
	case .Call:
		strings.write_string(out, "core.call ")
		print_function_ref(out, module, Function_Id(instruction.payload))
		print_instruction_operands(out, function, instruction)
	case .Call_Runtime:
		print_runtime_instruction(out, module, function, instruction)
	case .Branch:
		strings.write_string(out, "br ")
		if instruction.edge_count > 0 {
			print_edge_target(out, function, instruction.edge_start)
		} else {
			print_block_ref_for_offset(out, function, instruction.target)
			strings.write_string(out, "()")
		}
	case .Cond_Branch:
		strings.write_string(out, "cond ")
		if instruction.operand_count > 0 {
			print_register_pool_ref(
				out,
				function,
				function.operand_registers[:],
				instruction.operand_start,
				false,
				false,
			)
		} else {
			strings.write_string(out, "%invalid")
		}
		strings.write_string(out, " ? ")
		if instruction.edge_count >= 1 {
			print_edge_target(out, function, instruction.edge_start)
		} else {
			print_block_ref_for_offset(out, function, instruction.target)
			strings.write_string(out, "()")
		}
		strings.write_string(out, " : ")
		if instruction.edge_count >= 2 {
			print_edge_target(out, function, instruction.edge_start + 1)
		} else {
			print_block_ref_for_offset(out, function, Block_Offset(instruction.payload))
			strings.write_string(out, "()")
		}
	case .Return:
		strings.write_string(out, "return")
		print_instruction_operands(out, function, instruction)
	case .Unreachable:
		strings.write_string(out, "unreachable")
	case .Nop:
		strings.write_string(out, "nop")
	case .Move:
		strings.write_string(out, "move")
		print_instruction_operands(out, function, instruction)
	case .Unsupported:
		strings.write_string(out, "unsupported")
	}

	if options.show_source {
		print_source_annotation(out, instruction.source)
	}
}

print_runtime_instruction :: proc(
	out: ^strings.Builder,
	module: ^Module,
	function: ^Function,
	instruction: Instruction,
) {
	if int(instruction.payload) >= len(function.runtime_callbacks) {
		strings.write_string(out, "runtime#")
		strings.write_int(out, int(instruction.payload))
		print_instruction_operands(out, function, instruction)
		return
	}
	callback := function.runtime_callbacks[int(instruction.payload)]
	if callback.name != "" {
		strings.write_string(out, callback.name)
	} else {
		strings.write_string(out, ir.op_kind_name(callback.op_kind))
	}
	strings.write_byte(out, '#')
	strings.write_int(out, int(instruction.payload))
	print_runtime_payload(out, module, callback)
	print_instruction_operands(out, function, instruction)
}

print_runtime_payload :: proc(out: ^strings.Builder, module: ^Module, callback: Runtime_Callback) {
	payload := callback.payload
	if payload.system_field != "" {
		strings.write_string(out, " sy-")
		strings.write_string(out, payload.system_field)
	}
	if payload.has_call_function_target {
		strings.write_string(out, " ")
		print_function_ref(out, module, Function_Id(u32(payload.call_function_target)))
	} else if payload.callee_name != "" {
		strings.write_string(out, " @")
		strings.write_string(out, payload.callee_name)
	}
	if ir.print_op_call_kind(callback.op_kind, payload.call_kind) {
		strings.write_string(out, " call=")
		strings.write_string(out, ir.abap_call_kind_name(payload.call_kind))
	}
	if payload.message_form != .Unknown {
		strings.write_string(out, " form=")
		strings.write_string(out, ir.abap_message_form_name(payload.message_form))
	}
	if payload.message_id != "" {
		strings.write_string(out, " id=")
		strings.write_string(out, payload.message_id)
	}
	if payload.message_type != "" {
		strings.write_string(out, " type=")
		strings.write_string(out, payload.message_type)
	}
	if payload.message_number != "" {
		strings.write_string(out, " number=")
		strings.write_string(out, payload.message_number)
	}
	if payload.message_head_operands > 0 {
		strings.write_string(out, " head_operands=")
		strings.write_int(out, payload.message_head_operands)
	}
	if payload.message_arg_count > 0 {
		strings.write_string(out, " args=")
		strings.write_int(out, payload.message_arg_count)
	}
	if payload.message_has_into {
		strings.write_string(out, " into")
	}
	if payload.message_has_display_like {
		strings.write_string(out, " display_like")
		if payload.message_display_like != "" {
			strings.write_byte(out, '=')
			strings.write_string(out, payload.message_display_like)
		} else if payload.message_display_like_operand {
			strings.write_string(out, "=operand")
		}
	}
	if payload.message_has_raising {
		strings.write_string(out, " raising")
		if payload.message_raising != "" {
			strings.write_byte(out, '=')
			strings.write_string(out, payload.message_raising)
		} else if payload.message_raising_operand {
			strings.write_string(out, "=operand")
		}
	}
	if payload.table_access != .Unknown {
		strings.write_string(out, " access=")
		strings.write_string(out, ir.table_access_kind_name(payload.table_access))
	}
	if payload.table_key_kind != .None {
		strings.write_string(out, " key=")
		strings.write_string(out, ir.table_key_kind_name(payload.table_key_kind))
		if payload.table_key_name != "" {
			strings.write_byte(out, ':')
			strings.write_string(out, payload.table_key_name)
		}
	}
	if payload.table_result_kind != .None {
		strings.write_string(out, " result=")
		strings.write_string(out, ir.table_result_kind_name(payload.table_result_kind))
	}
	if payload.table_source_kind != .Unknown {
		strings.write_string(out, " source=")
		strings.write_string(out, ir.table_source_kind_name(payload.table_source_kind))
	}
	if payload.table_row_type != ir.BUILTIN_TYPE_VOID {
		strings.write_string(out, " row=")
		print_type_id(out, module, payload.table_row_type)
	}
	if payload.table_component_count > 0 {
		strings.write_string(out, " components=")
		strings.write_int(out, payload.table_component_count)
	}
	if payload.table_binary_search {
		strings.write_string(out, " binary_search")
	}
	if payload.table_stable {
		strings.write_string(out, " stable")
	}
	if payload.sql_source_kind != .Unknown {
		strings.write_string(out, " source=")
		strings.write_string(out, ir.sql_source_kind_name(payload.sql_source_kind))
		if payload.sql_source_name != "" {
			strings.write_byte(out, ':')
			strings.write_string(out, payload.sql_source_name)
		}
	}
	if payload.sql_source_alias != "" {
		strings.write_string(out, " alias=")
		strings.write_string(out, payload.sql_source_alias)
	}
	if payload.sql_result_kind != .None {
		strings.write_string(out, " result=")
		strings.write_string(out, ir.sql_result_kind_name(payload.sql_result_kind))
	}
	if payload.sql_row_type != ir.BUILTIN_TYPE_VOID {
		strings.write_string(out, " row=")
		print_type_id(out, module, payload.sql_row_type)
	}
	if payload.sql_scalar_type != ir.BUILTIN_TYPE_VOID {
		strings.write_string(out, " scalar=")
		print_type_id(out, module, payload.sql_scalar_type)
	}
	if payload.sql_source_count > 0 {
		strings.write_string(out, " sources=")
		strings.write_int(out, payload.sql_source_count)
	}
	if payload.sql_projection_count > 0 {
		strings.write_string(out, " projections=")
		strings.write_int(out, payload.sql_projection_count)
	}
	if payload.sql_assignment_count > 0 {
		strings.write_string(out, " assignments=")
		strings.write_int(out, payload.sql_assignment_count)
	}
	if payload.sql_single {
		strings.write_string(out, " single")
	}
	if payload.sql_distinct {
		strings.write_string(out, " distinct")
	}
	if payload.sql_for_all_entries {
		strings.write_string(out, " for_all_entries")
	}
	if payload.sql_from_table {
		strings.write_string(out, " from_table")
	}
	if ir.print_op_call_target(callback.op_kind) && payload.call_target != nil && payload.call_target.name != "" {
		strings.write_string(out, " target=")
		strings.write_string(out, payload.call_target.name)
	}
}

print_instruction_operands :: proc(
	out: ^strings.Builder,
	function: ^Function,
	instruction: Instruction,
) {
	strings.write_byte(out, '(')
	print_register_pool_range(
		out,
		function,
		function.operand_registers[:],
		instruction.operand_start,
		instruction.operand_count,
		false,
		false,
	)
	strings.write_byte(out, ')')
}

instruction_result_type :: proc(function: ^Function, instruction: Instruction) -> string {
	if instruction.result_count == 0 {
		return ""
	}
	index := int(instruction.result_start)
	if index >= len(function.result_registers) {
		return ""
	}
	register := function.result_registers[index]
	value_index := int(register)
	if value_index < 0 || value_index >= len(function.values) {
		return ""
	}
	return function.values[value_index].type_name
}

print_edge_target :: proc(out: ^strings.Builder, function: ^Function, edge_index: u32) {
	if int(edge_index) >= len(function.edges) {
		strings.write_string(out, "^invalid()")
		return
	}
	edge := function.edges[int(edge_index)]
	print_block_ref_for_offset(out, function, edge.target)
	strings.write_byte(out, '(')
	print_register_pool_range(
		out,
		function,
		function.edge_registers[:],
		edge.arg_start,
		edge.arg_count,
		false,
		false,
	)
	strings.write_byte(out, ')')
}

print_slot_ref :: proc(out: ^strings.Builder, function: ^Function, payload: u32) {
	strings.write_string(out, "%s")
	strings.write_int(out, int(payload))
	if int(payload) < len(function.slots) && function.slots[int(payload)].name != "" {
		strings.write_byte(out, ' ')
		strings.write_string(out, function.slots[int(payload)].name)
	}
}

print_field_ref :: proc(out: ^strings.Builder, function: ^Function, payload: u32) {
	if int(payload) < len(function.fields) {
		strings.write_byte(out, '.')
		strings.write_string(out, function.fields[int(payload)])
		strings.write_byte(out, '#')
		strings.write_int(out, int(payload))
		return
	}
	strings.write_string(out, "#")
	strings.write_int(out, int(payload))
}

print_type_id :: proc(out: ^strings.Builder, module: ^Module, typ: ir.Type_Id) {
	index := int(typ)
	if index >= 0 && index < len(module.type_names) {
		strings.write_string(out, module.type_names[index])
		return
	}
	strings.write_string(out, "!invalid")
}

print_function_ref :: proc(out: ^strings.Builder, module: ^Module, id: Function_Id) {
	strings.write_byte(out, '@')
	strings.write_int(out, int(id))
	if id != INVALID_FUNCTION_ID && int(id) < len(module.functions) && module.functions[int(id)].name != "" {
		strings.write_byte(out, ' ')
		strings.write_string(out, module.functions[int(id)].name)
	}
}

print_function_name :: proc(out: ^strings.Builder, module: ^Module, id: Function_Id) {
	if id != INVALID_FUNCTION_ID && int(id) < len(module.functions) && module.functions[int(id)].name != "" {
		strings.write_string(out, module.functions[int(id)].name)
		return
	}
	strings.write_string(out, "function")
	strings.write_int(out, int(id))
}

print_block_ref_for_offset :: proc(out: ^strings.Builder, function: ^Function, offset: Block_Offset) {
	if block, ok := block_index_from_offset(function, offset); ok {
		print_block_ref(out, function, block)
		return
	}
	print_block_offset(out, offset)
}

block_index_from_offset :: proc "contextless" (function: ^Function, offset: Block_Offset) -> (int, bool) {
	for block_offset, i in function.block_offsets {
		if block_offset == offset {
			return i, true
		}
	}
	return 0, false
}

print_block_ref :: proc(out: ^strings.Builder, function: ^Function, block: int) {
	if block < 0 || block >= len(function.block_offsets) {
		strings.write_string(out, "^invalid")
		return
	}
	strings.write_string(out, "^b")
	strings.write_int(out, block)
	if block < len(function.block_debug) && function.block_debug[block].name != "" {
		strings.write_byte(out, '.')
		strings.write_string(out, function.block_debug[block].name)
	}
}

print_register_pool_range :: proc(
	out: ^strings.Builder,
	function: ^Function,
	registers: []Register,
	start: u32,
	count: u32,
	typed: bool,
	named: bool,
) {
	for i := 0; i < int(count); i += 1 {
		if i > 0 {
			strings.write_string(out, ", ")
		}
		print_register_pool_ref(out, function, registers, start + u32(i), typed, named)
	}
}

print_register_pool_ref :: proc(
	out: ^strings.Builder,
	function: ^Function,
	registers: []Register,
	index: u32,
	typed: bool,
	named: bool,
) {
	if int(index) >= len(registers) {
		print_register(out, INVALID_REGISTER)
		return
	}
	print_register_debug(out, function, registers[int(index)], typed, named)
}

print_register_debug :: proc(
	out: ^strings.Builder,
	function: ^Function,
	register: Register,
	typed: bool,
	named: bool,
) {
	print_register(out, register)
	if register == INVALID_REGISTER {
		return
	}
	index := int(register)
	if index < 0 || index >= len(function.values) {
		return
	}
	value := function.values[index]
	if named && value.name != "" {
		strings.write_byte(out, ' ')
		strings.write_string(out, value.name)
	}
	if typed && value.type_name != "" {
		strings.write_string(out, " : ")
		strings.write_string(out, value.type_name)
	}
}

print_register_range_raw :: proc(
	out: ^strings.Builder,
	registers: []Register,
	start: u32,
	count: u32,
) {
	strings.write_byte(out, '(')
	for i := 0; i < int(count); i += 1 {
		if i > 0 {
			strings.write_string(out, ", ")
		}
		index := int(start) + i
		if index < len(registers) {
			print_register(out, registers[index])
		} else {
			print_register(out, INVALID_REGISTER)
		}
	}
	strings.write_byte(out, ')')
}

print_register :: proc(out: ^strings.Builder, register: Register) {
	if register == INVALID_REGISTER {
		strings.write_string(out, "%invalid")
		return
	}
	strings.write_string(out, "%r")
	strings.write_int(out, int(register))
}

print_block_offset :: proc(out: ^strings.Builder, offset: Block_Offset) {
	if offset == INVALID_BLOCK_OFFSET {
		strings.write_string(out, "ipinvalid")
		return
	}
	strings.write_string(out, "ip")
	strings.write_int(out, int(offset))
}

print_literal_value :: proc(out: ^strings.Builder, literal: string, type_name: string) {
	if literal_type_prints_bare(type_name, literal) {
		strings.write_string(out, literal)
		return
	}
	print_literal_spelling(out, literal)
}

print_literal_spelling :: proc(out: ^strings.Builder, text: string) {
	if literal_has_delimiters(text, '\'') ||
	   literal_has_delimiters(text, '"') ||
	   literal_has_delimiters(text, '`') {
		strings.write_string(out, text)
		return
	}
	print_quoted(out, text)
}

literal_type_prints_bare :: proc "contextless" (type_name: string, literal: string) -> bool {
	if type_name == "!i" || type_name == "!predicate" || type_name == "!numeric" {
		return true
	}
	if type_name != "!string" && literal_is_simple_atom(literal) {
		return true
	}
	return false
}

literal_is_simple_atom :: proc "contextless" (literal: string) -> bool {
	if literal == "" {
		return false
	}
	for ch in literal {
		if (ch >= '0' && ch <= '9') ||
		   ch == '+' ||
		   ch == '-' ||
		   ch == '.' ||
		   ch == '_' {
			continue
		}
		if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') {
			continue
		}
		return false
	}
	return true
}

literal_has_delimiters :: proc "contextless" (text: string, delimiter: u8) -> bool {
	return len(text) >= 2 && text[0] == delimiter && text[len(text) - 1] == delimiter
}

print_source_annotation :: proc(out: ^strings.Builder, source: ir.Source_Loc) {
	if source.file == nil && source.range.end <= source.range.start {
		return
	}
	strings.write_string(out, " ; source=")
	if source.file != nil && source.file.path != "" {
		strings.write_string(out, source.file.path)
	} else {
		strings.write_string(out, "<unknown>")
	}
	if source.range.end > source.range.start {
		strings.write_byte(out, ':')
		strings.write_int(out, source.range.start)
		strings.write_string(out, "..")
		strings.write_int(out, source.range.end)
	}
}

print_quoted :: proc(out: ^strings.Builder, text: string) {
	strings.write_byte(out, '"')
	for ch in text {
		switch ch {
		case '"', '\\':
			strings.write_byte(out, '\\')
			strings.write_rune(out, ch)
		case '\n':
			strings.write_string(out, "\\n")
		case '\r':
			strings.write_string(out, "\\r")
		case '\t':
			strings.write_string(out, "\\t")
		case:
			strings.write_rune(out, ch)
		}
	}
	strings.write_byte(out, '"')
}

print_function_raw :: proc(
	out: ^strings.Builder,
	module: ^Module,
	function: ^Function,
	id: Function_Id,
	options: Print_Options,
) {
	strings.write_string(out, "bytecode @")
	if function.name != "" {
		strings.write_string(out, function.name)
	} else {
		strings.write_string(out, "function")
		strings.write_int(out, int(id))
	}
	strings.write_string(out, " registers=")
	strings.write_int(out, int(function.register_count))
	strings.write_string(out, " {\n")

	for offset, i in function.block_offsets {
		strings.write_string(out, "  block ^b")
		strings.write_int(out, i)
		if i < len(function.block_debug) && function.block_debug[i].name != "" {
			strings.write_byte(out, '.')
			strings.write_string(out, function.block_debug[i].name)
		}
		strings.write_string(out, " = ")
		print_block_offset(out, offset)
		strings.write_byte(out, '\n')
	}
	for constant, i in function.constants {
		strings.write_string(out, "  const #")
		strings.write_int(out, i)
		strings.write_string(out, " = ")
		print_quoted(out, constant)
		strings.write_byte(out, '\n')
	}
	for field, i in function.fields {
		strings.write_string(out, "  field #")
		strings.write_int(out, i)
		strings.write_string(out, " = .")
		strings.write_string(out, field)
		strings.write_byte(out, '\n')
	}
	for callback, i in function.runtime_callbacks {
		strings.write_string(out, "  callback #")
		strings.write_int(out, i)
		strings.write_string(out, " kind=")
		strings.write_string(out, runtime_callback_kind_name(callback.kind))
		strings.write_string(out, " op=")
		strings.write_string(out, ir.op_kind_name(callback.op_kind))
		if callback.name != "" {
			strings.write_string(out, " name=")
			strings.write_string(out, callback.name)
		}
		strings.write_byte(out, '\n')
	}
	for edge, i in function.edges {
		print_edge_raw(out, function, edge, i)
	}
	for instruction, i in function.instructions {
		print_instruction_raw(out, module, function, instruction, i, options)
	}
	strings.write_string(out, "}\n")
}

print_edge_raw :: proc(
	out: ^strings.Builder,
	function: ^Function,
	edge: Edge,
	index: int,
) {
	strings.write_string(out, "  edge #")
	strings.write_int(out, index)
	strings.write_string(out, " target=")
	print_block_offset(out, edge.target)
	strings.write_string(out, " args=")
	print_register_range_raw(out, function.edge_registers[:], edge.arg_start, edge.arg_count)
	strings.write_string(out, " params=")
	print_register_range_raw(out, function.edge_registers[:], edge.param_start, edge.param_count)
	strings.write_byte(out, '\n')
}

print_instruction_raw :: proc(
	out: ^strings.Builder,
	module: ^Module,
	function: ^Function,
	instruction: Instruction,
	ip: int,
	options: Print_Options,
) {
	strings.write_string(out, "  ")
	strings.write_int(out, ip)
	strings.write_string(out, ": ")
	strings.write_string(out, op_name(instruction.op))
	if instruction.dst != INVALID_REGISTER {
		strings.write_string(out, " dst=")
		print_register(out, instruction.dst)
	}
	if instruction.src0 != INVALID_REGISTER {
		strings.write_string(out, " src0=")
		print_register(out, instruction.src0)
	}
	if instruction.src1 != INVALID_REGISTER {
		strings.write_string(out, " src1=")
		print_register(out, instruction.src1)
	}
	print_instruction_payload_raw(out, module, function, instruction)
	if instruction.operand_count > 0 {
		strings.write_string(out, " operands=")
		print_register_range_raw(
			out,
			function.operand_registers[:],
			instruction.operand_start,
			instruction.operand_count,
		)
	}
	if instruction.result_count > 0 {
		strings.write_string(out, " results=")
		print_register_range_raw(
			out,
			function.result_registers[:],
			instruction.result_start,
			instruction.result_count,
		)
	}
	if instruction.edge_count == 1 {
		strings.write_string(out, " edge=#")
		strings.write_int(out, int(instruction.edge_start))
	} else if instruction.edge_count > 1 {
		strings.write_string(out, " edges=#")
		strings.write_int(out, int(instruction.edge_start))
		strings.write_string(out, "..#")
		strings.write_int(out, int(instruction.edge_start + instruction.edge_count - 1))
	}
	if options.show_source {
		print_source_annotation(out, instruction.source)
	}
	strings.write_byte(out, '\n')
}

print_instruction_payload_raw :: proc(
	out: ^strings.Builder,
	module: ^Module,
	function: ^Function,
	instruction: Instruction,
) {
	switch instruction.op {
	case .Const:
		strings.write_string(out, " const=#")
		strings.write_int(out, int(instruction.payload))
		if int(instruction.payload) < len(function.constants) {
			strings.write_byte(out, ' ')
			print_quoted(out, function.constants[int(instruction.payload)])
		}
	case .Load, .Store:
		strings.write_string(out, " slot=%s")
		strings.write_int(out, int(instruction.payload))
	case .Field_Load, .Field_Store:
		strings.write_string(out, " field=#")
		strings.write_int(out, int(instruction.payload))
		if int(instruction.payload) < len(function.fields) {
			strings.write_string(out, " .")
			strings.write_string(out, function.fields[int(instruction.payload)])
		}
	case .Cast:
		strings.write_string(out, " type=")
		print_type_id(out, module, ir.Type_Id(instruction.payload))
	case .Call:
		strings.write_string(out, " function=")
		print_function_ref(out, module, Function_Id(instruction.payload))
	case .Call_Runtime:
		strings.write_string(out, " callback=#")
		strings.write_int(out, int(instruction.payload))
		if int(instruction.payload) < len(function.runtime_callbacks) {
			strings.write_byte(out, ' ')
			strings.write_string(out, function.runtime_callbacks[int(instruction.payload)].name)
		}
	case .Branch:
		strings.write_string(out, " target=")
		print_block_offset(out, instruction.target)
	case .Cond_Branch:
		strings.write_string(out, " true=")
		print_block_offset(out, instruction.target)
		strings.write_string(out, " false=")
		print_block_offset(out, Block_Offset(instruction.payload))
	case .Return:
		strings.write_string(out, " count=")
		strings.write_int(out, int(instruction.payload))
	case .Nop, .Move, .Unreachable, .Unsupported:
	}
}

op_name :: proc "contextless" (op: Op) -> string {
	switch op {
	case .Nop:
		return "nop"
	case .Const:
		return "const"
	case .Move:
		return "move"
	case .Load:
		return "load"
	case .Store:
		return "store"
	case .Field_Load:
		return "field_load"
	case .Field_Store:
		return "field_store"
	case .Cast:
		return "cast"
	case .Call:
		return "call"
	case .Call_Runtime:
		return "call_runtime"
	case .Branch:
		return "branch"
	case .Cond_Branch:
		return "cond_branch"
	case .Return:
		return "return"
	case .Unreachable:
		return "unreachable"
	case .Unsupported:
		return "unsupported"
	}
	unreachable()
}

runtime_callback_kind_name :: proc "contextless" (kind: Runtime_Callback_Kind) -> string {
	switch kind {
	case .Abap:
		return "abap"
	case .Call:
		return "call"
	case .Table:
		return "table"
	case .Sql:
		return "sql"
	case .System_Field:
		return "system_field"
	case .Unsupported:
		return "unsupported"
	}
	unreachable()
}
