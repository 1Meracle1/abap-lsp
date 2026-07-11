package abap_frontend_ir

import "src:parser"
import semantic "src:semantic"
import "src:tokenizer"

import "core:strings"
import "core:testing"

expect_slot_address_attrs :: proc(t: ^testing.T, attrs: Instruction_Attrs) -> Slot_Address_Attrs {
	value, ok := attrs.(Slot_Address_Attrs)
	testing.expect(t, ok)
	return value
}

expect_projection_attrs :: proc(t: ^testing.T, attrs: Instruction_Attrs) -> Projection_Id {
	value, ok := attrs.(Projection_Id)
	testing.expect(t, ok)
	return value
}

projection_attrs :: proc "contextless" (attrs: Instruction_Attrs) -> (Projection_Id, bool) {
	value, ok := attrs.(Projection_Id)
	return value, ok
}

expect_address_def :: proc(t: ^testing.T, function: ^Function, op: ^Op, operand_index: int) -> ^Op {
	testing.expect(t, op != nil)
	if op == nil || operand_index < 0 || operand_index >= len(op.operands) {
		testing.expect(t, false)
		return nil
	}
	address := op.operands[operand_index]
	testing.expect(t, address != INVALID_VALUE_ID && int(address) < len(function.values))
	if address == INVALID_VALUE_ID || int(address) >= len(function.values) {
		return nil
	}
	value := value_ptr(function, address)
	testing.expect(t, value.op != INVALID_OP_ID)
	if value.op == INVALID_OP_ID {
		return nil
	}
	return op_ptr(function, value.op)
}

expect_memory_slot :: proc(t: ^testing.T, function: ^Function, op: ^Op) -> (Slot_Id, bool) {
	address_op := expect_address_def(t, function, op, 1)
	if address_op == nil {
		return INVALID_SLOT_ID, false
	}
	testing.expect_value(t, address_op.opcode, Opcode.Addr_Of)
	attrs := expect_slot_address_attrs(t, address_op.attrs)
	return attrs.slot, address_op.opcode == .Addr_Of
}

expect_field_projection :: proc(t: ^testing.T, function: ^Function, op: ^Op) -> Projection_Id {
	address_op := op
	if op != nil && op.opcode != .Field_Addr {
		address_op = expect_address_def(t, function, op, 1)
	}
	testing.expect(t, address_op != nil)
	if address_op == nil {
		return INVALID_PROJECTION_ID
	}
	testing.expect_value(t, address_op.opcode, Opcode.Field_Addr)
	return expect_projection_attrs(t, address_op.attrs)
}

projection_last_field_name :: proc(function: ^Function, projection: Projection_Id) -> string {
	if projection == INVALID_PROJECTION_ID || int(projection) >= len(function.projections) {
		return ""
	}
	path := projection_ptr(function, projection)
	if len(path.segments) == 0 {
		return ""
	}
	return path.segments[len(path.segments) - 1].name
}

expect_intrinsic :: proc(t: ^testing.T, module: ^Module, op: ^Op) -> Intrinsic {
	testing.expect(t, op != nil)
	if op == nil {
		return {}
	}
	testing.expect(t, op.opcode == .Intrinsic || op.opcode == .Invoke)
	testing.expect(t, op.intrinsic != INVALID_INTRINSIC_ID)
	if op.intrinsic == INVALID_INTRINSIC_ID || int(op.intrinsic) >= len(module.intrinsics) {
		return {}
	}
	return module.intrinsics[int(op.intrinsic)]
}

expect_call_payload :: proc(t: ^testing.T, module: ^Module, op: ^Op) -> Intrinsic_Call_Payload {
	value, ok := expect_intrinsic(t, module, op).payload.(Intrinsic_Call_Payload)
	testing.expect(t, ok)
	return value
}

expect_message_payload :: proc(t: ^testing.T, module: ^Module, op: ^Op) -> Intrinsic_Message_Payload {
	value, ok := expect_intrinsic(t, module, op).payload.(Intrinsic_Message_Payload)
	testing.expect(t, ok)
	return value
}

expect_exception_payload :: proc(t: ^testing.T, module: ^Module, op: ^Op) -> Intrinsic_Exception_Payload {
	value, ok := expect_intrinsic(t, module, op).payload.(Intrinsic_Exception_Payload)
	testing.expect(t, ok)
	return value
}

expect_string_payload :: proc(t: ^testing.T, module: ^Module, op: ^Op) -> Intrinsic_String_Payload {
	value, ok := expect_intrinsic(t, module, op).payload.(Intrinsic_String_Payload)
	testing.expect(t, ok)
	return value
}

expect_table_payload :: proc(t: ^testing.T, module: ^Module, op: ^Op) -> Intrinsic_Table_Payload {
	value, ok := expect_intrinsic(t, module, op).payload.(Intrinsic_Table_Payload)
	testing.expect(t, ok)
	return value
}

expect_sql_payload :: proc(t: ^testing.T, module: ^Module, op: ^Op) -> Intrinsic_SQL_Payload {
	value, ok := expect_intrinsic(t, module, op).payload.(Intrinsic_SQL_Payload)
	testing.expect(t, ok)
	return value
}

expect_system_payload :: proc(t: ^testing.T, module: ^Module, op: ^Op) -> Intrinsic_System_Field_Payload {
	value, ok := expect_intrinsic(t, module, op).payload.(Intrinsic_System_Field_Payload)
	testing.expect(t, ok)
	return value
}

expect_unsupported_attrs :: proc(t: ^testing.T, attrs: Instruction_Attrs) -> Unsupported_Attrs {
	value, ok := attrs.(Unsupported_Attrs)
	testing.expect(t, ok)
	return value
}

test_block_op :: proc(function: ^Function, block_id: Block_Id, index: int) -> ^Op {
	block := block_ptr(function, block_id)
	assert(index >= 0 && index < len(block.instructions))
	return op_ptr(function, Op_Id(block.instructions[index]))
}

test_block_terminator :: proc(function: ^Function, block_id: Block_Id) -> ^Instruction {
	block := block_ptr(function, block_id)
	assert(block.terminator != INVALID_INSTRUCTION_ID)
	return op_ptr(function, Op_Id(block.terminator))
}

test_instruction_is_regular :: proc(function: ^Function, op: ^Op) -> bool {
	if function == nil || op == nil || op.parent == INVALID_BLOCK_ID || int(op.parent) >= len(function.blocks) {
		return false
	}
	block := block_ptr(function, op.parent)
	return block.terminator != Instruction_Id(op.id)
}

test_call_operand_count :: proc "contextless" (op: ^Op) -> int {
	if op == nil {
		return 0
	}
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

// Builder / verifier invariant fixtures.

@(test)
builder_verifier_creates_valid_block_ssa_function :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "main")
	function := builder_function(&builder)
	slot := function_add_slot(function, .Local, "lv_value", BUILTIN_TYPE_INTEGER)
	one := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	builder_emit_store(&builder, slot, one)
	builder_emit_load(&builder, slot)
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, verify.ok)

	text := print_module(&module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "func @main"))
	testing.expect(t, strings.contains(text, "addr_of %s0"))
	testing.expect(t, strings.contains(text, "store ("))
	testing.expect(t, strings.contains(text, "cf.return"))
}

// Printer snapshot fixtures.

@(test)
printer_snapshot_hand_built_store_and_return_is_stable :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "main")
	function := builder_function(&builder)
	slot := function_add_slot(function, .Local, "lv_value", BUILTIN_TYPE_INTEGER)
	one := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	builder_emit_store(&builder, slot, one)
	builder_emit_load(&builder, slot)
	builder_set_return_world(&builder)

	expect_module_print_snapshot(
		t,
		&module,
`func @main -> (world) {
  slot %s0 local lv_value : i
^b0.entry(%v0 world : world):
  %v1 : i = const 1
  %v2 : ref = addr_of %s0
  %v3 : world = store (%v0, %v2, %v1)
  %v4 : ref = addr_of %s0
  %v5 : i = load (%v3, %v4)
  cf.return(%v3)
}
`,
	)
}

@(test)
printer_snapshot_constants_use_type_appropriate_literal_syntax :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "literals")
	builder_emit_const(&builder, "42", BUILTIN_TYPE_INTEGER)
	builder_emit_const(&builder, "'single quoted'", BUILTIN_TYPE_STRING)
	builder_emit_const(&builder, `template segment`, BUILTIN_TYPE_STRING)
	builder_set_return_world(&builder)

	expect_module_print_snapshot(
		t,
		&module,
`func @literals -> (world) {
^b0.entry(%v0 world : world):
  %v1 : i = const 42
  %v2 : string = const 'single quoted'
  %v3 : string = const "template segment"
  cf.return(%v0)
}
`,
	)
}

@(test)
printer_snapshot_unsupported_operation_keeps_message_and_flag :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "unsupported")
	source := Source_Loc{range = tokenizer.text_range(1, 5)}
	builder_emit_unsupported(&builder, "not yet", BUILTIN_TYPE_INTEGER, source)
	builder_set_return_world(&builder)

	expect_module_print_snapshot(
		t,
		&module,
`func @unsupported -> (world) {
^b0.entry(%v0 world : world):
  %v1 : world, %v2 : i = unsupported "not yet" (%v0) [unsupported]
  cf.return(%v1)
}
`,
	)
}

@(test)
canonical_block_arguments_and_successor_edges_are_verified :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "join_args")
	join := builder_add_world_block(&builder, "join")
	carried := function_add_block_param(builder_function(&builder), join, BUILTIN_TYPE_INTEGER, "carried")
	one := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	args := [?]Value_Id{builder.current_world, one}
	builder_set_branch(&builder, join, args[:])

	builder_position_at_end(&builder, join)
	returns := [?]Value_Id{builder.current_world}
	builder_set_return(&builder, returns[:])

	function := builder_function(&builder)
	entry := block_ptr(function, Block_Id(0))
	entry_term := test_block_terminator(function, entry.id)
	testing.expect_value(t, len(block_ptr(function, join).args), 2)
	testing.expect_value(t, block_ptr(function, join).args[1], carried)
	testing.expect_value(t, len(entry_term.successors), 1)
	testing.expect_value(t, entry_term.successors[0].target, join)
	testing.expect_value(t, entry_term.successors[0].args[1], one)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, verify.ok)
}

@(test)
canonical_use_list_mutation_updates_old_and_new_operands :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "uses")
	one := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	two := builder_emit_const(&builder, "2", BUILTIN_TYPE_INTEGER)
	three := builder_emit_const(&builder, "3", BUILTIN_TYPE_INTEGER)
	inputs := [?]Value_Id{one, two}
	result_types := [?]Type_Id{BUILTIN_TYPE_INTEGER}
	add := builder_emit_intrinsic(&builder, .ABAP_Add, inputs[:], result_types[:])
	builder_set_return_world(&builder)

	function := builder_function(&builder)
	op := op_ptr(function, add)
	testing.expect_value(t, value_ptr(function, two).use_count, u32(1))
	testing.expect(t, instruction_set_operand(function, Instruction_Id(add), 1, three))
	testing.expect_value(t, op.operands[1], three)
	testing.expect_value(t, function.uses[int(op.operand_uses[1])].value, three)
	testing.expect_value(t, value_ptr(function, two).use_count, u32(0))
	testing.expect_value(t, value_ptr(function, three).use_count, u32(1))
}

@(test)
verifier_and_query_reject_cyclic_use_lists :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "cyclic_uses")
	one := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	inputs := [?]Value_Id{one, one}
	results := [?]Type_Id{BUILTIN_TYPE_INTEGER}
	op_id := builder_emit_op(&builder, .Add, inputs[:], results[:])
	builder_set_return_world(&builder)

	function := builder_function(&builder)
	op := op_ptr(function, op_id)
	use := op.operand_uses[0]
	function.uses[int(use)].next_for_value = use

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Use_List, "value use list contains a cycle"))

	uses := make([dynamic]Use_Id, context.allocator)
	defer delete(uses)
	testing.expect(t, !value_uses(function, one, &uses))
}

@(test)
verifier_rejects_world_values_in_pure_ssa_operations :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "world_laundering")
	condition := builder_emit_const(&builder, "1", BUILTIN_TYPE_PREDICATE)
	function := builder_function(&builder)
	operands := [?]Value_Id{condition, function.world_param, function.world_param}
	results := [?]Type_Id{BUILTIN_TYPE_WORLD}
	op_id := builder_emit_op(&builder, .Select, operands[:], results[:])
	laundered := op_ptr(function, op_id).results[0]
	builder.current_world = laundered
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_World_Chain, "world token appears in a non-effect operand position"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_World_Chain, "world token appears in a non-effect result position"))
}

@(test)
module_add_type_clones_owned_type_data :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	name := strings.clone("pair", context.allocator)
	field_name := strings.clone("value", context.allocator)
	fields := make([]Aggregate_Field, 1, context.allocator)
	fields[0] = Aggregate_Field{name = field_name, type = BUILTIN_TYPE_INTEGER}
	type_id := module_add_type(&module, Type{kind = .Struct, name = name, data = Struct_Type_Data{fields = fields}})
	delete(name, context.allocator)
	delete(field_name, context.allocator)
	delete(fields, context.allocator)

	typ := type_ptr(&module, type_id)
	testing.expect_value(t, typ.name, "pair")
	data, ok := typ.data.(Struct_Type_Data)
	testing.expect(t, ok)
	if ok {
		testing.expect_value(t, data.fields[0].name, "value")
	}
}

@(test)
canonical_instruction_typing_rejects_mismatched_binary_operands :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "typing")
	one := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	text := builder_emit_const(&builder, "'x'", BUILTIN_TYPE_STRING)
	inputs := [?]Value_Id{one, text}
	result_types := [?]Type_Id{BUILTIN_TYPE_INTEGER}
	builder_emit_intrinsic(&builder, .ABAP_Add, inputs[:], result_types[:])
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_kind(&verify, .Bad_Op_Signature))
}

@(test)
canonical_arithmetic_rejects_unknown_types :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)
	builder := builder_begin_function(&module, "unknown_arithmetic")
	left := builder_emit_const(&builder, "1", BUILTIN_TYPE_UNKNOWN)
	right := builder_emit_const(&builder, "2", BUILTIN_TYPE_UNKNOWN)
	operands := [?]Value_Id{left, right}
	results := [?]Type_Id{BUILTIN_TYPE_UNKNOWN}
	builder_emit_intrinsic(&builder, .ABAP_Add, operands[:], results[:])
	builder_set_return_world(&builder)
	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_kind(&verify, .Bad_Op_Signature))
}

@(test)
canonical_intrinsic_signature_and_effects_are_validated :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "intrinsic")
	text := builder_emit_const(&builder, "'hello'", BUILTIN_TYPE_STRING)
	inputs := [?]Value_Id{text}
	builder_emit_write(&builder, inputs[:])
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, verify.ok)

	function := builder_function(&builder)
	write_op := test_block_op(function, builder.block, 1)
	module.intrinsics[int(write_op.intrinsic)].effects = {}
	verify_bad := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify_bad)
	testing.expect(t, !verify_bad.ok)
	testing.expect(t, verify_has_kind(&verify_bad, .Bad_Intrinsic))
}

@(test)
canonical_memory_alias_metadata_is_validated :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "memory")
	function := builder_function(&builder)
	slot := function_add_slot(function, .Local, "lv_value", BUILTIN_TYPE_INTEGER)
	load := builder_emit_load(&builder, slot)
	builder_set_return_world(&builder)

	op := op_ptr(function, value_ptr(function, load).op)
	alias := module_add_alias_class(&module, "locals")
	scope := module_add_effect_scope(&module, "frame", BUILTIN_TYPE_INTEGER)
	op.memory[0].alias_class = alias
	op.memory[0].scope = scope
	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, verify.ok)

	op.memory[0].alias_class = Alias_Class_Id(999)
	verify_bad := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify_bad)
	testing.expect(t, !verify_bad.ok)
	testing.expect(t, verify_has_kind(&verify_bad, .Bad_Memory_Alias))
}

@(test)
canonical_printing_is_deterministic :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "stable")
	one := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	two := builder_emit_const(&builder, "2", BUILTIN_TYPE_INTEGER)
	inputs := [?]Value_Id{one, two}
	result_types := [?]Type_Id{BUILTIN_TYPE_INTEGER}
	builder_emit_intrinsic(&builder, .ABAP_Add, inputs[:], result_types[:])
	builder_set_return_world(&builder)

	first := print_module(&module, context.allocator)
	defer delete(first, context.allocator)
	second := print_module(&module, context.allocator)
	defer delete(second, context.allocator)
	testing.expect_value(t, first, second)
	testing.expect(t, strings.contains(first, "abap.add"))
}

verify_has_kind :: proc(result: ^Verify_Result, kind: Verify_Diagnostic_Kind) -> bool {
	for diagnostic in result.diagnostics {
		if diagnostic.kind == kind {
			return true
		}
	}
	return false
}

// Inspection fixtures.

Inspection_Walk_Counts :: struct {
	functions:      int,
	blocks:       int,
	ops:         int,
	terminators:     int,
	first_op_seen:    bool,
	first_op:      Opcode,
	last_op:       Opcode,
	last_terminator:   Opcode,
	entry_block_visited: bool,
}

inspection_walk_visit_function :: proc(
	visitor: ^Walk_Visitor,
	_: ^Module,
	_: Function_Id,
	function: ^Function,
) -> bool {
	counts := cast(^Inspection_Walk_Counts)visitor.data
	counts.functions += 1
	if function.name == "main" {
		counts.entry_block_visited = counts.entry_block_visited || function.entry == Block_Id(0)
	}
	return true
}

inspection_walk_visit_block :: proc(
	visitor: ^Walk_Visitor,
	_: ^Module,
	_: Function_Id,
	_: ^Function,
	block_id: Block_Id,
	block: ^Block,
) -> bool {
	counts := cast(^Inspection_Walk_Counts)visitor.data
	counts.blocks += 1
	if block_id == Block_Id(0) && block.name == "entry" {
		counts.entry_block_visited = true
	}
	return true
}

inspection_walk_visit_op :: proc(
	visitor: ^Walk_Visitor,
	_: ^Module,
	_: Function_Id,
	_: ^Function,
	_: Block_Id,
	_: ^Block,
	op: ^Op,
) -> bool {
	counts := cast(^Inspection_Walk_Counts)visitor.data
	counts.ops += 1
	if !counts.first_op_seen {
		counts.first_op = op.opcode
		counts.first_op_seen = true
	}
	counts.last_op = op.opcode
	return true
}

inspection_walk_visit_terminator :: proc(
	visitor: ^Walk_Visitor,
	_: ^Module,
	_: Function_Id,
	_: ^Function,
	_: Block_Id,
	_: ^Block,
	term: ^Instruction,
) -> bool {
	counts := cast(^Inspection_Walk_Counts)visitor.data
	counts.terminators += 1
	counts.last_terminator = term.opcode
	return true
}

@(test)
walk_module_visits_functions_blocks_ops_and_terminators :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "main")
	function := builder_function(&builder)
	slot := function_add_slot(function, .Local, "lv_value", BUILTIN_TYPE_INTEGER)
	one := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	builder_emit_store(&builder, slot, one)
	builder_emit_load(&builder, slot)
	builder_set_return_world(&builder)

	counts := Inspection_Walk_Counts{}
	visitor := Walk_Visitor {
		visit_function  = inspection_walk_visit_function,
		visit_block   = inspection_walk_visit_block,
		visit_op     = inspection_walk_visit_op,
		visit_terminator = inspection_walk_visit_terminator,
		data       = rawptr(&counts),
	}
	testing.expect(t, walk_module(&visitor, &module))
	testing.expect_value(t, counts.functions, 1)
	testing.expect_value(t, counts.blocks, 1)
	testing.expect_value(t, counts.ops, 5)
	testing.expect_value(t, counts.terminators, 1)
	testing.expect(t, counts.entry_block_visited)
	testing.expect_value(t, counts.first_op, Opcode.Const)
	testing.expect_value(t, counts.last_op, Opcode.Load)
	testing.expect_value(t, counts.last_terminator, Opcode.Return)
}

Inspection_Stop_Counts :: struct {
	functions: int,
	ops:    int,
	stopped:  bool,
}

inspection_stop_visit_function :: proc(
	visitor: ^Walk_Visitor,
	_: ^Module,
	_: Function_Id,
	_: ^Function,
) -> bool {
	counts := cast(^Inspection_Stop_Counts)visitor.data
	counts.functions += 1
	return true
}

inspection_stop_visit_op :: proc(
	visitor: ^Walk_Visitor,
	_: ^Module,
	_: Function_Id,
	_: ^Function,
	_: Block_Id,
	_: ^Block,
	op: ^Op,
) -> bool {
	counts := cast(^Inspection_Stop_Counts)visitor.data
	counts.ops += 1
	if .Unsupported in op.effects {
		counts.stopped = true
		return false
	}
	return true
}

@(test)
walk_module_stops_when_callback_returns_false :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	first := builder_begin_function(&module, "first")
	builder_emit_unsupported(&first, "stop here")
	builder_set_return_world(&first)

	second := builder_begin_function(&module, "second")
	builder_emit_const(&second, "1", BUILTIN_TYPE_INTEGER)
	builder_set_return_world(&second)

	counts := Inspection_Stop_Counts{}
	visitor := Walk_Visitor {
		visit_function = inspection_stop_visit_function,
		visit_op    = inspection_stop_visit_op,
		data      = rawptr(&counts),
	}
	testing.expect(t, !walk_module(&visitor, &module))
	testing.expect(t, counts.stopped)
	testing.expect_value(t, counts.functions, 1)
	testing.expect_value(t, counts.ops, 1)
}

@(test)
inspection_queries_return_locations_definitions_slots_and_sources :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	entry_source := Source_Loc{range = tokenizer.text_range(0, 4)}
	const_source := Source_Loc{range = tokenizer.text_range(10, 11)}
	slot_source := Source_Loc{range = tokenizer.text_range(20, 28)}
	return_source := Source_Loc{range = tokenizer.text_range(30, 37)}

	builder := builder_begin_function(&module, "main", source = entry_source)
	function := builder_function(&builder)
	slot := function_add_slot(function, .Local, "lv_value", BUILTIN_TYPE_INTEGER, source = slot_source)
	value := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER, source = const_source)
	builder_set_return_world(&builder, source = return_source)

	def, def_ok := function_value_definition(function, value)
	testing.expect(t, def_ok)
	testing.expect_value(t, def.kind, Value_Kind.Op_Result)
	testing.expect_value(t, def.block, function.entry)
	testing.expect_value(t, def.op, Op_Id(0))
	testing.expect_value(t, def.op_location.block, function.entry)
	testing.expect_value(t, def.op_location.index, u32(0))
	testing.expect_value(t, def.result_index, u32(0))

	loc, loc_ok := function_op_location(function, def.op)
	testing.expect(t, loc_ok)
	testing.expect_value(t, loc.block, function.entry)
	testing.expect_value(t, loc.index, u32(0))

	op, op_ok := function_op_record(function, def.op)
	testing.expect(t, op_ok)
	testing.expect_value(t, op.opcode, Opcode.Const)

	value_source, value_source_ok := function_value_source(function, value)
	testing.expect(t, value_source_ok)
	testing.expect_value(t, value_source.range.start, const_source.range.start)
	testing.expect_value(t, value_source.range.end, const_source.range.end)

	world_def, world_def_ok := function_value_definition(function, function.world_param)
	testing.expect(t, world_def_ok)
	testing.expect_value(t, world_def.kind, Value_Kind.Block_Param)
	testing.expect_value(t, world_def.block, function.entry)
	testing.expect_value(t, world_def.block_param_index, u32(0))

	world_source, world_source_ok := function_value_source(function, function.world_param)
	testing.expect(t, world_source_ok)
	testing.expect_value(t, world_source.range.start, entry_source.range.start)
	testing.expect_value(t, world_source.range.end, entry_source.range.end)

	slot_record, slot_ok := function_slot_record(function, slot)
	testing.expect(t, slot_ok)
	testing.expect_value(t, slot_record.name, "lv_value")

	queried_slot_source, slot_source_ok := function_slot_source(function, slot)
	testing.expect(t, slot_source_ok)
	testing.expect_value(t, queried_slot_source.range.start, slot_source.range.start)
	testing.expect_value(t, queried_slot_source.range.end, slot_source.range.end)

	_, missing_op_ok := function_op_location(function, INVALID_OP_ID)
	testing.expect(t, !missing_op_ok)
	_, missing_value_ok := function_value_definition(function, Value_Id(999))
	testing.expect(t, !missing_value_ok)
}

// Builder / verifier malformed-module fixtures.

@(test)
builder_verifier_rejects_missing_terminator_bad_branch_and_world_chain :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	missing := builder_begin_function(&module, "missing")
	builder_emit_const(&missing, "1", BUILTIN_TYPE_INTEGER)

	bad_branch := builder_begin_function(&module, "bad_branch")
	target := builder_add_world_block(&bad_branch, "target")
	function_add_block_param(builder_function(&bad_branch), target, BUILTIN_TYPE_INTEGER, "value")
	builder_set_branch_world(&bad_branch, target)
	builder_position_at_end(&bad_branch, target)
	builder_set_return_world(&bad_branch)

	bad_world := builder_begin_function(&module, "bad_world")
	function := builder_function(&bad_world)
	slot := function_add_slot(function, .Local, "lv", BUILTIN_TYPE_INTEGER)
	value := builder_emit_const(&bad_world, "1", BUILTIN_TYPE_INTEGER)
	builder_emit_store(&bad_world, slot, value)
	address := builder_emit_slot_address(&bad_world, slot)
	bad_world_operands := [?]Value_Id{function.world_param, address, value}
	bad_world_types := [?]Type_Id{BUILTIN_TYPE_WORLD}
	builder_emit_op(
		&bad_world,
		.Store,
		bad_world_operands[:],
		bad_world_types[:],
		effects = {.Read_Local, .Write_Local},
	)
	builder_set_return_world(&bad_world)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic(verify, .Missing_Terminator))
	testing.expect(t, verify_has_diagnostic(verify, .Bad_Terminator_Args))
	testing.expect(t, verify_has_diagnostic(verify, .Bad_World_Chain))
}

@(test)
builder_verifier_rejects_value_that_does_not_dominate_use :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "dominance")
	later := builder_add_world_block(&builder, "later")
	builder_position_at_end(&builder, later)
	later_value := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	builder_set_return_world(&builder)

	builder_position_at_end(&builder, function_ptr(&module, builder.function_id).entry)
	bad_operands := [?]Value_Id{later_value, later_value}
	bad_types := [?]Type_Id{BUILTIN_TYPE_INTEGER}
	builder_emit_intrinsic(&builder, .ABAP_Add, bad_operands[:], bad_types[:])
	builder_set_branch_world(&builder, later)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic(verify, .Dominance_Error))
}

@(test)
builder_verifier_rejects_structural_back_pointers_and_invalid_references :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	source := Source_Loc{range = tokenizer.text_range(1, 2)}

	structural := builder_begin_function(&module, "structural")
	function := builder_function(&structural)
	slot := function_add_slot(function, .Local, "lv", BUILTIN_TYPE_INTEGER, source = source)
	value := builder_emit_const(&structural, "1", BUILTIN_TYPE_INTEGER, source)
	builder_set_return_world(&structural)

	function.slots[int(slot)].type = Type_Id(999)
	function.blocks[int(function.entry)].args[0] = Value_Id(999)
	function.values[int(value)].op = INVALID_OP_ID
	function.op_locations[0].index = u32(999)

	bad_slot := builder_begin_function(&module, "bad_slot")
	result_types := [?]Type_Id{module_reference_type(&module, BUILTIN_TYPE_INTEGER)}
	builder_emit_op(
		&bad_slot,
		.Addr_Of,
		result_types = result_types[:],
		attrs = Slot_Address_Attrs{slot = Slot_Id(999)},
		source = source,
	)
	builder_set_return_world(&bad_slot)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic(verify, .Invalid_Type))
	testing.expect(t, verify_has_diagnostic(verify, .Invalid_Value))
	testing.expect(t, verify_has_diagnostic(verify, .Invalid_Function))
	testing.expect(t, verify_has_diagnostic(verify, .Bad_Op_Signature))
}

@(test)
builder_verifier_rejects_object_reference_deref_by_descriptor :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	class_ref_type := module_add_type(
		&module,
		Type {
			kind = .Reference,
			name = "ref:zagent",
			runtime = Runtime_Type_Descriptor {
				family = .Reference,
				display_name = "ref:zagent",
				reference = Runtime_Reference_Descriptor {
					kind = .Class,
					target_name = "zagent",
				},
			},
		},
	)
	builder := builder_begin_function(&module, "bad_object_deref")
	object_ref := builder_emit_const(&builder, "lo", class_ref_type)
	operands := [?]Value_Id{object_ref}
	result_types := [?]Type_Id{BUILTIN_TYPE_INTEGER}
	builder_emit_op(&builder, .Deref, operands[:], result_types[:])
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "deref operand must be a data reference"))
}

@(test)
builder_verifier_rejects_unsupported_without_message_or_source :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	with_source := Source_Loc{range = tokenizer.text_range(10, 20)}
	missing_message := builder_begin_function(&module, "missing_message")
	builder_emit_effect_op(
		&missing_message,
		.Unsupported,
		effects = {.May_Trap, .Unsupported},
		source = with_source,
	)
	builder_set_return_world(&missing_message)

	missing_source := builder_begin_function(&module, "missing_source")
	builder_emit_effect_op(
		&missing_source,
		.Unsupported,
		effects = {.May_Trap, .Unsupported},
		attrs = Unsupported_Attrs{message = "missing source"},
	)
	builder_set_return_world(&missing_source)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "unsupported instruction must carry message attrs"))
}

@(test)
builder_verifier_rejects_stale_world_branch_argument_for_nonleading_world_param :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	source := Source_Loc{range = tokenizer.text_range(1, 4)}
	builder := builder_begin_function(&module, "world_order")
	function := builder_function(&builder)
	slot := function_add_slot(function, .Local, "lv", BUILTIN_TYPE_INTEGER, source = source)
	value := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER, source)
	builder_emit_store(&builder, slot, value, source)

	target := builder_add_block(&builder, "target", source)
	function_add_block_param(function, target, BUILTIN_TYPE_INTEGER, "value")
	function_add_block_param(function, target, BUILTIN_TYPE_WORLD, "world")
	args := [?]Value_Id{value, function.world_param}
	builder_set_branch(&builder, target, args[:], source)

	builder_position_at_end(&builder, target)
	builder_set_return_world(&builder, source)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic(verify, .Bad_World_Chain))
}

@(test)
verifier_rejects_entry_signature_drift :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "entry_signature")
	builder_set_return_world(&builder)
	function := builder_function(&builder)
	function.signature.params[0] = BUILTIN_TYPE_INTEGER

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Invalid_Function, "entry block argument type must match function signature parameter"))
}

@(test)
verifier_rejects_bad_block_argument_and_instruction_id :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "mirror_drift")
	builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	builder_set_return_world(&builder)

	function := builder_function(&builder)
	entry := block_ptr(function, function.entry)
	entry.args[0] = INVALID_VALUE_ID
	entry.instructions[0] = Instruction_Id(999)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Invalid_Value, "block parameter references invalid value"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Invalid_Function, "block instruction list references invalid instruction"))
}

@(test)
verifier_rejects_select_opcode_type_errors :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "bad_select")
	condition := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	left := builder_emit_const(&builder, "2", BUILTIN_TYPE_INTEGER)
	right := builder_emit_const(&builder, "text", BUILTIN_TYPE_STRING)
	operands := [?]Value_Id{condition, left, right}
	results := [?]Type_Id{BUILTIN_TYPE_INTEGER}
	builder_emit_op(&builder, .Select, operands[:], results[:])
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "select condition must be predicate"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "select value operand types must match"))
}

@(test)
verifier_rejects_intrinsic_signature_and_attr_drift :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "intrinsic_drift")
	op_id := builder_emit_effect_intrinsic(
		&builder,
		.Call_Method,
		effects = {.Calls_IR, .May_Throw},
		payload = Intrinsic_Call_Payload {
			callee_name = "lcl_demo.run",
			call_kind = .Method,
		},
	)
	builder_set_return_world(&builder)

	function := builder_function(&builder)
	op := op_ptr(function, op_id)
	module.intrinsics[int(op.intrinsic)].signature.effects = {.Calls_IR}
	module.intrinsics[int(op.intrinsic)].signature.can_throw = false
	op.attrs = Intrinsic_Call_Attrs{intrinsic = INVALID_INTRINSIC_ID}

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "intrinsic instruction attrs must mirror intrinsic declaration id"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "intrinsic instruction effects must match signature effects"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "intrinsic call can_throw flag must match may-throw effect"))
}

@(test)
verifier_rejects_missing_memory_metadata_for_memory_effects :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "memory_metadata")
	function := builder_function(&builder)
	slot := function_add_slot(function, .Local, "lv", BUILTIN_TYPE_INTEGER)
	value := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	builder_emit_store(&builder, slot, value)
	builder_set_return_world(&builder)

	store := test_block_op(function, function.entry, 2)
	delete(store.memory)
	store.memory = nil

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Memory_Alias, "memory-effect instruction must carry memory metadata"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Memory_Alias, "store instruction must carry write memory metadata"))
}

@(test)
verifier_rejects_unsupported_may_throw_policy :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "bad_throw_policy")
	builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	op := op_ptr(builder_function(&builder), Op_Id(0))
	op.effects = {.May_Throw}
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Exception_Edge, "may-throw operation must use invoke with valid exception edges"))
}

@(test)
verifier_default_rejects_non_invoke_may_throw_intrinsic :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "default_exception_edges")
	builder_emit_exception_raise(&builder, "cx_root")
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Exception_Edge, "may-throw operation must use invoke with valid exception edges"))
}

@(test)
verifier_accepts_valid_intrinsic_invoke_exception_edges :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "invoke_exception_edges")
	normal := builder_add_world_block(&builder, "normal")
	catch := builder_add_world_block(&builder, "catch")
	builder_emit_exception_raise_invoke(&builder, "cx_root", normal, catch)
	builder_set_unreachable(&builder)

	builder_position_at_end(&builder, normal)
	builder_set_return_world(&builder)

	builder_position_at_end(&builder, catch)
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, verify.ok)

	entry := block_ptr(builder_function(&builder), Block_Id(0))
	invoke := op_ptr(builder_function(&builder), Op_Id(entry.instructions[0]))
	testing.expect_value(t, invoke.opcode, Opcode.Invoke)
	testing.expect_value(t, len(invoke.successors), 2)
	if len(invoke.successors) == 2 {
		testing.expect_value(t, invoke.successors[0].kind, Edge_Kind.Normal)
		testing.expect_value(t, invoke.successors[0].target, normal)
		testing.expect_value(t, invoke.successors[1].kind, Edge_Kind.Exception)
		testing.expect_value(t, invoke.successors[1].target, catch)
	}
}

@(test)
verifier_rejects_terminator_and_exception_edge_drift :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "edge_drift")
	function := builder_function(&builder)
	join := builder_add_world_block(&builder, "join")
	value := builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	args := [?]Value_Id{builder.current_world}
	builder_set_branch(&builder, join, args[:])
	entry := block_ptr(function, function.entry)
	entry_term := test_block_terminator(function, function.entry)
	entry_term.successors[0].args[0] = value

	builder_position_at_end(&builder, join)
	builder_set_return_world(&builder)

	op := test_block_op(function, entry.id, 0)
	append(
		&op.successors,
		Successor_Edge {
			target = join,
			args = make([dynamic]Value_Id, 0, 0, context.allocator),
			kind = .Exception,
		},
	)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Terminator_Args, "terminator successor args must mirror operand slice"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Exception_Edge, "only invoke or branch terminators may carry canonical successor edges"))
}

@(test)
verifier_rejects_invoke_without_exception_successor :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "bad_invoke")
	operands := [?]Value_Id{builder.current_world}
	results := [?]Type_Id{BUILTIN_TYPE_WORLD}
	op_id := builder_emit_op(
		&builder,
		.Invoke,
		operands[:],
		results[:],
		effects = {.Calls_IR, .May_Throw},
		attrs = Call_Attrs{target = builder.function_id},
	)
	builder.current_world = op_ptr(builder_function(&builder), op_id).results[0]
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Exception_Edge, "invoke must have exactly one normal successor"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Exception_Edge, "invoke must have exactly one exception successor"))
}

@(test)
verifier_rejects_core_call_without_attrs :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	target := builder_begin_function(&module, "callee")
	builder_set_return_world(&target)

	caller := builder_begin_function(&module, "caller")
	call := builder_emit_core_call(&caller, target.function_id)
	builder_set_return_world(&caller)

	op := op_ptr(builder_function(&caller), call)
	op.attrs = Instruction_None_Attrs{}

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "call instruction must carry call attrs"))
}

@(test)
verifier_rejects_direct_call_without_calls_ir_effect :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	target := builder_begin_function(&module, "effect_callee")
	builder_set_return_world(&target)

	caller := builder_begin_function(&module, "effect_caller")
	call := builder_emit_core_call(&caller, target.function_id)
	builder_set_return_world(&caller)

	op := op_ptr(builder_function(&caller), call)
	op.effects = {}

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "direct call instruction must carry calls-ir effect"))
}

@(test)
verifier_rejects_invoke_without_attrs :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "bad_invoke_attrs")
	operands := [?]Value_Id{builder.current_world}
	results := [?]Type_Id{BUILTIN_TYPE_WORLD}
	op_id := builder_emit_op(
		&builder,
		.Invoke,
		operands[:],
		results[:],
		effects = {.Calls_IR, .May_Throw},
	)
	builder.current_world = op_ptr(builder_function(&builder), op_id).results[0]
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "call instruction must carry call attrs"))
}

@(test)
verifier_rejects_pure_core_opcode_effects :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "pure_effect")
	builder_emit_const(&builder, "1", BUILTIN_TYPE_INTEGER)
	op := test_block_op(builder_function(&builder), builder.block, 0)
	op.effects = {.Read_Local}
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "core opcode must not carry unsupported effects"))
}

@(test)
verifier_accepts_canonical_switch :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "canonical_switch")
	selector := builder_emit_const(&builder, "2", BUILTIN_TYPE_INTEGER)
	case_value := builder_emit_const(&builder, "2", BUILTIN_TYPE_INTEGER)
	default_block := builder_add_world_block(&builder, "default")
	case_block := builder_add_world_block(&builder, "case")
	default_args := [?]Value_Id{builder.current_world}
	case_args := [?]Value_Id{builder.current_world}
	cases := [?]Switch_Case{{value = case_value, target = case_block, args = case_args[:]}}
	builder_set_switch(&builder, selector, default_block, default_args[:], cases[:])
	builder_position_at_end(&builder, default_block)
	builder_set_return_world(&builder)
	builder_position_at_end(&builder, case_block)
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, verify.ok)
	term := test_block_terminator(builder_function(&builder), builder_function(&builder).entry)
	testing.expect(t, instruction_set_operand(builder_function(&builder), term.id, 2, selector))
	testing.expect_value(t, term.successors[1].case_value, selector)
}

// Lowering snippet fixtures.

@(test)
lowering_snippet_assignment_emits_typed_load_and_store :: proc(t: ^testing.T) {
	source := `DATA lv_source TYPE i.
DATA lv_target TYPE i.
lv_target = lv_source.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "slot %s0 global lv_source : i"))
	testing.expect(t, strings.contains(text, "slot %s1 global lv_target : i"))
	testing.expect(t, strings.contains(text, "addr_of %s0"))
	testing.expect(t, strings.contains(text, "addr_of %s1"))
	testing.expect(t, strings.contains(text, "store ("))
}

@(test)
lowering_report_entry_dispatches_explicit_report_events :: proc(t: ^testing.T) {
	source := `DATA gv TYPE i.
INITIALIZATION.
 gv = 1.
START-OF-SELECTION.
 gv = gv + 1.
END-OF-SELECTION.
 gv = gv + 1.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	module := &fixture.lowered.module

	text := print_module(module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "invoke @start_of_selection"))
	testing.expect(t, !strings.contains(text, "target=f"))

	testing.expect_value(t, len(module.entries), 1)
	if len(module.entries) == 0 {
		return
	}
	entry := function_ptr(module, module.entries[0])
	testing.expect_value(t, entry.role, Function_Role.Report_Entry)
	testing.expect_value(t, lower_test_core_call_count(module, entry), 3)
	testing.expect(t, lower_test_core_call_target_name_contains(module, entry, 0, "initialization"))
	testing.expect(t, lower_test_core_call_target_name_contains(module, entry, 1, "start_of_selection"))
	testing.expect(t, lower_test_core_call_target_name_contains(module, entry, 2, "end_of_selection"))

	start, _, start_ok := lower_test_function_by_name(module, "start_of_selection")
	testing.expect(t, start_ok)
	if start_ok {
		slot, _, slot_ok := lower_test_slot_by_name(start, "gv")
		testing.expect(t, slot_ok)
		if slot_ok {
			testing.expect_value(t, slot.kind, Slot_Kind.Global)
		}
	}
}

@(test)
lowering_report_entry_splits_global_initializers_from_implicit_start :: proc(t: ^testing.T) {
	source := `DATA gv TYPE i VALUE 1.
gv = gv + 1.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	module := &fixture.lowered.module

	testing.expect_value(t, len(module.entries), 1)
	if len(module.entries) == 0 {
		return
	}
	entry := function_ptr(module, module.entries[0])
	testing.expect_value(t, lower_test_core_call_count(module, entry), 2)
	testing.expect(t, lower_test_core_call_target_name_contains(module, entry, 0, "load_globals"))
	testing.expect(t, lower_test_core_call_target_name_contains(module, entry, 1, "start_of_selection"))

	load, _, load_ok := lower_test_function_by_name_contains(module, "load_globals")
	start := lower_test_primary_source_function(module)
	testing.expect(t, load_ok)
	if load_ok {
		testing.expect_value(t, load.role, Function_Role.Load_Of_Program)
		slot, _, slot_ok := lower_test_slot_by_name(load, "gv")
		testing.expect(t, slot_ok)
		if slot_ok {
			testing.expect_value(t, slot.kind, Slot_Kind.Global)
		}
		_, store_ok := lower_test_first_opcode(load, .Store)
		testing.expect(t, store_ok)
	}
	slot, _, slot_ok := lower_test_slot_by_name(start, "gv")
	testing.expect(t, slot_ok)
	if slot_ok {
		testing.expect_value(t, slot.kind, Slot_Kind.Global)
	}
}

@(test)
lowering_report_globals_alias_across_routines_while_locals_stay_local :: proc(t: ^testing.T) {
	source := `DATA gv TYPE i.
FORM sub.
 DATA lv TYPE i.
 gv = lv.
ENDFORM.
START-OF-SELECTION.
 PERFORM sub.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	module := &fixture.lowered.module

	form, _, form_ok := lower_test_function_by_name(module, "sub")
	testing.expect(t, form_ok)
	if !form_ok {
		return
	}
	gv, _, gv_ok := lower_test_slot_by_name(form, "gv")
	lv, _, lv_ok := lower_test_slot_by_name(form, "lv")
	testing.expect(t, gv_ok)
	testing.expect(t, lv_ok)
	if gv_ok {
		testing.expect_value(t, gv.kind, Slot_Kind.Global)
	}
	if lv_ok {
		testing.expect_value(t, lv.kind, Slot_Kind.Local)
	}
}

@(test)
lowering_perform_changing_exposes_output_result_for_runtime_copy_back :: proc(t: ^testing.T) {
	source := `FORM set_total CHANGING cv_total TYPE i.
 cv_total = 5.
ENDFORM.
DATA gv_total TYPE i.
START-OF-SELECTION.
 PERFORM set_total CHANGING gv_total.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	module := &fixture.lowered.module

	start := lower_test_primary_source_function(module)
	call, call_ok := lower_test_first_intrinsic_op(module, start, .Call_Routine)
	testing.expect(t, call_ok)
	if !call_ok {
		return
	}
	call_payload := expect_call_payload(t, module, call)
	testing.expect_value(t, call_payload.call_kind, Abap_Call_Kind.Form)
	testing.expect_value(t, len(call.results), 2)
	if len(call.results) == 2 {
		testing.expect_value(t, value_type(start, call.results[1]), BUILTIN_TYPE_INTEGER)
	}

	store_found := false
	for &op in start.instructions {
		if test_instruction_is_regular(start, &op) &&
		  op.opcode == .Store &&
		  len(op.operands) > 2 &&
		  op.operands[2] == call.results[1] {
			store_found = true
		}
	}
	testing.expect(t, store_found)
}

@(test)
lowering_snippet_if_emits_predicate_branch :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
IF lv = 1.
ENDIF.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.eq"))
	testing.expect(t, strings.contains(text, "cf.cond_br"))
	testing.expect(t, strings.contains(text, ".if_then"))
	testing.expect(t, strings.contains(text, ".if_after"))
}

@(test)
lowering_snippet_covers_assignments_branches_loops_and_flow :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
lv = 1.
IF lv = 1.
 lv = lv + 1.
ELSE.
 CHECK lv = 0.
ENDIF.
WHILE lv < 3.
 lv = lv + 1.
ENDWHILE.
RETURN.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.add"))
	testing.expect(t, strings.contains(text, "cf.cond_br"))
	testing.expect(t, strings.contains(text, ".while_cond"))
	testing.expect(t, strings.contains(text, ".check_return"))
}

@(test)
lowering_snippet_covers_table_and_sql_domain_ops :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zcust,
     id TYPE i,
    END OF zcust.
DATA lt_numbers TYPE STANDARD TABLE OF i.
DATA lv_number TYPE i.
LOOP AT lt_numbers INTO lv_number.
ENDLOOP.
READ TABLE lt_numbers INTO lv_number INDEX 1.
APPEND lv_number TO lt_numbers.
SELECT SINGLE id FROM zcust INTO lv_number WHERE id = lv_number.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "table.iter"))
	testing.expect(t, strings.contains(text, "table.next"))
	testing.expect(t, strings.contains(text, "table.read"))
	testing.expect(t, strings.contains(text, "table.append"))
	testing.expect(t, strings.contains(text, "sql.select"))
	testing.expect(t, strings.contains(text, "system.write .subrc"))
}

@(test)
lowering_snippet_uses_structured_targets_for_loop_flow :: proc(t: ^testing.T) {
	source := `DATA lt_numbers TYPE STANDARD TABLE OF i.
DATA lv_number TYPE i.
LOOP AT lt_numbers INTO lv_number.
 IF lv_number = 0.
  CONTINUE.
 ELSEIF lv_number = 1.
  EXIT.
 ELSE.
  CHECK lv_number > 1.
 ENDIF.
ENDLOOP.
RETURN.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	function := lower_test_primary_source_function(&result.module)
	loop_next, loop_next_ok := lower_test_block_by_name(function, "loop_next")
	loop_after, loop_after_ok := lower_test_block_by_name(function, "loop_after")
	testing.expect(t, loop_next_ok)
	testing.expect(t, loop_after_ok)
	if loop_next_ok {
		testing.expect(t, lower_test_has_branch_to_with_arg_count(function, loop_next, 2))
	}
	if loop_after_ok {
		testing.expect(t, lower_test_has_branch_to_with_arg_count(function, loop_after, 1))
	}

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, ".loop_next"))
	testing.expect(t, strings.contains(text, ".loop_after"))
	testing.expect(t, !strings.contains(text, "CONTINUE requires loop target lowering"))
}

@(test)
lowering_snippet_models_select_body_control_region_with_deferred_iteration :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zcust,
     id TYPE i,
    END OF zcust.
DATA lv_number TYPE i.
SELECT id FROM zcust INTO lv_number.
 CHECK lv_number > 0.
 CONTINUE.
ENDSELECT.
RETURN.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	function := lower_test_primary_source_function(&result.module)
	select_next, select_next_ok := lower_test_block_by_name(function, "select_next")
	select_after, select_after_ok := lower_test_block_by_name(function, "select_after")
	testing.expect(t, select_next_ok)
	testing.expect(t, select_after_ok)
	if select_next_ok {
		testing.expect(t, lower_test_has_branch_to_with_arg_count(function, select_next, 1))
	}
	if select_after_ok {
		testing.expect(t, lower_test_has_branch_to_with_arg_count(function, select_after, 1))
	}
	testing.expect(t, lower_test_has_unsupported_with_source(function, "SELECT loop body semantics"))

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, ".select_next"))
	testing.expect(t, strings.contains(text, ".select_body"))
	testing.expect(t, strings.contains(text, ".select_after"))
	testing.expect(t, strings.contains(text, `"SELECT loop body semantics"`))
}

// Unsupported boundary fixtures.

@(test)
unsupported_boundary_deferred_control_semantics_are_source_bearing :: proc(t: ^testing.T) {
	source := `DATA lt_numbers TYPE STANDARD TABLE OF i.
DATA lv_number TYPE i.
DO 2 TIMES.
 CONTINUE.
ENDDO.
LOOP AT lt_numbers INTO lv_number FROM 1 WHERE lv_number > 0.
 EXIT.
ENDLOOP.
CONTINUE.
RETURN.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	function := lower_test_primary_source_function(&result.module)
	testing.expect(t, lower_test_has_unsupported_with_source(function, "DO loop count semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "LOOP range semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "LOOP WHERE filtering"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "CONTINUE outside structured control target"))
}

@(test)
lowering_snippet_models_create_object_direct_call_case_and_write :: proc(t: ^testing.T) {
	source := `CLASS lcl_accumulator DEFINITION.
 PUBLIC SECTION.
  METHODS add CHANGING cv_total TYPE i.
ENDCLASS.
CLASS lcl_accumulator IMPLEMENTATION.
 METHOD add.
  cv_total = cv_total + 1.
 ENDMETHOD.
ENDCLASS.
DATA lo_acc TYPE REF TO lcl_accumulator.
DATA lv_total TYPE i.
DATA lv_message TYPE string.
CREATE OBJECT lo_acc.
lo_acc->add( CHANGING cv_total = lv_total ).
CASE lv_total.
 WHEN 1 OR 2.
  lv_message = 'small'.
 WHEN OTHERS.
  lv_message = 'other'.
ENDCASE.
WRITE: / lv_total, lv_message.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.construct @new"))
	testing.expect(t, strings.contains(text, "abap.call.method @lcl_accumulator.add"))
	testing.expect(t, !strings.contains(text, "abap.call.method @lcl_accumulator.add call=method"))
	testing.expect(t, !strings.contains(text, "abap.call.method @lcl_accumulator.add target=add"))
	testing.expect(t, !strings.contains(text, "receiver="))
	testing.expect(t, strings.contains(text, ".case_when"))
	testing.expect(t, strings.contains(text, "abap.or"))
	testing.expect(t, strings.contains(text, "abap.write"))
	testing.expect(t, !strings.contains(text, "unsupported"))
}

@(test)
lowering_snippet_preserves_constructor_reference_target_type_names :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
 PUBLIC SECTION.
ENDCLASS.
CLASS lcl_class IMPLEMENTATION.
ENDCLASS.
DATA(lo_inline) = NEW lcl_class( ).
DATA lo_old TYPE REF TO lcl_class.
lo_old = NEW #( ).`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "lo_inline : ref:lcl_class"))
	testing.expect(t, strings.contains(text, "lo_old : ref:lcl_class"))
	testing.expect(t, strings.contains(text, ": ref:lcl_class = intrinsic @abap.construct @new"))

	function := lower_test_primary_source_function(&result.module)
	explicit_new_offset := strings.index(source, "NEW lcl_class")
	inferred_new_offset := strings.index(source, "NEW #")
	testing.expect(t, explicit_new_offset >= 0)
	testing.expect(t, inferred_new_offset >= 0)
	explicit_new, explicit_new_ok := lower_test_intrinsic_at_start(&result.module, function, .ABAP_Construct, explicit_new_offset)
	inferred_new, inferred_new_ok := lower_test_intrinsic_at_start(&result.module, function, .ABAP_Construct, inferred_new_offset)
	testing.expect(t, explicit_new_ok)
	testing.expect(t, inferred_new_ok)
	if explicit_new_ok && len(explicit_new.results) > 0 {
		explicit_type := type_ptr(&result.module, value_type(function, explicit_new.results[0]))
		testing.expect_value(t, explicit_type.kind, Type_Kind.Reference)
		testing.expect_value(t, explicit_type.name, "ref:lcl_class")
	}
	if inferred_new_ok && len(inferred_new.results) > 0 {
		inferred_type := type_ptr(&result.module, value_type(function, inferred_new.results[0]))
		testing.expect_value(t, inferred_type.kind, Type_Kind.Reference)
		testing.expect_value(t, inferred_type.name, "ref:lcl_class")
	}
}

@(test)
lowering_create_object_type_addition_uses_concrete_constructor_type :: proc(t: ^testing.T) {
	source := `CLASS lcl_parent DEFINITION.
ENDCLASS.
CLASS lcl_parent IMPLEMENTATION.
ENDCLASS.
CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
 PUBLIC SECTION.
  METHODS constructor.
ENDCLASS.
CLASS lcl_child IMPLEMENTATION.
 METHOD constructor.
 ENDMETHOD.
ENDCLASS.
DATA lo TYPE REF TO lcl_parent.
CREATE OBJECT lo TYPE lcl_child.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	module := &fixture.lowered.module
	function := lower_test_primary_source_function(module)

	create_offset := strings.index(source, "CREATE OBJECT")
	testing.expect(t, create_offset >= 0)
	construct, construct_ok := lower_test_intrinsic_at_start(module, function, .ABAP_Construct, create_offset)
	testing.expect(t, construct_ok)
	if construct_ok && len(construct.results) > 0 {
		construct_type := type_ptr(module, value_type(function, construct.results[0]))
		testing.expect_value(t, construct_type.kind, Type_Kind.Reference)
		testing.expect_value(t, construct_type.name, "ref:lcl_child")
	}

	constructor_call_ok := false
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		intrinsic_op, intrinsic_ok := lower_test_intrinsic_op(module, &op)
		if !intrinsic_ok || intrinsic_op != .Call_Method {
			continue
		}
		payload := expect_call_payload(t, module, &op)
		if payload.callee_name != "lcl_child.constructor" {
			continue
		}
		constructor_call_ok = true
		testing.expect(t, len(op.operands) >= 2)
		if len(op.operands) >= 2 {
			receiver_type := type_ptr(module, value_type(function, op.operands[1]))
			testing.expect_value(t, receiver_type.kind, Type_Kind.Reference)
			testing.expect_value(t, receiver_type.name, "ref:lcl_child")
		}
	}
	testing.expect(t, constructor_call_ok)
}

@(test)
unsupported_boundary_binary_lowers_to_flagged_predicate_value :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
IF lv CO '1'.
ENDIF.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "unsupported"))
	testing.expect(t, strings.contains(text, `"unsupported binary expression"`))
	testing.expect(t, strings.contains(text, "[unsupported]"))
}

@(test)
lowering_snippet_abap_bool_value_type_stays_distinct_from_predicate_type :: proc(t: ^testing.T) {
	source := `DATA lv TYPE abap_bool.
IF lv = abap_true.
ENDIF.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "global lv : abap_bool"))
	testing.expect(t, strings.contains(text, "abap.eq"))
	testing.expect(t, strings.contains(text, ": predicate"))
}

@(test)
lowering_snippet_preserves_fixed_character_type_shape :: proc(t: ^testing.T) {
	source := `DATA lv_c TYPE c LENGTH 10.
DATA lv_s TYPE string.
lv_s = lv_c.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "global lv_c : c(10)"))
	testing.expect(t, strings.contains(text, "global lv_s : string"))
}

@(test)
lowering_snippet_covers_string_templates_constructors_casts_and_builtins :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
     text TYPE string,
     count TYPE i,
    END OF ty_row.
DATA lv_name TYPE string.
DATA lv_count TYPE i.
DATA ls_row TYPE ty_row.
lv_name = |hello { lv_count WIDTH = 3 }|.
lv_count = strlen( lv_name ).
ls_row = VALUE ty_row( text = |{ lv_name }| count = CONV i( lv_count ) ).`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.string_template"))
	testing.expect(t, strings.contains(text, "abap.call.builtin @strlen"))
	testing.expect(t, strings.contains(text, "abap.construct @value"))
	testing.expect(t, strings.contains(text, "cast"))

	function := lower_test_primary_source_function(&result.module)
	template_offset := strings.index(source, "|hello")
	strlen_offset := strings.index(source, "strlen")
	value_offset := strings.index(source, "VALUE")
	conv_offset := strings.index(source, "CONV")
	testing.expect(t, template_offset >= 0)
	testing.expect(t, strlen_offset >= 0)
	testing.expect(t, value_offset >= 0)
	testing.expect(t, conv_offset >= 0)

	template_op, template_ok := lower_test_intrinsic_at_start(&result.module, function, .ABAP_String_Template, template_offset)
	builtin_op, builtin_ok := lower_test_first_intrinsic_op(&result.module, function, .Call_Builtin)
	construct_op, construct_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Construct)
	cast_op, cast_ok := lower_test_first_opcode(function, .Cast)
	testing.expect(t, template_ok)
	testing.expect(t, builtin_ok)
	testing.expect(t, construct_ok)
	testing.expect(t, cast_ok)
	if template_ok {
		testing.expect_value(t, template_op.source.range.start, template_offset)
		testing.expect_value(t, value_type(function, template_op.results[0]), BUILTIN_TYPE_STRING)
	}
	if builtin_ok {
		testing.expect_value(t, builtin_op.source.range.start, strlen_offset)
		payload := expect_call_payload(t, &result.module, builtin_op)
		testing.expect_value(t, payload.callee_name, "strlen")
	}
	if construct_ok {
		testing.expect_value(t, construct_op.source.range.start, value_offset)
		testing.expect(t, value_type(function, construct_op.results[0]) != BUILTIN_TYPE_UNKNOWN)
	}
	if cast_ok {
		testing.expect_value(t, cast_op.source.range.start, conv_offset)
		testing.expect(t, value_type(function, cast_op.results[0]) != BUILTIN_TYPE_UNKNOWN)
	}
}

@(test)
lowering_snippet_covers_statement_string_operations :: proc(t: ^testing.T) {
	source := `DATA lv_a TYPE string VALUE 'A'.
DATA lv_b TYPE string VALUE 'B'.
DATA lv_sep TYPE string VALUE '-'.
DATA lv_text TYPE string.
CONCATENATE lv_a lv_b INTO lv_text SEPARATED BY lv_sep RESPECTING BLANKS.
CONDENSE lv_text NO-GAPS.
TRANSLATE lv_text TO UPPER CASE.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.concatenate separator respecting_blanks"))
	testing.expect(t, strings.contains(text, "abap.condense no_gaps"))
	testing.expect(t, strings.contains(text, "abap.translate mode=to_upper"))

	function := lower_test_primary_source_function(&result.module)
	concatenate_op, concatenate_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Concatenate)
	condense_op, condense_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Condense)
	translate_op, translate_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Translate)
	testing.expect(t, concatenate_ok)
	testing.expect(t, condense_ok)
	testing.expect(t, translate_ok)
	if concatenate_ok {
		payload := expect_string_payload(t, &result.module, concatenate_op)
		testing.expect(t, payload.has_separator)
		testing.expect(t, payload.respecting_blanks)
		testing.expect_value(t, len(concatenate_op.operands), 3)
	}
	if condense_ok {
		payload := expect_string_payload(t, &result.module, condense_op)
		testing.expect(t, payload.no_gaps)
		testing.expect_value(t, len(condense_op.operands), 1)
	}
	if translate_ok {
		payload := expect_string_payload(t, &result.module, translate_op)
		testing.expect_value(t, payload.translate_mode, Abap_Translate_Mode.To_Upper)
		testing.expect_value(t, len(translate_op.operands), 1)
	}
}

@(test)
lowering_snippet_covers_remaining_scalar_string_operations :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string VALUE 'AA-BB-CC-DD'.
DATA lv_first TYPE string.
DATA lv_second TYPE string.
DATA lv_rest TYPE string.
DATA lv_offset TYPE i.
DATA lv_length TYPE i.
DATA lv_count TYPE i.
SPLIT lv_text AT '-' INTO lv_first lv_second lv_rest.
REPLACE ALL OCCURRENCES OF 'C' IN lv_rest WITH 'x'.
SHIFT lv_rest RIGHT BY 2 PLACES.
SHIFT lv_first.
FIND ALL OCCURRENCES OF 'x' IN lv_rest MATCH OFFSET lv_offset MATCH LENGTH lv_length MATCH COUNT lv_count.
SEARCH lv_rest FOR 'x'.
FIND REGEX 'x' IN lv_rest.
FIND 'x' IN SECTION OFFSET 0 OF lv_rest.
FIND 'x' IN lv_rest RESULTS lv_rest.
SEARCH lv_rest FOR 'x' ABBREVIATED.
SPLIT lv_text AT '-' INTO TABLE lv_rest.
REPLACE SECTION OFFSET 0 LENGTH 1 OF lv_rest WITH 'z'.
SHIFT lv_rest UP TO '-'.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.split"))
	testing.expect(t, strings.contains(text, "abap.replace occurrence=all"))
	testing.expect(t, strings.contains(text, "abap.shift direction=right"))
	testing.expect(t, strings.contains(text, "abap.shift direction=left"))
	testing.expect(t, strings.contains(text, "abap.find occurrence=all"))
	testing.expect(t, strings.contains(text, "abap.search"))

	function := lower_test_primary_source_function(&result.module)
	split_op, split_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Split)
	replace_op, replace_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Replace)
	shift_op, shift_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Shift)
	find_op, find_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Find)
	search_op, search_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Search)
	testing.expect(t, split_ok)
	testing.expect(t, replace_ok)
	testing.expect(t, shift_ok)
	testing.expect(t, find_ok)
	testing.expect(t, search_ok)
	if split_ok {
		testing.expect_value(t, len(split_op.operands), 2)
		testing.expect_value(t, len(split_op.results), 3)
	}
	if replace_ok {
		payload := expect_string_payload(t, &result.module, replace_op)
		testing.expect_value(t, payload.replace_occurrence, Abap_Replace_Occurrence.All)
		testing.expect_value(t, len(replace_op.operands), 3)
	}
	if shift_ok {
		payload := expect_string_payload(t, &result.module, shift_op)
		testing.expect_value(t, payload.shift_direction, Abap_Shift_Direction.Right)
		testing.expect_value(t, len(shift_op.operands), 2)
	}
	if find_ok {
		payload := expect_string_payload(t, &result.module, find_op)
		testing.expect_value(t, payload.find_occurrence, Abap_Find_Occurrence.All)
		testing.expect_value(t, len(find_op.operands), 2)
		testing.expect_value(t, len(find_op.results), 4)
	}
	if search_ok {
		testing.expect_value(t, len(search_op.operands), 2)
		testing.expect_value(t, len(search_op.results), 2)
	}
	testing.expect(t, lower_test_has_unsupported_with_source(function, "FIND REGEX semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "FIND SECTION semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "FIND RESULTS semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "SEARCH ABBREVIATED semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "SPLIT INTO TABLE semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "REPLACE SECTION semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "SHIFT UP TO semantics"))
}

@(test)
lowering_snippet_covers_resolved_call_and_oop_domain_ops :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
 PUBLIC SECTION.
  METHODS run IMPORTING iv_value TYPE i.
  METHODS get RETURNING VALUE(rv_value) TYPE i.
  CLASS-METHODS stat RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
 METHOD run.
 ENDMETHOD.
 METHOD get.
  rv_value = 1.
 ENDMETHOD.
 METHOD stat.
  rv_value = 2.
 ENDMETHOD.
ENDCLASS.
FORM sub USING iv_value TYPE i.
ENDFORM.
DATA lo TYPE REF TO lcl_demo.
DATA lv TYPE i.
CALL METHOD lo->run EXPORTING iv_value = lv.
lv = lo->get( ).
lv = lcl_demo=>stat( ).
PERFORM sub USING lv.
DATA lv_class TYPE string.
CALL METHOD (lv_class)=>run.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.call.method @lcl_demo.get"))
	testing.expect(t, strings.contains(text, "abap.call.method @lcl_demo.stat"))
	testing.expect(t, !strings.contains(text, "abap.call.method @lcl_demo.get call=method"))
	testing.expect(t, !strings.contains(text, "abap.call.method @lcl_demo.stat call=method"))
	testing.expect(t, !strings.contains(text, "abap.call.method @lcl_demo.get target=get"))
	testing.expect(t, !strings.contains(text, "abap.call.method @lcl_demo.stat target=stat"))
	testing.expect(t, !strings.contains(text, "receiver="))
	testing.expect(t, strings.contains(text, "abap.call.routine @sub"))
	testing.expect(t, !strings.contains(text, "abap.call.routine @sub call=form"))
	testing.expect(t, !strings.contains(text, "abap.call.routine @sub target=sub"))
	testing.expect(t, strings.contains(text, `"unresolved or dynamic CALL METHOD target"`))

	function := lower_test_primary_source_function(&result.module)
	method_count := 0
	instance_count := 0
	static_count := 0
	routine_count := 0
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		intrinsic_op, intrinsic_ok := lower_test_intrinsic_op(&result.module, &op)
		if !intrinsic_ok {
			continue
		}
		#partial switch intrinsic_op {
			case .Call_Method:
				method_count += 1
				payload := expect_call_payload(t, &result.module, &op)
				testing.expect_value(t, payload.call_kind, Abap_Call_Kind.Method)
				testing.expect(t, payload.has_call_function_target)
				call_operand_count := test_call_operand_count(&op)
				if payload.callee_name == "lcl_demo.get" {
					instance_count += 1
					testing.expect(t, call_operand_count >= 2)
					if call_operand_count >= 2 {
						receiver_type := value_type(function, op.operands[1])
						testing.expect(t, receiver_type != INVALID_TYPE_ID)
						if receiver_type != INVALID_TYPE_ID {
						testing.expect_value(t, type_ptr(&result.module, receiver_type).kind, Type_Kind.Reference)
					}
					}
				} else if payload.callee_name == "lcl_demo.stat" {
					static_count += 1
					testing.expect_value(t, call_operand_count, 1)
				}
			if payload.callee_name == "lcl_demo.get" {
				testing.expect_value(t, len(op.results), 2)
				testing.expect(t, value_type(function, op.results[1]) != BUILTIN_TYPE_UNKNOWN)
			}
		case .Call_Routine:
			routine_count += 1
			payload := expect_call_payload(t, &result.module, &op)
			testing.expect_value(t, payload.call_kind, Abap_Call_Kind.Form)
			testing.expect(t, payload.has_call_function_target)
		}
	}
	testing.expect_value(t, method_count, 2)
	testing.expect_value(t, instance_count, 1)
	testing.expect_value(t, static_count, 1)
	testing.expect_value(t, routine_count, 1)
	testing.expect(t, lower_test_has_unsupported_with_source(function, "unresolved or dynamic CALL METHOD target"))
}

@(test)
lowering_exception_flow_models_raise_try_catch_payloads :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
TRY.
 RAISE EXCEPTION TYPE cx_root.
 lv_text = 'miss'.
CATCH cx_root INTO DATA(lx_error).
 lv_text = 'hit'.
ENDTRY.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)

	module := &fixture.lowered.module
	function := lower_test_primary_source_function(module)
	raise_count := 0
	match_count := 0
	catch_count := 0
	raise_invoke := INVALID_OP_ID
	match_block := INVALID_BLOCK_ID
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		intrinsic_op, intrinsic_ok := lower_test_intrinsic_op(module, &op)
		if !intrinsic_ok {
			continue
		}
		#partial switch intrinsic_op {
		case .ABAP_Exception_Raise:
			raise_count += 1
			raise_invoke = op.id
			testing.expect_value(t, op.opcode, Opcode.Invoke)
			testing.expect_value(t, len(op.successors), 2)
			payload := expect_exception_payload(t, module, &op)
			testing.expect_value(t, payload.exception_name, "cx_root")
		case .ABAP_Exception_Match:
			match_count += 1
			match_block = op.parent
			payload := expect_exception_payload(t, module, &op)
			testing.expect_value(t, payload.exception_name, "cx_root")
			testing.expect_value(t, value_type(function, op.results[0]), BUILTIN_TYPE_PREDICATE)
		case .ABAP_Exception_Catch:
			catch_count += 1
			testing.expect_value(t, len(op.results), 2)
			if len(op.results) == 2 {
				testing.expect(t, value_type(function, op.results[1]) != BUILTIN_TYPE_UNKNOWN)
			}
		}
	}
	testing.expect_value(t, raise_count, 1)
	testing.expect_value(t, match_count, 1)
	testing.expect_value(t, catch_count, 1)
	if raise_invoke != INVALID_OP_ID && match_block != INVALID_BLOCK_ID {
		op := op_ptr(function, raise_invoke)
		normal_count := 0
		exception_count := 0
		for edge in op.successors {
			testing.expect_value(t, len(edge.args), 1)
			if len(edge.args) == 1 {
				testing.expect_value(t, edge.args[0], op.results[0])
			}
			#partial switch edge.kind {
			case .Normal:
				normal_count += 1
			case .Exception:
				exception_count += 1
				testing.expect_value(t, edge.target, match_block)
			}
		}
		testing.expect_value(t, normal_count, 1)
		testing.expect_value(t, exception_count, 1)
	}

	text := print_module(&fixture.lowered.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "invoke @abap.exception.raise exception=cx_root"))
	testing.expect(t, strings.contains(text, "abap.exception.match exception=cx_root"))
	testing.expect(t, strings.contains(text, "abap.exception.catch "))
}

@(test)
unsupported_boundary_exception_variants_are_precisely_source_bearing :: proc(t: ^testing.T) {
	source := `RAISE EVENT changed.
DATA lo_error TYPE REF TO cx_root.
RAISE EXCEPTION lo_error.
TRY.
CLEANUP.
ENDTRY.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)

	function := lower_test_primary_source_function(&fixture.lowered.module)
	testing.expect(t, lower_test_has_unsupported_with_source(function, "RAISE EVENT semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "RAISE EXCEPTION object semantics"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "TRY CLEANUP semantics"))
}

@(test)
lowering_method_instance_attributes_use_receiver_slot :: proc(t: ^testing.T) {
	source := `CLASS lcl_accumulator DEFINITION.
 PUBLIC SECTION.
  METHODS add.
  METHODS total RETURNING VALUE(rv_total) TYPE i.
  METHODS repeat_total RETURNING VALUE(rv_total) TYPE i.
 PRIVATE SECTION.
  DATA mv_total TYPE i.
ENDCLASS.
CLASS lcl_accumulator IMPLEMENTATION.
 METHOD add.
  mv_total = mv_total + 1.
 ENDMETHOD.
 METHOD total.
  rv_total = mv_total.
 ENDMETHOD.
 METHOD repeat_total.
  rv_total = total( ).
 ENDMETHOD.
ENDCLASS.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	module := &fixture.lowered.module

	add, _, add_ok := lower_test_function_by_name(module, "lcl_accumulator.add")
	total, _, total_ok := lower_test_function_by_name(module, "lcl_accumulator.total")
	repeat_total, _, repeat_total_ok := lower_test_function_by_name(module, "lcl_accumulator.repeat_total")
	testing.expect(t, add_ok)
	testing.expect(t, total_ok)
	testing.expect(t, repeat_total_ok)
	if !add_ok || !total_ok || !repeat_total_ok {
		return
	}

	me_slot, _, me_ok := lower_test_slot_by_name(total, "me")
	_, _, mv_total_slot_ok := lower_test_slot_by_name(total, "mv_total")
	testing.expect(t, me_ok)
	testing.expect(t, !mv_total_slot_ok)
	if me_ok {
		testing.expect_value(t, me_slot.kind, Slot_Kind.Instance)
	}

	total_field_load, total_field_load_ok := lower_test_field_op_by_name(total, "mv_total")
	add_field_store, add_field_store_ok := lower_test_field_store_by_name(add, "mv_total")
	testing.expect(t, total_field_load_ok)
	testing.expect(t, add_field_store_ok)
	if total_field_load_ok {
		projection := expect_field_projection(t, total, total_field_load)
		testing.expect_value(t, projection_last_field_name(total, projection), "mv_total")
	}
	if add_field_store_ok {
		projection := expect_field_projection(t, add, add_field_store)
		testing.expect_value(t, projection_last_field_name(add, projection), "mv_total")
	}

	implicit_call_ok := false
	for &op in repeat_total.instructions {
		if !test_instruction_is_regular(repeat_total, &op) {
			continue
		}
		intrinsic_op, intrinsic_ok := lower_test_intrinsic_op(module, &op)
		if !intrinsic_ok || intrinsic_op != .Call_Method {
			continue
		}
		payload := expect_call_payload(t, module, &op)
		if payload.callee_name == "lcl_accumulator.total" {
			implicit_call_ok = true
			testing.expect(t, len(op.operands) >= 2)
			if len(op.operands) >= 2 {
				receiver_type := value_type(repeat_total, op.operands[1])
				testing.expect(t, receiver_type != INVALID_TYPE_ID)
				if receiver_type != INVALID_TYPE_ID {
					testing.expect_value(t, type_ptr(module, receiver_type).kind, Type_Kind.Reference)
				}
			}
		}
	}
	testing.expect(t, implicit_call_ok)
}

@(test)
lowering_instance_method_frame_keeps_unused_receiver_slot :: proc(t: ^testing.T) {
	source := `CLASS lcl_parent DEFINITION.
 PUBLIC SECTION.
  METHODS mul
   IMPORTING
    iv_val1 TYPE i
    iv_val2 TYPE i
   RETURNING
    VALUE(rv_res) TYPE i.
ENDCLASS.
CLASS lcl_parent IMPLEMENTATION.
 METHOD mul.
  rv_res = iv_val1 * iv_val2.
 ENDMETHOD.
ENDCLASS.
CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
ENDCLASS.
DATA lo_child TYPE REF TO lcl_child.
DATA lv_result TYPE i.
CREATE OBJECT lo_child.
lv_result = lo_child->mul( iv_val1 = 2 iv_val2 = 3 ).`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	module := &fixture.lowered.module

	mul, _, mul_ok := lower_test_function_by_name(module, "lcl_parent.mul")
	testing.expect(t, mul_ok)
	if !mul_ok {
		return
	}

	me_slot, _, me_ok := lower_test_slot_by_name(mul, "me")
	testing.expect(t, me_ok)
	if me_ok {
		testing.expect_value(t, me_slot.kind, Slot_Kind.Instance)
		testing.expect(t, me_slot.type != INVALID_TYPE_ID)
		if me_slot.type != INVALID_TYPE_ID {
			testing.expect_value(t, type_ptr(module, me_slot.type).kind, Type_Kind.Reference)
		}
	}
}

@(test)
lowering_snippet_preserves_message_domain_payloads :: proc(t: ^testing.T) {
	source := `DATA lv_type TYPE c LENGTH 1.
DATA lv_no TYPE c LENGTH 3.
DATA lv_text TYPE string.
DATA lv_msg TYPE string.
MESSAGE e001(zmsg) WITH lv_text INTO lv_msg DISPLAY LIKE 'I' RAISING cx_msg.
MESSAGE ID zmsg TYPE lv_type NUMBER lv_no WITH lv_text DISPLAY LIKE lv_type.
MESSAGE 'hello' TYPE 'I'.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.message form=compact id=zmsg type=e number=001 args=1 into display_like"))
	testing.expect(t, strings.contains(text, "raising=cx_msg"))
	testing.expect(t, strings.contains(text, "abap.message form=explicit id=zmsg head_operands=2 args=1 display_like=operand"))
	testing.expect(t, strings.contains(text, "abap.message form=default type=I args=1"))

	function := lower_test_primary_source_function(&result.module)
	message_count := 0
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		intrinsic_op, intrinsic_ok := lower_test_intrinsic_op(&result.module, &op)
		if !intrinsic_ok || intrinsic_op != .ABAP_Message {
			continue
		}
		message_count += 1
		payload := expect_message_payload(t, &result.module, &op)
		if message_count == 1 {
			testing.expect_value(t, payload.form, Abap_Message_Form.Compact)
			testing.expect_value(t, payload.id, "zmsg")
			testing.expect_value(t, payload.msg_type, "e")
			testing.expect_value(t, payload.number, "001")
			testing.expect_value(t, payload.arg_count, 1)
			testing.expect(t, payload.has_into)
			testing.expect(t, payload.has_display_like)
			testing.expect(t, payload.has_raising)
			testing.expect_value(t, len(op.results), 2)
		} else if message_count == 2 {
			testing.expect_value(t, payload.form, Abap_Message_Form.Explicit)
			testing.expect_value(t, payload.id, "zmsg")
			testing.expect_value(t, payload.head_operands, 2)
			testing.expect_value(t, payload.arg_count, 1)
			testing.expect(t, payload.display_like_operand)
		} else if message_count == 3 {
			testing.expect_value(t, payload.form, Abap_Message_Form.Default)
			testing.expect_value(t, payload.msg_type, "I")
			testing.expect_value(t, payload.number, "")
			testing.expect_value(t, payload.arg_count, 1)
		}
	}
	testing.expect_value(t, message_count, 3)
}

@(test)
lowering_snippet_covers_clear_refresh_and_free_data_movement :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
DATA lt TYPE STANDARD TABLE OF i.
CLEAR lv.
REFRESH lt.
FREE lt.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.clear"))
	testing.expect(t, strings.contains(text, "abap.refresh"))
	testing.expect(t, strings.contains(text, "abap.free"))
	testing.expect(t, strings.contains(text, "store"))

	function := lower_test_primary_source_function(&result.module)
	clear_offset := strings.index(source, "CLEAR")
	refresh_offset := strings.index(source, "REFRESH")
	free_offset := strings.index(source, "FREE")
	clear_op, clear_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Clear)
	refresh_op, refresh_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Refresh)
	free_op, free_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Free)
	testing.expect(t, clear_ok)
	testing.expect(t, refresh_ok)
	testing.expect(t, free_ok)
	if clear_ok {
		testing.expect_value(t, clear_op.source.range.start, clear_offset)
		testing.expect_value(t, value_type(function, clear_op.results[1]), lower_test_slot_type(function, "lv"))
	}
	if refresh_ok {
		testing.expect_value(t, refresh_op.source.range.start, refresh_offset)
		testing.expect_value(t, value_type(function, refresh_op.results[1]), lower_test_slot_type(function, "lt"))
	}
	if free_ok {
		testing.expect_value(t, free_op.source.range.start, free_offset)
		testing.expect_value(t, value_type(function, free_op.results[1]), lower_test_slot_type(function, "lt"))
	}
}

@(test)
lowering_snippet_covers_selector_paths_and_field_symbol_assignment :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_inner,
     amount TYPE i,
    END OF ty_inner.
TYPES: BEGIN OF ty_row,
     count TYPE i,
     inner TYPE ty_inner,
    END OF ty_row.
DATA ls TYPE ty_row.
DATA lv TYPE i.
FIELD-SYMBOLS <fs> TYPE i.
lv = ls-count.
lv = ls-inner-amount.
lv = sy-subrc.
sy-subrc = lv.
ls-count = lv.
ASSIGN lv TO <fs>.
UNASSIGN <fs>.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "field_addr path=-count#0"))
	testing.expect(t, strings.contains(text, "system.read .subrc"))
	testing.expect(t, strings.contains(text, "abap.assign_field"))
	testing.expect(t, strings.contains(text, "abap.unassign"))
	testing.expect(t, strings.contains(text, "system.write .subrc"))

	function := lower_test_primary_source_function(&result.module)
	field_load, field_load_ok := lower_test_field_op_by_name(function, "count")
	nested_field_load, nested_field_load_ok := lower_test_field_op_by_name(function, "amount")
	system_read, system_read_ok := lower_test_intrinsic_at_start(
		&result.module,
		function,
		.System_Read,
		strings.index(source, "sy-subrc."),
	)
	system_write_assignment, system_write_assignment_ok := lower_test_intrinsic_at_start(
		&result.module,
		function,
		.System_Write,
		strings.index(source, "sy-subrc ="),
	)
	assign_field, assign_field_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Assign_Field)
	unassign, unassign_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Unassign)
	testing.expect(t, field_load_ok)
	testing.expect(t, nested_field_load_ok)
	testing.expect(t, system_read_ok)
	testing.expect(t, system_write_assignment_ok)
	testing.expect(t, assign_field_ok)
	testing.expect(t, unassign_ok)
	if field_load_ok {
		projection_id := expect_field_projection(t, function, field_load)
		testing.expect_value(t, projection_last_field_name(function, projection_id), "count")
		projection := projection_ptr(function, projection_id)
		testing.expect_value(t, len(projection.segments), 1)
		if len(projection.segments) == 1 {
			testing.expect_value(t, projection.segments[0].name, "count")
			testing.expect_value(t, projection.segments[0].field_index, i32(0))
		}
		testing.expect(t, value_type(function, field_load.results[0]) != BUILTIN_TYPE_UNKNOWN)
	}
	if nested_field_load_ok {
		projection_id := expect_field_projection(t, function, nested_field_load)
		projection := projection_ptr(function, projection_id)
		testing.expect_value(t, len(projection.segments), 2)
		if len(projection.segments) == 2 {
			testing.expect_value(t, projection.segments[0].name, "inner")
			testing.expect_value(t, projection.segments[0].field_index, i32(1))
			testing.expect_value(t, projection.segments[1].name, "amount")
			testing.expect_value(t, projection.segments[1].field_index, i32(0))
		}
	}
	if system_read_ok {
		payload := expect_system_payload(t, &result.module, system_read)
		testing.expect_value(t, payload.system_field, "subrc")
		testing.expect(t, value_type(function, system_read.results[0]) != BUILTIN_TYPE_UNKNOWN)
	}
	if system_write_assignment_ok {
		payload := expect_system_payload(t, &result.module, system_write_assignment)
		testing.expect_value(t, payload.system_field, "subrc")
	}
	if assign_field_ok {
		testing.expect_value(t, assign_field.source.range.start, strings.index(source, "ASSIGN"))
	}
	if unassign_ok {
		testing.expect_value(t, unassign.source.range.start, strings.index(source, "UNASSIGN"))
	}
	fs_slot, _, fs_slot_ok := lower_test_slot_by_name(function, "<fs>")
	testing.expect(t, fs_slot_ok)
	if fs_slot_ok {
		testing.expect(t, fs_slot.entity != nil)
		testing.expect_value(t, fs_slot.entity.kind, semantic.Entity_Kind.Field_Symbol)
		testing.expect(t, fs_slot.type != BUILTIN_TYPE_UNKNOWN)
	}
}

@(test)
lowering_snippet_models_reference_lvalues_and_create_data :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
DATA lr TYPE REF TO i.
FIELD-SYMBOLS <fs> TYPE i.
ASSIGN lv TO <fs>.
lr = REF #( lv ).
lr->* = 2.
CREATE DATA lr.
ASSIGN COMPONENT 'amount' OF STRUCTURE lv TO <fs>.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered
	function := lower_test_primary_source_function(&result.module)

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.assign_field"))
	testing.expect(t, strings.contains(text, "abap.construct @ref"))
	testing.expect(t, strings.contains(text, "abap.construct @create_data"))
	testing.expect(t, strings.contains(text, "field_addr path=->*"))
	testing.expect(t, lower_test_has_unsupported_with_source(function, "ASSIGN COMPONENT semantics"))

	assign_op, assign_ok := lower_test_first_intrinsic_op(&result.module, function, .ABAP_Assign_Field)
	testing.expect(t, assign_ok)
	if assign_ok {
		testing.expect_value(t, len(assign_op.operands), 2)
		testing.expect_value(t, len(assign_op.results), 2)
	}
}

@(test)
lowering_snippet_preserves_semantic_slots_types_and_source_ranges :: proc(t: ^testing.T) {
	source := `DATA lv_value TYPE c LENGTH 10.
DATA lv_copy TYPE c LENGTH 10.
lv_copy = lv_value.`
	parsed := parser.parse(source, "mem://ir_fact_test.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := semantic.project_make()
	defer semantic.project_destroy(&project)
	checker := semantic.checker_make(&project)
	file := semantic.checker_add_file(&checker, parsed.path, parsed.root)
	semantic.checker_check_file(&checker, file)

	lowered := lower_project(&project, &checker, context.allocator)
	defer lower_result_destroy(&lowered)
	result := &lowered

	verify := verify_module(&result.module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, verify.ok)
	if len(result.module.functions) == 0 {
		testing.expect(t, false)
		return
	}

	function := lower_test_primary_source_function(&result.module)
	value_decl_offset := strings.index(source, "lv_value")
	value_use_offset := strings.last_index(source, "lv_value")
	copy_assign_offset := strings.index(source, "lv_copy =")
	testing.expect(t, value_decl_offset >= 0 && value_use_offset > value_decl_offset && copy_assign_offset >= 0)
	if value_decl_offset < 0 || value_use_offset <= value_decl_offset || copy_assign_offset < 0 {
		return
	}

	value_slot, value_slot_id, value_slot_ok := lower_test_slot_by_name(function, "lv_value")
	copy_slot, copy_slot_id, copy_slot_ok := lower_test_slot_by_name(function, "lv_copy")
	testing.expect(t, value_slot_ok)
	testing.expect(t, copy_slot_ok)
	if !value_slot_ok || !copy_slot_ok {
		return
	}

	testing.expect(t, value_slot.entity != nil)
	testing.expect(t, copy_slot.entity != nil)
	testing.expect_value(t, value_slot.entity.name, "lv_value")
	testing.expect_value(t, copy_slot.entity.name, "lv_copy")
	testing.expect(t, value_slot.source.file == value_slot.entity.source_file)
	testing.expect_value(t, value_slot.source.range.start, value_decl_offset)
	testing.expect_value(t, value_slot.source.range.end, value_decl_offset + len("lv_value"))

	value_type_record, value_type_ok := module_type_record(&result.module, value_slot.type)
	copy_type_record, copy_type_ok := module_type_record(&result.module, copy_slot.type)
	testing.expect(t, value_type_ok)
	testing.expect(t, copy_type_ok)
	if value_type_ok && copy_type_ok {
		testing.expect(t, value_type_record.semantic_type == value_slot.entity.type)
		testing.expect(t, copy_type_record.semantic_type == copy_slot.entity.type)
		testing.expect_value(t, value_type_record.name, "c(10)")
		testing.expect_value(t, copy_type_record.name, "c(10)")
	}

	query := semantic.semantic_query(&project, &checker)
	files := semantic.semantic_query_files(query)
	testing.expect_value(t, len(files), 1)
	if len(files) == 0 {
		return
	}
	file_query := semantic.semantic_query(&project, &checker, file)
	ref_query := semantic.semantic_query_refs(file_query)
	fact_query := semantic.semantic_query_facts(file_query)

	load_found := false
	store_found := false
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		if op.opcode == .Load {
			slot_id, slot_ok := expect_memory_slot(t, function, &op)
			if !slot_ok || slot_id != value_slot_id {
				continue
			}
			load_found = true
			testing.expect(t, op.source.file == value_slot.entity.source_file)
			testing.expect(t, op.source.node != nil)
			testing.expect_value(t, op.source.range.start, value_use_offset)
			testing.expect_value(t, op.source.range.end, value_use_offset + len("lv_value"))
			if len(op.results) > 0 {
				testing.expect_value(t, value_type(function, op.results[0]), value_slot.type)
			}
			use := semantic.semantic_ref_use_for_node(ref_query, op.source.node)
			testing.expect(t, use != nil)
			if use != nil {
				testing.expect(t, use.entity == value_slot.entity)
			}
			node_info, node_info_ok := semantic.semantic_fact_operand_info_for_node(fact_query, op.source.node)
			range_info, range_info_ok := semantic.semantic_fact_operand_info_at_range(fact_query, op.source.range)
			testing.expect(t, node_info_ok)
			testing.expect(t, range_info_ok)
			if node_info_ok && range_info_ok {
				testing.expect(t, node_info.type == value_slot.entity.type)
				testing.expect(t, range_info.type == value_slot.entity.type)
			}
		} else if op.opcode == .Store {
			slot_id, slot_ok := expect_memory_slot(t, function, &op)
			if !slot_ok || slot_id != copy_slot_id {
				continue
			}
			store_found = true
			testing.expect(t, op.source.file == copy_slot.entity.source_file)
			testing.expect(t, op.source.node != nil)
			testing.expect_value(t, op.source.range.start, copy_assign_offset)
			testing.expect_value(t, op.source.range.end, copy_assign_offset + len("lv_copy"))
		}
	}
	testing.expect(t, load_found)
	testing.expect(t, store_found)
}

@(test)
builder_verifier_rejects_bad_operation_signature_and_non_predicate_branch :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	unsupported := builder_begin_function(&module, "bad_unsupported")
	builder_emit_op(&unsupported, .Unsupported)
	builder_set_return_world(&unsupported)

	branch := builder_begin_function(&module, "bad_branch_condition")
	condition := builder_emit_const(&branch, "1", BUILTIN_TYPE_INTEGER)
	true_block := builder_add_world_block(&branch, "true")
	false_block := builder_add_world_block(&branch, "false")
	true_args := [?]Value_Id{branch.current_world}
	false_args := [?]Value_Id{branch.current_world}
	builder_set_cond_branch(&branch, condition, true_block, true_args[:], false_block, false_args[:])
	builder_position_at_end(&branch, true_block)
	builder_set_return_world(&branch)
	builder_position_at_end(&branch, false_block)
	builder_set_return_world(&branch)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic(verify, .Bad_Op_Signature))
	testing.expect(t, verify_has_diagnostic(verify, .Bad_Terminator_Args))
}

@(test)
builder_verifier_rejects_resolved_method_call_without_target :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "bad_call")
	builder_emit_effect_intrinsic(
		&builder,
		.Call_Method,
		payload = Intrinsic_Call_Payload{call_kind = .Method},
	)
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "call intrinsic must carry callee name"))
}

@(test)
builder_verifier_rejects_instance_method_call_without_receiver :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "bad_instance_call")
	builder_emit_effect_intrinsic(
		&builder,
		.Call_Method,
		payload = Intrinsic_Call_Payload {
			callee_name = "lcl_demo.run",
			call_kind = .Method,
			call_function_target = INVALID_FUNCTION_ID,
			has_call_function_target = true,
		},
	)
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "call intrinsic target flag must carry function target"))
}

@(test)
builder_verifier_rejects_table_operation_without_domain_payload :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	builder := builder_begin_function(&module, "bad_table")
	table_type := module_add_type(&module, Type{kind = .Table, name = "table"})
	table := builder_emit_const(&builder, "itab", table_type)
	inputs := [?]Value_Id{table}
	result_types := [?]Type_Id{BUILTIN_TYPE_INTEGER, BUILTIN_TYPE_INTEGER}
	builder_emit_effect_intrinsic(&builder, .Table_Read, inputs[:], result_types[:], effects = {.Read_Table})
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "table intrinsic must carry table payload"))
}

@(test)
builder_verifier_covers_message_domain_payloads :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	valid := builder_begin_function(&module, "valid_message")
	arg := builder_emit_const(&valid, "text", BUILTIN_TYPE_STRING)
	inputs := [?]Value_Id{arg}
	result := builder_emit_message(
		&valid,
		inputs[:],
		BUILTIN_TYPE_STRING,
		Intrinsic_Message_Payload {
			form = .Compact,
			id = "zmsg",
			msg_type = "e",
			number = "001",
			arg_count = 1,
			has_into = true,
			has_display_like = true,
			display_like = "i",
		},
	)
	testing.expect(t, result != INVALID_VALUE_ID)
	builder_set_return_world(&valid)

	invalid := builder_begin_function(&module, "bad_message")
	builder_emit_message(&invalid, payload = Intrinsic_Message_Payload{has_display_like = true})
	builder_set_return_world(&invalid)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "message intrinsic must carry message form"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "message DISPLAY LIKE must carry static text or dynamic operand"))
}

@(test)
builder_verifier_covers_sql_domain_payloads :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	valid := builder_begin_function(&module, "valid_sql")
	sql_payload := Intrinsic_SQL_Payload {
		source_kind = .Internal,
		result_kind = .Into,
		row_type = BUILTIN_TYPE_INTEGER,
		scalar_type = BUILTIN_TYPE_INTEGER,
		source_count = 1,
		projection_count = 1,
	}
	builder_emit_sql_select(&valid, BUILTIN_TYPE_INTEGER, sql_payload)
	builder_emit_sql_mutation(
		&valid,
		.SQL_Insert,
		payload = Intrinsic_SQL_Payload {
			source_kind = .Internal,
			row_type = BUILTIN_TYPE_INTEGER,
			source_count = 1,
		},
	)
	builder_set_return_world(&valid)

	invalid := builder_begin_function(&module, "bad_sql")
	builder_emit_sql_select(&invalid, BUILTIN_TYPE_INTEGER)
	builder_set_return_world(&invalid)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "SQL intrinsic must carry source kind"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "SQL intrinsic must carry valid row type"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Intrinsic, "SQL query intrinsic must carry projection count"))
}

@(test)
lowering_snippet_preserves_table_domain_payloads :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
     id TYPE i,
     text TYPE string,
    END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row.
DATA ls_row TYPE ty_row.
DATA lv_id TYPE i.
LOOP AT lt_rows INTO ls_row.
ENDLOOP.
READ TABLE lt_rows INTO ls_row WITH KEY id = lv_id.
SORT lt_rows STABLE BY id.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "table.iter access=sequential result=into source=row row=ty_row"))
	testing.expect(t, strings.contains(text, ": table_iter = intrinsic @abap.table.iter "))
	testing.expect(t, strings.contains(text, "iter : table_iter"))
	testing.expect(t, !strings.contains(text, "unknown"))
	testing.expect(t, strings.contains(text, "table.read access=key key=free result=into row=ty_row components=1"))
	testing.expect(t, strings.contains(text, "table.sort access=sort row=ty_row components=1 stable"))

	function := lower_test_primary_source_function(&result.module)
	iter_op, iter_ok := lower_test_first_intrinsic_op(&result.module, function, .Table_Iter)
	read_op, read_ok := lower_test_first_intrinsic_op(&result.module, function, .Table_Read)
	sort_op, sort_ok := lower_test_first_intrinsic_op(&result.module, function, .Table_Sort)
	testing.expect(t, iter_ok)
	testing.expect(t, read_ok)
	testing.expect(t, sort_ok)
	if iter_ok {
		payload := expect_table_payload(t, &result.module, iter_op)
		testing.expect_value(t, payload.access, Table_Access_Kind.Sequential)
		testing.expect_value(t, payload.result_kind, Table_Result_Kind.Into)
		testing.expect(t, payload.row_type != BUILTIN_TYPE_UNKNOWN)
		testing.expect(t, len(iter_op.results) > 1)
		if len(iter_op.results) > 1 {
			testing.expect_value(t, value_type(function, iter_op.results[1]), BUILTIN_TYPE_TABLE_ITERATOR)
		}
	}
	if read_ok {
		payload := expect_table_payload(t, &result.module, read_op)
		testing.expect_value(t, payload.access, Table_Access_Kind.Key)
		testing.expect_value(t, payload.key_kind, Table_Key_Kind.Free)
		testing.expect_value(t, payload.result_kind, Table_Result_Kind.Into)
		testing.expect_value(t, payload.component_count, 1)
		testing.expect_value(t, len(payload.components), 1)
		if len(payload.components) == 1 {
			testing.expect_value(t, payload.components[0].value_index, 1)
			testing.expect_value(t, len(payload.components[0].path), 1)
			if len(payload.components[0].path) == 1 {
				testing.expect_value(t, payload.components[0].path[0], "id")
			}
		}
		row_record, row_ok := module_type_record(&result.module, payload.row_type)
		testing.expect(t, row_ok)
		if row_ok {
			testing.expect_value(t, row_record.name, "ty_row")
			testing.expect(t, row_record.semantic_type != nil)
		}
	}
	if sort_ok {
		payload := expect_table_payload(t, &result.module, sort_op)
		testing.expect_value(t, payload.access, Table_Access_Kind.Sort)
		testing.expect_value(t, payload.component_count, 1)
		testing.expect_value(t, len(payload.sort_components), 1)
		if len(payload.sort_components) == 1 {
			testing.expect_value(t, len(payload.sort_components[0].path), 1)
			if len(payload.sort_components[0].path) == 1 {
				testing.expect_value(t, payload.sort_components[0].path[0], "id")
			}
			testing.expect(t, !payload.sort_components[0].descending)
		}
		testing.expect(t, payload.stable)
	}
}

@(test)
lowering_snippet_preserves_table_where_and_result_binding_payloads :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
     id TYPE i,
     text TYPE string,
    END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row.
DATA ls_row TYPE ty_row.
DATA lv_id TYPE i.
FIELD-SYMBOLS <row> TYPE ty_row.
APPEND ls_row TO lt_rows ASSIGNING <row>.
LOOP AT lt_rows INTO ls_row WHERE id = lv_id.
ENDLOOP.
MODIFY lt_rows FROM ls_row WHERE id = lv_id.
DELETE lt_rows WHERE id = lv_id.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "table.append access=full result=assigning source=row row=ty_row"))
	testing.expect(t, strings.contains(text, "table.iter access=sequential result=into source=row row=ty_row components=1"))
	testing.expect(t, strings.contains(text, "table.modify access=where source=row row=ty_row components=1"))
	testing.expect(t, strings.contains(text, "table.delete access=where row=ty_row components=1"))
	testing.expect(t, !strings.contains(text, "APPEND result binding semantics"))
	testing.expect(t, !strings.contains(text, "LOOP WHERE filtering"))
	testing.expect(t, !strings.contains(text, "DELETE WHERE semantics"))

	function := lower_test_primary_source_function(&result.module)
	append_op, append_ok := lower_test_first_intrinsic_op(&result.module, function, .Table_Append)
	iter_op, iter_ok := lower_test_first_intrinsic_op(&result.module, function, .Table_Iter)
	modify_op, modify_ok := lower_test_first_intrinsic_op(&result.module, function, .Table_Modify)
	delete_op, delete_ok := lower_test_first_intrinsic_op(&result.module, function, .Table_Delete)
	testing.expect(t, append_ok)
	testing.expect(t, iter_ok)
	testing.expect(t, modify_ok)
	testing.expect(t, delete_ok)
	if append_ok {
		payload := expect_table_payload(t, &result.module, append_op)
		testing.expect_value(t, payload.result_kind, Table_Result_Kind.Assigning)
		testing.expect(t, len(append_op.results) == 3)
	}
	if iter_ok {
		payload := expect_table_payload(t, &result.module, iter_op)
		testing.expect_value(t, payload.component_count, 1)
		testing.expect_value(t, len(payload.components), 1)
		if len(payload.components) == 1 {
			testing.expect_value(t, payload.components[0].value_index, 1)
			testing.expect_value(t, len(payload.components[0].path), 1)
			if len(payload.components[0].path) == 1 {
				testing.expect_value(t, payload.components[0].path[0], "id")
			}
		}
	}
	if modify_ok {
		payload := expect_table_payload(t, &result.module, modify_op)
		testing.expect_value(t, payload.access, Table_Access_Kind.Where)
		testing.expect_value(t, payload.component_count, 1)
		testing.expect_value(t, len(payload.components), 1)
		if len(payload.components) == 1 {
			testing.expect_value(t, payload.components[0].value_index, 2)
			testing.expect_value(t, len(payload.components[0].path), 1)
			if len(payload.components[0].path) == 1 {
				testing.expect_value(t, payload.components[0].path[0], "id")
			}
		}
	}
	if delete_ok {
		payload := expect_table_payload(t, &result.module, delete_op)
		testing.expect_value(t, payload.access, Table_Access_Kind.Where)
		testing.expect_value(t, payload.component_count, 1)
		testing.expect_value(t, len(payload.components), 1)
		if len(payload.components) == 1 {
			testing.expect_value(t, payload.components[0].value_index, 1)
			testing.expect_value(t, len(payload.components[0].path), 1)
			if len(payload.components[0].path) == 1 {
				testing.expect_value(t, payload.components[0].path[0], "id")
			}
		}
	}
}

@(test)
lowering_snippet_preserves_sql_domain_payloads :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zcust,
     id TYPE i,
     name TYPE string,
    END OF zcust.
DATA lt_rows TYPE STANDARD TABLE OF zcust WITH EMPTY KEY.
DATA ls_row TYPE zcust.
DATA lv_id TYPE i.
DATA lv_name TYPE string.
SELECT id, name FROM zcust INTO TABLE @DATA(lt_selected).
OPEN CURSOR @DATA(lv_cursor) FOR SELECT id FROM zcust.
FETCH NEXT CURSOR lv_cursor INTO @DATA(lv_fetch_id).
INSERT zcust FROM TABLE lt_rows.
MODIFY zcust FROM ls_row WHERE id = lv_id.
UPDATE zcust SET name = lv_name WHERE id = lv_id.
DELETE FROM zcust WHERE id = lv_id.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "sql.select source=resolved:zcust"))
	testing.expect(t, strings.contains(text, "result=into_table"))
	testing.expect(t, strings.contains(text, "projections=2"))
	testing.expect(t, strings.contains(text, "sql.open_cursor source=resolved:zcust"))
	testing.expect(t, strings.contains(text, "sql.fetch "))
	testing.expect(t, strings.contains(text, "sql.insert source=resolved:zcust"))
	testing.expect(t, strings.contains(text, "sql.modify source=resolved:zcust"))
	testing.expect(t, strings.contains(text, "sql.update source=resolved:zcust"))
	testing.expect(t, strings.contains(text, "sql.delete source=resolved:zcust"))

	function := lower_test_primary_source_function(&result.module)
	select_op, select_ok := lower_test_first_intrinsic_op(&result.module, function, .SQL_Select)
	insert_op, insert_ok := lower_test_first_intrinsic_op(&result.module, function, .SQL_Insert)
	update_op, update_ok := lower_test_first_intrinsic_op(&result.module, function, .SQL_Update)
	delete_op, delete_ok := lower_test_first_intrinsic_op(&result.module, function, .SQL_Delete)
	testing.expect(t, select_ok)
	testing.expect(t, insert_ok)
	testing.expect(t, update_ok)
	testing.expect(t, delete_ok)
	if select_ok {
		payload := expect_sql_payload(t, &result.module, select_op)
		testing.expect_value(t, payload.source_kind, Sql_Source_Kind.Resolved)
		testing.expect_value(t, payload.source_name, "zcust")
		testing.expect_value(t, payload.result_kind, Sql_Result_Kind.Into_Table)
		testing.expect_value(t, payload.projection_count, 2)
		testing.expect_value(t, payload.source_count, 1)
		testing.expect(t, payload.row_type != BUILTIN_TYPE_UNKNOWN)
	}
	if insert_ok {
		payload := expect_sql_payload(t, &result.module, insert_op)
		testing.expect(t, payload.from_table)
		testing.expect_value(t, payload.source_kind, Sql_Source_Kind.Resolved)
		testing.expect_value(t, payload.source_name, "zcust")
	}
	if update_ok {
		payload := expect_sql_payload(t, &result.module, update_op)
		testing.expect_value(t, payload.assignment_count, 1)
		testing.expect_value(t, payload.source_kind, Sql_Source_Kind.Resolved)
	}
	if delete_ok {
		payload := expect_sql_payload(t, &result.module, delete_op)
		testing.expect_value(t, payload.source_kind, Sql_Source_Kind.Resolved)
	}
	testing.expect(t, lower_test_has_unsupported_with_source(function, "FETCH result value"))
}

// Shared test helpers.

Lower_Test_Result :: struct {
	lowered: Lower_Result,
	project: semantic.Project,
}

expect_module_print_snapshot :: proc(t: ^testing.T, module: ^Module, expected: string) {
	text := print_module(module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect_value(t, text, expected)
}

lower_test_source :: proc(t: ^testing.T, source: string) -> Lower_Test_Result {
	parsed := parser.parse(source, "mem://ir_test.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := semantic.project_make()
	checker := semantic.checker_make(&project)
	file := semantic.checker_add_file(&checker, parsed.path, parsed.root)
	semantic.checker_check_file(&checker, file)

	result := lower_project(&project, &checker, context.allocator)
	return Lower_Test_Result{lowered = result, project = project}
}

lower_test_verified_source :: proc(t: ^testing.T, source: string) -> Lower_Test_Result {
	fixture := lower_test_source(t, source)
	verify := verify_module(&fixture.lowered.module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, verify.ok)
	return fixture
}

lower_test_result_destroy :: proc(result: ^Lower_Test_Result) {
	assert(result != nil)
	lower_result_destroy(&result.lowered)
	semantic.project_destroy(&result.project)
	result^ = {}
}

lower_test_primary_source_function :: proc(module: ^Module) -> ^Function {
	assert(module != nil)
	for &function in module.functions {
		if function.role == .Event && strings.contains(function.name, "start_of_selection") {
			return &function
		}
	}
	for &function in module.functions {
		if function.role != .Report_Entry {
			return &function
		}
	}
	assert(len(module.functions) > 0)
	return &module.functions[0]
}

lower_test_function_by_name :: proc(module: ^Module, name: string) -> (^Function, Function_Id, bool) {
	assert(module != nil)
	for &function, i in module.functions {
		if function.name == name {
			return &function, Function_Id(i), true
		}
	}
	return nil, INVALID_FUNCTION_ID, false
}

lower_test_function_by_name_contains :: proc(module: ^Module, name: string) -> (^Function, Function_Id, bool) {
	assert(module != nil)
	for &function, i in module.functions {
		if strings.contains(function.name, name) {
			return &function, Function_Id(i), true
		}
	}
	return nil, INVALID_FUNCTION_ID, false
}

lower_test_core_call_count :: proc(_: ^Module, function: ^Function) -> int {
	count := 0
	for &op in function.instructions {
		if test_instruction_is_regular(function, &op) && lower_test_is_direct_call_op(&op) {
			count += 1
		}
	}
	return count
}

lower_test_is_direct_call_op :: proc "contextless" (op: ^Op) -> bool {
	if op == nil {
		return false
	}
	if op.opcode != .Call && op.opcode != .Invoke {
		return false
	}
	_, ok := op.attrs.(Call_Attrs)
	return ok
}

lower_test_core_call_target_name_contains :: proc(
	module: ^Module,
	function: ^Function,
	index: int,
	needle: string,
) -> bool {
	name, ok := lower_test_core_call_target_name(module, function, index)
	return ok && strings.contains(name, needle)
}

lower_test_core_call_target_name :: proc(
	module: ^Module,
	function: ^Function,
	index: int,
) -> (
	string,
	bool,
) {
	call_index := 0
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) || !lower_test_is_direct_call_op(&op) {
			continue
		}
		if call_index == index {
			attrs, attrs_ok := op.attrs.(Call_Attrs)
			if !attrs_ok {
				return "", false
			}
			target := attrs.target
			if target == INVALID_FUNCTION_ID || int(target) >= len(module.functions) {
				return "", false
			}
			return module.functions[int(target)].name, true
		}
		call_index += 1
	}
	return "", false
}

lower_test_slot_by_name :: proc(
	function: ^Function,
	name: string,
) -> (
	^Slot,
	Slot_Id,
	bool,
) {
	for slot, i in function.slots {
		if slot.name == name {
			id := Slot_Id(i)
			return &function.slots[int(id)], id, true
		}
	}
	return nil, INVALID_SLOT_ID, false
}

lower_test_block_by_name :: proc(function: ^Function, name: string) -> (Block_Id, bool) {
	for block, i in function.blocks {
		if block.name == name {
			return Block_Id(i), true
		}
	}
	return INVALID_BLOCK_ID, false
}

lower_test_first_opcode :: proc(function: ^Function, opcode: Opcode) -> (^Op, bool) {
	for &op in function.instructions {
		if test_instruction_is_regular(function, &op) && op.opcode == opcode {
			return &op, true
		}
	}
	return nil, false
}

lower_test_first_intrinsic_op :: proc(module: ^Module, function: ^Function, intrinsic_op: Intrinsic_Op) -> (^Op, bool) {
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		current, ok := lower_test_intrinsic_op(module, &op)
		if ok && current == intrinsic_op {
			return &op, true
		}
	}
	return nil, false
}

lower_test_intrinsic_op :: proc(module: ^Module, op: ^Op) -> (Intrinsic_Op, bool) {
	if op == nil ||
	  (op.opcode != .Intrinsic && op.opcode != .Invoke) ||
	  op.intrinsic == INVALID_INTRINSIC_ID ||
	  int(op.intrinsic) >= len(module.intrinsics) {
		return .Unknown, false
	}
	return module.intrinsics[int(op.intrinsic)].op, true
}

lower_test_field_op_by_name :: proc(function: ^Function, field_name: string) -> (^Op, bool) {
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		projection, projection_ok := projection_attrs(op.attrs)
		if op.opcode == .Field_Addr &&
		  projection_ok &&
		  projection_last_field_name(function, projection) == field_name {
			return &op, true
		}
	}
	return nil, false
}

lower_test_field_store_by_name :: proc(function: ^Function, field_name: string) -> (^Op, bool) {
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		projection, projection_ok := projection_attrs(op.attrs)
		if op.opcode == .Field_Addr &&
		  projection_ok &&
		  projection_last_field_name(function, projection) == field_name {
			return &op, true
		}
	}
	return nil, false
}

lower_test_intrinsic_at_start :: proc(module: ^Module, function: ^Function, intrinsic_op: Intrinsic_Op, start: int) -> (^Op, bool) {
	for &op in function.instructions {
		if !test_instruction_is_regular(function, &op) {
			continue
		}
		current, ok := lower_test_intrinsic_op(module, &op)
		if ok && current == intrinsic_op && op.source.range.start == start {
			return &op, true
		}
	}
	return nil, false
}

lower_test_opcode_at_start :: proc(function: ^Function, opcode: Opcode, start: int) -> (^Op, bool) {
	for &op in function.instructions {
		if test_instruction_is_regular(function, &op) && op.opcode == opcode && op.source.range.start == start {
			return &op, true
		}
	}
	return nil, false
}

lower_test_slot_type :: proc(function: ^Function, name: string) -> Type_Id {
	slot, _, ok := lower_test_slot_by_name(function, name)
	if !ok {
		return INVALID_TYPE_ID
	}
	return slot.type
}

lower_test_has_branch_to_with_arg_count :: proc(
	function: ^Function,
	target: Block_Id,
	arg_count: int,
) -> bool {
	for block in function.blocks {
		if block.terminator == INVALID_INSTRUCTION_ID {
			continue
		}
		term := op_ptr(function, Op_Id(block.terminator))
		#partial switch term.opcode {
		case .Br, .Cond_Br:
			for edge in term.successors {
				if edge.target == target && len(edge.args) == arg_count {
					return true
				}
			}
		}
	}
	return false
}

lower_test_has_unsupported_with_source :: proc(function: ^Function, message: string) -> bool {
	for &op in function.instructions {
		if test_instruction_is_regular(function, &op) {
			attrs, attrs_ok := op.attrs.(Unsupported_Attrs)
			if op.opcode == .Unsupported &&
			  .Unsupported in op.effects &&
			  attrs_ok &&
			  attrs.message == message &&
			  (op.source.file != nil || op.source.range.end > op.source.range.start) {
				return true
			}
		}
	}
	return false
}

verify_has_diagnostic :: proc(result: Verify_Result, kind: Verify_Diagnostic_Kind) -> bool {
	for diagnostic in result.diagnostics {
		if diagnostic.kind == kind {
			return true
		}
	}
	return false
}

verify_has_diagnostic_message :: proc(result: Verify_Result, kind: Verify_Diagnostic_Kind, message: string) -> bool {
	for diagnostic in result.diagnostics {
		if diagnostic.kind == kind && diagnostic.message == message {
			return true
		}
	}
	return false
}
