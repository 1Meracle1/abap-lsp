package abap_frontend_ir

import "src:ast"
import "src:parser"
import semantic "src:semantic"
import "src:tokenizer"

import "core:strings"
import "core:testing"

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
	testing.expect(t, strings.contains(text, "core.store %s0"))
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
		`func @main -> (!world) {
  slot %s0 local lv_value : !i
^b0.entry(%v0 world : !world):
  %v1 : !i = core.const 1
  %v2 : !world = core.store %s0 (%v0, %v1)
  %v3 : !i = core.load %s0 (%v2)
  cf.return(%v2)
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
		`func @literals -> (!world) {
^b0.entry(%v0 world : !world):
  %v1 : !i = core.const 42
  %v2 : !string = core.const 'single quoted'
  %v3 : !string = core.const "template segment"
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
		`func @unsupported -> (!world) {
^b0.entry(%v0 world : !world):
  %v1 : !world, %v2 : !i = core.unsupported "not yet" (%v0) [unsupported]
  cf.return(%v1)
}
`,
	)
}

// Inspection fixtures.

Inspection_Walk_Counts :: struct {
	functions:           int,
	blocks:              int,
	ops:                 int,
	terminators:         int,
	first_op_seen:       bool,
	first_op:            Op_Kind,
	last_op:             Op_Kind,
	last_terminator:     Terminator_Kind,
	entry_block_visited: bool,
}

inspection_walk_visit_function :: proc(
	visitor: ^Walk_Visitor,
	module: ^Module,
	function_id: Function_Id,
	function: ^Function,
) -> bool {
	_ = module
	_ = function_id
	counts := cast(^Inspection_Walk_Counts)visitor.data
	counts.functions += 1
	if function.name == "main" {
		counts.entry_block_visited = counts.entry_block_visited || function.entry == Block_Id(0)
	}
	return true
}

inspection_walk_visit_block :: proc(
	visitor: ^Walk_Visitor,
	module: ^Module,
	function_id: Function_Id,
	function: ^Function,
	block_id: Block_Id,
	block: ^Block,
) -> bool {
	_ = module
	_ = function_id
	_ = function
	counts := cast(^Inspection_Walk_Counts)visitor.data
	counts.blocks += 1
	if block_id == Block_Id(0) && block.name == "entry" {
		counts.entry_block_visited = true
	}
	return true
}

inspection_walk_visit_op :: proc(
	visitor: ^Walk_Visitor,
	module: ^Module,
	function_id: Function_Id,
	function: ^Function,
	block_id: Block_Id,
	block: ^Block,
	op: ^Op,
) -> bool {
	_ = module
	_ = function_id
	_ = function
	_ = block_id
	_ = block
	counts := cast(^Inspection_Walk_Counts)visitor.data
	counts.ops += 1
	if !counts.first_op_seen {
		counts.first_op = op.kind
		counts.first_op_seen = true
	}
	counts.last_op = op.kind
	return true
}

inspection_walk_visit_terminator :: proc(
	visitor: ^Walk_Visitor,
	module: ^Module,
	function_id: Function_Id,
	function: ^Function,
	block_id: Block_Id,
	block: ^Block,
	term: ^Terminator,
) -> bool {
	_ = module
	_ = function_id
	_ = function
	_ = block_id
	_ = block
	counts := cast(^Inspection_Walk_Counts)visitor.data
	counts.terminators += 1
	counts.last_terminator = term.kind
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
		visit_function   = inspection_walk_visit_function,
		visit_block      = inspection_walk_visit_block,
		visit_op         = inspection_walk_visit_op,
		visit_terminator = inspection_walk_visit_terminator,
		data             = rawptr(&counts),
	}
	testing.expect(t, walk_module(&visitor, &module))
	testing.expect_value(t, counts.functions, 1)
	testing.expect_value(t, counts.blocks, 1)
	testing.expect_value(t, counts.ops, 3)
	testing.expect_value(t, counts.terminators, 1)
	testing.expect(t, counts.entry_block_visited)
	testing.expect_value(t, counts.first_op, Op_Kind.Core_Const)
	testing.expect_value(t, counts.last_op, Op_Kind.Core_Load)
	testing.expect_value(t, counts.last_terminator, Terminator_Kind.Return)
}

Inspection_Stop_Counts :: struct {
	functions: int,
	ops:       int,
	stopped:   bool,
}

inspection_stop_visit_function :: proc(
	visitor: ^Walk_Visitor,
	module: ^Module,
	function_id: Function_Id,
	function: ^Function,
) -> bool {
	_ = module
	_ = function_id
	_ = function
	counts := cast(^Inspection_Stop_Counts)visitor.data
	counts.functions += 1
	return true
}

inspection_stop_visit_op :: proc(
	visitor: ^Walk_Visitor,
	module: ^Module,
	function_id: Function_Id,
	function: ^Function,
	block_id: Block_Id,
	block: ^Block,
	op: ^Op,
) -> bool {
	_ = module
	_ = function_id
	_ = function
	_ = block_id
	_ = block
	counts := cast(^Inspection_Stop_Counts)visitor.data
	counts.ops += 1
	if .Unsupported in op.flags {
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
		visit_op       = inspection_stop_visit_op,
		data           = rawptr(&counts),
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
	testing.expect_value(t, op.kind, Op_Kind.Core_Const)

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
	bad_world_operands := [?]Value_Id{function.world_param, value}
	bad_world_types := [?]Type_Id{BUILTIN_TYPE_WORLD}
	builder_emit_op(
		&bad_world,
		.Core_Store,
		bad_world_operands[:],
		bad_world_types[:],
		{.Reads_World, .Writes_World},
		Op_Payload{slot = slot},
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
	builder_emit_op(&builder, .Abap_Add, bad_operands[:], bad_types[:])
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
	function.blocks[int(function.entry)].params[0].value = Value_Id(999)
	function.values[int(value)].op = INVALID_OP_ID
	function.op_locations[0].index = u32(999)

	bad_slot := builder_begin_function(&module, "bad_slot")
	operands := [?]Value_Id{bad_slot.current_world}
	result_types := [?]Type_Id{BUILTIN_TYPE_INTEGER}
	builder_emit_op(
		&bad_slot,
		.Core_Load,
		operands[:],
		result_types[:],
		{.Reads_World},
		Op_Payload{slot = Slot_Id(999)},
		source,
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
builder_verifier_rejects_unsupported_without_message_or_source :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	with_source := Source_Loc{range = tokenizer.text_range(10, 20)}
	missing_message := builder_begin_function(&module, "missing_message")
	builder_emit_effect_op(
		&missing_message,
		.Core_Unsupported,
		flags = {.Reads_World, .Writes_World, .May_Trap, .Unsupported},
		payload = Op_Payload{},
		source = with_source,
	)
	builder_set_return_world(&missing_message)

	missing_source := builder_begin_function(&module, "missing_source")
	builder_emit_effect_op(
		&missing_source,
		.Core_Unsupported,
		flags = {.Reads_World, .Writes_World, .May_Trap, .Unsupported},
		payload = Op_Payload{unsupported_message = "missing source"},
	)
	builder_set_return_world(&missing_source)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "core.unsupported operation must carry unsupported message"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "core.unsupported operation must carry source provenance"))
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
	testing.expect(t, strings.contains(text, "slot %s0 global lv_source : !i"))
	testing.expect(t, strings.contains(text, "slot %s1 global lv_target : !i"))
	testing.expect(t, strings.contains(text, "core.load %s0"))
	testing.expect(t, strings.contains(text, "core.store %s1"))
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
	testing.expect(t, strings.contains(text, "core.call @start_of_selection"))
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
		_, store_ok := lower_test_first_op_by_kind(load, .Core_Store)
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
	call, call_ok := lower_test_first_op_by_kind(start, .Abap_Routine_Call)
	testing.expect(t, call_ok)
	if !call_ok {
		return
	}
	testing.expect_value(t, call.payload.call_kind, Abap_Call_Kind.Form)
	testing.expect_value(t, len(call.results), 2)
	if len(call.results) == 2 {
		testing.expect_value(t, value_type(start, call.results[1]), BUILTIN_TYPE_INTEGER)
	}

	store_found := false
	for block in start.blocks {
		for op in block.ops {
			if op.kind == .Core_Store && len(op.operands) > 1 && op.operands[1] == call.results[1] {
				store_found = true
			}
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
	testing.expect(t, strings.contains(text, "abap.method_call @lcl_accumulator.add"))
	testing.expect(t, !strings.contains(text, "abap.method_call @lcl_accumulator.add call=method"))
	testing.expect(t, !strings.contains(text, "abap.method_call @lcl_accumulator.add target=add"))
	testing.expect(t, !strings.contains(text, "receiver="))
	testing.expect(t, strings.contains(text, ".case_when"))
	testing.expect(t, strings.contains(text, "abap.or"))
	testing.expect(t, strings.contains(text, "abap.write"))
	testing.expect(t, !strings.contains(text, "core.unsupported"))
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
	testing.expect(t, strings.contains(text, "lo_inline : !ref:lcl_class"))
	testing.expect(t, strings.contains(text, "lo_old : !ref:lcl_class"))
	testing.expect(t, strings.contains(text, ": !ref:lcl_class = abap.construct @new"))

	function := lower_test_primary_source_function(&result.module)
	explicit_new_offset := strings.index(source, "NEW lcl_class")
	inferred_new_offset := strings.index(source, "NEW #")
	testing.expect(t, explicit_new_offset >= 0)
	testing.expect(t, inferred_new_offset >= 0)
	explicit_new, explicit_new_ok := lower_test_op_by_kind_at_start(function, .Abap_Construct, explicit_new_offset)
	inferred_new, inferred_new_ok := lower_test_op_by_kind_at_start(function, .Abap_Construct, inferred_new_offset)
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
	construct, construct_ok := lower_test_op_by_kind_at_start(function, .Abap_Construct, create_offset)
	testing.expect(t, construct_ok)
	if construct_ok && len(construct.results) > 0 {
		construct_type := type_ptr(module, value_type(function, construct.results[0]))
		testing.expect_value(t, construct_type.kind, Type_Kind.Reference)
		testing.expect_value(t, construct_type.name, "ref:lcl_child")
	}

	constructor_call_ok := false
	for block in function.blocks {
		for op in block.ops {
			if op.kind != .Abap_Method_Call || op.payload.callee_name != "lcl_child.constructor" {
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
	testing.expect(t, strings.contains(text, "core.unsupported"))
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
	testing.expect(t, strings.contains(text, "global lv : !abap_bool"))
	testing.expect(t, strings.contains(text, "abap.eq"))
	testing.expect(t, strings.contains(text, ": !predicate"))
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
	testing.expect(t, strings.contains(text, "global lv_c : !c(10)"))
	testing.expect(t, strings.contains(text, "global lv_s : !string"))
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
	testing.expect(t, strings.contains(text, "abap.builtin_call @strlen"))
	testing.expect(t, strings.contains(text, "abap.construct @value"))
	testing.expect(t, strings.contains(text, "core.cast"))

	function := lower_test_primary_source_function(&result.module)
	template_offset := strings.index(source, "|hello")
	strlen_offset := strings.index(source, "strlen")
	value_offset := strings.index(source, "VALUE")
	conv_offset := strings.index(source, "CONV")
	testing.expect(t, template_offset >= 0)
	testing.expect(t, strlen_offset >= 0)
	testing.expect(t, value_offset >= 0)
	testing.expect(t, conv_offset >= 0)

	template_op, template_ok := lower_test_op_by_kind_at_start(function, .Abap_String_Template, template_offset)
	builtin_op, builtin_ok := lower_test_first_op_by_kind(function, .Abap_Builtin_Call)
	construct_op, construct_ok := lower_test_first_op_by_kind(function, .Abap_Construct)
	cast_op, cast_ok := lower_test_first_op_by_kind(function, .Core_Cast)
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
		testing.expect_value(t, builtin_op.payload.callee_name, "strlen")
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
	testing.expect(t, strings.contains(text, "abap.method_call @lcl_demo.get"))
	testing.expect(t, strings.contains(text, "abap.method_call @lcl_demo.stat"))
	testing.expect(t, !strings.contains(text, "abap.method_call @lcl_demo.get call=method"))
	testing.expect(t, !strings.contains(text, "abap.method_call @lcl_demo.stat call=method"))
	testing.expect(t, !strings.contains(text, "abap.method_call @lcl_demo.get target=get"))
	testing.expect(t, !strings.contains(text, "abap.method_call @lcl_demo.stat target=stat"))
	testing.expect(t, !strings.contains(text, "receiver="))
	testing.expect(t, strings.contains(text, "abap.routine_call @sub"))
	testing.expect(t, !strings.contains(text, "abap.routine_call @sub call=form"))
	testing.expect(t, !strings.contains(text, "abap.routine_call @sub target=sub"))
	testing.expect(t, strings.contains(text, `"unresolved or dynamic CALL METHOD target"`))

	function := lower_test_primary_source_function(&result.module)
	method_count := 0
	instance_count := 0
	static_count := 0
	routine_count := 0
	for block in function.blocks {
		for op in block.ops {
			#partial switch op.kind {
			case .Abap_Method_Call:
				method_count += 1
				testing.expect(t, op.payload.call_target != nil)
				if op.payload.call_target != nil {
					testing.expect_value(t, op.payload.call_target.kind, semantic.Entity_Kind.Method)
				}
				if op.payload.callee_name == "lcl_demo.get" {
					instance_count += 1
					testing.expect(t, len(op.operands) >= 2)
					if len(op.operands) >= 2 {
						receiver_type := value_type(function, op.operands[1])
						testing.expect(t, receiver_type != INVALID_TYPE_ID)
						if receiver_type != INVALID_TYPE_ID {
							testing.expect_value(t, type_ptr(&result.module, receiver_type).kind, Type_Kind.Reference)
						}
					}
				} else if op.payload.callee_name == "lcl_demo.stat" {
					static_count += 1
					testing.expect_value(t, len(op.operands), 1)
				}
				if op.payload.callee_name == "lcl_demo.get" {
					testing.expect_value(t, len(op.results), 2)
					testing.expect(t, value_type(function, op.results[1]) != BUILTIN_TYPE_UNKNOWN)
				}
			case .Abap_Routine_Call:
				routine_count += 1
				testing.expect_value(t, op.payload.call_kind, Abap_Call_Kind.Form)
				testing.expect(t, op.payload.call_target != nil)
				if op.payload.call_target != nil {
					testing.expect_value(t, op.payload.call_target.kind, semantic.Entity_Kind.Form)
				}
			}
		}
	}
	testing.expect_value(t, method_count, 2)
	testing.expect_value(t, instance_count, 1)
	testing.expect_value(t, static_count, 1)
	testing.expect_value(t, routine_count, 1)
	testing.expect(t, lower_test_has_unsupported_with_source(function, "unresolved or dynamic CALL METHOD target"))
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

	total_field_load, total_field_load_ok := lower_test_first_op_by_kind(total, .Core_Field_Load)
	add_field_store, add_field_store_ok := lower_test_first_op_by_kind(add, .Core_Field_Store)
	testing.expect(t, total_field_load_ok)
	testing.expect(t, add_field_store_ok)
	if total_field_load_ok {
		testing.expect_value(t, total_field_load.payload.field_name, "mv_total")
	}
	if add_field_store_ok {
		testing.expect_value(t, add_field_store.payload.field_name, "mv_total")
	}

	implicit_call_ok := false
	for block in repeat_total.blocks {
		for op in block.ops {
			if op.kind == .Abap_Method_Call && op.payload.callee_name == "lcl_accumulator.total" {
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
MESSAGE ID zmsg TYPE lv_type NUMBER lv_no WITH lv_text DISPLAY LIKE lv_type.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)
	result := &fixture.lowered

	text := print_module(&result.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect(t, strings.contains(text, "abap.message form=compact id=zmsg type=e number=001 args=1 into display_like"))
	testing.expect(t, strings.contains(text, "raising=cx_msg"))
	testing.expect(t, strings.contains(text, "abap.message form=explicit id=zmsg head_operands=2 args=1 display_like=operand"))

	function := lower_test_primary_source_function(&result.module)
	message_count := 0
	for block in function.blocks {
		for op in block.ops {
			if op.kind != .Abap_Message {
				continue
			}
			message_count += 1
			if message_count == 1 {
				testing.expect_value(t, op.payload.message_form, Abap_Message_Form.Compact)
				testing.expect_value(t, op.payload.message_id, "zmsg")
				testing.expect_value(t, op.payload.message_type, "e")
				testing.expect_value(t, op.payload.message_number, "001")
				testing.expect_value(t, op.payload.message_arg_count, 1)
				testing.expect(t, op.payload.message_has_into)
				testing.expect(t, op.payload.message_has_display_like)
				testing.expect(t, op.payload.message_has_raising)
				testing.expect_value(t, len(op.results), 2)
			} else if message_count == 2 {
				testing.expect_value(t, op.payload.message_form, Abap_Message_Form.Explicit)
				testing.expect_value(t, op.payload.message_id, "zmsg")
				testing.expect_value(t, op.payload.message_head_operands, 2)
				testing.expect_value(t, op.payload.message_arg_count, 1)
				testing.expect(t, op.payload.message_display_like_operand)
			}
		}
	}
	testing.expect_value(t, message_count, 2)
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
	testing.expect(t, strings.contains(text, "core.store"))

	function := lower_test_primary_source_function(&result.module)
	clear_offset := strings.index(source, "CLEAR")
	refresh_offset := strings.index(source, "REFRESH")
	free_offset := strings.index(source, "FREE")
	clear_op, clear_ok := lower_test_first_op_by_kind(function, .Abap_Clear)
	refresh_op, refresh_ok := lower_test_first_op_by_kind(function, .Abap_Refresh)
	free_op, free_ok := lower_test_first_op_by_kind(function, .Abap_Free)
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
	testing.expect(t, strings.contains(text, "core.field_load .count"))
	testing.expect(t, strings.contains(text, "core.field_store .count"))
	testing.expect(t, strings.contains(text, "system.read .subrc"))
	testing.expect(t, strings.contains(text, "abap.assign_field"))
	testing.expect(t, strings.contains(text, "abap.unassign"))
	testing.expect(t, strings.contains(text, "system.write .subrc"))

	function := lower_test_primary_source_function(&result.module)
	field_load, field_load_ok := lower_test_first_op_by_kind(function, .Core_Field_Load)
	nested_field_load, nested_field_load_ok := lower_test_field_op_by_name(function, .Core_Field_Load, "amount")
	system_read, system_read_ok := lower_test_op_by_kind_at_start(
		function,
		.System_Read,
		strings.index(source, "sy-subrc."),
	)
	system_write_assignment, system_write_assignment_ok := lower_test_op_by_kind_at_start(
		function,
		.System_Write,
		strings.index(source, "sy-subrc ="),
	)
	assign_field, assign_field_ok := lower_test_first_op_by_kind(function, .Abap_Assign_Field)
	unassign, unassign_ok := lower_test_first_op_by_kind(function, .Abap_Unassign)
	testing.expect(t, field_load_ok)
	testing.expect(t, nested_field_load_ok)
	testing.expect(t, system_read_ok)
	testing.expect(t, system_write_assignment_ok)
	testing.expect(t, assign_field_ok)
	testing.expect(t, unassign_ok)
	if field_load_ok {
		testing.expect_value(t, field_load.payload.field_name, "count")
		testing.expect(t, field_load.payload.has_projection)
		if field_load.payload.has_projection {
			projection := projection_ptr(function, field_load.payload.projection)
			testing.expect_value(t, len(projection.segments), 1)
			if len(projection.segments) == 1 {
				testing.expect_value(t, projection.segments[0].name, "count")
				testing.expect_value(t, projection.segments[0].field_index, i32(0))
			}
		}
		testing.expect(t, value_type(function, field_load.results[0]) != BUILTIN_TYPE_UNKNOWN)
	}
	if nested_field_load_ok {
		testing.expect(t, nested_field_load.payload.has_projection)
		if nested_field_load.payload.has_projection {
			projection := projection_ptr(function, nested_field_load.payload.projection)
			testing.expect_value(t, len(projection.segments), 2)
			if len(projection.segments) == 2 {
				testing.expect_value(t, projection.segments[0].name, "inner")
				testing.expect_value(t, projection.segments[0].field_index, i32(1))
				testing.expect_value(t, projection.segments[1].name, "amount")
				testing.expect_value(t, projection.segments[1].field_index, i32(0))
			}
		}
	}
	if system_read_ok {
		testing.expect_value(t, system_read.payload.system_field, "subrc")
		testing.expect(t, value_type(function, system_read.results[0]) != BUILTIN_TYPE_UNKNOWN)
	}
	if system_write_assignment_ok {
		testing.expect_value(t, system_write_assignment.payload.system_field, "subrc")
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
	for block in function.blocks {
		for op in block.ops {
			if op.kind == .Core_Load {
				if op.payload.slot != value_slot_id {
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
			} else if op.kind == .Core_Store {
				if op.payload.slot != copy_slot_id {
					continue
				}
				store_found = true
				testing.expect(t, op.source.file == copy_slot.entity.source_file)
				testing.expect(t, op.source.node != nil)
				testing.expect_value(t, op.source.range.start, copy_assign_offset)
				testing.expect_value(t, op.source.range.end, copy_assign_offset + len("lv_copy"))
			}
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
	builder_emit_op(&unsupported, .Core_Unsupported)
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
	builder_emit_effect_op(
		&builder,
		.Abap_Method_Call,
		payload = Op_Payload {
			callee_name = "run",
			call_kind   = .Method,
		},
	)
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "resolved call operation must carry semantic target"))
}

@(test)
builder_verifier_rejects_instance_method_call_without_receiver :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	owner := semantic.Entity{kind = .Class}
	method := semantic.Entity{kind = .Method, owner = &owner}
	builder := builder_begin_function(&module, "bad_instance_call")
	builder_emit_method_call(&builder, &method, "lcl_demo.run")
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "instance method call must carry receiver operand"))
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
	builder_emit_effect_op(&builder, .Table_Read, inputs[:], result_types[:])
	builder_set_return_world(&builder)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "table operation must carry access mode"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "table operation must carry valid row type"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "table read operation must carry result mode"))
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
		Op_Payload {
			message_form = .Compact,
			message_id = "zmsg",
			message_type = "e",
			message_number = "001",
			message_arg_count = 1,
			message_has_into = true,
			message_has_display_like = true,
			message_display_like = "i",
		},
	)
	testing.expect(t, result != INVALID_VALUE_ID)
	builder_set_return_world(&valid)

	invalid := builder_begin_function(&module, "bad_message")
	builder_emit_message(&invalid, payload = Op_Payload{message_has_display_like = true})
	builder_set_return_world(&invalid)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "message operation must carry message form"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "message DISPLAY LIKE must carry static text or dynamic operand"))
}

@(test)
builder_verifier_covers_sql_domain_payloads :: proc(t: ^testing.T) {
	module := module_make(context.allocator)
	defer module_destroy(&module)

	query := ast.Select_Query_Clause{}
	valid := builder_begin_function(&module, "valid_sql")
	sql_payload := Op_Payload {
		sql_query = &query,
		sql_source_kind = .Internal,
		sql_result_kind = .Into,
		sql_row_type = BUILTIN_TYPE_INTEGER,
		sql_scalar_type = BUILTIN_TYPE_INTEGER,
		sql_source_count = 1,
		sql_projection_count = 1,
	}
	builder_emit_sql_select(&valid, BUILTIN_TYPE_INTEGER, sql_payload)
	builder_emit_sql_mutation(
		&valid,
		.Sql_Insert,
		payload = Op_Payload {
			sql_source_kind = .Internal,
			sql_row_type = BUILTIN_TYPE_INTEGER,
			sql_source_count = 1,
		},
	)
	builder_set_return_world(&valid)

	invalid := builder_begin_function(&module, "bad_sql")
	builder_emit_sql_select(&invalid, BUILTIN_TYPE_INTEGER, Op_Payload{sql_query = &query})
	builder_set_return_world(&invalid)

	verify := verify_module(&module, context.allocator)
	defer verify_result_destroy(&verify)
	testing.expect(t, !verify.ok)
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "SQL operation must carry source kind"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "SQL operation must carry valid row type"))
	testing.expect(t, verify_has_diagnostic_message(verify, .Bad_Op_Signature, "SQL query operation must carry projection count"))
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
	testing.expect(t, strings.contains(text, "table.iter access=sequential result=into source=row row=!ty_row"))
	testing.expect(t, strings.contains(text, ": !table_iter = table.iter"))
	testing.expect(t, strings.contains(text, "iter : !table_iter"))
	testing.expect(t, !strings.contains(text, "!unknown"))
	testing.expect(t, strings.contains(text, "table.read access=key key=free result=into row=!ty_row components=1"))
	testing.expect(t, strings.contains(text, "table.sort access=sort row=!ty_row components=1 stable"))

	function := lower_test_primary_source_function(&result.module)
	iter_op, iter_ok := lower_test_first_op_by_kind(function, .Table_Iter)
	read_op, read_ok := lower_test_first_op_by_kind(function, .Table_Read)
	sort_op, sort_ok := lower_test_first_op_by_kind(function, .Table_Sort)
	testing.expect(t, iter_ok)
	testing.expect(t, read_ok)
	testing.expect(t, sort_ok)
	if iter_ok {
		testing.expect_value(t, iter_op.payload.table_access, Table_Access_Kind.Sequential)
		testing.expect_value(t, iter_op.payload.table_result_kind, Table_Result_Kind.Into)
		testing.expect(t, iter_op.payload.table_row_type != BUILTIN_TYPE_UNKNOWN)
		testing.expect(t, len(iter_op.results) > 1)
		if len(iter_op.results) > 1 {
			testing.expect_value(t, value_type(function, iter_op.results[1]), BUILTIN_TYPE_TABLE_ITERATOR)
		}
	}
	if read_ok {
		testing.expect_value(t, read_op.payload.table_access, Table_Access_Kind.Key)
		testing.expect_value(t, read_op.payload.table_key_kind, Table_Key_Kind.Free)
		testing.expect_value(t, read_op.payload.table_result_kind, Table_Result_Kind.Into)
		testing.expect_value(t, read_op.payload.table_component_count, 1)
		row_record, row_ok := module_type_record(&result.module, read_op.payload.table_row_type)
		testing.expect(t, row_ok)
		if row_ok {
			testing.expect_value(t, row_record.name, "ty_row")
			testing.expect(t, row_record.semantic_type != nil)
		}
	}
	if sort_ok {
		testing.expect_value(t, sort_op.payload.table_access, Table_Access_Kind.Sort)
		testing.expect_value(t, sort_op.payload.table_component_count, 1)
		testing.expect(t, sort_op.payload.table_stable)
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
	testing.expect(t, strings.contains(text, "sql.fetch"))
	testing.expect(t, strings.contains(text, "sql.insert source=resolved:zcust"))
	testing.expect(t, strings.contains(text, "sql.modify source=resolved:zcust"))
	testing.expect(t, strings.contains(text, "sql.update source=resolved:zcust"))
	testing.expect(t, strings.contains(text, "sql.delete source=resolved:zcust"))

	function := lower_test_primary_source_function(&result.module)
	select_op, select_ok := lower_test_first_op_by_kind(function, .Sql_Select)
	insert_op, insert_ok := lower_test_first_op_by_kind(function, .Sql_Insert)
	update_op, update_ok := lower_test_first_op_by_kind(function, .Sql_Update)
	delete_op, delete_ok := lower_test_first_op_by_kind(function, .Sql_Delete)
	testing.expect(t, select_ok)
	testing.expect(t, insert_ok)
	testing.expect(t, update_ok)
	testing.expect(t, delete_ok)
	if select_ok {
		testing.expect_value(t, select_op.payload.sql_source_kind, Sql_Source_Kind.Resolved)
		testing.expect_value(t, select_op.payload.sql_source_name, "zcust")
		testing.expect_value(t, select_op.payload.sql_result_kind, Sql_Result_Kind.Into_Table)
		testing.expect_value(t, select_op.payload.sql_projection_count, 2)
		testing.expect_value(t, select_op.payload.sql_source_count, 1)
		testing.expect(t, select_op.payload.sql_row_type != BUILTIN_TYPE_UNKNOWN)
		testing.expect(t, select_op.payload.sql_source_entity != nil)
	}
	if insert_ok {
		testing.expect(t, insert_op.payload.sql_from_table)
		testing.expect_value(t, insert_op.payload.sql_source_kind, Sql_Source_Kind.Resolved)
		testing.expect_value(t, insert_op.payload.sql_source_name, "zcust")
	}
	if update_ok {
		testing.expect_value(t, update_op.payload.sql_assignment_count, 1)
		testing.expect_value(t, update_op.payload.sql_source_kind, Sql_Source_Kind.Resolved)
	}
	if delete_ok {
		testing.expect_value(t, delete_op.payload.sql_source_kind, Sql_Source_Kind.Resolved)
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

lower_test_core_call_count :: proc(module: ^Module, function: ^Function) -> int {
	_ = module
	count := 0
	for block in function.blocks {
		for op in block.ops {
			if op.kind == .Core_Call {
				count += 1
			}
		}
	}
	return count
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
	for block in function.blocks {
		for op in block.ops {
			if op.kind != .Core_Call {
				continue
			}
			if call_index == index {
				if !op.payload.has_call_function_target {
					return "", false
				}
				target := op.payload.call_function_target
				if target == INVALID_FUNCTION_ID || int(target) >= len(module.functions) {
					return "", false
				}
				return module.functions[int(target)].name, true
			}
			call_index += 1
		}
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

lower_test_first_op_by_kind :: proc(function: ^Function, kind: Op_Kind) -> (^Op, bool) {
	for &block in function.blocks {
		for &op in block.ops {
			if op.kind == kind {
				return &op, true
			}
		}
	}
	return nil, false
}

lower_test_field_op_by_name :: proc(function: ^Function, kind: Op_Kind, field_name: string) -> (^Op, bool) {
	for &block in function.blocks {
		for &op in block.ops {
			if op.kind == kind && op.payload.field_name == field_name {
				return &op, true
			}
		}
	}
	return nil, false
}

lower_test_op_by_kind_at_start :: proc(function: ^Function, kind: Op_Kind, start: int) -> (^Op, bool) {
	for &block in function.blocks {
		for &op in block.ops {
			if op.kind == kind && op.source.range.start == start {
				return &op, true
			}
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
		#partial switch block.term.kind {
		case .Branch:
			if block.term.target == target && len(block.term.target_args) == arg_count {
				return true
			}
		case .Cond_Branch:
			if block.term.true_target == target && len(block.term.true_args) == arg_count {
				return true
			}
			if block.term.false_target == target && len(block.term.false_args) == arg_count {
				return true
			}
		}
	}
	return false
}

lower_test_has_unsupported_with_source :: proc(function: ^Function, message: string) -> bool {
	for block in function.blocks {
		for op in block.ops {
			if op.kind == .Core_Unsupported &&
			   .Unsupported in op.flags &&
			   op.payload.unsupported_message == message &&
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
