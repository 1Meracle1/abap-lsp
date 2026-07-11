package abap_frontend_vm

import ir "src:ir"
import "src:parser"
import runtime "src:vm/runtime"
import semantic "src:semantic"

import "core:os"
import "core:strings"
import "core:testing"

@(test)
prepare_module_rejects_invalid_ir :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	ir.module_add_entry(&module, builder.function_id)

	prepared := prepare_module(&module, context.allocator)
	defer prepare_result_destroy(&prepared)
	testing.expect(t, !prepared.ok)
	testing.expect_value(t, prepared.message, "block is missing terminator")
}

@(test)
prepare_module_copies_field_projection_and_source_metadata :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         count TYPE i,
       END OF ty_row.
DATA ls_row TYPE ty_row.
ls_row-count = 7.
WRITE ls_row-count.`
	parsed := parser.parse(source, "mem://prepare_boundary.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	if len(parsed.errors) != 0 {
		return
	}

	project := semantic.project_make()
	checker := semantic.checker_make(&project)
	file := semantic.checker_add_file(&checker, parsed.path, parsed.root)
	semantic.checker_check_file(&checker, file)
	lowered := ir.lower_project(&project, &checker, context.allocator)
	prepared := prepare_module(&lowered.module, context.allocator)
	ir.lower_result_destroy(&lowered)
	semantic.project_destroy(&project)
	defer prepare_result_destroy(&prepared)

	testing.expect(t, prepared.ok)
	if !prepared.ok {
		return
	}
	found_field := false
	found_source := false
	for function in prepared.module.functions {
		for field in function.fields {
			if field.name == "count" && field.field_index == 0 && len(field.projection) == 1 {
				testing.expect_value(t, field.projection[0].name, "count")
				testing.expect_value(t, field.projection[0].field_index, i32(0))
				testing.expect(t, field.result_type != nil)
				testing.expect_value(t, field.result_type_name, "i")
				found_field = true
			}
		}
		for instruction in function.instructions {
			if instruction.source.path == "mem://prepare_boundary.abap" &&
			   instruction.source.range.end > instruction.source.range.start {
				found_source = true
			}
		}
	}
	testing.expect(t, found_field)
	testing.expect(t, found_source)
}

@(test)
execute_module_returns_owned_source_provenance :: proc(t: ^testing.T) {
	run := runtime_test_execute_source(t, `MESSAGE 'owned source'.`)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) == 1 {
		testing.expect_value(t, run.events[0].source.path, "mem://runtime_test.abap")
	}
}

@(test)
intrinsic_dispatcher_can_read_operands_and_set_results :: proc(t: ^testing.T) {
	module := runtime_test_scalar_write_module()
	defer ir.module_destroy(&module)
	run := execute_module(
		&module,
		Run_Options {
			dispatcher = Intrinsic_Dispatcher{dispatch = runtime_test_dispatch_add},
		},
		context.allocator,
	)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "gv_total", 42))
}

@(test)
vm_reclaims_scratch_allocations_after_each_execution_cycle :: proc(t: ^testing.T) {
	module := runtime_test_scalar_write_module()
	defer ir.module_destroy(&module)
	prepared := prepare_module(&module, context.allocator)
	defer prepare_result_destroy(&prepared)
	testing.expect(t, prepared.ok)
	if !prepared.ok {
		return
	}
	vm := vm_make_prepared(&prepared.module, {}, context.allocator)
	defer vm_destroy(&vm)
	for !vm_is_finished(&vm) {
		vm_step(&vm)
		testing.expect(t, !vm.scratch_active)
		testing.expect_value(t, vm.scratch_arena.temp_count, uint(0))
		testing.expect_value(t, vm.scratch_arena.total_used, uint(0))
	}
	testing.expect_value(t, vm.state, VM_State.Completed)
	run := run_result_from_vm(&vm, context.allocator)
	defer run_result_destroy(&run)
	testing.expect_value(t, vm.scratch_arena.total_used, uint(0))
}

@(test)
runtime_executes_scalar_intrinsics_and_captured_write :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	function := ir.builder_function(&builder)
	slot := ir.function_add_slot(function, .Global, "gv_total", ir.BUILTIN_TYPE_INTEGER)
	one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	two := ir.builder_emit_const(&builder, "2", ir.BUILTIN_TYPE_INTEGER)
	inputs := [?]ir.Value_Id{one, two}
	result_types := [?]ir.Type_Id{ir.BUILTIN_TYPE_INTEGER}
	add := ir.builder_emit_intrinsic(&builder, .ABAP_Add, inputs[:], result_types[:])
	sum := ir.op_ptr(function, add).results[0]
	ir.builder_emit_store(&builder, slot, sum)
	loaded := ir.builder_emit_load(&builder, slot)
	write_inputs := [?]ir.Value_Id{loaded}
	ir.builder_emit_write(&builder, write_inputs[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].kind, runtime.IO_Event_Kind.Write)
		testing.expect_value(t, run.events[0].text, "3")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "gv_total", 3))
}

@(test)
runtime_executes_verified_core_add_cmp_select_ir :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	function := ir.builder_function(&builder)
	slot := ir.function_add_slot(function, .Global, "gv_selected", ir.BUILTIN_TYPE_INTEGER)

	one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	two := ir.builder_emit_const(&builder, "2", ir.BUILTIN_TYPE_INTEGER)
	add_operands := [?]ir.Value_Id{one, two}
	add_results := [?]ir.Type_Id{ir.BUILTIN_TYPE_INTEGER}
	add := ir.builder_emit_op(&builder, .Add, add_operands[:], add_results[:])
	sum := ir.op_ptr(function, add).results[0]

	three := ir.builder_emit_const(&builder, "3", ir.BUILTIN_TYPE_INTEGER)
	cmp_operands := [?]ir.Value_Id{sum, three}
	cmp_results := [?]ir.Type_Id{ir.BUILTIN_TYPE_PREDICATE}
	cmp := ir.builder_emit_op(
		&builder,
		.Cmp,
		cmp_operands[:],
		cmp_results[:],
		attrs = ir.Compare_Attrs{predicate = .EQ, mode = .Signed_Integer},
	)
	is_three := ir.op_ptr(function, cmp).results[0]

	fallback := ir.builder_emit_const(&builder, "99", ir.BUILTIN_TYPE_INTEGER)
	select_operands := [?]ir.Value_Id{is_three, sum, fallback}
	select_results := [?]ir.Type_Id{ir.BUILTIN_TYPE_INTEGER}
	selected_op := ir.builder_emit_op(&builder, .Select, select_operands[:], select_results[:])
	selected := ir.op_ptr(function, selected_op).results[0]

	ir.builder_emit_store(&builder, slot, selected)
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	verify := ir.verify_module(&module, context.allocator)
	defer ir.verify_result_destroy(&verify)
	testing.expect(t, verify.ok)
	if !verify.ok {
		return
	}

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "gv_selected", 3))
}

@(test)
prepare_module_rejects_verified_non_executable_core_opcode :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	ref_type := ir.module_reference_type(&module, ir.BUILTIN_TYPE_INTEGER)
	result_types := [?]ir.Type_Id{ref_type}
	ir.builder_emit_op(&builder, .Global_Addr, result_types = result_types[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	verify := ir.verify_module(&module, context.allocator)
	defer ir.verify_result_destroy(&verify)
	testing.expect(t, verify.ok)
	if !verify.ok {
		return
	}

	prepared := prepare_module(&module, context.allocator)
	defer prepare_result_destroy(&prepared)
	testing.expect(t, !prepared.ok)
	testing.expect_value(t, prepared.message, "VM executable IR does not support global_addr opcode")

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Trapped)
	testing.expect_value(t, run.trap.kind, runtime.Trap_Kind.Invalid_Module)
	testing.expect_value(t, run.trap.message, "VM executable IR does not support global_addr opcode")
	testing.expect_value(t, run.instruction_count, u64(0))
}

@(test)
runtime_vm_exposes_in_memory_state_for_stepping_and_snapshot :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	function := ir.builder_function(&builder)
	slot := ir.function_add_slot(function, .Global, "gv_total", ir.BUILTIN_TYPE_INTEGER)
	one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	two := ir.builder_emit_const(&builder, "2", ir.BUILTIN_TYPE_INTEGER)
	inputs := [?]ir.Value_Id{one, two}
	result_types := [?]ir.Type_Id{ir.BUILTIN_TYPE_INTEGER}
	add := ir.builder_emit_intrinsic(&builder, .ABAP_Add, inputs[:], result_types[:])
	sum := ir.op_ptr(function, add).results[0]
	ir.builder_emit_store(&builder, slot, sum)
	loaded := ir.builder_emit_load(&builder, slot)
	write_inputs := [?]ir.Value_Id{loaded}
	ir.builder_emit_write(&builder, write_inputs[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	prepared := prepare_module(&module, context.allocator)
	defer prepare_result_destroy(&prepared)
	testing.expect(t, prepared.ok)
	if !prepared.ok {
		return
	}

	vm := vm_make_prepared(&prepared.module, Run_Options{step_limit = 32, io_policy = runtime.io_policy_captured()}, context.allocator)
	defer vm_destroy(&vm)
	testing.expect_value(t, vm.state, VM_State.Ready)
	testing.expect_value(t, runtime.value_kind(vm.runtime_context.world), runtime.Value_Kind.World)
	testing.expect_value(t, vm_start(&vm), VM_State.Running)
	testing.expect_value(t, len(vm.frames), 1)
	if len(vm.frames) > 0 {
		testing.expect(t, len(vm.frames[0].registers) > 0)
		testing.expect(t, len(vm.frames[0].slots) > 0)
	}

	testing.expect_value(t, vm_step(&vm), VM_State.Running)
	testing.expect_value(t, vm.instruction_count, u64(1))
	testing.expect(t, len(vm.frames) > 0 && runtime_test_frame_has_int(&vm.frames[0], 1))

	testing.expect_value(t, vm_run_until_complete(&vm), VM_State.Completed)
	testing.expect_value(t, len(vm.runtime_context.events), 1)
	if len(vm.runtime_context.events) > 0 {
		testing.expect_value(t, vm.runtime_context.events[0].text, "3")
	}
	total := runtime.context_global_read(&vm.runtime_context, "gv_total")
	testing.expect_value(t, runtime.value_kind(total), runtime.Value_Kind.Integer)
	testing.expect_value(t, runtime.value_int(total), i64(3))

	run := run_result_from_vm(&vm, context.allocator)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, run.instruction_count, vm.instruction_count)
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "gv_total", 3))
}

@(test)
runtime_executes_source_report_through_canonical_dispatch :: proc(t: ^testing.T) {
	source := `DATA LV TYPE I VALUE 1.
LV = LV + 2.
IF LV = 3.
  WRITE LV.
ENDIF.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].text, "3")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv", 3))
}

@(test)
runtime_executes_typed_numeric_arithmetic :: proc(t: ^testing.T) {
	source := `DATA lv_p TYPE p LENGTH 8 DECIMALS 2 VALUE 10.
lv_p = lv_p / 4.
WRITE lv_p.
DATA lv_div TYPE i VALUE 11.
lv_div = lv_div DIV 4.
WRITE lv_div.
DATA lv_mod TYPE i VALUE 11.
lv_mod = lv_mod MOD 4.
WRITE lv_mod.
DATA lv_f TYPE f VALUE 1.
lv_f = lv_f + 2.
WRITE lv_f.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 4)
	if len(run.events) == 4 {
		testing.expect_value(t, run.events[0].text, "2.50")
		testing.expect_value(t, run.events[1].text, "2")
		testing.expect_value(t, run.events[2].text, "3")
		testing.expect_value(t, run.events[3].text, "3")
	}
}

@(test)
runtime_executes_local_try_catch_exception_flow :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string VALUE 'start'.
TRY.
  RAISE EXCEPTION TYPE cx_root.
  lv_text = 'miss'.
CATCH cx_root INTO DATA(lx_error).
  lv_text = 'hit'.
ENDTRY.
WRITE lv_text.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].text, "hit")
	}
	testing.expect(t, runtime_test_named_value_is_string(run.final_values[:], "global", "lv_text", "hit"))
}

@(test)
runtime_traps_unhandled_local_exception :: proc(t: ^testing.T) {
	source := `RAISE EXCEPTION TYPE cx_root.
WRITE 'miss'.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Trapped)
	testing.expect_value(t, run.trap.kind, runtime.Trap_Kind.Exception)
	testing.expect(t, strings.contains(run.trap.message, "cx_root"))
	testing.expect_value(t, len(run.events), 0)
}

@(test)
runtime_traps_unhandled_exception_with_abap_stack_trace :: proc(t: ^testing.T) {
	source := `FORM layer_one.
  PERFORM layer_two.
ENDFORM.

FORM layer_two.
  PERFORM layer_three.
ENDFORM.

FORM layer_three.
  RAISE EXCEPTION TYPE cx_root.
ENDFORM.

START-OF-SELECTION.
  PERFORM layer_one.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Trapped)
	testing.expect(t, strings.contains(run.trap.message, "cx_root"))
	testing.expect_value(t, len(run.stack_trace), 4)
	if len(run.stack_trace) == 4 {
		testing.expect_value(t, run.stack_trace[0].name, "layer_three")
		testing.expect_value(t, run.stack_trace[1].name, "layer_two")
		testing.expect_value(t, run.stack_trace[2].name, "layer_one")
		testing.expect(t, strings.contains(run.stack_trace[3].name, "start_of_selection"))

		raise_offset := strings.index(source, "RAISE EXCEPTION TYPE cx_root")
		layer_three_call := strings.index(source, "PERFORM layer_three")
		layer_two_call := strings.index(source, "PERFORM layer_two")
		layer_one_call := strings.index(source, "PERFORM layer_one")
		testing.expect_value(t, run.stack_trace[0].source.range.start, raise_offset)
		testing.expect_value(t, run.stack_trace[1].source.range.start, layer_three_call)
		testing.expect_value(t, run.stack_trace[2].source.range.start, layer_two_call)
		testing.expect_value(t, run.stack_trace[3].source.range.start, layer_one_call)
	}
}

@(test)
runtime_executes_statement_string_operations :: proc(t: ^testing.T) {
	source := `DATA lv_a TYPE string VALUE 'A'.
DATA lv_b TYPE string VALUE 'b'.
DATA lv_sep TYPE string VALUE '-'.
DATA lv_text TYPE string.
CONCATENATE lv_a lv_b INTO lv_text SEPARATED BY lv_sep.
WRITE lv_text.
lv_text = '  a   b  '.
CONDENSE lv_text.
WRITE lv_text.
TRANSLATE lv_text TO UPPER CASE.
WRITE lv_text.
CONDENSE lv_text NO-GAPS.
WRITE lv_text.
TRANSLATE lv_text TO LOWER CASE.
WRITE lv_text.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 5)
	if len(run.events) == 5 {
		testing.expect_value(t, run.events[0].text, "A-b")
		testing.expect_value(t, run.events[1].text, "a b")
		testing.expect_value(t, run.events[2].text, "A B")
		testing.expect_value(t, run.events[3].text, "AB")
		testing.expect_value(t, run.events[4].text, "ab")
	}
	testing.expect(t, runtime_test_named_value_is_string(run.final_values[:], "global", "lv_text", "ab"))
}

@(test)
runtime_executes_concatenate_respecting_blanks :: proc(t: ^testing.T) {
	source := `DATA lv_a TYPE c LENGTH 4 VALUE 'A  '.
DATA lv_b TYPE c LENGTH 3 VALUE 'B '.
DATA lv_sep TYPE c LENGTH 2 VALUE '- '.
DATA lv_s TYPE string VALUE 'S  '.
DATA lv_text TYPE string.
CONCATENATE lv_a lv_b INTO lv_text SEPARATED BY lv_sep.
WRITE lv_text.
CONCATENATE lv_a lv_b INTO lv_text SEPARATED BY lv_sep RESPECTING BLANKS.
WRITE lv_text.
CONCATENATE lv_s lv_b INTO lv_text SEPARATED BY lv_sep.
WRITE lv_text.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 3)
	if len(run.events) == 3 {
		testing.expect_value(t, run.events[0].text, "A- B")
		testing.expect_value(t, run.events[1].text, "A  - B ")
		testing.expect_value(t, run.events[2].text, "S  - B")
	}
	testing.expect(t, runtime_test_named_value_is_string(run.final_values[:], "global", "lv_text", "S  - B"))
}

@(test)
runtime_executes_remaining_scalar_string_operations :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string VALUE 'AA-BB-CC-DD'.
DATA lv_first TYPE string.
DATA lv_second TYPE string.
DATA lv_rest TYPE string.
SPLIT lv_text AT '-' INTO lv_first lv_second lv_rest.
WRITE lv_first.
WRITE lv_second.
WRITE lv_rest.
REPLACE ALL OCCURRENCES OF 'C' IN lv_rest WITH 'x'.
WRITE lv_rest.
SHIFT lv_rest.
WRITE lv_rest.
SHIFT lv_rest RIGHT BY 2 PLACES.
WRITE lv_rest.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 6)
	if len(run.events) == 6 {
		testing.expect_value(t, run.events[0].text, "AA")
		testing.expect_value(t, run.events[1].text, "BB")
		testing.expect_value(t, run.events[2].text, "CC-DD")
		testing.expect_value(t, run.events[3].text, "xx-DD")
		testing.expect_value(t, run.events[4].text, "x-DD")
		testing.expect_value(t, run.events[5].text, "  x-DD")
	}
	testing.expect(t, runtime_test_named_value_is_string(run.final_values[:], "global", "lv_rest", "  x-DD"))
}

@(test)
runtime_executes_scalar_search_operations :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string VALUE 'Alpha beta alpha'.
DATA lv_offset TYPE i.
DATA lv_length TYPE i.
DATA lv_count TYPE i.
FIND ALL OCCURRENCES OF 'alpha' IN lv_text IGNORING CASE MATCH OFFSET lv_offset MATCH LENGTH lv_length MATCH COUNT lv_count.
WRITE sy-subrc.
WRITE lv_offset.
WRITE lv_length.
WRITE lv_count.
SEARCH lv_text FOR 'BETA'.
WRITE sy-subrc.
WRITE sy-fdpos.
SEARCH lv_text FOR 'missing'.
WRITE sy-subrc.
WRITE sy-fdpos.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 8)
	if len(run.events) == 8 {
		testing.expect_value(t, run.events[0].text, "0")
		testing.expect_value(t, run.events[1].text, "11")
		testing.expect_value(t, run.events[2].text, "5")
		testing.expect_value(t, run.events[3].text, "2")
		testing.expect_value(t, run.events[4].text, "0")
		testing.expect_value(t, run.events[5].text, "6")
		testing.expect_value(t, run.events[6].text, "4")
		testing.expect_value(t, run.events[7].text, "16")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv_offset", 11))
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv_length", 5))
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv_count", 2))
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "subrc", 4))
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "fdpos", 16))
}

@(test)
runtime_executes_field_symbol_and_data_reference_lvalues :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_inner,
         amount TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_row,
         inner TYPE ty_inner,
       END OF ty_row.
DATA lv TYPE i VALUE 1.
DATA ls TYPE ty_row.
FIELD-SYMBOLS <fs> TYPE i.
ASSIGN lv TO <fs>.
<fs> = 7.
WRITE lv.
ls-inner-amount = lv.
ASSIGN ls-inner-amount TO <fs>.
<fs> = 9.
WRITE ls-inner-amount.
UNASSIGN <fs>.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 2)
	if len(run.events) == 2 {
		testing.expect_value(t, run.events[0].text, "7")
		testing.expect_value(t, run.events[1].text, "9")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv", 7))
}

@(test)
runtime_executes_data_reference_dereference_lvalues :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i VALUE 1.
DATA lr TYPE REF TO i.
DATA lr_new TYPE REF TO i.
lr = REF #( lv ).
lr->* = 11.
WRITE lv.
lr_new = NEW i( 13 ).
WRITE lr_new->*.
CREATE DATA lr_new.
lr_new->* = 15.
WRITE lr_new->*.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 3)
	if len(run.events) == 3 {
		testing.expect_value(t, run.events[0].text, "11")
		testing.expect_value(t, run.events[1].text, "13")
		testing.expect_value(t, run.events[2].text, "15")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv", 11))
}

@(test)
runtime_allocates_objects_and_runs_instance_constructors :: proc(t: ^testing.T) {
	source := `CLASS lcl_counter DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_start TYPE i.
    METHODS increment.
    METHODS total RETURNING VALUE(rv_total) TYPE i.
  PRIVATE SECTION.
    DATA mv_total TYPE i.
ENDCLASS.
CLASS lcl_counter IMPLEMENTATION.
  METHOD constructor.
    mv_total = iv_start.
  ENDMETHOD.
  METHOD increment.
    mv_total = mv_total + 1.
  ENDMETHOD.
  METHOD total.
    rv_total = mv_total.
  ENDMETHOD.
ENDCLASS.
DATA lo TYPE REF TO lcl_counter.
DATA lv TYPE i.
lo = NEW lcl_counter( iv_start = 4 ).
lo->increment( ).
lv = lo->total( ).
WRITE lv.
CLEAR lo.
CREATE OBJECT lo EXPORTING iv_start = 9.
lv = lo->total( ).
WRITE lv.
FREE lo.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 2)
	if len(run.events) == 2 {
		testing.expect_value(t, run.events[0].text, "5")
		testing.expect_value(t, run.events[1].text, "9")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv", 9))
	testing.expect(t, runtime_test_named_value_is_initial(run.final_values[:], "global", "lo"))
}

@(test)
runtime_allocates_non_prefix_class_references_by_descriptor :: proc(t: ^testing.T) {
	source := `CLASS zagent DEFINITION.
  PUBLIC SECTION.
    METHODS set IMPORTING iv_total TYPE i.
    METHODS total RETURNING VALUE(rv_total) TYPE i.
  PRIVATE SECTION.
    DATA mv_total TYPE i.
ENDCLASS.
CLASS zagent IMPLEMENTATION.
  METHOD set.
    mv_total = iv_total.
  ENDMETHOD.
  METHOD total.
    rv_total = mv_total.
  ENDMETHOD.
ENDCLASS.
DATA lo TYPE REF TO zagent.
DATA lv TYPE i.
lo = NEW zagent( ).
lo->set( iv_total = 7 ).
lv = lo->total( ).
WRITE lv.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) == 1 {
		testing.expect_value(t, run.events[0].text, "7")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv", 7))
}

@(test)
runtime_reads_and_keeps_system_values_in_memory :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	four := ir.builder_emit_const(&builder, "4", ir.BUILTIN_TYPE_INTEGER)
	ir.builder_emit_system_write(&builder, "subrc", four)
	subrc := ir.builder_emit_system_read(&builder, "subrc", ir.BUILTIN_TYPE_INTEGER)
	write_inputs := [?]ir.Value_Id{subrc}
	ir.builder_emit_write(&builder, write_inputs[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].text, "4")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "subrc", 4))
}

@(test)
runtime_captures_message_events :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	text := ir.builder_emit_const(&builder, "'hello'", ir.BUILTIN_TYPE_STRING)
	inputs := [?]ir.Value_Id{text}
	ir.builder_emit_message(
		&builder,
		inputs[:],
		payload = ir.Intrinsic_Message_Payload {
			form = .Compact,
			id = "zmsg",
			msg_type = "e",
			number = "001",
			arg_count = 1,
		},
	)
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].kind, runtime.IO_Event_Kind.Message)
		testing.expect_value(t, run.events[0].message_type, "e")
		testing.expect_value(t, run.events[0].text, "e 001 (zmsg) hello")
	}
}

@(test)
runtime_switch_selects_matching_case_and_default :: proc(t: ^testing.T) {
	selector_literals := [?]string{"2", "9"}
	expected_outputs := [?]string{"case-2", "default"}
	for selector_literal, i in selector_literals {
		expected := expected_outputs[i]
		module := ir.module_make(context.allocator)
		builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
		selector := ir.builder_emit_const(&builder, selector_literal, ir.BUILTIN_TYPE_INTEGER)
		one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
		two := ir.builder_emit_const(&builder, "2", ir.BUILTIN_TYPE_INTEGER)
		default_text := ir.builder_emit_const(&builder, "'default'", ir.BUILTIN_TYPE_STRING)
		one_text := ir.builder_emit_const(&builder, "'case-1'", ir.BUILTIN_TYPE_STRING)
		two_text := ir.builder_emit_const(&builder, "'case-2'", ir.BUILTIN_TYPE_STRING)
		default_block := ir.builder_add_world_block(&builder, "default")
		one_block := ir.builder_add_world_block(&builder, "one")
		two_block := ir.builder_add_world_block(&builder, "two")
		args := [?]ir.Value_Id{builder.current_world}
		cases := [?]ir.Switch_Case {
			{value = one, target = one_block, args = args[:]},
			{value = two, target = two_block, args = args[:]},
		}
		ir.builder_set_switch(&builder, selector, default_block, args[:], cases[:])

		ir.builder_position_at_end(&builder, default_block)
		write_args := [?]ir.Value_Id{default_text}
		ir.builder_emit_write(&builder, write_args[:])
		ir.builder_set_return_world(&builder)
		ir.builder_position_at_end(&builder, one_block)
		write_args[0] = one_text
		ir.builder_emit_write(&builder, write_args[:])
		ir.builder_set_return_world(&builder)
		ir.builder_position_at_end(&builder, two_block)
		write_args[0] = two_text
		ir.builder_emit_write(&builder, write_args[:])
		ir.builder_set_return_world(&builder)
		ir.module_add_entry(&module, builder.function_id)

		run := runtime_test_execute_ir(t, &module)
		testing.expect_value(t, run.status, Run_Status.Completed)
		testing.expect_value(t, len(run.events), 1)
		if len(run.events) == 1 {
			testing.expect_value(t, run.events[0].text, expected)
		}
		run_result_destroy(&run)
		ir.module_destroy(&module)
	}
}

@(test)
runtime_executes_source_message_literals_without_source_quotes :: proc(t: ^testing.T) {
	source := `MESSAGE 'hello' TYPE 'I'.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].kind, runtime.IO_Event_Kind.Message)
		testing.expect_value(t, run.events[0].message_type, "I")
		testing.expect_value(t, run.events[0].text, "I hello")
	}
}

@(test)
runtime_traps_divide_by_zero :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	zero := ir.builder_emit_const(&builder, "0", ir.BUILTIN_TYPE_INTEGER)
	inputs := [?]ir.Value_Id{one, zero}
	result_types := [?]ir.Type_Id{ir.BUILTIN_TYPE_INTEGER}
	ir.builder_emit_intrinsic(&builder, .ABAP_Divide, inputs[:], result_types[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Trapped)
	testing.expect_value(t, run.trap.kind, runtime.Trap_Kind.Divide_By_Zero)
}

@(test)
runtime_step_limit_traps_infinite_branch :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	ir.builder_set_branch_world(&builder, builder.block)
	ir.module_add_entry(&module, builder.function_id)

	prepared := prepare_module(&module, context.allocator)
	defer prepare_result_destroy(&prepared)
	testing.expect(t, prepared.ok)
	if !prepared.ok {
		return
	}
	run := execute_prepared_module(&prepared.module, Run_Options{step_limit = 8}, context.allocator)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Trapped)
	testing.expect_value(t, run.trap.kind, runtime.Trap_Kind.Step_Limit)
	testing.expect_value(t, run.instruction_count, u64(8))
}

@(test)
runtime_vm_step_limit_traps_while_stepping :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	ir.builder_set_branch_world(&builder, builder.block)
	ir.module_add_entry(&module, builder.function_id)

	prepared := prepare_module(&module, context.allocator)
	defer prepare_result_destroy(&prepared)
	testing.expect(t, prepared.ok)
	if !prepared.ok {
		return
	}

	vm := vm_make_prepared(&prepared.module, Run_Options{step_limit = 3}, context.allocator)
	defer vm_destroy(&vm)
	for _ in 0 ..< 5 {
		if vm_step(&vm) != .Running {
			break
		}
	}
	testing.expect_value(t, vm.state, VM_State.Trapped)
	testing.expect_value(t, vm.runtime_context.trap.kind, runtime.Trap_Kind.Step_Limit)
	testing.expect_value(t, vm.instruction_count, u64(3))
}

@(test)
runtime_table_intrinsics_append_and_read_in_memory :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	table_type := ir.module_add_type(
		&module,
		ir.Type{
			kind = .Table,
			name = "table",
			data = ir.Table_Type_Data{row_type = ir.BUILTIN_TYPE_INTEGER},
		},
	)
	table_slot := ir.function_add_slot(ir.builder_function(&builder), .Global, "gt_numbers", table_type)
	table := ir.builder_emit_load(&builder, table_slot)
	seven := ir.builder_emit_const(&builder, "7", ir.BUILTIN_TYPE_INTEGER)
	append_inputs := [?]ir.Value_Id{seven, table}
	ir.builder_emit_table_mutation(
		&builder,
		.Table_Append,
		append_inputs[:],
		ir.BUILTIN_TYPE_INTEGER,
		ir.Intrinsic_Table_Payload{access = .Full, source_kind = .Row},
	)
	loaded := ir.builder_emit_load(&builder, table_slot)
	index := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	read_inputs := [?]ir.Value_Id{loaded, index}
	row, subrc, tabix := ir.builder_emit_table_read(
		&builder,
		read_inputs[:],
		ir.BUILTIN_TYPE_INTEGER,
		ir.BUILTIN_TYPE_INTEGER,
		ir.Intrinsic_Table_Payload{access = .Index, result_kind = .Into},
	)
	ir.builder_emit_system_write(&builder, "subrc", subrc)
	ir.builder_emit_system_write(&builder, "tabix", tabix)
	write_inputs := [?]ir.Value_Id{row}
	ir.builder_emit_write(&builder, write_inputs[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].text, "7")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "subrc", 0))
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "tabix", 1))
}

@(test)
runtime_executes_internal_table_key_where_sort_and_bindings :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
         text TYPE string,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row.
DATA ls_row TYPE ty_row.
DATA lt_nums TYPE STANDARD TABLE OF i.
DATA lv_num TYPE i.
DATA lr_num TYPE REF TO i.
FIELD-SYMBOLS <num> TYPE i.

ls_row-id = 2.
ls_row-text = 'b'.
APPEND ls_row TO lt_rows.
ls_row-id = 1.
ls_row-text = 'a'.
APPEND ls_row TO lt_rows.
ls_row-id = 3.
ls_row-text = 'c'.
APPEND ls_row TO lt_rows.

READ TABLE lt_rows INTO ls_row WITH KEY id = 2.
WRITE ls_row-text.
WRITE sy-tabix.

LOOP AT lt_rows INTO ls_row WHERE id = 3.
  WRITE sy-tabix.
ENDLOOP.

ls_row-id = 2.
ls_row-text = 'bb'.
MODIFY lt_rows FROM ls_row WHERE id = 2.
READ TABLE lt_rows INTO ls_row WITH KEY id = 2.
WRITE ls_row-text.

DELETE lt_rows WHERE id = 1.
READ TABLE lt_rows INTO ls_row INDEX 2.
WRITE ls_row-id.

SORT lt_rows BY text DESCENDING.
READ TABLE lt_rows INTO ls_row INDEX 1.
WRITE ls_row-text.

APPEND 5 TO lt_nums ASSIGNING <num>.
<num> = 7.
WRITE <num>.
READ TABLE lt_nums INTO lv_num INDEX 1.
WRITE lv_num.
INSERT 9 INTO TABLE lt_nums INDEX 1 REFERENCE INTO lr_num.
WRITE lr_num->*.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 9)
	if len(run.events) == 9 {
		testing.expect_value(t, run.events[0].text, "b")
		testing.expect_value(t, run.events[1].text, "1")
		testing.expect_value(t, run.events[2].text, "3")
		testing.expect_value(t, run.events[3].text, "bb")
		testing.expect_value(t, run.events[4].text, "3")
		testing.expect_value(t, run.events[5].text, "c")
		testing.expect_value(t, run.events[6].text, "7")
		testing.expect_value(t, run.events[7].text, "5")
		testing.expect_value(t, run.events[8].text, "9")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "subrc", 0))
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "tabix", 1))
}

@(test)
runtime_initializes_named_table_types_by_descriptor :: proc(t: ^testing.T) {
	source := `TYPES ty_people TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA people TYPE ty_people.
DATA lv_name TYPE string.
APPEND 'Ada' TO people.
APPEND 'Bob' TO people.
READ TABLE people INTO lv_name INDEX 2.
WRITE lv_name.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) == 1 {
		testing.expect_value(t, run.events[0].text, "Bob")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "subrc", 0))
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "tabix", 2))
}

@(test)
runtime_interprets_mixed_example_file :: proc(t: ^testing.T) {
	data, err := os.read_entire_file("examples/ZPERF_PARSER_MIXED.abap", context.allocator)
	testing.expect(t, err == nil)
	if err != nil {
		return
	}
	defer delete(data, context.allocator)

	run := runtime_test_execute_source(t, string(data))
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, Run_Status.Completed)
	testing.expect_value(t, len(run.events), 5)
	if len(run.events) == 5 {
		testing.expect_value(t, run.events[0].text, "60 #1:MAT-001#2:MAT-002#3:MAT-003 first=MAT-001")
		testing.expect_value(t, run.events[1].text, "4")
		testing.expect_value(t, run.events[2].text, "3")
		testing.expect_value(t, run.events[3].kind, runtime.IO_Event_Kind.Message)
		testing.expect_value(t, run.events[3].message_type, "I")
		testing.expect_value(t, run.events[3].text, "I runtime coverage message")
		testing.expect_value(t, run.events[4].text, "|check|caught|3|AA|BB|XX|1|1|2|3|8|8|9|8|4")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "gv_total", 60))
	testing.expect(
		t,
		runtime_test_named_value_is_string(
			run.final_values[:],
			"global",
			"gv_message",
			"#1:MAT-001#2:MAT-002#3:MAT-003 first=MAT-001",
		),
	)
	testing.expect(
		t,
		runtime_test_named_value_is_string(
			run.final_values[:],
			"global",
			"gv_runtime_message",
			"|check|caught|3|AA|BB|XX|1|1|2|3|8|8|9|8|4",
		),
	)
}

runtime_test_execute_ir :: proc(t: ^testing.T, module: ^ir.Module) -> Run_Result {
	return execute_module(module, {}, context.allocator)
}

runtime_test_scalar_write_module :: proc() -> ir.Module {
	module := ir.module_make(context.allocator)
	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	function := ir.builder_function(&builder)
	slot := ir.function_add_slot(function, .Global, "gv_total", ir.BUILTIN_TYPE_INTEGER)
	one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	two := ir.builder_emit_const(&builder, "2", ir.BUILTIN_TYPE_INTEGER)
	inputs := [?]ir.Value_Id{one, two}
	result_types := [?]ir.Type_Id{ir.BUILTIN_TYPE_INTEGER}
	add := ir.builder_emit_intrinsic(&builder, .ABAP_Add, inputs[:], result_types[:])
	sum := ir.op_ptr(function, add).results[0]
	ir.builder_emit_store(&builder, slot, sum)
	loaded := ir.builder_emit_load(&builder, slot)
	write_inputs := [?]ir.Value_Id{loaded}
	ir.builder_emit_write(&builder, write_inputs[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)
	return module
}

runtime_test_execute_source :: proc(t: ^testing.T, source: string) -> Run_Result {
	parsed := parser.parse(source, "mem://runtime_test.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	if len(parsed.errors) != 0 {
		return Run_Result{status = Run_Status.Trapped}
	}

	project := semantic.project_make()
	defer semantic.project_destroy(&project)
	checker := semantic.checker_make(&project)
	file := semantic.checker_add_file(&checker, parsed.path, parsed.root)
	semantic.checker_check_file(&checker, file)

	lowered := ir.lower_project(&project, &checker, context.allocator)
	defer ir.lower_result_destroy(&lowered)
	return runtime_test_execute_ir(t, &lowered.module)
}

runtime_test_named_value_is_int :: proc(values: []runtime.Named_Value, scope, name: string, expected: i64) -> bool {
	for value in values {
		if value.scope == scope &&
		   value.name == name &&
		   runtime.value_kind(value.value) == runtime.Value_Kind.Integer &&
		   runtime.value_int(value.value) == expected {
			return true
		}
	}
	return false
}

runtime_test_named_value_is_string :: proc(values: []runtime.Named_Value, scope, name: string, expected: string) -> bool {
	for value in values {
		if value.scope == scope &&
		   value.name == name &&
		   runtime.value_kind(value.value) == runtime.Value_Kind.String &&
		   runtime.value_text(value.value) == expected {
			return true
		}
	}
	return false
}

runtime_test_named_value_is_initial :: proc(values: []runtime.Named_Value, scope, name: string) -> bool {
	for value in values {
		if value.scope == scope &&
		   value.name == name &&
		   runtime.value_kind(value.value) == runtime.Value_Kind.Initial {
			return true
		}
	}
	return false
}

runtime_test_frame_has_int :: proc(frame: ^Frame, expected: i64) -> bool {
	for value in frame.registers {
		if runtime.value_kind(value) == runtime.Value_Kind.Integer && runtime.value_int(value) == expected {
			return true
		}
	}
	return false
}

runtime_test_dispatch_add :: proc(ctx: ^Intrinsic_Dispatch_Context) -> Intrinsic_Dispatch_Result {
	if ctx.intrinsic != .ABAP_Add {
		return .Unsupported
	}
	if len(ctx.operands) != 2 || len(ctx.result_types) != 1 {
		intrinsic_dispatch_trap(ctx, .Invalid_Instruction, "unexpected test dispatcher signature")
		return .Trap
	}
	return .Ok if intrinsic_dispatch_set_result(ctx, 0, runtime.value_integer_make(42)) else .Trap
}
