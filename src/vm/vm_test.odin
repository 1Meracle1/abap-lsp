package abap_frontend_vm

import bytecode "src:ir/bytecode"
import ir "src:ir"
import "src:parser"
import runtime "src:runtime"
import semantic "src:semantic"

import "core:os"
import "core:testing"

@(test)
runtime_executes_scalar_callbacks_and_captured_write :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	function := ir.builder_function(&builder)
	slot := ir.function_add_slot(function, .Global, "gv_total", ir.BUILTIN_TYPE_INTEGER)
	one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	two := ir.builder_emit_const(&builder, "2", ir.BUILTIN_TYPE_INTEGER)
	inputs := [?]ir.Value_Id{one, two}
	result_types := [?]ir.Type_Id{ir.BUILTIN_TYPE_INTEGER}
	add := ir.builder_emit_op(&builder, .Abap_Add, inputs[:], result_types[:])
	sum := ir.op_ptr(function, add).results[0]
	ir.builder_emit_store(&builder, slot, sum)
	loaded := ir.builder_emit_load(&builder, slot)
	write_inputs := [?]ir.Value_Id{loaded}
	ir.builder_emit_write(&builder, write_inputs[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, runtime.Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].kind, runtime.IO_Event_Kind.Write)
		testing.expect_value(t, run.events[0].text, "3")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "gv_total", 3))
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
	add := ir.builder_emit_op(&builder, .Abap_Add, inputs[:], result_types[:])
	sum := ir.op_ptr(function, add).results[0]
	ir.builder_emit_store(&builder, slot, sum)
	loaded := ir.builder_emit_load(&builder, slot)
	write_inputs := [?]ir.Value_Id{loaded}
	ir.builder_emit_write(&builder, write_inputs[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	lowered := runtime_test_lower_ir(t, &module)
	defer bytecode.module_destroy(&lowered.module)
	if !lowered.ok {
		return
	}

	vm := vm_make(&lowered.module, Run_Options{step_limit = 32, io_policy = runtime.io_policy_captured()}, context.allocator)
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
	testing.expect_value(t, run.status, runtime.Run_Status.Completed)
	testing.expect_value(t, run.instruction_count, vm.instruction_count)
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "gv_total", 3))
}

@(test)
runtime_executes_source_report_through_bytecode_boundary :: proc(t: ^testing.T) {
	source := `DATA LV TYPE I VALUE 1.
LV = LV + 2.
IF LV = 3.
  WRITE LV.
ENDIF.`
	run := runtime_test_execute_source(t, source)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, runtime.Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].text, "3")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv", 3))
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
	testing.expect_value(t, run.status, runtime.Run_Status.Completed)
	testing.expect_value(t, len(run.events), 2)
	if len(run.events) == 2 {
		testing.expect_value(t, run.events[0].text, "5")
		testing.expect_value(t, run.events[1].text, "9")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "global", "lv", 9))
	testing.expect(t, runtime_test_named_value_is_initial(run.final_values[:], "global", "lo"))
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
	testing.expect_value(t, run.status, runtime.Run_Status.Completed)
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
		payload = ir.Op_Payload {
			message_form = .Compact,
			message_id = "zmsg",
			message_type = "e",
			message_number = "001",
			message_arg_count = 1,
		},
	)
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, runtime.Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].kind, runtime.IO_Event_Kind.Message)
		testing.expect_value(t, run.events[0].message_type, "e")
		testing.expect_value(t, run.events[0].text, "e 001 (zmsg) hello")
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
	ir.builder_emit_op(&builder, .Abap_Divide, inputs[:], result_types[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, runtime.Run_Status.Trapped)
	testing.expect_value(t, run.trap.kind, runtime.Trap_Kind.Divide_By_Zero)
}

@(test)
runtime_step_limit_traps_infinite_branch :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	ir.builder_set_branch_world(&builder, builder.block)
	ir.module_add_entry(&module, builder.function_id)

	bytecode_result := bytecode.lower_module(&module, context.allocator)
	defer bytecode.module_destroy(&bytecode_result.module)
	testing.expect(t, bytecode_result.ok)
	if !bytecode_result.ok {
		return
	}
	run := execute_module(&bytecode_result.module, Run_Options{step_limit = 8}, context.allocator)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, runtime.Run_Status.Trapped)
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

	bytecode_result := bytecode.lower_module(&module, context.allocator)
	defer bytecode.module_destroy(&bytecode_result.module)
	testing.expect(t, bytecode_result.ok)
	if !bytecode_result.ok {
		return
	}

	vm := vm_make(&bytecode_result.module, Run_Options{step_limit = 3}, context.allocator)
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
runtime_table_callbacks_append_and_read_in_memory :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main", role = .Report_Entry)
	table_type := ir.module_add_type(&module, ir.Type{kind = .Table, name = "table"})
	table_slot := ir.function_add_slot(ir.builder_function(&builder), .Global, "gt_numbers", table_type)
	table := ir.builder_emit_load(&builder, table_slot)
	seven := ir.builder_emit_const(&builder, "7", ir.BUILTIN_TYPE_INTEGER)
	append_inputs := [?]ir.Value_Id{seven, table}
	ir.builder_emit_table_mutation(
		&builder,
		.Table_Append,
		append_inputs[:],
		ir.BUILTIN_TYPE_INTEGER,
		ir.Op_Payload {
			table_access = .Full,
			table_source_kind = .Row,
		},
	)
	loaded := ir.builder_emit_load(&builder, table_slot)
	index := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	read_inputs := [?]ir.Value_Id{loaded, index}
	row, subrc := ir.builder_emit_table_read(
		&builder,
		read_inputs[:],
		ir.BUILTIN_TYPE_INTEGER,
		ir.BUILTIN_TYPE_INTEGER,
		ir.Op_Payload {
			table_access = .Index,
			table_result_kind = .Into,
		},
	)
	ir.builder_emit_system_write(&builder, "subrc", subrc)
	write_inputs := [?]ir.Value_Id{row}
	ir.builder_emit_write(&builder, write_inputs[:])
	ir.builder_set_return_world(&builder)
	ir.module_add_entry(&module, builder.function_id)

	run := runtime_test_execute_ir(t, &module)
	defer run_result_destroy(&run)
	testing.expect_value(t, run.status, runtime.Run_Status.Completed)
	testing.expect_value(t, len(run.events), 1)
	if len(run.events) > 0 {
		testing.expect_value(t, run.events[0].text, "7")
	}
	testing.expect(t, runtime_test_named_value_is_int(run.final_values[:], "system", "subrc", 0))
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
	testing.expect_value(t, run.status, runtime.Run_Status.Completed)
	testing.expect_value(t, len(run.events), 3)
	if len(run.events) == 3 {
		testing.expect_value(t, run.events[0].text, "60 #1:MAT-001#2:MAT-002#3:MAT-003 first=MAT-001")
		testing.expect_value(t, run.events[1].text, "4")
		testing.expect_value(t, run.events[2].text, "3")
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
}

runtime_test_execute_ir :: proc(t: ^testing.T, module: ^ir.Module) -> Run_Result {
	lowered := runtime_test_lower_ir(t, module)
	defer bytecode.module_destroy(&lowered.module)
	if !lowered.ok {
		return Run_Result{status = runtime.Run_Status.Trapped}
	}
	return execute_module(&lowered.module, {}, context.allocator)
}

runtime_test_lower_ir :: proc(t: ^testing.T, module: ^ir.Module) -> bytecode.Lower_Result {
	verify := ir.verify_module(module, context.allocator)
	defer ir.verify_result_destroy(&verify)
	testing.expect(t, verify.ok)
	if !verify.ok {
		return bytecode.Lower_Result{ok = false}
	}
	lowered := bytecode.lower_module(module, context.allocator)
	testing.expect(t, lowered.ok)
	return lowered
}

runtime_test_execute_source :: proc(t: ^testing.T, source: string) -> Run_Result {
	parsed := parser.parse(source, "mem://runtime_test.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	if len(parsed.errors) != 0 {
		return Run_Result{status = runtime.Run_Status.Trapped}
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
