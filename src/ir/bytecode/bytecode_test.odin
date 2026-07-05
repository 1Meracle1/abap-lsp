package abap_frontend_ir_bytecode

import "src:ast"
import ir "src:ir"
import "src:parser"
import semantic "src:semantic"
import "src:tokenizer"

import "core:testing"

@(test)
printer_snapshot_hand_built_store_and_return_is_stable :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "main")
	function := ir.builder_function(&builder)
	slot := ir.function_add_slot(function, .Local, "lv_value", ir.BUILTIN_TYPE_INTEGER)
	one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	ir.builder_emit_store(&builder, slot, one)
	ir.builder_emit_load(&builder, slot)
	ir.builder_set_return_world(&builder)

	expect_print_snapshot(
		t,
		&module,
		`bytecode @main registers=4 -> (!world) {
  slots:
    %s0 local lv_value : !i
  blocks:
    ^b0.entry = ip0(%r0 world : !world)
  constants:
    #0 raw="1"
  code:
  ^b0.entry @ip0(%r0 world : !world):
    0: %r1 : !i = core.const literal=1
    1: %r2 : !world = core.store %s0 lv_value(%r0, %r1)
    2: %r3 : !i = core.load %s0 lv_value(%r2)
    3: return(%r2)
}
`,
	)
}

@(test)
printer_snapshot_branch_edges_are_stable :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	builder := ir.builder_begin_function(&module, "branch")
	target := ir.builder_add_world_block(&builder, "target")
	ir.builder_set_branch_world(&builder, target)
	ir.builder_position_at_end(&builder, target)
	ir.builder_set_return_world(&builder)

	expect_print_snapshot(
		t,
		&module,
		`bytecode @branch registers=2 -> (!world) {
  blocks:
    ^b0.entry = ip0(%r0 world : !world)
    ^b1.target = ip1(%r1 world : !world)
  code:
  ^b0.entry @ip0(%r0 world : !world):
    0: br ^b1.target(%r0)
  ^b1.target @ip1(%r1 world : !world):
    1: return(%r1)
}
`,
	)
}

@(test)
unsupported_statement_keeps_source_and_blocks_bytecode_lowering :: proc(t: ^testing.T) {
	source := `ASSERT 1 = 1.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)

	bytecode := lower_module(&fixture.lowered.module, context.allocator)
	defer module_destroy(&bytecode.module)
	testing.expect(t, !bytecode.ok)
	testing.expect_value(t, bytecode.message, "unsupported statement")
	testing.expect_value(t, bytecode.source.range.start, 0)
	testing.expect(t, bytecode.source.range.end > bytecode.source.range.start)
}

@(test)
unsupported_binary_lowers_to_error :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
IF lv CO '1'.
ENDIF.`
	fixture := lower_test_verified_source(t, source)
	defer lower_test_result_destroy(&fixture)

	bytecode := lower_module(&fixture.lowered.module, context.allocator)
	defer module_destroy(&bytecode.module)
	testing.expect(t, !bytecode.ok)
	testing.expect_value(t, bytecode.message, "unsupported binary expression")
}

@(test)
boundary_lowers_core_store_load_and_return_slice :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	const_source := ir.Source_Loc{range = tokenizer.text_range(1, 2)}
	store_source := ir.Source_Loc{range = tokenizer.text_range(3, 8)}
	load_source := ir.Source_Loc{range = tokenizer.text_range(9, 14)}
	return_source := ir.Source_Loc{range = tokenizer.text_range(15, 21)}

	builder := ir.builder_begin_function(&module, "main")
	function := ir.builder_function(&builder)
	slot := ir.function_add_slot(function, .Local, "lv_value", ir.BUILTIN_TYPE_INTEGER)
	one := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER, const_source)
	ir.builder_emit_store(&builder, slot, one, store_source)
	ir.builder_emit_load(&builder, slot, load_source)
	ir.builder_set_return_world(&builder, return_source)

	bytecode := lower_module(&module, context.allocator)
	defer module_destroy(&bytecode.module)
	testing.expect(t, bytecode.ok)
	testing.expect_value(t, len(bytecode.module.functions), 1)
	if !bytecode.ok || len(bytecode.module.functions) != 1 {
		return
	}

	lowered := &bytecode.module.functions[0]
	testing.expect_value(t, lowered.name, "main")
	testing.expect_value(t, lowered.register_count, u32(len(function.values)))
	testing.expect_value(t, len(lowered.block_offsets), 1)
	testing.expect_value(t, lowered.block_offsets[0], Block_Offset(0))
	testing.expect_value(t, len(lowered.constants), 1)
	testing.expect_value(t, lowered.constants[0], "1")
	testing.expect_value(t, len(lowered.instructions), 4)
	if len(lowered.instructions) != 4 {
		return
	}

	testing.expect_value(t, lowered.instructions[0].op, Op.Const)
	testing.expect_value(t, lowered.instructions[0].dst, Register(one))
	testing.expect_value(t, lowered.instructions[0].payload, u32(0))
	testing.expect_value(t, lowered.instructions[0].source.range.start, const_source.range.start)

	testing.expect_value(t, lowered.instructions[1].op, Op.Store)
	testing.expect_value(t, lowered.instructions[1].src0, Register(function.world_param))
	testing.expect_value(t, lowered.instructions[1].src1, Register(one))
	testing.expect_value(t, lowered.instructions[1].payload, u32(slot))
	testing.expect_value(t, lowered.instructions[1].source.range.start, store_source.range.start)

	testing.expect_value(t, lowered.instructions[2].op, Op.Load)
	testing.expect_value(t, lowered.instructions[2].payload, u32(slot))
	testing.expect_value(t, lowered.instructions[2].source.range.start, load_source.range.start)

	testing.expect_value(t, lowered.instructions[3].op, Op.Return)
	testing.expect_value(t, lowered.instructions[3].src0, lowered.instructions[1].dst)
	testing.expect_value(t, lowered.instructions[3].payload, u32(1))
	testing.expect_value(t, lowered.instructions[3].operand_count, u32(1))
	testing.expect_value(t, lowered.instructions[3].source.range.start, return_source.range.start)
}

@(test)
boundary_rejects_unverified_ir_before_emitting :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	source := ir.Source_Loc{range = tokenizer.text_range(4, 9)}
	builder := ir.builder_begin_function(&module, "missing_terminator", source = source)
	ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)

	bytecode := lower_module(&module, context.allocator)
	defer module_destroy(&bytecode.module)
	testing.expect(t, !bytecode.ok)
	testing.expect_value(t, bytecode.message, "block is missing terminator")
	testing.expect_value(t, bytecode.source.range.start, source.range.start)
	testing.expect_value(t, len(bytecode.module.functions), 0)
}

@(test)
boundary_lowers_domain_ops_to_runtime_callbacks :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	abap_source := ir.Source_Loc{range = tokenizer.text_range(12, 20)}
	table_source := ir.Source_Loc{range = tokenizer.text_range(21, 30)}
	sql_source := ir.Source_Loc{range = tokenizer.text_range(31, 40)}
	system_source := ir.Source_Loc{range = tokenizer.text_range(41, 50)}

	builder := ir.builder_begin_function(&module, "domain_ops")
	left := ir.builder_emit_const(&builder, "1", ir.BUILTIN_TYPE_INTEGER)
	right := ir.builder_emit_const(&builder, "2", ir.BUILTIN_TYPE_INTEGER)
	add_operands := [?]ir.Value_Id{left, right}
	add_result_types := [?]ir.Type_Id{ir.BUILTIN_TYPE_INTEGER}
	ir.builder_emit_op(&builder, .Abap_Add, add_operands[:], add_result_types[:], source = abap_source)
	ir.builder_emit_system_read(&builder, "subrc", ir.BUILTIN_TYPE_INTEGER, system_source)
	table_type := ir.module_add_type(&module, ir.Type{kind = .Table, name = "table"})
	table := ir.builder_emit_const(&builder, "itab", table_type)
	table_inputs := [?]ir.Value_Id{table}
	ir.builder_emit_table_read(
		&builder,
		table_inputs[:],
		ir.BUILTIN_TYPE_INTEGER,
		ir.BUILTIN_TYPE_INTEGER,
		ir.Op_Payload {
			table_access = .Full,
			table_key_kind = .None,
			table_result_kind = .Value,
		},
		table_source,
	)
	query := ast.Select_Query_Clause{}
	ir.builder_emit_sql_select(
		&builder,
		ir.BUILTIN_TYPE_INTEGER,
		ir.Op_Payload {
			sql_query = &query,
			sql_source_kind = .Internal,
			sql_result_kind = .Into,
			sql_row_type = ir.BUILTIN_TYPE_INTEGER,
			sql_scalar_type = ir.BUILTIN_TYPE_INTEGER,
			sql_source_count = 1,
			sql_projection_count = 1,
		},
		sql_source,
	)
	ir.builder_set_return_world(&builder)

	bytecode := lower_module(&module, context.allocator)
	defer module_destroy(&bytecode.module)
	testing.expect(t, bytecode.ok)
	testing.expect_value(t, len(bytecode.module.functions), 1)
	if !bytecode.ok || len(bytecode.module.functions) != 1 {
		return
	}

	lowered := &bytecode.module.functions[0]
	testing.expect_value(t, len(lowered.runtime_callbacks), 4)
	if len(lowered.runtime_callbacks) != 4 {
		return
	}

	testing.expect_value(t, lowered.runtime_callbacks[0].kind, Runtime_Callback_Kind.Abap)
	testing.expect_value(t, lowered.runtime_callbacks[0].op_kind, ir.Op_Kind.Abap_Add)
	testing.expect_value(t, lowered.runtime_callbacks[0].name, "abap.add")
	testing.expect_value(t, lowered.runtime_callbacks[1].kind, Runtime_Callback_Kind.System_Field)
	testing.expect_value(t, lowered.runtime_callbacks[1].op_kind, ir.Op_Kind.System_Read)
	testing.expect_value(t, lowered.runtime_callbacks[1].payload.system_field, "subrc")
	testing.expect_value(t, lowered.runtime_callbacks[2].kind, Runtime_Callback_Kind.Table)
	testing.expect_value(t, lowered.runtime_callbacks[2].op_kind, ir.Op_Kind.Table_Read)
	testing.expect_value(t, lowered.runtime_callbacks[2].payload.table_result_kind, ir.Table_Result_Kind.Value)
	testing.expect_value(t, lowered.runtime_callbacks[3].kind, Runtime_Callback_Kind.Sql)
	testing.expect_value(t, lowered.runtime_callbacks[3].op_kind, ir.Op_Kind.Sql_Select)
	testing.expect_value(t, lowered.runtime_callbacks[3].payload.sql_result_kind, ir.Sql_Result_Kind.Into)

	callback_count := 0
	for instruction in lowered.instructions {
		if instruction.op == .Call_Runtime {
			testing.expect_value(t, instruction.payload, u32(callback_count))
			callback_count += 1
		}
	}
	testing.expect_value(t, callback_count, 4)
}

@(test)
boundary_lowers_conditional_block_arguments_as_edges :: proc(t: ^testing.T) {
	module := ir.module_make(context.allocator)
	defer ir.module_destroy(&module)

	source := ir.Source_Loc{range = tokenizer.text_range(21, 27)}
	builder := ir.builder_begin_function(&module, "block_args")
	function := ir.builder_function(&builder)
	condition := ir.builder_emit_const(&builder, "true", ir.BUILTIN_TYPE_PREDICATE)
	true_block := ir.builder_add_world_block(&builder, "true")
	false_block := ir.builder_add_world_block(&builder, "false")
	true_args := [?]ir.Value_Id{builder.current_world}
	false_args := [?]ir.Value_Id{builder.current_world}
	ir.builder_set_cond_branch(&builder, condition, true_block, true_args[:], false_block, false_args[:], source)
	ir.builder_position_at_end(&builder, true_block)
	ir.builder_set_return_world(&builder)
	ir.builder_position_at_end(&builder, false_block)
	ir.builder_set_return_world(&builder)

	bytecode := lower_module(&module, context.allocator)
	defer module_destroy(&bytecode.module)
	testing.expect(t, bytecode.ok)
	testing.expect_value(t, len(bytecode.module.functions), 1)
	if !bytecode.ok || len(bytecode.module.functions) != 1 {
		return
	}

	lowered := &bytecode.module.functions[0]
	testing.expect_value(t, len(lowered.instructions), 4)
	if len(lowered.instructions) != 4 {
		return
	}
	testing.expect_value(t, lowered.instructions[1].op, Op.Cond_Branch)
	testing.expect_value(t, lowered.instructions[1].src0, Register(condition))
	testing.expect_value(t, lowered.instructions[1].edge_count, u32(2))
	testing.expect_value(t, len(lowered.edges), 2)
	if len(lowered.edges) == 2 {
		true_edge := lowered.edges[0]
		false_edge := lowered.edges[1]
		testing.expect_value(t, true_edge.target, lowered.block_offsets[int(true_block)])
		testing.expect_value(t, false_edge.target, lowered.block_offsets[int(false_block)])
		testing.expect_value(t, true_edge.arg_count, u32(1))
		testing.expect_value(t, false_edge.arg_count, u32(1))
		testing.expect_value(t, lowered.edge_registers[int(true_edge.arg_start)], Register(function.world_param))
		testing.expect_value(t, lowered.edge_registers[int(false_edge.arg_start)], Register(function.world_param))
	}
}

expect_print_snapshot :: proc(t: ^testing.T, module: ^ir.Module, expected: string) {
	bytecode := lower_module(module, context.allocator)
	defer module_destroy(&bytecode.module)
	testing.expect(t, bytecode.ok)
	if !bytecode.ok {
		return
	}

	text := print_module(&bytecode.module, context.allocator)
	defer delete(text, context.allocator)
	testing.expect_value(t, text, expected)
}

Lower_Test_Result :: struct {
	lowered: ir.Lower_Result,
	project: semantic.Project,
}

lower_test_source :: proc(t: ^testing.T, source: string) -> Lower_Test_Result {
	parsed := parser.parse(source, "mem://ir-bytecode-test.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := semantic.project_make()
	checker := semantic.checker_make(&project)
	file := semantic.checker_add_file(&checker, parsed.path, parsed.root)
	semantic.checker_check_file(&checker, file)

	result := ir.lower_project(&project, &checker, context.allocator)
	return Lower_Test_Result{lowered = result, project = project}
}

lower_test_verified_source :: proc(t: ^testing.T, source: string) -> Lower_Test_Result {
	fixture := lower_test_source(t, source)
	verify := ir.verify_module(&fixture.lowered.module, context.allocator)
	defer ir.verify_result_destroy(&verify)
	testing.expect(t, verify.ok)
	return fixture
}

lower_test_result_destroy :: proc(result: ^Lower_Test_Result) {
	assert(result != nil)
	ir.lower_result_destroy(&result.lowered)
	semantic.project_destroy(&result.project)
	result^ = {}
}
