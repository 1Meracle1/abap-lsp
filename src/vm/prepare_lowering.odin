package abap_frontend_vm

import ir "src:ir"
import runtime "src:vm/runtime"

import "core:mem"
import "core:strings"

prepare_function :: proc(
	module: ^Prepared_Module,
	ir_module: ^ir.Module,
	function: ^ir.Function,
) -> Prepare_Result {
	out_function := Prepared_Function {
		name = strings.clone(function.name, module.allocator),
		role = function.role,
		frame_slot_count = u32(len(function.values)),
		block_offsets = make([dynamic]u32, 0, len(function.blocks), module.allocator),
		block_debug = make([dynamic]Block_Debug, 0, len(function.blocks), module.allocator),
		block_params = make([dynamic]Register, 0, len(function.values), module.allocator),
		constants = make([dynamic]string, 0, 8, module.allocator),
		fields = make([dynamic]Field_Ref, 0, 8, module.allocator),
		slots = make([dynamic]Slot_Debug, 0, len(function.slots), module.allocator),
		values = make([dynamic]Value_Debug, 0, len(function.values), module.allocator),
		return_type_names = make([dynamic]string, 0, len(function.return_types), module.allocator),
		operand_registers = make([dynamic]Register, 0, 32, module.allocator),
		result_registers = make([dynamic]Register, 0, 32, module.allocator),
		edge_registers = make([dynamic]Register, 0, 8, module.allocator),
		edges = make([dynamic]Prepared_Edge, 0, 8, module.allocator),
		instructions = make([dynamic]Prepared_Instruction, 0, 32, module.allocator),
	}
	prepared_function_copy_debug_metadata(&out_function, module, function)

	ip: u32
	for block in function.blocks {
		append(&out_function.block_offsets, ip)
		for _ in block.instructions {
			ip += 1
		}
		ip += 1
	}

	ctx := Prepare_Context {
		module = module,
		ir_module = ir_module,
		out_function = &out_function,
		ir_function = function,
	}
	for &block in function.blocks {
		for instruction in block.instructions {
			op := function.instructions[int(instruction)]
			result := prepare_op(&ctx, op)
			if !result.ok {
				prepared_function_destroy(&out_function, module.allocator)
				return result
			}
		}
		term := function.instructions[int(block.terminator)]
		result := prepare_terminator(&ctx, term)
		if !result.ok {
			prepared_function_destroy(&out_function, module.allocator)
			return result
		}
	}

	append(&module.functions, out_function)
	return Prepare_Result{ok = true}
}

prepared_function_copy_debug_metadata :: proc(
	out_function: ^Prepared_Function,
	module: ^Prepared_Module,
	function: ^ir.Function,
) {
	for typ in function.return_types {
		append(&out_function.return_type_names, clone_type_name_from_cache(module, typ))
	}
	for slot in function.slots {
		append(
			&out_function.slots,
			Slot_Debug {
				kind = slot.kind,
				name = strings.clone(slot.name, module.allocator),
				type = prepared_type_descriptor(module, slot.type),
				type_name = clone_type_name_from_cache(module, slot.type),
				is_field_symbol = ir.slot_is_field_symbol(slot),
			},
		)
	}
	for value in function.values {
		append(
			&out_function.values,
			Value_Debug {
				name = strings.clone(value.name, module.allocator),
				type = prepared_type_descriptor(module, value.type),
				type_name = clone_type_name_from_cache(module, value.type),
			},
		)
	}
	for block in function.blocks {
		debug := Block_Debug {
			name = strings.clone(block.name, module.allocator),
			param_start = u32(len(out_function.block_params)),
			param_count = u32(len(block.args)),
		}
		for arg in block.args {
			append(&out_function.block_params, register(arg))
		}
		append(&out_function.block_debug, debug)
	}
}

clone_type_name_from_cache :: proc(module: ^Prepared_Module, typ: ir.Type_Id) -> string {
	index := int(typ)
	if index >= 0 && index < len(module.type_names) {
		return strings.clone(module.type_names[index], module.allocator)
	}
	return strings.clone("invalid", module.allocator)
}

prepared_type_descriptor :: proc "contextless" (module: ^Prepared_Module, typ: ir.Type_Id) -> runtime.Type_Descriptor {
	if module == nil || typ == ir.INVALID_TYPE_ID || int(typ) < 0 || int(typ) >= len(module.type_descriptors) {
		return nil
	}
	return &module.type_descriptors[int(typ)]
}

clone_type_name :: proc(
	module: ^ir.Module,
	typ: ir.Type_Id,
	allocator: mem.Allocator,
) -> string {
	out: strings.Builder
	strings.builder_init(&out, 0, 32, allocator)
	ir.print_type(&out, module, typ)
	return strings.to_string(out)
}

op_supported :: proc "contextless" (op: ir.Op) -> (bool, string) {
	#partial switch op.opcode {
	case .Const,
	     .Initial,
	     .Null_Ref,
	     .Add,
	     .Sub,
	     .Mul,
	     .Div,
	     .Mod,
	     .Neg,
	     .And,
	     .Or,
	     .Xor,
	     .Not,
	     .Cmp,
	     .Select,
	     .Cast,
	     .Int_Extend,
	     .Int_Truncate,
	     .Ref_Cast,
	     .Addr_Cast,
	     .Alloca,
	     .Addr_Of,
	     .Deref,
	     .Field_Addr,
	     .Load,
	     .Store,
	     .Struct_Init,
	     .Extract_Value,
	     .Call,
	     .Invoke,
	     .Debug_Value,
	     .Trap:
		return true, ""
	case .Intrinsic:
		if op.intrinsic != ir.INVALID_INTRINSIC_ID {
			return true, ""
		}
		return false, "VM executable IR intrinsic operation is missing a canonical intrinsic declaration"
	case .Global_Addr:
		return false, "VM executable IR does not support global_addr opcode"
	case .Function_Addr:
		return false, "VM executable IR does not support function_addr opcode"
	case .Index_Addr:
		return false, "VM executable IR does not support index_addr opcode"
	case .Table_Row_Addr:
		return false, "VM executable IR does not support table_row_addr opcode"
	case .Insert_Value:
		return false, "VM executable IR does not support insert_value opcode"
	case .Br, .Cond_Br, .Switch, .Return, .Unreachable:
		return false, "VM executable IR control-flow opcode must be a block terminator"
	case .Unsupported:
		return false, "VM executable IR does not support unsupported opcode"
	}
	return false, "VM executable IR does not support opcode"
}

terminator_supported :: proc "contextless" (term: ir.Op) -> (bool, string) {
	#partial switch term.opcode {
	case .Br, .Cond_Br, .Return, .Unreachable:
		return true, ""
	case .Switch:
		return true, ""
	case .Const,
	     .Initial,
	     .Null_Ref,
	     .Global_Addr,
	     .Function_Addr,
	     .Add,
	     .Sub,
	     .Mul,
	     .Div,
	     .Mod,
	     .Neg,
	     .And,
	     .Or,
	     .Xor,
	     .Not,
	     .Cmp,
	     .Select,
	     .Cast,
	     .Int_Extend,
	     .Int_Truncate,
	     .Ref_Cast,
	     .Addr_Cast,
	     .Alloca,
	     .Addr_Of,
	     .Deref,
	     .Field_Addr,
	     .Index_Addr,
	     .Table_Row_Addr,
	     .Load,
	     .Store,
	     .Struct_Init,
	     .Extract_Value,
	     .Insert_Value,
	     .Call,
	     .Invoke,
	     .Intrinsic,
	     .Trap,
	     .Debug_Value,
	     .Unsupported:
		return false, "VM executable IR terminator must use a control-flow opcode"
	}
	return false, "VM executable IR does not support terminator opcode"
}

prepare_op :: proc(ctx: ^Prepare_Context, op: ir.Op) -> Prepare_Result {
	#partial switch op.opcode {
	case .Const:
		constant_id, ok := op.attrs.(ir.Constant_Id)
		if !ok || constant_id == ir.INVALID_CONSTANT_ID || int(constant_id) >= len(ctx.ir_module.constants) {
			return prepare_error("constant operation is missing canonical constant attribute", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Const, op.source, ctx.module.allocator)
		instruction.payload = prepared_function_add_constant(
			ctx.out_function,
			ctx.ir_module.constants[int(constant_id)].literal,
			ctx.module.allocator,
		)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Initial, .Null_Ref:
		instruction := prepared_instruction_make(op.opcode, op.source, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Alloca:
		instruction := prepared_instruction_make(.Alloca, op.source, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Add,
	     .Sub,
	     .Mul,
	     .Div,
	     .Mod,
	     .Neg,
	     .And,
	     .Or,
	     .Xor,
	     .Not,
	     .Select,
	     .Int_Extend,
	     .Int_Truncate,
	     .Ref_Cast,
	     .Addr_Cast,
	     .Debug_Value:
		instruction := prepared_instruction_make(op.opcode, op.source, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Cmp:
		attrs, ok := op.attrs.(ir.Compare_Attrs)
		if !ok {
			return prepare_error("cmp operation is missing canonical compare attrs", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Cmp, op.source, ctx.module.allocator)
		instruction.compare_predicate = attrs.predicate
		instruction.compare_mode = attrs.mode
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Addr_Of:
		attrs, ok := op.attrs.(ir.Slot_Address_Attrs)
		if !ok || attrs.slot == ir.INVALID_SLOT_ID || int(attrs.slot) >= len(ctx.ir_function.slots) {
			return prepare_error("addr_of operation is missing canonical slot address attribute", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Addr_Of, op.source, ctx.module.allocator)
		instruction.address_kind = .Slot
		instruction.payload = u32(attrs.slot)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Field_Addr:
		projection, ok := op.attrs.(ir.Projection_Id)
		if !ok || projection == ir.INVALID_PROJECTION_ID || int(projection) >= len(ctx.ir_function.projections) {
			return prepare_error("field_addr operation is missing canonical projection attribute", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Field_Addr, op.source, ctx.module.allocator)
		instruction.address_kind = .Field
		instruction.payload = prepared_function_add_field_projection(ctx, projection, prepared_field_address_pointee_type(ctx, op))
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Deref:
		instruction := prepared_instruction_make(.Deref, op.source, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Load:
		if len(op.operands) < 2 {
			return prepare_error("load operation is missing canonical address operand", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Load, op.source, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Store:
		if len(op.operands) < 3 {
			return prepare_error("store operation is missing canonical address/value operands", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Store, op.source, ctx.module.allocator)
		prepared_instruction_set_address_from_operand(ctx, &instruction, op.operands[1])
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Struct_Init:
		instruction := prepared_instruction_make(.Struct_Init, op.source, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Extract_Value:
		projection, ok := op.attrs.(ir.Projection_Id)
		if !ok || projection == ir.INVALID_PROJECTION_ID || int(projection) >= len(ctx.ir_function.projections) {
			return prepare_error("extract_value operation is missing canonical projection attribute", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Extract_Value, op.source, ctx.module.allocator)
		result_type := ir.value_type(ctx.ir_function, op.results[0])
		instruction.payload = prepared_function_add_field_projection(ctx, projection, result_type)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Cast:
		instruction := prepared_instruction_make(.Cast, op.source, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Call:
		attrs, ok := op.attrs.(ir.Call_Attrs)
		if !ok || attrs.target == ir.INVALID_FUNCTION_ID {
			return prepare_error("call operation is missing canonical call target", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Call, op.source, ctx.module.allocator)
		instruction.call_target = attrs.target
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Invoke:
		instruction := prepared_instruction_make(.Invoke, op.source, ctx.module.allocator)
		if op.intrinsic != ir.INVALID_INTRINSIC_ID {
			if int(op.intrinsic) >= len(ctx.ir_module.intrinsics) {
				return prepare_error("invoke operation is missing canonical intrinsic declaration", op.source, ctx.module.allocator)
			}
			intrinsic := ctx.ir_module.intrinsics[int(op.intrinsic)]
			instruction.intrinsic_op = intrinsic.op
			instruction.intrinsic_family = intrinsic.family
			instruction.intrinsic_name = strings.clone(intrinsic.name, ctx.module.allocator)
			instruction.intrinsic_payload = ir.intrinsic_payload_clone(intrinsic.payload, ctx.module.allocator)
		} else {
			attrs, ok := op.attrs.(ir.Call_Attrs)
			if !ok || attrs.target == ir.INVALID_FUNCTION_ID {
				return prepare_error("invoke operation is missing canonical call target", op.source, ctx.module.allocator)
			}
			instruction.call_target = attrs.target
		}
		call_operand_count := prepared_call_operand_count(op)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:call_operand_count], op.results[:])
		if !prepared_instruction_set_invoke_edges(ctx, &instruction, op) {
			return prepare_error("invoke operation is missing canonical normal/exception edges", op.source, ctx.module.allocator)
		}
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Trap:
		attrs, ok := op.attrs.(ir.Trap_Attrs)
		if !ok || attrs.message == "" {
			return prepare_error("trap operation is missing canonical message attrs", op.source, ctx.module.allocator)
		}
		instruction := prepared_instruction_make(.Trap, op.source, ctx.module.allocator)
		instruction.trap_message = strings.clone(attrs.message, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Intrinsic:
		if op.intrinsic == ir.INVALID_INTRINSIC_ID || int(op.intrinsic) >= len(ctx.ir_module.intrinsics) {
			return prepare_error("intrinsic operation is missing canonical intrinsic declaration", op.source, ctx.module.allocator)
		}
		intrinsic := ctx.ir_module.intrinsics[int(op.intrinsic)]
		instruction := prepared_instruction_make(.Intrinsic, op.source, ctx.module.allocator)
		instruction.intrinsic_op = intrinsic.op
		instruction.intrinsic_family = intrinsic.family
		instruction.intrinsic_name = strings.clone(intrinsic.name, ctx.module.allocator)
		instruction.intrinsic_payload = ir.intrinsic_payload_clone(intrinsic.payload, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	}
	_, message := op_supported(op)
	return prepare_error(message, op.source, ctx.module.allocator)
}

prepared_call_operand_count :: proc "contextless" (op: ir.Op) -> int {
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

prepared_instruction_set_invoke_edges :: proc(
	ctx: ^Prepare_Context,
	instruction: ^Prepared_Instruction,
	op: ir.Op,
) -> bool {
	normal, normal_ok := prepared_invoke_edge(op, .Normal)
	exception, exception_ok := prepared_invoke_edge(op, .Exception)
	if !normal_ok || !exception_ok {
		return false
	}
	normal_edge := prepared_emit_edge(ctx, normal.target, normal.args[:])
	exception_edge := prepared_emit_edge(ctx, exception.target, exception.args[:])
	assert(exception_edge == normal_edge + 1)
	instruction.edge_start = normal_edge
	instruction.edge_count = 2
	return true
}

prepared_invoke_edge :: proc "contextless" (op: ir.Op, kind: ir.Edge_Kind) -> (ir.Successor_Edge, bool) {
	for edge in op.successors {
		if edge.kind == kind {
			return edge, true
		}
	}
	return {}, false
}

prepare_terminator :: proc(ctx: ^Prepare_Context, term: ir.Op) -> Prepare_Result {
	#partial switch term.opcode {
	case .Br:
		edge := term.successors[0]
		edge_start := prepared_emit_edge(ctx, edge.target, edge.args[:])
		instruction := prepared_instruction_make(.Br, term.source, ctx.module.allocator)
		instruction.edge_start = edge_start
		instruction.edge_count = 1
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Cond_Br:
		true_successor := term.successors[0]
		false_successor := term.successors[1]
		true_edge := prepared_emit_edge(ctx, true_successor.target, true_successor.args[:])
		false_edge := prepared_emit_edge(ctx, false_successor.target, false_successor.args[:])
		assert(false_edge == true_edge + 1)
		instruction := prepared_instruction_make(.Cond_Br, term.source, ctx.module.allocator)
		instruction.edge_start = true_edge
		instruction.edge_count = 2
		condition := [?]ir.Value_Id{term.operands[0]}
		prepared_instruction_set_registers(ctx, &instruction, condition[:], nil)
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Switch:
		instruction := prepared_instruction_make(.Switch, term.source, ctx.module.allocator)
		selector := [?]ir.Value_Id{term.operands[0]}
		prepared_instruction_set_registers(ctx, &instruction, selector[:], nil)
		instruction.edge_start = u32(len(ctx.out_function.edges))
		for edge in term.successors {
			edge_index := prepared_emit_edge(ctx, edge.target, edge.args[:])
			if edge.case_value != ir.INVALID_VALUE_ID {
				ctx.out_function.edges[int(edge_index)].case_value = register(edge.case_value)
			}
		}
		instruction.edge_count = u32(len(term.successors))
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Return:
		instruction := prepared_instruction_make(.Return, term.source, ctx.module.allocator)
		prepared_instruction_set_registers(ctx, &instruction, term.operands[:], nil)
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	case .Unreachable:
		instruction := prepared_instruction_make(.Unreachable, term.source, ctx.module.allocator)
		prepared_emit(ctx, instruction)
		return Prepare_Result{ok = true}
	}
	_, message := terminator_supported(term)
	return prepare_error(message, term.source, ctx.module.allocator)
}

prepared_emit :: proc(ctx: ^Prepare_Context, instruction: Prepared_Instruction) {
	append(&ctx.out_function.instructions, instruction)
}

prepared_instruction_make :: proc(
	opcode: ir.Opcode,
	source: ir.Source_Loc = {},
	allocator: mem.Allocator = context.allocator,
) -> Prepared_Instruction {
	return Prepared_Instruction {
		opcode = opcode,
		call_target = ir.INVALID_FUNCTION_ID,
		intrinsic_op = .Unknown,
		intrinsic_family = .Unknown,
		source = prepared_source_from_ir(source, allocator),
	}
}

prepared_source_from_ir :: proc(source: ir.Source_Loc, allocator: mem.Allocator) -> runtime.Source_Loc {
	path := ""
	if source.file != nil {
		path = strings.clone(source.file.path, allocator)
	}
	return runtime.Source_Loc {
		path = path,
		range = runtime.Source_Range {
			start = source.range.start,
			end = source.range.end,
		},
	}
}

prepared_instruction_set_registers :: proc(
	ctx: ^Prepare_Context,
	instruction: ^Prepared_Instruction,
	operands: []ir.Value_Id,
	results: []ir.Value_Id,
) {
	instruction.operand_start = u32(len(ctx.out_function.operand_registers))
	instruction.operand_count = u32(len(operands))
	for operand in operands {
		append(&ctx.out_function.operand_registers, register(operand))
	}
	instruction.result_start = u32(len(ctx.out_function.result_registers))
	instruction.result_count = u32(len(results))
	for result in results {
		append(&ctx.out_function.result_registers, register(result))
	}
}

prepared_instruction_set_address_from_operand :: proc(
	ctx: ^Prepare_Context,
	instruction: ^Prepared_Instruction,
	address: ir.Value_Id,
) {
	if address == ir.INVALID_VALUE_ID || int(address) >= len(ctx.ir_function.values) {
		return
	}
	value := ir.value_ptr(ctx.ir_function, address)
	if value.op == ir.INVALID_OP_ID || int(value.op) >= len(ctx.ir_function.instructions) {
		return
	}
	address_op := ir.op_ptr(ctx.ir_function, value.op)
	#partial switch address_op.opcode {
	case .Addr_Of:
		if attrs, ok := address_op.attrs.(ir.Slot_Address_Attrs); ok {
			instruction.address_kind = .Slot
			instruction.payload = u32(attrs.slot)
		}
	case .Field_Addr:
		if projection, ok := address_op.attrs.(ir.Projection_Id); ok {
			instruction.address_kind = .Field
			instruction.payload = prepared_function_add_field_projection(ctx, projection, prepared_field_address_pointee_type(ctx, address_op^))
		}
	}
}

prepared_field_address_pointee_type :: proc(ctx: ^Prepare_Context, op: ir.Op) -> ir.Type_Id {
	if len(op.results) == 0 {
		return ir.BUILTIN_TYPE_UNKNOWN
	}
	result_type := ir.value_type(ctx.ir_function, op.results[0])
	if result_type == ir.INVALID_TYPE_ID || int(result_type) >= len(ctx.ir_module.types) {
		return ir.BUILTIN_TYPE_UNKNOWN
	}
	type_record := ir.type_ptr(ctx.ir_module, result_type)
	if data, ok := type_record.data.(ir.Reference_Type_Data); ok &&
	   data.pointee != ir.INVALID_TYPE_ID &&
	   int(data.pointee) < len(ctx.ir_module.types) {
		return data.pointee
	}
	return result_type
}

prepared_emit_edge :: proc(
	ctx: ^Prepare_Context,
	target: ir.Block_Id,
	args: []ir.Value_Id,
) -> u32 {
	target_block := ir.block_ptr(ctx.ir_function, target)
	assert(len(target_block.args) == len(args))
	edge := Prepared_Edge {
		target = ctx.out_function.block_offsets[int(target)],
		case_value = INVALID_REGISTER,
		arg_start = u32(len(ctx.out_function.edge_registers)),
		arg_count = u32(len(args)),
	}
	for arg in args {
		append(&ctx.out_function.edge_registers, register(arg))
	}
	edge.param_start = u32(len(ctx.out_function.edge_registers))
	edge.param_count = u32(len(target_block.args))
	for param in target_block.args {
		append(&ctx.out_function.edge_registers, register(param))
	}
	edge_index := u32(len(ctx.out_function.edges))
	append(&ctx.out_function.edges, edge)
	return edge_index
}

prepared_function_add_field_projection :: proc(
	ctx: ^Prepare_Context,
	projection: ir.Projection_Id,
	result_type: ir.Type_Id,
) -> u32 {
	ref := Field_Ref {
		result_type = prepared_type_descriptor(ctx.module, result_type),
		result_type_name = clone_type_name_from_cache(ctx.module, result_type),
		field_index = UNKNOWN_FIELD_INDEX,
		byte_offset = UNKNOWN_FIELD_BYTE_OFFSET,
		projection = make([dynamic]Field_Segment_Ref, 0, 1, ctx.module.allocator),
	}
	if projection != ir.INVALID_PROJECTION_ID &&
	   int(projection) < len(ctx.ir_function.projections) {
		path := ir.projection_ptr(ctx.ir_function, projection)
		if len(path.segments) > 0 {
			for segment in path.segments {
				prepared_segment := Field_Segment_Ref {
					name = strings.clone(segment.name, ctx.module.allocator),
					field_index = segment.field_index,
					byte_offset = UNKNOWN_FIELD_BYTE_OFFSET,
				}
				append(&ref.projection, prepared_segment)
			}
			last := path.segments[len(path.segments) - 1]
			ref.name = last.name
			ref.field_index = last.field_index
		}
	}
	return prepared_function_add_field(ctx.out_function, ref, ctx.module.allocator)
}

prepared_function_add_field :: proc(
	function: ^Prepared_Function,
	ref: Field_Ref,
	allocator: mem.Allocator,
) -> u32 {
	for field, i in function.fields {
		if field_ref_equal(field, ref) {
			discard := ref
			field_ref_destroy_projection(&discard, allocator)
			return u32(i)
		}
	}
	index := u32(len(function.fields))
	stored := ref
	stored.name = strings.clone(ref.name, allocator)
	append(&function.fields, stored)
	return index
}

field_ref_equal :: proc "contextless" (left, right: Field_Ref) -> bool {
	if left.name != right.name ||
	   left.result_type != right.result_type ||
	   left.result_type_name != right.result_type_name ||
	   left.field_index != right.field_index ||
	   left.byte_offset != right.byte_offset ||
	   len(left.projection) != len(right.projection) {
		return false
	}
	for segment, i in left.projection {
		other := right.projection[i]
		if segment.name != other.name ||
		   segment.field_index != other.field_index ||
		   segment.byte_offset != other.byte_offset {
			return false
		}
	}
	return true
}

field_ref_destroy_projection :: proc(ref: ^Field_Ref, allocator: mem.Allocator) {
	delete(ref.result_type_name, allocator)
	for segment in ref.projection {
		delete(segment.name, allocator)
	}
	delete(ref.projection)
	ref.projection = nil
}

field_ref_destroy_owned :: proc(ref: ^Field_Ref, allocator: mem.Allocator) {
	delete(ref.name, allocator)
	delete(ref.result_type_name, allocator)
	for segment in ref.projection {
		delete(segment.name, allocator)
	}
	delete(ref.projection)
	ref^ = {}
}

prepared_function_add_constant :: proc(
	function: ^Prepared_Function,
	literal: string,
	allocator: mem.Allocator,
) -> u32 {
	for constant, i in function.constants {
		if constant == literal {
			return u32(i)
		}
	}
	index := u32(len(function.constants))
	append(&function.constants, strings.clone(literal, allocator))
	return index
}

register :: proc "contextless" (value: ir.Value_Id) -> Register {
	if value == ir.INVALID_VALUE_ID {
		return INVALID_REGISTER
	}
	return Register(value)
}

prepare_error :: proc(
	message: string,
	source: ir.Source_Loc = {},
	allocator: mem.Allocator = context.allocator,
) -> Prepare_Result {
	return Prepare_Result {
		ok = false,
		message = strings.clone(message, allocator),
		source = source,
	}
}

run_result_trapped :: proc(
	kind: runtime.Trap_Kind,
	message: string,
	source: ir.Source_Loc,
	allocator: mem.Allocator,
) -> Run_Result {
	return Run_Result {
		status = .Trapped,
		trap = runtime.Trap {
				kind = kind,
				message = strings.clone(message, allocator),
				source = runtime.source_loc_clone(runtime_source_from_ir(source), allocator),
		},
		stack_trace = make([dynamic]Stack_Trace_Frame, 0, 0, allocator),
		events = make([dynamic]runtime.IO_Event, 0, 0, allocator),
		final_values = make([dynamic]runtime.Named_Value, 0, 0, allocator),
	}
}
