package abap_frontend_ir_bytecode

import ir "src:ir"

import "core:mem"
import "core:strings"

Function_Id :: distinct u32
Block_Offset :: distinct u32
Register :: distinct u32

INVALID_FUNCTION_ID :: Function_Id(0xffffffff)
INVALID_BLOCK_OFFSET :: Block_Offset(0xffffffff)
INVALID_REGISTER :: Register(0xffffffff)

Op :: enum {
	Nop,
	Const,
	Move,
	Load,
	Store,
	Field_Load,
	Field_Store,
	Cast,
	Call,
	Call_Runtime,
	Branch,
	Cond_Branch,
	Return,
	Unreachable,
	Unsupported,
}

Edge :: struct {
	target:      Block_Offset,
	arg_start:   u32,
	arg_count:   u32,
	param_start: u32,
	param_count: u32,
}

Instruction :: struct {
	op:           Op,
	dst:          Register,
	src0:         Register,
	src1:         Register,
	target:       Block_Offset,
	payload:      u32,
	operand_start: u32,
	operand_count: u32,
	result_start:  u32,
	result_count:  u32,
	edge_start:    u32,
	edge_count:    u32,
	source:       ir.Source_Loc,
}

Function :: struct {
	name:             string,
	role:             ir.Function_Role,
	register_count:   u32,
	block_offsets:    [dynamic]Block_Offset,
	block_debug:      [dynamic]Block_Debug,
	block_params:     [dynamic]Register,
	constants:        [dynamic]string,
	fields:           [dynamic]string,
	slots:            [dynamic]Slot_Debug,
	values:           [dynamic]Value_Debug,
	return_type_names: [dynamic]string,
	operand_registers: [dynamic]Register,
	result_registers:  [dynamic]Register,
	edge_registers:    [dynamic]Register,
	edges:            [dynamic]Edge,
	runtime_callbacks: [dynamic]Runtime_Callback,
	instructions:     [dynamic]Instruction,
}

Module :: struct {
	allocator:  mem.Allocator,
	entries:    [dynamic]Function_Id,
	type_names: [dynamic]string,
	functions:  [dynamic]Function,
}

Lower_Result :: struct {
	module:      Module,
	ok:          bool,
	message:     string,
	source:      ir.Source_Loc,
}

Runtime_Callback_Kind :: enum {
	Abap,
	Call,
	Table,
	Sql,
	System_Field,
	Unsupported,
}

Runtime_Callback_Result :: enum {
	Ok,
	Trap,
	Unsupported,
}

Runtime_Callback_Proc :: #type proc(
	callback: ^Runtime_Callback,
	instruction: Instruction,
) -> Runtime_Callback_Result

Runtime_Callback :: struct {
	kind:    Runtime_Callback_Kind,
	name:    string,
	op_kind: ir.Op_Kind,
	payload: ir.Op_Payload,
	invoke:  Runtime_Callback_Proc,
	data:    rawptr,
}

Slot_Debug :: struct {
	kind:      ir.Slot_Kind,
	name:      string,
	type_name: string,
}

Value_Debug :: struct {
	name:      string,
	type_name: string,
}

Block_Debug :: struct {
	name:        string,
	param_start: u32,
	param_count: u32,
}

Unsupported_Search :: struct {
	found:   bool,
	message: string,
	source:  ir.Source_Loc,
}

Lower_Context :: struct {
	module:         ^Module,
	out_function:   ^Function,
	ir_function:    ^ir.Function,
	instruction_ip: u32,
}

module_make :: proc(allocator: mem.Allocator = context.allocator) -> Module {
	return Module {
		allocator = allocator,
		entries = make([dynamic]Function_Id, 0, 4, allocator),
		type_names = make([dynamic]string, 0, 16, allocator),
		functions = make([dynamic]Function, 0, 8, allocator),
	}
}

module_destroy :: proc(module: ^Module) {
	assert(module != nil)
	for &function in module.functions {
		function_destroy(&function, module.allocator)
	}
	for type_name in module.type_names {
		delete(type_name, module.allocator)
	}
	delete(module.entries)
	delete(module.type_names)
	delete(module.functions)
	module^ = {}
}

// Bytecode lowering is the runtime boundary. It consumes only verified IR,
// rejects source-bearing unsupported IR before instruction emission, assigns one
// stable bytecode register per IR value, lays blocks out in IR block order, and
// reports the source range of the first operation or terminator outside the
// current bytecode slice. Structural core operations lower to native bytecode;
// ABAP, call, table, SQL, and system-field behavior lowers to callback table
// entries whose instruction payloads are bytecode-local indexes. Callback invoke
// procedures remain nil until a VM or interpreter owns execution.
lower_module :: proc(
	module: ^ir.Module,
	allocator: mem.Allocator = context.allocator,
) -> Lower_Result {
	verify := ir.verify_module(module, context.temp_allocator)
	defer ir.verify_result_destroy(&verify)
	if !verify.ok {
		return Lower_Result {
			ok = false,
			message = verify.diagnostics[0].message if len(verify.diagnostics) > 0 else "IR verification failed",
			source = verify.diagnostics[0].source if len(verify.diagnostics) > 0 else ir.Source_Loc{},
		}
	}
	unsupported := find_unsupported(module)
	if unsupported.found {
		return Lower_Result {
			ok = false,
			message = unsupported.message,
			source = unsupported.source,
		}
	}
	bytecode := module_make(allocator)
	module_copy_debug_metadata(&bytecode, module)
	for _, function_index in module.functions {
		result := lower_function(&bytecode, ir.function_ptr(module, ir.Function_Id(function_index)))
		if !result.ok {
			module_destroy(&bytecode)
			return result
		}
	}
	return Lower_Result {
		module = bytecode,
		ok = true,
	}
}

find_unsupported :: proc "contextless" (module: ^ir.Module) -> Unsupported_Search {
	for function in module.functions {
		for block in function.blocks {
			for op in block.ops {
				if .Unsupported in op.flags {
					return Unsupported_Search {
						found = true,
						message = op.payload.unsupported_message if op.payload.unsupported_message != "" else "unsupported IR operation",
						source = op.source,
					}
				}
			}
		}
	}
	return {}
}

lower_function :: proc(
	bytecode: ^Module,
	function: ^ir.Function,
) -> Lower_Result {
	out_function := Function {
		name = strings.clone(function.name, bytecode.allocator),
		role = function.role,
		register_count = u32(len(function.values)),
		block_offsets = make([dynamic]Block_Offset, 0, len(function.blocks), bytecode.allocator),
		block_debug = make([dynamic]Block_Debug, 0, len(function.blocks), bytecode.allocator),
		block_params = make([dynamic]Register, 0, len(function.values), bytecode.allocator),
		constants = make([dynamic]string, 0, 8, bytecode.allocator),
		fields = make([dynamic]string, 0, 8, bytecode.allocator),
		slots = make([dynamic]Slot_Debug, 0, len(function.slots), bytecode.allocator),
		values = make([dynamic]Value_Debug, 0, len(function.values), bytecode.allocator),
		return_type_names = make([dynamic]string, 0, len(function.return_types), bytecode.allocator),
		operand_registers = make([dynamic]Register, 0, 32, bytecode.allocator),
		result_registers = make([dynamic]Register, 0, 32, bytecode.allocator),
		edge_registers = make([dynamic]Register, 0, 8, bytecode.allocator),
		edges = make([dynamic]Edge, 0, 8, bytecode.allocator),
		runtime_callbacks = make([dynamic]Runtime_Callback, 0, 8, bytecode.allocator),
		instructions = make([dynamic]Instruction, 0, 32, bytecode.allocator),
	}
	function_copy_debug_metadata(&out_function, bytecode, function)

	ip: u32
	for block in function.blocks {
		append(&out_function.block_offsets, Block_Offset(ip))
		for op in block.ops {
			count, ok := op_instruction_count(op)
			if !ok {
				function_destroy(&out_function, bytecode.allocator)
				return lower_error("bytecode lowering does not support IR operation", op.source)
			}
			ip += count
		}
		count, term_ok := terminator_instruction_count(block.term)
		if !term_ok {
			function_destroy(&out_function, bytecode.allocator)
			return lower_error("bytecode lowering does not support IR terminator", block.term.source)
		}
		ip += count
	}

	ctx := Lower_Context {
		module = bytecode,
		out_function = &out_function,
		ir_function = function,
	}
	for &block in function.blocks {
		for &op in block.ops {
			result := emit_op(&ctx, op)
			if !result.ok {
				function_destroy(&out_function, bytecode.allocator)
				return result
			}
		}
		result := emit_terminator(&ctx, block.term)
		if !result.ok {
			function_destroy(&out_function, bytecode.allocator)
			return result
		}
	}

	append(&bytecode.functions, out_function)
	return Lower_Result{ok = true}
}

function_destroy :: proc(function: ^Function, allocator: mem.Allocator) {
	assert(function != nil)
	delete(function.name, allocator)
	for return_type_name in function.return_type_names {
		delete(return_type_name, allocator)
	}
	for block in function.block_debug {
		delete(block.name, allocator)
	}
	for constant in function.constants {
		delete(constant, allocator)
	}
	for field in function.fields {
		delete(field, allocator)
	}
	for slot in function.slots {
		delete(slot.name, allocator)
		delete(slot.type_name, allocator)
	}
	for value in function.values {
		delete(value.name, allocator)
		delete(value.type_name, allocator)
	}
	for callback in function.runtime_callbacks {
		delete(callback.name, allocator)
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
	delete(function.runtime_callbacks)
	delete(function.instructions)
	function^ = {}
}

module_copy_debug_metadata :: proc(bytecode: ^Module, module: ^ir.Module) {
	for entry in module.entries {
		append(&bytecode.entries, Function_Id(u32(entry)))
	}
	for _, i in module.types {
		append(&bytecode.type_names, clone_type_name(module, ir.Type_Id(i), bytecode.allocator))
	}
}

function_copy_debug_metadata :: proc(
	out_function: ^Function,
	bytecode: ^Module,
	function: ^ir.Function,
) {
	for typ in function.return_types {
		append(&out_function.return_type_names, clone_type_name_from_cache(bytecode, typ))
	}
	for slot in function.slots {
		append(
			&out_function.slots,
			Slot_Debug {
				kind = slot.kind,
				name = strings.clone(slot.name, bytecode.allocator),
				type_name = clone_type_name_from_cache(bytecode, slot.type),
			},
		)
	}
	for value in function.values {
		append(
			&out_function.values,
			Value_Debug {
				name = strings.clone(value.name, bytecode.allocator),
				type_name = clone_type_name_from_cache(bytecode, value.type),
			},
		)
	}
	for block in function.blocks {
		debug := Block_Debug {
			name = strings.clone(block.name, bytecode.allocator),
			param_start = u32(len(out_function.block_params)),
			param_count = u32(len(block.params)),
		}
		for param in block.params {
			append(&out_function.block_params, register(param.value))
		}
		append(&out_function.block_debug, debug)
	}
}

clone_type_name_from_cache :: proc(bytecode: ^Module, typ: ir.Type_Id) -> string {
	index := int(typ)
	if index >= 0 && index < len(bytecode.type_names) {
		return strings.clone(bytecode.type_names[index], bytecode.allocator)
	}
	return strings.clone("!invalid", bytecode.allocator)
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

op_instruction_count :: proc "contextless" (op: ir.Op) -> (u32, bool) {
	#partial switch op.kind {
	case .Core_Const,
	     .Core_Load,
	     .Core_Store,
	     .Core_Field_Load,
	     .Core_Field_Store,
	     .Core_Cast,
	     .Core_Call:
		return 1, true
	}
	if _, ok := runtime_callback_kind(op.kind); ok {
		return 1, true
	}
	return 0, false
}

terminator_instruction_count :: proc "contextless" (term: ir.Terminator) -> (u32, bool) {
	#partial switch term.kind {
	case .Branch:
		return 1, true
	case .Cond_Branch:
		return 1, true
	case .Return:
		return 1, true
	case .Unreachable:
		return 1, true
	}
	return 0, false
}

runtime_callback_kind :: proc "contextless" (kind: ir.Op_Kind) -> (Runtime_Callback_Kind, bool) {
	#partial switch kind {
	case .Abap_Move,
	     .Abap_Add,
	     .Abap_Subtract,
	     .Abap_Multiply,
	     .Abap_Divide,
	     .Abap_Equal,
	     .Abap_Not_Equal,
	     .Abap_Less,
	     .Abap_Less_Equal,
	     .Abap_Greater,
	     .Abap_Greater_Equal,
	     .Abap_And,
	     .Abap_Or,
	     .Abap_Not,
	     .Abap_Is_Initial,
	     .Abap_String_Concat,
	     .Abap_String_Template,
	     .Abap_Construct,
	     .Abap_Message,
	     .Abap_Write,
	     .Abap_Clear,
	     .Abap_Refresh,
	     .Abap_Free,
	     .Abap_Assign_Field,
	     .Abap_Unassign:
		return .Abap, true
	case .Abap_Builtin_Call,
	     .Abap_Routine_Call,
	     .Abap_Method_Call:
		return .Call, true
	case .Table_Iter,
	     .Table_Next,
	     .Table_Read,
	     .Table_Append,
	     .Table_Insert,
	     .Table_Modify,
	     .Table_Delete,
	     .Table_Sort:
		return .Table, true
	case .Sql_Select,
	     .Sql_Open_Cursor,
	     .Sql_Fetch,
	     .Sql_Close_Cursor,
	     .Sql_Insert,
	     .Sql_Update,
	     .Sql_Delete,
	     .Sql_Modify:
		return .Sql, true
	case .System_Read,
	     .System_Write:
		return .System_Field, true
	}
	return .Unsupported, false
}

emit_op :: proc(ctx: ^Lower_Context, op: ir.Op) -> Lower_Result {
	#partial switch op.kind {
	case .Core_Const:
		payload := function_add_constant(ctx.out_function, op.payload.literal, ctx.module.allocator)
		instruction := instruction_make(.Const, op.source)
		instruction.payload = payload
		instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Core_Load:
		instruction := instruction_make(.Load, op.source)
		instruction.payload = u32(op.payload.slot)
		instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Core_Store:
		instruction := instruction_make(.Store, op.source)
		instruction.payload = u32(op.payload.slot)
		instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Core_Field_Load:
		instruction := instruction_make(.Field_Load, op.source)
		instruction.payload = function_add_field(ctx.out_function, op.payload.field_name, ctx.module.allocator)
		instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Core_Field_Store:
		instruction := instruction_make(.Field_Store, op.source)
		instruction.payload = function_add_field(ctx.out_function, op.payload.field_name, ctx.module.allocator)
		instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Core_Cast:
		instruction := instruction_make(.Cast, op.source)
		instruction.payload = u32(ir.value_type(ctx.ir_function, op.results[0]))
		instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Core_Call:
		instruction := instruction_make(.Call, op.source)
		instruction.payload = u32(op.payload.call_function_target)
		instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	}
	if kind, ok := runtime_callback_kind(op.kind); ok {
		callback_index := function_add_runtime_callback(ctx.out_function, op, kind, ctx.module.allocator)
		instruction := instruction_make(.Call_Runtime, op.source)
		instruction.payload = callback_index
		instruction_set_registers(ctx, &instruction, op.operands[:], op.results[:])
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	}
	return lower_error("bytecode lowering does not support IR operation", op.source)
}

emit_terminator :: proc(ctx: ^Lower_Context, term: ir.Terminator) -> Lower_Result {
	#partial switch term.kind {
	case .Branch:
		edge_start := emit_edge(ctx, term.target, term.target_args[:])
		edge := &ctx.out_function.edges[int(edge_start)]
		instruction := instruction_make(.Branch, term.source)
		instruction.target = edge.target
		instruction.edge_start = edge_start
		instruction.edge_count = 1
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Cond_Branch:
		true_edge := emit_edge(ctx, term.true_target, term.true_args[:])
		false_edge := emit_edge(ctx, term.false_target, term.false_args[:])
		assert(false_edge == true_edge + 1)
		instruction := instruction_make(.Cond_Branch, term.source)
		instruction.target = ctx.out_function.edges[int(true_edge)].target
		instruction.payload = u32(ctx.out_function.edges[int(false_edge)].target)
		instruction.edge_start = true_edge
		instruction.edge_count = 2
		condition := [?]ir.Value_Id{term.condition}
		instruction_set_registers(ctx, &instruction, condition[:], nil)
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Return:
		instruction := instruction_make(.Return, term.source)
		instruction.payload = u32(len(term.values))
		instruction_set_registers(ctx, &instruction, term.values[:], nil)
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	case .Unreachable:
		instruction := instruction_make(.Unreachable, term.source)
		emit(ctx, instruction)
		return Lower_Result{ok = true}
	}
	return lower_error("bytecode lowering does not support IR terminator", term.source)
}

emit :: proc(ctx: ^Lower_Context, instruction: Instruction) {
	append(&ctx.out_function.instructions, instruction)
	ctx.instruction_ip += 1
}

instruction_make :: proc "contextless" (op: Op, source: ir.Source_Loc = {}) -> Instruction {
	return Instruction {
		op = op,
		dst = INVALID_REGISTER,
		src0 = INVALID_REGISTER,
		src1 = INVALID_REGISTER,
		target = INVALID_BLOCK_OFFSET,
		source = source,
	}
}

instruction_set_registers :: proc(
	ctx: ^Lower_Context,
	instruction: ^Instruction,
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
	instruction.src0 = register(operands[0]) if len(operands) > 0 else INVALID_REGISTER
	instruction.src1 = register(operands[1]) if len(operands) > 1 else INVALID_REGISTER
	instruction.dst = register(results[0]) if len(results) > 0 else INVALID_REGISTER
}

emit_edge :: proc(
	ctx: ^Lower_Context,
	target: ir.Block_Id,
	args: []ir.Value_Id,
) -> u32 {
	target_block := ir.block_ptr(ctx.ir_function, target)
	assert(len(target_block.params) == len(args))
	edge := Edge {
		target = ctx.out_function.block_offsets[int(target)],
		arg_start = u32(len(ctx.out_function.edge_registers)),
		arg_count = u32(len(args)),
	}
	for arg in args {
		append(&ctx.out_function.edge_registers, register(arg))
	}
	edge.param_start = u32(len(ctx.out_function.edge_registers))
	edge.param_count = u32(len(target_block.params))
	for param in target_block.params {
		append(&ctx.out_function.edge_registers, register(param.value))
	}
	edge_index := u32(len(ctx.out_function.edges))
	append(&ctx.out_function.edges, edge)
	return edge_index
}

function_add_field :: proc(
	function: ^Function,
	name: string,
	allocator: mem.Allocator,
) -> u32 {
	for field, i in function.fields {
		if field == name {
			return u32(i)
		}
	}
	index := u32(len(function.fields))
	append(&function.fields, strings.clone(name, allocator))
	return index
}

function_add_constant :: proc(
	function: ^Function,
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

function_add_runtime_callback :: proc(
	function: ^Function,
	op: ir.Op,
	kind: Runtime_Callback_Kind,
	allocator: mem.Allocator,
) -> u32 {
	index := u32(len(function.runtime_callbacks))
	append(
		&function.runtime_callbacks,
		Runtime_Callback {
			kind = kind,
			name = strings.clone(ir.op_kind_name(op.kind), allocator),
			op_kind = op.kind,
			payload = op.payload,
		},
	)
	return index
}

register :: proc "contextless" (value: ir.Value_Id) -> Register {
	if value == ir.INVALID_VALUE_ID {
		return INVALID_REGISTER
	}
	return Register(value)
}

lower_error :: proc(
	message: string,
	source: ir.Source_Loc = {},
) -> Lower_Result {
	return Lower_Result {
		ok = false,
		message = message,
		source = source,
	}
}
