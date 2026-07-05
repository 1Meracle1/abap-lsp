package abap_frontend_ir

import "src:ast"
import semantic "src:semantic"

import "core:mem"
import "core:strconv"
import "core:strings"

Builder :: struct {
	module:        ^Module,
	function_id:   Function_Id,
	block:         Block_Id,
	current_world: Value_Id,
}

module_add_type :: proc(module: ^Module, typ: Type) -> Type_Id {
	assert(module != nil)
	if typ.semantic_type != nil {
		for existing, i in module.types {
			if existing.semantic_type == typ.semantic_type {
				return Type_Id(i)
			}
		}
	}
	if typ.name != "" {
		for existing, i in module.types {
			if existing.kind == typ.kind && existing.name == typ.name && existing.semantic_type == typ.semantic_type {
				return Type_Id(i)
			}
		}
	}
	id := Type_Id(len(module.types))
	append(&module.types, typ)
	return id
}

module_type_from_semantic :: proc(module: ^Module, typ: ^semantic.Type) -> Type_Id {
	assert(module != nil)
	if typ == nil {
		return BUILTIN_TYPE_UNKNOWN
	}
	for existing, i in module.types {
		if existing.semantic_type == typ {
			return Type_Id(i)
		}
	}
	kind := Type_Kind.Semantic
	#partial switch typ.kind {
	case .Unknown:
		kind = .Unknown
	case .Builtin:
		if typ.name == "i" {
			return BUILTIN_TYPE_INTEGER
		}
		if typ.name == "string" {
			return BUILTIN_TYPE_STRING
		}
		kind = .Semantic
	case .Structure:
		kind = .Structure
	case .Table:
		kind = .Table
	case .Ref:
		kind = .Reference
	case .Routine:
		kind = .Routine
	}
	name := module_semantic_type_name(module, typ)
	if name == "" {
		#partial switch kind {
		case .Structure:
			name = "structure"
		case .Table:
			name = "table"
		case .Reference:
			name = "ref"
		case .Routine:
			name = "routine"
		case .Unknown:
			name = "unknown"
		}
	}
	return module_add_type(module, Type{kind = kind, name = name, semantic_type = typ})
}

module_semantic_type_name :: proc(module: ^Module, typ: ^semantic.Type) -> string {
	name := typ.name
	if typ.kind == .Builtin && typ.has_length {
		builder := strings.builder_make(module.allocator)
		strings.write_string(&builder, name)
		strings.write_byte(&builder, '(')
		buf: [32]byte
		strings.write_string(&builder, strconv.write_int(buf[:], i64(typ.length), 10))
		if typ.has_decimals {
			strings.write_byte(&builder, ',')
			strings.write_string(&builder, strconv.write_int(buf[:], i64(typ.decimals), 10))
		}
		strings.write_byte(&builder, ')')
		return strings.to_string(builder)
	}
	if typ.kind == .Ref {
		base_name := module_semantic_type_name(module, typ.base) if typ.base != nil else ""
		if base_name != "" {
			builder := strings.builder_make(module.allocator)
			strings.write_string(&builder, "ref:")
			strings.write_string(&builder, base_name)
			return strings.to_string(builder)
		}
	}
	return name
}

module_add_function :: proc(
	module: ^Module,
	name: string,
	entity: ^semantic.Entity = nil,
	source: Source_Loc = {},
	role: Function_Role = .Unknown,
) -> Function_Id {
	assert(module != nil)
	id := Function_Id(len(module.functions))
	function := Function {
		name = name,
		role = role,
		entity = entity,
		source = source,
		return_types = make([dynamic]Type_Id, 0, 2, module.allocator),
		slots = make([dynamic]Slot, 0, 16, module.allocator),
		projections = make([dynamic]Projection_Path, 0, 8, module.allocator),
		values = make([dynamic]Value, 0, 32, module.allocator),
		blocks = make([dynamic]Block, 0, 8, module.allocator),
		op_locations = make([dynamic]Op_Location, 0, 32, module.allocator),
		entry = INVALID_BLOCK_ID,
		world_param = INVALID_VALUE_ID,
	}
	append(&function.return_types, BUILTIN_TYPE_WORLD)
	append(&module.functions, function)
	return id
}

module_add_entry :: proc(module: ^Module, function_id: Function_Id) {
	assert(module != nil && function_id != INVALID_FUNCTION_ID && int(function_id) < len(module.functions))
	for entry in module.entries {
		if entry == function_id {
			return
		}
	}
	append(&module.entries, function_id)
}

function_add_block :: proc(
	function: ^Function,
	name: string,
	source: Source_Loc = {},
	allocator: mem.Allocator = context.allocator,
) -> Block_Id {
	assert(function != nil)
	id := Block_Id(len(function.blocks))
	block := Block {
		name = name,
		params = make([dynamic]Block_Param, 0, 2, allocator),
		ops = make([dynamic]Op, 0, 8, allocator),
		source = source,
	}
	append(&function.blocks, block)
	if function.entry == INVALID_BLOCK_ID {
		function.entry = id
	}
	return id
}

function_add_value :: proc(
	function: ^Function,
	kind: Value_Kind,
	typ: Type_Id,
	block: Block_Id,
	op: Op_Id = INVALID_OP_ID,
	result_index: u32 = 0,
	name: string = "",
) -> Value_Id {
	assert(function != nil)
	id := Value_Id(len(function.values))
	append(
		&function.values,
		Value {
			kind = kind,
			type = typ,
			block = block,
			op = op,
			result_index = result_index,
			name = name,
		},
	)
	return id
}

function_add_block_param :: proc(
	function: ^Function,
	block_id: Block_Id,
	typ: Type_Id,
	name: string,
) -> Value_Id {
	block := block_ptr(function, block_id)
	value := function_add_value(function, .Block_Param, typ, block_id, name = name)
	append(&block.params, Block_Param{value = value, name = name})
	return value
}

function_add_slot :: proc(
	function: ^Function,
	kind: Slot_Kind,
	name: string,
	typ: Type_Id,
	entity: ^semantic.Entity = nil,
	source: Source_Loc = {},
) -> Slot_Id {
	assert(function != nil)
	for slot, i in function.slots {
		if entity != nil && slot.entity == entity {
			return Slot_Id(i)
		}
		if entity == nil && slot.entity == nil && slot.kind == kind && slot.name == name {
			return Slot_Id(i)
		}
	}
	id := Slot_Id(len(function.slots))
	append(&function.slots, Slot{kind = kind, name = name, type = typ, entity = entity, source = source})
	return id
}

function_add_projection :: proc(
	function: ^Function,
	segments: []Projection_Segment,
	allocator: mem.Allocator,
) -> Projection_Id {
	assert(function != nil)
	assert(len(segments) > 0)
	id := Projection_Id(len(function.projections))
	path := Projection_Path {
		segments = make([dynamic]Projection_Segment, 0, len(segments), allocator),
	}
	for segment in segments {
		append(&path.segments, segment)
	}
	append(&function.projections, path)
	return id
}

builder_begin_function :: proc(
	module: ^Module,
	name: string,
	entity: ^semantic.Entity = nil,
	source: Source_Loc = {},
	role: Function_Role = .Unknown,
) -> Builder {
	id := module_add_function(module, name, entity, source, role)
	function := function_ptr(module, id)
	entry := function_add_block(function, "entry", source, module.allocator)
	world := function_add_block_param(function, entry, BUILTIN_TYPE_WORLD, "world")
	function.world_param = world
	return Builder {
		module = module,
		function_id = id,
		block = entry,
		current_world = world,
	}
}

builder_function :: #force_inline proc(builder: ^Builder) -> ^Function {
	return function_ptr(builder.module, builder.function_id)
}

builder_position_at_end :: proc(builder: ^Builder, block: Block_Id) {
	function := builder_function(builder)
	builder.block = block
	builder.current_world = block_world_param(function, block)
}

block_world_param :: proc(function: ^Function, block_id: Block_Id) -> Value_Id {
	block := block_ptr(function, block_id)
	for param in block.params {
		if value_type(function, param.value) == BUILTIN_TYPE_WORLD {
			return param.value
		}
	}
	return INVALID_VALUE_ID
}

builder_add_block :: proc(builder: ^Builder, name: string, source: Source_Loc = {}) -> Block_Id {
	return function_add_block(builder_function(builder), name, source, builder.module.allocator)
}

builder_add_world_block :: proc(builder: ^Builder, name: string, source: Source_Loc = {}) -> Block_Id {
	function := builder_function(builder)
	block := function_add_block(function, name, source, builder.module.allocator)
	function_add_block_param(function, block, BUILTIN_TYPE_WORLD, "world")
	return block
}

builder_emit_op :: proc(
	builder: ^Builder,
	kind: Op_Kind,
	operands: []Value_Id = nil,
	result_types: []Type_Id = nil,
	flags: Op_Flags = {},
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> Op_Id {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.term.kind == .Invalid, "cannot emit operation after block terminator")
	for operand in operands {
		assert(operand != INVALID_VALUE_ID && int(operand) < len(function.values), "operation operand must reference an existing value")
	}
	for typ in result_types {
		assert(typ != INVALID_TYPE_ID && int(typ) < len(builder.module.types), "operation result type must reference an existing type")
	}
	op_id := Op_Id(len(function.op_locations))
	op := Op {
		id = op_id,
		kind = kind,
		operands = make([dynamic]Value_Id, 0, len(operands), builder.module.allocator),
		results = make([dynamic]Value_Id, 0, len(result_types), builder.module.allocator),
		type = result_types[0] if len(result_types) > 0 else BUILTIN_TYPE_VOID,
		flags = flags,
		source = source,
		payload = payload,
	}
	for operand in operands {
		append(&op.operands, operand)
	}
	for typ, i in result_types {
		value := function_add_value(function, .Op_Result, typ, builder.block, op_id, u32(i))
		append(&op.results, value)
	}
	append(&block.ops, op)
	append(&function.op_locations, Op_Location{block = builder.block, index = u32(len(block.ops) - 1)})
	return op_id
}

builder_emit_effect_op :: proc(
	builder: ^Builder,
	kind: Op_Kind,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	flags: Op_Flags = {.Reads_World, .Writes_World},
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> Op_Id {
	assert(builder.current_world != INVALID_VALUE_ID, "effect operation requires current world token")
	assert(.Reads_World in flags && .Writes_World in flags, "effect operation must read and write world")
	operands := make([dynamic]Value_Id, 0, 1 + len(inputs), context.temp_allocator)
	defer delete(operands)
	append(&operands, builder.current_world)
	for input in inputs {
		append(&operands, input)
	}

	results := make([dynamic]Type_Id, 0, 1 + len(result_types), context.temp_allocator)
	defer delete(results)
	append(&results, BUILTIN_TYPE_WORLD)
	for typ in result_types {
		append(&results, typ)
	}

	op_id := builder_emit_op(builder, kind, operands[:], results[:], flags, payload, source)
	op := op_ptr(builder_function(builder), op_id)
	builder.current_world = op.results[0]
	return op_id
}

builder_emit_const :: proc(
	builder: ^Builder,
	literal: string,
	typ: Type_Id,
	source: Source_Loc = {},
) -> Value_Id {
	result_types := [?]Type_Id{typ}
	op_id := builder_emit_op(
		builder,
		.Core_Const,
		result_types = result_types[:],
		payload = Op_Payload{literal = literal},
		source = source,
	)
	return op_ptr(builder_function(builder), op_id).results[0]
}

builder_emit_load :: proc(builder: ^Builder, slot: Slot_Id, source: Source_Loc = {}) -> Value_Id {
	function := builder_function(builder)
	typ := slot_ptr(function, slot).type
	operands := [?]Value_Id{builder.current_world}
	result_types := [?]Type_Id{typ}
	op_id := builder_emit_op(
		builder,
		.Core_Load,
		operands = operands[:],
		result_types = result_types[:],
		flags = {.Reads_World},
		payload = Op_Payload{slot = slot},
		source = source,
	)
	return op_ptr(function, op_id).results[0]
}

builder_emit_store :: proc(
	builder: ^Builder,
	slot: Slot_Id,
	value: Value_Id,
	source: Source_Loc = {},
) {
	inputs := [?]Value_Id{value}
	builder_emit_effect_op(
		builder,
		.Core_Store,
		inputs[:],
		flags = {.Reads_World, .Writes_World},
		payload = Op_Payload{slot = slot},
		source = source,
	)
}

builder_emit_core_call :: proc(
	builder: ^Builder,
	target: Function_Id,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	source: Source_Loc = {},
) -> Op_Id {
	assert(builder != nil && builder.module != nil)
	assert(target != INVALID_FUNCTION_ID && int(target) < len(builder.module.functions))
	return builder_emit_effect_op(
		builder,
		.Core_Call,
		inputs,
		result_types,
		payload = Op_Payload {
			call_function_target = target,
			has_call_function_target = true,
		},
		source = source,
	)
}

builder_emit_builtin_call :: proc(
	builder: ^Builder,
	target: ^semantic.Entity,
	callee_name: string,
	result_type: Type_Id,
	inputs: []Value_Id = nil,
	source: Source_Loc = {},
) -> Value_Id {
	result_types := [?]Type_Id{result_type}
	op_id := builder_emit_op(
		builder,
		.Abap_Builtin_Call,
		inputs,
		result_types[:],
		payload = Op_Payload {
			callee_name = callee_name,
			call_kind   = .Builtin,
			call_target = target,
		},
		source = source,
	)
	return op_ptr(builder_function(builder), op_id).results[0]
}

builder_emit_routine_call :: proc(
	builder: ^Builder,
	target: ^semantic.Entity,
	callee_name: string,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	source: Source_Loc = {},
	ast_stmt: ^ast.Stmt = nil,
	call_kind: Abap_Call_Kind = .Unknown,
) -> Op_Id {
	kind := call_kind
	if kind == .Unknown {
		kind = abap_call_kind_for_entity(target)
	}
	return builder_emit_effect_op(
		builder,
		.Abap_Routine_Call,
		inputs,
		result_types,
		payload = Op_Payload {
			callee_name = callee_name,
			call_kind   = kind,
			call_target = target,
			ast_stmt    = ast_stmt,
		},
		source = source,
	)
}

builder_emit_method_call :: proc(
	builder: ^Builder,
	target: ^semantic.Entity,
	callee_name: string,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	source: Source_Loc = {},
	ast_stmt: ^ast.Stmt = nil,
) -> Op_Id {
	return builder_emit_effect_op(
		builder,
		.Abap_Method_Call,
		inputs,
		result_types,
		payload = Op_Payload {
			callee_name   = callee_name,
			call_kind     = .Method,
			call_target   = target,
			ast_stmt      = ast_stmt,
		},
		source = source,
	)
}

builder_emit_message :: proc(
	builder: ^Builder,
	inputs: []Value_Id = nil,
	result_type: Type_Id = INVALID_TYPE_ID,
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> Value_Id {
	result_types := make([dynamic]Type_Id, 0, 1, context.temp_allocator)
	defer delete(result_types)
	if result_type != INVALID_TYPE_ID {
		append(&result_types, result_type)
	}
	op_id := builder_emit_effect_op(
		builder,
		.Abap_Message,
		inputs,
		result_types[:],
		flags = {.Reads_World, .Writes_World, .May_Trap},
		payload = payload,
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	if result_type != INVALID_TYPE_ID && len(op.results) > 1 {
		return op.results[1]
	}
	return INVALID_VALUE_ID
}

builder_emit_write :: proc(
	builder: ^Builder,
	inputs: []Value_Id = nil,
	source: Source_Loc = {},
) {
	builder_emit_effect_op(
		builder,
		.Abap_Write,
		inputs,
		flags = {.Reads_World, .Writes_World},
		source = source,
	)
}

builder_emit_table_iter :: proc(
	builder: ^Builder,
	table: Value_Id,
	row_type: Type_Id,
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> Value_Id {
	inputs := [?]Value_Id{table}
	result_types := [?]Type_Id{BUILTIN_TYPE_TABLE_ITERATOR}
	op_id := builder_emit_effect_op(
		builder,
		.Table_Iter,
		inputs[:],
		result_types[:],
		payload = builder_table_payload(payload, row_type),
		source = source,
	)
	return op_ptr(builder_function(builder), op_id).results[1]
}

builder_emit_table_next :: proc(
	builder: ^Builder,
	iter: Value_Id,
	row_result_type: Type_Id,
	row_type: Type_Id,
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> (
	Value_Id,
	Value_Id,
) {
	inputs := [?]Value_Id{iter}
	result_types := [?]Type_Id{BUILTIN_TYPE_PREDICATE, row_result_type}
	op_id := builder_emit_effect_op(
		builder,
		.Table_Next,
		inputs[:],
		result_types[:],
		payload = builder_table_payload(payload, row_type),
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	return op.results[1], op.results[2]
}

builder_emit_table_read :: proc(
	builder: ^Builder,
	inputs: []Value_Id,
	row_result_type: Type_Id,
	row_type: Type_Id,
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> (
	Value_Id,
	Value_Id,
) {
	result_types := [?]Type_Id{row_result_type, BUILTIN_TYPE_INTEGER}
	op_id := builder_emit_effect_op(
		builder,
		.Table_Read,
		inputs,
		result_types[:],
		payload = builder_table_payload(payload, row_type),
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	return op.results[1], op.results[2]
}

builder_emit_table_mutation :: proc(
	builder: ^Builder,
	kind: Op_Kind,
	inputs: []Value_Id,
	row_type: Type_Id,
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> Op_Id {
	assert(
		kind == .Table_Append ||
		kind == .Table_Insert ||
		kind == .Table_Modify ||
		kind == .Table_Delete ||
		kind == .Table_Sort,
		"table mutation builder requires a table mutation operation",
	)
	return builder_emit_effect_op(
		builder,
		kind,
		inputs,
		flags = {.Reads_World, .Writes_World},
		payload = builder_table_payload(payload, row_type),
		source = source,
	)
}

builder_table_payload :: proc "contextless" (payload: Op_Payload, row_type: Type_Id) -> Op_Payload {
	out := payload
	out.table_row_type = row_type
	return out
}

builder_emit_sql_select :: proc(
	builder: ^Builder,
	result_type: Type_Id,
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> (
	Value_Id,
	Value_Id,
) {
	result_types := [?]Type_Id{result_type, BUILTIN_TYPE_INTEGER}
	op_id := builder_emit_effect_op(
		builder,
		.Sql_Select,
		result_types = result_types[:],
		payload = payload,
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	return op.results[1], op.results[2]
}

builder_emit_sql_cursor :: proc(
	builder: ^Builder,
	kind: Op_Kind,
	handle: Value_Id,
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> Op_Id {
	assert(
		kind == .Sql_Open_Cursor ||
		kind == .Sql_Fetch ||
		kind == .Sql_Close_Cursor,
		"SQL cursor builder requires a cursor operation",
	)
	inputs := [?]Value_Id{handle}
	return builder_emit_effect_op(
		builder,
		kind,
		inputs[:],
		payload = payload,
		source = source,
	)
}

builder_emit_sql_mutation :: proc(
	builder: ^Builder,
	kind: Op_Kind,
	inputs: []Value_Id = nil,
	payload: Op_Payload = {},
	source: Source_Loc = {},
) -> Op_Id {
	assert(
		kind == .Sql_Insert ||
		kind == .Sql_Update ||
		kind == .Sql_Delete ||
		kind == .Sql_Modify,
		"SQL mutation builder requires a database mutation operation",
	)
	return builder_emit_effect_op(
		builder,
		kind,
		inputs,
		payload = payload,
		source = source,
	)
}

abap_call_kind_for_entity :: proc "contextless" (entity: ^semantic.Entity) -> Abap_Call_Kind {
	if entity == nil {
		return .Unknown
	}
	#partial switch entity.kind {
	case .Builtin:
		return .Builtin
	case .Form:
		return .Form
	case .Method:
		return .Method
	case .Module:
		return .Module
	case .Event:
		return .Routine
	}
	return .Routine
}

builder_emit_system_write :: proc(
	builder: ^Builder,
	field_name: string,
	value: Value_Id = INVALID_VALUE_ID,
	source: Source_Loc = {},
) {
	stored := value
	if stored == INVALID_VALUE_ID {
		stored = builder_emit_const(builder, "0", BUILTIN_TYPE_INTEGER, source)
	}
	inputs := [?]Value_Id{stored}
	builder_emit_effect_op(
		builder,
		.System_Write,
		inputs[:],
		flags = {.Reads_World, .Writes_World},
		payload = Op_Payload{system_field = field_name},
		source = source,
	)
}

builder_emit_system_read :: proc(
	builder: ^Builder,
	field_name: string,
	result_type: Type_Id,
	source: Source_Loc = {},
) -> Value_Id {
	operands := [?]Value_Id{builder.current_world}
	result_types := [?]Type_Id{result_type}
	op_id := builder_emit_op(
		builder,
		.System_Read,
		operands[:],
		result_types[:],
		flags = {.Reads_World},
		payload = Op_Payload{system_field = field_name},
		source = source,
	)
	return op_ptr(builder_function(builder), op_id).results[0]
}

builder_emit_unsupported :: proc(
	builder: ^Builder,
	message: string,
	result_type: Type_Id = INVALID_TYPE_ID,
	source: Source_Loc = {},
) -> Value_Id {
	result_types := make([dynamic]Type_Id, 0, 1, context.temp_allocator)
	defer delete(result_types)
	if result_type != INVALID_TYPE_ID {
		append(&result_types, result_type)
	}
	op_id := builder_emit_effect_op(
		builder,
		.Core_Unsupported,
		result_types = result_types[:],
		flags = {.Reads_World, .Writes_World, .May_Trap, .Unsupported},
		payload = Op_Payload{unsupported_message = message},
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	if result_type != INVALID_TYPE_ID && len(op.results) > 1 {
		return op.results[1]
	}
	return INVALID_VALUE_ID
}

builder_set_branch :: proc(
	builder: ^Builder,
	target: Block_Id,
	args: []Value_Id = nil,
	source: Source_Loc = {},
) {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.term.kind == .Invalid, "cannot replace block terminator")
	assert(target != INVALID_BLOCK_ID && int(target) < len(function.blocks), "branch target must reference an existing block")
	for arg in args {
		assert(arg != INVALID_VALUE_ID && int(arg) < len(function.values), "branch argument must reference an existing value")
	}
	terminator_destroy(&block.term)
	block.term = Terminator {
		kind = .Branch,
		target = target,
		target_args = make([dynamic]Value_Id, 0, len(args), builder.module.allocator),
		source = source,
	}
	for arg in args {
		append(&block.term.target_args, arg)
	}
}

builder_set_branch_world :: proc(builder: ^Builder, target: Block_Id, source: Source_Loc = {}) {
	args := [?]Value_Id{builder.current_world}
	builder_set_branch(builder, target, args[:], source)
}

builder_set_cond_branch :: proc(
	builder: ^Builder,
	condition: Value_Id,
	true_target: Block_Id,
	true_args: []Value_Id,
	false_target: Block_Id,
	false_args: []Value_Id,
	source: Source_Loc = {},
) {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.term.kind == .Invalid, "cannot replace block terminator")
	assert(condition != INVALID_VALUE_ID && int(condition) < len(function.values), "conditional branch must reference an existing condition value")
	assert(true_target != INVALID_BLOCK_ID && int(true_target) < len(function.blocks), "true branch target must reference an existing block")
	assert(false_target != INVALID_BLOCK_ID && int(false_target) < len(function.blocks), "false branch target must reference an existing block")
	for arg in true_args {
		assert(arg != INVALID_VALUE_ID && int(arg) < len(function.values), "true branch argument must reference an existing value")
	}
	for arg in false_args {
		assert(arg != INVALID_VALUE_ID && int(arg) < len(function.values), "false branch argument must reference an existing value")
	}
	terminator_destroy(&block.term)
	block.term = Terminator {
		kind = .Cond_Branch,
		condition = condition,
		true_target = true_target,
		false_target = false_target,
		true_args = make([dynamic]Value_Id, 0, len(true_args), builder.module.allocator),
		false_args = make([dynamic]Value_Id, 0, len(false_args), builder.module.allocator),
		source = source,
	}
	for arg in true_args {
		append(&block.term.true_args, arg)
	}
	for arg in false_args {
		append(&block.term.false_args, arg)
	}
}

builder_set_return :: proc(builder: ^Builder, values: []Value_Id = nil, source: Source_Loc = {}) {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.term.kind == .Invalid, "cannot replace block terminator")
	for value in values {
		assert(value != INVALID_VALUE_ID && int(value) < len(function.values), "return value must reference an existing value")
	}
	terminator_destroy(&block.term)
	block.term = Terminator {
		kind = .Return,
		values = make([dynamic]Value_Id, 0, len(values), builder.module.allocator),
		source = source,
	}
	for value in values {
		append(&block.term.values, value)
	}
}

builder_set_return_world :: proc(builder: ^Builder, source: Source_Loc = {}) {
	values := [?]Value_Id{builder.current_world}
	builder_set_return(builder, values[:], source)
}

builder_set_unreachable :: proc(builder: ^Builder, source: Source_Loc = {}) {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.term.kind == .Invalid, "cannot replace block terminator")
	terminator_destroy(&block.term)
	block.term = Terminator{kind = .Unreachable, source = source}
}
