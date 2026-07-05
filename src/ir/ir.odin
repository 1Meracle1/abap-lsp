package abap_frontend_ir

import "src:ast"
import semantic "src:semantic"
import "src:tokenizer"

import "core:mem"

Function_Id :: distinct u32
Block_Id :: distinct u32
Op_Id :: distinct u32
Value_Id :: distinct u32
Type_Id :: distinct u32
Slot_Id :: distinct u32

INVALID_FUNCTION_ID :: Function_Id(0xffffffff)
INVALID_BLOCK_ID :: Block_Id(0xffffffff)
INVALID_OP_ID :: Op_Id(0xffffffff)
INVALID_VALUE_ID :: Value_Id(0xffffffff)
INVALID_TYPE_ID :: Type_Id(0xffffffff)
INVALID_SLOT_ID :: Slot_Id(0xffffffff)

BUILTIN_TYPE_VOID :: Type_Id(0)
BUILTIN_TYPE_WORLD :: Type_Id(1)
BUILTIN_TYPE_PREDICATE :: Type_Id(2)
BUILTIN_TYPE_INTEGER :: Type_Id(3)
BUILTIN_TYPE_STRING :: Type_Id(4)
BUILTIN_TYPE_UNKNOWN :: Type_Id(5)
BUILTIN_TYPE_TABLE_ITERATOR :: Type_Id(6)

Source_Loc :: struct {
	file:  ^semantic.Project_File,
	node:  ^ast.Node,
	range: tokenizer.Range,
}

Type_Kind :: enum {
	Void,
	World,
	Predicate,
	Integer,
	String,
	Structure,
	Table,
	Table_Iterator,
	Reference,
	Routine,
	Unknown,
	Semantic,
}

Type :: struct {
	kind:          Type_Kind,
	name:          string,
	semantic_type: ^semantic.Type,
}

Function_Role :: enum {
	Unknown,
	Report_Entry,
	Load_Of_Program,
	Event,
	Form,
	Function_Module,
	Module,
	Method,
}

Slot_Kind :: enum {
	Local,
	Parameter,
	Global,
	Instance,
	System_Field,
	Field,
	Table_Handle,
	Temporary,
}

Slot :: struct {
	kind:   Slot_Kind,
	name:   string,
	type:   Type_Id,
	entity: ^semantic.Entity,
	source: Source_Loc,
}

Value_Kind :: enum {
	Block_Param,
	Op_Result,
}

Value :: struct {
	kind:         Value_Kind,
	type:         Type_Id,
	block:        Block_Id,
	op:           Op_Id,
	result_index: u32,
	name:         string,
}

Block_Param :: struct {
	value: Value_Id,
	name:  string,
}

Op_Kind :: enum {
	Core_Const,
	Core_Load,
	Core_Store,
	Core_Field_Load,
	Core_Field_Store,
	Core_Cast,
	Core_Call,
	Core_Unsupported,

	Abap_Move,
	Abap_Add,
	Abap_Subtract,
	Abap_Multiply,
	Abap_Divide,
	Abap_Equal,
	Abap_Not_Equal,
	Abap_Less,
	Abap_Less_Equal,
	Abap_Greater,
	Abap_Greater_Equal,
	Abap_And,
	Abap_Or,
	Abap_Not,
	Abap_Is_Initial,
	Abap_String_Concat,
	Abap_String_Template,
	Abap_Construct,
	Abap_Builtin_Call,
	Abap_Routine_Call,
	Abap_Method_Call,
	Abap_Message,
	Abap_Write,
	Abap_Clear,
	Abap_Refresh,
	Abap_Free,
	Abap_Assign_Field,
	Abap_Unassign,

	Table_Iter,
	Table_Next,
	Table_Read,
	Table_Append,
	Table_Insert,
	Table_Modify,
	Table_Delete,
	Table_Sort,
	Table_Length,

	Sql_Select,
	Sql_Open_Cursor,
	Sql_Fetch,
	Sql_Close_Cursor,
	Sql_Insert,
	Sql_Update,
	Sql_Delete,
	Sql_Modify,

	System_Read,
	System_Write,
}

Abap_Call_Kind :: enum {
	Unknown,
	Builtin,
	Form,
	Function_Module,
	Method,
	Module,
	Routine,
}

Abap_Message_Form :: enum {
	Unknown,
	Default,
	Compact,
	Explicit,
}

Table_Access_Kind :: enum {
	Unknown,
	Sequential,
	Index,
	Key,
	Table_Key,
	Where,
	Full,
	Sort,
}

Table_Key_Kind :: enum {
	None,
	Primary,
	Named,
	Dynamic,
	Free,
	Table,
}

Table_Result_Kind :: enum {
	None,
	Value,
	Into,
	Assigning,
	Reference_Into,
	No_Fields,
}

Table_Source_Kind :: enum {
	Unknown,
	Row,
	Lines_Of,
	Initial_Line,
	From_Table,
}

Sql_Source_Kind :: enum {
	Unknown,
	Resolved,
	Internal,
	Dynamic,
	Unresolved,
}

Sql_Result_Kind :: enum {
	None,
	Into,
	Into_Table,
	Appending,
	Appending_Table,
}

Op_Flag :: enum {
	Reads_World,
	Writes_World,
	May_Trap,
	Unsupported,
}
Op_Flags :: bit_set[Op_Flag]

Op_Payload :: struct {
	slot:                Slot_Id,
	field_name:          string,
	literal:             string,
	callee_name:         string,
	call_kind:           Abap_Call_Kind,
	call_target:         ^semantic.Entity,
	call_function_target: Function_Id,
	has_call_function_target: bool,
	message_form:         Abap_Message_Form,
	message_id:           string,
	message_type:         string,
	message_number:       string,
	message_display_like: string,
	message_raising:      string,
	message_head_operands: int,
	message_arg_count:     int,
	message_has_into:         bool,
	message_has_display_like: bool,
	message_has_raising:      bool,
	message_display_like_operand: bool,
	message_raising_operand:      bool,
	table_access:        Table_Access_Kind,
	table_key_kind:      Table_Key_Kind,
	table_result_kind:   Table_Result_Kind,
	table_source_kind:   Table_Source_Kind,
	table_key_name:        string,
	table_row_type:        Type_Id,
	table_component_count: int,
	table_binary_search:   bool,
	table_stable:          bool,
	sql_source_kind:       Sql_Source_Kind,
	sql_result_kind:       Sql_Result_Kind,
	sql_source_name:       string,
	sql_source_alias:      string,
	sql_source_entity:     ^semantic.Entity,
	sql_row_type:          Type_Id,
	sql_scalar_type:       Type_Id,
	sql_source_count:      int,
	sql_projection_count:  int,
	sql_assignment_count:  int,
	sql_single:            bool,
	sql_distinct:          bool,
	sql_for_all_entries:   bool,
	sql_from_table:        bool,
	system_field:        string,
	unsupported_message: string,
	sql_query:           ^ast.Select_Query_Clause,
	ast_stmt:            ^ast.Stmt,
}

Op :: struct {
	id:       Op_Id,
	kind:     Op_Kind,
	operands: [dynamic]Value_Id,
	results:  [dynamic]Value_Id,
	type:     Type_Id,
	flags:    Op_Flags,
	source:   Source_Loc,
	payload:  Op_Payload,
}

Terminator_Kind :: enum {
	Invalid,
	Branch,
	Cond_Branch,
	Return,
	Unreachable,
}

Terminator :: struct {
	kind:         Terminator_Kind,
	condition:    Value_Id,
	target:       Block_Id,
	target_args:  [dynamic]Value_Id,
	true_target:  Block_Id,
	true_args:    [dynamic]Value_Id,
	false_target: Block_Id,
	false_args:   [dynamic]Value_Id,
	values:       [dynamic]Value_Id,
	source:       Source_Loc,
}

Block :: struct {
	name:   string,
	params: [dynamic]Block_Param,
	ops:    [dynamic]Op,
	term:   Terminator,
	source: Source_Loc,
}

Op_Location :: struct {
	block: Block_Id,
	index: u32,
}

Function :: struct {
	name:         string,
	role:         Function_Role,
	entity:       ^semantic.Entity,
	source:       Source_Loc,
	return_types: [dynamic]Type_Id,
	slots:        [dynamic]Slot,
	values:       [dynamic]Value,
	blocks:       [dynamic]Block,
	op_locations: [dynamic]Op_Location,
	entry:        Block_Id,
	world_param:  Value_Id,
}

Module :: struct {
	allocator: mem.Allocator,
	types:     [dynamic]Type,
	functions: [dynamic]Function,
	entries:   [dynamic]Function_Id,
}

module_make :: proc(allocator: mem.Allocator = context.allocator) -> Module {
	module: Module
	module_init(&module, allocator)
	return module
}

module_init :: proc(module: ^Module, allocator: mem.Allocator = context.allocator) {
	module^ = {}
	module.allocator = allocator
	module.types = make([dynamic]Type, 0, 16, allocator)
	module.functions = make([dynamic]Function, 0, 8, allocator)
	module.entries = make([dynamic]Function_Id, 0, 2, allocator)
	append(&module.types, Type{kind = .Void, name = "void"})
	append(&module.types, Type{kind = .World, name = "world"})
	append(&module.types, Type{kind = .Predicate, name = "predicate"})
	append(&module.types, Type{kind = .Integer, name = "i"})
	append(&module.types, Type{kind = .String, name = "string"})
	append(&module.types, Type{kind = .Unknown, name = "unknown"})
	append(&module.types, Type{kind = .Table_Iterator, name = "table_iter"})
}

module_destroy :: proc(module: ^Module) {
	assert(module != nil)
	for &function in module.functions {
		function_destroy(&function)
	}
	delete(module.entries)
	delete(module.functions)
	delete(module.types)
	module^ = {}
}

function_destroy :: proc(function: ^Function) {
	assert(function != nil)
	for &block in function.blocks {
		block_destroy(&block)
	}
	delete(function.return_types)
	delete(function.slots)
	delete(function.values)
	delete(function.blocks)
	delete(function.op_locations)
	function^ = {}
}

block_destroy :: proc(block: ^Block) {
	assert(block != nil)
	for &op in block.ops {
		op_destroy(&op)
	}
	terminator_destroy(&block.term)
	delete(block.params)
	delete(block.ops)
	block^ = {}
}

op_destroy :: proc(op: ^Op) {
	assert(op != nil)
	delete(op.operands)
	delete(op.results)
	op^ = {}
}

terminator_destroy :: proc(term: ^Terminator) {
	assert(term != nil)
	delete(term.target_args)
	delete(term.true_args)
	delete(term.false_args)
	delete(term.values)
	term^ = {}
}

function_ptr :: #force_inline proc(module: ^Module, id: Function_Id) -> ^Function {
	assert(module != nil && id != INVALID_FUNCTION_ID && int(id) < len(module.functions))
	return &module.functions[int(id)]
}

block_ptr :: #force_inline proc(function: ^Function, id: Block_Id) -> ^Block {
	assert(function != nil && id != INVALID_BLOCK_ID && int(id) < len(function.blocks))
	return &function.blocks[int(id)]
}

op_ptr :: proc(function: ^Function, id: Op_Id) -> ^Op {
	assert(function != nil && id != INVALID_OP_ID && int(id) < len(function.op_locations))
	loc := function.op_locations[int(id)]
	block := block_ptr(function, loc.block)
	assert(int(loc.index) < len(block.ops))
	return &block.ops[int(loc.index)]
}

value_ptr :: #force_inline proc(function: ^Function, id: Value_Id) -> ^Value {
	assert(function != nil && id != INVALID_VALUE_ID && int(id) < len(function.values))
	return &function.values[int(id)]
}

slot_ptr :: #force_inline proc(function: ^Function, id: Slot_Id) -> ^Slot {
	assert(function != nil && id != INVALID_SLOT_ID && int(id) < len(function.slots))
	return &function.slots[int(id)]
}

type_ptr :: #force_inline proc(module: ^Module, id: Type_Id) -> ^Type {
	assert(module != nil && id != INVALID_TYPE_ID && int(id) < len(module.types))
	return &module.types[int(id)]
}

value_type :: #force_inline proc(function: ^Function, value: Value_Id) -> Type_Id {
	return value_ptr(function, value).type if value != INVALID_VALUE_ID else INVALID_TYPE_ID
}

type_is_world :: #force_inline proc "contextless" (typ: Type_Id) -> bool {
	return typ == BUILTIN_TYPE_WORLD
}

source_loc_from_node :: proc(file: ^semantic.Project_File, node: ^ast.Node) -> Source_Loc {
	assert(node != nil)
	return Source_Loc {
		file = file,
		node = node,
		range = node.range,
	}
}
