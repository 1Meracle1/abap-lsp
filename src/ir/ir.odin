package abap_frontend_ir

import "src:ast"
import semantic "src:semantic"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Module_Id :: distinct u32
Function_Id :: distinct u32
Block_Id :: distinct u32
Instruction_Id :: distinct u32
Value_Id :: distinct u32
Type_Id :: distinct u32
Constant_Id :: distinct u32
Slot_Id :: distinct u32
Projection_Id :: distinct u32
Global_Id :: distinct u32
Intrinsic_Id :: distinct u32
Metadata_Id :: distinct u32
Effect_Scope_Id :: distinct u32
Alias_Class_Id :: distinct u32
Use_Id :: distinct u32

Op_Id :: Instruction_Id

INVALID_FUNCTION_ID :: Function_Id(0xffffffff)
INVALID_BLOCK_ID :: Block_Id(0xffffffff)
INVALID_INSTRUCTION_ID :: Instruction_Id(0xffffffff)
INVALID_VALUE_ID :: Value_Id(0xffffffff)
INVALID_TYPE_ID :: Type_Id(0xffffffff)
INVALID_CONSTANT_ID :: Constant_Id(0xffffffff)
INVALID_SLOT_ID :: Slot_Id(0xffffffff)
INVALID_PROJECTION_ID :: Projection_Id(0xffffffff)
INVALID_MODULE_ID :: Module_Id(0xffffffff)
INVALID_OP_ID :: Op_Id(INVALID_INSTRUCTION_ID)
INVALID_GLOBAL_ID :: Global_Id(0xffffffff)
INVALID_INTRINSIC_ID :: Intrinsic_Id(0xffffffff)
INVALID_METADATA_ID :: Metadata_Id(0xffffffff)
INVALID_EFFECT_SCOPE_ID :: Effect_Scope_Id(0xffffffff)
INVALID_ALIAS_CLASS_ID :: Alias_Class_Id(0xffffffff)
INVALID_USE_ID :: Use_Id(0xffffffff)
INVALID_OPERAND_INDEX :: u32(0xffffffff)

INVALID_FUNCTION :: INVALID_FUNCTION_ID
INVALID_BLOCK :: INVALID_BLOCK_ID
INVALID_INSTRUCTION :: INVALID_INSTRUCTION_ID
INVALID_VALUE :: INVALID_VALUE_ID
INVALID_TYPE :: INVALID_TYPE_ID
INVALID_CONSTANT :: INVALID_CONSTANT_ID
INVALID_GLOBAL :: INVALID_GLOBAL_ID
INVALID_INTRINSIC :: INVALID_INTRINSIC_ID
INVALID_METADATA :: INVALID_METADATA_ID
INVALID_EFFECT_SCOPE :: INVALID_EFFECT_SCOPE_ID
INVALID_ALIAS_CLASS :: INVALID_ALIAS_CLASS_ID
INVALID_USE :: INVALID_USE_ID

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

String_Encoding :: enum {
	Unknown,
	UTF8,
	UTF16,
	ABAP_Internal,
}

Target_Info :: struct {
	pointer_bits:         u32,
	default_integer_bits: u32,
	string_encoding:      String_Encoding,
}

Source_Metadata :: struct {
	path:  string,
	range: tokenizer.Range,
}

Debug_Metadata :: struct {
	name:   string,
	source: Metadata_Id,
}

Semantic_Metadata :: struct {
	object_name: string,
	entity_kind: string,
	type_name:   string,
}

Metadata_Record :: struct {
	source:   Source_Metadata,
	debug:    Debug_Metadata,
	semantic: Semantic_Metadata,
}

Type_Kind :: enum {
	Void,
	Token,
	World,
	Predicate,
	Numeric,
	Integer,
	Decimal,
	Float,
	String,
	Char,
	Numc,
	Bytes,
	Date,
	Time,
	Structure,
	Struct,
	Table,
	Table_Iterator,
	Reference,
	Pointer,
	Object,
	Interface,
	Exception,
	Routine,
	Unknown,
	Semantic,
}

Runtime_Type_Family :: enum {
	Unknown,
	Void,
	World,
	Predicate,
	Numeric,
	Integer,
	Decimal,
	Float,
	Text,
	Bytes,
	Date,
	Time,
	Structure,
	Table,
	Table_Iterator,
	Reference,
	Object,
	Interface,
	Exception,
	Routine,
}

Runtime_Text_Kind :: enum {
	None,
	String,
	Fixed,
	Numeric,
	Date,
	Time,
}

Runtime_Table_Category :: enum {
	Unknown,
	Any,
	Index,
	Standard,
	Sorted,
	Hashed,
	Range,
}

Runtime_Table_Key_Uniqueness :: enum {
	Unknown,
	Unique,
	Non_Unique,
	Empty,
	Default,
}

Runtime_Reference_Kind :: enum {
	Unknown,
	Data,
	Object,
	Class,
	Interface,
	Exception,
}

Runtime_Elementary_Descriptor :: struct {
	bits:                u32,
	signed:              bool,
	length:              int,
	has_length:          bool,
	decimals:            int,
	has_decimals:        bool,
	text_kind:           Runtime_Text_Kind,
	preserves_trailing_blanks: bool,
}

Runtime_Field_Descriptor :: struct {
	name:        string,
	display_name: string,
	type:        Type_Id,
	field_index: i32,
}

Runtime_Table_Key_Component :: struct {
	name:        string,
	display_name: string,
	path:        [dynamic]string,
	type:        Type_Id,
	field_index: i32,
}

Runtime_Table_Key_Descriptor :: struct {
	name:        string,
	display_name: string,
	primary:     bool,
	sorted:      bool,
	hashed:      bool,
	uniqueness:  Runtime_Table_Key_Uniqueness,
	components:  [dynamic]Runtime_Table_Key_Component,
}

Runtime_Structure_Descriptor :: struct {
	fields: [dynamic]Runtime_Field_Descriptor,
}

Runtime_Table_Descriptor :: struct {
	row_type:       Type_Id,
	category:       Runtime_Table_Category,
	primary_key:    Runtime_Table_Key_Descriptor,
	secondary_keys: [dynamic]Runtime_Table_Key_Descriptor,
}

Runtime_Reference_Descriptor :: struct {
	kind:        Runtime_Reference_Kind,
	target_type: Type_Id,
	target_name: string,
}

Runtime_Type_Descriptor :: struct {
	family:       Runtime_Type_Family,
	display_name: string,
	elementary:   Runtime_Elementary_Descriptor,
	structure:    Runtime_Structure_Descriptor,
	table:        Runtime_Table_Descriptor,
	reference:    Runtime_Reference_Descriptor,
}

Type_None_Data :: struct {}
Integer_Type_Data :: struct {
	bits:   u32,
	signed: bool,
}
Aggregate_Field :: struct {
	name: string,
	type: Type_Id,
}
Struct_Type_Data :: struct {
	fields: []Aggregate_Field,
}
Table_Type_Data :: struct {
	row_type: Type_Id,
}
Reference_Type_Data :: struct {
	pointee: Type_Id,
}
Type_Data :: union {
	Type_None_Data,
	Integer_Type_Data,
	Struct_Type_Data,
	Table_Type_Data,
	Reference_Type_Data,
}

Type :: struct {
	id:            Type_Id,
	kind:          Type_Kind,
	name:          string,
	data:          Type_Data,
	runtime:       Runtime_Type_Descriptor,
	runtime_owned: bool,
	semantic_meta: Metadata_Id,
	semantic_type: ^semantic.Type,
}

Function_Role :: enum {
	Unknown,
	Internal,
	Report_Entry,
	Report_Start,
	Load_Of_Program,
	Event,
	Report_Event,
	Form,
	Function_Module,
	Module,
	Method,
	Constructor,
	Class_Constructor,
	Test_Entry,
}

Linkage :: enum {
	Private,
	Internal,
	Exported,
	External,
}

Calling_Convention :: enum {
	IR,
	ABAP_Report,
	ABAP_Form,
	ABAP_Method,
	ABAP_Function,
	Host_ABI,
}

Function_Signature :: struct {
	params:             [dynamic]Type_Id,
	results:            [dynamic]Type_Id,
	calling_convention: Calling_Convention,
	effects:            Effect_Set,
	can_throw:          bool,
	can_trap:           bool,
}

Effect_Kind :: enum {
	Read_Local,
	Write_Local,
	Read_Global,
	Write_Global,
	Read_System,
	Write_System,
	Read_Table,
	Write_Table,
	SQL,
	IO,
	May_Trap,
	May_Throw,
	Calls_IR,
	Calls_Host,
	Unsupported,
}
Effect_Set :: bit_set[Effect_Kind]

Slot_Kind :: enum {
	Local,
	Parameter,
	Global,
	Runtime,
	Instance,
	Field,
	Table_Handle,
	Temporary,
}

Slot :: struct {
	kind:            Slot_Kind,
	name:            string,
	type:            Type_Id,
	is_field_symbol: bool,
	entity:          ^semantic.Entity,
	source:          Source_Loc,
}

Constant :: struct {
	literal: string,
	type:    Type_Id,
	source:  Metadata_Id,
}

Global :: struct {
	name:     string,
	type:     Type_Id,
	source:   Metadata_Id,
	semantic: Metadata_Id,
}

Projection_Segment_Kind :: enum {
	Field,
}

Projection_Segment :: struct {
	kind:        Projection_Segment_Kind,
	name:        string,
	selector:    ast.Selector_Op,
	field_index: i32,
	entity:      ^semantic.Entity,
	source:      Source_Loc,
}

Projection_Path :: struct {
	segments: [dynamic]Projection_Segment,
}

Value_Kind :: enum {
	Block_Param,
	Op_Result,
	Constant,
	Global,
	Function,
}

Block_Arg_Def :: struct {
	block: Block_Id,
	index: u32,
}

Instruction_Result_Def :: struct {
	instruction: Instruction_Id,
	index:       u32,
}

Value_Def :: union {
	Block_Arg_Def,
	Instruction_Result_Def,
	Constant_Id,
	Global_Id,
	Function_Id,
}

Value :: struct {
	id:           Value_Id,
	kind:         Value_Kind,
	type:         Type_Id,
	def:          Value_Def,
	first_use:    Use_Id,
	use_count:    u32,
	block:        Block_Id,
	op:           Op_Id,
	result_index: u32,
	name:         string,
	debug_name:   string,
	debug:        Metadata_Id,
}

Use :: struct {
	id:             Use_Id,
	value:          Value_Id,
	user:           Instruction_Id,
	operand_index:  u32,
	prev_for_value: Use_Id,
	next_for_value: Use_Id,
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

Abap_Translate_Mode :: enum {
	Unknown,
	To_Upper,
	To_Lower,
}

Abap_Replace_Occurrence :: enum {
	Unknown,
	First,
	All,
}

Abap_Shift_Direction :: enum {
	Unknown,
	Left,
	Right,
}

Abap_Find_Occurrence :: enum {
	Unknown,
	First,
	All,
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

Intrinsic_Family :: enum {
	Unknown,
	ABAP,
	Call,
	Table,
	SQL,
	System_Field,
	Host,
	Unsupported,
}

Intrinsic_Op :: enum {
	Unknown,
	ABAP_Move,
	ABAP_Add,
	ABAP_Subtract,
	ABAP_Multiply,
	ABAP_Divide,
	ABAP_Integer_Divide,
	ABAP_Modulo,
	ABAP_Equal,
	ABAP_Not_Equal,
	ABAP_Less,
	ABAP_Less_Equal,
	ABAP_Greater,
	ABAP_Greater_Equal,
	ABAP_And,
	ABAP_Or,
	ABAP_Not,
	ABAP_Is_Initial,
	ABAP_String_Concat,
	ABAP_String_Template,
	ABAP_Concatenate,
	ABAP_Condense,
	ABAP_Translate,
	ABAP_Split,
	ABAP_Replace,
	ABAP_Shift,
	ABAP_Find,
	ABAP_Search,
	ABAP_Construct,
	ABAP_Exception_Raise,
	ABAP_Exception_Match,
	ABAP_Exception_Catch,
	ABAP_Exception_Unhandled,
	ABAP_Message,
	ABAP_Write,
	ABAP_Clear,
	ABAP_Refresh,
	ABAP_Free,
	ABAP_Assign_Field,
	ABAP_Unassign,
	Call_Builtin,
	Call_Routine,
	Call_Method,
	Table_Iter,
	Table_Next,
	Table_Read,
	Table_Append,
	Table_Insert,
	Table_Modify,
	Table_Delete,
	Table_Sort,
	SQL_Select,
	SQL_Open_Cursor,
	SQL_Fetch,
	SQL_Close_Cursor,
	SQL_Insert,
	SQL_Update,
	SQL_Delete,
	SQL_Modify,
	System_Read,
	System_Write,
	Host_Call,
	Unsupported,
}

Intrinsic_Call_Payload :: struct {
	callee_name:              string,
	call_kind:                Abap_Call_Kind,
	call_function_target:     Function_Id,
	has_call_function_target: bool,
}

Intrinsic_Message_Payload :: struct {
	form:                 Abap_Message_Form,
	id:                   string,
	msg_type:             string,
	number:               string,
	display_like:         string,
	raising:              string,
	head_operands:        int,
	arg_count:            int,
	has_into:             bool,
	has_display_like:     bool,
	has_raising:          bool,
	display_like_operand: bool,
	raising_operand:      bool,
}

Intrinsic_Exception_Payload :: struct {
	exception_name: string,
}

Intrinsic_String_Payload :: struct {
	has_separator:      bool,
	respecting_blanks:  bool,
	no_gaps:            bool,
	translate_mode:     Abap_Translate_Mode,
	replace_occurrence: Abap_Replace_Occurrence,
	shift_direction:    Abap_Shift_Direction,
	find_occurrence:    Abap_Find_Occurrence,
	find_ignoring_case: bool,
}

Intrinsic_Table_Component :: struct {
	path:        [dynamic]string,
	value_index: int,
}

Intrinsic_Table_Sort_Component :: struct {
	path:       [dynamic]string,
	descending: bool,
}

Intrinsic_Table_Payload :: struct {
	access:            Table_Access_Kind,
	key_kind:          Table_Key_Kind,
	result_kind:       Table_Result_Kind,
	source_kind:       Table_Source_Kind,
	key_name:          string,
	row_type:          Type_Id,
	component_count:   int,
	components:        [dynamic]Intrinsic_Table_Component,
	sort_components:   [dynamic]Intrinsic_Table_Sort_Component,
	binary_search:     bool,
	stable:            bool,
	descending:        bool,
	dynamic_key:       bool,
	dynamic_component: bool,
}

Intrinsic_SQL_Payload :: struct {
	source_kind:      Sql_Source_Kind,
	result_kind:      Sql_Result_Kind,
	source_name:      string,
	source_alias:     string,
	row_type:         Type_Id,
	scalar_type:      Type_Id,
	source_count:     int,
	projection_count: int,
	assignment_count: int,
	single:           bool,
	is_distinct:      bool,
	for_all_entries:  bool,
	from_table:       bool,
}

Intrinsic_System_Field_Payload :: struct {
	system_field: string,
}

Intrinsic_Host_Payload :: struct {
	symbol_name: string,
	abi_name:    string,
}

Intrinsic_None_Payload :: struct {}

Intrinsic_Unsupported_Payload :: struct {
	message: string,
}

Intrinsic_Payload :: union {
	Intrinsic_None_Payload,
	Intrinsic_Call_Payload,
	Intrinsic_Message_Payload,
	Intrinsic_Exception_Payload,
	Intrinsic_String_Payload,
	Intrinsic_Table_Payload,
	Intrinsic_SQL_Payload,
	Intrinsic_System_Field_Payload,
	Intrinsic_Host_Payload,
	Intrinsic_Unsupported_Payload,
}

Intrinsic_Signature :: struct {
	params:    [dynamic]Type_Id,
	results:   [dynamic]Type_Id,
	effects:   Effect_Set,
	can_throw: bool,
	can_trap:  bool,
}

Intrinsic :: struct {
	family:    Intrinsic_Family,
	op:        Intrinsic_Op,
	name:      string,
	payload:   Intrinsic_Payload,
	effects:   Effect_Set,
	signature: Intrinsic_Signature,
	source:    Metadata_Id,
	data:      rawptr,
}

Slot_Address_Attrs :: struct {
	slot: Slot_Id,
}

Table_Component :: Intrinsic_Table_Component
Table_Sort_Component :: Intrinsic_Table_Sort_Component
Table_Payload :: Intrinsic_Table_Payload
SQL_Payload :: Intrinsic_SQL_Payload
Call_Payload :: Intrinsic_Call_Payload
Message_Payload :: Intrinsic_Message_Payload
Exception_Payload :: Intrinsic_Exception_Payload
String_Payload :: Intrinsic_String_Payload
System_Field_Payload :: Intrinsic_System_Field_Payload
Unsupported_Payload :: Intrinsic_Unsupported_Payload

Effect_Scope :: struct {
	name: string,
	type: Type_Id,
}

Opcode :: enum {
	Const,
	Initial,
	Null_Ref,
	Global_Addr,
	Function_Addr,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Neg,
	And,
	Or,
	Xor,
	Not,
	Cmp,
	Select,
	Cast,
	Int_Extend,
	Int_Truncate,
	Ref_Cast,
	Addr_Cast,
	Alloca,
	Addr_Of,
	Deref,
	Field_Addr,
	Index_Addr,
	Table_Row_Addr,
	Load,
	Store,
	Struct_Init,
	Extract_Value,
	Insert_Value,
	Call,
	Invoke,
	Intrinsic,
	Br,
	Cond_Br,
	Switch,
	Return,
	Unreachable,
	Trap,
	Debug_Value,
	Unsupported,
}

Compare_Predicate :: enum {
	EQ,
	NE,
	LT,
	LE,
	GT,
	GE,
}

Compare_Mode :: enum {
	Predicate,
	Signed_Integer,
	Unsigned_Integer,
	Decimal,
	String,
	ABAP_Generic,
}

Compare_Attrs :: struct {
	predicate: Compare_Predicate,
	mode:      Compare_Mode,
}

Cast_Attrs :: struct {
	checked: bool,
}

Call_Attrs :: struct {
	target: Function_Id,
}

Intrinsic_Call_Attrs :: struct {
	intrinsic: Intrinsic_Id,
}

Trap_Attrs :: struct {
	message: string,
}

Unsupported_Attrs :: struct {
	message: string,
}

Instruction_None_Attrs :: struct {}

Instruction_Attrs :: union {
	Instruction_None_Attrs,
	Constant_Id,
	Slot_Address_Attrs,
	Compare_Attrs,
	Cast_Attrs,
	Projection_Id,
	Call_Attrs,
	Intrinsic_Call_Attrs,
	Trap_Attrs,
	Unsupported_Attrs,
}

Memory_Access_Kind :: enum {
	Read,
	Write,
	Read_Write,
	Allocate,
	Free,
}

Memory_Access :: struct {
	kind:            Memory_Access_Kind,
	type:            Type_Id,
	alias_class:     Alias_Class_Id,
	scope:           Effect_Scope_Id,
	address_operand: u32,
	value_operand:   u32,
	source:          Metadata_Id,
}

Alias_Class :: struct {
	name:     string,
	parent:   Alias_Class_Id,
	disjoint: [dynamic]Alias_Class_Id,
	source:   Metadata_Id,
}

Successor_Edge :: struct {
	target:        Block_Id,
	args:          [dynamic]Value_Id,
	case_value:    Value_Id,
	operand_start: u32,
	operand_count: u32,
	kind:          Edge_Kind,
	source:        Metadata_Id,
}

Switch_Case :: struct {
	value:  Value_Id,
	target: Block_Id,
	args:   []Value_Id,
}

Edge_Kind :: enum {
	Normal,
	True,
	False,
	Switch_Case,
	Exception,
	Cleanup,
}

Instruction :: struct {
	id:           Instruction_Id,
	parent:       Block_Id,
	opcode:       Opcode,
	operands:     [dynamic]Value_Id,
	operand_uses: [dynamic]Use_Id,
	results:      [dynamic]Value_Id,
	successors:   [dynamic]Successor_Edge,
	attrs:        Instruction_Attrs,
	effects:      Effect_Set,
	memory:       [dynamic]Memory_Access,
	intrinsic:    Intrinsic_Id,
	source:       Source_Loc,
	debug:        Metadata_Id,
	semantic:     Metadata_Id,
}

Op :: Instruction

Basic_Block :: struct {
	id:           Block_Id,
	name:         string,
	args:         [dynamic]Value_Id,
	instructions: [dynamic]Instruction_Id,
	terminator:   Instruction_Id,
	source:       Source_Loc,
	debug:        Metadata_Id,
}

Block :: Basic_Block

Op_Location :: struct {
	block: Block_Id,
	index: u32,
}

Function :: struct {
	id:                  Function_Id,
	name:                string,
	linkage:             Linkage,
	role:                Function_Role,
	signature:           Function_Signature,
	entity:              ^semantic.Entity,
	source:              Source_Loc,
	debug:               Metadata_Id,
	source_metadata:     Metadata_Id,
	semantic:            Metadata_Id,
	return_types:        [dynamic]Type_Id,
	slots:               [dynamic]Slot,
	projections:         [dynamic]Projection_Path,
	instructions:        [dynamic]Instruction,
	values:              [dynamic]Value,
	uses:                [dynamic]Use,
	blocks:              [dynamic]Block,
	block_order:         [dynamic]Block_Id,
	op_locations:        [dynamic]Op_Location,
	entry:               Block_Id,
	world_param:         Value_Id,
	next_instruction_id: u32,
	analysis_generation: u64,
	mutation_generation: u64,
}

Module :: struct {
	allocator:      mem.Allocator,
	source_name:    string,
	target:         Target_Info,
	types:          [dynamic]Type,
	constants:      [dynamic]Constant,
	globals:        [dynamic]Global,
	intrinsics:     [dynamic]Intrinsic,
	metadata:       [dynamic]Metadata_Record,
	effect_scopes:  [dynamic]Effect_Scope,
	alias_classes:  [dynamic]Alias_Class,
	functions:      [dynamic]Function,
	entries:        [dynamic]Function_Id,
}

module_make :: proc(allocator: mem.Allocator = context.allocator) -> Module {
	module: Module
	module_init(&module, allocator)
	return module
}

module_init :: proc(module: ^Module, allocator: mem.Allocator = context.allocator) {
	module^ = {}
	module.allocator = allocator
	module.target = Target_Info {
		pointer_bits = 64,
		default_integer_bits = 32,
		string_encoding = .ABAP_Internal,
	}
	module.types = make([dynamic]Type, 0, 16, allocator)
	module.constants = make([dynamic]Constant, 0, 16, allocator)
	module.globals = make([dynamic]Global, 0, 8, allocator)
	module.intrinsics = make([dynamic]Intrinsic, 0, 16, allocator)
	module.metadata = make([dynamic]Metadata_Record, 0, 16, allocator)
	module.effect_scopes = make([dynamic]Effect_Scope, 0, 8, allocator)
	module.alias_classes = make([dynamic]Alias_Class, 0, 8, allocator)
	module.functions = make([dynamic]Function, 0, 8, allocator)
	module.entries = make([dynamic]Function_Id, 0, 2, allocator)
	append(&module.types, module_builtin_type(module, BUILTIN_TYPE_VOID, .Void, "void", .Void))
	append(&module.types, module_builtin_type(module, BUILTIN_TYPE_WORLD, .World, "world", .World))
	append(&module.types, module_builtin_type(module, BUILTIN_TYPE_PREDICATE, .Predicate, "predicate", .Predicate))
	append(
		&module.types,
		module_builtin_type(
			module,
			BUILTIN_TYPE_INTEGER,
			.Integer,
			"i",
			.Integer,
			Runtime_Elementary_Descriptor{bits = module.target.default_integer_bits, signed = true},
		),
	)
	append(
		&module.types,
		module_builtin_type(
			module,
			BUILTIN_TYPE_STRING,
			.String,
			"string",
			.Text,
			Runtime_Elementary_Descriptor{text_kind = .String, preserves_trailing_blanks = true},
		),
	)
	append(&module.types, module_builtin_type(module, BUILTIN_TYPE_UNKNOWN, .Unknown, "unknown", .Unknown))
	append(&module.types, module_builtin_type(module, BUILTIN_TYPE_TABLE_ITERATOR, .Table_Iterator, "table_iter", .Table_Iterator))
}

module_builtin_type :: proc(
	module: ^Module,
	id: Type_Id,
	kind: Type_Kind,
	name: string,
	family: Runtime_Type_Family,
	elementary: Runtime_Elementary_Descriptor = {},
) -> Type {
	return Type {
		id = id,
		kind = kind,
		name = strings.clone(name, module.allocator),
		runtime = Runtime_Type_Descriptor {
			family = family,
			display_name = strings.clone(name, module.allocator),
			elementary = elementary,
		},
		runtime_owned = true,
	}
}

type_complete_runtime_descriptor :: proc(module: ^Module, typ: ^Type) {
	if typ.runtime.family == .Unknown {
		if typ.runtime_owned {
			runtime_type_descriptor_destroy(&typ.runtime, module.allocator)
		}
		typ.runtime = runtime_descriptor_from_type_record(module, typ^)
		typ.runtime_owned = true
	} else if typ.runtime.display_name == "" {
		name := typ.name
		if name == "" {
			name = runtime_type_family_default_name(typ.runtime.family)
		}
		typ.runtime.display_name = strings.clone(name, module.allocator)
		typ.runtime_owned = true
	}
	if typ.runtime.display_name == "" {
		typ.runtime.display_name = strings.clone("unknown", module.allocator)
		typ.runtime_owned = true
	}
}

runtime_descriptor_from_type_record :: proc(module: ^Module, typ: Type) -> Runtime_Type_Descriptor {
	display := typ.name
	if display == "" {
		display = type_kind_display_name(typ.kind)
	}
	descriptor := Runtime_Type_Descriptor {
		family = runtime_type_family_from_kind(typ.kind),
		display_name = strings.clone(display, module.allocator),
	}
	#partial switch typ.kind {
	case .Integer:
		descriptor.elementary = Runtime_Elementary_Descriptor {
			bits = module.target.default_integer_bits,
			signed = true,
		}
	case .String:
		descriptor.elementary = Runtime_Elementary_Descriptor {
			text_kind = .String,
			preserves_trailing_blanks = true,
		}
	case .Char:
		descriptor.elementary = Runtime_Elementary_Descriptor {
			text_kind = .Fixed,
			preserves_trailing_blanks = false,
		}
	case .Numc:
		descriptor.elementary = Runtime_Elementary_Descriptor {
			text_kind = .Numeric,
			preserves_trailing_blanks = false,
		}
	case .Date:
		descriptor.elementary = Runtime_Elementary_Descriptor {
			text_kind = .Date,
			length = 8,
			has_length = true,
			preserves_trailing_blanks = false,
		}
	case .Time:
		descriptor.elementary = Runtime_Elementary_Descriptor {
			text_kind = .Time,
			length = 6,
			has_length = true,
			preserves_trailing_blanks = false,
		}
	case .Bytes:
		descriptor.elementary = Runtime_Elementary_Descriptor {
			preserves_trailing_blanks = true,
		}
	}
	#partial switch data in typ.data {
	case Integer_Type_Data:
		descriptor.family = .Integer
		descriptor.elementary.bits = data.bits
		descriptor.elementary.signed = data.signed
	case Struct_Type_Data:
		descriptor.family = .Structure
		descriptor.structure.fields = make([dynamic]Runtime_Field_Descriptor, 0, len(data.fields), module.allocator)
		for field, i in data.fields {
			append(
				&descriptor.structure.fields,
				Runtime_Field_Descriptor {
					name = strings.clone(field.name, module.allocator),
					display_name = strings.clone(field.name, module.allocator),
					type = field.type,
					field_index = i32(i),
				},
			)
		}
	case Table_Type_Data:
		descriptor.family = .Table
		descriptor.table.row_type = data.row_type
		descriptor.table.category = .Standard
	case Reference_Type_Data:
		descriptor.family = .Reference
		descriptor.reference.kind = .Data
		descriptor.reference.target_type = data.pointee
		if data.pointee != INVALID_TYPE_ID && int(data.pointee) < len(module.types) {
			descriptor.reference.target_name = strings.clone(module.types[int(data.pointee)].runtime.display_name, module.allocator)
		}
	}
	return descriptor
}

runtime_type_family_from_kind :: proc "contextless" (kind: Type_Kind) -> Runtime_Type_Family {
	#partial switch kind {
	case .Void:
		return .Void
	case .World:
		return .World
	case .Predicate:
		return .Predicate
	case .Numeric:
		return .Numeric
	case .Integer:
		return .Integer
	case .Decimal:
		return .Decimal
	case .Float:
		return .Float
	case .String, .Char, .Numc:
		return .Text
	case .Bytes:
		return .Bytes
	case .Date:
		return .Date
	case .Time:
		return .Time
	case .Structure, .Struct:
		return .Structure
	case .Table:
		return .Table
	case .Table_Iterator:
		return .Table_Iterator
	case .Reference, .Pointer:
		return .Reference
	case .Object:
		return .Object
	case .Interface:
		return .Interface
	case .Exception:
		return .Exception
	case .Routine:
		return .Routine
	}
	return .Unknown
}

runtime_type_family_default_name :: proc "contextless" (family: Runtime_Type_Family) -> string {
	#partial switch family {
	case .Void:
		return "void"
	case .World:
		return "world"
	case .Predicate:
		return "predicate"
	case .Numeric:
		return "numeric"
	case .Integer:
		return "i"
	case .Decimal:
		return "p"
	case .Float:
		return "f"
	case .Text:
		return "text"
	case .Bytes:
		return "bytes"
	case .Date:
		return "d"
	case .Time:
		return "t"
	case .Structure:
		return "structure"
	case .Table:
		return "table"
	case .Table_Iterator:
		return "table_iter"
	case .Reference:
		return "ref"
	case .Object:
		return "object"
	case .Interface:
		return "interface"
	case .Exception:
		return "exception"
	case .Routine:
		return "routine"
	}
	return "unknown"
}

type_kind_display_name :: proc "contextless" (kind: Type_Kind) -> string {
	#partial switch kind {
	case .Void:
		return "void"
	case .Token:
		return "token"
	case .World:
		return "world"
	case .Predicate:
		return "predicate"
	case .Integer:
		return "i"
	case .Decimal:
		return "p"
	case .Float:
		return "f"
	case .String:
		return "string"
	case .Char:
		return "c"
	case .Numc:
		return "n"
	case .Bytes:
		return "x"
	case .Date:
		return "d"
	case .Time:
		return "t"
	case .Structure, .Struct:
		return "structure"
	case .Table:
		return "table"
	case .Table_Iterator:
		return "table_iter"
	case .Reference, .Pointer:
		return "ref"
	case .Object:
		return "object"
	case .Interface:
		return "interface"
	case .Exception:
		return "exception"
	case .Routine:
		return "routine"
	case .Semantic:
		return "semantic"
	}
	return "unknown"
}

type_destroy_runtime_metadata :: proc(typ: ^Type, allocator: mem.Allocator) {
	if !typ.runtime_owned {
		return
	}
	runtime_type_descriptor_destroy(&typ.runtime, allocator)
	typ.runtime_owned = false
}

type_destroy_owned_data :: proc(typ: ^Type, allocator: mem.Allocator) {
	delete(typ.name, allocator)
	#partial switch data in typ.data {
	case Struct_Type_Data:
		for field in data.fields {
			delete(field.name, allocator)
		}
		delete(data.fields, allocator)
	}
	typ.name = ""
	typ.data = Type_None_Data{}
}

runtime_type_descriptor_destroy :: proc(descriptor: ^Runtime_Type_Descriptor, allocator: mem.Allocator) {
	delete(descriptor.display_name, allocator)
	for &field in descriptor.structure.fields {
		delete(field.name, allocator)
		delete(field.display_name, allocator)
	}
	delete(descriptor.structure.fields)
	runtime_table_key_descriptor_destroy(&descriptor.table.primary_key, allocator)
	for &key in descriptor.table.secondary_keys {
		runtime_table_key_descriptor_destroy(&key, allocator)
	}
	delete(descriptor.table.secondary_keys)
	delete(descriptor.reference.target_name, allocator)
	descriptor^ = {}
}

runtime_type_descriptor_clone :: proc(
	descriptor: Runtime_Type_Descriptor,
	allocator: mem.Allocator,
) -> Runtime_Type_Descriptor {
	out := descriptor
	out.display_name = strings.clone(descriptor.display_name, allocator)
	out.structure.fields = make([dynamic]Runtime_Field_Descriptor, 0, len(descriptor.structure.fields), allocator)
	for field in descriptor.structure.fields {
		copy := field
		copy.name = strings.clone(field.name, allocator)
		copy.display_name = strings.clone(field.display_name, allocator)
		append(&out.structure.fields, copy)
	}
	out.table.primary_key = runtime_table_key_descriptor_clone(descriptor.table.primary_key, allocator)
	out.table.secondary_keys = make(
		[dynamic]Runtime_Table_Key_Descriptor,
		0,
		len(descriptor.table.secondary_keys),
		allocator,
	)
	for key in descriptor.table.secondary_keys {
		append(&out.table.secondary_keys, runtime_table_key_descriptor_clone(key, allocator))
	}
	out.reference.target_name = strings.clone(descriptor.reference.target_name, allocator)
	return out
}

runtime_table_key_descriptor_clone :: proc(
	key: Runtime_Table_Key_Descriptor,
	allocator: mem.Allocator,
) -> Runtime_Table_Key_Descriptor {
	out := key
	out.name = strings.clone(key.name, allocator)
	out.display_name = strings.clone(key.display_name, allocator)
	out.components = make([dynamic]Runtime_Table_Key_Component, 0, len(key.components), allocator)
	for component in key.components {
		copy := component
		copy.name = strings.clone(component.name, allocator)
		copy.display_name = strings.clone(component.display_name, allocator)
		copy.path = make([dynamic]string, 0, len(component.path), allocator)
		for segment in component.path {
			append(&copy.path, strings.clone(segment, allocator))
		}
		append(&out.components, copy)
	}
	return out
}

runtime_table_key_descriptor_destroy :: proc(key: ^Runtime_Table_Key_Descriptor, allocator: mem.Allocator) {
	delete(key.name, allocator)
	delete(key.display_name, allocator)
	for &component in key.components {
		delete(component.name, allocator)
		delete(component.display_name, allocator)
		for segment in component.path {
			delete(segment, allocator)
		}
		delete(component.path)
	}
	delete(key.components)
	key^ = {}
}

module_destroy :: proc(module: ^Module) {
	for &function in module.functions {
		function_destroy(&function, module.allocator)
	}
	for &intrinsic in module.intrinsics {
		intrinsic_destroy(&intrinsic, module.allocator)
	}
	for &global in module.globals {
		delete(global.name, module.allocator)
	}
	for &constant in module.constants {
		delete(constant.literal, module.allocator)
	}
	for &typ in module.types {
		type_destroy_owned_data(&typ, module.allocator)
		type_destroy_runtime_metadata(&typ, module.allocator)
	}
	for &alias in module.alias_classes {
		delete(alias.name, module.allocator)
		delete(alias.disjoint)
	}
	for &scope in module.effect_scopes {
		delete(scope.name, module.allocator)
	}
	for &metadata in module.metadata {
		metadata_destroy(&metadata, module.allocator)
	}
	delete(module.source_name, module.allocator)
	delete(module.entries)
	delete(module.functions)
	delete(module.alias_classes)
	delete(module.effect_scopes)
	delete(module.metadata)
	delete(module.intrinsics)
	delete(module.globals)
	delete(module.constants)
	delete(module.types)
	module^ = {}
}

function_destroy :: proc(function: ^Function, allocator: mem.Allocator) {
	function_signature_destroy(&function.signature)
	for &block in function.blocks {
		block_destroy(&block)
	}
	for &instruction in function.instructions {
		op_destroy(&instruction, allocator)
	}
	delete(function.return_types)
	delete(function.slots)
	for &projection in function.projections {
		delete(projection.segments)
	}
	delete(function.projections)
	delete(function.instructions)
	delete(function.values)
	delete(function.uses)
	delete(function.blocks)
	delete(function.block_order)
	delete(function.op_locations)
	function^ = {}
}

function_signature_destroy :: proc(signature: ^Function_Signature) {
	delete(signature.params)
	delete(signature.results)
	signature^ = {}
}

block_destroy :: proc(block: ^Block) {
	delete(block.args)
	delete(block.instructions)
	block^ = {}
}

op_destroy :: proc(op: ^Op, allocator: mem.Allocator) {
	for &edge in op.successors {
		delete(edge.args)
	}
	delete(op.operand_uses)
	delete(op.operands)
	delete(op.results)
	delete(op.successors)
	delete(op.memory)
	op^ = {}
}

metadata_destroy :: proc(metadata: ^Metadata_Record, allocator: mem.Allocator) {
	delete(metadata.source.path, allocator)
	delete(metadata.debug.name, allocator)
	delete(metadata.semantic.object_name, allocator)
	delete(metadata.semantic.entity_kind, allocator)
	delete(metadata.semantic.type_name, allocator)
	metadata^ = {}
}

intrinsic_destroy :: proc(intrinsic: ^Intrinsic, allocator: mem.Allocator) {
	delete(intrinsic.name, allocator)
	intrinsic_payload_destroy(&intrinsic.payload, allocator)
	delete(intrinsic.signature.params)
	delete(intrinsic.signature.results)
	intrinsic^ = {}
}

function_ptr :: #force_inline proc(module: ^Module, id: Function_Id) -> ^Function {
	assert(id != INVALID_FUNCTION_ID && int(id) < len(module.functions))
	return &module.functions[int(id)]
}

block_ptr :: #force_inline proc(function: ^Function, id: Block_Id) -> ^Block {
	assert(id != INVALID_BLOCK_ID && int(id) < len(function.blocks))
	return &function.blocks[int(id)]
}

op_ptr :: proc(function: ^Function, id: Op_Id) -> ^Op {
	assert(id != INVALID_OP_ID && int(id) < len(function.instructions))
	instruction := &function.instructions[int(id)]
	assert(instruction.id == Instruction_Id(id))
	return instruction
}

value_ptr :: #force_inline proc(function: ^Function, id: Value_Id) -> ^Value {
	assert(id != INVALID_VALUE_ID && int(id) < len(function.values))
	return &function.values[int(id)]
}

slot_ptr :: #force_inline proc(function: ^Function, id: Slot_Id) -> ^Slot {
	assert(id != INVALID_SLOT_ID && int(id) < len(function.slots))
	return &function.slots[int(id)]
}

slot_is_field_symbol :: proc "contextless" (slot: Slot) -> bool {
	return slot.is_field_symbol
}

projection_ptr :: #force_inline proc(function: ^Function, id: Projection_Id) -> ^Projection_Path {
	assert(id != INVALID_PROJECTION_ID && int(id) < len(function.projections))
	return &function.projections[int(id)]
}

type_ptr :: #force_inline proc(module: ^Module, id: Type_Id) -> ^Type {
	assert(id != INVALID_TYPE_ID && int(id) < len(module.types))
	return &module.types[int(id)]
}

value_type :: #force_inline proc(function: ^Function, value: Value_Id) -> Type_Id {
	return value_ptr(function, value).type if value != INVALID_VALUE_ID else INVALID_TYPE_ID
}

type_is_world :: #force_inline proc "contextless" (typ: Type_Id) -> bool {
	return typ == BUILTIN_TYPE_WORLD
}

source_loc_from_node :: proc(file: ^semantic.Project_File, node: ^ast.Node) -> Source_Loc {
	return Source_Loc{file = file, node = node, range = node.range}
}
