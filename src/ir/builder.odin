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
	stored := typ
	stored.name = strings.clone(typ.name, module.allocator)
	#partial switch data in typ.data {
	case Struct_Type_Data:
		fields := make([]Aggregate_Field, len(data.fields), module.allocator)
		for field, i in data.fields {
			fields[i] = Aggregate_Field {
				name = strings.clone(field.name, module.allocator),
				type = field.type,
			}
		}
		stored.data = Struct_Type_Data{fields = fields}
	}
	type_complete_runtime_descriptor(module, &stored)
	if typ.semantic_type != nil {
		for existing, i in module.types {
			if existing.semantic_type == typ.semantic_type {
				type_destroy_owned_data(&stored, module.allocator)
				type_destroy_runtime_metadata(&stored, module.allocator)
				return Type_Id(i)
			}
		}
	}
	if typ.name != "" {
		for existing, i in module.types {
			if existing.kind == typ.kind && existing.name == typ.name && existing.semantic_type == typ.semantic_type {
				type_destroy_owned_data(&stored, module.allocator)
				type_destroy_runtime_metadata(&stored, module.allocator)
				return Type_Id(i)
			}
		}
	}
	id := Type_Id(len(module.types))
	stored.id = id
	append(&module.types, stored)
	return id
}

module_type_from_semantic :: proc(module: ^Module, typ: ^semantic.Type) -> Type_Id {
	if typ == nil {
		return BUILTIN_TYPE_UNKNOWN
	}
	if builtin, ok := module_builtin_type_id_from_semantic(typ); ok {
		return builtin
	}
	for existing, i in module.types {
		if existing.semantic_type == typ {
			return Type_Id(i)
		}
	}
	kind := module_type_kind_from_semantic(typ)
	name, name_owned := module_semantic_type_name(module, typ)
	if name_owned {
		defer delete(name, context.temp_allocator)
	}
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
	return module_add_type(
		module,
		Type {
			kind = kind,
			name = name,
			runtime = module_runtime_descriptor_from_semantic(module, typ, name),
			runtime_owned = true,
			semantic_type = typ,
		},
	)
}

module_builtin_type_id_from_semantic :: proc "contextless" (typ: ^semantic.Type, depth := 0) -> (Type_Id, bool) {
	if typ == nil || depth > 16 {
		return INVALID_TYPE_ID, false
	}
	#partial switch typ.kind {
	case .Builtin:
		if typ.name == "i" {
			return BUILTIN_TYPE_INTEGER, true
		}
		if typ.name == "string" {
			return BUILTIN_TYPE_STRING, true
		}
	case .Named:
		return module_builtin_type_id_from_semantic(typ.base, depth + 1)
	}
	return INVALID_TYPE_ID, false
}

module_type_kind_from_semantic :: proc(typ: ^semantic.Type, depth := 0) -> Type_Kind {
	if typ == nil || depth > 32 {
		return .Unknown
	}
	#partial switch typ.kind {
	case .Unknown:
		return .Unknown
	case .Builtin:
		switch typ.name {
	case "i", "int1", "int2", "int4", "int8":
		return .Integer
	case "numeric":
		return .Semantic
		case "p":
			return .Decimal
		case "decfloat16", "decfloat34", "f":
			return .Float
		case "string", "csequence":
			return .String
		case "c", "clike", "abap_bool":
			return .Char
		case "n":
			return .Numc
		case "x", "xstring", "xsequence":
			return .Bytes
		case "d":
			return .Date
		case "t":
			return .Time
		}
		return .Semantic
	case .Named:
		if typ.base != nil {
			return module_type_kind_from_semantic(typ.base, depth + 1)
		}
		return .Semantic
	case .Structure:
		return .Structure
	case .Table:
		return .Table
	case .Ref:
		return .Reference
	case .Class:
		return .Object
	case .Interface:
		return .Interface
	case .Routine:
		return .Routine
	}
	return .Semantic
}

module_semantic_type_name :: proc(module: ^Module, typ: ^semantic.Type) -> (string, bool) {
	name := typ.name
	if typ.kind == .Builtin && typ.has_length {
		builder := strings.builder_make(context.temp_allocator)
		strings.write_string(&builder, name)
		strings.write_byte(&builder, '(')
		buf: [32]byte
		strings.write_string(&builder, strconv.write_int(buf[:], i64(typ.length), 10))
		if typ.has_decimals {
			strings.write_byte(&builder, ',')
			strings.write_string(&builder, strconv.write_int(buf[:], i64(typ.decimals), 10))
		}
		strings.write_byte(&builder, ')')
		return strings.to_string(builder), true
	}
	if typ.kind == .Ref {
		base_name := ""
		base_name_owned := false
		if typ.base != nil {
			base_name, base_name_owned = module_semantic_type_name(module, typ.base)
		}
		if base_name_owned {
			defer delete(base_name, context.temp_allocator)
		}
		if base_name != "" {
			builder := strings.builder_make(context.temp_allocator)
			strings.write_string(&builder, "ref:")
			strings.write_string(&builder, base_name)
			return strings.to_string(builder), true
		}
	}
	return name, false
}

module_runtime_descriptor_from_semantic :: proc(
	module: ^Module,
	typ: ^semantic.Type,
	display_name: string,
	depth := 0,
) -> Runtime_Type_Descriptor {
	if typ == nil || depth > 32 {
		return module_runtime_descriptor_named(module, .Unknown, "unknown")
	}
	display := display_name
	if display == "" {
		display_owned := false
		display, display_owned = module_semantic_type_name(module, typ)
		if display_owned {
			defer delete(display, context.temp_allocator)
		}
	}
	#partial switch typ.kind {
	case .Unknown:
		return module_runtime_descriptor_named(module, .Unknown, display)
	case .Builtin:
		return module_runtime_descriptor_from_builtin(module, typ, display)
	case .Named:
		if typ.base != nil {
			descriptor := module_runtime_descriptor_from_semantic(module, typ.base, display, depth + 1)
			module_runtime_descriptor_apply_table_keys(module, &descriptor, typ)
			return descriptor
		}
		return module_runtime_descriptor_named(module, .Unknown, display)
	case .Structure:
		return module_runtime_descriptor_from_structure(module, typ, display)
	case .Table:
		return module_runtime_descriptor_from_table(module, typ, display)
	case .Ref:
		return module_runtime_descriptor_from_reference(module, typ, display)
	case .Class:
		descriptor := module_runtime_descriptor_named(module, .Object, display)
		return descriptor
	case .Interface:
		descriptor := module_runtime_descriptor_named(module, .Interface, display)
		return descriptor
	case .Routine:
		return module_runtime_descriptor_named(module, .Routine, display)
	}
	return module_runtime_descriptor_named(module, .Unknown, display)
}

module_runtime_descriptor_named :: proc(
	module: ^Module,
	family: Runtime_Type_Family,
	display_name: string,
) -> Runtime_Type_Descriptor {
	name := display_name
	if name == "" {
		name = runtime_type_family_default_name(family)
	}
	return Runtime_Type_Descriptor {
		family = family,
		display_name = strings.clone(name, module.allocator),
	}
}

module_runtime_descriptor_from_builtin :: proc(
	module: ^Module,
	typ: ^semantic.Type,
	display_name: string,
) -> Runtime_Type_Descriptor {
	descriptor := module_runtime_descriptor_named(module, .Unknown, display_name)
	name := typ.name
	length := typ.length
	has_length := typ.has_length
	decimals := typ.decimals
	has_decimals := typ.has_decimals
	switch name {
	case "numeric":
		descriptor.family = .Numeric
	case "i", "int4":
		descriptor.family = .Integer
		descriptor.elementary = Runtime_Elementary_Descriptor{bits = 32, signed = true}
	case "int1":
		descriptor.family = .Integer
		descriptor.elementary = Runtime_Elementary_Descriptor{bits = 8, signed = false}
	case "int2":
		descriptor.family = .Integer
		descriptor.elementary = Runtime_Elementary_Descriptor{bits = 16, signed = true}
	case "int8":
		descriptor.family = .Integer
		descriptor.elementary = Runtime_Elementary_Descriptor{bits = 64, signed = true}
	case "p":
		if !has_length {
			length = 8
			has_length = true
		}
		descriptor.family = .Decimal
		descriptor.elementary = Runtime_Elementary_Descriptor {
			length = length,
			has_length = has_length,
			decimals = decimals,
			has_decimals = has_decimals,
		}
	case "decfloat16", "decfloat34", "f":
		descriptor.family = .Float
	case "string", "csequence":
		descriptor.family = .Text
		descriptor.elementary = Runtime_Elementary_Descriptor {
			text_kind = .String,
			preserves_trailing_blanks = true,
		}
	case "c", "clike", "abap_bool":
		descriptor.family = .Text
		if !has_length {
			length = 1
			has_length = true
		}
		descriptor.elementary = Runtime_Elementary_Descriptor {
			length = length,
			has_length = has_length,
			text_kind = .Fixed,
			preserves_trailing_blanks = false,
		}
	case "n":
		descriptor.family = .Text
		if !has_length {
			length = 1
			has_length = true
		}
		descriptor.elementary = Runtime_Elementary_Descriptor {
			length = length,
			has_length = has_length,
			text_kind = .Numeric,
			preserves_trailing_blanks = false,
		}
	case "d":
		descriptor.family = .Date
		descriptor.elementary = Runtime_Elementary_Descriptor {
			length = 8,
			has_length = true,
			text_kind = .Date,
			preserves_trailing_blanks = false,
		}
	case "t":
		descriptor.family = .Time
		descriptor.elementary = Runtime_Elementary_Descriptor {
			length = 6,
			has_length = true,
			text_kind = .Time,
			preserves_trailing_blanks = false,
		}
	case "x", "xstring", "xsequence":
		descriptor.family = .Bytes
		descriptor.elementary.length = length
		descriptor.elementary.has_length = has_length
	case "data":
		descriptor.family = .Unknown
	case "object":
		descriptor.family = .Object
	}
	return descriptor
}

module_runtime_descriptor_from_structure :: proc(
	module: ^Module,
	typ: ^semantic.Type,
	display_name: string,
) -> Runtime_Type_Descriptor {
	descriptor := module_runtime_descriptor_named(module, .Structure, display_name)
	structure := semantic.checker_type_structure(typ)
	if structure == nil {
		return descriptor
	}
	descriptor.structure.fields = make([dynamic]Runtime_Field_Descriptor, 0, len(structure.fields), module.allocator)
	for field, i in structure.fields {
		field_type := module_type_from_semantic(module, field.type)
		append(
			&descriptor.structure.fields,
			Runtime_Field_Descriptor {
				name = strings.clone(field.name, module.allocator),
				display_name = strings.clone(field.name, module.allocator),
				type = field_type,
				field_index = i32(i),
			},
		)
	}
	return descriptor
}

module_runtime_descriptor_from_table :: proc(
	module: ^Module,
	typ: ^semantic.Type,
	display_name: string,
) -> Runtime_Type_Descriptor {
	descriptor := module_runtime_descriptor_named(module, .Table, display_name)
	row := typ.base if typ.base != nil else nil
	descriptor.table.row_type = module_type_from_semantic(module, row)
	descriptor.table.category = module_table_category_from_semantic_form(typ.table_form)
	module_runtime_descriptor_apply_table_keys(module, &descriptor, typ)
	return descriptor
}

module_runtime_descriptor_from_reference :: proc(
	module: ^Module,
	typ: ^semantic.Type,
	display_name: string,
) -> Runtime_Type_Descriptor {
	descriptor := module_runtime_descriptor_named(module, .Reference, display_name)
	target := typ.base if typ.base != nil else nil
	descriptor.reference.target_type = module_type_from_semantic(module, target)
	descriptor.reference.kind = module_reference_kind_from_semantic(target)
	target_name := ""
	target_name_owned := false
	if target != nil {
		target_name, target_name_owned = module_semantic_type_name(module, target)
	}
	if target_name_owned {
		defer delete(target_name, context.temp_allocator)
	}
	if target_name != "" {
		descriptor.reference.target_name = strings.clone(target_name, module.allocator)
	}
	return descriptor
}

module_table_category_from_semantic_form :: proc "contextless" (form: ast.Data_Type_Form) -> Runtime_Table_Category {
	#partial switch form {
	case .Any_Table:
		return .Any
	case .Index_Table:
		return .Index
	case .Table,
	     .Like_Table,
	     .Standard_Table,
	     .Like_Standard_Table:
		return .Standard
	case .Sorted_Table,
	     .Like_Sorted_Table:
		return .Sorted
	case .Hashed_Table,
	     .Like_Hashed_Table:
		return .Hashed
	case .Range_Of:
		return .Range
	}
	return .Unknown
}

module_reference_kind_from_semantic :: proc(typ: ^semantic.Type, depth := 0) -> Runtime_Reference_Kind {
	if typ == nil || depth > 32 {
		return .Unknown
	}
	#partial switch typ.kind {
	case .Named:
		if typ.base != nil {
			return module_reference_kind_from_semantic(typ.base, depth + 1)
		}
	case .Builtin:
		switch typ.name {
		case "data":
			return .Data
		case "object":
			return .Object
		}
	case .Class:
		if typ.entity != nil && typ.entity.kind == .Exception {
			return .Exception
		}
		return .Class
	case .Interface:
		return .Interface
	case .Ref:
		return module_reference_kind_from_semantic(typ.base, depth + 1)
	}
	if entity := semantic.checker_type_entity(typ); entity != nil {
		#partial switch entity.kind {
		case .Class:
			return .Class
		case .Interface:
			return .Interface
		case .Exception:
			return .Exception
		}
	}
	return .Data
}

module_runtime_descriptor_apply_table_keys :: proc(
	module: ^Module,
	descriptor: ^Runtime_Type_Descriptor,
	typ: ^semantic.Type,
) {
	if descriptor == nil || descriptor.family != .Table || typ == nil {
		return
	}
	entity := semantic.checker_type_entity(typ)
	if entity == nil || entity.decl_info == nil || entity.decl_info.type_clause == nil || entity.decl_info.type_clause.type_ref == nil {
		return
	}
	ref, ok := entity.decl_info.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	if !ok {
		return
	}
	if len(ref.keys) > 0 {
		for key in ref.keys {
			module_runtime_descriptor_add_table_key(module, descriptor, key)
		}
		return
	}
	module_runtime_descriptor_add_table_key(module, descriptor, ref.key)
}

module_runtime_descriptor_add_table_key :: proc(
	module: ^Module,
	descriptor: ^Runtime_Type_Descriptor,
	key: ^ast.Type_Ref_Key_Clause,
) {
	if key == nil {
		return
	}
	out := Runtime_Table_Key_Descriptor {
		name = strings.clone(key.name.text, module.allocator),
		display_name = strings.clone(key.name.text, module.allocator),
		primary = key.name.text == "" || strings.equal_fold(key.name.text, "primary_key"),
		sorted = key.sorted,
		hashed = key.hashed,
		uniqueness = module_table_key_uniqueness(key),
		components = make([dynamic]Runtime_Table_Key_Component, 0, len(key.components), module.allocator),
	}
	for component, i in key.components {
		path := make([dynamic]string, 0, 1, module.allocator)
		append(&path, strings.clone(component.text, module.allocator))
		append(
			&out.components,
			Runtime_Table_Key_Component {
				name = strings.clone(component.text, module.allocator),
				display_name = strings.clone(component.text, module.allocator),
				path = path,
				type = INVALID_TYPE_ID,
				field_index = i32(i),
			},
		)
	}
	if out.primary && descriptor.table.primary_key.components == nil && descriptor.table.primary_key.name == "" {
		descriptor.table.primary_key = out
		return
	}
	if descriptor.table.secondary_keys == nil {
		descriptor.table.secondary_keys = make([dynamic]Runtime_Table_Key_Descriptor, 0, 1, module.allocator)
	}
	append(&descriptor.table.secondary_keys, out)
}

module_table_key_uniqueness :: proc "contextless" (key: ^ast.Type_Ref_Key_Clause) -> Runtime_Table_Key_Uniqueness {
	if key == nil {
		return .Unknown
	}
	#partial switch key.kind {
	case .Unique:
		return .Unique
	case .Non_Unique:
		return .Non_Unique
	case .Empty:
		return .Empty
	case .Default:
		return .Default
	}
	return .Unknown
}

module_add_function :: proc(
	module: ^Module,
	name: string,
	entity: ^semantic.Entity = nil,
	source: Source_Loc = {},
	role: Function_Role = .Unknown,
) -> Function_Id {
	id := Function_Id(len(module.functions))
	function := Function {
		id = id,
		name = name,
		linkage = .Private,
		role = role,
		signature = Function_Signature {
			params = make([dynamic]Type_Id, 0, 4, module.allocator),
			results = make([dynamic]Type_Id, 0, 2, module.allocator),
			calling_convention = calling_convention_for_role(role),
		},
		entity = entity,
		source = source,
		return_types = make([dynamic]Type_Id, 0, 2, module.allocator),
		slots = make([dynamic]Slot, 0, 16, module.allocator),
		projections = make([dynamic]Projection_Path, 0, 8, module.allocator),
		instructions = make([dynamic]Instruction, 0, 32, module.allocator),
		values = make([dynamic]Value, 0, 32, module.allocator),
		uses = make([dynamic]Use, 0, 64, module.allocator),
		blocks = make([dynamic]Block, 0, 8, module.allocator),
		block_order = make([dynamic]Block_Id, 0, 8, module.allocator),
		op_locations = make([dynamic]Op_Location, 0, 32, module.allocator),
		entry = INVALID_BLOCK_ID,
		world_param = INVALID_VALUE_ID,
	}
	append(&function.return_types, BUILTIN_TYPE_WORLD)
	append(&function.signature.results, BUILTIN_TYPE_WORLD)
	append(&module.functions, function)
	return id
}

calling_convention_for_role :: proc "contextless" (role: Function_Role) -> Calling_Convention {
	#partial switch role {
	case .Report_Entry, .Report_Start, .Report_Event, .Event, .Load_Of_Program:
		return .ABAP_Report
	case .Form:
		return .ABAP_Form
	case .Method, .Constructor, .Class_Constructor:
		return .ABAP_Method
	case .Function_Module:
		return .ABAP_Function
	}
	return .IR
}

module_add_entry :: proc(module: ^Module, function_id: Function_Id) {
	assert(function_id != INVALID_FUNCTION_ID && int(function_id) < len(module.functions))
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
	id := Block_Id(len(function.blocks))
	block := Block {
		id = id,
		name = name,
		args = make([dynamic]Value_Id, 0, 2, allocator),
		instructions = make([dynamic]Instruction_Id, 0, 8, allocator),
		terminator = INVALID_INSTRUCTION_ID,
		source = source,
	}
	append(&function.blocks, block)
	append(&function.block_order, id)
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
	id := Value_Id(len(function.values))
	def := Value_Def{}
	#partial switch kind {
	case .Block_Param:
		block_record := block_ptr(function, block)
		def = Block_Arg_Def{block = block, index = u32(len(block_record.args))}
	case .Op_Result:
		def = Instruction_Result_Def{instruction = Instruction_Id(op), index = result_index}
	case .Constant, .Global, .Function:
	}
	append(
		&function.values,
		Value {
			id = id,
			kind = kind,
			type = typ,
			def = def,
			first_use = INVALID_USE_ID,
			block = block,
			op = op,
			result_index = result_index,
			name = name,
			debug_name = name,
			debug = INVALID_METADATA_ID,
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
	append(&block.args, value)
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
	for slot, i in function.slots {
		if entity != nil && slot.entity == entity {
			return Slot_Id(i)
		}
		if entity == nil && slot.entity == nil && slot.kind == kind && slot.name == name {
			return Slot_Id(i)
		}
	}
	id := Slot_Id(len(function.slots))
	append(
		&function.slots,
		Slot {
			kind = kind,
			name = name,
			type = typ,
			is_field_symbol = entity != nil && entity.kind == .Field_Symbol,
			entity = entity,
			source = source,
		},
	)
	return id
}

function_add_projection :: proc(
	function: ^Function,
	segments: []Projection_Segment,
	allocator: mem.Allocator,
) -> Projection_Id {
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

module_add_constant :: proc(
	module: ^Module,
	literal: string,
	typ: Type_Id,
	source: Metadata_Id = INVALID_METADATA_ID,
) -> Constant_Id {
	id := Constant_Id(len(module.constants))
	append(
		&module.constants,
		Constant {
			literal = strings.clone(literal, module.allocator),
			type = typ,
			source = source,
		},
	)
	return id
}

module_add_alias_class :: proc(module: ^Module, name: string, parent: Alias_Class_Id = INVALID_ALIAS_CLASS_ID) -> Alias_Class_Id {
	id := Alias_Class_Id(len(module.alias_classes))
	append(
		&module.alias_classes,
		Alias_Class {
			name = strings.clone(name, module.allocator),
			parent = parent,
			disjoint = make([dynamic]Alias_Class_Id, 0, 2, module.allocator),
			source = INVALID_METADATA_ID,
		},
	)
	return id
}

module_add_effect_scope :: proc(module: ^Module, name: string, typ: Type_Id = INVALID_TYPE_ID) -> Effect_Scope_Id {
	id := Effect_Scope_Id(len(module.effect_scopes))
	append(&module.effect_scopes, Effect_Scope{name = strings.clone(name, module.allocator), type = typ})
	return id
}

module_reference_type :: proc(module: ^Module, pointee: Type_Id) -> Type_Id {
	for typ, i in module.types {
		if typ.kind != .Reference {
			continue
		}
		if data, ok := typ.data.(Reference_Type_Data); ok && data.pointee == pointee {
			return Type_Id(i)
		}
	}
	return module_add_type(
		module,
		Type {
			kind = .Reference,
			data = Reference_Type_Data{pointee = pointee},
		},
	)
}

module_intern_effect_scope :: proc(module: ^Module, name: string, typ: Type_Id = INVALID_TYPE_ID) -> Effect_Scope_Id {
	for scope, i in module.effect_scopes {
		if scope.name == name {
			return Effect_Scope_Id(i)
		}
	}
	return module_add_effect_scope(module, name, typ)
}

module_intern_alias_class :: proc(module: ^Module, name: string) -> Alias_Class_Id {
	for alias, i in module.alias_classes {
		if alias.name == name {
			return Alias_Class_Id(i)
		}
	}
	return module_add_alias_class(module, name)
}

memory_domain_name_for_effects :: proc "contextless" (effects: Effect_Set) -> string {
	if .Read_Global in effects || .Write_Global in effects {
		return "global"
	}
	if .Read_System in effects || .Write_System in effects {
		return "system"
	}
	if .Read_Table in effects || .Write_Table in effects {
		return "table"
	}
	if .SQL in effects {
		return "sql"
	}
	return "local"
}

slot_load_effects :: proc "contextless" (slot: Slot) -> Effect_Set {
	#partial switch slot.kind {
	case .Global:
		return {.Read_Global}
	case .Runtime:
		return {.Read_System}
	}
	return {.Read_Local}
}

slot_store_effects :: proc "contextless" (slot: Slot) -> Effect_Set {
	#partial switch slot.kind {
	case .Global:
		return {.Read_Global, .Write_Global}
	case .Runtime:
		return {.Read_System, .Write_System}
	}
	return {.Read_Local, .Write_Local}
}

module_add_intrinsic :: proc(
	module: ^Module,
	name: string,
	family: Intrinsic_Family,
	op: Intrinsic_Op,
	params: []Type_Id = nil,
	results: []Type_Id = nil,
	effects: Effect_Set = {},
	payload: Intrinsic_Payload = {},
) -> Intrinsic_Id {
	id := Intrinsic_Id(len(module.intrinsics))
	signature := Intrinsic_Signature {
		params = make([dynamic]Type_Id, 0, len(params), module.allocator),
		results = make([dynamic]Type_Id, 0, len(results), module.allocator),
		effects = effects,
		can_throw = .May_Throw in effects,
		can_trap = .May_Trap in effects,
	}
	for param in params {
		append(&signature.params, param)
	}
	for result in results {
		append(&signature.results, result)
	}
	append(
		&module.intrinsics,
		Intrinsic {
			family = family,
			op = op,
			name = strings.clone(name, module.allocator),
			payload = intrinsic_payload_clone(payload, module.allocator),
			effects = effects,
			signature = signature,
			source = INVALID_METADATA_ID,
		},
	)
	return id
}

function_next_instruction :: proc(function: ^Function) -> Instruction_Id {
	id := Instruction_Id(len(function.instructions))
	assert(function.next_instruction_id == u32(id))
	function.next_instruction_id += 1
	append(&function.instructions, Instruction{id = id, parent = INVALID_BLOCK_ID})
	return id
}

function_record_op_location :: proc(function: ^Function, id: Op_Id, location: Op_Location) {
	assert(id != INVALID_OP_ID)
	for len(function.op_locations) <= int(id) {
		append(&function.op_locations, Op_Location{block = INVALID_BLOCK_ID, index = 0xffffffff})
	}
	function.op_locations[int(id)] = location
}

function_store_instruction :: proc(function: ^Function, instruction: Instruction, location: Op_Location) {
	assert(instruction.id != INVALID_INSTRUCTION_ID && int(instruction.id) < len(function.instructions))
	function.instructions[int(instruction.id)] = instruction
	function_record_op_location(function, Op_Id(instruction.id), location)
}

function_add_use :: proc(
	function: ^Function,
	value: Value_Id,
	user: Instruction_Id,
	operand_index: u32,
) -> Use_Id {
	assert(value != INVALID_VALUE_ID && int(value) < len(function.values))
	id := Use_Id(len(function.uses))
	record := Use {
		id = id,
		value = value,
		user = user,
		operand_index = operand_index,
		prev_for_value = INVALID_USE_ID,
		next_for_value = function.values[int(value)].first_use,
	}
	if record.next_for_value != INVALID_USE_ID {
		function.uses[int(record.next_for_value)].prev_for_value = id
	}
	function.values[int(value)].first_use = id
	function.values[int(value)].use_count += 1
	append(&function.uses, record)
	return id
}

instruction_append_operand :: proc(function: ^Function, instruction: ^Instruction, value: Value_Id) {
	use := function_add_use(function, value, instruction.id, u32(len(instruction.operands)))
	append(&instruction.operands, value)
	append(&instruction.operand_uses, use)
}

instruction_append_successor :: proc(
	function: ^Function,
	instruction: ^Instruction,
	target: Block_Id,
	args: []Value_Id,
	kind: Edge_Kind,
) {
	edge := Successor_Edge {
		target = target,
		args = make([dynamic]Value_Id, 0, len(args), context.allocator),
		case_value = INVALID_VALUE_ID,
		operand_start = u32(len(instruction.operands)),
		operand_count = u32(len(args)),
		kind = kind,
		source = INVALID_METADATA_ID,
	}
	for arg in args {
		append(&edge.args, arg)
		instruction_append_operand(function, instruction, arg)
	}
	append(&instruction.successors, edge)
}

intrinsic_family_for_op :: proc "contextless" (op: Intrinsic_Op) -> Intrinsic_Family {
	#partial switch op {
	case .Call_Builtin, .Call_Routine, .Call_Method:
		return .Call
	case .Table_Iter, .Table_Next, .Table_Read, .Table_Append, .Table_Insert, .Table_Modify, .Table_Delete, .Table_Sort:
		return .Table
	case .SQL_Select, .SQL_Open_Cursor, .SQL_Fetch, .SQL_Close_Cursor, .SQL_Insert, .SQL_Update, .SQL_Delete, .SQL_Modify:
		return .SQL
	case .System_Read, .System_Write:
		return .System_Field
	case .Host_Call:
		return .Host
	case .Unsupported:
		return .Unsupported
	}
	return .ABAP
}

intrinsic_name_for_op :: proc "contextless" (op: Intrinsic_Op) -> string {
	switch op {
	case .Unknown:
		return "unknown"
	case .ABAP_Move:
		return "abap.move"
	case .ABAP_Add:
		return "abap.add"
	case .ABAP_Subtract:
		return "abap.sub"
	case .ABAP_Multiply:
		return "abap.mul"
	case .ABAP_Divide:
		return "abap.div"
	case .ABAP_Integer_Divide:
		return "abap.idiv"
	case .ABAP_Modulo:
		return "abap.mod"
	case .ABAP_Equal:
		return "abap.eq"
	case .ABAP_Not_Equal:
		return "abap.ne"
	case .ABAP_Less:
		return "abap.lt"
	case .ABAP_Less_Equal:
		return "abap.le"
	case .ABAP_Greater:
		return "abap.gt"
	case .ABAP_Greater_Equal:
		return "abap.ge"
	case .ABAP_And:
		return "abap.and"
	case .ABAP_Or:
		return "abap.or"
	case .ABAP_Not:
		return "abap.not"
	case .ABAP_Is_Initial:
		return "abap.is_initial"
	case .ABAP_String_Concat:
		return "abap.string.concat"
	case .ABAP_String_Template:
		return "abap.string_template"
	case .ABAP_Concatenate:
		return "abap.concatenate"
	case .ABAP_Condense:
		return "abap.condense"
	case .ABAP_Translate:
		return "abap.translate"
	case .ABAP_Split:
		return "abap.split"
	case .ABAP_Replace:
		return "abap.replace"
	case .ABAP_Shift:
		return "abap.shift"
	case .ABAP_Find:
		return "abap.find"
	case .ABAP_Search:
		return "abap.search"
	case .ABAP_Construct:
		return "abap.construct"
	case .ABAP_Exception_Raise:
		return "abap.exception.raise"
	case .ABAP_Exception_Match:
		return "abap.exception.match"
	case .ABAP_Exception_Catch:
		return "abap.exception.catch"
	case .ABAP_Exception_Unhandled:
		return "abap.exception.unhandled"
	case .ABAP_Message:
		return "abap.message"
	case .ABAP_Write:
		return "abap.write"
	case .ABAP_Clear:
		return "abap.clear"
	case .ABAP_Refresh:
		return "abap.refresh"
	case .ABAP_Free:
		return "abap.free"
	case .ABAP_Assign_Field:
		return "abap.assign_field"
	case .ABAP_Unassign:
		return "abap.unassign"
	case .Call_Builtin:
		return "abap.call.builtin"
	case .Call_Routine:
		return "abap.call.routine"
	case .Call_Method:
		return "abap.call.method"
	case .Table_Iter:
		return "abap.table.iter"
	case .Table_Next:
		return "abap.table.next"
	case .Table_Read:
		return "abap.table.read"
	case .Table_Append:
		return "abap.table.append"
	case .Table_Insert:
		return "abap.table.insert"
	case .Table_Modify:
		return "abap.table.modify"
	case .Table_Delete:
		return "abap.table.delete"
	case .Table_Sort:
		return "abap.table.sort"
	case .SQL_Select:
		return "abap.sql.select"
	case .SQL_Open_Cursor:
		return "abap.sql.open_cursor"
	case .SQL_Fetch:
		return "abap.sql.fetch"
	case .SQL_Close_Cursor:
		return "abap.sql.close_cursor"
	case .SQL_Insert:
		return "abap.sql.insert"
	case .SQL_Update:
		return "abap.sql.update"
	case .SQL_Delete:
		return "abap.sql.delete"
	case .SQL_Modify:
		return "abap.sql.modify"
	case .System_Read:
		return "abap.system.read"
	case .System_Write:
		return "abap.system.write"
	case .Host_Call:
		return "host.call"
	case .Unsupported:
		return "unsupported"
	}
	return "unknown"
}

intrinsic_payload_clone :: proc(payload: Intrinsic_Payload, allocator: mem.Allocator) -> Intrinsic_Payload {
	#partial switch p in payload {
	case Intrinsic_Call_Payload:
		out := p
		out.callee_name = strings.clone(p.callee_name, allocator)
		return out
	case Intrinsic_Message_Payload:
		out := p
		out.id = strings.clone(p.id, allocator)
		out.msg_type = strings.clone(p.msg_type, allocator)
		out.number = strings.clone(p.number, allocator)
		out.display_like = strings.clone(p.display_like, allocator)
		out.raising = strings.clone(p.raising, allocator)
		return out
	case Intrinsic_Exception_Payload:
		out := p
		out.exception_name = strings.clone(p.exception_name, allocator)
		return out
	case Intrinsic_String_Payload:
		return p
	case Intrinsic_Table_Payload:
		return intrinsic_table_payload_clone(p, allocator)
	case Intrinsic_SQL_Payload:
		out := p
		out.source_name = strings.clone(p.source_name, allocator)
		out.source_alias = strings.clone(p.source_alias, allocator)
		return out
	case Intrinsic_System_Field_Payload:
		out := p
		out.system_field = strings.clone(p.system_field, allocator)
		return out
	case Intrinsic_Host_Payload:
		out := p
		out.symbol_name = strings.clone(p.symbol_name, allocator)
		out.abi_name = strings.clone(p.abi_name, allocator)
		return out
	case Intrinsic_Unsupported_Payload:
		out := p
		out.message = strings.clone(p.message, allocator)
		return out
	case Intrinsic_None_Payload:
		return p
	}
	return Intrinsic_None_Payload{}
}

intrinsic_payload_destroy :: proc(payload: ^Intrinsic_Payload, allocator: mem.Allocator) {
	if payload == nil {
		return
	}
	#partial switch p in payload^ {
	case Intrinsic_Call_Payload:
		delete(p.callee_name, allocator)
	case Intrinsic_Message_Payload:
		delete(p.id, allocator)
		delete(p.msg_type, allocator)
		delete(p.number, allocator)
		delete(p.display_like, allocator)
		delete(p.raising, allocator)
	case Intrinsic_Exception_Payload:
		delete(p.exception_name, allocator)
	case Intrinsic_Table_Payload:
		table_payload := p
		intrinsic_table_payload_destroy(&table_payload, allocator)
	case Intrinsic_SQL_Payload:
		delete(p.source_name, allocator)
		delete(p.source_alias, allocator)
	case Intrinsic_System_Field_Payload:
		delete(p.system_field, allocator)
	case Intrinsic_Host_Payload:
		delete(p.symbol_name, allocator)
		delete(p.abi_name, allocator)
	case Intrinsic_Unsupported_Payload:
		delete(p.message, allocator)
	}
	payload^ = {}
}

intrinsic_table_payload_clone :: proc(payload: Intrinsic_Table_Payload, allocator: mem.Allocator) -> Intrinsic_Table_Payload {
	out := payload
	out.key_name = strings.clone(payload.key_name, allocator)
	out.components = make([dynamic]Intrinsic_Table_Component, 0, len(payload.components), allocator)
	for component in payload.components {
		out_component := Intrinsic_Table_Component {
			path = make([dynamic]string, 0, len(component.path), allocator),
			value_index = component.value_index,
		}
		for segment in component.path {
			append(&out_component.path, strings.clone(segment, allocator))
		}
		append(&out.components, out_component)
	}
	out.sort_components = make([dynamic]Intrinsic_Table_Sort_Component, 0, len(payload.sort_components), allocator)
	for component in payload.sort_components {
		out_component := Intrinsic_Table_Sort_Component {
			path = make([dynamic]string, 0, len(component.path), allocator),
			descending = component.descending,
		}
		for segment in component.path {
			append(&out_component.path, strings.clone(segment, allocator))
		}
		append(&out.sort_components, out_component)
	}
	return out
}

intrinsic_table_payload_destroy :: proc(payload: ^Intrinsic_Table_Payload, allocator: mem.Allocator) {
	if payload == nil {
		return
	}
	delete(payload.key_name, allocator)
	for &component in payload.components {
		for segment in component.path {
			delete(segment, allocator)
		}
		delete(component.path)
	}
	for &component in payload.sort_components {
		for segment in component.path {
			delete(segment, allocator)
		}
		delete(component.path)
	}
	delete(payload.components)
	delete(payload.sort_components)
	payload^ = {}
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
	append(&function.signature.params, BUILTIN_TYPE_WORLD)
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
	for arg in block.args {
		if value_type(function, arg) == BUILTIN_TYPE_WORLD {
			return arg
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
	opcode: Opcode,
	operands: []Value_Id = nil,
	result_types: []Type_Id = nil,
	effects: Effect_Set = {},
	attrs: Instruction_Attrs = Instruction_None_Attrs{},
	intrinsic: Intrinsic_Id = INVALID_INTRINSIC_ID,
	source: Source_Loc = {},
) -> Op_Id {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.terminator == INVALID_INSTRUCTION_ID, "cannot emit operation after block terminator")
	for operand in operands {
		assert(operand != INVALID_VALUE_ID && int(operand) < len(function.values), "operation operand must reference an existing value")
	}
	for typ in result_types {
		assert(typ != INVALID_TYPE_ID && int(typ) < len(builder.module.types), "operation result type must reference an existing type")
	}
	op_id := Op_Id(function_next_instruction(function))
	op_attrs := attrs
	if intrinsic != INVALID_INTRINSIC_ID && (opcode == .Intrinsic || opcode == .Invoke) {
		op_attrs = Intrinsic_Call_Attrs{intrinsic = intrinsic}
	}
	op := Op {
		id = op_id,
		parent = builder.block,
		opcode = opcode,
		operands = make([dynamic]Value_Id, 0, len(operands), builder.module.allocator),
		operand_uses = make([dynamic]Use_Id, 0, len(operands), builder.module.allocator),
		results = make([dynamic]Value_Id, 0, len(result_types), builder.module.allocator),
		successors = make([dynamic]Successor_Edge, 0, 0, builder.module.allocator),
		attrs = op_attrs,
		effects = effects,
		memory = make([dynamic]Memory_Access, 0, 1, builder.module.allocator),
		intrinsic = intrinsic,
		source = source,
		debug = INVALID_METADATA_ID,
		semantic = INVALID_METADATA_ID,
	}
	for operand in operands {
		instruction_append_operand(function, &op, operand)
	}
	for typ, i in result_types {
		value := function_add_value(function, .Op_Result, typ, builder.block, op_id, u32(i))
		append(&op.results, value)
	}
	op_memory_access(builder.module, opcode, effects, &op, function)
	op_index := len(block.instructions)
	function_store_instruction(function, op, Op_Location{block = builder.block, index = u32(op_index)})
	append(&block.instructions, Instruction_Id(op_id))
	return op_id
}

builder_add_intrinsic_for_call :: proc(
	builder: ^Builder,
	intrinsic_op: Intrinsic_Op,
	operands: []Value_Id,
	result_types: []Type_Id,
	effects: Effect_Set,
	payload: Intrinsic_Payload = Intrinsic_None_Payload{},
) -> Intrinsic_Id {
	params := make([dynamic]Type_Id, 0, len(operands), context.temp_allocator)
	defer delete(params)
	function := builder_function(builder)
	for operand in operands {
		append(&params, value_type(function, operand))
	}
	return module_add_intrinsic(
		builder.module,
		intrinsic_name_for_op(intrinsic_op),
		intrinsic_family_for_op(intrinsic_op),
		intrinsic_op,
		params[:],
		result_types,
		effects,
		payload,
	)
}

op_memory_access :: proc(
	module: ^Module,
	opcode: Opcode,
	effects: Effect_Set,
	op: ^Op,
	function: ^Function,
) -> bool {
	access: Memory_Access
	access.address_operand = INVALID_OPERAND_INDEX
	access.value_operand = INVALID_OPERAND_INDEX
	#partial switch opcode {
	case .Load:
		access.kind = .Read
		access.address_operand = 1
		if len(op.results) > 0 {
			access.type = value_type(function, op.results[0])
		}
	case .Store:
		access.kind = .Write
		access.address_operand = 1
		access.value_operand = 2
		if len(op.operands) > int(access.value_operand) {
			access.type = value_type(function, op.operands[int(access.value_operand)])
		}
	case:
		if .Read_Table in effects || .SQL in effects || .Read_System in effects {
			access.kind = .Read
		} else if .Write_Table in effects || .Write_System in effects || .Write_Local in effects {
			access.kind = .Write
		} else {
			return false
		}
		access.type = value_type(function, op.results[0]) if len(op.results) > 0 else INVALID_TYPE_ID
	}
	if access.type == INVALID_TYPE_ID {
		return false
	}
	domain := memory_domain_name_for_effects(effects)
	access.alias_class = module_intern_alias_class(module, domain)
	access.scope = module_intern_effect_scope(module, domain, access.type)
	access.source = INVALID_METADATA_ID
	append(&op.memory, access)
	return true
}

terminator_begin :: proc(
	builder: ^Builder,
	opcode: Opcode,
	source: Source_Loc,
) -> Instruction {
	function := builder_function(builder)
	return Instruction {
		id = function_next_instruction(function),
		parent = builder.block,
		opcode = opcode,
		operands = make([dynamic]Value_Id, 0, 4, builder.module.allocator),
		operand_uses = make([dynamic]Use_Id, 0, 4, builder.module.allocator),
		results = make([dynamic]Value_Id, 0, 0, builder.module.allocator),
		successors = make([dynamic]Successor_Edge, 0, 2, builder.module.allocator),
		attrs = Instruction_None_Attrs{},
		memory = make([dynamic]Memory_Access, 0, 0, builder.module.allocator),
		intrinsic = INVALID_INTRINSIC_ID,
		source = source,
		debug = INVALID_METADATA_ID,
		semantic = INVALID_METADATA_ID,
	}
}

terminator_store :: proc(builder: ^Builder, block: ^Block, instruction: Instruction) {
	function := builder_function(builder)
	location := Op_Location{block = builder.block, index = u32(len(block.instructions))}
	function_store_instruction(function, instruction, location)
	block.terminator = instruction.id
}

terminator_append_successor :: proc(
	function: ^Function,
	term: ^Instruction,
	target: Block_Id,
	args: []Value_Id,
	kind: Edge_Kind,
	allocator: mem.Allocator,
) {
	edge := Successor_Edge {
		target = target,
		args = make([dynamic]Value_Id, 0, len(args), allocator),
		case_value = INVALID_VALUE_ID,
		operand_start = u32(len(term.operands)),
		operand_count = u32(len(args)),
		kind = kind,
		source = INVALID_METADATA_ID,
	}
	for arg in args {
		append(&edge.args, arg)
		instruction_append_operand(function, term, arg)
	}
	append(&term.successors, edge)
}

builder_emit_effect_op :: proc(
	builder: ^Builder,
	opcode: Opcode,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	effects: Effect_Set = {},
	attrs: Instruction_Attrs = Instruction_None_Attrs{},
	intrinsic: Intrinsic_Id = INVALID_INTRINSIC_ID,
	source: Source_Loc = {},
) -> Op_Id {
	assert(builder.current_world != INVALID_VALUE_ID, "effect operation requires current world token")
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

	op_id := builder_emit_op(builder, opcode, operands[:], results[:], effects, attrs, intrinsic, source)
	op := op_ptr(builder_function(builder), op_id)
	builder.current_world = op.results[0]
	return op_id
}

builder_emit_intrinsic :: proc(
	builder: ^Builder,
	intrinsic_op: Intrinsic_Op,
	operands: []Value_Id = nil,
	result_types: []Type_Id = nil,
	effects: Effect_Set = {},
	payload: Intrinsic_Payload = Intrinsic_None_Payload{},
	source: Source_Loc = {},
) -> Op_Id {
	intrinsic := builder_add_intrinsic_for_call(builder, intrinsic_op, operands, result_types, effects, payload)
	return builder_emit_op(builder, .Intrinsic, operands, result_types, effects, intrinsic = intrinsic, source = source)
}

builder_emit_effect_intrinsic :: proc(
	builder: ^Builder,
	intrinsic_op: Intrinsic_Op,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	effects: Effect_Set = {},
	payload: Intrinsic_Payload = Intrinsic_None_Payload{},
	source: Source_Loc = {},
) -> Op_Id {
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
	intrinsic := builder_add_intrinsic_for_call(builder, intrinsic_op, operands[:], results[:], effects, payload)
	op_id := builder_emit_op(builder, .Intrinsic, operands[:], results[:], effects, intrinsic = intrinsic, source = source)
	op := op_ptr(builder_function(builder), op_id)
	builder.current_world = op.results[0]
	return op_id
}

builder_emit_core_call_invoke :: proc(
	builder: ^Builder,
	target: Function_Id,
	normal_target: Block_Id,
	exception_target: Block_Id,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	source: Source_Loc = {},
) -> Op_Id {
	assert(target != INVALID_FUNCTION_ID && int(target) < len(builder.module.functions))
	op_id := builder_emit_effect_op(
		builder,
		.Invoke,
		inputs,
		result_types,
		effects = {.Calls_IR, .May_Throw},
		attrs = Call_Attrs{target = target},
		source = source,
	)
	builder_append_invoke_world_successors(builder, op_id, normal_target, exception_target)
	return op_id
}

builder_emit_effect_intrinsic_invoke :: proc(
	builder: ^Builder,
	intrinsic_op: Intrinsic_Op,
	normal_target: Block_Id,
	exception_target: Block_Id,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	effects: Effect_Set = {},
	payload: Intrinsic_Payload = Intrinsic_None_Payload{},
	source: Source_Loc = {},
) -> Op_Id {
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
	intrinsic := builder_add_intrinsic_for_call(builder, intrinsic_op, operands[:], results[:], effects, payload)
	op_id := builder_emit_op(builder, .Invoke, operands[:], results[:], effects, intrinsic = intrinsic, source = source)
	op := op_ptr(builder_function(builder), op_id)
	builder.current_world = op.results[0]
	builder_append_invoke_world_successors(builder, op_id, normal_target, exception_target)
	return op_id
}

builder_append_invoke_world_successors :: proc(
	builder: ^Builder,
	op_id: Op_Id,
	normal_target: Block_Id,
	exception_target: Block_Id,
) {
	function := builder_function(builder)
	op := op_ptr(function, op_id)
	assert(op.opcode == .Invoke)
	assert(len(op.results) > 0 && value_type(function, op.results[0]) == BUILTIN_TYPE_WORLD)
	assert(normal_target != INVALID_BLOCK_ID && int(normal_target) < len(function.blocks))
	assert(exception_target != INVALID_BLOCK_ID && int(exception_target) < len(function.blocks))
	args := [?]Value_Id{op.results[0]}
	instruction_append_successor(function, op, normal_target, args[:], .Normal)
	instruction_append_successor(function, op, exception_target, args[:], .Exception)
}

builder_emit_const :: proc(
	builder: ^Builder,
	literal: string,
	typ: Type_Id,
	source: Source_Loc = {},
) -> Value_Id {
	result_types := [?]Type_Id{typ}
	constant := module_add_constant(builder.module, literal, typ)
	op_id := builder_emit_op(
		builder,
		.Const,
		result_types = result_types[:],
		attrs = constant,
		source = source,
	)
	return op_ptr(builder_function(builder), op_id).results[0]
}

builder_emit_slot_address :: proc(builder: ^Builder, slot: Slot_Id, source: Source_Loc = {}) -> Value_Id {
	function := builder_function(builder)
	slot_record := slot_ptr(function, slot)
	result_types := [?]Type_Id{module_reference_type(builder.module, slot_record.type)}
	op_id := builder_emit_op(
		builder,
		.Addr_Of,
		result_types = result_types[:],
		attrs = Slot_Address_Attrs{slot = slot},
		source = source,
	)
	return op_ptr(function, op_id).results[0]
}

builder_emit_load :: proc(builder: ^Builder, slot: Slot_Id, source: Source_Loc = {}) -> Value_Id {
	function := builder_function(builder)
	slot_record := slot_ptr(function, slot)
	address := builder_emit_slot_address(builder, slot, source)
	typ := slot_record.type
	operands := [?]Value_Id{builder.current_world, address}
	result_types := [?]Type_Id{typ}
	op_id := builder_emit_op(
		builder,
		.Load,
		operands = operands[:],
		result_types = result_types[:],
		effects = slot_load_effects(slot_record^),
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
	function := builder_function(builder)
	slot_record := slot_ptr(function, slot)
	address := builder_emit_slot_address(builder, slot, source)
	inputs := [?]Value_Id{address, value}
	builder_emit_effect_op(
		builder,
		.Store,
		inputs[:],
		effects = slot_store_effects(slot_record^),
		source = source,
	)
}

builder_emit_field_address :: proc(
	builder: ^Builder,
	base: Value_Id,
	projection: Projection_Id,
	result_type: Type_Id,
	source: Source_Loc = {},
) -> Value_Id {
	operands := [?]Value_Id{base}
	result_types := [?]Type_Id{module_reference_type(builder.module, result_type)}
	op_id := builder_emit_op(
		builder,
		.Field_Addr,
		operands[:],
		result_types[:],
		attrs = projection,
		source = source,
	)
	return op_ptr(builder_function(builder), op_id).results[0]
}

builder_emit_field_load :: proc(
	builder: ^Builder,
	base: Value_Id,
	projection: Projection_Id,
	result_type: Type_Id,
	source: Source_Loc = {},
) -> Value_Id {
	address := builder_emit_field_address(builder, base, projection, result_type, source)
	operands := [?]Value_Id{builder.current_world, address}
	result_types := [?]Type_Id{result_type}
	op_id := builder_emit_op(
		builder,
		.Load,
		operands[:],
		result_types[:],
			effects = {.Read_Local},
			source = source,
		)
	return op_ptr(builder_function(builder), op_id).results[0]
}

builder_emit_field_store :: proc(
	builder: ^Builder,
	base: Value_Id,
	value: Value_Id,
	projection: Projection_Id,
	source: Source_Loc = {},
) -> Op_Id {
	address := builder_emit_field_address(builder, base, projection, value_type(builder_function(builder), value), source)
	inputs := [?]Value_Id{address, value}
	return builder_emit_effect_op(
		builder,
		.Store,
		inputs[:],
		effects = {.Read_Local, .Write_Local},
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
	assert(target != INVALID_FUNCTION_ID && int(target) < len(builder.module.functions))
	return builder_emit_effect_op(
		builder,
		.Call,
		inputs,
		result_types,
		effects = {.Calls_IR, .May_Throw},
		attrs = Call_Attrs{target = target},
		source = source,
	)
}

builder_emit_builtin_call :: proc(
	builder: ^Builder,
	callee_name: string,
	result_type: Type_Id,
	inputs: []Value_Id = nil,
	source: Source_Loc = {},
) -> Value_Id {
	result_types := [?]Type_Id{result_type}
	op_id := builder_emit_intrinsic(
		builder,
		.Call_Builtin,
		inputs,
		result_types[:],
		payload = Intrinsic_Call_Payload {
			callee_name = callee_name,
			call_kind   = .Builtin,
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
	call_kind: Abap_Call_Kind = .Unknown,
) -> Op_Id {
	kind := call_kind
	if kind == .Unknown {
		kind = abap_call_kind_for_entity(target)
	}
	call_function_target, has_call_function_target := builder_call_function_target(builder.module, target)
	return builder_emit_effect_intrinsic(
		builder,
		.Call_Routine,
		inputs,
		result_types,
		effects = {.Calls_IR, .May_Throw},
		payload = Intrinsic_Call_Payload {
			callee_name = callee_name,
			call_kind = kind,
			call_function_target = call_function_target,
			has_call_function_target = has_call_function_target,
		},
		source = source,
	)
}

builder_emit_routine_invoke :: proc(
	builder: ^Builder,
	target: ^semantic.Entity,
	callee_name: string,
	normal_target: Block_Id,
	exception_target: Block_Id,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	source: Source_Loc = {},
	call_kind: Abap_Call_Kind = .Unknown,
) -> Op_Id {
	kind := call_kind
	if kind == .Unknown {
		kind = abap_call_kind_for_entity(target)
	}
	call_function_target, has_call_function_target := builder_call_function_target(builder.module, target)
	return builder_emit_effect_intrinsic_invoke(
		builder,
		.Call_Routine,
		normal_target,
		exception_target,
		inputs,
		result_types,
		effects = {.Calls_IR, .May_Throw},
		payload = Intrinsic_Call_Payload {
			callee_name = callee_name,
			call_kind = kind,
			call_function_target = call_function_target,
			has_call_function_target = has_call_function_target,
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
) -> Op_Id {
	call_function_target, has_call_function_target := builder_call_function_target(builder.module, target)
	return builder_emit_effect_intrinsic(
		builder,
		.Call_Method,
		inputs,
		result_types,
		effects = {.Calls_IR, .May_Throw},
		payload = Intrinsic_Call_Payload {
			callee_name = callee_name,
			call_kind = .Method,
			call_function_target = call_function_target,
			has_call_function_target = has_call_function_target,
		},
		source = source,
	)
}

builder_emit_method_invoke :: proc(
	builder: ^Builder,
	target: ^semantic.Entity,
	callee_name: string,
	normal_target: Block_Id,
	exception_target: Block_Id,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	source: Source_Loc = {},
) -> Op_Id {
	call_function_target, has_call_function_target := builder_call_function_target(builder.module, target)
	return builder_emit_effect_intrinsic_invoke(
		builder,
		.Call_Method,
		normal_target,
		exception_target,
		inputs,
		result_types,
		effects = {.Calls_IR, .May_Throw},
		payload = Intrinsic_Call_Payload {
			callee_name = callee_name,
			call_kind = .Method,
			call_function_target = call_function_target,
			has_call_function_target = has_call_function_target,
		},
		source = source,
	)
}

builder_call_function_target :: proc "contextless" (
	module: ^Module,
	entity: ^semantic.Entity,
) -> (
	Function_Id,
	bool,
) {
	if entity == nil {
		return INVALID_FUNCTION_ID, false
	}
	for function, i in module.functions {
		if function.entity == entity {
			return Function_Id(i), true
		}
	}
	return INVALID_FUNCTION_ID, false
}

builder_emit_message :: proc(
	builder: ^Builder,
	inputs: []Value_Id = nil,
	result_type: Type_Id = INVALID_TYPE_ID,
	payload: Intrinsic_Message_Payload = {},
	source: Source_Loc = {},
) -> Value_Id {
	result_types := make([dynamic]Type_Id, 0, 1, context.temp_allocator)
	defer delete(result_types)
	if result_type != INVALID_TYPE_ID {
		append(&result_types, result_type)
	}
	op_id := builder_emit_effect_intrinsic(
		builder,
		.ABAP_Message,
		inputs,
		result_types[:],
		effects = {.IO, .May_Trap},
		payload = payload,
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	if result_type != INVALID_TYPE_ID && len(op.results) > 1 {
		return op.results[1]
	}
	return INVALID_VALUE_ID
}

builder_emit_exception_raise :: proc(
	builder: ^Builder,
	exception_name: string,
	source: Source_Loc = {},
) {
	builder_emit_effect_intrinsic(
		builder,
		.ABAP_Exception_Raise,
		effects = {.May_Throw},
		payload = Intrinsic_Exception_Payload{exception_name = exception_name},
		source = source,
	)
}

builder_emit_exception_raise_invoke :: proc(
	builder: ^Builder,
	exception_name: string,
	normal_target: Block_Id,
	exception_target: Block_Id,
	source: Source_Loc = {},
) {
	builder_emit_effect_intrinsic_invoke(
		builder,
		.ABAP_Exception_Raise,
		normal_target,
		exception_target,
		effects = {.May_Throw},
		payload = Intrinsic_Exception_Payload{exception_name = exception_name},
		source = source,
	)
}

builder_emit_exception_match :: proc(
	builder: ^Builder,
	exception_name: string,
	source: Source_Loc = {},
) -> Value_Id {
	operands := [?]Value_Id{builder.current_world}
	result_types := [?]Type_Id{BUILTIN_TYPE_PREDICATE}
	op_id := builder_emit_intrinsic(
		builder,
		.ABAP_Exception_Match,
		operands[:],
		result_types[:],
		payload = Intrinsic_Exception_Payload{exception_name = exception_name},
		source = source,
	)
	return op_ptr(builder_function(builder), op_id).results[0]
}

builder_emit_exception_catch :: proc(
	builder: ^Builder,
	result_type: Type_Id = INVALID_TYPE_ID,
	source: Source_Loc = {},
) -> Value_Id {
	result_types := make([dynamic]Type_Id, 0, 1, context.temp_allocator)
	defer delete(result_types)
	if result_type != INVALID_TYPE_ID {
		append(&result_types, result_type)
	}
	op_id := builder_emit_effect_intrinsic(
		builder,
		.ABAP_Exception_Catch,
		result_types = result_types[:],
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	if result_type != INVALID_TYPE_ID && len(op.results) > 1 {
		return op.results[1]
	}
	return INVALID_VALUE_ID
}

builder_emit_exception_unhandled :: proc(
	builder: ^Builder,
	source: Source_Loc = {},
) {
	builder_emit_effect_intrinsic(
		builder,
		.ABAP_Exception_Unhandled,
		effects = {.May_Trap},
		source = source,
	)
}

builder_emit_write :: proc(
	builder: ^Builder,
	inputs: []Value_Id = nil,
	source: Source_Loc = {},
) {
	builder_emit_effect_intrinsic(
		builder,
		.ABAP_Write,
		inputs,
		effects = {.IO},
		source = source,
	)
}

builder_emit_table_iter :: proc(
	builder: ^Builder,
	table: Value_Id,
	row_type: Type_Id,
	payload: Intrinsic_Table_Payload = {},
	source: Source_Loc = {},
	filter_inputs: []Value_Id = nil,
) -> Value_Id {
	inputs := make([dynamic]Value_Id, 0, 1 + len(filter_inputs), context.temp_allocator)
	defer delete(inputs)
	append(&inputs, table)
	for input in filter_inputs {
		append(&inputs, input)
	}
	result_types := [?]Type_Id{BUILTIN_TYPE_TABLE_ITERATOR}
	op_id := builder_emit_effect_intrinsic(
		builder,
		.Table_Iter,
		inputs[:],
		result_types[:],
		effects = {.Read_Table},
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
	payload: Intrinsic_Table_Payload = {},
	source: Source_Loc = {},
) -> (
	Value_Id,
	Value_Id,
	Value_Id,
	Value_Id,
) {
	inputs := [?]Value_Id{iter}
	result_types := [?]Type_Id{BUILTIN_TYPE_PREDICATE, row_result_type, BUILTIN_TYPE_INTEGER, BUILTIN_TYPE_INTEGER}
	op_id := builder_emit_effect_intrinsic(
		builder,
		.Table_Next,
		inputs[:],
		result_types[:],
		effects = {.Read_Table},
			payload = builder_table_payload(payload, row_type),
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	return op.results[1], op.results[2], op.results[3], op.results[4]
}

builder_emit_table_read :: proc(
	builder: ^Builder,
	inputs: []Value_Id,
	row_result_type: Type_Id,
	row_type: Type_Id,
	payload: Intrinsic_Table_Payload = {},
	source: Source_Loc = {},
) -> (
	Value_Id,
	Value_Id,
	Value_Id,
) {
	result_types := [?]Type_Id{row_result_type, BUILTIN_TYPE_INTEGER, BUILTIN_TYPE_INTEGER}
	op_id := builder_emit_effect_intrinsic(
		builder,
		.Table_Read,
		inputs,
		result_types[:],
		effects = {.Read_Table},
			payload = builder_table_payload(payload, row_type),
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	return op.results[1], op.results[2], op.results[3]
}

builder_emit_table_mutation :: proc(
	builder: ^Builder,
	intrinsic_op: Intrinsic_Op,
	inputs: []Value_Id,
	row_type: Type_Id,
	payload: Intrinsic_Table_Payload = {},
	source: Source_Loc = {},
) -> Op_Id {
	assert(
		intrinsic_op == .Table_Append ||
		intrinsic_op == .Table_Insert ||
		intrinsic_op == .Table_Modify ||
		intrinsic_op == .Table_Delete ||
		intrinsic_op == .Table_Sort,
		"table mutation builder requires a table mutation operation",
	)
	result_types := [?]Type_Id{BUILTIN_TYPE_INTEGER, BUILTIN_TYPE_INTEGER}
	return builder_emit_effect_intrinsic(
		builder,
		intrinsic_op,
		inputs,
		result_types[:],
		effects = {.Read_Table, .Write_Table},
			payload = builder_table_payload(payload, row_type),
		source = source,
	)
}

builder_table_payload :: proc(payload: Intrinsic_Table_Payload, row_type: Type_Id) -> Intrinsic_Payload {
	out := payload
	out.row_type = row_type
	return out
}

builder_emit_sql_select :: proc(
	builder: ^Builder,
	result_type: Type_Id,
	payload: Intrinsic_SQL_Payload = {},
	source: Source_Loc = {},
) -> (
	Value_Id,
	Value_Id,
) {
	result_types := [?]Type_Id{result_type, BUILTIN_TYPE_INTEGER}
	op_id := builder_emit_effect_intrinsic(
		builder,
		.SQL_Select,
		result_types = result_types[:],
		effects = {.SQL},
		payload = payload,
		source = source,
	)
	op := op_ptr(builder_function(builder), op_id)
	return op.results[1], op.results[2]
}

builder_emit_sql_cursor :: proc(
	builder: ^Builder,
	intrinsic_op: Intrinsic_Op,
	handle: Value_Id,
	payload: Intrinsic_SQL_Payload = {},
	source: Source_Loc = {},
) -> Op_Id {
	assert(
		intrinsic_op == .SQL_Open_Cursor ||
		intrinsic_op == .SQL_Fetch ||
		intrinsic_op == .SQL_Close_Cursor,
		"SQL cursor builder requires a cursor operation",
	)
	inputs := [?]Value_Id{handle}
	return builder_emit_effect_intrinsic(
		builder,
		intrinsic_op,
		inputs[:],
		effects = {.SQL},
		payload = payload,
		source = source,
	)
}

builder_emit_sql_mutation :: proc(
	builder: ^Builder,
	intrinsic_op: Intrinsic_Op,
	inputs: []Value_Id = nil,
	payload: Intrinsic_SQL_Payload = {},
	source: Source_Loc = {},
) -> Op_Id {
	assert(
		intrinsic_op == .SQL_Insert ||
		intrinsic_op == .SQL_Update ||
		intrinsic_op == .SQL_Delete ||
		intrinsic_op == .SQL_Modify,
		"SQL mutation builder requires a database mutation operation",
	)
	return builder_emit_effect_intrinsic(
		builder,
		intrinsic_op,
		inputs,
		effects = {.SQL},
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
	builder_emit_effect_intrinsic(
		builder,
		.System_Write,
		inputs[:],
		effects = {.Write_System},
		payload = Intrinsic_System_Field_Payload{system_field = field_name},
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
	op_id := builder_emit_intrinsic(
		builder,
		.System_Read,
		operands[:],
		result_types[:],
		effects = {.Read_System},
		payload = Intrinsic_System_Field_Payload{system_field = field_name},
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
		.Unsupported,
		result_types = result_types[:],
		effects = {.May_Trap, .Unsupported},
		attrs = Unsupported_Attrs{message = message},
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
	assert(block.terminator == INVALID_INSTRUCTION_ID, "cannot replace block terminator")
	assert(target != INVALID_BLOCK_ID && int(target) < len(function.blocks), "branch target must reference an existing block")
	for arg in args {
		assert(arg != INVALID_VALUE_ID && int(arg) < len(function.values), "branch argument must reference an existing value")
	}
	term := terminator_begin(builder, .Br, source)
	terminator_append_successor(function, &term, target, args, .Normal, builder.module.allocator)
	terminator_store(builder, block, term)
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
	assert(block.terminator == INVALID_INSTRUCTION_ID, "cannot replace block terminator")
	assert(condition != INVALID_VALUE_ID && int(condition) < len(function.values), "conditional branch must reference an existing condition value")
	assert(true_target != INVALID_BLOCK_ID && int(true_target) < len(function.blocks), "true branch target must reference an existing block")
	assert(false_target != INVALID_BLOCK_ID && int(false_target) < len(function.blocks), "false branch target must reference an existing block")
	for arg in true_args {
		assert(arg != INVALID_VALUE_ID && int(arg) < len(function.values), "true branch argument must reference an existing value")
	}
	for arg in false_args {
		assert(arg != INVALID_VALUE_ID && int(arg) < len(function.values), "false branch argument must reference an existing value")
	}
	term := terminator_begin(builder, .Cond_Br, source)
	instruction_append_operand(function, &term, condition)
	terminator_append_successor(function, &term, true_target, true_args, .True, builder.module.allocator)
	terminator_append_successor(function, &term, false_target, false_args, .False, builder.module.allocator)
	terminator_store(builder, block, term)
}

builder_set_switch :: proc(
	builder: ^Builder,
	selector: Value_Id,
	default_target: Block_Id,
	default_args: []Value_Id,
	cases: []Switch_Case,
	source: Source_Loc = {},
) {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.terminator == INVALID_INSTRUCTION_ID, "cannot replace block terminator")
	assert(selector != INVALID_VALUE_ID && int(selector) < len(function.values), "switch must reference an existing selector value")
	assert(default_target != INVALID_BLOCK_ID && int(default_target) < len(function.blocks), "switch default target must reference an existing block")
	for arg in default_args {
		assert(arg != INVALID_VALUE_ID && int(arg) < len(function.values), "switch default argument must reference an existing value")
	}
	term := terminator_begin(builder, .Switch, source)
	instruction_append_operand(function, &term, selector)
	terminator_append_successor(function, &term, default_target, default_args, .Normal, builder.module.allocator)
	for switch_case in cases {
		assert(switch_case.value != INVALID_VALUE_ID && int(switch_case.value) < len(function.values), "switch case must reference an existing value")
		assert(switch_case.target != INVALID_BLOCK_ID && int(switch_case.target) < len(function.blocks), "switch case target must reference an existing block")
		for arg in switch_case.args {
			assert(arg != INVALID_VALUE_ID && int(arg) < len(function.values), "switch case argument must reference an existing value")
		}
		instruction_append_operand(function, &term, switch_case.value)
		terminator_append_successor(function, &term, switch_case.target, switch_case.args, .Switch_Case, builder.module.allocator)
		term.successors[len(term.successors) - 1].case_value = switch_case.value
	}
	terminator_store(builder, block, term)
}

builder_set_return :: proc(builder: ^Builder, values: []Value_Id = nil, source: Source_Loc = {}) {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.terminator == INVALID_INSTRUCTION_ID, "cannot replace block terminator")
	for value in values {
		assert(value != INVALID_VALUE_ID && int(value) < len(function.values), "return value must reference an existing value")
	}
	term := terminator_begin(builder, .Return, source)
	for value in values {
		instruction_append_operand(function, &term, value)
	}
	terminator_store(builder, block, term)
}

builder_set_return_world :: proc(builder: ^Builder, source: Source_Loc = {}) {
	values := [?]Value_Id{builder.current_world}
	builder_set_return(builder, values[:], source)
}

builder_set_unreachable :: proc(builder: ^Builder, source: Source_Loc = {}) {
	function := builder_function(builder)
	block := block_ptr(function, builder.block)
	assert(block.terminator == INVALID_INSTRUCTION_ID, "cannot replace block terminator")
	term := terminator_begin(builder, .Unreachable, source)
	terminator_store(builder, block, term)
}
