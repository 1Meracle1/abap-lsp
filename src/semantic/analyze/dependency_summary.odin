package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Summary_Provider_Input :: struct {
	uri:               string,
	payload:           string,
	object_kind:       string,
	object_name:       string,
	provided_names:    [dynamic]string,
	exports:           [dynamic]Summary_Provider_Export_Input,
	classes:           [dynamic]Summary_Provider_Class_Input,
	members:           [dynamic]Summary_Provider_Class_Member_Input,
	functions:         [dynamic]Summary_Provider_Function_Input,
	types:             [dynamic]Summary_Provider_Type_Input,
	type_pool_name:    string,
	type_pool_symbols: [dynamic]string,
}

Summary_Provider_Export_Input :: struct {
	kind:  string,
	name:  string,
	owner: string,
}

Summary_Provider_Type_Ref_Input :: struct {
	namespace:     string,
	is_ref:        bool,
	base_name:     string,
	display:       string,
	form:          string,
	table_has_of:  bool,
}

Summary_Provider_Parameter_Input :: struct {
	name:        string,
	section:     string,
	passing:     string,
	type_ref:    Summary_Provider_Type_Ref_Input,
	is_optional: bool,
	has_default: bool,
}

Summary_Provider_Exception_Input :: struct {
	name: string,
}

Summary_Provider_Class_Input :: struct {
	name:                   string,
	kind:                   string,
	signature:              string,
	superclass_name:        string,
	implemented_interfaces: [dynamic]string,
	friends:                [dynamic]string,
	is_abstract:            bool,
}

Summary_Provider_Class_Member_Input :: struct {
	owner:            string,
	name:             string,
	kind:             string,
	visibility:       string,
	signature:        string,
	type_ref:         Summary_Provider_Type_Ref_Input,
	alias_interface:  string,
	alias_member:     string,
	event_name:       string,
	event_source:     Summary_Provider_Type_Ref_Input,
	parameters:       [dynamic]Summary_Provider_Parameter_Input,
	exceptions:       [dynamic]Summary_Provider_Exception_Input,
	is_static:        bool,
	is_redefinition:  bool,
	for_event:        bool,
}

Summary_Provider_Function_Input :: struct {
	name:       string,
	signature:  string,
	parameters: [dynamic]Summary_Provider_Parameter_Input,
	exceptions: [dynamic]Summary_Provider_Exception_Input,
}

Summary_Provider_Type_Field_Input :: struct {
	name:        string,
	type_ref:    Summary_Provider_Type_Ref_Input,
	description: string,
	is_key:      bool,
	is_include:  bool,
}

Summary_Provider_Type_Input :: struct {
	name:       string,
	shape_kind: string,
	type_ref:   Summary_Provider_Type_Ref_Input,
	fields:     [dynamic]Summary_Provider_Type_Field_Input,
}

dependency_summary_input_make :: proc(allocator: mem.Allocator) -> Summary_Provider_Input {
	return Summary_Provider_Input {
		provided_names    = make([dynamic]string, 0, 4, allocator),
		exports           = make([dynamic]Summary_Provider_Export_Input, 0, 8, allocator),
		classes           = make([dynamic]Summary_Provider_Class_Input, 0, 2, allocator),
		members           = make([dynamic]Summary_Provider_Class_Member_Input, 0, 8, allocator),
		functions         = make([dynamic]Summary_Provider_Function_Input, 0, 2, allocator),
		types             = make([dynamic]Summary_Provider_Type_Input, 0, 4, allocator),
		type_pool_symbols = make([dynamic]string, 0, 8, allocator),
	}
}

dependency_summary_input_has_export :: proc(
	input: ^Summary_Provider_Input,
	name: string,
	kind := "",
) -> bool {
	if input == nil || name == "" {
		return false
	}
	canonical := canonical_name(name, context.temp_allocator)
	for export in input.exports {
		if export.name == canonical && (kind == "" || export.kind == kind) {
			return true
		}
	}
	return false
}

summary_provider_entity_lookup :: proc(
	input: ^Summary_Provider_Input,
	namespace: Namespace,
	name: string,
) -> (Entity_Id, bool) {
	if input == nil || name == "" {
		return INVALID_SYMBOL_ID, false
	}
	canonical := canonical_name(name, context.temp_allocator)
	for export, i in input.exports {
		if export.name != canonical || !summary_provider_export_occupies(export.kind, namespace) {
			continue
		}
		return Entity_Id(Symbol_Id(u32(i))), true
	}
	for class, i in input.classes {
		if class.name == canonical && namespace == .Type {
			return Entity_Id(Symbol_Id(u32(i))), true
		}
	}
	for function, i in input.functions {
		if function.name == canonical && namespace == .Routine {
			return Entity_Id(Symbol_Id(u32(i))), true
		}
	}
	for typ, i in input.types {
		if typ.name == canonical && namespace == .Type {
			return Entity_Id(Symbol_Id(u32(i))), true
		}
	}
	for symbol_name, i in input.type_pool_symbols {
		if symbol_name != canonical {
			continue
		}
		kind := dependency_summary_typepool_symbol_kind(input^, canonical)
		if symbol_kind_occupies(kind, namespace) {
			return Entity_Id(Symbol_Id(u32(i))), true
		}
	}
	if input.object_name == canonical {
		switch input.object_kind {
		case "class", "interface", "type":
			if namespace == .Type {
				return Entity_Id(Symbol_Id(0)), true
			}
		case "function-module":
			if namespace == .Routine {
				return Entity_Id(Symbol_Id(0)), true
			}
		}
	}
	return INVALID_SYMBOL_ID, false
}

summary_provider_slot :: proc(
	project: ^Project_Analysis,
	provider: Provider_Handle,
) -> (^Summary_Provider_Input, bool) {
	if project == nil || provider.kind != .Summary_Provider {
		return nil, false
	}
	index := int(provider.id)
	if index < 0 || index >= len(project.providers.summaries) {
		return nil, false
	}
	return &project.providers.summaries[index], true
}

summary_provider_export_occupies :: proc(kind: string, namespace: Namespace) -> bool {
	if strings.equal_fold(kind, "class") ||
	   strings.equal_fold(kind, "interface") ||
	   strings.equal_fold(kind, "type") {
		return namespace == .Type
	}
	if strings.equal_fold(kind, "function-module") {
		return namespace == .Routine
	}
	if strings.equal_fold(kind, "constant") ||
	   strings.equal_fold(kind, "variable") {
		return namespace == .Value
	}
	return false
}

summary_provider_class_member_lookup :: proc(
	input: ^Summary_Provider_Input,
	class_name: string,
	member_name: string,
) -> (^Summary_Provider_Class_Member_Input, bool) {
	if input == nil || class_name == "" || member_name == "" {
		return nil, false
	}
	class_canonical := canonical_name(class_name, context.temp_allocator)
	member_canonical := canonical_name(member_name, context.temp_allocator)
	for &member in input.members {
		if member.owner == class_canonical && member.name == member_canonical {
			return &member, true
		}
	}
	return nil, false
}

summary_provider_class_lookup :: proc(
	input: ^Summary_Provider_Input,
	name: string,
) -> (^Summary_Provider_Class_Input, bool) {
	if input == nil || name == "" {
		return nil, false
	}
	canonical := canonical_name(name, context.temp_allocator)
	for &class in input.classes {
		if class.name == canonical {
			return &class, true
		}
	}
	return nil, false
}

summary_provider_type_lookup :: proc(
	input: ^Summary_Provider_Input,
	name: string,
) -> (^Summary_Provider_Type_Input, bool) {
	if input == nil || name == "" {
		return nil, false
	}
	canonical := canonical_name(name, context.temp_allocator)
	for &typ in input.types {
		if typ.name == canonical {
			return &typ, true
		}
	}
	return nil, false
}

summary_provider_field_type_ref :: proc(
	ref: Summary_Provider_Type_Ref_Input,
	allocator: mem.Allocator,
) -> Field_Type_Ref_Data {
	return dependency_summary_field_type_ref(ref, allocator)
}

dependency_summary_input_clone :: proc(
	input: Summary_Provider_Input,
	allocator: mem.Allocator,
) -> Summary_Provider_Input {
	out := dependency_summary_input_make(allocator)
	out.uri = strings.clone(input.uri, allocator)
	out.payload = strings.clone(input.payload, allocator)
	out.object_kind = strings.clone(input.object_kind, allocator)
	out.object_name = canonical_name(input.object_name, allocator)
	for name in input.provided_names {
		append(&out.provided_names, canonical_name(name, allocator))
	}
	for export in input.exports {
		append(
			&out.exports,
			Summary_Provider_Export_Input {
				kind  = strings.clone(export.kind, allocator),
				name  = canonical_name(export.name, allocator),
				owner = canonical_name(export.owner, allocator),
			},
		)
	}
	for class in input.classes {
		next := Summary_Provider_Class_Input {
			name                   = canonical_name(class.name, allocator),
			kind                   = strings.clone(class.kind, allocator),
			signature              = strings.clone(class.signature, allocator),
			superclass_name        = canonical_name(class.superclass_name, allocator),
			implemented_interfaces = make([dynamic]string, 0, len(class.implemented_interfaces), allocator),
			friends                = make([dynamic]string, 0, len(class.friends), allocator),
			is_abstract            = class.is_abstract,
		}
		for interface_name in class.implemented_interfaces {
			append(&next.implemented_interfaces, canonical_name(interface_name, allocator))
		}
		for friend in class.friends {
			append(&next.friends, canonical_name(friend, allocator))
		}
		append(&out.classes, next)
	}
	for member in input.members {
		next := Summary_Provider_Class_Member_Input {
			owner           = canonical_name(member.owner, allocator),
			name            = canonical_name(member.name, allocator),
			kind            = strings.clone(member.kind, allocator),
			visibility      = strings.clone(member.visibility, allocator),
			signature       = strings.clone(member.signature, allocator),
			type_ref        = dependency_summary_type_ref_clone(member.type_ref, allocator),
			alias_interface = canonical_name(member.alias_interface, allocator),
			alias_member    = canonical_name(member.alias_member, allocator),
			event_name      = canonical_name(member.event_name, allocator),
			event_source    = dependency_summary_type_ref_clone(member.event_source, allocator),
			parameters      = make([dynamic]Summary_Provider_Parameter_Input, 0, len(member.parameters), allocator),
			exceptions      = make([dynamic]Summary_Provider_Exception_Input, 0, len(member.exceptions), allocator),
			is_static       = member.is_static,
			is_redefinition = member.is_redefinition,
			for_event       = member.for_event,
		}
		for param in member.parameters {
			append(&next.parameters, dependency_summary_parameter_clone(param, allocator))
		}
		for exception in member.exceptions {
			append(&next.exceptions, Summary_Provider_Exception_Input {
				name = canonical_name(exception.name, allocator),
			})
		}
		append(&out.members, next)
	}
	for function in input.functions {
		next := Summary_Provider_Function_Input {
			name       = canonical_name(function.name, allocator),
			signature  = strings.clone(function.signature, allocator),
			parameters = make([dynamic]Summary_Provider_Parameter_Input, 0, len(function.parameters), allocator),
			exceptions = make([dynamic]Summary_Provider_Exception_Input, 0, len(function.exceptions), allocator),
		}
		for param in function.parameters {
			append(&next.parameters, dependency_summary_parameter_clone(param, allocator))
		}
		for exception in function.exceptions {
			append(&next.exceptions, Summary_Provider_Exception_Input {
				name = canonical_name(exception.name, allocator),
			})
		}
		append(&out.functions, next)
	}
	for typ in input.types {
		next := Summary_Provider_Type_Input {
			name       = canonical_name(typ.name, allocator),
			shape_kind = strings.clone(typ.shape_kind, allocator),
			type_ref   = dependency_summary_type_ref_clone(typ.type_ref, allocator),
			fields     = make([dynamic]Summary_Provider_Type_Field_Input, 0, len(typ.fields), allocator),
		}
		for field in typ.fields {
			append(
				&next.fields,
				Summary_Provider_Type_Field_Input {
					name        = canonical_name(field.name, allocator),
					type_ref    = dependency_summary_type_ref_clone(field.type_ref, allocator),
					description = strings.clone(field.description, allocator),
					is_key      = field.is_key,
					is_include  = field.is_include,
				},
			)
		}
		append(&out.types, next)
	}
	out.type_pool_name = canonical_name(input.type_pool_name, allocator)
	for symbol in input.type_pool_symbols {
		append(&out.type_pool_symbols, canonical_name(symbol, allocator))
	}
	return out
}

dependency_summary_type_ref_clone :: proc(
	ref: Summary_Provider_Type_Ref_Input,
	allocator: mem.Allocator,
) -> Summary_Provider_Type_Ref_Input {
	return Summary_Provider_Type_Ref_Input {
		namespace    = strings.clone(ref.namespace, allocator),
		is_ref       = ref.is_ref,
		base_name    = canonical_name(ref.base_name, allocator),
		display      = strings.clone(ref.display, allocator),
		form         = strings.clone(ref.form, allocator),
		table_has_of = ref.table_has_of,
	}
}

dependency_summary_parameter_clone :: proc(
	param: Summary_Provider_Parameter_Input,
	allocator: mem.Allocator,
) -> Summary_Provider_Parameter_Input {
	return Summary_Provider_Parameter_Input {
		name        = canonical_name(param.name, allocator),
		section     = strings.clone(param.section, allocator),
		passing     = strings.clone(param.passing, allocator),
		type_ref    = dependency_summary_type_ref_clone(param.type_ref, allocator),
		is_optional = param.is_optional,
		has_default = param.has_default,
	}
}

@(private)
dependency_summary_field_type_ref :: proc(
	ref: Summary_Provider_Type_Ref_Input,
	allocator: mem.Allocator,
) -> Field_Type_Ref_Data {
	return Field_Type_Ref_Data {
		namespace       = dependency_summary_namespace(ref.namespace),
		is_ref          = ref.is_ref,
		base_name       = canonical_name(ref.base_name, allocator),
		field_path      = make([dynamic]string, 0, 0, allocator),
		field_ranges    = make([dynamic]tokenizer.Range, 0, 0, allocator),
		field_derefs    = make([dynamic]bool, 0, 0, allocator),
		field_selectors = make([dynamic]ast.Selector_Op, 0, 0, allocator),
	}
}

dependency_summary_typepool_symbol_kind :: proc(
	input: Summary_Provider_Input,
	name: string,
) -> Symbol_Kind {
	for export in input.exports {
		if export.name != name {
			continue
		}
		if strings.equal_fold(export.kind, "constant") {
			return .Constant
		}
		if strings.equal_fold(export.kind, "type") {
			return .Type_Def
		}
	}
	return .Type_Def
}

@(private)
dependency_summary_namespace :: proc(value: string) -> Namespace {
	if strings.equal_fold(value, "value") {
		return .Value
	}
	if strings.equal_fold(value, "routine") {
		return .Routine
	}
	return .Type
}
