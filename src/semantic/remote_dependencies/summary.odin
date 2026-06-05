package abap_frontend_semantic_remote_dependencies

import "src:ast"
import "src:parser"
import analyze "src:semantic/analyze"
import deps "src:semantic/dependencies"

import base_runtime "base:runtime"
import "core:mem"
import "core:strings"

DEPENDENCY_INTERFACE_SUMMARY_PAYLOAD_VERSION :: "abapls-summary-v1"

Dependency_Interface_Object_Summary :: struct {
	kind:           string,
	name:           string,
	object_kind:    string,
	object_type:    string,
	object_uri:     string,
	file_extension: string,
}

Dependency_Interface_Export_Summary :: struct {
	kind:  string,
	name:  string,
	owner: string,
}

Dependency_Interface_Type_Ref_Summary :: struct {
	namespace:     string,
	is_ref:        bool,
	base_name:     string,
	field_path:    [dynamic]string,
	display:       string,
	form:          string,
	table_has_of:  bool,
}

Dependency_Interface_Parameter_Summary :: struct {
	name:        string,
	section:     string,
	passing:     string,
	type_ref:    Dependency_Interface_Type_Ref_Summary,
	is_optional: bool,
	has_default: bool,
}

Dependency_Interface_Exception_Summary :: struct {
	name: string,
}

Dependency_Interface_Class_Member_Summary :: struct {
	owner:            string,
	name:             string,
	kind:             string,
	visibility:       string,
	signature:        string,
	type_ref:         Dependency_Interface_Type_Ref_Summary,
	alias_interface:  string,
	alias_member:     string,
	event_name:       string,
	event_source:     Dependency_Interface_Type_Ref_Summary,
	parameters:       [dynamic]Dependency_Interface_Parameter_Summary,
	exceptions:       [dynamic]Dependency_Interface_Exception_Summary,
	is_static:        bool,
	is_redefinition:  bool,
	for_event:        bool,
}

Dependency_Interface_Class_Summary :: struct {
	name:                   string,
	kind:                   string,
	signature:              string,
	superclass_name:        string,
	implemented_interfaces: [dynamic]string,
	friends:                [dynamic]string,
	is_abstract:            bool,
}

Dependency_Interface_Function_Summary :: struct {
	name:       string,
	signature:  string,
	parameters: [dynamic]Dependency_Interface_Parameter_Summary,
	exceptions: [dynamic]Dependency_Interface_Exception_Summary,
}

Dependency_Interface_Type_Field_Summary :: struct {
	name:        string,
	type_ref:    Dependency_Interface_Type_Ref_Summary,
	description: string,
	is_key:      bool,
	is_include:  bool,
}

Dependency_Interface_Type_Summary :: struct {
	name:       string,
	shape_kind: string,
	type_ref:   Dependency_Interface_Type_Ref_Summary,
	fields:     [dynamic]Dependency_Interface_Type_Field_Summary,
}

Dependency_Interface_Typepool_Summary :: struct {
	name:    string,
	symbols: [dynamic]string,
}

Dependency_Interface_Summary :: struct {
	object:         Dependency_Interface_Object_Summary,
	provided_names: [dynamic]string,
	exports:        [dynamic]Dependency_Interface_Export_Summary,
	classes:        [dynamic]Dependency_Interface_Class_Summary,
	members:        [dynamic]Dependency_Interface_Class_Member_Summary,
	functions:      [dynamic]Dependency_Interface_Function_Summary,
	types:          [dynamic]Dependency_Interface_Type_Summary,
	type_pool:      Dependency_Interface_Typepool_Summary,
}

dependency_interface_summary_make :: proc(
	allocator: mem.Allocator,
) -> Dependency_Interface_Summary {
	return Dependency_Interface_Summary {
		provided_names = make([dynamic]string, 0, 4, allocator),
		exports        = make([dynamic]Dependency_Interface_Export_Summary, 0, 8, allocator),
		classes        = make([dynamic]Dependency_Interface_Class_Summary, 0, 2, allocator),
		members        = make([dynamic]Dependency_Interface_Class_Member_Summary, 0, 8, allocator),
		functions      = make([dynamic]Dependency_Interface_Function_Summary, 0, 2, allocator),
		types          = make([dynamic]Dependency_Interface_Type_Summary, 0, 4, allocator),
	}
}

dependency_interface_summary_payload_from_artifact :: proc(
	object_kind, object_name, object_uri, object_type, file_extension, source: string,
	allocator: mem.Allocator,
) -> string {
	scratch_arena: mem.Dynamic_Arena
	scratch_backing := base_runtime.heap_allocator()
	mem.dynamic_arena_init(&scratch_arena, scratch_backing, scratch_backing, alignment = 64)
	defer mem.dynamic_arena_destroy(&scratch_arena)
	scratch_allocator := mem.dynamic_arena_allocator(&scratch_arena)
	summary := dependency_interface_summary_from_artifact(
		object_kind,
		object_name,
		object_uri,
		object_type,
		file_extension,
		source,
		scratch_allocator,
	)
	if !dependency_interface_summary_has_payload(&summary) {
		return ""
	}
	payload := serialize_dependency_interface_summary(&summary, scratch_allocator)
	return strings.clone(payload, allocator)
}

dependency_interface_summary_from_artifact :: proc(
	object_kind, object_name, object_uri, object_type, file_extension, source: string,
	allocator: mem.Allocator,
) -> Dependency_Interface_Summary {
	summary := dependency_interface_summary_make(allocator)
	summary.object = Dependency_Interface_Object_Summary {
		kind           = dependency_summary_object_kind(object_kind, object_type, allocator),
		name           = dependency_summary_name(object_name, allocator),
		object_kind    = strings.to_lower(strings.trim_space(object_kind), allocator),
		object_type    = strings.clone(strings.trim_space(object_type), allocator),
		object_uri     = strings.clone(strings.trim_space(object_uri), allocator),
		file_extension = strings.to_lower(strings.trim_space(file_extension), allocator),
	}
	if strings.equal_fold(summary.object.object_kind, TYPEPOOL_OBJECT_KIND) {
		summary.type_pool = Dependency_Interface_Typepool_Summary {
			name    = summary.object.name,
			symbols = make([dynamic]string, 0, 8, allocator),
		}
	}

	summary_source := dependency_summary_source(
		object_kind,
		object_name,
		file_extension,
		source,
		allocator,
	)
	if summary_source == "" {
		dependency_interface_summary_add_object_export(&summary, allocator)
		return summary
	}

	parsed := parser.parse(summary_source, dependency_summary_uri(&summary.object, allocator), allocator)
	unit := analyze.collect_source_file(
		analyze.Source_File_Id(0),
		dependency_summary_uri(&summary.object, allocator),
		summary_source,
		parsed,
		allocator,
		.Dependency_Interface_Source,
	)
	analyze.scope_index_destroy(&unit.scope_index)
	unit.scope_index = analyze.build_scope_index(&unit, allocator)
	analyze.resolve_unit_with_index(&unit, &unit.scope_index)
	dependency_interface_summary_populate_from_unit(&summary, &unit, allocator)
	dependency_interface_summary_add_object_export(&summary, allocator)
	return summary
}

dependency_interface_summary_from_payload :: proc(
	payload: string,
	allocator: mem.Allocator,
) -> (Dependency_Interface_Summary, bool) {
	summary := dependency_interface_summary_make(allocator)
	lines := payload
	first := true
	for line in strings.split_lines_iterator(&lines) {
		if strings.trim_space(line) == "" {
			continue
		}
		fields := dependency_summary_payload_fields(line, allocator)
		if len(fields) == 0 {
			continue
		}
		if first {
			first = false
			if fields[0] != DEPENDENCY_INTERFACE_SUMMARY_PAYLOAD_VERSION {
				return summary, false
			}
			continue
		}
		dependency_interface_summary_apply_payload_fields(&summary, fields[:], allocator)
	}
	return summary, !first
}

dependency_interface_summary_exports :: proc(
	summary: ^Dependency_Interface_Summary,
	allocator: mem.Allocator,
) -> [dynamic]Dependency_Interface_Export_Summary {
	exports := make([dynamic]Dependency_Interface_Export_Summary, 0, len(summary.exports), allocator)
	for export in summary.exports {
		append(&exports, Dependency_Interface_Export_Summary {
			kind  = strings.clone(export.kind, allocator),
			name  = strings.clone(export.name, allocator),
			owner = strings.clone(export.owner, allocator),
		})
	}
	return exports
}

dependency_interface_summary_exports_from_payload :: proc(
	payload: string,
	allocator: mem.Allocator,
) -> [dynamic]Dependency_Interface_Export_Summary {
	exports := make([dynamic]Dependency_Interface_Export_Summary, 0, 8, allocator)
	lines := payload
	first := true
	for line in strings.split_lines_iterator(&lines) {
		fields := dependency_summary_payload_fields(line, allocator)
		if len(fields) == 0 {
			continue
		}
		if first {
			first = false
			if fields[0] != DEPENDENCY_INTERFACE_SUMMARY_PAYLOAD_VERSION {
				return exports
			}
			continue
		}
		if fields[0] == "export" && len(fields) >= 4 {
			append(&exports, Dependency_Interface_Export_Summary {
				kind  = strings.clone(fields[1], allocator),
				name  = strings.clone(fields[2], allocator),
				owner = strings.clone(fields[3], allocator),
			})
		}
	}
	return exports
}

serialize_dependency_interface_summary :: proc(
	summary: ^Dependency_Interface_Summary,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	dependency_summary_write_record(&out, {DEPENDENCY_INTERFACE_SUMMARY_PAYLOAD_VERSION})
	dependency_summary_write_record(
		&out,
		{
			"object",
			summary.object.kind,
			summary.object.name,
			summary.object.object_kind,
			summary.object.object_type,
			summary.object.object_uri,
			summary.object.file_extension,
		},
	)
	for name in summary.provided_names {
		dependency_summary_write_record(&out, {"provided", name})
	}
	for export in summary.exports {
		dependency_summary_write_record(&out, {"export", export.kind, export.name, export.owner})
	}
	for class in summary.classes {
		dependency_summary_write_record(
			&out,
			{
				"class",
				class.kind,
				class.name,
				class.signature,
				class.superclass_name,
				dependency_summary_bool_text(class.is_abstract),
			},
		)
		for interface_name in class.implemented_interfaces {
			dependency_summary_write_record(&out, {"implements", class.name, interface_name})
		}
		for friend in class.friends {
			dependency_summary_write_record(&out, {"friend", class.name, friend})
		}
	}
	for member in summary.members {
		dependency_summary_write_record(
			&out,
			{
				"member",
				member.owner,
				member.kind,
				member.visibility,
				dependency_summary_bool_text(member.is_static),
				dependency_summary_bool_text(member.is_redefinition),
				member.name,
				member.signature,
				member.type_ref.display,
				member.type_ref.base_name,
				dependency_summary_bool_text(member.type_ref.is_ref),
				member.alias_interface,
				member.alias_member,
				member.event_name,
				member.event_source.display,
				member.event_source.base_name,
				dependency_summary_bool_text(member.for_event),
			},
		)
		for param in member.parameters {
			dependency_summary_write_parameter_record(&out, "member-param", member.owner, member.name, param)
		}
		for exception in member.exceptions {
			dependency_summary_write_record(&out, {"member-exception", member.owner, member.name, exception.name})
		}
	}
	for function in summary.functions {
		dependency_summary_write_record(&out, {"function", function.name, function.signature})
		for param in function.parameters {
			dependency_summary_write_parameter_record(&out, "function-param", function.name, "", param)
		}
		for exception in function.exceptions {
			dependency_summary_write_record(&out, {"function-exception", function.name, exception.name})
		}
	}
	for typ in summary.types {
		dependency_summary_write_record(
			&out,
			{
				"type",
				typ.name,
				typ.shape_kind,
				typ.type_ref.display,
				typ.type_ref.base_name,
				dependency_summary_bool_text(typ.type_ref.is_ref),
				typ.type_ref.form,
				dependency_summary_bool_text(typ.type_ref.table_has_of),
			},
		)
		for field in typ.fields {
			dependency_summary_write_record(
				&out,
				{
					"type-field",
					typ.name,
					field.name,
					field.type_ref.display,
					field.type_ref.base_name,
					dependency_summary_bool_text(field.type_ref.is_ref),
					dependency_summary_bool_text(field.is_key),
					dependency_summary_bool_text(field.is_include),
					field.description,
				},
			)
		}
	}
	if summary.type_pool.name != "" {
		dependency_summary_write_record(&out, {"typepool", summary.type_pool.name})
		for symbol in summary.type_pool.symbols {
			dependency_summary_write_record(&out, {"typepool-symbol", symbol})
		}
	}
	return strings.to_string(out)
}

@(private)
dependency_summary_write_parameter_record :: proc(
	out: ^strings.Builder,
	record_kind, owner, member: string,
	param: Dependency_Interface_Parameter_Summary,
) {
	dependency_summary_write_record(
		out,
		{
			record_kind,
			owner,
			member,
			param.section,
			param.passing,
			param.name,
			param.type_ref.display,
			param.type_ref.base_name,
			dependency_summary_bool_text(param.type_ref.is_ref),
			dependency_summary_bool_text(param.is_optional),
			dependency_summary_bool_text(param.has_default),
			param.type_ref.form,
			dependency_summary_bool_text(param.type_ref.table_has_of),
		},
	)
}

@(private)
dependency_interface_summary_has_payload :: proc(summary: ^Dependency_Interface_Summary) -> bool {
	return len(summary.exports) > 0 ||
	       len(summary.classes) > 0 ||
	       len(summary.functions) > 0 ||
	       len(summary.types) > 0 ||
	       len(summary.type_pool.symbols) > 0
}

@(private)
dependency_interface_summary_populate_from_unit :: proc(
	summary: ^Dependency_Interface_Summary,
	unit: ^analyze.Source_File_Provider,
	allocator: mem.Allocator,
) {
	for name in unit.provided_names {
		dependency_summary_append_unique_string(&summary.provided_names, name, allocator)
	}
	for &s in unit.symbols {
		if s.scope != unit.root_scope || analyze.symbol_kind_is_builtin(s.kind) {
			continue
		}
		export_kind := dependency_summary_export_kind_from_symbol(s.kind)
		if export_kind == "" {
			continue
		}
		dependency_interface_summary_add_export(summary, export_kind, s.name, "", allocator)
		#partial switch s.kind {
		case .Class, .Interface:
			dependency_interface_summary_add_class(summary, unit, s.id, allocator)
		case .Type_Def:
			dependency_interface_summary_add_type(summary, unit, &s, allocator)
			if strings.equal_fold(summary.object.object_kind, TYPEPOOL_OBJECT_KIND) {
				dependency_summary_append_unique_string(&summary.type_pool.symbols, s.name, allocator)
			}
		case .Constant:
			if strings.equal_fold(summary.object.object_kind, TYPEPOOL_OBJECT_KIND) {
				dependency_summary_append_unique_string(&summary.type_pool.symbols, s.name, allocator)
			}
		case .Module:
			dependency_interface_summary_add_function(summary, unit, &s, allocator)
		case:
		}
	}
	for &s in unit.symbols {
		dependency_interface_summary_add_member(summary, unit, &s, allocator)
	}
	dependency_interface_summary_add_structure_fields(summary, unit, allocator)
}

@(private)
dependency_interface_summary_add_object_export :: proc(
	summary: ^Dependency_Interface_Summary,
	allocator: mem.Allocator,
) {
	if summary.object.name == "" || summary.object.kind == "" {
		return
	}
	dependency_interface_summary_add_export(summary, summary.object.kind, summary.object.name, "", allocator)
}

@(private)
dependency_interface_summary_add_class :: proc(
	summary: ^Dependency_Interface_Summary,
	unit: ^analyze.Source_File_Provider,
	symbol_id: analyze.Symbol_Id,
	allocator: mem.Allocator,
) {
	s := analyze.symbol(unit, symbol_id)
	if s == nil {
		return
	}
	for existing in summary.classes {
		if existing.name == s.name {
			return
		}
	}
	info := analyze.entity_decl_info(unit, symbol_id)
	class := Dependency_Interface_Class_Summary {
		name                   = strings.clone(s.name, allocator),
		kind                   = "interface" if s.kind == .Interface else "class",
		signature              = strings.clone(info.signature, allocator) if info != nil else "",
		implemented_interfaces = make([dynamic]string, 0, 2, allocator),
		friends                = make([dynamic]string, 0, 2, allocator),
	}
	if info != nil {
		class.is_abstract = .Is_Abstract in info.flags
	}
	for inheritance in unit.class_inheritance {
		if inheritance.class_symbol == symbol_id {
			class.superclass_name = strings.clone(inheritance.superclass_name, allocator)
			break
		}
	}
	for definition in unit.class_definitions {
		if definition.class_symbol == symbol_id && definition.is_abstract {
			class.is_abstract = true
			break
		}
	}
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol == symbol_id {
			dependency_summary_append_unique_string(
				&class.implemented_interfaces,
				implemented.interface_name,
				allocator,
			)
		}
	}
	for friend in unit.class_friends {
		if friend.class_symbol == symbol_id {
			dependency_summary_append_unique_string(&class.friends, friend.friend_name, allocator)
		}
	}
	append(&summary.classes, class)
}

@(private)
dependency_interface_summary_add_member :: proc(
	summary: ^Dependency_Interface_Summary,
	unit: ^analyze.Source_File_Provider,
	s: ^analyze.Symbol_Data,
	allocator: mem.Allocator,
) {
	info := analyze.decl_info(unit, s.decl_info)
	scope_data := analyze.scope(unit, s.scope)
	if info == nil ||
	   scope_data == nil ||
	   !(scope_data.kind == .Class || scope_data.kind == .Interface) ||
	   scope_data.owner != info.owner ||
	   info.visibility == .Private {
		return
	}
	owner := analyze.symbol(unit, info.owner)
	if owner == nil {
		return
	}
	kind := dependency_summary_member_kind(info, s.kind)
	if kind == "" {
		return
	}
	for existing in summary.members {
		if existing.owner == owner.name && existing.name == s.name && existing.kind == kind {
			return
		}
	}
	member := Dependency_Interface_Class_Member_Summary {
		owner           = strings.clone(owner.name, allocator),
		name            = strings.clone(s.name, allocator),
		kind            = strings.clone(kind, allocator),
		visibility      = dependency_summary_visibility_text(info.visibility),
		signature       = strings.clone(info.signature, allocator),
		alias_interface = strings.clone(info.alias_target_interface_name, allocator),
		alias_member    = strings.clone(info.alias_target_member_name, allocator),
		event_name      = strings.clone(info.event_name, allocator),
		parameters      = make([dynamic]Dependency_Interface_Parameter_Summary, 0, len(info.signature_parameters), allocator),
		exceptions      = make([dynamic]Dependency_Interface_Exception_Summary, 0, len(info.signature_exceptions), allocator),
		is_static       = .Is_Static in info.flags,
		is_redefinition = .Is_Redefinition in info.flags,
		for_event       = .For_Event in info.flags,
	}
	member.type_ref = dependency_summary_type_ref_from_symbol(s, allocator)
	member.event_source = dependency_summary_type_ref_from_field_ref(
		info.event_source_type,
		"",
		"",
		false,
		allocator,
	)
	for param in info.signature_parameters {
		append(&member.parameters, dependency_summary_parameter_from_decl(param, allocator))
	}
	for exception in info.signature_exceptions {
		append(&member.exceptions, Dependency_Interface_Exception_Summary {
			name = strings.clone(exception.name, allocator),
		})
	}
	append(&summary.members, member)
	dependency_interface_summary_add_export(summary, member.kind, member.name, member.owner, allocator)
}

@(private)
dependency_interface_summary_add_function :: proc(
	summary: ^Dependency_Interface_Summary,
	unit: ^analyze.Source_File_Provider,
	s: ^analyze.Symbol_Data,
	allocator: mem.Allocator,
) {
	info := analyze.entity_decl_info(unit, s.id)
	if info == nil {
		return
	}
	for existing in summary.functions {
		if existing.name == s.name {
			return
		}
	}
	function := Dependency_Interface_Function_Summary {
		name       = strings.clone(s.name, allocator),
		signature  = strings.clone(info.signature, allocator),
		parameters = make([dynamic]Dependency_Interface_Parameter_Summary, 0, len(info.signature_parameters), allocator),
		exceptions = make([dynamic]Dependency_Interface_Exception_Summary, 0, len(info.signature_exceptions), allocator),
	}
	for param in info.signature_parameters {
		append(&function.parameters, dependency_summary_parameter_from_decl(param, allocator))
	}
	for exception in info.signature_exceptions {
		append(&function.exceptions, Dependency_Interface_Exception_Summary {
			name = strings.clone(exception.name, allocator),
		})
	}
	append(&summary.functions, function)
}

@(private)
dependency_interface_summary_add_type :: proc(
	summary: ^Dependency_Interface_Summary,
	unit: ^analyze.Source_File_Provider,
	s: ^analyze.Symbol_Data,
	allocator: mem.Allocator,
) {
	if s.name == "" {
		return
	}
	for existing in summary.types {
		if existing.name == s.name {
			return
		}
	}
	shape := "alias"
	if s.structure != analyze.INVALID_STRUCTURE_ID {
		shape = "structure"
	} else if s.has_type_clause_form && dependency_summary_type_form_is_table(s.type_clause_form) {
		shape = "table"
	}
	append(&summary.types, Dependency_Interface_Type_Summary {
		name       = strings.clone(s.name, allocator),
		shape_kind = shape,
		type_ref   = dependency_summary_type_ref_from_symbol(s, allocator),
		fields     = make([dynamic]Dependency_Interface_Type_Field_Summary, 0, 4, allocator),
	})
	_ = unit
}

@(private)
dependency_interface_summary_add_structure_fields :: proc(
	summary: ^Dependency_Interface_Summary,
	unit: ^analyze.Source_File_Provider,
	allocator: mem.Allocator,
) {
	for st in unit.structures {
		if st.name == "" {
			continue
		}
		type_index := dependency_summary_type_index(summary, st.name)
		if type_index < 0 {
			if !dependency_summary_export_name_exists(summary, st.name) {
				continue
			}
			append(&summary.types, Dependency_Interface_Type_Summary {
				name       = strings.clone(st.name, allocator),
				shape_kind = "structure",
				fields     = make([dynamic]Dependency_Interface_Type_Field_Summary, 0, len(st.fields), allocator),
			})
			type_index = len(summary.types) - 1
		}
		summary.types[type_index].shape_kind = "structure"
		for field in st.fields {
			if field.name == "" {
				continue
			}
			append(&summary.types[type_index].fields, Dependency_Interface_Type_Field_Summary {
				name        = strings.clone(field.name, allocator),
				type_ref    = dependency_summary_type_ref_from_field(
					field,
					dependency_summary_data_type_form_text(field.type_clause_form) if field.has_type_clause_form else "",
					allocator,
				),
				description = strings.clone(field.description, allocator),
				is_key      = .Is_Key in field.flags,
				is_include  = .Is_Include in field.flags,
			})
		}
	}
}

@(private)
dependency_summary_parameter_from_decl :: proc(
	param: analyze.Decl_Signature_Parameter_Data,
	allocator: mem.Allocator,
) -> Dependency_Interface_Parameter_Summary {
	return Dependency_Interface_Parameter_Summary {
		name        = strings.clone(param.name, allocator),
		section     = dependency_summary_decl_section_text(param.section),
		passing     = dependency_summary_decl_passing_text(param.passing),
		type_ref    = dependency_summary_type_ref_from_field_ref(
			param.declared_type,
			param.type_clause_display,
			dependency_summary_data_type_form_text(param.type_clause_form) if param.has_type_clause_form else "",
			param.type_clause_table_has_of,
			allocator,
		),
		is_optional = .Is_Optional in param.flags,
		has_default = .Has_Default_Value in param.flags,
	}
}

@(private)
dependency_summary_type_ref_from_symbol :: proc(
	s: ^analyze.Symbol_Data,
	allocator: mem.Allocator,
) -> Dependency_Interface_Type_Ref_Summary {
	return dependency_summary_type_ref_from_field_ref(
		s.declared_type,
		s.type_clause_display,
		dependency_summary_data_type_form_text(s.type_clause_form) if s.has_type_clause_form else "",
		s.type_clause_table_has_of,
		allocator,
	)
}

@(private)
dependency_summary_type_ref_from_field :: proc(
	field: analyze.Structure_Field_Data,
	form: string,
	allocator: mem.Allocator,
) -> Dependency_Interface_Type_Ref_Summary {
	display := field.type_ref.base_name
	return dependency_summary_type_ref_from_field_ref(
		field.type_ref,
		display,
		form,
		false,
		allocator,
	)
}

@(private)
dependency_summary_type_ref_from_field_ref :: proc(
	ref: analyze.Field_Type_Ref_Data,
	display, form: string,
	table_has_of: bool,
	allocator: mem.Allocator,
) -> Dependency_Interface_Type_Ref_Summary {
	field_path := make([dynamic]string, 0, len(ref.field_path), allocator)
	for field in ref.field_path {
		append(&field_path, strings.clone(field, allocator))
	}
	return Dependency_Interface_Type_Ref_Summary {
		namespace    = dependency_summary_namespace_text(ref.namespace),
		is_ref       = ref.is_ref,
		base_name    = strings.clone(ref.base_name, allocator),
		field_path   = field_path,
		display      = strings.clone(display, allocator),
		form         = strings.clone(form, allocator),
		table_has_of = table_has_of,
	}
}

@(private)
dependency_summary_source :: proc(
	object_kind, object_name, file_extension, source: string,
	allocator: mem.Allocator,
) -> string {
	if source == "" {
		return ""
	}
	candidate := deps.Remote_Dependency_Candidate {
		name = object_name,
		kind = .Type,
	}
	if dependency_object_kind_is_ddic(object_kind) ||
	   dependency_source_is_xml(object_kind, file_extension, source) {
		return dependency_input_source(
			candidate,
			object_name,
			object_kind,
			file_extension,
			source,
			allocator,
		)
	}
	if strings.equal_fold(object_kind, TYPEPOOL_OBJECT_KIND) {
		return typepool_dependency_source(source, allocator)
	}
	return strings.clone(source, allocator)
}

@(private)
dependency_summary_uri :: proc(
	object: ^Dependency_Interface_Object_Summary,
	allocator: mem.Allocator,
) -> string {
	if object.object_uri != "" {
		return strings.clone(object.object_uri, allocator)
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-summary:/")
	strings.write_string(&out, object.object_kind)
	strings.write_byte(&out, '/')
	strings.write_string(&out, object.name)
	return strings.to_string(out)
}

@(private)
dependency_summary_object_kind :: proc(
	object_kind, object_type: string,
	allocator: mem.Allocator,
) -> string {
	kind := strings.to_lower(strings.trim_space(object_kind), allocator)
	switch {
	case kind == "global-class":
		return "class"
	case kind == "global-interface":
		return "interface"
	case kind == "function-module":
		return "function-module"
	case kind == TYPEPOOL_OBJECT_KIND:
		return "type-pool"
	case dependency_object_kind_is_ddic(kind):
		return "type"
	}
	lower_type := strings.to_lower(object_type, allocator)
	switch {
	case strings.has_prefix(lower_type, "clas/"):
		return "class"
	case strings.has_prefix(lower_type, "intf/"):
		return "interface"
	case strings.has_prefix(lower_type, "fugr/"):
		return "function-module"
	}
	return kind
}

@(private)
dependency_summary_name :: proc(name: string, allocator: mem.Allocator) -> string {
	return strings.to_lower(strings.trim_space(name), allocator)
}

@(private)
dependency_interface_summary_add_export :: proc(
	summary: ^Dependency_Interface_Summary,
	kind, name, owner: string,
	allocator: mem.Allocator,
) {
	if kind == "" || name == "" {
		return
	}
	canonical_name := dependency_summary_name(name, allocator)
	canonical_owner := dependency_summary_name(owner, allocator)
	for existing in summary.exports {
		if existing.kind == kind && existing.name == canonical_name && existing.owner == canonical_owner {
			return
		}
	}
	append(&summary.exports, Dependency_Interface_Export_Summary {
		kind  = strings.clone(kind, allocator),
		name  = canonical_name,
		owner = canonical_owner,
	})
}

@(private)
dependency_summary_append_unique_string :: proc(
	values: ^[dynamic]string,
	value: string,
	allocator: mem.Allocator,
) {
	if value == "" {
		return
	}
	canonical := dependency_summary_name(value, allocator)
	for existing in values^ {
		if existing == canonical {
			return
		}
	}
	append(values, canonical)
}

@(private)
dependency_summary_export_name_exists :: proc(
	summary: ^Dependency_Interface_Summary,
	name: string,
) -> bool {
	for export in summary.exports {
		if export.name == name {
			return true
		}
	}
	return false
}

@(private)
dependency_summary_type_index :: proc(summary: ^Dependency_Interface_Summary, name: string) -> int {
	for typ, i in summary.types {
		if typ.name == name {
			return i
		}
	}
	return -1
}

@(private)
dependency_summary_export_kind_from_symbol :: proc(kind: analyze.Symbol_Kind) -> string {
	#partial switch kind {
	case .Class:
		return "class"
	case .Interface:
		return "interface"
	case .Type_Def:
		return "type"
	case .Constant:
		return "constant"
	case .Variable:
		return "variable"
	case .Report:
		return "report"
	case .Include:
		return "include"
	case .Module:
		return "function-module"
	case:
		return ""
	}
}

@(private)
dependency_summary_member_kind :: proc(
	info: ^analyze.Decl_Info_Data,
	symbol_kind: analyze.Symbol_Kind,
) -> string {
	if symbol_kind == .Alias {
		return "alias"
	}
	switch info.member_kind {
	case .Attribute:
		return "attribute"
	case .Method:
		return "method"
	case .Event:
		return "event"
	}
	return ""
}

@(private)
dependency_summary_visibility_text :: proc(visibility: analyze.Visibility) -> string {
	switch visibility {
	case .Public:
		return "public"
	case .Protected:
		return "protected"
	case .Private:
		return "private"
	}
	return ""
}

@(private)
dependency_summary_namespace_text :: proc(namespace: analyze.Namespace) -> string {
	switch namespace {
	case .Value:
		return "value"
	case .Type:
		return "type"
	case .Routine:
		return "routine"
	}
	return ""
}

@(private)
dependency_summary_decl_section_text :: proc(section: analyze.Decl_Parameter_Section) -> string {
	switch section {
	case .Method_Importing:
		return "method-importing"
	case .Method_Exporting:
		return "method-exporting"
	case .Method_Changing:
		return "method-changing"
	case .Method_Receiving:
		return "method-receiving"
	case .Method_Returning:
		return "method-returning"
	case .Form_Tables:
		return "form-tables"
	case .Form_Using:
		return "form-using"
	case .Form_Changing:
		return "form-changing"
	case .Function_Importing:
		return "function-importing"
	case .Function_Exporting:
		return "function-exporting"
	case .Function_Changing:
		return "function-changing"
	case .Function_Tables:
		return "function-tables"
	case .None:
		return ""
	}
	return ""
}

@(private)
dependency_summary_decl_passing_text :: proc(passing: analyze.Decl_Parameter_Passing) -> string {
	switch passing {
	case .Direct:
		return "direct"
	case .Value:
		return "value"
	case .Reference:
		return "reference"
	case .None:
		return ""
	}
	return ""
}

@(private)
dependency_summary_data_type_form_text :: proc(form: ast.Data_Type_Form) -> string {
	switch form {
	case .Type:
		return "type"
	case .Like:
		return "like"
	case .Structure:
		return "structure"
	case .Ref_To:
		return "ref-to"
	case .Like_Line_Of:
		return "like-line-of"
	case .Type_Line_Of:
		return "type-line-of"
	case .Any_Table:
		return "any-table"
	case .Table:
		return "table"
	case .Like_Table:
		return "like-table"
	case .Index_Table:
		return "index-table"
	case .Standard_Table:
		return "standard-table"
	case .Sorted_Table:
		return "sorted-table"
	case .Hashed_Table:
		return "hashed-table"
	case .Like_Standard_Table:
		return "like-standard-table"
	case .Like_Sorted_Table:
		return "like-sorted-table"
	case .Like_Hashed_Table:
		return "like-hashed-table"
	case .Range_Of:
		return "range-of"
	}
	return ""
}

@(private)
dependency_summary_type_form_is_table :: proc(form: ast.Data_Type_Form) -> bool {
	#partial switch form {
	case .Any_Table,
	     .Table,
	     .Like_Table,
	     .Index_Table,
	     .Standard_Table,
	     .Sorted_Table,
	     .Hashed_Table,
	     .Like_Standard_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table:
		return true
	}
	return false
}

@(private)
dependency_summary_bool_text :: #force_inline proc "contextless" (value: bool) -> string {
	return "1" if value else "0"
}

@(private)
dependency_summary_parse_bool :: #force_inline proc(value: string) -> bool {
	return value == "1" || strings.equal_fold(value, "true")
}

@(private)
dependency_summary_write_record :: proc(out: ^strings.Builder, fields: []string) {
	for field, i in fields {
		if i > 0 {
			strings.write_byte(out, '\t')
		}
		dependency_summary_write_escaped_field(out, field)
	}
	strings.write_byte(out, '\n')
}

@(private)
dependency_summary_write_escaped_field :: proc(out: ^strings.Builder, value: string) {
	hex := "0123456789ABCDEF"
	for ch in value {
		switch ch {
		case '%', '\t', '\n', '\r':
			strings.write_byte(out, '%')
			strings.write_byte(out, hex[int(ch) >> 4])
			strings.write_byte(out, hex[int(ch) & 0xf])
		case:
			strings.write_rune(out, ch)
		}
	}
}

@(private)
dependency_summary_payload_fields :: proc(
	line: string,
	allocator: mem.Allocator,
) -> [dynamic]string {
	fields := make([dynamic]string, 0, 8, allocator)
	start := 0
	for i := 0; i <= len(line); i += 1 {
		if i < len(line) && line[i] != '\t' {
			continue
		}
		append(&fields, dependency_summary_unescape_field(line[start:i], allocator))
		start = i + 1
	}
	return fields
}

@(private)
dependency_summary_unescape_field :: proc(value: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	i := 0
	for i < len(value) {
		if value[i] == '%' && i + 2 < len(value) {
			hi, hi_ok := dependency_summary_hex_digit(value[i + 1])
			lo, lo_ok := dependency_summary_hex_digit(value[i + 2])
			if hi_ok && lo_ok {
				strings.write_byte(&out, byte((hi << 4) | lo))
				i += 3
				continue
			}
		}
		strings.write_byte(&out, value[i])
		i += 1
	}
	return strings.to_string(out)
}

@(private)
dependency_summary_hex_digit :: proc "contextless" (value: byte) -> (int, bool) {
	switch value {
	case '0'..='9':
		return int(value - '0'), true
	case 'a'..='f':
		return int(value - 'a') + 10, true
	case 'A'..='F':
		return int(value - 'A') + 10, true
	}
	return 0, false
}

@(private)
dependency_interface_summary_apply_payload_fields :: proc(
	summary: ^Dependency_Interface_Summary,
	fields: []string,
	allocator: mem.Allocator,
) {
	switch fields[0] {
	case "object":
		if len(fields) >= 7 {
			summary.object = Dependency_Interface_Object_Summary {
				kind           = strings.clone(fields[1], allocator),
				name           = strings.clone(fields[2], allocator),
				object_kind    = strings.clone(fields[3], allocator),
				object_type    = strings.clone(fields[4], allocator),
				object_uri     = strings.clone(fields[5], allocator),
				file_extension = strings.clone(fields[6], allocator),
			}
		}
	case "provided":
		if len(fields) >= 2 {
			dependency_summary_append_unique_string(&summary.provided_names, fields[1], allocator)
		}
	case "export":
		if len(fields) >= 4 {
			dependency_interface_summary_add_export(summary, fields[1], fields[2], fields[3], allocator)
		}
	case "class":
		if len(fields) >= 6 {
			append(&summary.classes, Dependency_Interface_Class_Summary {
				kind                   = strings.clone(fields[1], allocator),
				name                   = strings.clone(fields[2], allocator),
				signature              = strings.clone(fields[3], allocator),
				superclass_name        = strings.clone(fields[4], allocator),
				implemented_interfaces = make([dynamic]string, 0, 2, allocator),
				friends                = make([dynamic]string, 0, 2, allocator),
				is_abstract            = dependency_summary_parse_bool(fields[5]),
			})
		}
	case "implements":
		if len(fields) >= 3 {
			if index := dependency_summary_class_index(summary, fields[1]); index >= 0 {
				dependency_summary_append_unique_string(&summary.classes[index].implemented_interfaces, fields[2], allocator)
			}
		}
	case "friend":
		if len(fields) >= 3 {
			if index := dependency_summary_class_index(summary, fields[1]); index >= 0 {
				dependency_summary_append_unique_string(&summary.classes[index].friends, fields[2], allocator)
			}
		}
	case "member":
		if len(fields) >= 17 {
			append(&summary.members, Dependency_Interface_Class_Member_Summary {
				owner           = strings.clone(fields[1], allocator),
				kind            = strings.clone(fields[2], allocator),
				visibility      = strings.clone(fields[3], allocator),
				is_static       = dependency_summary_parse_bool(fields[4]),
				is_redefinition = dependency_summary_parse_bool(fields[5]),
				name            = strings.clone(fields[6], allocator),
				signature       = strings.clone(fields[7], allocator),
				type_ref        = dependency_summary_payload_type_ref(fields[8], fields[9], fields[10], "", "", allocator),
				alias_interface = strings.clone(fields[11], allocator),
				alias_member    = strings.clone(fields[12], allocator),
				event_name      = strings.clone(fields[13], allocator),
				event_source    = dependency_summary_payload_type_ref(fields[14], fields[15], "0", "", "", allocator),
				for_event       = dependency_summary_parse_bool(fields[16]),
				parameters      = make([dynamic]Dependency_Interface_Parameter_Summary, 0, 2, allocator),
				exceptions      = make([dynamic]Dependency_Interface_Exception_Summary, 0, 1, allocator),
			})
		}
	case "member-param":
		if len(fields) >= 13 {
			if index := dependency_summary_member_index(summary, fields[1], fields[2]); index >= 0 {
				append(&summary.members[index].parameters, dependency_summary_payload_parameter(fields, allocator))
			}
		}
	case "member-exception":
		if len(fields) >= 4 {
			if index := dependency_summary_member_index(summary, fields[1], fields[2]); index >= 0 {
				append(&summary.members[index].exceptions, Dependency_Interface_Exception_Summary {
					name = strings.clone(fields[3], allocator),
				})
			}
		}
	case "function":
		if len(fields) >= 3 {
			append(&summary.functions, Dependency_Interface_Function_Summary {
				name       = strings.clone(fields[1], allocator),
				signature  = strings.clone(fields[2], allocator),
				parameters = make([dynamic]Dependency_Interface_Parameter_Summary, 0, 2, allocator),
				exceptions = make([dynamic]Dependency_Interface_Exception_Summary, 0, 1, allocator),
			})
		}
	case "function-param":
		if len(fields) >= 13 {
			if index := dependency_summary_function_index(summary, fields[1]); index >= 0 {
				append(&summary.functions[index].parameters, dependency_summary_payload_parameter(fields, allocator))
			}
		}
	case "function-exception":
		if len(fields) >= 3 {
			if index := dependency_summary_function_index(summary, fields[1]); index >= 0 {
				append(&summary.functions[index].exceptions, Dependency_Interface_Exception_Summary {
					name = strings.clone(fields[2], allocator),
				})
			}
		}
	case "type":
		if len(fields) >= 8 {
			append(&summary.types, Dependency_Interface_Type_Summary {
				name       = strings.clone(fields[1], allocator),
				shape_kind = strings.clone(fields[2], allocator),
				type_ref   = dependency_summary_payload_type_ref(fields[3], fields[4], fields[5], fields[6], fields[7], allocator),
				fields     = make([dynamic]Dependency_Interface_Type_Field_Summary, 0, 4, allocator),
			})
		}
	case "type-field":
		if len(fields) >= 9 {
			if index := dependency_summary_type_index(summary, fields[1]); index >= 0 {
				append(&summary.types[index].fields, Dependency_Interface_Type_Field_Summary {
					name        = strings.clone(fields[2], allocator),
					type_ref    = dependency_summary_payload_type_ref(fields[3], fields[4], fields[5], "", "", allocator),
					is_key      = dependency_summary_parse_bool(fields[6]),
					is_include  = dependency_summary_parse_bool(fields[7]),
					description = strings.clone(fields[8], allocator),
				})
			}
		}
	case "typepool":
		if len(fields) >= 2 {
			summary.type_pool.name = strings.clone(fields[1], allocator)
			if len(summary.type_pool.symbols) == 0 {
				summary.type_pool.symbols = make([dynamic]string, 0, 8, allocator)
			}
		}
	case "typepool-symbol":
		if len(fields) >= 2 {
			if len(summary.type_pool.symbols) == 0 {
				summary.type_pool.symbols = make([dynamic]string, 0, 8, allocator)
			}
			dependency_summary_append_unique_string(&summary.type_pool.symbols, fields[1], allocator)
		}
	}
}

@(private)
dependency_summary_payload_type_ref :: proc(
	display, base_name, is_ref, form, table_has_of: string,
	allocator: mem.Allocator,
) -> Dependency_Interface_Type_Ref_Summary {
	return Dependency_Interface_Type_Ref_Summary {
		is_ref       = dependency_summary_parse_bool(is_ref),
		base_name    = strings.clone(base_name, allocator),
		display      = strings.clone(display, allocator),
		form         = strings.clone(form, allocator),
		table_has_of = dependency_summary_parse_bool(table_has_of),
		field_path   = make([dynamic]string, 0, 0, allocator),
	}
}

@(private)
dependency_summary_payload_parameter :: proc(
	fields: []string,
	allocator: mem.Allocator,
) -> Dependency_Interface_Parameter_Summary {
	return Dependency_Interface_Parameter_Summary {
		section     = strings.clone(fields[3], allocator),
		passing     = strings.clone(fields[4], allocator),
		name        = strings.clone(fields[5], allocator),
		type_ref    = dependency_summary_payload_type_ref(fields[6], fields[7], fields[8], fields[11], fields[12], allocator),
		is_optional = dependency_summary_parse_bool(fields[9]),
		has_default = dependency_summary_parse_bool(fields[10]),
	}
}

@(private)
dependency_summary_class_index :: proc(summary: ^Dependency_Interface_Summary, name: string) -> int {
	for class, i in summary.classes {
		if class.name == name {
			return i
		}
	}
	return -1
}

@(private)
dependency_summary_member_index :: proc(summary: ^Dependency_Interface_Summary, owner, name: string) -> int {
	for member, i in summary.members {
		if member.owner == owner && member.name == name {
			return i
		}
	}
	return -1
}

@(private)
dependency_summary_function_index :: proc(summary: ^Dependency_Interface_Summary, name: string) -> int {
	for function, i in summary.functions {
		if function.name == name {
			return i
		}
	}
	return -1
}
