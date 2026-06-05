package abap_frontend_semantic_remote_dependencies

import analyze "src:semantic/analyze"
import deps "src:semantic/dependencies"

import base_runtime "base:runtime"
import "core:mem"
import "core:strings"

dependency_summary_input_from_payload :: proc(
	payload: string,
	candidate: deps.Remote_Dependency_Candidate,
	uri: string,
	allocator: mem.Allocator,
) -> (analyze.Summary_Provider_Input, bool) {
	scratch_arena := dependency_summary_scratch_arena_make()
	defer mem.dynamic_arena_destroy(&scratch_arena)
	summary, ok := dependency_interface_summary_from_payload(
		payload,
		mem.dynamic_arena_allocator(&scratch_arena),
	)
	if !ok || !dependency_interface_summary_satisfies_candidate(&summary, candidate) {
		return {}, false
	}
	return dependency_summary_input_from_interface_summary(&summary, payload, uri, allocator), true
}

dependency_summary_payload_satisfies_candidate :: proc(
	payload: string,
	candidate: deps.Remote_Dependency_Candidate,
) -> bool {
	scratch_arena := dependency_summary_scratch_arena_make()
	defer mem.dynamic_arena_destroy(&scratch_arena)
	summary, ok := dependency_interface_summary_from_payload(
		payload,
		mem.dynamic_arena_allocator(&scratch_arena),
	)
	return ok && dependency_interface_summary_satisfies_candidate(&summary, candidate)
}

@(private)
dependency_summary_scratch_arena_make :: proc() -> mem.Dynamic_Arena {
	arena: mem.Dynamic_Arena
	backing := base_runtime.heap_allocator()
	mem.dynamic_arena_init(&arena, backing, backing, alignment = 64)
	return arena
}

dependency_interface_summary_satisfies_candidate :: proc(
	summary: ^Dependency_Interface_Summary,
	candidate: deps.Remote_Dependency_Candidate,
) -> bool {
	if summary == nil || candidate.name == "" {
		return false
	}
	name := strings.to_lower(candidate.name, context.temp_allocator)
	#partial switch candidate.kind {
	case .Type, .Static:
		return dependency_interface_summary_has_export(summary, name, "class") ||
		       dependency_interface_summary_has_export(summary, name, "interface") ||
		       dependency_interface_summary_has_export(summary, name, "type")
	case .Symbol:
		return dependency_interface_summary_has_export(summary, name, "constant") ||
		       dependency_interface_summary_has_export(summary, name, "variable") ||
		       dependency_interface_summary_has_export(summary, name, "type")
	case .Function:
		return dependency_interface_summary_has_export(summary, name, "function-module")
	}
	return false
}

dependency_summary_input_from_interface_summary :: proc(
	summary: ^Dependency_Interface_Summary,
	payload: string,
	uri: string,
	allocator: mem.Allocator,
) -> analyze.Summary_Provider_Input {
	out := analyze.dependency_summary_input_make(allocator)
	out.uri = strings.clone(uri if uri != "" else dependency_summary_provider_uri(summary, allocator), allocator)
	out.payload = strings.clone(payload, allocator)
	out.object_kind = strings.clone(summary.object.kind, allocator)
	out.object_name = strings.clone(summary.object.name, allocator)
	for name in summary.provided_names {
		append(&out.provided_names, strings.clone(name, allocator))
	}
	for export in summary.exports {
		append(
			&out.exports,
			analyze.Summary_Provider_Export_Input {
				kind  = strings.clone(export.kind, allocator),
				name  = strings.clone(export.name, allocator),
				owner = strings.clone(export.owner, allocator),
			},
		)
	}
	for class in summary.classes {
		next := analyze.Summary_Provider_Class_Input {
			name                   = strings.clone(class.name, allocator),
			kind                   = strings.clone(class.kind, allocator),
			signature              = strings.clone(class.signature, allocator),
			superclass_name        = strings.clone(class.superclass_name, allocator),
			implemented_interfaces = make([dynamic]string, 0, len(class.implemented_interfaces), allocator),
			friends                = make([dynamic]string, 0, len(class.friends), allocator),
			is_abstract            = class.is_abstract,
		}
		for interface_name in class.implemented_interfaces {
			append(&next.implemented_interfaces, strings.clone(interface_name, allocator))
		}
		for friend in class.friends {
			append(&next.friends, strings.clone(friend, allocator))
		}
		append(&out.classes, next)
	}
	for member in summary.members {
		next := analyze.Summary_Provider_Class_Member_Input {
			owner           = strings.clone(member.owner, allocator),
			name            = strings.clone(member.name, allocator),
			kind            = strings.clone(member.kind, allocator),
			visibility      = strings.clone(member.visibility, allocator),
			signature       = strings.clone(member.signature, allocator),
			type_ref        = dependency_summary_type_ref_input(member.type_ref, allocator),
			alias_interface = strings.clone(member.alias_interface, allocator),
			alias_member    = strings.clone(member.alias_member, allocator),
			event_name      = strings.clone(member.event_name, allocator),
			event_source    = dependency_summary_type_ref_input(member.event_source, allocator),
			parameters      = make([dynamic]analyze.Summary_Provider_Parameter_Input, 0, len(member.parameters), allocator),
			exceptions      = make([dynamic]analyze.Summary_Provider_Exception_Input, 0, len(member.exceptions), allocator),
			is_static       = member.is_static,
			is_redefinition = member.is_redefinition,
			for_event       = member.for_event,
		}
		for param in member.parameters {
			append(&next.parameters, dependency_summary_parameter_input(param, allocator))
		}
		for exception in member.exceptions {
			append(&next.exceptions, analyze.Summary_Provider_Exception_Input {
				name = strings.clone(exception.name, allocator),
			})
		}
		append(&out.members, next)
	}
	for function in summary.functions {
		next := analyze.Summary_Provider_Function_Input {
			name       = strings.clone(function.name, allocator),
			signature  = strings.clone(function.signature, allocator),
			parameters = make([dynamic]analyze.Summary_Provider_Parameter_Input, 0, len(function.parameters), allocator),
			exceptions = make([dynamic]analyze.Summary_Provider_Exception_Input, 0, len(function.exceptions), allocator),
		}
		for param in function.parameters {
			append(&next.parameters, dependency_summary_parameter_input(param, allocator))
		}
		for exception in function.exceptions {
			append(&next.exceptions, analyze.Summary_Provider_Exception_Input {
				name = strings.clone(exception.name, allocator),
			})
		}
		append(&out.functions, next)
	}
	for typ in summary.types {
		next := analyze.Summary_Provider_Type_Input {
			name       = strings.clone(typ.name, allocator),
			shape_kind = strings.clone(typ.shape_kind, allocator),
			type_ref   = dependency_summary_type_ref_input(typ.type_ref, allocator),
			fields     = make([dynamic]analyze.Summary_Provider_Type_Field_Input, 0, len(typ.fields), allocator),
		}
		for field in typ.fields {
			append(
				&next.fields,
				analyze.Summary_Provider_Type_Field_Input {
					name        = strings.clone(field.name, allocator),
					type_ref    = dependency_summary_type_ref_input(field.type_ref, allocator),
					description = strings.clone(field.description, allocator),
					is_key      = field.is_key,
					is_include  = field.is_include,
				},
			)
		}
		append(&out.types, next)
	}
	out.type_pool_name = strings.clone(summary.type_pool.name, allocator)
	for symbol in summary.type_pool.symbols {
		append(&out.type_pool_symbols, strings.clone(symbol, allocator))
	}
	return out
}

dependency_summary_provider_uri :: proc(
	summary: ^Dependency_Interface_Summary,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-summary:/")
	strings.write_string(&out, summary.object.kind if summary.object.kind != "" else "dependency")
	strings.write_byte(&out, '/')
	strings.write_string(&out, summary.object.name if summary.object.name != "" else "anonymous")
	return strings.to_string(out)
}

dependency_summary_type_ref_input :: proc(
	ref: Dependency_Interface_Type_Ref_Summary,
	allocator: mem.Allocator,
) -> analyze.Summary_Provider_Type_Ref_Input {
	return analyze.Summary_Provider_Type_Ref_Input {
		namespace    = strings.clone(ref.namespace, allocator),
		is_ref       = ref.is_ref,
		base_name    = strings.clone(ref.base_name, allocator),
		display      = strings.clone(ref.display, allocator),
		form         = strings.clone(ref.form, allocator),
		table_has_of = ref.table_has_of,
	}
}

dependency_summary_parameter_input :: proc(
	param: Dependency_Interface_Parameter_Summary,
	allocator: mem.Allocator,
) -> analyze.Summary_Provider_Parameter_Input {
	return analyze.Summary_Provider_Parameter_Input {
		name        = strings.clone(param.name, allocator),
		section     = strings.clone(param.section, allocator),
		passing     = strings.clone(param.passing, allocator),
		type_ref    = dependency_summary_type_ref_input(param.type_ref, allocator),
		is_optional = param.is_optional,
		has_default = param.has_default,
	}
}

@(private)
dependency_interface_summary_has_export :: proc(
	summary: ^Dependency_Interface_Summary,
	name: string,
	kind: string,
) -> bool {
	for export in summary.exports {
		if export.name == name && export.kind == kind {
			return true
		}
	}
	return false
}
