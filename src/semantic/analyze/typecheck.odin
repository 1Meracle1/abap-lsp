#+private
package abap_frontend_semantic_analyze

import "src:tokenizer"
import "src:ast"

import "core:mem"
import "core:slice"
import "core:strings"

Typecheck_Call_Signature :: struct {
	info:       ^Decl_Info_Data,
	unit_index: int,
	direct:     bool,
}

Typecheck_Ref_Target_Kind :: enum {
	Data,
	Data_Generic,
	Object_Generic,
	Class,
	Interface,
}

Typecheck_Ref_Target :: struct {
	kind:   Typecheck_Ref_Target_Kind,
	name:   string,
	handle: Symbol_Handle,
}

Typecheck_Scalar_Group :: enum {
	Unknown,
	Numeric,
	Character,
	Byte,
	Date,
	Time,
	Generic_Simple,
}

Typecheck_Writable_Index :: struct {
	ranges: [dynamic]tokenizer.Range,
}

Typecheck_Call_Range :: struct {
	range: tokenizer.Range,
	index: int,
}

Typecheck_Call_Index :: struct {
	calls: [dynamic]Typecheck_Call_Range,
}

validate_typecheck_diagnostics :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	if project.units[unit_index].source_mode != .Full {
		return
	}
	call_index := typecheck_call_index_make(&project.units[unit_index], context.temp_allocator)
	typecheck_assignments(project, lookup, unit_index, &call_index, out, seen, allocator)
	typecheck_calls(project, lookup, unit_index, &call_index, out, seen, allocator)
	typecheck_open_sql_targets(project, lookup, unit_index, out, seen, allocator)
}

typecheck_assignments :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	call_index: ^Typecheck_Call_Index,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for &site in unit.assignment_sites {
		if !typecheck_assignment_has_high_confidence(site) {
			continue
		}
		rhs := site.rhs
		if call_fact, ok, is_call := typecheck_assignment_exact_call_result_fact(
			project,
			lookup,
			unit_index,
			call_index,
			site,
		); is_call {
			if !ok {
				continue
			}
			rhs = call_fact
		}
		if ok, known := typecheck_assignment_compatible(
			project,
			lookup,
			unit_index,
			rhs,
			site.lhs,
			.Is_Downcast in site.flags,
		);
		   known && !ok {
			append_diag(
				out,
				seen,
				.Incompatible_Assignment_Type,
				site.rhs_range,
				typecheck_message_with_type_detail(
					typecheck_two_operand_message(
						unit,
						"The type of ",
						site.rhs_range,
						" cannot be converted to the type of ",
						site.lhs_range,
						allocator,
					),
					project,
					rhs,
					site.lhs,
					allocator,
				),
			)
		}
	}
}

typecheck_calls :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	call_index: ^Typecheck_Call_Index,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	if len(unit.call_sites) == 0 {
		return
	}
	facts := typecheck_fact_index_make(unit, context.temp_allocator)
	writable := typecheck_writable_index_make(unit, context.temp_allocator)
	for &site in unit.call_sites {
		signature, ok := typecheck_call_signature(project, lookup, unit_index, site)
		if !ok || signature.info == nil {
			continue
		}
		if !typecheck_signature_trusted_for_diagnostics(project, lookup, signature, site.target.kind) {
			continue
		}
		seen_args := make(map[string]bool, len(site.arguments), context.temp_allocator)
		required_mapping_ok := true
		for arg, arg_index in site.arguments {
			if arg.section == .Exceptions {
				continue
			}
			if arg.name != "" {
				key := typecheck_arg_key(arg, context.temp_allocator)
				if key in seen_args {
					append_diag(
						out,
						seen,
						.Duplicate_Named_Parameter,
						arg.range,
						typecheck_name_message("Duplicate formal parameter ", arg.name, allocator),
					)
					required_mapping_ok = false
					continue
				}
				seen_args[key] = true
			}
			param, param_ok := typecheck_call_parameter(signature.info, site.target.kind, site, arg_index)
			if !param_ok {
				required_mapping_ok = false
				if typecheck_signature_reports_parameter_names(signature.info) &&
				   typecheck_simple_parameter_name(arg.name) {
					append_diag(
						out,
						seen,
						.Unknown_Named_Parameter,
						arg.range,
						typecheck_name_message(
							"Formal parameter does not exist: ",
							arg.name,
							allocator,
						),
					)
				}
				continue
			}
			arg_mapping_ok := typecheck_argument_mapping_has_high_confidence(
				signature.info,
				site.target.kind,
				site,
				arg_index,
				param^,
			)
			if !arg_mapping_ok {
				required_mapping_ok = false
			}
			if typecheck_argument_requires_writable(site.target.kind, arg.section) &&
			   !typecheck_argument_is_writable(&writable, arg) {
				append_diag(
					out,
					seen,
					.Incompatible_Argument_Type,
					arg.value_range,
					typecheck_name_message(
						"The field cannot be modified: ",
						typecheck_range_text(unit, arg.value_range, context.temp_allocator),
						allocator,
					),
				)
				continue
			}
			actual := typecheck_argument_fact(&facts, arg)
			if call_fact, call_ok, is_call := typecheck_argument_call_result_fact(
				project,
				lookup,
				unit_index,
				call_index,
				arg,
			); is_call {
				if !call_ok {
					continue
				}
				actual = call_fact
			}
			formal := typecheck_parameter_fact(project, lookup, signature.unit_index, signature.info, param^)
			if !arg_mapping_ok ||
			   !type_fact_is_high_confidence(actual) ||
			   !type_fact_is_high_confidence(formal) {
				continue
			}
			if !typecheck_formal_requires_typecheck(project, formal) {
				continue
			}
			if compatible, known := typecheck_call_compatible(project, lookup, unit_index, actual, formal);
			   known && !compatible {
				append_diag(
					out,
					seen,
					.Incompatible_Argument_Type,
					arg.value_range,
					typecheck_message_with_type_detail(
						typecheck_two_name_message(
							typecheck_range_text(unit, arg.value_range, context.temp_allocator),
							" is not type-compatible with formal parameter ",
							param.name,
							allocator,
						),
						project,
						actual,
						formal,
						allocator,
					),
				)
			}
		}
		if required_mapping_ok &&
		   typecheck_required_signature_is_complete(signature, site.target.kind) {
			typecheck_required_parameters(project, unit_index, signature.info, site, out, seen, allocator)
		}
	}
}

typecheck_open_sql_targets :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	unit := &project.units[unit_index]
	for &target in unit.sql_targets {
		if .Is_Table in target.flags || .Is_Corresponding in target.flags || target.target_name == "" {
			continue
		}
		source, source_ok := typecheck_single_sql_projection_fact(project, lookup, unit_index, target)
		if !source_ok {
			continue
		}
		handle, handle_ok := value_handle_for_site(
			project,
			lookup,
			unit_index,
			target.scope,
			target.target_range,
			target.target_name,
		)
		if !handle_ok {
			continue
		}
		target_fact := type_fact_from_symbol_handle(project, unit_index, handle)
		if !type_fact_is_high_confidence(source) || !type_fact_is_high_confidence(target_fact) {
			continue
		}
		if ok, known := typecheck_sql_target_compatible(project, source, target_fact);
		   known && !ok {
			append_diag(
				out,
				seen,
				.Invalid_Open_Sql_Into_Target,
				target.target_range,
				typecheck_message_with_type_detail(
					typecheck_name_message("Open SQL target is not compatible: ", target.target_name, allocator),
					project,
					source,
					target_fact,
					allocator,
				),
			)
		}
	}
}

typecheck_assignment_has_high_confidence :: proc(site: Assignment_Site_Data) -> bool {
	if .Is_Corresponding in site.flags {
		return false
	}
	if !type_fact_is_high_confidence(site.lhs) || !type_fact_is_high_confidence(site.rhs) {
		return false
	}
	return true
}

typecheck_sql_target_compatible :: proc(
	project: ^Project_Analysis,
	source, target: Type_Fact_Data,
) -> (bool, bool) {
	source_name, source_ok := typecheck_builtin_name(project, source)
	target_name, target_ok := typecheck_builtin_name(project, target)
	if !source_ok || !target_ok {
		return false, false
	}
	return typecheck_scalar_assignment_conversion(source_name, target_name)
}

typecheck_assignment_exact_call_result_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	call_index: ^Typecheck_Call_Index,
	site: Assignment_Site_Data,
) -> (Type_Fact_Data, bool, bool) {
	unit := &project.units[unit_index]
	if call, ok := typecheck_exact_call_for_range(unit, call_index, site.rhs_range); ok {
		fact := call_result_type_fact(project, lookup, unit_index, call)
		return fact, type_fact_is_high_confidence(fact), true
	}
	return unknown_type_fact(), false, false
}

typecheck_argument_call_result_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	call_index: ^Typecheck_Call_Index,
	arg: Call_Argument_Data,
) -> (Type_Fact_Data, bool, bool) {
	unit := &project.units[unit_index]
	if call, ok := typecheck_first_contained_call_for_range(unit, call_index, arg.value_range); ok {
		if !typecheck_range_equal(call.range, arg.value_range) {
			return unknown_type_fact(), false, true
		}
		fact := call_result_type_fact(project, lookup, unit_index, call)
		return fact, type_fact_is_high_confidence(fact), true
	}
	return unknown_type_fact(), false, false
}

typecheck_signature_trusted_for_diagnostics :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	signature: Typecheck_Call_Signature,
	target_kind: Named_Argument_Target_Kind,
) -> bool {
	if signature.unit_index < 0 ||
	   signature.unit_index >= len(project.units) ||
	   signature.info == nil {
		return false
	}
	if project.units[signature.unit_index].source_mode == .Full {
		return true
	}
	return typecheck_external_signature_complete(project, lookup, signature, target_kind)
}

typecheck_external_signature_complete :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	signature: Typecheck_Call_Signature,
	target_kind: Named_Argument_Target_Kind,
) -> bool {
	info := signature.info
	if project.units[signature.unit_index].source_mode != .Dependency_Interface {
		return false
	}
	if target_kind == .Function {
		if info.kind != .Module {
			return false
		}
	} else if target_kind == .Method || target_kind == .Implicit_Method {
		if info.member_kind != .Method {
			return false
		}
	} else {
		return false
	}
	for &param in info.signature_parameters {
		if param.name == "" ||
		   !typecheck_parameter_section_complete(target_kind, param.section) ||
		   !typecheck_parameter_passing_complete(param.passing) ||
		   !(.Has_Declared_Type in param.flags) ||
		   param.declared_type.base_name == "" ||
		   !type_fact_is_high_confidence(typecheck_parameter_fact(project, lookup, signature.unit_index, info, param)) {
			return false
		}
	}
	return true
}

typecheck_required_signature_is_complete :: proc(
	signature: Typecheck_Call_Signature,
	target_kind: Named_Argument_Target_Kind,
) -> bool {
	if signature.info == nil {
		return false
	}
	info := signature.info
	if .Is_Redefinition in info.flags || .For_Event in info.flags || info.kind == .Alias {
		return false
	}
	if target_kind == .Function {
		if info.kind != .Module {
			return false
		}
		for param in info.signature_parameters {
			if param.name == "" || !typecheck_function_parameter_section_complete(param.section) {
				return false
			}
		}
		return true
	}
	if target_kind != .Method && target_kind != .Implicit_Method {
		return false
	}
	if !signature.direct || info.member_kind != .Method {
		return false
	}
	for &param in info.signature_parameters {
		if param.name == "" || !typecheck_method_parameter_section_complete(param.section) {
			return false
		}
	}
	return true
}

typecheck_parameter_section_complete :: proc(
	target_kind: Named_Argument_Target_Kind,
	section: Decl_Parameter_Section,
) -> bool {
	return typecheck_function_parameter_section_complete(section) if target_kind == .Function else typecheck_method_parameter_section_complete(section)
}

typecheck_method_parameter_section_complete :: proc(section: Decl_Parameter_Section) -> bool {
	#partial switch section {
	case .Method_Importing, .Method_Exporting, .Method_Changing, .Method_Receiving, .Method_Returning:
		return true
	}
	return false
}

typecheck_function_parameter_section_complete :: proc(section: Decl_Parameter_Section) -> bool {
	#partial switch section {
	case .Function_Importing, .Function_Exporting, .Function_Changing, .Function_Tables:
		return true
	}
	return false
}

typecheck_parameter_passing_complete :: proc(passing: Decl_Parameter_Passing) -> bool {
	#partial switch passing {
	case .Direct, .Value, .Reference:
		return true
	}
	return false
}

typecheck_argument_mapping_has_high_confidence :: proc(
	info: ^Decl_Info_Data,
	target_kind: Named_Argument_Target_Kind,
	site: Call_Site_Data,
	arg_index: int,
	param: Decl_Signature_Parameter_Data,
) -> bool {
	if site.arguments[arg_index].name != "" {
		return true
	}
	pos_param, ok := typecheck_positional_call_parameter(info, target_kind, site, arg_index)
	return ok && pos_param.name == param.name && pos_param.section == param.section
}

typecheck_call_signature :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	site: Call_Site_Data,
) -> (Typecheck_Call_Signature, bool) {
	#partial switch site.target.kind {
	case .Method:
		if site.target.method_name == "" {
			return {}, false
		}
		class_handle, ok := class_handle_for_call_target(project, lookup, unit_index, site)
		if !ok {
			return {}, false
		}
		member, member_unit_index, member_ok := class_member_in_hierarchy_with_unit(
			project,
			lookup,
			class_handle,
			site.target.method_name,
			false,
			unit_index,
			site.scope,
		)
		if !member_ok {
			return {}, false
		}
		direct, direct_ok := class_member_handle_lookup(project, lookup, class_handle, site.target.method_name)
		member_unit_index = unit_id_index(member.unit)
		if member_unit_index < 0 || member_unit_index >= len(project.units) {
			return {}, false
		}
		info := entity_decl_info(&project.units[member_unit_index], member.symbol)
		return Typecheck_Call_Signature {
			info = info,
			unit_index = member_unit_index,
			direct = direct_ok && direct.unit == member.unit && direct.symbol == member.symbol,
		}, info != nil
	case .Implicit_Method:
		class_symbol, ok := enclosing_class_owner_unit(&project.units[unit_index], site.scope)
		if !ok {
			return {}, false
		}
		class_handle := Symbol_Handle{unit = project.units[unit_index].unit_id, symbol = class_symbol}
		member, member_unit_index, member_ok := class_member_in_hierarchy_with_unit(
			project,
			lookup,
			class_handle,
			site.target.method_name,
			false,
			unit_index,
			site.scope,
		)
		if !member_ok {
			return {}, false
		}
		direct, direct_ok := class_member_handle_lookup(project, lookup, class_handle, site.target.method_name)
		member_unit_index = unit_id_index(member.unit)
		if member_unit_index < 0 || member_unit_index >= len(project.units) {
			return {}, false
		}
		info := entity_decl_info(&project.units[member_unit_index], member.symbol)
		return Typecheck_Call_Signature {
			info = info,
			unit_index = member_unit_index,
			direct = direct_ok && direct.unit == member.unit && direct.symbol == member.symbol,
		}, info != nil
	case .Function:
		member, ok := resolve_function_module_in_project_lookup(
			project,
			lookup,
			unit_index,
			site.target.function_name,
		)
		if !ok {
			return {}, false
		}
		info := entity_decl_info(&project.units[unit_id_index(member.unit)], member.symbol)
		return Typecheck_Call_Signature {
			info = info,
			unit_index = unit_id_index(member.unit),
			direct = true,
		}, info != nil
	case:
	}
	return {}, false
}

typecheck_call_parameter :: proc(
	info: ^Decl_Info_Data,
	target_kind: Named_Argument_Target_Kind,
	site: Call_Site_Data,
	arg_index: int,
) -> (^Decl_Signature_Parameter_Data, bool) {
	if info == nil {
		return nil, false
	}
	arg := site.arguments[arg_index]
	if arg.name == "" {
		return typecheck_positional_call_parameter(info, target_kind, site, arg_index)
	}
	for &param in info.signature_parameters {
		if !strings.equal_fold(param.name, arg.name) {
			continue
		}
		if typecheck_parameter_section_matches(target_kind, param.section, arg.section) {
			return &param, true
		}
	}
	return nil, false
}

typecheck_positional_call_parameter :: proc(
	info: ^Decl_Info_Data,
	target_kind: Named_Argument_Target_Kind,
	site: Call_Site_Data,
	arg_index: int,
) -> (^Decl_Signature_Parameter_Data, bool) {
	if info == nil || .Is_Redefinition in info.flags || info.kind == .Alias {
		return nil, false
	}
	arg := site.arguments[arg_index]
	if !arg.has_section {
		if len(site.arguments) != 1 {
			return nil, false
		}
		return typecheck_short_positional_call_parameter(info, target_kind, arg.section)
	}
	position := 0
	for i in 0 ..< arg_index {
		prev := site.arguments[i]
		if prev.name == "" && prev.section == arg.section {
			position += 1
		}
	}
	for &param in info.signature_parameters {
		if !typecheck_parameter_section_matches(target_kind, param.section, arg.section) {
			continue
		}
		if position == 0 {
			return &param, true
		}
		position -= 1
	}
	return nil, false
}

typecheck_short_positional_call_parameter :: proc(
	info: ^Decl_Info_Data,
	target_kind: Named_Argument_Target_Kind,
	section: Named_Argument_Section,
) -> (^Decl_Signature_Parameter_Data, bool) {
	if target_kind != .Method && target_kind != .Implicit_Method && target_kind != .Constructor {
		return nil, false
	}
	candidates := 0
	result: ^Decl_Signature_Parameter_Data
	for &param in info.signature_parameters {
		if !typecheck_parameter_section_matches(target_kind, param.section, section) {
			continue
		}
		candidates += 1
		result = &param
	}
	return result, candidates == 1
}

typecheck_parameter_section_matches :: proc(
	target_kind: Named_Argument_Target_Kind,
	param: Decl_Parameter_Section,
	arg: Named_Argument_Section,
) -> bool {
	if target_kind == .Function {
		#partial switch arg {
		case .Exporting:
			return param == .Function_Importing
		case .Importing:
			return param == .Function_Exporting
		case .Changing:
			return param == .Function_Changing
		case .Tables:
			return param == .Function_Tables
		}
		return false
	}
	#partial switch arg {
	case .Exporting:
		return param == .Method_Importing
	case .Importing:
		return param == .Method_Exporting
	case .Changing:
		return param == .Method_Changing
	case .Receiving:
		return param == .Method_Receiving || param == .Method_Returning
	}
	return false
}

typecheck_argument_requires_writable :: proc(
	target_kind: Named_Argument_Target_Kind,
	section: Named_Argument_Section,
) -> bool {
	_ = target_kind
	return section == .Importing || section == .Changing ||
	       section == .Receiving || section == .Tables
}

typecheck_argument_is_writable :: proc(index: ^Typecheck_Writable_Index, arg: Call_Argument_Data) -> bool {
	for i := typecheck_range_lower_bound(index.ranges[:], arg.value_range.start);
	    i < len(index.ranges) && index.ranges[i].start <= arg.value_range.end;
	    i += 1 {
		if typecheck_range_contains(arg.value_range, index.ranges[i]) {
			return true
		}
	}
	return false
}

typecheck_required_parameters :: proc(
	project: ^Project_Analysis,
	unit_index: int,
	info: ^Decl_Info_Data,
	site: Call_Site_Data,
	out: ^[dynamic]Diagnostic,
	seen: ^map[Diagnostic_Key]bool,
	allocator: mem.Allocator,
) {
	_ = project
	_ = unit_index
	for &param in info.signature_parameters {
		if !typecheck_parameter_is_required(site.target.kind, param) {
			continue
		}
		found := false
		for arg in site.arguments {
			if strings.equal_fold(arg.name, param.name) &&
			   typecheck_parameter_section_matches(site.target.kind, param.section, arg.section) {
				found = true
				break
			}
		}
		if !found {
			for arg, arg_index in site.arguments {
				if arg.name != "" {
					continue
				}
				pos_param, ok := typecheck_positional_call_parameter(info, site.target.kind, site, arg_index)
				if ok &&
				   strings.equal_fold(pos_param.name, param.name) &&
				   pos_param.section == param.section {
					found = true
					break
				}
			}
		}
		if !found {
			append_diag(
				out,
				seen,
				.Missing_Required_Parameter,
				site.range,
				typecheck_name_message("Missing required formal parameter: ", param.name, allocator),
			)
		}
	}
}

typecheck_parameter_is_required :: proc(
	target_kind: Named_Argument_Target_Kind,
	param: Decl_Signature_Parameter_Data,
) -> bool {
	if .Is_Optional in param.flags || .Has_Default_Value in param.flags {
		return false
	}
	if target_kind == .Function {
		return param.section == .Function_Importing ||
		       param.section == .Function_Changing ||
		       param.section == .Function_Tables
	}
	return param.section == .Method_Importing || param.section == .Method_Changing
}

typecheck_argument_fact :: proc(
	index: ^Range_Type_Fact_Index,
	arg: Call_Argument_Data,
) -> Type_Fact_Data {
	indexed := typecheck_fact_for_range_indexed(index, arg.value_range)
	if type_fact_known(indexed) {
		return indexed
	}
	return arg.type_fact
}

typecheck_formal_requires_typecheck :: proc(project: ^Project_Analysis, formal: Type_Fact_Data) -> bool {
	name, ok := typecheck_builtin_name(project, formal)
	return ok && is_generic_builtin_type_name(name)
}

typecheck_signature_reports_parameter_names :: proc(info: ^Decl_Info_Data) -> bool {
	return info != nil && !(.Is_Redefinition in info.flags)
}

typecheck_simple_parameter_name :: proc(name: string) -> bool {
	return name != "" &&
	       strings.index_byte(name, '-') < 0 &&
	       strings.index_byte(name, '>') < 0 &&
	       strings.index_byte(name, '~') < 0
}

typecheck_parameter_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	info: ^Decl_Info_Data,
	param: Decl_Signature_Parameter_Data,
) -> Type_Fact_Data {
	unit := &project.units[unit_index]
	fact := Type_Fact_Data {
		type_id = param.type_id,
		type_unit = unit.unit_id if type_id_is_known(param.type_id) else INVALID_UNIT_ID,
		structure = INVALID_STRUCTURE_ID,
		structure_unit = INVALID_UNIT_ID,
		declared_type = param.declared_type,
		has_declared_type = .Has_Declared_Type in param.flags,
		type_clause_display = param.type_clause_display,
		confidence = .High if unit.source_mode == .Full else .Low,
	}
	if type_fact_known(fact) {
		if !(.Has_Declared_Type in param.flags) {
			return fact
		}
	}
	if .Has_Declared_Type in param.flags {
		scope_id := info.signature_scope if info.signature_scope != INVALID_SCOPE_ID else info.scope
		if resolved, ok := typecheck_declared_field_fact(
			project,
			lookup,
			unit_index,
			scope_id,
			param.declared_type,
			param.type_clause_form,
			param.has_type_clause_form,
		); ok {
			return resolved
		}
		if resolved, _, ok := type_fact_from_declared_type(
			project,
			lookup,
			unit_index,
			scope_id,
			param.declared_type,
			param.type_clause_form,
			param.has_type_clause_form,
			0,
		); ok {
			return resolved
		}
	}
	return fact if type_fact_known(fact) else unknown_type_fact()
}

typecheck_declared_field_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	type_form: ast.Data_Type_Form,
	has_type_form: bool,
) -> (Type_Fact_Data, bool) {
	if len(type_ref.field_path) == 0 {
		return unknown_type_fact(), false
	}
	base_ref := Field_Type_Ref_Data {
		namespace = type_ref.namespace,
		is_ref = type_ref.is_ref,
		base_name = type_ref.base_name,
		base_range = type_ref.base_range,
	}
	base, _, ok := type_fact_from_declared_type(
		project,
		lookup,
		unit_index,
		scope_id,
		base_ref,
		type_form,
		has_type_form,
		0,
	)
	if !ok || base.structure == INVALID_STRUCTURE_ID {
		return unknown_type_fact(), false
	}
	base_unit_index := unit_id_index(base.structure_unit)
	if base_unit_index < 0 || base_unit_index >= len(project.units) {
		base_unit_index = unit_index
	}
	path := make([dynamic]Field_Access_Segment, 0, len(type_ref.field_path), context.temp_allocator)
	for name, i in type_ref.field_path {
		append(
			&path,
			Field_Access_Segment {
				name = name,
				range = type_ref.field_ranges[i] if i < len(type_ref.field_ranges) else tokenizer.Range{},
				selector = type_selector_at(type_ref.field_selectors[:], i),
				deref = type_ref.field_derefs[i] if i < len(type_ref.field_derefs) else false,
			},
		)
	}
	return type_fact_from_structure_path(
		project,
		lookup,
		unit_index,
		&project.units[base_unit_index],
		base.structure,
		path[:],
		base,
	)
}

typecheck_fact_index_make :: proc(
	unit: ^Unit_Analysis,
	allocator: mem.Allocator,
) -> Range_Type_Fact_Index {
	index := Range_Type_Fact_Index {
		facts = make(
			[dynamic]Range_Type_Fact,
			0,
			len(unit.operands) + len(unit.expression_facts),
			allocator,
		),
	}
	for &operand in unit.operands {
		if type_fact_known(operand.type_fact) {
			append(
				&index.facts,
				Range_Type_Fact{range = operand.range, type_fact = operand.type_fact, rank = 3},
			)
		}
	}
	for &fact in unit.expression_facts {
		if type_fact_known(fact.type_fact) {
			rank := 2 if fact.kind == .Selector else 1
			append(
				&index.facts,
				Range_Type_Fact{range = fact.range, type_fact = fact.type_fact, rank = rank},
			)
		}
	}
	slice.sort_by(index.facts[:], range_type_fact_less)
	return index
}

typecheck_writable_index_make :: proc(
	unit: ^Unit_Analysis,
	allocator: mem.Allocator,
) -> Typecheck_Writable_Index {
	index := Typecheck_Writable_Index {
		ranges = make([dynamic]tokenizer.Range, 0, len(unit.operands), allocator),
	}
	for &operand in unit.operands {
		if .Assignable in operand.flags {
			append(&index.ranges, operand.range)
		}
	}
	slice.sort_by(index.ranges[:], typecheck_range_less)
	return index
}

typecheck_call_index_make :: proc(
	unit: ^Unit_Analysis,
	allocator: mem.Allocator,
) -> Typecheck_Call_Index {
	index := Typecheck_Call_Index {
		calls = make([dynamic]Typecheck_Call_Range, 0, len(unit.call_sites), allocator),
	}
	for call, i in unit.call_sites {
		append(&index.calls, Typecheck_Call_Range{range = call.range, index = i})
	}
	slice.sort_by(index.calls[:], typecheck_call_range_less)
	return index
}

typecheck_exact_call_for_range :: proc(
	unit: ^Unit_Analysis,
	index: ^Typecheck_Call_Index,
	range: tokenizer.Range,
) -> (Call_Site_Data, bool) {
	for i := typecheck_call_range_lower_bound(index.calls[:], range.start);
	    i < len(index.calls) && index.calls[i].range.start == range.start;
	    i += 1 {
		if typecheck_range_equal(index.calls[i].range, range) {
			return unit.call_sites[index.calls[i].index], true
		}
	}
	return {}, false
}

typecheck_first_contained_call_for_range :: proc(
	unit: ^Unit_Analysis,
	index: ^Typecheck_Call_Index,
	range: tokenizer.Range,
) -> (Call_Site_Data, bool) {
	best_index := len(unit.call_sites)
	for i := typecheck_call_range_lower_bound(index.calls[:], range.start);
	    i < len(index.calls) && index.calls[i].range.start <= range.end;
	    i += 1 {
		call := index.calls[i]
		if call.index < best_index && typecheck_range_contains(range, call.range) {
			best_index = call.index
		}
	}
	if best_index < len(unit.call_sites) {
		return unit.call_sites[best_index], true
	}
	return {}, false
}

typecheck_call_range_less :: proc(a, b: Typecheck_Call_Range) -> bool {
	if a.range.start != b.range.start {
		return a.range.start < b.range.start
	}
	return a.index < b.index
}

typecheck_call_range_lower_bound :: proc(calls: []Typecheck_Call_Range, start: int) -> int {
	left, right := 0, len(calls)
	for left < right {
		mid := int(uint(left + right) >> 1)
		if calls[mid].range.start < start {
			left = mid + 1
		} else {
			right = mid
		}
	}
	return left
}

typecheck_fact_for_range_indexed :: proc(
	index: ^Range_Type_Fact_Index,
	range: tokenizer.Range,
) -> Type_Fact_Data {
	best := unknown_type_fact()
	best_rank := -1
	best_width := -1
	for i := range_type_fact_lower_bound(index.facts[:], range.start);
	    i < len(index.facts) && index.facts[i].range.start <= range.end;
	    i += 1 {
		fact := &index.facts[i]
		if type_fact_known(fact.type_fact) {
			if fact.range == range {
				return fact.type_fact
			}
			typecheck_take_fact(&best, &best_rank, &best_width, range, fact.range, fact.rank, fact.type_fact)
		}
	}
	return best
}

typecheck_take_fact :: proc(
	best: ^Type_Fact_Data,
	best_rank: ^int,
	best_width: ^int,
	outer, inner: tokenizer.Range,
	rank: int,
	fact: Type_Fact_Data,
) {
	if !typecheck_range_contains(outer, inner) {
		return
	}
	width := inner.end - inner.start
	if rank > best_rank^ || (rank == best_rank^ && width > best_width^) {
		best^ = fact
		best_rank^ = rank
		best_width^ = width
	}
}

typecheck_range_less :: proc(a, b: tokenizer.Range) -> bool {
	if a.start != b.start {
		return a.start < b.start
	}
	return a.end < b.end
}

typecheck_range_lower_bound :: proc(ranges: []tokenizer.Range, start: int) -> int {
	left, right := 0, len(ranges)
	for left < right {
		mid := int(uint(left + right) >> 1)
		if ranges[mid].start < start {
			left = mid + 1
		} else {
			right = mid
		}
	}
	return left
}

typecheck_range_equal :: proc "contextless" (a, b: tokenizer.Range) -> bool {
	return a.start == b.start && a.end == b.end
}

typecheck_assignment_compatible :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	src, dst: Type_Fact_Data,
	downcast := false,
) -> (bool, bool) {
	if field_type_refs_equal(src.declared_type, dst.declared_type) &&
	   src.has_declared_type && dst.has_declared_type {
		return true, true
	}
	if ok, known := typecheck_ref_compatible(project, lookup, unit_index, src, dst, downcast); known {
		return ok, true
	}
	if typecheck_exact_or_generic(project, lookup, unit_index, src, dst, false) {
		return true, true
	}
	src_name, src_ok := typecheck_builtin_name(project, src)
	dst_name, dst_ok := typecheck_builtin_name(project, dst)
	if src_ok && dst_ok {
		return typecheck_scalar_assignment_conversion(src_name, dst_name)
	}
	src_table := typecheck_fact_is_table(project, src)
	dst_table := typecheck_fact_is_table(project, dst)
	if src_table || dst_table {
		if src_table && dst_table {
			return true, true
		}
		return false, false
	}
	if typecheck_fact_is_structure(src) || typecheck_fact_is_structure(dst) {
		if typecheck_fact_is_structure(src) && typecheck_fact_is_structure(dst) {
			return true, true
		}
		return false, false
	}
	return false, false
}

typecheck_call_compatible :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	src, dst: Type_Fact_Data,
) -> (bool, bool) {
	if ok, known := typecheck_ref_compatible(project, lookup, unit_index, src, dst); known {
		return ok, true
	}
	src_name, src_ok := typecheck_builtin_name(project, src)
	dst_name, dst_ok := typecheck_builtin_name(project, dst)
	if dst_ok && is_generic_builtin_type_name(dst_name) {
		if dst_name != "numeric" && dst_name != "decfloat" && dst_name != "clike" {
			return true, true
		}
		return typecheck_generic_accepts(project, src, dst),
		       typecheck_generic_actual_family_known(project, src, src_name, src_ok)
	}
	if typecheck_exact_or_generic(project, lookup, unit_index, src, dst, true) {
		return true, true
	}
	if src_ok && dst_ok {
		return src_name == dst_name, true
	}
	src_table := typecheck_fact_is_table(project, src)
	dst_table := typecheck_fact_is_table(project, dst)
	if src_table || dst_table {
		if src_table && dst_table {
			return true, true
		}
		return false, false
	}
	if typecheck_fact_is_structure(src) || typecheck_fact_is_structure(dst) {
		if typecheck_same_structure(src, dst) {
			return true, true
		}
		return false, false
	}
	return false, false
}

typecheck_exact_or_generic :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	src, dst: Type_Fact_Data,
	strict: bool,
) -> bool {
	if field_type_refs_equal(src.declared_type, dst.declared_type) &&
	   src.has_declared_type && dst.has_declared_type {
		return true
	}
	if typecheck_generic_accepts(project, src, dst) {
		return true
	}
	src_name, src_ok := typecheck_builtin_name(project, src)
	dst_name, dst_ok := typecheck_builtin_name(project, dst)
	if src_ok && dst_ok {
		if strict {
			return src_name == dst_name
		}
		ok, known := typecheck_scalar_assignment_conversion(src_name, dst_name)
		return known && ok
	}
	_ = lookup
	_ = unit_index
	return false
}

typecheck_ref_compatible :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	src, dst: Type_Fact_Data,
	downcast := false,
) -> (bool, bool) {
	src_ref := typecheck_fact_is_ref(project, src)
	dst_ref := typecheck_fact_is_ref(project, dst)
	if !src_ref && !dst_ref {
		return false, false
	}
	if src_ref != dst_ref {
		return false, false
	}
	src_name, src_ok := typecheck_ref_target_name(project, src)
	dst_name, dst_ok := typecheck_ref_target_name(project, dst)
	if !src_ok || !dst_ok {
		return true, false
	}
	if src_name == dst_name {
		return true, true
	}
	src_target, src_known := typecheck_ref_target(project, lookup, unit_index, src_name)
	dst_target, dst_known := typecheck_ref_target(project, lookup, unit_index, dst_name)
	if !src_known || !dst_known {
		return false, false
	}
	if dst_target.kind == .Data_Generic {
		return typecheck_ref_target_kind_is_data(src_target.kind), true
	}
	if dst_target.kind == .Object_Generic {
		return typecheck_ref_target_kind_is_object(src_target.kind), true
	}
	if dst_target.kind == .Data {
		if src_target.kind == .Data_Generic {
			return false, true
		}
		if typecheck_ref_target_kind_is_object(src_target.kind) {
			return false, true
		}
		return false, false
	}
	if dst_target.kind == .Class {
		#partial switch src_target.kind {
		case .Object_Generic:
			return downcast, true
		case .Class:
			if class_is_or_inherits_from_name(project, lookup, src_target.handle, dst_target.name) {
				return true, true
			}
			if downcast && class_is_or_inherits_from_name(project, lookup, dst_target.handle, src_target.name) {
				return true, true
			}
			return false, true
		case .Data, .Data_Generic:
			return false, true
		case .Interface:
			if downcast && type_exposes_interface(project, lookup, dst_target.handle, src_target.name, 0) {
				return true, true
			}
			return false, false
		}
	}
	if dst_target.kind == .Interface {
		#partial switch src_target.kind {
		case .Object_Generic:
			return downcast, true
		case .Class, .Interface:
			if type_exposes_interface(project, lookup, src_target.handle, dst_target.name, 0) {
				return true, true
			}
			if downcast &&
			   src_target.kind == .Interface &&
			   type_exposes_interface(project, lookup, dst_target.handle, src_target.name, 0) {
				return true, true
			}
			return false, false
		case .Data, .Data_Generic:
			return false, true
		}
	}
	return false, false
}

typecheck_ref_target :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	name: string,
) -> (Typecheck_Ref_Target, bool) {
	if name == "data" {
		return Typecheck_Ref_Target{kind = .Data_Generic, name = name}, true
	}
	if name == "object" {
		return Typecheck_Ref_Target{kind = .Object_Generic, name = name}, true
	}
	if is_builtin_type_name(name) {
		return Typecheck_Ref_Target{kind = .Data, name = name}, true
	}
	handle, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, name)
	if !ok {
		return {}, false
	}
	s := symbol_for_project_handle(project, handle)
	if s == nil {
		return {}, false
	}
	if s.kind == .Class {
		return Typecheck_Ref_Target{kind = .Class, name = s.name, handle = handle}, true
	}
	if s.kind == .Interface {
		return Typecheck_Ref_Target{kind = .Interface, name = s.name, handle = handle}, true
	}
	return Typecheck_Ref_Target{kind = .Data, name = name, handle = handle}, true
}

typecheck_ref_target_kind_is_data :: proc "contextless" (kind: Typecheck_Ref_Target_Kind) -> bool {
	return kind == .Data || kind == .Data_Generic
}

typecheck_ref_target_kind_is_object :: proc "contextless" (kind: Typecheck_Ref_Target_Kind) -> bool {
	return kind == .Object_Generic || kind == .Class || kind == .Interface
}

typecheck_generic_accepts :: proc(project: ^Project_Analysis, src, dst: Type_Fact_Data) -> bool {
	dst_name, ok := typecheck_builtin_name(project, dst)
	if !ok || !is_generic_builtin_type_name(dst_name) {
		return false
	}
	if dst_name == "any" || dst_name == "data" {
		return true
	}
	src_name, src_ok := typecheck_builtin_name(project, src)
	if !src_ok {
		return false
	}
	if src_name == dst_name {
		return true
	}
	switch dst_name {
	case "numeric":
		return typecheck_builtin_numeric(src_name)
	case "decfloat":
		return src_name == "decfloat16" || src_name == "decfloat34"
	case "clike":
		return typecheck_builtin_clike(src_name)
	case "csequence":
		return src_name == "c" || src_name == "string"
	case "xsequence":
		return src_name == "x" || src_name == "xstring"
	case "simple":
		return !typecheck_fact_is_ref(project, src) && !typecheck_fact_is_table(project, src)
	}
	return false
}

typecheck_generic_actual_family_known :: proc(
	project: ^Project_Analysis,
	src: Type_Fact_Data,
	src_name: string,
	src_ok: bool,
) -> bool {
	if src_ok {
		group := typecheck_scalar_group(src_name)
		if group != .Unknown && group != .Generic_Simple {
			return true
		}
	}
	return typecheck_fact_is_structure(src) ||
	       typecheck_fact_is_table(project, src) ||
	       typecheck_fact_is_ref(project, src)
}

typecheck_scalar_assignment_conversion :: proc(src_name, dst_name: string) -> (bool, bool) {
	if src_name == dst_name {
		return true, true
	}
	src_group := typecheck_scalar_group(src_name)
	dst_group := typecheck_scalar_group(dst_name)
	if src_group == .Unknown || dst_group == .Unknown ||
	   src_group == .Generic_Simple || dst_group == .Generic_Simple {
		return false, false
	}
	if (src_group == .Date && dst_group == .Time) ||
	   (src_group == .Time && dst_group == .Date) {
		return false, true
	}
	return true, true
}

typecheck_scalar_group :: proc(name: string, depth := 0) -> Typecheck_Scalar_Group {
	if depth > 8 {
		return .Unknown
	}
	switch name {
	case "i", "int1", "int2", "int4", "int8", "p", "decfloat16", "decfloat34", "f":
		return .Numeric
	case "c", "n", "string", "abap_bool":
		return .Character
	case "x", "xstring":
		return .Byte
	case "d":
		return .Date
	case "t":
		return .Time
	case "simple", "numeric", "decfloat", "clike", "csequence", "xsequence", "any", "data":
		return .Generic_Simple
	}
	if metadata, ok := builtin_type_metadata(name); ok && !metadata.is_ref {
		return typecheck_scalar_group(metadata.type_name, depth + 1)
	}
	return .Unknown
}

typecheck_builtin_numeric :: proc "contextless" (name: string) -> bool {
	switch name {
	case "i", "int1", "int2", "int4", "int8", "p", "decfloat16", "decfloat34", "f":
		return true
	}
	return false
}

typecheck_builtin_clike :: proc "contextless" (name: string) -> bool {
	switch name {
	case "c", "n", "string", "d", "t", "abap_bool":
		return true
	}
	return false
}

typecheck_fact_is_structure :: proc(fact: Type_Fact_Data) -> bool {
	return fact.structure != INVALID_STRUCTURE_ID
}

typecheck_same_structure :: proc(a, b: Type_Fact_Data) -> bool {
	return a.structure != INVALID_STRUCTURE_ID &&
	       b.structure != INVALID_STRUCTURE_ID &&
	       a.structure == b.structure &&
	       a.structure_unit == b.structure_unit
}

typecheck_fact_is_table :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> bool {
	if fact.table_line != nil {
		return true
	}
	if t := typecheck_type_data(project, fact); t != nil {
		return t.kind == .Table
	}
	name, ok := typecheck_builtin_name(project, fact)
	return ok && name == "any table"
}

typecheck_table_row_fact :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> (Type_Fact_Data, bool) {
	if fact.table_line != nil {
		return fact.table_line^, true
	}
	raw := typecheck_raw_type_data(project, fact)
	t := typecheck_type_data(project, fact)
	if t == nil || t.kind != .Table || !type_id_is_known(t.base) {
		return unknown_type_fact(), false
	}
	row := Type_Fact_Data {
		type_id = t.base,
		type_unit = fact.type_unit,
		structure = INVALID_STRUCTURE_ID,
		structure_unit = INVALID_UNIT_ID,
		confidence = fact.confidence,
	}
	if raw != nil && raw.kind == .Table && fact.has_declared_type {
		row.declared_type = fact.declared_type
		row.has_declared_type = true
		row.type_clause_display = fact.declared_type.base_name
	}
	if row_type := typecheck_type_data(project, row); row_type != nil && row_type.kind == .Structure {
		row.structure = row_type.structure
		row.structure_unit = fact.type_unit if row_type.structure != INVALID_STRUCTURE_ID else INVALID_UNIT_ID
	}
	return row, true
}

typecheck_fact_is_ref :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> bool {
	if fact.has_declared_type && fact.declared_type.is_ref {
		return true
	}
	if t := typecheck_type_data(project, fact); t != nil {
		return t.kind == .Ref
	}
	return false
}

typecheck_ref_target_name :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> (string, bool) {
	if fact.has_declared_type && fact.declared_type.is_ref && fact.declared_type.base_name != "" {
		return fact.declared_type.base_name, true
	}
	t := typecheck_type_data(project, fact)
	if t == nil || t.kind != .Ref {
		return "", false
	}
	unit_index := unit_id_index(fact.type_unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return "", false
	}
	target := type_data(&project.units[unit_index], t.base)
	if target == nil {
		return "", false
	}
	return target.name, target.name != ""
}

typecheck_builtin_name :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> (string, bool) {
	if fact.has_declared_type &&
	   !fact.declared_type.is_ref &&
	   len(fact.declared_type.field_path) == 0 &&
	   is_builtin_type_name(fact.declared_type.base_name) {
		return fact.declared_type.base_name, true
	}
	if t := typecheck_type_data(project, fact); t != nil {
		if t.kind == .Builtin {
			return t.name, t.name != ""
		}
	}
	return "", false
}

typecheck_raw_type_data :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> ^Type_Data {
	if !type_id_is_known(fact.type_id) || fact.type_unit == INVALID_UNIT_ID {
		return nil
	}
	unit_index := unit_id_index(fact.type_unit)
	if unit_index < 0 || unit_index >= len(project.units) {
		return nil
	}
	return type_data(&project.units[unit_index], fact.type_id)
}

typecheck_type_data :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> ^Type_Data {
	t := typecheck_raw_type_data(project, fact)
	unit_index := unit_id_index(fact.type_unit)
	for depth := 0; t != nil && t.kind == .Named && type_id_is_known(t.base) && depth < 16; depth += 1 {
		t = type_data(&project.units[unit_index], t.base)
	}
	return t
}

typecheck_single_sql_projection_fact :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_index: int,
	target: Sql_Target_Data,
) -> (Type_Fact_Data, bool) {
	unit := &project.units[unit_index]
	projection := cast(^Sql_Projection_Data)nil
	for &candidate in unit.sql_projections {
		if candidate.query_id != target.query_id || candidate.kind != .Column || candidate.name == "" {
			continue
		}
		if projection != nil {
			return unknown_type_fact(), false
		}
		projection = &candidate
	}
	if projection == nil {
		return unknown_type_fact(), false
	}
	source := cast(^Sql_Source_Data)nil
	for &candidate in unit.sql_sources {
		if candidate.query_id != target.query_id || candidate.resolution != .External {
			continue
		}
		if projection.source_alias != "" &&
		   !(candidate.alias == projection.source_alias || candidate.name == projection.source_alias) {
			continue
		}
		if source != nil {
			return unknown_type_fact(), false
		}
		source = &candidate
	}
	if source == nil {
		return unknown_type_fact(), false
	}
	handle, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, source.name)
	if !ok {
		return unknown_type_fact(), false
	}
	source_unit_index := unit_id_index(handle.unit)
	source_symbol := symbol(&project.units[source_unit_index], handle.symbol)
	if source_symbol == nil || source_symbol.structure == INVALID_STRUCTURE_ID {
		return unknown_type_fact(), false
	}
	field, field_unit_index, field_ok := project_structure_field_lookup(
		project,
		handle.unit,
		source_symbol.structure,
		projection.name,
	)
	if !field_ok {
		return unknown_type_fact(), false
	}
	fact := Type_Fact_Data {
		type_id = field.type_id,
		type_unit = project.units[field_unit_index].unit_id if type_id_is_known(field.type_id) else INVALID_UNIT_ID,
		structure = field.structure,
		structure_unit = project.units[field_unit_index].unit_id if field.structure != INVALID_STRUCTURE_ID else INVALID_UNIT_ID,
		declared_type = field.type_ref,
		has_declared_type = .Has_Type_Ref in field.flags,
		type_clause_display = field.type_ref.base_name,
		confidence = .Low,
	}
	if project.units[field_unit_index].source_mode == .Full ||
	   typecheck_sql_scalar_fact_is_complete(project, fact) {
		fact.confidence = .High
	}
	return fact, true
}

typecheck_sql_scalar_fact_is_complete :: proc(project: ^Project_Analysis, fact: Type_Fact_Data) -> bool {
	name, ok := typecheck_builtin_name(project, fact)
	if !ok {
		return false
	}
	group := typecheck_scalar_group(name)
	return group != .Unknown && group != .Generic_Simple
}

typecheck_range_contains :: proc "contextless" (outer, inner: tokenizer.Range) -> bool {
	return outer.start <= inner.start && inner.end <= outer.end && inner.start < inner.end
}

typecheck_arg_key :: proc(arg: Call_Argument_Data, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_int(&out, int(arg.section))
	strings.write_byte(&out, ':')
	strings.write_string(&out, arg.name)
	return strings.to_string(out)
}

typecheck_range_text :: proc(unit: ^Unit_Analysis, range: tokenizer.Range, allocator: mem.Allocator) -> string {
	if !range_valid(range) || range.start < 0 || range.end > len(unit.source) {
		return "operand"
	}
	return strings.trim_space(strings.clone(unit.source[range.start:range.end], allocator))
}

typecheck_name_message :: proc(prefix, name: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, prefix)
	strings.write_byte(&out, '\'')
	strings.write_string(&out, name)
	strings.write_byte(&out, '\'')
	return strings.to_string(out)
}

typecheck_two_name_message :: proc(left, middle, right: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_byte(&out, '\'')
	strings.write_string(&out, left)
	strings.write_byte(&out, '\'')
	strings.write_string(&out, middle)
	strings.write_byte(&out, '\'')
	strings.write_string(&out, right)
	strings.write_byte(&out, '\'')
	return strings.to_string(out)
}

typecheck_two_operand_message :: proc(
	unit: ^Unit_Analysis,
	prefix: string,
	left: tokenizer.Range,
	middle: string,
	right: tokenizer.Range,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, prefix)
	strings.write_byte(&out, '\'')
	strings.write_string(&out, typecheck_range_text(unit, left, context.temp_allocator))
	strings.write_byte(&out, '\'')
	strings.write_string(&out, middle)
	strings.write_byte(&out, '\'')
	strings.write_string(&out, typecheck_range_text(unit, right, context.temp_allocator))
	strings.write_byte(&out, '\'')
	return strings.to_string(out)
}

typecheck_message_with_type_detail :: proc(
	message: string,
	project: ^Project_Analysis,
	current, expected: Type_Fact_Data,
	allocator: mem.Allocator,
) -> string {
	current_name, current_ok := typecheck_diagnostic_type_name(project, current, allocator)
	expected_name, expected_ok := typecheck_diagnostic_type_name(project, expected, allocator)
	if !current_ok || !expected_ok {
		return message
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, message)
	strings.write_string(&out, " (current type '")
	strings.write_string(&out, current_name)
	strings.write_string(&out, "', expected type '")
	strings.write_string(&out, expected_name)
	strings.write_string(&out, "')")
	return strings.to_string(out)
}

typecheck_diagnostic_type_name :: proc(
	project: ^Project_Analysis,
	fact: Type_Fact_Data,
	allocator: mem.Allocator,
) -> (string, bool) {
	if fact.has_declared_type && fact.declared_type.base_name != "" && len(fact.declared_type.field_path) == 0 {
		if fact.declared_type.is_ref {
			return typecheck_prefixed_type_name("REF TO ", fact.declared_type.base_name, allocator), true
		}
		return fact.declared_type.base_name, true
	}
	if name, ok := typecheck_builtin_name(project, fact); ok {
		return name, true
	}
	if typecheck_fact_is_ref(project, fact) {
		if name, ok := typecheck_ref_target_name(project, fact); ok {
			return typecheck_prefixed_type_name("REF TO ", name, allocator), true
		}
		return "", false
	}
	if fact.structure != INVALID_STRUCTURE_ID {
		unit_index := unit_id_index(fact.structure_unit)
		if unit_index >= 0 && unit_index < len(project.units) {
			if s := structure(&project.units[unit_index], fact.structure); s != nil && s.name != "" {
				return s.name, true
			}
		}
	}
	if t := typecheck_raw_type_data(project, fact); t != nil {
		#partial switch t.kind {
		case .Builtin, .Named, .Structure, .Class, .Interface:
			return t.name, t.name != ""
		case .Table:
			row := Type_Fact_Data {
				type_id = t.base,
				type_unit = fact.type_unit,
				structure = INVALID_STRUCTURE_ID,
				structure_unit = INVALID_UNIT_ID,
			}
			if name, ok := typecheck_diagnostic_type_name(project, row, allocator); ok {
				return typecheck_prefixed_type_name("TABLE OF ", name, allocator), true
			}
		}
	}
	if fact.table_line != nil {
		if name, ok := typecheck_diagnostic_type_name(project, fact.table_line^, allocator); ok {
			return typecheck_prefixed_type_name("TABLE OF ", name, allocator), true
		}
	}
	return "", false
}

typecheck_prefixed_type_name :: proc(prefix, name: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, prefix)
	strings.write_string(&out, name)
	return strings.to_string(out)
}
