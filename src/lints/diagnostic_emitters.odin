package abap_frontend_lints

import "src:semantic"

import "core:mem"
import "core:strings"

emit_semantic_lint_diagnostics :: proc(
	out: ^Unit_Lints,
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	file: ^semantic.Project_File,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if project == nil || checker == nil || file == nil {
		return
	}
	query := semantic.semantic_query(project, checker, file)
	diagnostics := semantic.semantic_diagnostic_copies(
		semantic.semantic_query_diagnostics(query),
		context.temp_allocator,
	)
	for diagnostic in diagnostics {
		if metadata, ok := metadata_for_semantic_kind(diagnostic.kind); ok {
			emit_diagnostic(
				out,
				metadata,
				diagnostic.range,
				diagnostic.message,
				policy,
				allocator,
			)
		}
	}
}

emit_binary_search_order_lints :: proc(
	out: ^Unit_Lints,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if metadata, ok := metadata_for(UNSORTED_READ_TABLE_BINARY_SEARCH); ok {
		for read in out.read_table_binary_searches {
			if read.table_name == "" || read_table_has_known_sort(out, read) {
				continue
			}
			builder := strings.builder_make(context.temp_allocator)
			strings.write_string(&builder, "READ TABLE ... BINARY SEARCH on '")
			strings.write_string(&builder, read.table_name)
			strings.write_string(&builder, "' is not preceded by a matching SORT or ordered SELECT")
			emit_diagnostic(out, metadata, read.range, strings.to_string(builder), policy, allocator)
		}
	}
	emit_result_handling_lints(out, policy, allocator)
}

read_table_has_known_sort :: proc(out: ^Unit_Lints, read: Read_Table_Binary_Search_Data) -> bool {
	for order in out.internal_table_orders {
		if !strings.equal_fold(order.table_name, read.table_name) {
			continue
		}
		if order.range.end > read.range.start {
			continue
		}
		if key_prefix_matches(order.key_fields[:], read.key_fields[:]) {
			return true
		}
	}
	return false
}

key_prefix_matches :: proc(sorted_fields, read_fields: []string) -> bool {
	if len(read_fields) == 0 || len(sorted_fields) < len(read_fields) {
		return false
	}
	for field, i in read_fields {
		if !strings.equal_fold(sorted_fields[i], field) {
			return false
		}
	}
	return true
}

emit_result_handling_lints :: proc(
	out: ^Unit_Lints,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if metadata, ok := metadata_for(IGNORED_AUTHORITY_CHECK); ok {
		for update in out.system_field_updates {
			if update.statement != .Authority_Check || !strings.equal_fold(update.field_name, "subrc") {
				continue
			}
			if subrc_update_observed(out, update) {
				continue
			}
			emit_diagnostic(
				out,
				metadata,
				update.range,
				"AUTHORITY-CHECK result is not checked via sy-subrc before it is overwritten",
				policy,
				allocator,
			)
		}
	}
	if metadata, ok := metadata_for(IGNORED_CALL_FUNCTION_RESULT); ok {
		for call in out.call_function_results {
			update, update_ok := system_field_update_for_call_function(out, call)
			if !update_ok || subrc_update_observed(out, update) {
				continue
			}
			if call_function_has_potentially_handled_result_argument(out, call) {
				continue
			}
			output_ignored := call_function_has_ignored_output_result(out, call)
			subrc_overwritten := call.has_exception_mapping && next_subrc_update_start(out, update) >= 0
			if !output_ignored && !subrc_overwritten {
				continue
			}
			message := "CALL FUNCTION result in sy-subrc is not checked before it is overwritten"
			if output_ignored {
				message = "CALL FUNCTION output result is ignored and sy-subrc is not checked"
			}
			emit_diagnostic(out, metadata, call.range, message, policy, allocator)
		}
	}
}

call_function_has_potentially_handled_result_argument :: proc(
	out: ^Unit_Lints,
	call: Call_Function_Result_Data,
) -> bool {
	if call.has_changing_argument {
		return true
	}
	for target in call.output_targets {
		if !call_function_output_target_is_local(target) || value_read_after_range(out, target, call) {
			return true
		}
	}
	return false
}

call_function_has_ignored_output_result :: proc(
	out: ^Unit_Lints,
	call: Call_Function_Result_Data,
) -> bool {
	if len(call.output_targets) == 0 {
		return false
	}
	for target in call.output_targets {
		if !call_function_output_target_is_local(target) || value_read_after_range(out, target, call) {
			return false
		}
	}
	return true
}

call_function_output_target_is_local :: proc(target: Call_Function_Output_Target_Data) -> bool {
	return target.entity != nil &&
	       target.entity.kind == .Variable &&
	       target.entity.scope != nil &&
	       target.entity.scope.kind != .File &&
	       target.entity.member_kind == .None &&
	       target.entity.owner == nil
}

value_read_after_range :: proc(
	out: ^Unit_Lints,
	target: Call_Function_Output_Target_Data,
	call: Call_Function_Result_Data,
) -> bool {
	for read in out.value_reads {
		if read.range.start < call.range.end ||
		   !fact_scopes_may_share_sequential_flow(out, call.scope, read.scope) {
			continue
		}
		if target.entity != nil && read.entity != nil {
			if read.entity == target.entity {
				return true
			}
			continue
		}
		if strings.equal_fold(read.name, target.name) {
			return true
		}
	}
	return false
}

system_field_update_for_call_function :: proc(
	out: ^Unit_Lints,
	call: Call_Function_Result_Data,
) -> (System_Field_Update_Data, bool) {
	for update in out.system_field_updates {
		if update.statement == .Call_Function &&
		   update.scope == call.scope &&
		   update.range == call.range &&
		   strings.equal_fold(update.field_name, "subrc") {
			return update, true
		}
	}
	return {}, false
}

subrc_update_observed :: proc(out: ^Unit_Lints, update: System_Field_Update_Data) -> bool {
	for check in out.value_state_checks {
		if !value_state_check_is_sy_subrc(check) {
			continue
		}
		if check.range.start < update.range.end {
			continue
		}
		latest, latest_ok := latest_subrc_update_before_check(out, check)
		if !latest_ok || !same_system_field_update(latest, update) {
			continue
		}
		return true
	}
	return false
}

next_subrc_update_start :: proc(out: ^Unit_Lints, update: System_Field_Update_Data) -> int {
	best := -1
	for candidate in out.system_field_updates {
		if same_system_field_update(candidate, update) {
			continue
		}
		if !strings.equal_fold(candidate.field_name, "subrc") || candidate.range.start < update.range.end {
			continue
		}
		if !subrc_update_is_proven_later_on_same_flow(out, update, candidate) {
			continue
		}
		if best < 0 || candidate.range.start < best {
			best = candidate.range.start
		}
	}
	return best
}

latest_subrc_update_before_check :: proc(
	out: ^Unit_Lints,
	check: Value_State_Check_Data,
) -> (System_Field_Update_Data, bool) {
	best: System_Field_Update_Data
	best_ok := false
	for update in out.system_field_updates {
		if !strings.equal_fold(update.field_name, "subrc") ||
		   update.range.end > check.range.start ||
		   !fact_scope_descends_from(out, check.scope, update.scope) {
			continue
		}
		if !best_ok ||
		   update.range.end > best.range.end ||
		   (update.range.end == best.range.end && update.range.start > best.range.start) {
			best = update
			best_ok = true
		}
	}
	return best, best_ok
}

subrc_update_is_proven_later_on_same_flow :: proc(
	out: ^Unit_Lints,
	earlier: System_Field_Update_Data,
	later: System_Field_Update_Data,
) -> bool {
	return earlier.scope == later.scope || fact_scope_descends_from(out, earlier.scope, later.scope)
}

same_system_field_update :: proc(left, right: System_Field_Update_Data) -> bool {
	return left.scope == right.scope &&
	       left.range == right.range &&
	       left.statement == right.statement &&
	       strings.equal_fold(left.field_name, right.field_name)
}

value_state_check_is_sy_subrc :: proc(check: Value_State_Check_Data) -> bool {
	return(strings.equal_fold(check.field_name, "subrc") &&
	       (strings.equal_fold(check.symbol_name, "sy") || strings.equal_fold(check.symbol_name, "syst")))
}
