package abap_frontend_vm

import ir "src:ir"
import runtime "src:vm/runtime"

import "core:mem"

dispatch_table_intrinsic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	operation, operation_ok := table_operation(instruction.intrinsic_op)
	if !operation_ok {
		vm_trap(vm, .Unsupported, "ABAP table intrinsic is not implemented", instruction.source)
		return
	}
	payload, payload_ok := instruction.intrinsic_payload.(ir.Intrinsic_Table_Payload)
	if !payload_ok {
		vm_trap(vm, .Invalid_Instruction, "table intrinsic payload is missing", instruction.source)
		return
	}
	values := intrinsic_values(frame, instruction, 1)
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	request := runtime.Table_Request {
		operation = operation,
		values = values,
	}
	if !table_request_apply_payload(vm, &request, payload, instruction) {
		return
	}
	#partial switch operation {
	case .Iter:
		request.result_type = result_type_descriptor(frame.function, instruction, 1)
		iter, ok := runtime.context_table_iter(
			&vm.runtime_context,
			request,
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&iter)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_result(vm, frame, instruction, 1, iter)
		case .Next:
			request.result_type = result_type_descriptor(frame.function, instruction, 2)
			result, ok := runtime.context_table_next(
				&vm.runtime_context,
				request,
				instruction_runtime_source(instruction),
			)
			defer runtime.value_destroy(&result.has_row)
			defer runtime.value_destroy(&result.row)
			defer runtime.value_destroy(&result.next_iter)
			defer runtime.value_destroy(&result.subrc)
			defer runtime.value_destroy(&result.tabix)
			if !ok {
				vm_sync_runtime_trap(vm)
				return
			}
			set_operand(frame, instruction, 1, result.next_iter)
			set_result(vm, frame, instruction, 1, result.has_row)
			result_row, result_ok := table_result_value(vm, result.row, payload.result_kind, true, instruction.source)
			defer runtime.value_destroy(&result_row)
			if !result_ok {
				return
			}
			set_result(vm, frame, instruction, 2, result_row)
			set_result(vm, frame, instruction, 3, result.tabix)
			set_result(vm, frame, instruction, 4, result.subrc)
		case .Read:
			request.result_type = result_type_descriptor(frame.function, instruction, 1)
			result, ok := runtime.context_table_read(
				&vm.runtime_context,
				request,
				instruction_runtime_source(instruction),
			)
			defer runtime.value_destroy(&result.row)
			defer runtime.value_destroy(&result.subrc)
			defer runtime.value_destroy(&result.tabix)
			if !ok {
				vm_sync_runtime_trap(vm)
				return
			}
			found := runtime.value_int(result.subrc) == 0
			result_row, result_ok := table_result_value(vm, result.row, payload.result_kind, found, instruction.source)
			defer runtime.value_destroy(&result_row)
			if !result_ok {
				return
			}
			set_result(vm, frame, instruction, 1, result_row)
			set_result(vm, frame, instruction, 2, result.subrc)
			set_result(vm, frame, instruction, 3, result.tabix)
		case:
			result, ok := runtime.context_table_mutate(
				&vm.runtime_context,
				request,
				instruction_runtime_source(instruction),
			)
			defer runtime.value_destroy(&result.subrc)
			defer runtime.value_destroy(&result.tabix)
			if !ok {
				vm_sync_runtime_trap(vm)
				return
			}
			set_result(vm, frame, instruction, 1, result.subrc)
			set_result(vm, frame, instruction, 2, result.tabix)
		}
	}

table_result_value :: proc(
	vm: ^VM,
	value: runtime.Value,
	kind: ir.Table_Result_Kind,
	found: bool,
	source: runtime.Source_Loc,
) -> (runtime.Value, bool) {
	#partial switch kind {
	case .Assigning:
		if !found {
			return runtime.value_reference_unassigned(.Binding, vm.allocator), true
		}
		binding, binding_ok := field_symbol_binding_for_value(value, vm.allocator)
		if !binding_ok {
			vm_trap(vm, .Type, "table ASSIGNING row is not addressable", source)
			return {}, false
		}
		return binding, true
	case .Reference_Into:
		if !found {
			return runtime.value_initial(), true
		}
		reference := data_reference_for_value(value, vm.allocator)
		return reference, true
	}
	return runtime.value_clone(value, vm.allocator), true
}

field_symbol_binding_for_value :: proc(value: runtime.Value, allocator: mem.Allocator) -> (runtime.Value, bool) {
	cell := runtime.cell_make(value, allocator)
	defer runtime.cell_release(cell)
	alias := runtime.value_alias_cell(cell, allocator)
	defer runtime.value_destroy(&alias)
	return runtime.value_field_symbol_binding_from_alias(alias, allocator)
}

data_reference_for_value :: proc(value: runtime.Value, allocator: mem.Allocator) -> runtime.Value {
	cell := runtime.cell_make(value, allocator)
	defer runtime.cell_release(cell)
	return runtime.value_data_reference_cell(cell, allocator)
}

table_request_apply_payload :: proc(
	vm: ^VM,
	request: ^runtime.Table_Request,
	payload: ir.Intrinsic_Table_Payload,
	instruction: Prepared_Instruction,
) -> bool {
	#partial switch request.operation {
	case .Iter:
		return table_request_add_components(vm, request, payload, instruction.source)
	case .Read:
		return table_request_add_components(vm, request, payload, instruction.source)
	case .Modify:
		return table_request_add_components(vm, request, payload, instruction.source)
	case .Delete:
		return table_request_add_components(vm, request, payload, instruction.source)
	case .Sort:
		return table_request_add_sort(vm, request, payload, instruction.source)
	}
	return true
}

table_request_add_components :: proc(
	vm: ^VM,
	request: ^runtime.Table_Request,
	payload: ir.Intrinsic_Table_Payload,
	source: runtime.Source_Loc,
) -> bool {
	if payload.dynamic_key {
		vm_trap(vm, .Unsupported, "table dynamic key semantics are not implemented", source)
		return false
	}
	if payload.dynamic_component {
		vm_trap(vm, .Unsupported, "table dynamic component semantics are not implemented", source)
		return false
	}
	if len(payload.components) == 0 {
		return true
	}
	components := make([]runtime.Table_Component, len(payload.components), context.temp_allocator)
	for component, i in payload.components {
		if component.value_index < 0 || component.value_index >= len(request.values) {
			vm_trap(vm, .Invalid_Instruction, "table component value operand is missing", source)
			return false
		}
		components[i] = runtime.Table_Component{path = component.path[:], value = request.values[component.value_index]}
	}
	request.components = components
	return true
}

table_request_add_sort :: proc(
	vm: ^VM,
	request: ^runtime.Table_Request,
	payload: ir.Intrinsic_Table_Payload,
	source: runtime.Source_Loc,
) -> bool {
	request.descending = payload.descending
	if payload.dynamic_component {
		vm_trap(vm, .Unsupported, "SORT dynamic component semantics are not implemented", source)
		return false
	}
	if len(payload.sort_components) == 0 {
		return true
	}
	components := make([]runtime.Table_Sort_Component, len(payload.sort_components), context.temp_allocator)
	for component, i in payload.sort_components {
		if len(component.path) == 0 {
			vm_trap(vm, .Invalid_Instruction, "SORT component path is missing", source)
			return false
		}
		components[i] = runtime.Table_Sort_Component{path = component.path[:], descending = component.descending}
	}
	request.sort_components = components
	return true
}

dispatch_sql_intrinsic :: proc(
	vm: ^VM,
	frame: ^Frame,
	instruction: Prepared_Instruction,
) {
	operation, operation_ok := sql_operation(instruction.intrinsic_op)
	if !operation_ok {
		vm_trap(vm, .Unsupported, "Open SQL intrinsic is not implemented", instruction.source)
		return
	}
	set_result(vm, frame, instruction, 0, get_operand(frame, instruction, 0))
	if operation == .Select {
		value, subrc, ok := runtime.context_sql_select(
			&vm.runtime_context,
			instruction_runtime_source(instruction),
		)
		defer runtime.value_destroy(&value)
		defer runtime.value_destroy(&subrc)
		if !ok {
			vm_sync_runtime_trap(vm)
			return
		}
		set_result(vm, frame, instruction, 1, value)
		set_result(vm, frame, instruction, 2, subrc)
		return
	}
	if !runtime.context_sql_mutate(
		&vm.runtime_context,
		instruction_runtime_source(instruction),
	) {
		vm_sync_runtime_trap(vm)
	}
}

table_operation :: proc "contextless" (kind: ir.Intrinsic_Op) -> (runtime.Table_Operation, bool) {
	#partial switch kind {
	case .Table_Iter:
		return .Iter, true
	case .Table_Next:
		return .Next, true
	case .Table_Read:
		return .Read, true
	case .Table_Append:
		return .Append, true
	case .Table_Insert:
		return .Insert, true
	case .Table_Modify:
		return .Modify, true
	case .Table_Delete:
		return .Delete, true
	case .Table_Sort:
		return .Sort, true
	}
	return .Read, false
}

sql_operation :: proc "contextless" (kind: ir.Intrinsic_Op) -> (runtime.SQL_Operation, bool) {
	#partial switch kind {
	case .SQL_Select:
		return .Select, true
	case .SQL_Open_Cursor:
		return .Open_Cursor, true
	case .SQL_Fetch:
		return .Fetch, true
	case .SQL_Close_Cursor:
		return .Close_Cursor, true
	case .SQL_Modify:
		return .Modify, true
	case .SQL_Delete:
		return .Delete, true
	case .SQL_Insert:
		return .Insert, true
	case .SQL_Update:
		return .Update, true
	}
	return .Select, false
}

intrinsic_values :: proc(
	frame: ^Frame,
	instruction: Prepared_Instruction,
	skip: int,
) -> []runtime.Value {
	count := int(instruction.operand_count) - skip
	if count <= 0 {
		return nil
	}
	values := make([]runtime.Value, count, context.temp_allocator)
	for i in 0 ..< count {
		values[i] = get_operand(frame, instruction, i + skip)
	}
	return values
}
