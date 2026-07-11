package abap_frontend_vm_runtime

context_field_load :: proc(
	ctx: ^Context,
	request: Field_Request,
	source: Source_Loc = {},
) -> (Value, bool) {
	if reference := value_reference_data(request.base); reference != nil {
		if reference.mode == .Data && request.name == "*" {
			return value_alias_from_data_reference(request.base, ctx.allocator)
		}
		if reference.mode == .Alias && request.name == "*" {
			target := reference_borrow(reference)
			if target_ref := value_reference_data(target); target_ref != nil && target_ref.mode == .Data {
				return value_alias_from_data_reference(target, ctx.allocator)
			}
		}
		if reference.mode == .Alias {
			field_ref := value_reference_field(
				request.base,
				request.name,
				request.result_type,
				.Alias,
				ctx.allocator,
			)
			if field_data := value_reference_data(field_ref); field_data != nil {
				if !reference_materialize_field(field_data, request.result_type) {
					value_destroy(&field_ref)
					context_trap(ctx, .Type, "field access requires a structure or object reference", source)
					return {}, false
				}
			}
			return field_ref, true
		}
	}
	if value_kind(request.base) == .Structure && structure_is_system(request.base) {
		return value_clone(context_system_read(ctx, request.name), ctx.allocator), true
	}
	if structure := value_structure_data(request.base); structure != nil {
		if value, ok := structure.fields[request.name]; ok {
			return value_clone(value, ctx.allocator), true
		}
		return initial_for_type(request.result_type, ctx.allocator), true
	}
	if value_kind(request.base) == .Object {
		object := value_object_data(request.base)
		if object == nil {
			context_trap(ctx, .Type, "object reference is initial", source)
			return {}, false
		}
		if value, ok := object.fields[request.name]; ok {
			return value_clone(value, ctx.allocator), true
		}
		return initial_for_type(request.result_type, ctx.allocator), true
	}
	context_trap(ctx, .Unsupported, "object and structure field runtime semantics are not implemented", source)
	return {}, false
}

context_field_store :: proc(
	ctx: ^Context,
	request: Field_Request,
	source: Source_Loc = {},
) -> bool {
	if reference := value_reference_data(request.base); reference != nil {
		if reference.mode == .Data && request.name == "*" {
			alias, alias_ok := value_alias_from_data_reference(request.base, context.temp_allocator)
			if !alias_ok {
				context_trap(ctx, .Type, "data reference is initial", source)
				return false
			}
			defer value_destroy(&alias)
			if alias_ref := value_reference_data(alias); alias_ref != nil && reference_write(alias_ref, request.value, source) {
				return true
			}
			context_trap(ctx, .Type, "data reference target is not writable", source)
			return false
		}
		if reference.mode == .Alias && request.name == "*" {
			target := reference_borrow(reference)
			if target_ref := value_reference_data(target); target_ref != nil && target_ref.mode == .Data {
				alias, alias_ok := value_alias_from_data_reference(target, context.temp_allocator)
				if !alias_ok {
					context_trap(ctx, .Type, "data reference is initial", source)
					return false
				}
				defer value_destroy(&alias)
				if alias_ref := value_reference_data(alias); alias_ref != nil && reference_write(alias_ref, request.value, source) {
					return true
				}
				context_trap(ctx, .Type, "data reference target is not writable", source)
				return false
			}
		}
		if reference.mode == .Alias {
			field_ref := value_reference_field(
				request.base,
				request.name,
				request.result_type,
				.Alias,
				context.temp_allocator,
			)
			defer value_destroy(&field_ref)
			if field_data := value_reference_data(field_ref); field_data != nil &&
			   reference_write(field_data, request.value, source) {
				return true
			}
			context_trap(ctx, .Type, "field assignment requires a structure or object reference", source)
			return false
		}
	}
	if value_kind(request.base) == .Structure && structure_is_system(request.base) {
		context_system_write(ctx, request.name, request.value)
		return true
	}
	if structure := value_structure_data(request.base); structure != nil {
		structure_set_field(structure, request.name, request.value)
		return true
	}
	if value_kind(request.base) == .Object {
		object := value_object_data(request.base)
		if object == nil {
			context_trap(ctx, .Type, "object reference is initial", source)
			return false
		}
		object_set_field(object, request.name, request.value)
		return true
	}
	context_trap(ctx, .Unsupported, "object and structure field runtime semantics are not implemented", source)
	return false
}

context_assign_field :: proc(
	ctx: ^Context,
	request: Assign_Request,
	source: Source_Loc = {},
) -> (Value, bool) {
	if len(request.values) == 0 {
		context_trap(ctx, .Type, "ASSIGN requires a source operand", source)
		return {}, false
	}
	source_ref := value_reference_data(request.values[0])
	if source_ref == nil || source_ref.mode != .Alias || source_ref.target_kind == .None {
		context_trap(ctx, .Unsupported, "ASSIGN requires a deterministic addressable source", source)
		return {}, false
	}
	binding, binding_ok := value_field_symbol_binding_from_alias(request.values[0], ctx.allocator)
	if !binding_ok {
		context_trap(ctx, .Unsupported, "ASSIGN requires a deterministic addressable source", source)
		return {}, false
	}
	return binding, true
}
