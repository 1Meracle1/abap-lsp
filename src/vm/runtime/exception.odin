package abap_frontend_vm_runtime

import "core:mem"
import "core:strings"

context_exception_raise :: proc(
	ctx: ^Context,
	type_name: string,
	source: Source_Loc = {},
) -> bool {
	if type_name == "" {
		context_trap(ctx, .Type, "RAISE EXCEPTION requires an exception type", source)
		return false
	}
	exception_state_destroy(&ctx.exception)
	ctx.exception = Exception_State {
		raised = true,
		type_name = strings.clone(type_name, ctx.allocator),
		value = value_object(type_name, ctx.allocator),
		source = source_loc_clone(source, ctx.allocator),
	}
	return true
}

context_exception_matches :: proc(ctx: ^Context, catch_type: string) -> bool {
	if ctx == nil || !ctx.exception.raised || catch_type == "" {
		return false
	}
	if strings.equal_fold(catch_type, "cx_root") {
		return true
	}
	return strings.equal_fold(ctx.exception.type_name, catch_type)
}

context_exception_catch :: proc(
	ctx: ^Context,
	result_type: Type_Descriptor,
	allocator: mem.Allocator = context.allocator,
) -> (Value, bool) {
	if !ctx.exception.raised {
		if result_type != nil {
			return initial_for_type(result_type, allocator), true
		}
		return value_initial(), true
	}
	value := value_clone(ctx.exception.value, allocator)
	exception_state_destroy(&ctx.exception)
	return value, true
}

context_exception_unhandled :: proc(
	ctx: ^Context,
	source: Source_Loc = {},
) -> bool {
	if !ctx.exception.raised {
		return true
	}
	message := strings.builder_make(context.temp_allocator)
	strings.write_string(&message, "unhandled exception")
	if ctx.exception.type_name != "" {
		strings.write_byte(&message, ' ')
		strings.write_string(&message, ctx.exception.type_name)
	}
	trap_source := ctx.exception.source
	if trap_source.range.start == trap_source.range.end {
		trap_source = source
	}
	context_trap(ctx, .Exception, strings.to_string(message), trap_source)
	return false
}
