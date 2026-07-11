package abap_frontend_vm_runtime

import "core:mem"
import "core:slice"
import "core:strings"

io_policy_captured :: #force_inline proc "contextless" () -> IO_Policy {
	return IO_Policy {
		kind            = .Capture_All,
		capture_write   = true,
		capture_message = true,
	}
}

context_make :: proc(
	options: Context_Options = {},
	allocator: mem.Allocator = context.allocator,
) -> Context {
	io_policy := options.io_policy
	switch io_policy.kind {
	case .Default, .Capture_All:
		io_policy = io_policy_captured()
	case .Deny_All:
		io_policy.capture_write = false
		io_policy.capture_message = false
	case .Custom:
	}
	return Context {
		allocator = allocator,
		io_policy = io_policy,
		world = Value_World{},
		global_values = make(map[string]Value, 32, allocator),
		system_values = make(map[string]Value, 8, allocator),
		events = make([dynamic]IO_Event, 0, 8, allocator),
	}
}

context_destroy :: proc(ctx: ^Context) {
	for key, &value in ctx.global_values {
		delete(key, ctx.allocator)
		value_destroy(&value)
	}
	delete(ctx.global_values)
	for key, &value in ctx.system_values {
		delete(key, ctx.allocator)
		value_destroy(&value)
	}
	delete(ctx.system_values)
	for &event in ctx.events {
		io_event_destroy(&event)
	}
	delete(ctx.events)
	exception_state_destroy(&ctx.exception)
	value_destroy(&ctx.world)
	delete(ctx.trap.message)
	source_loc_destroy(&ctx.trap.source)
	ctx^ = {}
}

context_trapped :: #force_inline proc "contextless" (ctx: ^Context) -> bool {
	return ctx.trap.kind != .None
}

context_trap :: proc(
	ctx: ^Context,
	kind: Trap_Kind,
	message: string,
	source: Source_Loc = {},
) {
	delete(ctx.trap.message)
	source_loc_destroy(&ctx.trap.source)
	ctx.trap = Trap {
		kind = kind,
		message = strings.clone(message, ctx.allocator),
		source = source_loc_clone(source, ctx.allocator),
	}
}

io_event_destroy :: proc(event: ^IO_Event) {
	delete(event.text)
	delete(event.message_type)
	source_loc_destroy(&event.source)
	event^ = {}
}

source_loc_clone :: proc(source: Source_Loc, allocator: mem.Allocator) -> Source_Loc {
	return Source_Loc {
		path = strings.clone(source.path, allocator),
		range = source.range,
	}
}

source_loc_destroy :: proc(source: ^Source_Loc) {
	delete(source.path)
	source^ = {}
}

io_event_clone :: proc(event: IO_Event, allocator: mem.Allocator) -> IO_Event {
	return IO_Event {
		kind = event.kind,
		text = strings.clone(event.text, allocator),
		message_type = strings.clone(event.message_type, allocator),
		source = source_loc_clone(event.source, allocator),
	}
}

trap_clone :: proc(trap: Trap, allocator: mem.Allocator) -> Trap {
	return Trap {
		kind = trap.kind,
		message = strings.clone(trap.message, allocator),
		source = source_loc_clone(trap.source, allocator),
	}
}

named_value_clone :: proc(scope, name: string, value: Value, allocator: mem.Allocator) -> Named_Value {
	return Named_Value {
		scope = strings.clone(scope, allocator),
		name = strings.clone(name, allocator),
		value = value_clone(value, allocator),
	}
}

named_value_destroy :: proc(value: ^Named_Value) {
	delete(value.scope)
	delete(value.name)
	value_destroy(&value.value)
	value^ = {}
}

exception_state_destroy :: proc(exception: ^Exception_State) {
	delete(exception.type_name)
	value_destroy(&exception.value)
	source_loc_destroy(&exception.source)
	exception^ = {}
}

context_collect_final_values :: proc(
	ctx: ^Context,
	out: ^[dynamic]Named_Value,
	allocator: mem.Allocator,
) {
	values := make([dynamic]Named_Value, 0, len(ctx.global_values) + len(ctx.system_values), context.temp_allocator)
	defer {
		for &value in values {
			named_value_destroy(&value)
		}
	}
	for key, value in ctx.global_values {
		append(&values, named_value_clone("global", key, value, context.temp_allocator))
	}
	for key, value in ctx.system_values {
		append(&values, named_value_clone("system", key, value, context.temp_allocator))
	}
	slice.sort_by(values[:], named_value_less)
	for value in values {
		append(out, named_value_clone(value.scope, value.name, value.value, allocator))
	}
}

context_global_read :: #force_inline proc "contextless" (ctx: ^Context, name: string) -> Value {
	return map_value_or_initial(ctx.global_values, name)
}

context_global_write :: #force_inline proc(ctx: ^Context, name: string, value: Value) {
	map_set_value(&ctx.global_values, name, value, ctx.allocator)
}

context_global_bind :: #force_inline proc(ctx: ^Context, name: string, value: Value) {
	map_set_value_raw(&ctx.global_values, name, value, ctx.allocator)
}

context_runtime_read :: proc(ctx: ^Context, name: string) -> Value {
	switch name {
	case "sy", "syst":
		return Value_Structure{name = "sy"}
	}
	return map_value_or_initial(ctx.global_values, name)
}

context_runtime_write :: proc(
	ctx: ^Context,
	name: string,
	value: Value,
	source: Source_Loc = {},
) -> bool {
	switch name {
	case "sy", "syst":
		context_trap(ctx, .Unsupported, "runtime-provided structure assignment is not implemented", source)
		return false
	}
	context_global_write(ctx, name, value)
	return true
}

context_system_read :: #force_inline proc(ctx: ^Context, field_name: string) -> Value {
	value := map_value_or_initial(ctx.system_values, field_name)
	if value_kind(value) == .Initial {
		switch field_name {
		case "subrc", "tabix", "dbcnt", "fdpos":
			return value_integer_make(0)
		}
	}
	return value
}

context_system_write :: #force_inline proc(ctx: ^Context, field_name: string, value: Value) {
	map_set_value(&ctx.system_values, field_name, value, ctx.allocator)
}

context_write :: proc(
	ctx: ^Context,
	values: []Value,
	source: Source_Loc = {},
) -> bool {
	if !ctx.io_policy.capture_write {
		context_trap(ctx, .Unsupported, "WRITE output is denied by runtime I/O policy", source)
		return false
	}
	out := strings.builder_make(context.temp_allocator)
	for value, i in values {
		if i > 0 {
			strings.write_byte(&out, ' ')
		}
		if !value_write_scalar_text(&out, value) {
			context_trap(ctx, .Type, "WRITE requires scalar text operands", source)
			return false
		}
	}
	append_event(ctx, .Write, strings.to_string(out), "", source)
	return true
}

context_message :: proc(
	ctx: ^Context,
	descriptor: Message_Descriptor,
	values: []Value,
	source: Source_Loc = {},
) -> (string, bool) {
	if !ctx.io_policy.capture_message {
		context_trap(ctx, .Unsupported, "MESSAGE output is denied by runtime I/O policy", source)
		return "", false
	}
	text, text_ok := message_text(descriptor, values, context.temp_allocator)
	if !text_ok {
		context_trap(ctx, .Type, "MESSAGE requires scalar text operands", source)
		return "", false
	}
	append_event(ctx, .Message, text, descriptor.message_type, source)
	return text, true
}

append_event :: proc(
	ctx: ^Context,
	kind: IO_Event_Kind,
	text, message_type: string,
	source: Source_Loc,
) {
	append(
		&ctx.events,
		IO_Event {
			kind = kind,
			text = strings.clone(text, ctx.allocator),
			message_type = strings.clone(message_type, ctx.allocator),
				source = source_loc_clone(source, ctx.allocator),
		},
	)
}

message_text :: proc(
	descriptor: Message_Descriptor,
	values: []Value,
	allocator: mem.Allocator,
) -> (string, bool) {
	out := strings.builder_make(allocator)
	if descriptor.message_type != "" {
		strings.write_string(&out, descriptor.message_type)
	}
	if descriptor.message_number != "" {
		if strings.builder_len(out) > 0 {
			strings.write_byte(&out, ' ')
		}
		strings.write_string(&out, descriptor.message_number)
	}
	if descriptor.message_id != "" {
		if strings.builder_len(out) > 0 {
			strings.write_byte(&out, ' ')
		}
		strings.write_byte(&out, '(')
		strings.write_string(&out, descriptor.message_id)
		strings.write_byte(&out, ')')
	}
	for value in values {
		if strings.builder_len(out) > 0 {
			strings.write_byte(&out, ' ')
		}
		if !value_write_scalar_text(&out, value) {
			return "", false
		}
	}
	return strings.to_string(out), true
}

named_value_less :: proc(left, right: Named_Value) -> bool {
	if left.scope != right.scope {
		return strings.compare(left.scope, right.scope) < 0
	}
	return strings.compare(left.name, right.name) < 0
}
