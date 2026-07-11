package abap_frontend_vm

import ir "src:ir"

import "core:mem"
import virtual "core:mem/virtual"

prepare_module :: proc(
	module: ^ir.Module,
	allocator: mem.Allocator = context.allocator,
) -> Prepare_Result {
	scratch_arena: virtual.Arena
	arena_err := virtual.arena_init_growing(&scratch_arena)
	assert(arena_err == .None)
	defer virtual.arena_destroy(&scratch_arena)
	context.temp_allocator = virtual.arena_allocator(&scratch_arena)

	if module == nil {
		return prepare_error("IR module is nil", allocator = allocator)
	}
	verify := ir.verify_module(module, context.temp_allocator)
	defer ir.verify_result_destroy(&verify)
	if !verify.ok {
		return prepare_error(
			verify.diagnostics[0].message if len(verify.diagnostics) > 0 else "IR verification failed",
			verify.diagnostics[0].source if len(verify.diagnostics) > 0 else ir.Source_Loc{},
			allocator,
		)
	}
	unsupported := find_unsupported(module)
	if unsupported.found {
		return prepare_error(unsupported.message, unsupported.source, allocator)
	}

	prepared := prepared_module_make(module, allocator)
	for &function, function_index in module.functions {
		result := prepare_function(&prepared, module, &function)
		if !result.ok {
			prepared_module_destroy(&prepared)
			return result
		}
		assert(function.id == ir.Function_Id(function_index))
	}
	return Prepare_Result {
		module = prepared,
		ok = true,
	}
}

find_unsupported :: proc "contextless" (module: ^ir.Module) -> Unsupported_Search {
	for function in module.functions {
		for op in function.instructions {
			if op.opcode == .Unsupported || .Unsupported in op.effects {
				payload := op.attrs.(ir.Unsupported_Attrs)
				return Unsupported_Search {
					found = true,
					message = payload.message if payload.message != "" else "unsupported IR operation",
					source = op.source,
				}
			}
		}
	}
	return {}
}
