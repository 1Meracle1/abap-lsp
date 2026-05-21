package stack_trace

@(require) import "base:runtime"
import "core:debug/trace"
import win32 "core:sys/windows"

@(private)
debug_trace_ctx: trace.Context
@(private)
debug_crash_handler: win32.LPVOID

install_debug_crash_trace :: proc() {
	when ODIN_DEBUG {
		if trace.init(&debug_trace_ctx) {
			install_native_debug_crash_trace()
		}
	}
}

when ODIN_DEBUG {
	@(private)
	print_debug_stack_trace :: proc(skip: uint) {
		if trace.in_resolve(&debug_trace_ctx) {
			runtime.print_string("stack backtrace unavailable\n")
			return
		}
		buf: [64]trace.Frame
		runtime.print_string("stack backtrace:\n")
		frames := trace.frames(&debug_trace_ctx, skip + 1, buf[:])
		if len(frames) == 0 {
			runtime.print_string("  <empty>\n")
			return
		}
		for frame, i in frames {
			fl := trace.resolve(&debug_trace_ctx, frame, context.temp_allocator)
			runtime.print_string("  ")
			print_frame_index(i)
			runtime.print_string(": ")
			print_frame(fl, frame)
			runtime.print_byte('\n')
		}
	}

	when ODIN_OS == .Windows {
		@(private)
		STATUS_HEAP_CORRUPTION :: win32.DWORD(0xC0000374)
		@(private)
		STATUS_STACK_BUFFER_OVERRUN :: win32.DWORD(0xC0000409)

		@(private)
		install_native_debug_crash_trace :: proc() {
			debug_crash_handler = win32.AddVectoredExceptionHandler(1, debug_crash_exception_handler)
		}

		@(private)
		debug_crash_exception_handler :: proc "system" (info: ^win32.EXCEPTION_POINTERS) -> win32.LONG {
			context = runtime.default_context()
			if info == nil || info.ExceptionRecord == nil {
				return win32.EXCEPTION_CONTINUE_SEARCH
			}
			code := info.ExceptionRecord.ExceptionCode
			if !is_debug_crash_exception(code) {
				return win32.EXCEPTION_CONTINUE_SEARCH
			}

			print_native_exception_report(info)
			return win32.EXCEPTION_CONTINUE_SEARCH
		}

		@(private)
		print_native_exception_report :: proc(info: ^win32.EXCEPTION_POINTERS) {
			record := info.ExceptionRecord
			code := record.ExceptionCode

			runtime.print_byte('\n')
			runtime.print_string("fatal runtime error: process fault\n")
			runtime.print_string("  exception: ")
			runtime.print_string(exception_code_name(code))
			runtime.print_string(" (0x")
			print_hex(u64(code), 8)
			runtime.print_string(")\n")

			if record.ExceptionAddress != nil {
				runtime.print_string("  address: 0x")
				print_hex(u64(uintptr(record.ExceptionAddress)), 16)
				runtime.print_byte('\n')
			}

			if code == win32.EXCEPTION_ACCESS_VIOLATION && record.NumberParameters >= 2 {
				runtime.print_string("  detail: attempted to ")
				runtime.print_string(access_violation_action(uintptr(record.ExceptionInformation[0])))
				runtime.print_string(" address 0x")
				print_hex(u64(uintptr(record.ExceptionInformation[1])), 16)
				runtime.print_byte('\n')
			}

			print_debug_stack_trace(2)
		}

		@(private)
		exception_code_name :: proc(code: win32.DWORD) -> string {
			switch code {
			case win32.EXCEPTION_ACCESS_VIOLATION:
				return "access violation"
			case win32.EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
				return "array bounds exceeded"
			case win32.EXCEPTION_BREAKPOINT:
				return "breakpoint"
			case win32.EXCEPTION_DATATYPE_MISALIGNMENT:
				return "data type misalignment"
			case win32.EXCEPTION_FLT_DENORMAL_OPERAND:
				return "floating-point denormal operand"
			case win32.EXCEPTION_FLT_DIVIDE_BY_ZERO:
				return "floating-point divide by zero"
			case win32.EXCEPTION_FLT_INEXACT_RESULT:
				return "floating-point inexact result"
			case win32.EXCEPTION_FLT_INVALID_OPERATION:
				return "floating-point invalid operation"
			case win32.EXCEPTION_FLT_OVERFLOW:
				return "floating-point overflow"
			case win32.EXCEPTION_FLT_STACK_CHECK:
				return "floating-point stack check"
			case win32.EXCEPTION_FLT_UNDERFLOW:
				return "floating-point underflow"
			case win32.EXCEPTION_ILLEGAL_INSTRUCTION:
				return "illegal instruction"
			case win32.EXCEPTION_IN_PAGE_ERROR:
				return "in-page error"
			case win32.EXCEPTION_INT_DIVIDE_BY_ZERO:
				return "integer divide by zero"
			case win32.EXCEPTION_INT_OVERFLOW:
				return "integer overflow"
			case win32.EXCEPTION_PRIV_INSTRUCTION:
				return "privileged instruction"
			case win32.EXCEPTION_STACK_OVERFLOW:
				return "stack overflow"
			case STATUS_HEAP_CORRUPTION:
				return "heap corruption"
			case STATUS_STACK_BUFFER_OVERRUN:
				return "stack buffer overrun"
			}
			return "unknown exception"
		}

		@(private)
		access_violation_action :: proc(action: uintptr) -> string {
			switch action {
			case 0:
				return "read"
			case 1:
				return "write"
			case 8:
				return "execute"
			}
			return "access"
		}

		@(private)
		is_debug_crash_exception :: proc(code: win32.DWORD) -> bool {
			switch code {
			case win32.EXCEPTION_ACCESS_VIOLATION,
			     win32.EXCEPTION_IN_PAGE_ERROR,
			     win32.EXCEPTION_ILLEGAL_INSTRUCTION,
			     win32.EXCEPTION_NONCONTINUABLE_EXCEPTION,
			     win32.EXCEPTION_ARRAY_BOUNDS_EXCEEDED,
			     win32.EXCEPTION_INT_DIVIDE_BY_ZERO,
			     win32.EXCEPTION_INT_OVERFLOW,
			     win32.EXCEPTION_PRIV_INSTRUCTION,
			     win32.EXCEPTION_STACK_OVERFLOW,
			     STATUS_HEAP_CORRUPTION,
			     STATUS_STACK_BUFFER_OVERRUN:
				return true
			}
			return false
		}
	} else {
		@(private)
		install_native_debug_crash_trace :: proc() {}
	}

	@(private)
	print_hex :: proc(value: u64, width: int) {
		digits := "0123456789abcdef"
		for shift := (width - 1) * 4; shift >= 0; shift -= 4 {
			runtime.print_byte(digits[int((value >> uint(shift)) & 0xf)])
		}
	}

	@(private)
	print_frame_index :: proc(index: int) {
		if index < 10 {
			runtime.print_byte('0')
		}
		runtime.print_int(index)
	}

	@(private)
	print_frame :: proc(fl: trace.Frame_Location, frame: trace.Frame) {
		if fl.loc.procedure != "" {
			print_proc_name(fl.loc.procedure)
		} else {
			runtime.print_string("0x")
			print_hex(u64(uintptr(frame)), 16)
		}
		if fl.loc.file_path != "" {
			runtime.print_string(" (")
			runtime.print_string(fl.loc.file_path)
			if fl.loc.line != 0 {
				runtime.print_byte(':')
				runtime.print_int(int(fl.loc.line))
			}
			runtime.print_byte(')')
		}
	}

	@(private)
	print_proc_name :: proc(name: string) {
		for i := 0; i + 6 <= len(name); i += 1 {
			if name[i:i + 6] == ":proc(" {
				runtime.print_string(name[:i])
				return
			}
		}
		runtime.print_string(name)
	}
}
