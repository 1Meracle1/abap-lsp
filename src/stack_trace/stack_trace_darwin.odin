package stack_trace

import "core:c"
import posix "core:sys/posix"

foreign import system "system:System"

@(default_calling_convention = "c")
foreign system {
	backtrace            :: proc(buffer: [^]rawptr, size: c.int) -> c.int ---
	backtrace_symbols_fd :: proc(buffer: [^]rawptr, size: c.int, fd: c.int) ---
}

@(private)
debug_signal_stack: [posix.SIGSTKSZ]byte

install_debug_crash_trace :: proc() {
	when ODIN_DEBUG {
		install_native_debug_crash_trace()
	}
}

when ODIN_DEBUG {
	@(private)
	install_native_debug_crash_trace :: proc() {
		stack := posix.stack_t {
			ss_sp = rawptr(&debug_signal_stack[0]),
			ss_size = c.size_t(len(debug_signal_stack)),
			ss_flags = {},
		}
		use_alt_stack := posix.sigaltstack(&stack, nil) == .OK

		warm_up_backtrace()

		install_crash_signal(.SIGSEGV, use_alt_stack)
		install_crash_signal(.SIGBUS, use_alt_stack)
		install_crash_signal(.SIGILL, use_alt_stack)
		install_crash_signal(.SIGFPE, use_alt_stack)
		install_crash_signal(.SIGABRT, use_alt_stack)
		install_crash_signal(.SIGSYS, use_alt_stack)
	}

	@(private)
	warm_up_backtrace :: proc() {
		frames: [1]rawptr
		backtrace(raw_data(frames[:]), c.int(len(frames)))
	}

	@(private)
	install_crash_signal :: proc(sig: posix.Signal, use_alt_stack: bool) {
		action: posix.sigaction_t
		action.sa_sigaction = debug_crash_signal_handler
		action.sa_flags = {.SIGINFO}
		if use_alt_stack {
			action.sa_flags += {.ONSTACK}
		}
		posix.sigemptyset(&action.sa_mask)
		posix.sigaction(sig, &action, nil)
	}

	@(private)
	debug_crash_signal_handler :: proc "c" (sig: posix.Signal, info: ^posix.siginfo_t, _context: rawptr) {
		print_native_signal_report(sig, info)
		reset_signal_to_default(sig)
		posix.kill(posix.getpid(), sig)
		posix._exit(c.int(128) + c.int(sig))
	}

	@(private)
	print_native_signal_report :: proc "contextless" (sig: posix.Signal, info: ^posix.siginfo_t) {
		write_stderr("\nfatal runtime error: process signal\n")
		write_stderr("  signal: ")
		write_stderr(signal_name(sig))
		write_stderr(" (")
		write_decimal(c.int(sig))
		write_stderr(")\n")

		if signal_has_address(sig) && info != nil && info.si_addr != nil {
			write_stderr("  address: 0x")
			write_hex(u64(uintptr(info.si_addr)), 16)
			write_stderr("\n")
		}

		print_native_stack_trace(0)
	}

	@(private)
	print_native_stack_trace :: proc "contextless" (skip: int) {
		frames: [64]rawptr
		frame_count := backtrace(raw_data(frames[:]), c.int(len(frames)))
		skip_count := c.int(skip)
		if frame_count <= skip_count {
			write_stderr("stack backtrace:\n  <empty>\n")
			return
		}

		write_stderr("stack backtrace:\n")
		backtrace_symbols_fd(raw_data(frames[skip:]), frame_count - skip_count, posix.STDERR_FILENO)
	}

	@(private)
	reset_signal_to_default :: proc "contextless" (sig: posix.Signal) {
		action: posix.sigaction_t
		action.sa_handler = auto_cast posix.SIG_DFL
		posix.sigemptyset(&action.sa_mask)
		posix.sigaction(sig, &action, nil)
	}

	@(private)
	signal_name :: proc "contextless" (sig: posix.Signal) -> string {
		#partial switch sig {
		case .SIGSEGV:
			return "segmentation fault"
		case .SIGBUS:
			return "bus error"
		case .SIGILL:
			return "illegal instruction"
		case .SIGFPE:
			return "floating-point exception"
		case .SIGABRT:
			return "abort"
		case .SIGSYS:
			return "bad system call"
		case:
			return "unknown signal"
		}
	}

	@(private)
	signal_has_address :: proc "contextless" (sig: posix.Signal) -> bool {
		#partial switch sig {
		case .SIGSEGV, .SIGBUS, .SIGILL, .SIGFPE:
			return true
		}
		return false
	}

	@(private)
	write_stderr :: proc "contextless" (text: string) {
		if len(text) == 0 {
			return
		}
		posix.write(posix.FD(posix.STDERR_FILENO), raw_data(text), c.size_t(len(text)))
	}

	@(private)
	write_decimal :: proc "contextless" (value: c.int) {
		buf: [16]byte
		i := len(buf)
		v := value

		if v == 0 {
			write_stderr("0")
			return
		}

		for v > 0 && i > 0 {
			i -= 1
			buf[i] = byte('0' + (v % 10))
			v /= 10
		}

		posix.write(posix.FD(posix.STDERR_FILENO), raw_data(buf[i:]), c.size_t(len(buf) - i))
	}

	@(private)
	write_hex :: proc "contextless" (value: u64, width: int) {
		digits := "0123456789abcdef"
		buf: [16]byte
		for i := 0; i < width; i += 1 {
			shift := uint((width - 1 - i) * 4)
			buf[i] = digits[int((value >> shift) & 0xf)]
		}
		posix.write(posix.FD(posix.STDERR_FILENO), raw_data(buf[:width]), c.size_t(width))
	}
}
