package main

import "core:io"
import "core:log"
import win32 "core:sys/windows"

create_pipe :: proc(name: string) -> (handle: win32.HANDLE, ok: bool) {
	name := win32.utf8_to_wstring(name)
	handle = win32.CreateNamedPipeW(
		name,
		win32.PIPE_ACCESS_DUPLEX,
		win32.PIPE_TYPE_BYTE | win32.PIPE_READMODE_BYTE | win32.PIPE_WAIT,
		1,
		1 << 20,
		1 << 20,
		0,
		nil,
	)
	if handle == win32.INVALID_HANDLE_VALUE {
		log.errorf("failed to create named pipe: %v", win32.GetLastError())
		return
	}
	if !win32.ConnectNamedPipe(handle, nil) {
		log.errorf("failed to connect to named pipe: %v", win32.GetLastError())
		win32.CloseHandle(handle)
		return
	}
	ok = true
	return
}

deinit_pipe :: proc(handle: win32.HANDLE) {
    if handle != win32.INVALID_HANDLE_VALUE {
        win32.DisconnectNamedPipe(handle)
        win32.CloseHandle(handle)
    }
}

stream_from_handle :: proc(handle: win32.HANDLE) -> io.Stream {
	s: io.Stream
	s.data = rawptr(uintptr(handle))
	s.procedure = _file_stream_proc
	return s
}

@(private)
_file_stream_proc :: proc(stream_data: rawptr, mode: io.Stream_Mode, p: []byte, offset: i64, whence: io.Seek_From) -> (n: i64, err: io.Error) {
	handle := win32.HANDLE(uintptr(stream_data))
	n_int: int
	switch mode {
	case .Close:
		deinit_pipe(handle)
	case .Flush:
		win32.FlushFileBuffers(handle)
	case .Read:
		if len(p) == 0 {
			return 0, nil
		}
        n_int: u32
		win32.ReadFile(handle, raw_data(p), cast(u32)len(p), &n_int, nil)
		n = i64(n_int)
		if n == 0 {
			err = .EOF
		}

	case .Read_At:
		if len(p) == 0 {
			return 0, nil
		}
        o := win32.OVERLAPPED{
            OffsetHigh = u32(offset>>32),
            Offset = u32(offset),
        }
        n_int: u32
		win32.ReadFile(handle, raw_data(p), cast(u32)len(p), &n_int, &o)
		n = i64(n_int)
		if n == 0 {
			err = .EOF
		}
	case .Write:
		if len(p) == 0 {
			return 0, nil
		}
        n_int: u32
        win32.WriteFile(handle, raw_data(p), u32(len(p)), &n_int, nil)
		n = i64(n_int)
		if n == 0 {
			err = .EOF
		}
	case .Write_At:
		if len(p) == 0 {
			return 0, nil
		}
        o := win32.OVERLAPPED{
            OffsetHigh = u32(offset>>32),
            Offset = u32(offset),
        }
        n_int: u32
        win32.WriteFile(handle, raw_data(p), u32(len(p)), &n_int, &o)
		n = i64(n_int)
		if n == 0 {
			err = .EOF
		}
	case .Seek:
		return 0, nil
	case .Size:
		return 0, nil
	case .Destroy:
        deinit_pipe(handle)
		err = .Empty
	case .Query:
		return io.query_utility({.Close, .Flush, .Read, .Read_At, .Write, .Write_At, .Seek, .Size, .Query})
	}

	if err != nil {
		n = 0
	}
	return
}
