package persistence

import "core:mem"
import "core:strings"
import "core:unicode/utf8"

@(require_results)
clone_cstring :: proc(s: cstring, allocator := context.allocator, loc := #caller_location) -> cstring {
	c, _ := make([]byte, len(s) + 1, allocator, loc)
	copy(c, string(s))
	c[len(s)] = 0
	return cstring(raw_data(c))
}

split_lines_cstr :: proc(s: cstring, allocator := context.allocator) -> (res: []cstring, err: mem.Allocator_Error) #optional_allocator_error {
	sep :: "\n"
	lines := _split(s, sep, 0, -1, allocator) or_return
	for &line in lines {
		line = _trim_cr(line)
	}
	return lines, nil
}

@(private)
_split :: proc(s_, sep: cstring, sep_save, n_: int, allocator := context.allocator, loc := #caller_location) -> (res: []cstring, err: mem.Allocator_Error) {
	s, n := string(s_), n_
	sep := string(sep)

	if n == 0 {
		return nil, nil
	}

	if sep == "" {
		l := utf8.rune_count_in_string(s)
		if n < 0 || n > l {
			n = l
		}

		res = make([]cstring, n, allocator, loc) or_return
		for i := 0; i < n-1; i += 1 {
			_, w := utf8.decode_rune_in_string(s)
			res[i] = strings.clone_to_cstring(s[:w], allocator)
			s = s[w:]
		}
		if n > 0 {
			res[n-1] = strings.clone_to_cstring(s, allocator)
		}
		return res[:], nil
	}

	if n < 0 {
		n = strings.count(s, sep) + 1
	}

	res = make([]cstring, n, allocator, loc) or_return

	n -= 1

	i := 0
	for ; i < n; i += 1 {
		m := strings.index(string(s), string(sep))
		if m < 0 {
			break
		}
		res[i] = strings.clone_to_cstring(s[:m+sep_save], allocator)
		s = s[m+len(sep):]
	}
	res[i] = strings.clone_to_cstring(s, allocator)

	return res[:i+1], nil
}

@(private)
_trim_cr :: proc(s: cstring) -> (res: cstring) {
	n := len(s)
	s := transmute([^]u8)s
	if n > 0 {
		if s[n-1] == '\r' {
			return transmute(cstring)raw_data(s[:n-1])
		}
	}
	return transmute(cstring)s
}