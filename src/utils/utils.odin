package abap_frontend_utils

import "core:mem"
import "core:strings"

normalized_uri_path_key :: proc(uri: string, allocator: mem.Allocator) -> string {
	end := len(uri)
	for end > 0 && (uri[end - 1] == '/' || uri[end - 1] == '\\') {
		end -= 1
	}
	out := make([]byte, end, allocator)
	for i in 0 ..< end {
		ch := uri[i]
		if ch == '\\' {
			ch = '/'
		}
		if 'A' <= ch && ch <= 'Z' {
			ch += 'a' - 'A'
		}
		out[i] = ch
	}
	return string(out)
}

to_lower_ascii :: proc(name: string, allocator: mem.Allocator) -> string {
	b: strings.Builder
	strings.builder_init(&b, 0, len(name), allocator)
	for r in name {
		if 'A' <= r && r <= 'Z' {
			strings.write_rune(&b, r + ('a' - 'A'))
		} else {
			strings.write_rune(&b, r)
		}
	}
	return strings.to_string(b)
}
