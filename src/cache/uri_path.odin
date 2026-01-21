package cache

import "core:log"
import "core:os"
import "core:path/filepath"
import "core:strings"

// Hex digit to value (returns -1 if not a hex digit)
hex_digit_value :: proc(c: u8) -> int {
	switch c {
	case '0' ..= '9':
		return int(c - '0')
	case 'a' ..= 'f':
		return int(c - 'a' + 10)
	case 'A' ..= 'F':
		return int(c - 'A' + 10)
	}
	return -1
}

// Value to hex digit (uppercase)
value_to_hex_digit :: proc(v: int) -> u8 {
	if v < 10 {
		return u8('0' + v)
	}
	return u8('A' + v - 10)
}

// URL decode a string (decode %XX sequences)
url_decode :: proc(encoded: string, allocator := context.allocator) -> string {
	if len(encoded) == 0 {
		return strings.clone("", allocator)
	}

	// Count output size (may be smaller due to %XX -> single byte)
	result := make([dynamic]u8, allocator)

	i := 0
	for i < len(encoded) {
		if encoded[i] == '%' && i + 2 < len(encoded) {
			hi := hex_digit_value(encoded[i + 1])
			lo := hex_digit_value(encoded[i + 2])
			if hi >= 0 && lo >= 0 {
				append(&result, u8(hi * 16 + lo))
				i += 3
				continue
			}
		}
		append(&result, encoded[i])
		i += 1
	}

	return string(result[:])
}

// URL encode a string for use in file URIs
// Only encodes characters that are problematic in URIs
// Preserves: alphanumeric, - _ . ~ / :
url_encode_path :: proc(path: string, allocator := context.allocator) -> string {
	if len(path) == 0 {
		return strings.clone("", allocator)
	}

	result := make([dynamic]u8, allocator)

	for i := 0; i < len(path); i += 1 {
		c := path[i]
		// Characters that don't need encoding in file URI paths
		if (c >= 'a' && c <= 'z') ||
		   (c >= 'A' && c <= 'Z') ||
		   (c >= '0' && c <= '9') ||
		   c == '-' ||
		   c == '_' ||
		   c == '.' ||
		   c == '~' ||
		   c == '/' {
			append(&result, c)
		} else if c == ':' {
            append(&result, '%')
			append(&result, '3')
			append(&result, 'A')
		} else if c == ' ' {
			// Space -> %20
			append(&result, '%')
			append(&result, '2')
			append(&result, '0')
		} else if c == '#' {
			// Hash -> %23
			append(&result, '%')
			append(&result, '2')
			append(&result, '3')
		} else if c == '?' {
			// Question mark -> %3F
			append(&result, '%')
			append(&result, '3')
			append(&result, 'F')
		} else if c == '[' {
			append(&result, '%')
			append(&result, '5')
			append(&result, 'B')
		} else if c == ']' {
			append(&result, '%')
			append(&result, '5')
			append(&result, 'D')
		} else {
			// Encode other special characters
			append(&result, '%')
			append(&result, value_to_hex_digit(int(c) >> 4))
			append(&result, value_to_hex_digit(int(c) & 0x0F))
		}
	}

	return string(result[:])
}

// Convert file URI to filesystem path
// file:///D:/dev/folder/file.abap -> D:/dev/folder/file.abap
// file:///d%3A/dev/folder/file.abap -> d:/dev/folder/file.abap
uri_to_path :: proc(uri: string, allocator := context.allocator) -> string {
	// Handle file:// prefix (with 2 or 3 slashes)
	path: string
	if strings.has_prefix(uri, "file:///") {
		path = uri[len("file:///"):]
	} else if strings.has_prefix(uri, "file://") {
		path = uri[len("file://"):]
	} else {
		// Already a path or unsupported URI scheme
		return strings.clone(uri, allocator)
	}

	// URL decode the path (handle %20, %3A, etc.)
	decoded := url_decode(path, allocator)

	// On Windows, convert forward slashes to backslashes for consistency
	// (optional - many Windows APIs accept forward slashes)
	return decoded
}

// Convert filesystem path to file URI
// D:/dev/folder/file.abap -> file:///D:/dev/folder/file.abap
// D:\dev\folder\file.abap -> file:///D:/dev/folder/file.abap
path_to_uri :: proc(path: string, allocator := context.allocator) -> string {
	// First normalize path separators to forward slashes
	normalized, _ := strings.replace_all(path, "\\", "/", context.temp_allocator)

	// URL encode the path (handles spaces, special chars)
	encoded := url_encode_path(normalized, context.temp_allocator)

	return strings.concatenate({"file:///", encoded}, allocator)
}

// Get folder path from file URI
// file:///D:/dev/ZTTRP001/main.abap -> D:/dev/ZTTRP001
folder_from_uri :: proc(uri: string, allocator := context.allocator) -> string {
	path := uri_to_path(uri, allocator)
	dir := filepath.dir(path, allocator)
	return dir
}

// Get filename without extension from URI (lowercased for case-insensitive matching)
filename_from_uri :: proc(uri: string, allocator := context.allocator) -> string {
	path := uri_to_path(uri, context.temp_allocator)
	base := filepath.base(path)
	// Remove .abap extension
	if strings.has_suffix(strings.to_lower(base, context.temp_allocator), ".abap") {
		base = base[:len(base) - 5]
	}
	return strings.to_lower(base, allocator)
}

// List all .abap files in a folder (non-recursive)
list_abap_files :: proc(folder_path: string, allocator := context.allocator) -> []string {
	result := make([dynamic]string, allocator)

	dir_handle, open_err := os.open(folder_path)
	if open_err != os.ERROR_NONE {
		log.warnf("Failed to open folder: %s", folder_path)
		return result[:]
	}
	defer os.close(dir_handle)

	entries, read_err := os.read_dir(dir_handle, -1, allocator)
	if read_err != os.ERROR_NONE {
		log.warnf("Failed to read folder: %s", folder_path)
		return result[:]
	}

	for entry in entries {
		// Skip directories (they won't have .abap extension anyway, but be safe)
		if os.is_dir(entry.fullpath) {
			continue
		}
		name_lower := strings.to_lower(entry.name, context.temp_allocator)
		if strings.has_suffix(name_lower, ".abap") {
			full_path := filepath.join({folder_path, entry.name}, allocator)
			append(&result, full_path)
		}
	}

	return result[:]
}
