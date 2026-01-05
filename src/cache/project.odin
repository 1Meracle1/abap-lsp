package cache

import "../lang/ast"
import "../lang/parser"
import "../lang/symbols"
import "core:log"
import "core:os"
import "core:path/filepath"
import "core:strings"
import "core:time"

// Project represents a compilation unit (e.g., an ABAP Report with its includes)
Project :: struct {
	folder_path:         string, // Folder containing the project files
	root_uri:            string, // URI of file with REPORT/FUNCTION-POOL/CLASS-POOL
	include_order:       [dynamic]string, // Ordered list of file URIs (root first, then includes)
	merged_symbol_table: ^symbols.SymbolTable, // Accumulated symbols from all files
	diagnostics:         [dynamic]symbols.Diagnostic, // Project-level diagnostics (e.g., missing includes)
}

// Root declaration type found in a file
Root_Decl_Kind :: enum {
	None,
	Report,
	Function_Pool,
	Class_Pool,
}

// =========================================================================
// URI and Path Utilities
// =========================================================================

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
		   c == '/' ||
		   c == ':' {
			append(&result, c)
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
			uri := path_to_uri(full_path, allocator)
			append(&result, uri)
		}
	}

	return result[:]
}

// Find include file in folder (case-insensitive match)
// Returns the URI of the matching file, or empty string if not found
find_include_file :: proc(
	folder_path: string,
	include_name: string,
	allocator := context.allocator,
) -> (
	string,
	bool,
) {
	include_lower := strings.to_lower(include_name, context.temp_allocator)

	dir_handle, open_err := os.open(folder_path)
	if open_err != os.ERROR_NONE {
		return "", false
	}
	defer os.close(dir_handle)

	entries, read_err := os.read_dir(dir_handle, -1, context.temp_allocator)
	if read_err != os.ERROR_NONE {
		return "", false
	}

	for entry in entries {
		// Skip directories
		if os.is_dir(entry.fullpath) {
			continue
		}
		name_lower := strings.to_lower(entry.name, context.temp_allocator)
		if !strings.has_suffix(name_lower, ".abap") {
			continue
		}
		// Remove .abap extension for comparison
		name_without_ext := name_lower[:len(name_lower) - 5]
		if name_without_ext == include_lower {
			full_path := filepath.join({folder_path, entry.name}, allocator)
			uri := path_to_uri(full_path, allocator)
			return uri, true
		}
	}

	return "", false
}

// =========================================================================
// Project Discovery
// =========================================================================

// Check if a file AST contains a root declaration (REPORT, FUNCTION-POOL, CLASS-POOL)
check_root_decl :: proc(file_ast: ^ast.File) -> Root_Decl_Kind {
	if file_ast == nil {
		return .None
	}

	for decl in file_ast.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Report_Decl:
			return .Report
		}
		// TODO: Add FUNCTION-POOL and CLASS-POOL detection when those AST nodes exist
	}

	return .None
}

// Extract include names from a root file's AST
// Returns include names in declaration order
extract_includes :: proc(file_ast: ^ast.File, allocator := context.allocator) -> []string {
	result := make([dynamic]string, allocator)

	if file_ast == nil {
		return result[:]
	}

	for decl in file_ast.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Include_Decl:
			if d.name != nil {
				// Store lowercase for case-insensitive matching
				name_lower := strings.to_lower(d.name.name, allocator)
				append(&result, name_lower)
			}
		}
	}

	return result[:]
}

// Quick parse a file to check for root declaration
// Returns the AST and root decl kind
quick_parse_file :: proc(
	file_path: string,
	allocator := context.allocator,
) -> (
	^ast.File,
	Root_Decl_Kind,
) {
	data, ok := os.read_entire_file(file_path, allocator)
	if !ok {
		log.warnf("Failed to read file: %s", file_path)
		return nil, .None
	}

	text := string(data)

	// Set allocator for ast.new and other allocations
	context.allocator = allocator

	file_ast := ast.new(ast.File, {})
	file_ast.fullpath = strings.clone(file_path, allocator)
	file_ast.src = text

	p: parser.Parser
	parser.parse_file(&p, file_ast, allocator)

	root_kind := check_root_decl(file_ast)
	return file_ast, root_kind
}

// Discover project from a folder, finding the root file and building include order
discover_project :: proc(folder_path: string, allocator := context.allocator) -> ^Project {
	start := time.now()
	defer log.infof("discover_project took %.2fms", time.duration_milliseconds(time.since(start)))

	project := new(Project, allocator)
	project.folder_path = strings.clone(folder_path, allocator)
	project.include_order = make([dynamic]string, allocator)
	project.diagnostics = make([dynamic]symbols.Diagnostic, allocator)

	// List all .abap files in folder
	files := list_abap_files(folder_path, context.temp_allocator)
	log.infof("Found %d .abap files in %s", len(files), folder_path)

	// Find root file
	root_uri: string
	root_ast: ^ast.File
	for uri in files {
		path := uri_to_path(uri, context.temp_allocator)
		file_ast, root_kind := quick_parse_file(path, context.temp_allocator)
		if root_kind != .None {
			root_uri = strings.clone(uri, allocator)
			// Re-parse with persistent allocator for the root
			root_ast, _ = quick_parse_file(path, allocator)
			log.infof("Found root file: %s (kind: %v)", uri, root_kind)
			break
		}
	}

	if root_uri == "" {
		log.infof("No root file found in folder: %s", folder_path)
		// No root found - this folder doesn't contain a valid ABAP project
		// Each file will be treated independently
		return project
	}

	project.root_uri = root_uri

	// Add root as first in include order
	append(&project.include_order, root_uri)

	// Extract includes from root and build ordered list
	includes := extract_includes(root_ast, context.temp_allocator)
	log.infof("Root file has %d includes", len(includes))

	for include_name in includes {
		include_uri, found := find_include_file(folder_path, include_name, allocator)
		if found {
			append(&project.include_order, include_uri)
			log.infof("  Include '%s' -> %s", include_name, include_uri)
		} else {
			// Emit diagnostic for missing include
			log.warnf("  Include '%s' not found in folder", include_name)
			// Find the Include_Decl node to get its range for the diagnostic
			for decl in root_ast.decls {
				#partial switch d in decl.derived_stmt {
				case ^ast.Include_Decl:
					if d.name != nil {
						name_lower := strings.to_lower(d.name.name, context.temp_allocator)
						if name_lower == include_name {
							append(
								&project.diagnostics,
								symbols.Diagnostic {
									range = d.name.range,
									message = strings.concatenate(
										{"Include file not found: ", include_name, ".abap"},
										allocator,
									),
								},
							)
							break
						}
					}
				}
			}
		}
	}

	return project
}

// =========================================================================
// Project Symbol Resolution
// =========================================================================

// Resolve all files in a project and build merged symbol table
resolve_project :: proc(c: ^Cache, project: ^Project) {
	if project == nil || len(project.include_order) == 0 {
		return
	}

	start := time.now()
	defer log.infof("resolve_project took %.2fms", time.duration_milliseconds(time.since(start)))

	// Create merged symbol table
	merged := new(symbols.SymbolTable)
	merged.symbols = make(map[string]symbols.Symbol)
	merged.types = make([dynamic]^symbols.Type)
	merged.diagnostics = make([dynamic]symbols.Diagnostic)

	// Process files in include order
	for uri in project.include_order {
		snap := get_snapshot(c, uri)
		if snap == nil {
			// Try to load the file
			path := uri_to_path(uri, context.temp_allocator)
			data, ok := os.read_entire_file(path, context.temp_allocator)
			if !ok {
				log.warnf("Failed to read file for resolution: %s", uri)
				continue
			}
			// Parse and resolve this file
			refresh_document(c, uri, string(data), 0)
			snap = get_snapshot(c, uri)
			if snap == nil {
				continue
			}
		}
		defer release_snapshot(snap)

		if snap.ast != nil {
			// Resolve this file's declarations into the merged table
			symbols.resolve_file_into(snap.ast, merged)
		}
	}

	// Store merged table on project
	if project.merged_symbol_table != nil {
		symbols.destroy_symbol_table(project.merged_symbol_table)
	}
	project.merged_symbol_table = merged
}

// Get the project for a given file URI
// Returns nil if file doesn't belong to a project
get_project_for_uri :: proc(c: ^Cache, uri: string) -> ^Project {
	folder := folder_from_uri(uri, context.temp_allocator)

	for folder_path, project in c.projects {
		if strings.equal_fold(folder_path, folder) {
			return project
		}
	}

	return nil
}

// Ensure project context exists for a file
// Creates/discovers project if needed
ensure_project_context :: proc(c: ^Cache, uri: string) -> ^Project {
	folder := folder_from_uri(uri, context.temp_allocator)

	// Check if project already exists
	if project := get_project_for_uri(c, uri); project != nil {
		return project
	}

	// Discover and create project
	project := discover_project(folder)
	if project != nil {
		// Store with persistent folder path
		folder_key := strings.clone(folder)
		c.projects[folder_key] = project
	}

	return project
}

// Mark project as dirty (needs re-resolution)
invalidate_project :: proc(project: ^Project) {
	if project == nil {
		return
	}
	// Clear merged symbol table - will be rebuilt on next request
	if project.merged_symbol_table != nil {
		symbols.destroy_symbol_table(project.merged_symbol_table)
		project.merged_symbol_table = nil
	}
}

// Destroy a project and free its resources
destroy_project :: proc(project: ^Project) {
	if project == nil {
		return
	}

	if project.merged_symbol_table != nil {
		symbols.destroy_symbol_table(project.merged_symbol_table)
	}

	delete(project.folder_path)
	delete(project.root_uri)
	delete(project.include_order)
	delete(project.diagnostics)
	free(project)
}
