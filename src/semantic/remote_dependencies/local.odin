package abap_frontend_semantic_remote_dependencies

import "src:adt"
import dep_store "src:dependency_store"
import analyze "src:semantic/analyze"
import deps "src:semantic/dependencies"

import base_runtime "base:runtime"
import "core:mem"
import "core:mem/virtual"
import net_url "core:net"
import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:time"

add_local_export_matches :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []deps.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	roots: []string,
	target_uri: string,
	allocator: mem.Allocator,
	dependency_summaries: ^[dynamic]analyze.Summary_Provider_Input = nil,
) -> bool {
	uri_key_arena: virtual.Arena
	_ = virtual.arena_init_growing(&uri_key_arena)
	defer virtual.arena_destroy(&uri_key_arena)
	uri_key_allocator := virtual.arena_allocator(&uri_key_arena)
	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(remote_candidates),
		uri_key_allocator,
	)

	work_arena: virtual.Arena
	_ = virtual.arena_init_growing(&work_arena)
	defer virtual.arena_destroy(&work_arena)
	temp_allocator := virtual.arena_allocator(&work_arena)

	added := false
	for candidate in remote_candidates {
		candidate_temp := virtual.arena_temp_begin(&work_arena)
		defer virtual.arena_temp_end(candidate_temp)

		file_names := local_export_candidate_file_names(candidate, temp_allocator)
		if len(file_names) == 0 {
			continue
		}
		paths := make([dynamic]string, 0, 2, temp_allocator)
		for root in roots {
			collect_local_export_candidate_paths(root, file_names[:], &paths, temp_allocator)
		}
		for path in paths {
			if project_input_uri_key_exists(&uri_keys, path, uri_key_allocator) {
				continue
			}
			source, ok := read_text_file(path, temp_allocator)
			if !ok {
				continue
			}
			file_extension := strings.trim_prefix(filepath.ext(path), ".")
			input_source: string
			if dependency_source_is_xml("", file_extension, source) {
				input_source = dependency_input_source(
					candidate,
					candidate.name,
					"",
					file_extension,
					source,
					temp_allocator,
				)
			} else if !local_export_abap_source_matches(candidate, source) {
				continue
			} else {
				input_source = source
			}
			if !project_input_uri_key_add_if_missing(&uri_keys, path) {
				continue
			}
			object_kind, object_type := local_export_object_kind_type(
				candidate,
				file_extension,
				source,
				temp_allocator,
			)
			store_local_export_dependency(
				store,
				profile,
				candidate,
				path,
				roots,
				source,
				file_extension,
				allocator,
			)
			if dependency_summaries != nil {
				summary_payload := dependency_interface_summary_payload_from_artifact(
					object_kind,
					candidate.name,
					path,
					object_type,
					file_extension,
					source,
					temp_allocator,
				)
				if summary_input, summary_ok := dependency_summary_input_from_payload(
					   summary_payload,
					   candidate,
					   "",
					   dependency_summaries.allocator,
				   );
				   summary_ok {
					append(dependency_summaries, summary_input)
					added = true
					continue
				}
				continue
			}
			input := analyze.Source_Input {
				uri    = path,
				source = input_source,
				role = .Dependency_Interface_Source,
			}
			append_dependency_input(candidates, dependencies, input, candidate, candidate.name)
			added = true
		}
	}
	return added
}

store_local_export_dependency :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	candidate: deps.Remote_Dependency_Candidate,
	path: string,
	roots: []string,
	source: string,
	file_extension: string,
	allocator: mem.Allocator,
) {
	if store == nil || profile == nil {
		return
	}
	store_arena: mem.Dynamic_Arena
	store_backing := base_runtime.heap_allocator()
	mem.dynamic_arena_init(&store_arena, store_backing, store_backing, alignment = 64)
	defer mem.dynamic_arena_destroy(&store_arena)
	store_allocator := mem.dynamic_arena_allocator(&store_arena)
	object_kind, object_type := local_export_object_kind_type(
		candidate,
		file_extension,
		source,
		store_allocator,
	)
	package_name := local_export_package_name(path, roots, store_allocator)
	if package_name == "" {
		package_name = candidate.name
	}
	source_text := source
	extension := file_extension if file_extension != "" else "abap"
	if dependency_source_is_xml(object_kind, file_extension, source) &&
	   dependency_object_kind_is_ddic(object_kind) {
		extension = "xml"
	}
	fetched_at, _ := time.time_to_rfc3339(time.now(), allocator = store_allocator)
	artifact := dep_store.Stored_Artifact_Input {
		package_name    = package_name,
		object_kind     = object_kind,
		object_name     = candidate.name,
		object_uri      = path,
		object_type     = object_type,
		description     = "Local export dependency",
		file_extension  = extension,
		source_text     = source_text,
		fetched_at      = fetched_at,
		summary_payload = dependency_interface_summary_payload_from_artifact(
			object_kind,
			candidate.name,
			path,
			object_type,
			extension,
			source_text,
			store_allocator,
		),
	}
	_, _ = dep_store.put_artifact(store, profile, &artifact, store_allocator)
}

read_text_file :: proc(path: string, allocator: mem.Allocator) -> (string, bool) {
	data, err := os.read_entire_file(path, allocator)
	if err != nil {
		return "", false
	}
	return string(data), true
}

local_export_object_kind_type :: proc(
	candidate: deps.Remote_Dependency_Candidate,
	file_extension: string,
	source: string,
	allocator: mem.Allocator,
) -> (
	string,
	string,
) {
	if dependency_source_is_xml("", file_extension, source) {
		if candidate.kind == .Message_Class {
			return "message-class", "MSAG/N"
		}
		object_type := local_export_xml_attr(source, "adtcore:type", allocator)
		if object_type == "" {
			object_type = local_export_xml_attr(source, "type", allocator)
		}
		if object_type == "" {
			object_type = "TABL/DS"
		}
		return adt.infer_ddic_manifest_kind(&adt.Object_Ref{object_type = object_type}),
			object_type
	}
	switch candidate.kind {
	case .Include:
		return "include", "PROG/I"
	case .Report:
		return "report", "PROG/P"
	case .Function:
		return "function-module", "FUGR/FF"
	case .Message_Class, .Static, .Type, .Symbol:
		return "global-class", "CLAS/OC"
	}
	return "global-class", "CLAS/OC"
}

local_export_xml_attr :: proc(source, attr: string, allocator: mem.Allocator) -> string {
	needle := strings.concatenate({attr, "=\""}, allocator)
	start := strings.index(source, needle)
	if start < 0 {
		return ""
	}
	value_start := start + len(needle)
	end := strings.index_byte(source[value_start:], '"')
	if end < 0 {
		return ""
	}
	return strings.clone(source[value_start:value_start + end], allocator)
}

local_export_package_name :: proc(
	path: string,
	roots: []string,
	allocator: mem.Allocator,
) -> string {
	for root in roots {
		rel, err := filepath.rel(root, path, allocator)
		if err != .None {
			continue
		}
		normalized, normalize_err := filepath.replace_separators(rel, '/', allocator)
		if normalize_err != nil {
			continue
		}
		component, component_ok := strings.split_by_byte_iterator(&normalized, '/')
		if !component_ok {
			continue
		}
		if component == "" || component == "." || component == ".." {
			continue
		}
		if decoded, ok := net_url.percent_decode(component, allocator); ok {
			return decoded
		}
		return strings.clone(component, allocator)
	}
	return ""
}

collect_local_export_candidate_paths :: proc(
	root: string,
	file_names: []string,
	out: ^[dynamic]string,
	allocator: mem.Allocator,
) {
	entries, err := os.read_all_directory_by_path(root, allocator)
	if err != nil {
		return
	}
	for entry in entries {
		#partial switch entry.type {
		case .Directory:
			collect_local_export_candidate_paths(entry.fullpath, file_names, out, allocator)
		case .Regular:
			file_name := canonical_name(filepath.base(entry.fullpath), allocator)
			for wanted in file_names {
				if file_name == wanted && string_list_index(out^[:], entry.fullpath) < 0 {
					append(out, strings.clone(entry.fullpath, allocator))
				}
			}
		}
	}
}

local_export_candidate_file_names :: proc(
	candidate: deps.Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) -> [dynamic]string {
	names := make([dynamic]string, 0, 2, allocator)
	upper := strings.to_upper(candidate.name, allocator)
	encoded := net_url.percent_encode(upper, allocator)
	if encoded == "" {
		return names
	}
	switch candidate.kind {
	case .Include, .Function, .Static, .Report:
		append(&names, local_export_file_name(encoded, "abap", allocator))
	case .Message_Class:
		append(&names, local_export_file_name(encoded, "xml", allocator))
	case .Symbol, .Type:
		append(&names, local_export_file_name(encoded, "xml", allocator))
		append(&names, local_export_file_name(encoded, "abap", allocator))
	}
	return names
}

local_export_file_name :: proc(encoded, extension: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, encoded)
	strings.write_byte(&out, '.')
	strings.write_string(&out, extension)
	return canonical_name(strings.to_string(out), allocator)
}

local_export_abap_source_matches :: proc(
	candidate: deps.Remote_Dependency_Candidate,
	source: string,
) -> bool {
	if candidate.kind != .Static {
		return true
	}
	return source_declares_class_or_interface(source, candidate.name)
}

source_declares_class_or_interface :: proc(source, name: string) -> bool {
	lines := source
	for line in strings.split_lines_iterator(&lines) {
		trimmed := strings.trim_left_space(line)
		if strings.has_prefix(trimmed, "*") || strings.has_prefix(trimmed, "\"") {
			continue
		}
		words := trimmed
		keyword, keyword_ok := strings.fields_iterator(&words)
		decl_name, name_ok := strings.fields_iterator(&words)
		if !keyword_ok || !name_ok || !strings.equal_fold(trim_decl_token(decl_name), name) {
			continue
		}
		if strings.equal_fold(keyword, "INTERFACE") {
			return true
		}
		next, next_ok := strings.fields_iterator(&words)
		if strings.equal_fold(keyword, "CLASS") &&
		   next_ok &&
		   strings.equal_fold(trim_decl_token(next), "DEFINITION") {
			return true
		}
	}
	return false
}

trim_decl_token :: proc(token: string) -> string {
	end := len(token)
	for end > 0 && (token[end - 1] == '.' || token[end - 1] == ':') {
		end -= 1
	}
	return token[:end]
}

dependency_source_is_xml :: proc(object_kind, file_extension, source: string) -> bool {
	if dependency_file_extension_is_xml(file_extension) {
		return true
	}
	if dependency_file_extension_is_abap(file_extension) &&
	   !dependency_object_kind_is_ddic(object_kind) {
		return false
	}
	return strings.has_prefix(source, "<")
}

dependency_object_kind_is_ddic :: proc(object_kind: string) -> bool {
	return len(object_kind) >= 5 && strings.equal_fold(object_kind[:5], "ddic-")
}

dependency_file_extension_is_xml :: proc(file_extension: string) -> bool {
	ext := file_extension
	if strings.has_prefix(ext, ".") {
		ext = ext[1:]
	}
	return strings.equal_fold(ext, "xml")
}

dependency_file_extension_is_abap :: proc(file_extension: string) -> bool {
	ext := file_extension
	if strings.has_prefix(ext, ".") {
		ext = ext[1:]
	}
	return strings.equal_fold(ext, "abap")
}

@(private)
canonical_name :: #force_inline proc(name: string, allocator: mem.Allocator) -> string {
	return strings.to_lower(name, allocator)
}

@(private)
string_list_index :: proc(values: []string, name: string) -> int {
	for value, i in values {
		if value == name {
			return i
		}
	}
	return -1
}
