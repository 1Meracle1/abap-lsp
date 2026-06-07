#+private
package abap_frontend_remote_dependencies

import "src:adt"
import dep_store "src:dependency_store"
import uri_key "src:uri_key"

import net_url "core:net"
import "core:mem"
import "core:os"
import filepath "core:path/filepath"
import "core:slice"
import "core:strings"

store_candidate_kind :: proc(
	kind: Remote_Dependency_Kind,
) -> dep_store.Candidate_Kind {
	switch kind {
	case .Include:
		return .Include
	case .Message_Class:
		return .Message_Class
	case .Report:
		return .Report
	case .Function:
		return .Function
	case .Class, .Interface:
		return .Static
	case .Type:
		return .Type
	case .Symbol:
		return .Symbol
	}
	return .Symbol
}

remote_dependency_kind_text :: proc(kind: Remote_Dependency_Kind) -> string {
	return dep_store.candidate_kind_text(store_candidate_kind(kind))
}

request_kind_text :: proc(request: Request) -> string {
	if request.kind == .Interface {
		return "interface-type"
	}
	if request.kind == .Class {
		return "object-type"
	}
	if request.kind == .Type {
		return "ddic-type"
	}
	return remote_dependency_kind_text(request.kind)
}

object_role :: proc(
	object_kind, object_type: string,
) -> Remote_Dependency_Object_Role {
	kind := strings.to_lower(strings.trim_space(object_kind), context.temp_allocator)
	obj_type := strings.to_lower(strings.trim_space(object_type), context.temp_allocator)
	switch kind {
	case "report":
		return .Report
	case "function-module", "function-group":
		return .Function_Module
	case "global-class":
		return .Class
	case "global-interface":
		return .Interface
	case TYPEPOOL_OBJECT_KIND:
		return .Type_Pool
	case "ddic-table", "ddic-structure", "ddic-view":
		return .DDIC_Table
	case "ddic-data-element", "ddic-domain", "ddic-table-type":
		return .DDIC_Type
	}
	switch {
	case strings.has_prefix(obj_type, "clas/"):
		return .Class
	case strings.has_prefix(obj_type, "intf/"):
		return .Interface
	case strings.has_prefix(obj_type, "fugr/"):
		return .Function_Module
	case obj_type == "prog/p":
		return .Report
	case obj_type == "prog/i":
		return .Unknown
	case obj_type == strings.to_lower(TYPEPOOL_OBJECT_TYPE, context.temp_allocator):
		return .Type_Pool
	}
	if object_kind_is_ddic(kind) {
		return .DDIC_Type
	}
	return .Unknown
}

role_request_kind :: proc(
	role: Remote_Dependency_Object_Role,
) -> Remote_Dependency_Kind {
	switch role {
	case .Report:
		return .Report
	case .Function_Module:
		return .Function
	case .Class:
		return .Class
	case .Interface:
		return .Interface
	case .DDIC_Type, .DDIC_Table, .Type_Pool:
		return .Type
	case .Unknown:
	}
	return .Symbol
}

artifact_path :: proc(
	artifact: ^Artifact,
	allocator: mem.Allocator,
) -> string {
	if artifact.object_uri != "" {
		uri := strings.trim_space(artifact.object_uri)
		if strings.has_prefix(uri, "abapls-") || strings.has_prefix(uri, "file:") {
			return strings.clone(uri, allocator)
		}
	}
	switch artifact.source_kind {
	case .ADT:
		return remote_dependency_adt_uri(
			artifact.object_uri,
			artifact.file_extension,
			allocator,
		)
	case .Cache:
		return remote_dependency_cache_uri(artifact, allocator)
	case .Type_Pool:
		return typepool_dependency_uri(artifact.object_name, allocator)
	case .Local_Export:
		return strings.clone(artifact.object_uri, allocator)
	case .Unknown:
	}
	return remote_dependency_cache_uri(artifact, allocator)
}

remote_dependency_cache_uri :: proc(
	artifact: ^Artifact,
	allocator: mem.Allocator,
) -> string {
	if strings.equal_fold(artifact.object_kind, TYPEPOOL_OBJECT_KIND) {
		return typepool_dependency_uri(artifact.object_name, allocator)
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-cache:/")
	strings.write_string(&out, strings.to_lower(artifact.object_kind, allocator))
	strings.write_byte(&out, '/')
	name := artifact.object_name
	if name == "" {
		name = artifact.request.name
	}
	strings.write_string(&out, strings.to_lower(name, allocator))
	strings.write_string(&out, ".abap")
	return strings.to_string(out)
}

remote_dependency_adt_uri :: proc(
	uri, file_extension: string,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-adt:")
	strings.write_string(&out, uri)
	ext := strings.trim_prefix(file_extension, ".")
	if ext != "" && strings.index_byte(uri, '.') < 0 {
		strings.write_byte(&out, '.')
		strings.write_string(&out, ext)
	}
	return strings.to_string(out)
}

result_uri_add_if_missing :: proc(
	state: ^State,
	uri: string,
) -> bool {
	if state == nil || state.seen_result_uris == nil {
		return true
	}
	key := uri_key.normalized_uri_path_key(uri, state.seen_result_uris.allocator)
	if key in state.seen_result_uris {
		return false
	}
	state.seen_result_uris[key] = true
	return true
}

source_is_xml :: proc(object_kind, file_extension, source: string) -> bool {
	if remote_dependency_file_extension_is_xml(file_extension) {
		return true
	}
	if remote_dependency_file_extension_is_abap(file_extension) &&
	   !object_kind_is_ddic(object_kind) {
		return false
	}
	return strings.has_prefix(strings.trim_left_space(source), "<")
}

object_kind_is_ddic :: proc(object_kind: string) -> bool {
	kind := strings.to_lower(strings.trim_space(object_kind), context.temp_allocator)
	return len(kind) >= 5 && kind[:5] == "ddic-"
}

remote_dependency_file_extension_is_xml :: proc(file_extension: string) -> bool {
	ext := strings.trim_prefix(file_extension, ".")
	return strings.equal_fold(ext, "xml")
}

remote_dependency_file_extension_is_abap :: proc(file_extension: string) -> bool {
	ext := strings.trim_prefix(file_extension, ".")
	return strings.equal_fold(ext, "abap")
}

remote_dependency_file_extension_is_ddic :: proc(file_extension: string) -> bool {
	ext := strings.trim_prefix(file_extension, ".")
	return strings.equal_fold(ext, "ddic")
}

candidate_for_object :: proc(
	object_kind, object_name: string,
) -> (Request, bool) {
	name := strings.trim_space(object_name)
	kind := strings.to_lower(strings.trim_space(object_kind), context.temp_allocator)
	if name == "" || kind == "" {
		return {}, false
	}
	request := Request{name = name}
	switch kind {
	case "include":
		request.kind = .Include
	case "message-class":
		request.kind = .Message_Class
	case "report":
		request.kind = .Report
	case "function-module", "function-group":
		request.kind = .Function
	case "global-interface":
		request.kind = .Interface
	case "global-class":
		request.kind = .Class
	case TYPEPOOL_OBJECT_KIND:
		request.kind = .Type
	case:
		if object_kind_is_ddic(kind) {
			request.kind = .Type
		} else {
			return {}, false
		}
	}
	return normalize_request(request, context.temp_allocator)
}

open_adt_artifact_matches_object_kind :: proc(
	artifact: ^Artifact,
	object_kind: string,
) -> bool {
	kind := strings.to_lower(strings.trim_space(object_kind), context.temp_allocator)
	object_type := strings.to_lower(artifact.object_type, context.temp_allocator)
	switch kind {
	case "global-class":
		return strings.has_prefix(object_type, "clas/")
	case "global-interface":
		return strings.has_prefix(object_type, "intf/")
	case "include":
		return object_type == "prog/i"
	case "report":
		return object_type == "prog/p"
	case "function-module":
		return object_type == "fugr/ff"
	case "function-group":
		return object_type == "fugr/f"
	case "message-class":
		return object_type == "msag/n"
	case TYPEPOOL_OBJECT_KIND:
		return object_type == strings.to_lower(TYPEPOOL_OBJECT_TYPE, context.temp_allocator)
	case:
		if object_kind_is_ddic(kind) {
			return strings.equal_fold(adt.infer_ddic_manifest_kind_from_object_type(artifact.object_type), kind)
		}
	}
	return false
}

read_text_file :: proc(path: string, allocator: mem.Allocator) -> (string, bool) {
	data, err := os.read_entire_file(path, allocator)
	if err != nil {
		return "", false
	}
	return string(data), true
}

local_export_candidate_file_names :: proc(
	request: Request,
	allocator: mem.Allocator,
) -> [dynamic]string {
	names := make([dynamic]string, 0, 2, allocator)
	upper := strings.to_upper(request.name, allocator)
	encoded := net_url.percent_encode(upper, allocator)
	if encoded == "" {
		return names
	}
	switch request.kind {
	case .Include, .Function, .Class, .Interface, .Report:
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
	return strings.to_lower(strings.to_string(out), allocator)
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
			file_name := strings.to_lower(filepath.base(entry.fullpath), allocator)
			for wanted in file_names {
				if file_name == wanted && !slice.contains(out^[:], entry.fullpath) {
					append(out, strings.clone(entry.fullpath, allocator))
				}
			}
		}
	}
}

local_export_abap_source_matches :: proc(
	request: Request,
	source: string,
) -> bool {
	if request.kind != .Class && request.kind != .Interface {
		return true
	}
	return source_declares_class_or_interface(source, request.name)
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

local_export_object_kind_type :: proc(
	request: Request,
	file_extension: string,
	source: string,
	allocator: mem.Allocator,
) -> (string, string) {
	if source_is_xml("", file_extension, source) {
		if request.kind == .Message_Class {
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
	switch request.kind {
	case .Include:
		return "include", "PROG/I"
	case .Report:
		return "report", "PROG/P"
	case .Function:
		return "function-module", "FUGR/FF"
	case .Interface:
		return "global-interface", "INTF/OI"
	case .Message_Class, .Class, .Type, .Symbol:
		if source_declares_interface(source, request.name) {
			return "global-interface", "INTF/OI"
		}
		return "global-class", "CLAS/OC"
	}
	return "global-class", "CLAS/OC"
}

source_declares_interface :: proc(source, name: string) -> bool {
	lines := source
	for line in strings.split_lines_iterator(&lines) {
		trimmed := strings.trim_left_space(line)
		if strings.has_prefix(trimmed, "*") || strings.has_prefix(trimmed, "\"") {
			continue
		}
		words := trimmed
		keyword, keyword_ok := strings.fields_iterator(&words)
		decl_name, name_ok := strings.fields_iterator(&words)
		if !keyword_ok || !name_ok {
			continue
		}
		if strings.equal_fold(keyword, "INTERFACE") &&
		   strings.equal_fold(trim_decl_token(decl_name), name) {
			return true
		}
	}
	return false
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
