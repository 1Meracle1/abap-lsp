package abap_frontend_semantic

import dep_store "../dependency_store"
import "../adt"

import "core:mem"
import "core:os"
import "core:strings"

Remote_Dependency_Candidate :: struct {
	name: string,
	kind: string,
}

analyze_with_manifest_dependency_drain :: proc(
	manifest: ^Workspace_Manifest,
	target: Source_Input,
	candidates: [dynamic]Project_Candidate_Input,
	dependencies: [dynamic]Source_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	candidate_inputs := candidates
	dependency_inputs := dependencies
	project := analyze_target_with_candidate_inputs(target, candidate_inputs[:], dependency_inputs[:], options, allocator)
	store, has_store := manifest_dependency_store(manifest, options, allocator)
	roots := manifest_local_export_roots(manifest, allocator)
	has_adt := manifest_has_project_dotenv(manifest, allocator)
	if !has_store && len(roots) == 0 && !has_adt {
		return project
	}

	seen_artifacts := make(map[i64]bool, 16, allocator)
	for {
		remote_candidates := collect_project_remote_dependency_candidates(&project, allocator)
		added := false
		if has_store {
			added = add_dependency_store_matches(
				&candidate_inputs,
				&dependency_inputs,
				remote_candidates[:],
				&store,
				&manifest.dependency_store,
				&seen_artifacts,
				target.uri,
				allocator,
			)
		}
		if !added && len(roots) > 0 {
			added = add_local_export_matches(
				&candidate_inputs,
				&dependency_inputs,
				remote_candidates[:],
				roots[:],
				target.uri,
				allocator,
			)
		}
		if !added && has_adt {
			added = add_adt_matches(
				&candidate_inputs,
				&dependency_inputs,
				remote_candidates[:],
				manifest,
				target.uri,
				allocator,
			)
		}
		if !added {
			break
		}
		project = analyze_target_with_candidate_inputs(target, candidate_inputs[:], dependency_inputs[:], options, allocator)
	}
	return project
}

analyze_standalone_with_dependency_drain :: proc(
	target: Source_Input,
	candidates: [dynamic]Project_Candidate_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	candidate_inputs := candidates
	dependency_inputs := make([dynamic]Source_Input, 0, 4, allocator)
	project := analyze_target_with_candidate_inputs(target, candidate_inputs[:], dependency_inputs[:], options, allocator)
	store, err := dep_store.dependency_store_from_override_path(options.dependency_store_path, allocator)
	if err != .None {
		return project
	}

	seen_artifacts := make(map[i64]bool, 16, allocator)
	// Any-profile standalone cache lookup has no product/package boundary; keep it direct.
	remote_candidates := collect_project_remote_dependency_candidates(&project, allocator)
	if add_dependency_store_any_profile_matches(
		&candidate_inputs,
		&dependency_inputs,
		remote_candidates[:],
		&store,
		&seen_artifacts,
		target.uri,
		allocator,
	) {
		project = analyze_target_with_candidate_inputs(target, candidate_inputs[:], dependency_inputs[:], options, allocator)
	}
	return project
}

manifest_dependency_store :: proc(
	manifest: ^Workspace_Manifest,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> (dep_store.Dependency_Store, bool) {
	if !manifest.has_dependency_store {
		return {}, false
	}
	store, err := dep_store.dependency_store_from_override_path(options.dependency_store_path, allocator)
	return store, err == .None
}

manifest_local_export_roots :: proc(
	manifest: ^Workspace_Manifest,
	allocator: mem.Allocator,
) -> [dynamic]string {
	roots := make([dynamic]string, 0, len(manifest.local_export_roots), allocator)
	if strings.to_lower(strings.trim_space(manifest.dependency_source), allocator) == "adt-first" {
		return roots
	}
	for root in manifest.local_export_roots {
		path, ok := manifest_absolute_path(manifest.root_path, root, allocator)
		if ok {
			append(&roots, path)
		}
	}
	return roots
}

collect_project_remote_dependency_candidates :: proc(
	project: ^Project_Analysis,
	allocator: mem.Allocator,
) -> [dynamic]Remote_Dependency_Candidate {
	out := make([dynamic]Remote_Dependency_Candidate, 0, 8, allocator)
	for unit in project.units {
		for edge in unit.include_edges {
			if !edge.has_target && is_remote_lookup_candidate(edge.name, "include") {
				insert_remote_candidate(&out, edge.name, "include", allocator)
			}
		}
		for ref in unit.references {
			if ref.has_resolution && ref.resolution.kind != .External {
				continue
			}
			if candidate, ok := remote_dependency_candidate_for_reference(ref); ok {
				insert_remote_candidate(&out, candidate.name, candidate.kind, allocator)
			}
		}
		for symbol in unit.symbols {
			if symbol.decl_range.start == symbol.decl_range.end && symbol.has_declared_type &&
			   symbol.declared_type.namespace == .Type &&
			   is_remote_lookup_candidate_after_local_resolution(symbol.declared_type.base_name, "type") {
				insert_remote_candidate(&out, symbol.declared_type.base_name, "type", allocator)
			}
		}
		if unit.has_message_default_class {
			insert_message_class_candidate(&out, unit.message_default_class.name, allocator)
		}
		for message in unit.message_uses {
			if message.class_name != "" {
				insert_message_class_candidate(&out, message.class_name, allocator)
			}
		}
		for sql_source in unit.sql_sources {
			if sql_source.resolution == .External && is_remote_lookup_candidate(sql_source.name, "type") {
				insert_remote_candidate(&out, sql_source.name, "type", allocator)
			}
		}
		for call_site in unit.call_sites {
			#partial switch call_site.target.kind {
			case .Function:
				if is_remote_lookup_candidate_after_local_resolution(call_site.target.function_name, "function") {
					insert_remote_candidate(&out, call_site.target.function_name, "function", allocator)
				}
			case .Report:
				if is_remote_lookup_candidate_after_local_resolution(call_site.target.report_name, "report") {
					insert_remote_candidate(&out, call_site.target.report_name, "report", allocator)
				}
			}
		}
	}
	return out
}

remote_dependency_candidate_for_reference :: proc(ref: Reference_Data) -> (
	Remote_Dependency_Candidate,
	bool,
) {
	kind := ""
	after_local := false
	switch ref.kind {
	case .Include, .Structured_Decl_End:
		return {}, false
	case .Static_Target:
		kind = "static"
		after_local = true
	case .Type_Ref:
		kind = "type"
		after_local = true
	case .Message_Class:
		kind = "message-class"
	case .Routine_Call:
		if ref.namespace == .Routine {
			kind = "function"
			after_local = true
		} else {
			kind = "symbol"
		}
	case .Identifier:
		kind = "symbol"
	}
	if after_local {
		if !is_remote_lookup_candidate_after_local_resolution(ref.name, kind) {
			return {}, false
		}
	} else if !is_remote_lookup_candidate(ref.name, kind) {
		return {}, false
	}
	return Remote_Dependency_Candidate{name = ref.name, kind = kind}, true
}

insert_message_class_candidate :: proc(
	out: ^[dynamic]Remote_Dependency_Candidate,
	name: string,
	allocator: mem.Allocator,
) {
	if is_remote_lookup_candidate(name, "message-class") {
		insert_remote_candidate(out, name, "message-class", allocator)
	}
}

insert_remote_candidate :: proc(
	out: ^[dynamic]Remote_Dependency_Candidate,
	name, kind: string,
	allocator: mem.Allocator,
) {
	normalized_name := canonical_name(strings.trim_space(name), allocator)
	if normalized_name == "" {
		return
	}
	normalized_kind := canonical_name(strings.trim_space(kind), allocator)
	for existing, i in out^ {
		if existing.name == normalized_name {
			if remote_candidate_kind_priority(normalized_kind) > remote_candidate_kind_priority(existing.kind) {
				out^[i].kind = normalized_kind
			}
			return
		}
	}
	append(out, Remote_Dependency_Candidate{name = normalized_name, kind = normalized_kind})
}

remote_candidate_kind_priority :: proc(kind: string) -> int {
	if kind == "message-class" {return 5}
	if kind == "include" || kind == "function" {return 4}
	if kind == "static" {return 3}
	if kind == "type" {return 2}
	return 1
}

add_dependency_store_matches :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	seen_artifacts: ^map[i64]bool,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	added := false
	for candidate in remote_candidates {
		record, ok, err := dep_store.find_artifact_for_candidate(
			store,
			profile,
			candidate.name,
			candidate.kind,
			allocator,
		)
		if err != .None || !ok || record.artifact_id in seen_artifacts^ {
			continue
		}
		seen_artifacts^[record.artifact_id] = true
		input := source_input_from_dependency_record(&record, candidate, allocator)
		if project_input_uri_exists(target_uri, dependencies^[:], candidates^[:], input.uri, allocator) {
			continue
		}
		append_dependency_input(candidates, dependencies, input, candidate, record.object_name, allocator)
		added = true
	}
	return added
}

add_dependency_store_any_profile_matches :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	seen_artifacts: ^map[i64]bool,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	added := false
	reader, reader_err := dep_store.reader(store, allocator)
	if reader_err != .None {
		return false
	}
	defer dep_store.reader_destroy(&reader)
	for candidate in remote_candidates {
		record, ok, err := dep_store.reader_find_artifact_for_candidate_any_profile(
			&reader,
			candidate.name,
			candidate.kind,
			allocator,
		)
		if err != .None || !ok || record.artifact_id in seen_artifacts^ {
			continue
		}
		seen_artifacts^[record.artifact_id] = true
		input := source_input_from_dependency_record(&record, candidate, allocator)
		if project_input_uri_exists(target_uri, dependencies^[:], candidates^[:], input.uri, allocator) {
			continue
		}
		append_dependency_input(candidates, dependencies, input, candidate, record.object_name, allocator)
		added = true
	}
	return added
}

source_input_from_dependency_record :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	candidate: Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) -> Source_Input {
	source := record.source_text
	if source_looks_xml(source) {
		source = synthetic_dependency_source(record.object_name, candidate.kind, allocator)
	}
	return Source_Input {
		uri = dependency_record_uri(record, allocator),
		source = strings.clone(source, allocator),
	}
}

dependency_record_uri :: proc(record: ^dep_store.Stored_Artifact_Record, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-cache:/")
	strings.write_string(&out, record.object_kind)
	strings.write_byte(&out, '/')
	strings.write_string(&out, record.object_name)
	strings.write_string(&out, ".abap")
	return strings.to_string(out)
}

add_local_export_matches :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	roots: []string,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	added := false
	for candidate in remote_candidates {
		file_names := local_export_candidate_file_names(candidate, allocator)
		if len(file_names) == 0 {
			continue
		}
		paths := make([dynamic]string, 0, 2, allocator)
		for root in roots {
			collect_local_export_candidate_paths(root, file_names[:], &paths, allocator)
		}
		for path in paths {
			if project_input_uri_exists(target_uri, dependencies^[:], candidates^[:], path, allocator) {
				continue
			}
			source, ok := read_text_file(path, allocator)
			if !ok {
				continue
			}
			if source_looks_xml(source) {
				source = synthetic_dependency_source(candidate.name, candidate.kind, allocator)
			} else if !local_export_abap_source_matches(candidate, source) {
				continue
			}
			append_dependency_input(
				candidates,
				dependencies,
				Source_Input{uri = path, source = source},
				candidate,
				candidate.name,
				allocator,
			)
			added = true
		}
	}
	return added
}

add_adt_matches :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	manifest: ^Workspace_Manifest,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	env_path, ok := manifest_project_dotenv_path(manifest, allocator)
	if !ok {
		return false
	}
	defer delete(env_path, allocator)
	dotenv, dotenv_err := adt.parse_dotenv_file(env_path, allocator)
	if dotenv_err != .None {
		return false
	}
	defer adt.dotenv_defaults_destroy(&dotenv, allocator)

	overrides := adt.Connection_Overrides{}
	config, config_err := adt.connection_config_from_sources(&overrides, &dotenv, allocator)
	if config_err != .None {
		return false
	}
	defer adt.connection_config_destroy(&config, allocator)

	client: adt.Client
	adt.client_init(&client, config)
	defer adt.client_destroy(&client, allocator)
	return add_adt_matches_with_client(candidates, dependencies, remote_candidates, &client, target_uri, allocator)
}

add_adt_matches_with_client :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	client: ^adt.Client,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	added := false
	for candidate in remote_candidates {
		objects, err := adt.search_repository_objects(client, candidate.name, 50, allocator)
		if err != .None {
			objects = adt.direct_dependency_object_refs(candidate.name, candidate.kind, allocator)
		}
		selected := adt.select_dependency_objects(candidate.name, objects[:], candidate.kind, allocator)
		if len(selected) == 0 {
			adt.object_refs_destroy(&selected, allocator)
			selected = adt.direct_dependency_object_refs(candidate.name, candidate.kind, allocator)
		}
		for &object_ref in selected {
			if !add_adt_object_match(candidates, dependencies, candidate, &object_ref, client, target_uri, allocator) {
				continue
			}
			added = true
		}
		adt.object_refs_destroy(&selected, allocator)
		adt.object_refs_destroy(&objects, allocator)
	}
	return added
}

add_adt_object_match :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	candidate: Remote_Dependency_Candidate,
	object_ref: ^adt.Object_Ref,
	client: ^adt.Client,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	fetched, err := adt.fetch_dependency_object(client, object_ref, allocator)
	if err != .None {
		return false
	}
	defer adt.dependency_fetch_result_destroy(&fetched, allocator)

	added := add_adt_fetched_dependency_input(
		candidates,
		dependencies,
		candidate,
		object_ref,
		fetched.body,
		fetched.file_extension,
		target_uri,
		allocator,
	)
	for &shared in fetched.shared_dependencies {
		shared_candidate := Remote_Dependency_Candidate{name = shared.object_ref.name, kind = "include"}
		if add_adt_fetched_dependency_input(
			candidates,
			dependencies,
			shared_candidate,
			&shared.object_ref,
			shared.body,
			shared.file_extension,
			target_uri,
			allocator,
		) {
			added = true
		}
	}
	return added
}

add_adt_fetched_dependency_input :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	candidate: Remote_Dependency_Candidate,
	object_ref: ^adt.Object_Ref,
	source: string,
	file_extension: string,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	uri := adt_dependency_uri(object_ref, file_extension, allocator)
	if project_input_uri_exists(target_uri, dependencies^[:], candidates^[:], uri, allocator) {
		delete(uri, allocator)
		return false
	}
	input_source: string
	if source_looks_xml(source) {
		input_source = synthetic_dependency_source(object_ref.name, candidate.kind, allocator)
	} else {
		input_source = strings.clone(source, allocator)
	}
	append_dependency_input(
		candidates,
		dependencies,
		Source_Input{uri = uri, source = input_source},
		candidate,
		object_ref.name,
		allocator,
	)
	return true
}

adt_dependency_uri :: proc(
	object_ref: ^adt.Object_Ref,
	file_extension: string,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-adt:")
	strings.write_string(&out, object_ref.uri)
	ext := strings.trim_space(file_extension)
	if ext != "" && strings.index_byte(object_ref.uri, '.') < 0 {
		strings.write_byte(&out, '.')
		strings.write_string(&out, ext)
	}
	return strings.to_string(out)
}

manifest_has_project_dotenv :: proc(manifest: ^Workspace_Manifest, allocator: mem.Allocator) -> bool {
	path, ok := manifest_project_dotenv_path(manifest, allocator)
	if ok {
		delete(path, allocator)
	}
	return ok
}

manifest_project_dotenv_path :: proc(
	manifest: ^Workspace_Manifest,
	allocator: mem.Allocator,
) -> (string, bool) {
	path, ok := join_path2(manifest.root_path, ".env", allocator)
	if !ok {
		return "", false
	}
	info, err := os.stat(path, allocator)
	if err == nil && info.type == .Regular {
		return path, true
	}
	delete(path, allocator)
	return "", false
}

append_dependency_input :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	input: Source_Input,
	candidate: Remote_Dependency_Candidate,
	object_name: string,
	allocator: mem.Allocator,
) {
	if candidate.kind == "include" {
		append(
			candidates,
			Project_Candidate_Input {
				input = input,
				object_name = strings.clone(object_name if object_name != "" else candidate.name, allocator),
			},
		)
	} else {
		append(dependencies, input)
	}
}

project_input_uri_exists :: proc(
	target_uri: string,
	dependencies: []Source_Input,
	candidates: []Project_Candidate_Input,
	uri: string,
	allocator: mem.Allocator,
) -> bool {
	key := normalized_uri_path_key(uri, allocator)
	if normalized_uri_path_key(target_uri, allocator) == key {
		return true
	}
	for input in dependencies {
		if normalized_uri_path_key(input.uri, allocator) == key {
			return true
		}
	}
	for candidate in candidates {
		if normalized_uri_path_key(candidate.input.uri, allocator) == key {
			return true
		}
	}
	return false
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
	defer os.file_info_slice_delete(entries, allocator)
	for entry in entries {
		#partial switch entry.type {
		case .Directory:
			collect_local_export_candidate_paths(entry.fullpath, file_names, out, allocator)
		case .Regular:
			file_name := canonical_name(path_file_name(entry.fullpath), allocator)
			for wanted in file_names {
				if file_name == wanted && !string_list_contains(out^[:], entry.fullpath) {
					append(out, strings.clone(entry.fullpath, allocator))
				}
			}
		}
	}
}

local_export_candidate_file_names :: proc(
	candidate: Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) -> [dynamic]string {
	names := make([dynamic]string, 0, 2, allocator)
	encoded := encode_local_export_component(candidate.name, allocator)
	if encoded == "" {
		return names
	}
	switch candidate.kind {
	case "include", "function", "static", "report":
		append(&names, local_export_file_name(encoded, "abap", allocator))
	case "message-class":
		append(&names, local_export_file_name(encoded, "xml", allocator))
	case "symbol", "type":
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

local_export_abap_source_matches :: proc(candidate: Remote_Dependency_Candidate, source: string) -> bool {
	if candidate.kind != "static" {
		return true
	}
	return source_declares_class_or_interface(source, candidate.name)
}

source_declares_class_or_interface :: proc(source, name: string) -> bool {
	rest := source
	for rest != "" {
		line: string
		line, rest = split_line(rest)
		trimmed := trim_left_ascii_ws(line)
		if strings.has_prefix(trimmed, "*") || strings.has_prefix(trimmed, "\"") {
			continue
		}
		keyword, after_keyword, keyword_ok := split_first_word(trimmed)
		decl_name, rest2, name_ok := split_first_word(after_keyword)
		if !keyword_ok || !name_ok || !ascii_equal_ignore_case(trim_decl_token(decl_name), name) {
			continue
		}
		if ascii_equal_ignore_case(keyword, "INTERFACE") {
			return true
		}
		next, _, next_ok := split_first_word(rest2)
		if ascii_equal_ignore_case(keyword, "CLASS") && next_ok && ascii_equal_ignore_case(next, "DEFINITION") {
			return true
		}
	}
	return false
}

split_first_word :: proc(text: string) -> (string, string, bool) {
	trimmed := trim_left_ascii_ws(text)
	if trimmed == "" {
		return "", "", false
	}
	end := 0
	for end < len(trimmed) && trimmed[end] != ' ' && trimmed[end] != '\t' && trimmed[end] != '\r' && trimmed[end] != '\n' {
		end += 1
	}
	return trimmed[:end], trimmed[end:], true
}

trim_decl_token :: proc(token: string) -> string {
	end := len(token)
	for end > 0 && (token[end - 1] == '.' || token[end - 1] == ':') {
		end -= 1
	}
	return token[:end]
}

split_line :: proc(text: string) -> (string, string) {
	for i in 0 ..< len(text) {
		if text[i] == '\n' {
			line := text[:i]
			if len(line) > 0 && line[len(line) - 1] == '\r' {
				line = line[:len(line) - 1]
			}
			return line, text[i + 1:]
		}
	}
	return text, ""
}

trim_left_ascii_ws :: proc(text: string) -> string {
	i := 0
	for i < len(text) && (text[i] == ' ' || text[i] == '\t' || text[i] == '\r' || text[i] == '\n') {
		i += 1
	}
	return text[i:]
}

source_looks_xml :: proc(source: string) -> bool {
	return strings.has_prefix(strings.trim_space(source), "<")
}

synthetic_dependency_source :: proc(name, kind: string, allocator: mem.Allocator) -> string {
	n := canonical_name(name, allocator)
	out := strings.builder_make(allocator)
	switch kind {
	case "function":
		strings.write_string(&out, "FUNCTION ")
		strings.write_string(&out, n)
		strings.write_string(&out, ".\nENDFUNCTION.\n")
	case "static":
		strings.write_string(&out, "CLASS ")
		strings.write_string(&out, n)
		strings.write_string(&out, " DEFINITION.\nENDCLASS.\nCLASS ")
		strings.write_string(&out, n)
		strings.write_string(&out, " IMPLEMENTATION.\nENDCLASS.\n")
	case "report":
		strings.write_string(&out, "REPORT ")
		strings.write_string(&out, n)
		strings.write_string(&out, ".\n")
	case:
		strings.write_string(&out, "TYPES ")
		strings.write_string(&out, n)
		strings.write_string(&out, " TYPE string.\n")
	}
	return strings.to_string(out)
}

is_remote_lookup_candidate :: proc(name, kind: string) -> bool {
	trimmed := strings.trim_space(name)
	if trimmed == "" {
		return false
	}
	if is_remote_lookup_name(trimmed) {
		return true
	}
	if kind == "type" || kind == "static" || kind == "function" || kind == "report" {
		return is_standard_remote_type_like_name(trimmed)
	}
	if kind == "message-class" {
		return is_standard_message_class_name(trimmed)
	}
	return false
}

is_remote_lookup_candidate_after_local_resolution :: proc(name, kind: string) -> bool {
	trimmed := strings.trim_space(name)
	if trimmed == "" {
		return false
	}
	if is_remote_lookup_name(trimmed) {
		return true
	}
	if kind == "type" || kind == "static" || kind == "function" || kind == "report" {
		return is_standard_remote_type_like_name_after_local_resolution(trimmed)
	}
	if kind == "message-class" {
		return is_standard_message_class_name(trimmed)
	}
	return false
}

is_remote_lookup_name :: proc(name: string) -> bool {
	if name == "" {
		return false
	}
	if name[0] == '/' {
		return true
	}
	first := ascii_lower_byte(name[0])
	return first == 'z' || first == 'y'
}

is_standard_remote_type_like_name :: proc(name: string) -> bool {
	if name == "" || name[0] == '/' {
		return name != ""
	}
	if !ascii_alpha(name[0]) {
		return false
	}
	if is_likely_local_identifier_style(name) || is_likely_builtin_type_name(name) {
		return false
	}
	return ascii_name_bytes(name)
}

is_standard_remote_type_like_name_after_local_resolution :: proc(name: string) -> bool {
	if name == "" || name[0] == '/' {
		return name != ""
	}
	if !ascii_alpha(name[0]) {
		return false
	}
	if is_likely_builtin_type_name(name) {
		return false
	}
	return ascii_name_bytes(name)
}

is_standard_message_class_name :: proc(name: string) -> bool {
	if name == "" || name[0] == '/' {
		return name != ""
	}
	all_digits := true
	for b in transmute([]byte)name {
		if !ascii_digit(b) {
			all_digits = false
			break
		}
	}
	if all_digits {
		return true
	}
	if !ascii_alpha(name[0]) {
		return false
	}
	return !is_likely_local_identifier_style(name) && ascii_name_bytes(name)
}

is_likely_builtin_type_name :: proc(name: string) -> bool {
	builtins := [?]string {
		"i", "int1", "int2", "int4", "int8", "f", "p", "decfloat", "decfloat16",
		"decfloat34", "string", "c", "n", "d", "t", "x", "xstring", "data", "any",
		"abap_bool", "flag", "xfeld", "syst", "guid", "symsgv", "sydatum", "timestamp",
		"cursor", "tabname", "cdobjectcl", "rs38l_fnam", "memoryid", "time", "timestmp",
		"object", "standard", "table", "simple", "numeric", "csequence", "clike",
		"xsequence", "previous", "to",
	}
	for value in builtins {
		if ascii_equal_ignore_case(name, value) {
			return true
		}
	}
	if ascii_has_prefix_ignore_case(name, "char") {
		for i in 4 ..< len(name) {
			if !ascii_digit(name[i]) {
				return false
			}
		}
		return len(name) > 4
	}
	return false
}

is_likely_local_identifier_style :: proc(name: string) -> bool {
	prefixes := [?]string {
		"lv_", "ls_", "lt_", "lr_", "lo_", "li_", "lm_", "lx_", "lc_", "ld_",
		"gv_", "gs_", "gt_", "gr_", "go_", "gi_", "gm_", "gx_", "gc_", "gd_",
		"mv_", "ms_", "mt_", "mr_", "mo_", "mi_", "mm_", "mx_", "mc_", "md_",
		"iv_", "is_", "it_", "ir_", "io_", "ii_", "im_", "ix_", "ic_", "id_",
		"ev_", "es_", "et_", "er_", "eo_", "ei_", "em_", "ex_", "ec_", "ed_",
		"rv_", "rs_", "rt_", "rr_", "ro_", "ri_", "rm_", "rx_", "rc_", "rd_",
		"cv_", "cs_", "ct_", "cr_", "co_", "ci_", "cm_", "cc_", "cd_",
		"sv_", "ss_", "st_", "sr_", "so_", "si_", "sm_", "sx_", "sc_", "sd_",
		"tv_", "ts_", "tt_", "tr_", "to_", "ti_", "tm_", "tx_", "tc_", "td_",
		"uv_", "us_", "ut_", "ur_", "uo_", "ui_", "um_", "ux_", "uc_", "ud_",
		"wv_", "ws_", "wt_", "wr_", "wo_", "wi_", "wm_", "wx_", "wc_", "wd_",
		"xv_", "xs_", "xt_", "xr_", "xo_", "xi_", "xm_", "xx_", "xc_", "xd_",
		"yv_", "ys_", "yt_", "yr_", "yo_", "yi_", "ym_", "yx_", "yc_", "yd_",
		"zv_", "zs_", "zt_", "zr_", "zo_", "zi_", "zm_", "zx_", "zc_", "zd_",
	}
	for prefix in prefixes {
		if ascii_has_prefix_ignore_case(name, prefix) {
			return true
		}
	}
	return false
}

ascii_name_bytes :: proc(name: string) -> bool {
	for b in transmute([]byte)name {
		if !(ascii_alpha(b) || ascii_digit(b) || b == '_' || b == '/') {
			return false
		}
	}
	return true
}

ascii_has_prefix_ignore_case :: proc(text, prefix: string) -> bool {
	return len(text) >= len(prefix) && ascii_equal_ignore_case(text[:len(prefix)], prefix)
}

encode_local_export_component :: proc(value: string, allocator: mem.Allocator) -> string {
	upper := strings.to_upper(strings.trim_space(value), allocator)
	out := strings.builder_make(allocator)
	for b in transmute([]byte)upper {
		if ascii_alpha(b) || ascii_digit(b) ||
		   b == '-' || b == '_' || b == '.' || b == '!' || b == '~' ||
		   b == '*' || b == '\'' || b == '(' || b == ')' {
			strings.write_byte(&out, b)
		} else {
			strings.write_byte(&out, '%')
			strings.write_byte(&out, hex_digit(b >> 4))
			strings.write_byte(&out, hex_digit(b & 0x0f))
		}
	}
	return strings.to_string(out)
}

path_file_name :: proc(path: string) -> string {
	start := 0
	for i in 0 ..< len(path) {
		if path[i] == '/' || path[i] == '\\' {
			start = i + 1
		}
	}
	return path[start:]
}

hex_digit :: proc(value: byte) -> byte {
	if value <= 9 {
		return '0' + value
	}
	return 'A' + value - 10
}

ascii_lower_byte :: proc(value: byte) -> byte {
	if 'A' <= value && value <= 'Z' {
		return value + ('a' - 'A')
	}
	return value
}

ascii_alpha :: proc(value: byte) -> bool {
	return('a' <= value && value <= 'z') || ('A' <= value && value <= 'Z')
}

ascii_digit :: proc(value: byte) -> bool {
	return '0' <= value && value <= '9'
}
