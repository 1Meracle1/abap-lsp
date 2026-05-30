package adt

import http "src:http"

import base64 "core:encoding/base64"
import "core:fmt"
import "core:mem"
import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:time"

trace_eprintf :: fmt.eprintf

SESSION_BOOTSTRAP_ACCEPT :: "application/atom+xml;type=feed, application/xml"

Error :: enum u8 {
	None,
	Missing_Base_Url,
	Missing_Username,
	Missing_Password,
	Dotenv_Parse,
	File_Read,
	Invalid_Url,
	Http,
	Http_Unsupported_Scheme,
	Http_Network,
	Http_Response_Too_Large,
	Http_Bad_Response,
	Http_Invalid_Content_Length,
	Http_Unsupported_Transfer_Encoding,
	Http_Invalid_Chunk,
	Bad_Status,
	Missing_Csrf_Token,
}

DEPENDENCY_FETCH_TRACE :: #config(ABAP_FRONTEND_TRACE_ADT_FETCH, false)

Connection_Overrides :: struct {
	base_url:              string,
	username:              string,
	password:              string,
	sap_client:            string,
	typepool_resolver_url: string,
}

Connection_Config :: struct {
	base_url:              string,
	username:              string,
	password:              string,
	sap_client:            string,
	typepool_resolver_url: string,
}

Dotenv_Defaults :: struct {
	values: map[string]string,
}

Dotenv_Parse_Error :: struct {
	line:    int,
	message: string,
}

Source_Kind :: enum u8 {
	Report,
	Include,
	Class,
	Function_Group,
	Function_Module,
	Interface,
}

Ddic_Kind :: enum u8 {
	Data_Element,
	Table_Type,
	Structure,
	View,
	Table,
}

Child_Kind :: enum u8 {
	Package,
	Report,
	Function_Group,
}

Object_Ref :: struct {
	uri:          string,
	object_type:  string,
	name:         string,
	package_name: string,
	description:  string,
}

Dependency_Fetch_Result :: struct {
	body:                string,
	file_extension:      string,
	manifest_kind:       string,
	shared_dependencies: [dynamic]Dependency_Artifact,
}

Dependency_Artifact :: struct {
	object_ref:     Object_Ref,
	body:           string,
	file_extension: string,
	manifest_kind:  string,
}

Repository_Child :: struct {
	object_ref:        Object_Ref,
	category_tag:      string,
	object_type_label: string,
	expandable:        bool,
}

Source_Fetch :: struct {
	request_url: string,
	object_uri:  string,
	resolved_by: string,
	body:        string,
}

clone_dependency_fetch_result :: proc(
	res: ^Dependency_Fetch_Result,
	allocator: mem.Allocator,
) -> Dependency_Fetch_Result {
	shared := make([dynamic]Dependency_Artifact, 0, len(res.shared_dependencies), allocator)
	for &artifact in res.shared_dependencies {
		append(&shared, clone_dependency_artifact(&artifact, allocator))
	}
	return Dependency_Fetch_Result {
		body = strings.clone(res.body, allocator),
		file_extension = strings.clone(res.file_extension, allocator),
		manifest_kind = strings.clone(res.manifest_kind, allocator),
		shared_dependencies = shared,
	}
}

clone_dependency_artifact :: proc(
	artifact: ^Dependency_Artifact,
	allocator: mem.Allocator,
) -> Dependency_Artifact {
	return Dependency_Artifact {
		object_ref     = clone_object_ref(&artifact.object_ref, allocator),
		body           = strings.clone(artifact.body, allocator),
		file_extension = strings.clone(artifact.file_extension, allocator),
		manifest_kind  = strings.clone(artifact.manifest_kind, allocator),
	}
}

trace_dependency_fetch :: proc(object_ref: ^Object_Ref, manifest_kind, file_extension: string) {
	when DEPENDENCY_FETCH_TRACE {
		trace_eprintf(
			"adt_fetch\t%s\t%s\t%s\t%s\t%s\n",
			manifest_kind,
			object_ref.name,
			object_ref.object_type,
			file_extension,
			object_ref.uri,
		)
	}
}

Ddic_Fetch :: struct {
	request_url: string,
	body:        string,
}

Category_Info :: struct {
	category: string,
	label:    string,
}

Object_Type_Info :: struct {
	object_type:  string,
	category_tag: string,
	label:        string,
	node_id:      string,
}

Tree_Node :: struct {
	object_type:    string,
	object_name:    string,
	object_uri:     string,
	object_vit_uri: string,
	expandable:     bool,
}

Repository_Node_Structure :: struct {
	tree_content: [dynamic]Tree_Node,
	categories:   [dynamic]Category_Info,
	object_types: [dynamic]Object_Type_Info,
}

Child_Entry :: struct {
	category_tag:      string,
	object_type_label: string,
	object_type:       string,
	name:              string,
	uri:               string,
	vit_uri:           string,
	expandable:        bool,
}

Client :: struct {
	connection: Connection_Config,
	http:       http.Client,
	csrf_token: string,
	cookie:     string,
	allocator:  mem.Allocator,
}

connection_config_from_sources :: proc(
	overrides: ^Connection_Overrides,
	dotenv: ^Dotenv_Defaults,
	allocator: mem.Allocator,
) -> (
	Connection_Config,
	Error,
) {
	base_url, ok := first_config_value(
		{"ABAP_ADT_URL", "ABAP_ADT_BASE_URL", "SAPBASE_URL"},
		overrides.base_url,
		dotenv,
		allocator,
	)
	if !ok {
		return {}, .Missing_Base_Url
	}
	username, user_ok := first_config_value(
		{"ABAP_ADT_USER", "ABAP_ADT_USERNAME", "SAPUSER"},
		overrides.username,
		dotenv,
		allocator,
	)
	if !user_ok {
		delete(base_url, allocator)
		return {}, .Missing_Username
	}
	password, pass_ok := first_config_value(
		{"ABAP_ADT_PASSWORD", "SAPPASS"},
		overrides.password,
		dotenv,
		allocator,
	)
	if !pass_ok {
		delete(base_url, allocator)
		delete(username, allocator)
		return {}, .Missing_Password
	}
	sap_client, _ := first_config_value(
		{"ABAP_ADT_CLIENT", "SAPCLIENT"},
		overrides.sap_client,
		dotenv,
		allocator,
	)
	typepool_resolver_url, _ := first_config_value(
		{"ABAP_TYPEPOOL_RESOLVER_URL"},
		overrides.typepool_resolver_url,
		dotenv,
		allocator,
	)
	normalized := normalize_base_url(base_url, allocator)
	delete(base_url, allocator)
	return Connection_Config {
			base_url              = normalized,
			username              = username,
			password              = password,
			sap_client            = sap_client,
			typepool_resolver_url = typepool_resolver_url,
		},
		.None
}

connection_config_destroy :: proc(config: ^Connection_Config, allocator: mem.Allocator) {
	delete(config.base_url, allocator)
	delete(config.username, allocator)
	delete(config.password, allocator)
	delete(config.sap_client, allocator)
	delete(config.typepool_resolver_url, allocator)
	config^ = {}
}

connection_key :: proc(config: ^Connection_Config, allocator: mem.Allocator) -> string {
	client := strings.trim_space(config.sap_client)
	if client == "" {
		return strings.clone(config.base_url, allocator)
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, config.base_url)
	strings.write_string(&out, "?sap-client=")
	strings.write_string(&out, client)
	return strings.to_string(out)
}

dotenv_defaults_init :: proc(defaults: ^Dotenv_Defaults, allocator: mem.Allocator) {
	defaults.values = make(map[string]string, 0, allocator)
}

dotenv_defaults_destroy :: proc(defaults: ^Dotenv_Defaults, allocator: mem.Allocator) {
	for key, value in defaults.values {
		delete(key, allocator)
		delete(value, allocator)
	}
	delete(defaults.values)
	defaults^ = {}
}

load_dotenv_defaults :: proc(
	start_dir: string,
	allocator: mem.Allocator,
) -> (
	Dotenv_Defaults,
	Error,
) {
	bases := make([dynamic]string, 0, 2, allocator)
	defer delete(bases)
	if strings.trim_space(start_dir) != "" {
		append(&bases, strings.clone(start_dir, allocator))
	}
	if cwd, err := os.get_working_directory(allocator); err == nil {
		append(&bases, cwd)
	}
	defer for base in bases {
		delete(base, allocator)
	}

	for base in bases {
		if path, ok := find_dotenv_from_base(base, true, allocator); ok {
			defer delete(path, allocator)
			return parse_dotenv_file(path, allocator)
		}
		if path, ok := find_dotenv_from_base(base, false, allocator); ok {
			defer delete(path, allocator)
			return parse_dotenv_file(path, allocator)
		}
	}
	defaults: Dotenv_Defaults
	dotenv_defaults_init(&defaults, allocator)
	return defaults, .None
}

parse_dotenv_contents :: proc(
	content: string,
	allocator: mem.Allocator,
) -> (
	Dotenv_Defaults,
	Dotenv_Parse_Error,
	bool,
) {
	defaults: Dotenv_Defaults
	dotenv_defaults_init(&defaults, allocator)

	line_start := 0
	line_nr := 1
	for line_start <= len(content) {
		line_end := line_start
		for line_end < len(content) && content[line_end] != '\n' {
			line_end += 1
		}
		raw_line := content[line_start:line_end]
		if strings.has_suffix(raw_line, "\r") {
			raw_line = raw_line[:len(raw_line) - 1]
		}
		line := strings.trim_space(raw_line)
		if line != "" && !strings.has_prefix(line, "#") {
			if strings.has_prefix(line, "export ") {
				line = strings.trim_left(line[7:], " \t")
			}
			eq := strings.index_byte(line, '=')
			if eq < 0 {
				dotenv_defaults_destroy(&defaults, allocator)
				return {},
					Dotenv_Parse_Error{line = line_nr, message = "expected KEY=VALUE"},
					false
			}
			key := strings.trim_space(line[:eq])
			if key == "" {
				dotenv_defaults_destroy(&defaults, allocator)
				return {},
					Dotenv_Parse_Error{line = line_nr, message = "missing variable name"},
					false
			}
			value, value_ok := parse_dotenv_value(strings.trim_space(line[eq + 1:]), allocator)
			if !value_ok {
				dotenv_defaults_destroy(&defaults, allocator)
				return {},
					Dotenv_Parse_Error{line = line_nr, message = "unterminated quoted value"},
					false
			}
			defaults.values[strings.clone(key, allocator)] = value
		}
		if line_end == len(content) {
			break
		}
		line_start = line_end + 1
		line_nr += 1
	}
	return defaults, {}, true
}

source_kind_parse :: proc(raw: string) -> (Source_Kind, bool) {
	value := strings.trim_space(raw)
	switch {
	case strings.equal_fold(value, "report") ||
	     strings.equal_fold(value, "prog") ||
	     strings.equal_fold(value, "program"):
		return .Report, true
	case strings.equal_fold(value, "include"):
		return .Include, true
	case strings.equal_fold(value, "class"):
		return .Class, true
	case strings.equal_fold(value, "function-group") ||
	     strings.equal_fold(value, "functiongroup") ||
	     strings.equal_fold(value, "fugr"):
		return .Function_Group, true
	case strings.equal_fold(value, "function-module") ||
	     strings.equal_fold(value, "functionmodule") ||
	     strings.equal_fold(value, "fmodule") ||
	     strings.equal_fold(value, "fm"):
		return .Function_Module, true
	case strings.equal_fold(value, "interface") || strings.equal_fold(value, "intf"):
		return .Interface, true
	}
	return {}, false
}

source_kind_string :: proc(kind: Source_Kind) -> string {
	switch kind {
	case .Report:
		return "report"
	case .Include:
		return "include"
	case .Class:
		return "class"
	case .Function_Group:
		return "function-group"
	case .Function_Module:
		return "function-module"
	case .Interface:
		return "interface"
	}
	return ""
}

ddic_kind_parse :: proc(raw: string) -> (Ddic_Kind, bool) {
	value := strings.trim_space(raw)
	switch {
	case strings.equal_fold(value, "data-element") ||
	     strings.equal_fold(value, "dataelement") ||
	     strings.equal_fold(value, "dtel"):
		return .Data_Element, true
	case strings.equal_fold(value, "table-type") ||
	     strings.equal_fold(value, "tabletype") ||
	     strings.equal_fold(value, "ttyp"):
		return .Table_Type, true
	case strings.equal_fold(value, "structure") || strings.equal_fold(value, "struct"):
		return .Structure, true
	case strings.equal_fold(value, "view"):
		return .View, true
	case strings.equal_fold(value, "table") ||
	     strings.equal_fold(value, "database-table") ||
	     strings.equal_fold(value, "db-table"):
		return .Table, true
	}
	return {}, false
}

ddic_kind_string :: proc(kind: Ddic_Kind) -> string {
	switch kind {
	case .Data_Element:
		return "data-element"
	case .Table_Type:
		return "table-type"
	case .Structure:
		return "structure"
	case .View:
		return "view"
	case .Table:
		return "table"
	}
	return ""
}

child_kind_parse :: proc(raw: string) -> (Child_Kind, bool) {
	value := strings.trim_space(raw)
	switch {
	case strings.equal_fold(value, "package") || strings.equal_fold(value, "devclass"):
		return .Package, true
	case strings.equal_fold(value, "report") ||
	     strings.equal_fold(value, "prog") ||
	     strings.equal_fold(value, "program"):
		return .Report, true
	case strings.equal_fold(value, "function-group") ||
	     strings.equal_fold(value, "functiongroup") ||
	     strings.equal_fold(value, "fugr"):
		return .Function_Group, true
	}
	return {}, false
}

child_kind_string :: proc(kind: Child_Kind) -> string {
	switch kind {
	case .Package:
		return "package"
	case .Report:
		return "report"
	case .Function_Group:
		return "function-group"
	}
	return ""
}

child_kind_parent_type :: proc(kind: Child_Kind) -> string {
	switch kind {
	case .Package:
		return "DEVC/K"
	case .Report:
		return "PROG/P"
	case .Function_Group:
		return "FUGR/F"
	}
	return ""
}

client_init :: proc(client: ^Client, connection: Connection_Config, allocator: mem.Allocator) {
	client^ = Client {
		connection = connection,
		http       = http.default_client(),
		allocator  = allocator,
	}
	client.http.timeout = 60 * time.Second
}

client_destroy :: proc(client: ^Client, allocator: mem.Allocator) {
	delete(client.csrf_token, allocator)
	delete(client.cookie, allocator)
	client^ = {}
}

client_connection_key :: proc(client: ^Client, allocator: mem.Allocator) -> string {
	return connection_key(&client.connection, allocator)
}

search_repository_objects :: proc(
	client: ^Client,
	query: string,
	max_results: int,
	temp_allocator: mem.Allocator,
) -> (
	[dynamic]Object_Ref,
	Error,
) {
	url := absolute_url(&client.connection, "/repository/informationsystem/search", temp_allocator)
	url = append_query_param(url, "operation", "quickSearch", temp_allocator)
	url = append_query_param(url, "query", query, temp_allocator)
	max_builder := strings.builder_make(temp_allocator)
	strings.write_int(&max_builder, max_results)
	max_text := strings.to_string(max_builder)
	url = append_query_param(url, "maxResults", max_text, temp_allocator)
	body, err := send_text(client, .Get, url, "application/xml", "", "", temp_allocator)
	if err != .None {
		return make([dynamic]Object_Ref, temp_allocator), err
	}
	return parse_object_references(body, temp_allocator), .None
}

fetch_source :: proc(
	client: ^Client,
	kind: Source_Kind,
	name: string,
	function_group: string,
	allocator: mem.Allocator,
) -> (
	Source_Fetch,
	Error,
) {
	switch kind {
	case .Report:
		return fetch_source_by_path(
			client,
			source_path("/programs/programs/", name, allocator),
			"direct",
			allocator,
		)
	case .Include:
		return fetch_source_by_path(
			client,
			source_path("/programs/includes/", name, allocator),
			"direct",
			allocator,
		)
	case .Class:
		return fetch_source_by_path(
			client,
			source_path("/oo/classes/", name, allocator),
			"direct",
			allocator,
		)
	case .Interface:
		return fetch_source_by_path(
			client,
			source_path("/oo/interfaces/", name, allocator),
			"direct",
			allocator,
		)
	case .Function_Group:
		return fetch_source_by_path(
			client,
			source_path("/functions/groups/", name, allocator),
			"direct",
			allocator,
		)
	case .Function_Module:
		if strings.trim_space(function_group) != "" {
			path := strings.builder_make(allocator)
			strings.write_string(&path, "/functions/groups/")
			group := encode_path_segment(function_group, allocator)
			defer delete(group, allocator)
			strings.write_string(&path, group)
			strings.write_string(&path, "/fmodules/")
			module := encode_path_segment(name, allocator)
			defer delete(module, allocator)
			strings.write_string(&path, module)
			return fetch_source_by_path(client, strings.to_string(path), "direct", allocator)
		}
	}
	return {}, .Invalid_Url
}

fetch_object_source :: proc(
	client: ^Client,
	object_uri: string,
	allocator: mem.Allocator,
) -> (
	string,
	Error,
) {
	fetched, err := fetch_source_by_path(client, object_uri, "direct", allocator)
	if err != .None {
		return "", err
	}
	return fetched.body, .None
}

fetch_ddic :: proc(
	client: ^Client,
	kind: Ddic_Kind,
	name: string,
	temp_allocator: mem.Allocator,
) -> (
	Ddic_Fetch,
	Error,
) {
	accept := "application/vnd.sap.adt.elementinfo+xml"
	path := ""
	switch kind {
	case .Data_Element:
		path = source_path("/ddic/dataelements/", name, temp_allocator)
		accept = "application/vnd.sap.adt.dataelements.v1+xml, application/vnd.sap.adt.dataelements.v2+xml"
	case .Table_Type, .Structure, .View, .Table:
		path = absolute_url(&client.connection, "/ddic/elementinfo", temp_allocator)
		path = append_query_param(path, "path", name, temp_allocator)
	}
	url := path
	if kind == .Data_Element {
		url = absolute_url(&client.connection, path, temp_allocator)
	}
	body, err := send_text(client, .Get, url, accept, "", "", temp_allocator)
	if err != .None {
		return {}, err
	}
	return Ddic_Fetch{request_url = strings.clone(url, temp_allocator), body = body}, .None
}

list_children :: proc(
	client: ^Client,
	kind: Child_Kind,
	name: string,
	allocator: mem.Allocator,
) -> (
	Repository_Node_Structure,
	[dynamic]Child_Entry,
	Error,
) {
	root, err := fetch_repository_node_structure(
		client,
		name,
		child_kind_parent_type(kind),
		nil,
		allocator,
	)
	children := make([dynamic]Child_Entry, allocator)
	if err != .None {
		return root, children, err
	}
	if len(root.object_types) == 0 {
		for &node in root.tree_content {
			append_child_from_node(&children, &node, "", "", allocator)
		}
		return root, children, .None
	}
	for &object_type in root.object_types {
		if object_type.node_id == "" {
			continue
		}
		branch, branch_err := fetch_repository_node_structure(
			client,
			name,
			child_kind_parent_type(kind),
			[]string{object_type.node_id},
			allocator,
		)
		if branch_err != .None {
			return root, children, branch_err
		}
		for &node in branch.tree_content {
			append_child_from_node(
				&children,
				&node,
				object_type.category_tag,
				object_type.label,
				allocator,
			)
		}
		repository_node_structure_destroy(&branch, allocator)
	}
	return root, children, .None
}

fetch_ddic_object :: proc(
	client: ^Client,
	kind, name: string,
	temp_allocator: mem.Allocator,
) -> (
	string,
	Error,
) {
	ddic_kind := Ddic_Kind.Structure
	switch {
	case strings.equal_fold(strings.trim_space(kind), "ddic-data-element"):
		ddic_kind = .Data_Element
	case strings.equal_fold(strings.trim_space(kind), "ddic-table-type"):
		ddic_kind = .Table_Type
	case strings.equal_fold(strings.trim_space(kind), "ddic-table"):
		ddic_kind = .Table
	case strings.equal_fold(strings.trim_space(kind), "ddic-view"):
		ddic_kind = .View
	}
	fetched, err := fetch_ddic(client, ddic_kind, name, temp_allocator)
	if err != .None {
		return "", err
	}
	return format_ddic_xml(fetched.body, temp_allocator), .None
}

fetch_message_class :: proc(
	client: ^Client,
	name: string,
	allocator: mem.Allocator,
) -> (
	string,
	Error,
) {
	path := source_path("/messageclass/", name, allocator)
	url := absolute_url(&client.connection, path, allocator)
	body, err := send_text(
		client,
		.Get,
		url,
		"application/vnd.sap.adt.elementinfo+xml",
		"",
		"",
		allocator,
	)
	if err != .None {
		return "", err
	}
	return format_ddic_xml(body, allocator), .None
}

typepool_resolver_enabled :: proc(client: ^Client) -> bool {
	return strings.trim_space(client.connection.typepool_resolver_url) != ""
}

resolve_typepool_owner :: proc(
	client: ^Client,
	name: string,
	temp_allocator: mem.Allocator,
) -> (
	string,
	Error,
) {
	if !typepool_resolver_enabled(client) {
		return "", .Invalid_Url
	}
	url := typepool_resolver_url(client, "owner", "name", name, temp_allocator)
	body, err := send_text(client, .Get, url, "text/plain", "", "", temp_allocator)
	if err != .None {
		return "", err
	}
	return strings.trim_space(body), .None
}

fetch_typepool_source :: proc(
	client: ^Client,
	pool: string,
	temp_allocator: mem.Allocator,
) -> (
	string,
	Error,
) {
	if !typepool_resolver_enabled(client) {
		return "", .Invalid_Url
	}
	url := typepool_resolver_url(client, "source", "pool", pool, temp_allocator)
	return send_text(client, .Get, url, "text/plain", "", "", temp_allocator)
}

typepool_resolver_url :: proc(
	client: ^Client,
	op, key, value: string,
	allocator: mem.Allocator,
) -> string {
	url := strings.trim_right(strings.trim_space(client.connection.typepool_resolver_url), "/")
	url = append_query_param(url, "op", op, allocator)
	url = append_query_param(url, key, value, allocator)
	return absolute_url(&client.connection, url, allocator)
}

fetch_dependency_object :: proc(
	client: ^Client,
	object_ref: ^Object_Ref,
	temp_allocator: mem.Allocator,
) -> (
	Dependency_Fetch_Result,
	Error,
) {
	if client.csrf_token == "" {
		if err := ensure_session(client, temp_allocator); err != .None {
			return {}, err
		}
	}
	if is_message_class_dependency_object(object_ref) {
		body, err := fetch_message_class(client, object_ref.name, temp_allocator)
		if err != .None {
			return {}, err
		}
		return Dependency_Fetch_Result {
				body = body,
				file_extension = "xml",
				manifest_kind = "message-class",
				shared_dependencies = make([dynamic]Dependency_Artifact, temp_allocator),
			},
			.None
	}
	if is_direct_ddic_elementinfo_object(object_ref) {
		fetched, err := fetch_ddic(client, .Structure, object_ref.name, temp_allocator)
		if err != .None {
			return {}, err
		}
		object_type := ddic_object_type_from_xml(fetched.body)
		if object_type == "" {
			return {}, .Bad_Status
		}
		return Dependency_Fetch_Result {
				body = format_ddic_xml(fetched.body, temp_allocator),
				file_extension = "xml",
				manifest_kind = infer_ddic_manifest_kind_from_object_type(object_type),
				shared_dependencies = make([dynamic]Dependency_Artifact, temp_allocator),
			},
			.None
	}
	if is_fetchable_ddic_dependency_object(object_ref) {
		kind := infer_ddic_manifest_kind(object_ref)
		body, err := fetch_ddic_object(client, kind, object_ref.name, temp_allocator)
		if err != .None {
			return {}, err
		}
		return Dependency_Fetch_Result {
				body = body,
				file_extension = "xml",
				manifest_kind = kind,
				shared_dependencies = make([dynamic]Dependency_Artifact, temp_allocator),
			},
			.None
	}
	if is_function_module_object(object_ref) {
		return fetch_function_module_dependency_source(client, object_ref, temp_allocator)
	}

	body, err := fetch_object_source(client, object_ref.uri, temp_allocator)
	if err != .None {
		return {}, err
	}
	return Dependency_Fetch_Result {
			body = body,
			file_extension = "abap",
			manifest_kind = infer_repository_manifest_kind(object_ref),
			shared_dependencies = make([dynamic]Dependency_Artifact, temp_allocator),
		},
		.None
}

select_dependency_objects :: proc(
	query: string,
	objects: []Object_Ref,
	kind_hint: string,
	allocator: mem.Allocator,
) -> [dynamic]Object_Ref {
	normalized_query := strings.trim_space(query)
	out := make([dynamic]Object_Ref, allocator)
	if len(normalized_query) == 0 {
		return out
	}

	exact := make([dynamic]Object_Ref, allocator)
	for &object_ref in objects {
		if strings.equal_fold(strings.trim_space(object_ref.name), normalized_query) &&
		   is_supported_dependency_object(&object_ref, kind_hint) {
			found: bool
			for existing in exact {
				if strings.equal_fold(existing.object_type, object_ref.object_type) &&
				   strings.equal_fold(existing.uri, object_ref.uri) {
					found = true
					break
				}
			}
			if !found {
				append(&exact, object_ref)
			}
		}
	}
	if len(exact) > 0 {
		sort_object_refs(exact[:])
		return drop_shadowed_ddic_domains(exact, allocator)
	}

	supported := make([dynamic]Object_Ref, allocator)
	for &object_ref in objects {
		if is_supported_dependency_object(&object_ref, kind_hint) {
			append(&supported, object_ref)
		}
	}
	if len(supported) == 0 {
		for &object_ref in objects {
			if is_supported_dependency_object(&object_ref, "") {
				append(&supported, object_ref)
			}
		}
	}
	if len(supported) == 0 {
		return out
	}
	if best, ok := pick_best_dependency_object(
		normalized_query,
		supported[:],
		kind_hint,
		allocator,
	); ok {
		append(&out, best)
	} else {
		append(&out, supported[0])
	}
	return out
}

direct_dependency_object_refs :: proc(
	name, kind_hint: string,
	allocator: mem.Allocator,
) -> [dynamic]Object_Ref {
	out := make([dynamic]Object_Ref, allocator)
	hint := strings.trim_space(kind_hint)
	switch {
	case strings.equal_fold(hint, "message-class"):
		append(&out, build_message_class_object_ref(name, allocator))
	case strings.equal_fold(hint, "include"):
		append(&out, build_include_object_ref(name, "", allocator))
	case strings.equal_fold(hint, "report"):
		append(&out, build_report_object_ref(name, "", allocator))
	case strings.equal_fold(hint, "static"):
		append_direct_class_interface_refs(&out, name, true, allocator)
	case strings.equal_fold(hint, "object-type"):
		append(&out, build_class_object_ref(name, "", allocator))
		append(&out, build_interface_object_ref(name, "", allocator))
	case strings.equal_fold(hint, "interface-type"):
		append(&out, build_interface_object_ref(name, "", allocator))
	case strings.equal_fold(hint, "ddic-type"):
		append(&out, build_data_element_object_ref(name, allocator))
		append(&out, build_ddic_elementinfo_object_ref(name, allocator))
	case strings.equal_fold(hint, "type"):
		append_direct_class_interface_refs(&out, name, false, allocator)
	}
	return out
}

is_supported_dependency_object :: proc(object_ref: ^Object_Ref, kind_hint: string) -> bool {
	hint := strings.trim_space(kind_hint)
	uri := object_ref.uri
	object_type := object_ref.object_type
	switch {
	case strings.equal_fold(hint, "message-class"):
		return is_message_class_dependency_object(object_ref)
	case strings.equal_fold(hint, "include"):
		return(
			ascii_contains_ignore_case(uri, "/programs/includes/") ||
			strings.equal_fold(object_type, "PROG/I") \
		)
	case strings.equal_fold(hint, "report"):
		return(
			ascii_contains_ignore_case(uri, "/programs/programs/") ||
			strings.equal_fold(object_type, "PROG/P") \
		)
	case strings.equal_fold(hint, "function"):
		return(
			ascii_contains_ignore_case(uri, "/functions/groups/") ||
			strings.equal_fold(object_type, "FUGR/F") ||
			strings.equal_fold(object_type, "FUGR/FF") \
		)
	case strings.equal_fold(hint, "static"):
		return(
			ascii_contains_ignore_case(uri, "/oo/classes/") ||
			ascii_contains_ignore_case(uri, "/oo/interfaces/") ||
			ascii_starts_with_ignore_case(object_type, "CLAS/") ||
			ascii_starts_with_ignore_case(object_type, "INTF/") \
		)
	case strings.equal_fold(hint, "type"):
		return(
			is_fetchable_ddic_dependency_object(object_ref) ||
			ascii_contains_ignore_case(uri, "/oo/classes/") ||
			ascii_contains_ignore_case(uri, "/oo/interfaces/") ||
			ascii_starts_with_ignore_case(object_type, "CLAS/") ||
			ascii_starts_with_ignore_case(object_type, "INTF/") \
		)
	}
	return(
		ascii_contains_ignore_case(uri, "/programs/includes/") ||
		ascii_contains_ignore_case(uri, "/programs/programs/") ||
		ascii_contains_ignore_case(uri, "/oo/classes/") ||
		ascii_contains_ignore_case(uri, "/oo/interfaces/") ||
		ascii_contains_ignore_case(uri, "/functions/groups/") ||
		is_message_class_dependency_object(object_ref) ||
		is_fetchable_ddic_dependency_object(object_ref) ||
		strings.equal_fold(object_type, "PROG/I") ||
		strings.equal_fold(object_type, "PROG/P") ||
		ascii_starts_with_ignore_case(object_type, "CLAS/") ||
		ascii_starts_with_ignore_case(object_type, "INTF/") \
	)
}

is_ddic_dependency_object :: proc(object_ref: ^Object_Ref) -> bool {
	object_type := object_ref.object_type
	return(
		strings.equal_fold(object_type, "DTEL/DE") ||
		is_ddic_domain_object(object_ref) ||
		strings.equal_fold(object_type, "TABL/DS") ||
		strings.equal_fold(object_type, "TABL/DT") ||
		strings.equal_fold(object_type, "TABL/DA") ||
		strings.equal_fold(object_type, "TTYP/DA") ||
		strings.equal_fold(object_type, "VIEW/DV") \
	)
}

is_direct_ddic_elementinfo_object :: proc(object_ref: ^Object_Ref) -> bool {
	return strings.equal_fold(object_ref.object_type, "DDIC/EI")
}

is_message_class_dependency_object :: proc(object_ref: ^Object_Ref) -> bool {
	return(
		strings.equal_fold(object_ref.object_type, "MSAG/N") ||
		ascii_contains_ignore_case(object_ref.uri, "/sap/bc/adt/messageclass/") \
	)
}

is_function_module_object :: proc(object_ref: ^Object_Ref) -> bool {
	return(
		strings.equal_fold(object_ref.object_type, "FUGR/FF") ||
		(ascii_contains_ignore_case(object_ref.uri, "/functions/groups/") &&
				ascii_contains_ignore_case(object_ref.uri, "/fmodules/")) \
	)
}

infer_ddic_manifest_kind :: proc(object_ref: ^Object_Ref) -> string {
	return infer_ddic_manifest_kind_from_object_type(object_ref.object_type)
}

infer_ddic_manifest_kind_from_object_type :: proc(object_type: string) -> string {
	switch {
	case ascii_starts_with_ignore_case(object_type, "DOMA/"):
		return "ddic-domain"
	case strings.equal_fold(object_type, "DTEL/DE"):
		return "ddic-data-element"
	case strings.equal_fold(object_type, "TABL/DS"):
		return "ddic-structure"
	case strings.equal_fold(object_type, "TABL/DT"):
		return "ddic-table"
	case strings.equal_fold(object_type, "TABL/DA") || strings.equal_fold(object_type, "TTYP/DA"):
		return "ddic-table-type"
	case strings.equal_fold(object_type, "VIEW/DV"):
		return "ddic-view"
	}
	return "ddic-structure"
}

infer_repository_manifest_kind :: proc(object_ref: ^Object_Ref) -> string {
	switch {
	case ascii_contains_ignore_case(object_ref.uri, "/programs/includes/") ||
	     strings.equal_fold(object_ref.object_type, "PROG/I"):
		return "include"
	case ascii_contains_ignore_case(object_ref.uri, "/oo/classes/") ||
	     ascii_starts_with_ignore_case(object_ref.object_type, "CLAS/"):
		return "global-class"
	case ascii_contains_ignore_case(object_ref.uri, "/oo/interfaces/") ||
	     ascii_starts_with_ignore_case(object_ref.object_type, "INTF/"):
		return "global-interface"
	case ascii_contains_ignore_case(object_ref.uri, "/functions/groups/"):
		return "function-group"
	}
	return "report"
}

build_message_class_object_ref :: proc(name: string, allocator: mem.Allocator) -> Object_Ref {
	n := trim_upper(name, allocator)
	uri := strings.builder_make(allocator)
	strings.write_string(&uri, "/sap/bc/adt/messageclass/")
	encoded := encode_path_segment(n, allocator)
	defer delete(encoded, allocator)
	strings.write_string(&uri, encoded)
	return Object_Ref {
		uri = strings.to_string(uri),
		object_type = strings.clone("MSAG/N", allocator),
		name = n,
		description = strings.clone("Message class", allocator),
	}
}

build_data_element_object_ref :: proc(name: string, allocator: mem.Allocator) -> Object_Ref {
	return build_named_ref(
		name,
		"",
		"/sap/bc/adt/ddic/dataelements/",
		"DTEL/DE",
		"Data element",
		allocator,
	)
}

build_ddic_elementinfo_object_ref :: proc(name: string, allocator: mem.Allocator) -> Object_Ref {
	n := trim_upper(name, allocator)
	return Object_Ref {
		uri = ddic_dependency_uri_for_object_type(n, "DDIC/EI", allocator),
		object_type = strings.clone("DDIC/EI", allocator),
		name = n,
		description = strings.clone("DDIC element info", allocator),
	}
}

ddic_dependency_uri_for_object_type :: proc(
	name, object_type: string,
	allocator: mem.Allocator,
) -> string {
	switch {
	case strings.equal_fold(object_type, "DTEL/DE"):
		return source_path("/sap/bc/adt/ddic/dataelements/", name, allocator)
	case strings.equal_fold(object_type, "TABL/DS"):
		return source_path("/sap/bc/adt/ddic/structures/", name, allocator)
	case strings.equal_fold(object_type, "TABL/DT"):
		return vit_ddic_uri("tabldt", name, allocator)
	case strings.equal_fold(object_type, "TABL/DA") || strings.equal_fold(object_type, "TTYP/DA"):
		return vit_ddic_uri("ttypda", name, allocator)
	case strings.equal_fold(object_type, "VIEW/DV"):
		return vit_ddic_uri("viewdv", name, allocator)
	}
	return source_path("/sap/bc/adt/ddic/elementinfo/", name, allocator)
}

vit_ddic_uri :: proc(object_type_key, name: string, allocator: mem.Allocator) -> string {
	n := trim_upper(name, allocator)
	defer delete(n, allocator)
	encoded := encode_path_segment(n, allocator)
	defer delete(encoded, allocator)
	out := strings.builder_make(allocator)
	strings.write_string(&out, "/sap/bc/adt/vit/wb/object_type/")
	strings.write_string(&out, object_type_key)
	strings.write_string(&out, "/object_name/")
	strings.write_string(&out, encoded)
	return strings.to_string(out)
}

build_include_object_ref :: proc(
	name, package_name: string,
	allocator: mem.Allocator,
) -> Object_Ref {
	return build_named_ref(
		name,
		package_name,
		"/sap/bc/adt/programs/includes/",
		"PROG/I",
		"Include",
		allocator,
	)
}

build_report_object_ref :: proc(
	name, package_name: string,
	allocator: mem.Allocator,
) -> Object_Ref {
	return build_named_ref(
		name,
		package_name,
		"/sap/bc/adt/programs/programs/",
		"PROG/P",
		"Report",
		allocator,
	)
}

build_class_object_ref :: proc(
	name, package_name: string,
	allocator: mem.Allocator,
) -> Object_Ref {
	return build_named_ref(
		name,
		package_name,
		"/sap/bc/adt/oo/classes/",
		"CLAS/OC",
		"Global class",
		allocator,
	)
}

build_interface_object_ref :: proc(
	name, package_name: string,
	allocator: mem.Allocator,
) -> Object_Ref {
	return build_named_ref(
		name,
		package_name,
		"/sap/bc/adt/oo/interfaces/",
		"INTF/OI",
		"Global interface",
		allocator,
	)
}

fetch_function_module_dependency_source :: proc(
	client: ^Client,
	object_ref: ^Object_Ref,
	allocator: mem.Allocator,
) -> (
	Dependency_Fetch_Result,
	Error,
) {
	module_source, err := fetch_object_source(client, object_ref.uri, allocator)
	if err != .None {
		return {}, err
	}
	return Dependency_Fetch_Result {
			body = module_source,
			file_extension = strings.clone("abap", allocator),
			manifest_kind = strings.clone("function-module", allocator),
			shared_dependencies = make([dynamic]Dependency_Artifact, allocator),
		},
		.None
}

format_ddic_xml :: proc(xml: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_space(xml)
	if !strings.has_prefix(trimmed, "<") {
		return xml
	}
	out := strings.builder_make(allocator)
	indent := 0
	pos := 0
	for pos < len(trimmed) {
		if trimmed[pos] == '<' {
			end := strings.index_byte(trimmed[pos:], '>')
			if end < 0 {
				break
			}
			token := strings.trim_space(trimmed[pos:pos + end + 1])
			if strings.has_prefix(token, "</") && indent > 0 {
				indent -= 1
			}
			write_indented_line(&out, indent, token)
			if !strings.has_prefix(token, "</") &&
			   !strings.has_prefix(token, "<?") &&
			   !strings.has_prefix(token, "<!") &&
			   !strings.has_suffix(token, "/>") {
				indent += 1
			}
			pos += end + 1
			continue
		}
		next := strings.index_byte(trimmed[pos:], '<')
		end := len(trimmed)
		if next >= 0 {
			end = pos + next
		}
		text := strings.trim_space(trimmed[pos:end])
		if text != "" {
			write_indented_line(&out, indent, text)
		}
		pos = end
	}
	return strings.to_string(out)
}

ddic_object_type_from_xml :: proc(xml: string) -> string {
	search_from := 0
	for search_from < len(xml) {
		start_rel := strings.index_byte(xml[search_from:], '<')
		if start_rel < 0 {
			return ""
		}
		start := search_from + start_rel
		if start + 1 >= len(xml) {
			return ""
		}
		end_rel := strings.index_byte(xml[start:], '>')
		if end_rel < 0 {
			return ""
		}
		if xml[start + 1] == '?' || xml[start + 1] == '!' {
			search_from = start + end_rel + 1
			continue
		}
		return strings.trim_space(read_attr(xml[start:start + end_rel + 1], "type"))
	}
	return ""
}

parse_object_references :: proc(xml: string, allocator: mem.Allocator) -> [dynamic]Object_Ref {
	out := make([dynamic]Object_Ref, allocator)
	needle := "<adtcore:objectReference"
	search_from := 0
	for {
		start_rel := strings.index(xml[search_from:], needle)
		if start_rel < 0 {
			break
		}
		start := search_from + start_rel + len(needle)
		end_rel := strings.index_byte(xml[start:], '>')
		if end_rel < 0 {
			break
		}
		attrs := xml[start:start + end_rel]
		entry := Object_Ref {
			uri          = decode_xml_entities(read_attr(attrs, "adtcore:uri"), allocator),
			object_type  = decode_xml_entities(read_attr(attrs, "adtcore:type"), allocator),
			name         = decode_xml_entities(read_attr(attrs, "adtcore:name"), allocator),
			package_name = decode_xml_entities(read_attr(attrs, "adtcore:packageName"), allocator),
			description  = decode_xml_entities(read_attr(attrs, "adtcore:description"), allocator),
		}
		if len(entry.uri) != 0 && len(entry.name) != 0 {
			append(&out, entry)
		}
		search_from = start + end_rel + 1
	}
	return out
}

parse_repository_node_structure :: proc(
	xml: string,
	allocator: mem.Allocator,
) -> Repository_Node_Structure {
	structure := Repository_Node_Structure {
		tree_content = make([dynamic]Tree_Node, allocator),
		categories   = make([dynamic]Category_Info, allocator),
		object_types = make([dynamic]Object_Type_Info, allocator),
	}
	blocks := collect_blocks(xml, "SEU_ADT_REPOSITORY_OBJ_NODE", allocator)
	for block in blocks {
		expandable := read_tag_text(block, "EXPANDABLE", allocator)
		append(
			&structure.tree_content,
			Tree_Node {
				object_type = read_tag_text(block, "OBJECT_TYPE", allocator),
				object_name = read_tag_text(block, "OBJECT_NAME", allocator),
				object_uri = read_tag_text(block, "OBJECT_URI", allocator),
				object_vit_uri = read_tag_text(block, "OBJECT_VIT_URI", allocator),
				expandable = strings.equal_fold(expandable, "X"),
			},
		)
		delete(expandable, allocator)
		delete(block, allocator)
	}
	delete(blocks)
	blocks = collect_blocks(xml, "SEU_ADT_OBJECT_CATEGORY_INFO", allocator)
	for block in blocks {
		append(
			&structure.categories,
			Category_Info {
				category = read_tag_text(block, "CATEGORY", allocator),
				label = read_tag_text(block, "CATEGORY_LABEL", allocator),
			},
		)
		delete(block, allocator)
	}
	delete(blocks)
	blocks = collect_blocks(xml, "SEU_ADT_OBJECT_TYPE_INFO", allocator)
	for block in blocks {
		append(
			&structure.object_types,
			Object_Type_Info {
				object_type = read_tag_text(block, "OBJECT_TYPE", allocator),
				category_tag = read_tag_text(block, "CATEGORY_TAG", allocator),
				label = read_tag_text(block, "OBJECT_TYPE_LABEL", allocator),
				node_id = read_tag_text(block, "NODE_ID", allocator),
			},
		)
		delete(block, allocator)
	}
	delete(blocks)
	return structure
}

append_child_from_node :: proc(
	children: ^[dynamic]Child_Entry,
	node: ^Tree_Node,
	category_tag, object_type_label: string,
	allocator: mem.Allocator,
) {
	append(
		children,
		Child_Entry {
			category_tag = strings.clone(category_tag, allocator),
			object_type_label = strings.clone(object_type_label, allocator),
			object_type = strings.clone(node.object_type, allocator),
			name = strings.clone(node.object_name, allocator),
			uri = strings.clone(node.object_uri, allocator),
			vit_uri = strings.clone(node.object_vit_uri, allocator),
			expandable = node.expandable,
		},
	)
}

normalize_base_url :: proc(raw: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_right(strings.trim_space(raw), "/")
	if ascii_contains_ignore_case(trimmed, "/sap/bc/adt") {
		return strings.clone(trimmed, allocator)
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, trimmed)
	strings.write_string(&out, "/sap/bc/adt")
	return strings.to_string(out)
}

encode_path_segment :: proc(value: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	for byte in transmute([]byte)value {
		if ascii_uri_byte(byte) {
			strings.write_byte(&out, byte)
		} else {
			strings.write_byte(&out, '%')
			strings.write_byte(&out, hex_upper(byte >> 4))
			strings.write_byte(&out, hex_upper(byte & 0x0f))
		}
	}
	return strings.to_string(out)
}

object_ref_destroy :: proc(entry: ^Object_Ref, allocator: mem.Allocator) {
	delete(entry.uri, allocator)
	delete(entry.object_type, allocator)
	delete(entry.name, allocator)
	delete(entry.package_name, allocator)
	delete(entry.description, allocator)
	entry^ = {}
}

object_refs_destroy :: proc(entries: ^[dynamic]Object_Ref, allocator: mem.Allocator) {
	for &entry in entries^ {
		object_ref_destroy(&entry, allocator)
	}
	delete(entries^)
	entries^ = nil
}

dependency_fetch_result_destroy :: proc(
	result: ^Dependency_Fetch_Result,
	allocator: mem.Allocator,
) {
	delete(result.body, allocator)
	delete(result.file_extension, allocator)
	delete(result.manifest_kind, allocator)
	for &artifact in result.shared_dependencies {
		dependency_artifact_destroy(&artifact, allocator)
	}
	delete(result.shared_dependencies)
	result^ = {}
}

dependency_artifact_destroy :: proc(artifact: ^Dependency_Artifact, allocator: mem.Allocator) {
	object_ref_destroy(&artifact.object_ref, allocator)
	delete(artifact.body, allocator)
	delete(artifact.file_extension, allocator)
	delete(artifact.manifest_kind, allocator)
	artifact^ = {}
}

repository_node_structure_destroy :: proc(
	structure: ^Repository_Node_Structure,
	allocator: mem.Allocator,
) {
	for &node in structure.tree_content {
		delete(node.object_type, allocator)
		delete(node.object_name, allocator)
		delete(node.object_uri, allocator)
		delete(node.object_vit_uri, allocator)
	}
	for &category in structure.categories {
		delete(category.category, allocator)
		delete(category.label, allocator)
	}
	for &object_type in structure.object_types {
		delete(object_type.object_type, allocator)
		delete(object_type.category_tag, allocator)
		delete(object_type.label, allocator)
		delete(object_type.node_id, allocator)
	}
	delete(structure.tree_content)
	delete(structure.categories)
	delete(structure.object_types)
	structure^ = {}
}

absolute_url :: proc(
	config: ^Connection_Config,
	path_or_url: string,
	allocator: mem.Allocator,
) -> string {
	normalized := path_or_url
	if !ascii_starts_with_ignore_case(path_or_url, "http://") &&
	   !ascii_starts_with_ignore_case(path_or_url, "https://") &&
	   ascii_contains_ignore_case(config.base_url, "/sap/bc/adt") &&
	   ascii_starts_with_ignore_case(path_or_url, "/sap/bc/adt/") {
		normalized = path_or_url[len("/sap/bc/adt"):]
	}

	out := strings.builder_make(allocator)
	if ascii_starts_with_ignore_case(normalized, "http://") ||
	   ascii_starts_with_ignore_case(normalized, "https://") {
		strings.write_string(&out, normalized)
	} else {
		strings.write_string(&out, config.base_url)
		if !strings.has_prefix(normalized, "/") {
			strings.write_byte(&out, '/')
		}
		strings.write_string(&out, normalized)
	}
	url := strings.to_string(out)
	if strings.trim_space(config.sap_client) != "" &&
	   !ascii_contains_ignore_case(url, "sap-client=") {
		url = append_query_param(url, "sap-client", config.sap_client, allocator)
	}
	return url
}

fetch_repository_node_structure :: proc(
	client: ^Client,
	parent_name, parent_type: string,
	node_keys: []string,
	allocator: mem.Allocator,
) -> (
	Repository_Node_Structure,
	Error,
) {
	url := absolute_url(&client.connection, "/repository/nodestructure", allocator)
	url = append_query_param(url, "parent_name", parent_name, allocator)
	url = append_query_param(url, "parent_tech_name", parent_name, allocator)
	url = append_query_param(url, "parent_type", parent_type, allocator)
	url = append_query_param(url, "withShortDescriptions", "true", allocator)
	defer delete(url, allocator)

	body := build_node_structure_request_body(node_keys, allocator)
	defer delete(body, allocator)
	xml, err := send_text(
		client,
		.Post,
		url,
		"application/vnd.sap.as+xml;charset=UTF-8;dataname=com.sap.adt.RepositoryObjectTreeContent",
		"application/vnd.sap.as+xml; charset=UTF-8; dataname=null",
		body,
		allocator,
	)
	if err != .None {
		return {}, err
	}
	defer delete(xml, allocator)
	return parse_repository_node_structure(xml, allocator), .None
}

build_node_structure_request_body :: proc(
	node_keys: []string,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
	strings.write_string(
		&out,
		"<asx:abap version=\"1.0\" xmlns:asx=\"http://www.sap.com/abapxml\">\n<asx:values>\n<DATA>\n",
	)
	if len(node_keys) == 0 {
		strings.write_string(&out, "<TV_NODEKEY>000000</TV_NODEKEY>\n")
	} else {
		for key in node_keys {
			strings.write_string(&out, "<TV_NODEKEY>")
			escape_xml_text(&out, key)
			strings.write_string(&out, "</TV_NODEKEY>\n")
		}
	}
	strings.write_string(&out, "</DATA>\n</asx:values>\n</asx:abap>")
	return strings.to_string(out)
}

escape_xml_text :: proc(out: ^strings.Builder, value: string) {
	for i := 0; i < len(value); i += 1 {
		switch value[i] {
		case '&':
			strings.write_string(out, "&amp;")
		case '<':
			strings.write_string(out, "&lt;")
		case '>':
			strings.write_string(out, "&gt;")
		case:
			strings.write_string(out, value[i:i + 1])
		}
	}
}

send_text :: proc(
	client: ^Client,
	method: http.Method,
	url: string,
	accept: string,
	content_type: string,
	body: string,
	temp_allocator: mem.Allocator,
) -> (
	string,
	Error,
) {
	if method != .Get && client.csrf_token == "" {
		if err := ensure_session(client, temp_allocator); err != .None {
			return "", err
		}
	} else if client.csrf_token == "" {
		if err := ensure_session(client, temp_allocator); err != .None {
			return "", err
		}
	}

	request: http.Request
	http.request_init(&request, method, url, temp_allocator)
	set_common_headers(&request, client, accept, temp_allocator)
	if content_type != "" {
		http.header_set(&request.headers, "Content-Type", content_type, temp_allocator)
		request.body = transmute([]u8)body
	}
	response, http_err := http.client_do(&client.http, &request, temp_allocator)
	if http_err != .None {
		return "", http_error_to_adt(http_err)
	}
	if !status_success(response.status_code) {
		return "", .Bad_Status
	}
	return string(response.body), .None
}

ensure_session :: proc(client: ^Client, temp_allocator: mem.Allocator) -> Error {
	url := absolute_url(&client.connection, "/runtime/systemmessages", temp_allocator)
	request: http.Request
	http.request_init(&request, .Get, url, temp_allocator)
	set_auth_header(&request, &client.connection, temp_allocator)
	http.header_set(&request.headers, "Cache-Control", "no-cache", temp_allocator)
	http.header_set(&request.headers, "Accept", SESSION_BOOTSTRAP_ACCEPT, temp_allocator)
	http.header_set(&request.headers, "x-csrf-token", "Fetch", temp_allocator)

	response, http_err := http.client_do(&client.http, &request, temp_allocator)
	if http_err != .None {
		return http_error_to_adt(http_err)
	}
	if !status_success(response.status_code) {
		return .Bad_Status
	}
	token, ok := http.header_get(response.headers, "x-csrf-token")
	token = strings.trim_space(token)
	if !ok || token == "" {
		return .Missing_Csrf_Token
	}
	client.csrf_token = strings.clone(token, client.allocator)
	if cookie, cookie_ok := http.header_get(response.headers, "set-cookie"); cookie_ok {
		client.cookie = strings.clone(cookie_pair(cookie), client.allocator)
	}
	return .None
}

http_error_to_adt :: proc(err: http.Error) -> Error {
	switch err {
	case .None:
		return .None
	case .Invalid_Url:
		return .Invalid_Url
	case .Unsupported_Scheme:
		return .Http_Unsupported_Scheme
	case .Network:
		return .Http_Network
	case .Response_Too_Large:
		return .Http_Response_Too_Large
	case .Bad_Response:
		return .Http_Bad_Response
	case .Invalid_Content_Length:
		return .Http_Invalid_Content_Length
	case .Unsupported_Transfer_Encoding:
		return .Http_Unsupported_Transfer_Encoding
	case .Invalid_Chunk:
		return .Http_Invalid_Chunk
	}
	return .Http
}

set_common_headers :: proc(
	request: ^http.Request,
	client: ^Client,
	accept: string,
	allocator: mem.Allocator,
) {
	set_auth_header(request, &client.connection, allocator)
	http.header_set(&request.headers, "Cache-Control", "no-cache", allocator)
	http.header_set(&request.headers, "Accept", accept, allocator)
	if client.csrf_token != "" {
		http.header_set(&request.headers, "x-csrf-token", client.csrf_token, allocator)
	}
	if client.cookie != "" {
		http.header_set(&request.headers, "Cookie", client.cookie, allocator)
	}
}

set_auth_header :: proc(
	request: ^http.Request,
	config: ^Connection_Config,
	allocator: mem.Allocator,
) {
	credentials := strings.builder_make(allocator)
	strings.write_string(&credentials, config.username)
	strings.write_byte(&credentials, ':')
	strings.write_string(&credentials, config.password)
	raw := strings.to_string(credentials)
	encoded := base64.encode(transmute([]byte)raw, allocator = allocator)
	value := strings.builder_make(allocator)
	strings.write_string(&value, "Basic ")
	strings.write_string(&value, encoded)
	header := strings.to_string(value)
	http.header_set(&request.headers, "Authorization", header, allocator)
}

fetch_source_by_path :: proc(
	client: ^Client,
	object_path, resolved_by: string,
	allocator: mem.Allocator,
) -> (
	Source_Fetch,
	Error,
) {
	source_path := object_path
	if !strings.has_suffix(source_path, "/source/main") {
		out := strings.builder_make(allocator)
		strings.write_string(&out, source_path)
		strings.write_string(&out, "/source/main")
		source_path = strings.to_string(out)
	}
	url := absolute_url(&client.connection, source_path, allocator)
	body, err := send_text(client, .Get, url, "text/plain", "", "", allocator)
	if err != .None {
		return {}, err
	}
	return Source_Fetch {
			request_url = strings.clone(url, allocator),
			object_uri = strings.clone(object_path, allocator),
			resolved_by = resolved_by,
			body = body,
		},
		.None
}

source_path :: proc(prefix, name: string, allocator: mem.Allocator) -> string {
	encoded := encode_path_segment(name, allocator)
	out := strings.builder_make(allocator)
	strings.write_string(&out, prefix)
	strings.write_string(&out, encoded)
	return strings.to_string(out)
}

append_query_param :: proc(url, key, value: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, url)
	if strings.index_byte(url, '?') >= 0 {
		strings.write_byte(&out, '&')
	} else {
		strings.write_byte(&out, '?')
	}
	strings.write_string(&out, encode_query_component(key, allocator))
	strings.write_byte(&out, '=')
	strings.write_string(&out, encode_query_component(value, allocator))
	return strings.to_string(out)
}

encode_query_component :: proc(value: string, allocator: mem.Allocator) -> string {
	return encode_path_segment(value, allocator)
}

first_config_value :: proc(
	keys: []string,
	override: string,
	dotenv: ^Dotenv_Defaults,
	allocator: mem.Allocator,
) -> (
	string,
	bool,
) {
	if value, ok := normalized_non_empty_clone(override, allocator); ok {
		return value, true
	}
	for key in keys {
		if raw, ok := os.lookup_env(key, allocator); ok {
			if value, value_ok := normalized_non_empty_clone(raw, allocator); value_ok {
				delete(raw, allocator)
				return value, true
			}
			delete(raw, allocator)
		}
		if raw, ok := dotenv.values[key]; ok {
			if value, value_ok := normalized_non_empty_clone(raw, allocator); value_ok {
				return value, true
			}
		}
	}
	return "", false
}

normalized_non_empty_clone :: proc(value: string, allocator: mem.Allocator) -> (string, bool) {
	trimmed := strings.trim_space(value)
	if trimmed == "" {
		return "", false
	}
	return strings.clone(trimmed, allocator), true
}

find_dotenv_from_base :: proc(
	base: string,
	repo_first: bool,
	allocator: mem.Allocator,
) -> (
	string,
	bool,
) {
	dir := strings.clone(base, allocator)
	for {
		if repo_first {
			git_path := filepath.join({dir, ".git"}, allocator) or_else ""
			if git_path != "" && os.is_dir(git_path) {
				defer delete(git_path, allocator)
				env_path := filepath.join({dir, ".env"}, allocator) or_else ""
				if env_path != "" && os.is_file(env_path) {
					return env_path, true
				}
				delete(env_path, allocator)
				return "", false
			}
			delete(git_path, allocator)
		} else {
			env_path := filepath.join({dir, ".env"}, allocator) or_else ""
			if env_path != "" && os.is_file(env_path) {
				return env_path, true
			}
			delete(env_path, allocator)
		}
		parent := filepath.dir(dir)
		if parent == dir || parent == "" {
			break
		}
		old := dir
		dir = strings.clone(parent, allocator)
		delete(old, allocator)
	}
	delete(dir, allocator)
	return "", false
}

parse_dotenv_file :: proc(path: string, allocator: mem.Allocator) -> (Dotenv_Defaults, Error) {
	data, read_err := os.read_entire_file(path, allocator)
	if read_err != nil {
		defaults: Dotenv_Defaults
		dotenv_defaults_init(&defaults, allocator)
		return defaults, .File_Read
	}
	defer delete(data, allocator)
	defaults, _, ok := parse_dotenv_contents(string(data), allocator)
	if !ok {
		return defaults, .Dotenv_Parse
	}
	return defaults, .None
}

parse_dotenv_value :: proc(raw: string, allocator: mem.Allocator) -> (string, bool) {
	if len(raw) >= 2 {
		first := raw[0]
		last := raw[len(raw) - 1]
		if (first == '"' && last == '"') || (first == '\'' && last == '\'') {
			return strings.clone(raw[1:len(raw) - 1], allocator), true
		}
		if first == '"' || first == '\'' {
			return "", false
		}
	} else if raw == "\"" || raw == "'" {
		return "", false
	}
	value := raw
	if hash := strings.index(raw, " #"); hash >= 0 {
		value = raw[:hash]
	}
	return strings.clone(strings.trim_right(value, " \t"), allocator), true
}

build_named_ref :: proc(
	name, package_name, prefix, object_type, description: string,
	allocator: mem.Allocator,
) -> Object_Ref {
	n := trim_upper(name, allocator)
	encoded := encode_path_segment(n, allocator)
	defer delete(encoded, allocator)
	uri := strings.builder_make(allocator)
	strings.write_string(&uri, prefix)
	strings.write_string(&uri, encoded)
	return Object_Ref {
		uri = strings.to_string(uri),
		object_type = strings.clone(object_type, allocator),
		name = n,
		package_name = strings.clone(package_name, allocator),
		description = strings.clone(description, allocator),
	}
}

clone_object_ref :: proc(entry: ^Object_Ref, allocator: mem.Allocator) -> Object_Ref {
	return Object_Ref {
		uri = strings.clone(entry.uri, allocator),
		object_type = strings.clone(entry.object_type, allocator),
		name = strings.clone(entry.name, allocator),
		package_name = strings.clone(entry.package_name, allocator),
		description = strings.clone(entry.description, allocator),
	}
}

drop_shadowed_ddic_domains :: proc(
	objects: [dynamic]Object_Ref,
	allocator: mem.Allocator,
) -> [dynamic]Object_Ref {
	has_non_domain := false
	for &object_ref in objects {
		if is_ddic_dependency_object(&object_ref) && !is_ddic_domain_object(&object_ref) {
			has_non_domain = true
			break
		}
	}
	if !has_non_domain {
		return objects
	}
	out := make([dynamic]Object_Ref, allocator)
	for &object_ref in objects {
		if is_ddic_domain_object(&object_ref) {
			object_ref_destroy(&object_ref, allocator)
		} else {
			append(&out, object_ref)
		}
	}
	delete(objects)
	return out
}

pick_best_dependency_object :: proc(
	query: string,
	objects: []Object_Ref,
	kind_hint: string,
	allocator: mem.Allocator,
) -> (
	Object_Ref,
	bool,
) {
	exact := make([dynamic]Object_Ref, allocator)
	for &object_ref in objects {
		if strings.equal_fold(strings.trim_space(object_ref.name), query) &&
		   is_supported_dependency_object(&object_ref, kind_hint) {
			append(&exact, clone_object_ref(&object_ref, allocator))
		}
	}
	if len(exact) > 0 {
		if best, ok := pick_preferred_dependency_object(exact[:], kind_hint, allocator); ok {
			object_refs_destroy(&exact, allocator)
			return best, true
		}
		best := clone_object_ref(&exact[0], allocator)
		object_refs_destroy(&exact, allocator)
		return best, true
	}
	delete(exact)
	return pick_preferred_dependency_object(objects, kind_hint, allocator)
}

pick_preferred_dependency_object :: proc(
	objects: []Object_Ref,
	kind_hint: string,
	allocator: mem.Allocator,
) -> (
	Object_Ref,
	bool,
) {
	hint := strings.trim_space(kind_hint)
	for &object_ref in objects {
		if strings.equal_fold(hint, "report") &&
		   strings.equal_fold(object_ref.object_type, "PROG/P") {
			return clone_object_ref(&object_ref, allocator), true
		}
		if strings.equal_fold(hint, "function") &&
		   strings.equal_fold(object_ref.object_type, "FUGR/FF") {
			return clone_object_ref(&object_ref, allocator), true
		}
		if strings.equal_fold(hint, "static") &&
		   ascii_starts_with_ignore_case(object_ref.object_type, "CLAS/") {
			return clone_object_ref(&object_ref, allocator), true
		}
		if strings.equal_fold(hint, "type") &&
		   is_ddic_dependency_object(&object_ref) &&
		   !is_ddic_domain_object(&object_ref) {
			return clone_object_ref(&object_ref, allocator), true
		}
	}
	for &object_ref in objects {
		if strings.equal_fold(hint, "function") &&
		   strings.equal_fold(object_ref.object_type, "FUGR/F") {
			return clone_object_ref(&object_ref, allocator), true
		}
		if (strings.equal_fold(hint, "static") || strings.equal_fold(hint, "type")) &&
		   ascii_starts_with_ignore_case(object_ref.object_type, "INTF/") {
			return clone_object_ref(&object_ref, allocator), true
		}
		if strings.equal_fold(hint, "type") && is_ddic_domain_object(&object_ref) {
			return clone_object_ref(&object_ref, allocator), true
		}
	}
	return {}, false
}

append_direct_class_interface_refs :: proc(
	out: ^[dynamic]Object_Ref,
	name: string,
	fallback_both_kinds: bool,
	allocator: mem.Allocator,
) {
	local := local_object_name(name, allocator)
	defer delete(local, allocator)
	if looks_like_global_interface_name(local) {
		append(out, build_interface_object_ref(name, "", allocator))
		return
	}
	if looks_like_global_class_name(local) {
		append(out, build_class_object_ref(name, "", allocator))
		return
	}
	if fallback_both_kinds {
		append(out, build_class_object_ref(name, "", allocator))
		append(out, build_interface_object_ref(name, "", allocator))
	}
}

looks_like_global_class_name :: proc(local: string) -> bool {
	return(
		strings.has_prefix(local, "CL_") ||
		strings.has_prefix(local, "ZCL_") ||
		strings.has_prefix(local, "YCL_") ||
		strings.has_prefix(local, "CX_") ||
		strings.has_prefix(local, "ZCX_") ||
		strings.has_prefix(local, "YCX_") \
	)
}

looks_like_global_interface_name :: proc(local: string) -> bool {
	return(
		strings.has_prefix(local, "IF_") ||
		strings.has_prefix(local, "ZIF_") ||
		strings.has_prefix(local, "YIF_") \
	)
}

local_object_name :: proc(name: string, allocator: mem.Allocator) -> string {
	normalized := trim_upper(name, allocator)
	if !strings.has_prefix(normalized, "/") {
		return normalized
	}
	last := last_index_byte(normalized, '/')
	if last >= 0 && last + 1 < len(normalized) {
		out := strings.clone(normalized[last + 1:], allocator)
		delete(normalized, allocator)
		return out
	}
	return normalized
}

is_fetchable_ddic_dependency_object :: proc(object_ref: ^Object_Ref) -> bool {
	return is_ddic_dependency_object(object_ref) && !is_ddic_domain_object(object_ref)
}

is_ddic_domain_object :: proc(object_ref: ^Object_Ref) -> bool {
	return ascii_starts_with_ignore_case(object_ref.object_type, "DOMA/")
}

write_indented_line :: proc(out: ^strings.Builder, indent: int, text: string) {
	for _ in 0 ..< indent {
		strings.write_string(out, "  ")
	}
	strings.write_string(out, text)
	strings.write_byte(out, '\n')
}

collect_blocks :: proc(xml, tag: string, allocator: mem.Allocator) -> [dynamic]string {
	out := make([dynamic]string, allocator)
	open := strings.builder_make(allocator)
	strings.write_byte(&open, '<')
	strings.write_string(&open, tag)
	strings.write_byte(&open, '>')
	open_text := strings.to_string(open)
	defer delete(open_text, allocator)
	close := strings.builder_make(allocator)
	strings.write_string(&close, "</")
	strings.write_string(&close, tag)
	strings.write_byte(&close, '>')
	close_text := strings.to_string(close)
	defer delete(close_text, allocator)

	index := 0
	for {
		start_rel := strings.index(xml[index:], open_text)
		if start_rel < 0 {
			break
		}
		start := index + start_rel + len(open_text)
		end_rel := strings.index(xml[start:], close_text)
		if end_rel < 0 {
			break
		}
		end := start + end_rel
		append(&out, strings.clone(xml[start:end], allocator))
		index = end + len(close_text)
	}
	return out
}

read_attr :: proc(attrs, name: string) -> string {
	start := -1
	for i in 0 ..< len(attrs) {
		if i + len(name) + 2 <= len(attrs) &&
		   strings.has_prefix(attrs[i:], name) &&
		   attrs[i + len(name)] == '=' &&
		   attrs[i + len(name) + 1] == '"' {
			start = i
			break
		}
	}
	if start < 0 {
		return ""
	}
	value_start := start + len(name) + 2
	end := strings.index_byte(attrs[value_start:], '"')
	if end < 0 {
		return ""
	}
	return attrs[value_start:value_start + end]
}

read_tag_text :: proc(block, tag: string, allocator: mem.Allocator) -> string {
	open := strings.builder_make(allocator)
	strings.write_byte(&open, '<')
	strings.write_string(&open, tag)
	strings.write_byte(&open, '>')
	open_text := strings.to_string(open)
	defer delete(open_text, allocator)
	close := strings.builder_make(allocator)
	strings.write_string(&close, "</")
	strings.write_string(&close, tag)
	strings.write_byte(&close, '>')
	close_text := strings.to_string(close)
	defer delete(close_text, allocator)

	start := strings.index(block, open_text)
	if start < 0 {
		return ""
	}
	value_start := start + len(open_text)
	end := strings.index(block[value_start:], close_text)
	if end < 0 {
		return ""
	}
	return decode_xml_entities(block[value_start:value_start + end], allocator)
}

decode_xml_entities :: proc(value: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	i := 0
	for i < len(value) {
		switch {
		case strings.has_prefix(value[i:], "&quot;"):
			strings.write_byte(&out, '"')
			i += len("&quot;")
		case strings.has_prefix(value[i:], "&apos;"):
			strings.write_byte(&out, '\'')
			i += len("&apos;")
		case strings.has_prefix(value[i:], "&lt;"):
			strings.write_byte(&out, '<')
			i += len("&lt;")
		case strings.has_prefix(value[i:], "&gt;"):
			strings.write_byte(&out, '>')
			i += len("&gt;")
		case strings.has_prefix(value[i:], "&amp;"):
			strings.write_byte(&out, '&')
			i += len("&amp;")
		case:
			strings.write_byte(&out, value[i])
			i += 1
		}
	}
	return strings.to_string(out)
}

cookie_pair :: proc(value: string) -> string {
	if semi := strings.index_byte(value, ';'); semi >= 0 {
		return strings.trim_space(value[:semi])
	}
	return strings.trim_space(value)
}

status_success :: proc(status: http.Status) -> bool {
	code := int(status)
	return 200 <= code && code < 300
}

sort_object_refs :: proc(values: []Object_Ref) {
	for i in 1 ..< len(values) {
		value := values[i]
		j := i
		for j > 0 && object_ref_less(value, values[j - 1]) {
			values[j] = values[j - 1]
			j -= 1
		}
		values[j] = value
	}
}

object_ref_less :: proc(left, right: Object_Ref) -> bool {
	cmp_type := strings.compare(left.object_type, right.object_type)
	if cmp_type != 0 {
		return cmp_type < 0
	}
	return strings.compare(left.uri, right.uri) < 0
}

trim_upper :: proc(value: string, allocator: mem.Allocator) -> string {
	return strings.to_upper(strings.trim_space(value), allocator)
}

last_index_byte :: proc(value: string, needle: byte) -> int {
	i := len(value) - 1
	for i >= 0 {
		if value[i] == needle {
			return i
		}
		i -= 1
	}
	return -1
}

ascii_starts_with_ignore_case :: proc(value, prefix: string) -> bool {
	return len(value) >= len(prefix) && strings.equal_fold(value[:len(prefix)], prefix)
}

ascii_contains_ignore_case :: proc(value, needle: string) -> bool {
	return ascii_index_ignore_case(value, needle) >= 0
}

ascii_index_ignore_case :: proc(value, needle: string) -> int {
	if needle == "" {
		return 0
	}
	if len(needle) > len(value) {
		return -1
	}
	for i in 0 ..= len(value) - len(needle) {
		if strings.equal_fold(value[i:i + len(needle)], needle) {
			return i
		}
	}
	return -1
}

ascii_uri_byte :: proc(value: byte) -> bool {
	return(
		('0' <= value && value <= '9') ||
		('A' <= value && value <= 'Z') ||
		('a' <= value && value <= 'z') ||
		value == '-' ||
		value == '_' ||
		value == '.' ||
		value == '~' \
	)
}

hex_upper :: proc(value: byte) -> byte {
	if value <= 9 {
		return '0' + value
	}
	return 'A' + (value - 10)
}
