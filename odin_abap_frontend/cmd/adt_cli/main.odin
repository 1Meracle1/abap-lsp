package main

import adt "../../src/adt"

import json "core:encoding/json"
import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:strconv"
import "core:strings"

Parse_Status :: enum u8 {
	Ok,
	Help,
	Error,
}

Command_Kind :: enum u8 {
	None,
	Search,
	Get_Source,
	Get_Ddic,
	Children,
}

Command :: struct {
	kind:           Command_Kind,
	query:          string,
	max_results:    int,
	source_kind:    adt.Source_Kind,
	ddic_kind:      adt.Ddic_Kind,
	child_kind:     adt.Child_Kind,
	name:           string,
	function_group: string,
}

Cli :: struct {
	raw_output: bool,
	env_dir:    string,
	connection: adt.Connection_Overrides,
	command:    Command,
}

Arg_Cursor :: struct {
	args:  []string,
	index: int,
}

Object_Ref_JSON :: struct {
	uri:          string,
	object_type:  string `json:"type"`,
	name:         string,
	package_name: string,
	description:  string,
}

Search_JSON :: struct {
	command:     string,
	query:       string,
	max_results: int,
	results:     []Object_Ref_JSON,
}

Get_Source_JSON :: struct {
	command:        string,
	kind:           string,
	name:           string,
	function_group: json.Value,
	resolved_by:    string,
	object_uri:     json.Value,
	request_url:    string,
	source:         string,
}

Get_Ddic_JSON :: struct {
	command:     string,
	kind:        string,
	name:        string,
	request_url: string,
	xml:         string,
}

Category_JSON :: struct {
	category: string,
	label:    string,
}

Object_Type_JSON :: struct {
	object_type:  string,
	category_tag: string,
	label:        string,
	node_id:      string,
}

Child_JSON :: struct {
	category_tag:      string,
	object_type_label: string,
	object_type:       string,
	name:              string,
	uri:               string,
	vit_uri:           string,
	expandable:        bool,
}

Children_JSON :: struct {
	command:      string,
	kind:         string,
	name:         string,
	categories:   []Category_JSON,
	object_types: []Object_Type_JSON,
	children:     []Child_JSON,
}

main :: proc() {
	arena: virtual.Arena
	_ = virtual.arena_init_growing(&arena, mem.Gigabyte)
	allocator := virtual.arena_allocator(&arena)

	cli: Cli
	switch parse_cli_args(os.args[1:], &cli) {
	case .Ok:
	case .Help:
		print_usage()
		return
	case .Error:
		os.exit(1)
	}

	dotenv, dotenv_err := adt.load_dotenv_defaults(cli.env_dir, allocator)
	if dotenv_err != .None {
		fail_adt("load .env defaults", dotenv_err)
	}
	connection, config_err := adt.connection_config_from_sources(&cli.connection, &dotenv, allocator)
	if config_err != .None {
		fail_adt("connection config", config_err)
	}

	client: adt.Client
	adt.client_init(&client, connection, allocator)
	if err := run_command(&client, &cli, allocator); err != .None {
		fail_adt("ADT request", err)
	}
}

parse_cli_args :: proc(args: []string, cli: ^Cli) -> Parse_Status {
	if len(args) == 0 {
		print_usage()
		return .Error
	}

	cursor := Arg_Cursor{args = args}
	command_word: string
	if status := next_non_common(&cursor, cli, &command_word); status != .Ok {
		return status
	}

	status := Parse_Status.Ok
	switch command_word {
	case "search":
		status = parse_search_command(&cursor, cli)
	case "get":
		status = parse_get_command(&cursor, cli)
	case "children":
		status = parse_children_command(&cursor, cli)
	case "help", "-h", "--help":
		return .Help
	case:
		fmt.eprintf("unknown command %q\n", command_word)
		print_usage()
		return .Error
	}
	if status != .Ok {
		return status
	}
	if cli.raw_output && cli.command.kind != .Get_Source && cli.command.kind != .Get_Ddic {
		fmt.eprintln("--raw only applies to `get source` and `get ddic`")
		print_usage()
		return .Error
	}
	return .Ok
}

parse_search_command :: proc(cursor: ^Arg_Cursor, cli: ^Cli) -> Parse_Status {
	query := ""
	max_results := 51
	for {
		arg, ok := cursor_next(cursor)
		if !ok {
			break
		}
		if handled, status := handle_common_arg(arg, cursor, cli); handled || status != .Ok {
			if status != .Ok {
				return status
			}
			continue
		}
		if arg == "--max-results" {
			value, value_ok := next_required(cursor, "--max-results")
			if !value_ok {
				return .Error
			}
			if !parse_non_negative_int(value, "--max-results", &max_results) {
				return .Error
			}
			continue
		}
		if strings.has_prefix(arg, "--max-results=") {
			if !parse_non_negative_int(arg[len("--max-results="):], "--max-results", &max_results) {
				return .Error
			}
			continue
		}
		if strings.has_prefix(arg, "-") {
			fmt.eprintf("unknown option %q\n", arg)
			print_usage()
			return .Error
		}
		if query != "" {
			fmt.eprintln("unexpected extra argument")
			print_usage()
			return .Error
		}
		query = arg
	}
	if query == "" {
		fmt.eprintln("search requires a query")
		print_usage()
		return .Error
	}
	cli.command = Command{kind = .Search, query = query, max_results = max_results}
	return .Ok
}

parse_get_command :: proc(cursor: ^Arg_Cursor, cli: ^Cli) -> Parse_Status {
	family: string
	if status := next_non_common(cursor, cli, &family); status != .Ok {
		return status
	}
	switch family {
	case "source":
		return parse_get_source_command(cursor, cli)
	case "ddic":
		return parse_get_ddic_command(cursor, cli)
	case:
		fmt.eprintf("unknown `get` family %q; expected `source` or `ddic`\n", family)
		print_usage()
		return .Error
	}
}

parse_get_source_command :: proc(cursor: ^Arg_Cursor, cli: ^Cli) -> Parse_Status {
	kind_token: string
	if status := next_non_common(cursor, cli, &kind_token); status != .Ok {
		return status
	}
	kind, kind_ok := adt.source_kind_parse(kind_token)
	if !kind_ok {
		fmt.eprintf("unknown source kind %q; expected report/include/class/function-group/function-module/interface\n", kind_token)
		print_usage()
		return .Error
	}

	name := ""
	group := ""
	for {
		arg, ok := cursor_next(cursor)
		if !ok {
			break
		}
		if handled, status := handle_common_arg(arg, cursor, cli); handled || status != .Ok {
			if status != .Ok {
				return status
			}
			continue
		}
		if arg == "--group" {
			value, value_ok := next_required(cursor, "--group")
			if !value_ok {
				return .Error
			}
			group = value
			continue
		}
		if strings.has_prefix(arg, "--group=") {
			group = arg[len("--group="):]
			continue
		}
		if strings.has_prefix(arg, "-") {
			fmt.eprintf("unknown option %q\n", arg)
			print_usage()
			return .Error
		}
		if name != "" {
			fmt.eprintln("unexpected extra argument")
			print_usage()
			return .Error
		}
		name = arg
	}
	if name == "" {
		fmt.eprintln("get source requires an object name")
		print_usage()
		return .Error
	}
	cli.command = Command{kind = .Get_Source, source_kind = kind, name = name, function_group = group}
	return .Ok
}

parse_get_ddic_command :: proc(cursor: ^Arg_Cursor, cli: ^Cli) -> Parse_Status {
	kind_token: string
	if status := next_non_common(cursor, cli, &kind_token); status != .Ok {
		return status
	}
	kind, kind_ok := adt.ddic_kind_parse(kind_token)
	if !kind_ok {
		fmt.eprintf("unknown DDIC kind %q; expected data-element/table-type/structure/view/table\n", kind_token)
		print_usage()
		return .Error
	}

	name := ""
	for {
		arg, ok := cursor_next(cursor)
		if !ok {
			break
		}
		if handled, status := handle_common_arg(arg, cursor, cli); handled || status != .Ok {
			if status != .Ok {
				return status
			}
			continue
		}
		if strings.has_prefix(arg, "-") {
			fmt.eprintf("unknown option %q\n", arg)
			print_usage()
			return .Error
		}
		if name != "" {
			fmt.eprintln("unexpected extra argument")
			print_usage()
			return .Error
		}
		name = arg
	}
	if name == "" {
		fmt.eprintln("get ddic requires an object name")
		print_usage()
		return .Error
	}
	cli.command = Command{kind = .Get_Ddic, ddic_kind = kind, name = name}
	return .Ok
}

parse_children_command :: proc(cursor: ^Arg_Cursor, cli: ^Cli) -> Parse_Status {
	kind_token: string
	if status := next_non_common(cursor, cli, &kind_token); status != .Ok {
		return status
	}
	kind, kind_ok := adt.child_kind_parse(kind_token)
	if !kind_ok {
		fmt.eprintf("unknown children kind %q; expected package/report/function-group\n", kind_token)
		print_usage()
		return .Error
	}

	name := ""
	for {
		arg, ok := cursor_next(cursor)
		if !ok {
			break
		}
		if handled, status := handle_common_arg(arg, cursor, cli); handled || status != .Ok {
			if status != .Ok {
				return status
			}
			continue
		}
		if strings.has_prefix(arg, "-") {
			fmt.eprintf("unknown option %q\n", arg)
			print_usage()
			return .Error
		}
		if name != "" {
			fmt.eprintln("unexpected extra argument")
			print_usage()
			return .Error
		}
		name = arg
	}
	if name == "" {
		fmt.eprintln("children requires an object name")
		print_usage()
		return .Error
	}
	cli.command = Command{kind = .Children, child_kind = kind, name = name}
	return .Ok
}

next_non_common :: proc(cursor: ^Arg_Cursor, cli: ^Cli, out: ^string) -> Parse_Status {
	for {
		arg, ok := cursor_next(cursor)
		if !ok {
			print_usage()
			return .Error
		}
		if handled, status := handle_common_arg(arg, cursor, cli); handled || status != .Ok {
			if status != .Ok {
				return status
			}
			continue
		}
		out^ = arg
		return .Ok
	}
}

handle_common_arg :: proc(arg: string, cursor: ^Arg_Cursor, cli: ^Cli) -> (bool, Parse_Status) {
	switch arg {
	case "-h", "--help", "help":
		return true, .Help
	case "--raw":
		cli.raw_output = true
		return true, .Ok
	case "--url":
		return read_common_value(cursor, "--url", &cli.connection.base_url)
	case "--user":
		return read_common_value(cursor, "--user", &cli.connection.username)
	case "--password":
		return read_common_value(cursor, "--password", &cli.connection.password)
	case "--sap-client":
		return read_common_value(cursor, "--sap-client", &cli.connection.sap_client)
	case "--env-dir":
		return read_common_value(cursor, "--env-dir", &cli.env_dir)
	}
	if strings.has_prefix(arg, "--url=") {
		cli.connection.base_url = arg[len("--url="):]
		return true, .Ok
	}
	if strings.has_prefix(arg, "--user=") {
		cli.connection.username = arg[len("--user="):]
		return true, .Ok
	}
	if strings.has_prefix(arg, "--password=") {
		cli.connection.password = arg[len("--password="):]
		return true, .Ok
	}
	if strings.has_prefix(arg, "--sap-client=") {
		cli.connection.sap_client = arg[len("--sap-client="):]
		return true, .Ok
	}
	if strings.has_prefix(arg, "--env-dir=") {
		cli.env_dir = arg[len("--env-dir="):]
		return true, .Ok
	}
	return false, .Ok
}

read_common_value :: proc(cursor: ^Arg_Cursor, flag: string, out: ^string) -> (bool, Parse_Status) {
	value, ok := next_required(cursor, flag)
	if !ok {
		return true, .Error
	}
	out^ = value
	return true, .Ok
}

cursor_next :: proc(cursor: ^Arg_Cursor) -> (string, bool) {
	if cursor.index >= len(cursor.args) {
		return "", false
	}
	value := cursor.args[cursor.index]
	cursor.index += 1
	return value, true
}

next_required :: proc(cursor: ^Arg_Cursor, flag: string) -> (string, bool) {
	if value, ok := cursor_next(cursor); ok {
		return value, true
	}
	fmt.eprintf("expected value after %s\n", flag)
	print_usage()
	return "", false
}

parse_non_negative_int :: proc(value, flag: string, out: ^int) -> bool {
	parsed, ok := strconv.parse_int(value, 10)
	if !ok || parsed < 0 {
		fmt.eprintf("invalid value for %s: %q\n", flag, value)
		return false
	}
	out^ = parsed
	return true
}

run_command :: proc(client: ^adt.Client, cli: ^Cli, allocator: mem.Allocator) -> adt.Error {
	switch cli.command.kind {
	case .None:
		return .Invalid_Url
	case .Search:
		results, err := adt.search_repository_objects(client, cli.command.query, cli.command.max_results, allocator)
		if err != .None {
			return err
		}
		print_search_json(cli.command.query, cli.command.max_results, results[:], allocator)
	case .Get_Source:
		fetched, err := fetch_source_for_cli(client, &cli.command, allocator)
		if err != .None {
			return err
		}
		if cli.raw_output {
			fmt.print(fetched.body)
		} else {
			print_get_source_json(&cli.command, &fetched, allocator)
		}
	case .Get_Ddic:
		fetched, err := adt.fetch_ddic(client, cli.command.ddic_kind, cli.command.name, allocator)
		if err != .None {
			return err
		}
		if cli.raw_output {
			fmt.print(fetched.body)
		} else {
			print_get_ddic_json(&cli.command, &fetched, allocator)
		}
	case .Children:
		structure, children, err := adt.list_children(client, cli.command.child_kind, cli.command.name, allocator)
		if err != .None {
			return err
		}
		print_children_json(&cli.command, &structure, children[:], allocator)
	}
	return .None
}

fetch_source_for_cli :: proc(
	client: ^adt.Client,
	command: ^Command,
	allocator: mem.Allocator,
) -> (adt.Source_Fetch, adt.Error) {
	if command.source_kind != .Function_Module || strings.trim_space(command.function_group) != "" {
		return adt.fetch_source(client, command.source_kind, command.name, command.function_group, allocator)
	}

	objects, search_err := adt.search_repository_objects(client, command.name, 20, allocator)
	if search_err != .None {
		return {}, search_err
	}
	selected := adt.select_dependency_objects(command.name, objects[:], "function", allocator)
	if len(selected) == 0 {
		return {}, .Invalid_Url
	}

	body, fetch_err := adt.fetch_object_source(client, selected[0].uri, allocator)
	if fetch_err != .None {
		return {}, fetch_err
	}
	request_path := selected[0].uri
	if !strings.has_suffix(request_path, "/source/main") {
		out := strings.builder_make(allocator)
		strings.write_string(&out, request_path)
		strings.write_string(&out, "/source/main")
		request_path = strings.to_string(out)
	}
	return adt.Source_Fetch {
		request_url = adt.absolute_url(&client.connection, request_path, allocator),
		object_uri  = selected[0].uri,
		resolved_by = "search",
		body        = body,
	}, .None
}

print_search_json :: proc(query: string, max_results: int, results: []adt.Object_Ref, allocator: mem.Allocator) {
	out := make([]Object_Ref_JSON, len(results), allocator)
	for &entry, i in results {
		out[i] = object_ref_json(&entry)
	}
	emit_json(Search_JSON{"search", query, max_results, out}, allocator)
}

print_get_source_json :: proc(command: ^Command, fetched: ^adt.Source_Fetch, allocator: mem.Allocator) {
	emit_json(Get_Source_JSON {
		command        = "get-source",
		kind           = adt.source_kind_string(command.source_kind),
		name           = command.name,
		function_group = optional_json_string(command.function_group),
		resolved_by    = fetched.resolved_by,
		object_uri     = optional_json_string(fetched.object_uri),
		request_url    = fetched.request_url,
		source         = fetched.body,
	}, allocator)
}

print_get_ddic_json :: proc(command: ^Command, fetched: ^adt.Ddic_Fetch, allocator: mem.Allocator) {
	emit_json(Get_Ddic_JSON {
		command     = "get-ddic",
		kind        = adt.ddic_kind_string(command.ddic_kind),
		name        = command.name,
		request_url = fetched.request_url,
		xml         = fetched.body,
	}, allocator)
}

print_children_json :: proc(command: ^Command, structure: ^adt.Repository_Node_Structure, children: []adt.Child_Entry, allocator: mem.Allocator) {
	categories := make([]Category_JSON, len(structure.categories), allocator)
	for entry, i in structure.categories {
		categories[i] = Category_JSON{entry.category, entry.label}
	}
	object_types := make([]Object_Type_JSON, len(structure.object_types), allocator)
	for entry, i in structure.object_types {
		object_types[i] = Object_Type_JSON{entry.object_type, entry.category_tag, entry.label, entry.node_id}
	}
	child_items := make([]Child_JSON, len(children), allocator)
	for entry, i in children {
		child_items[i] = Child_JSON {
			category_tag      = entry.category_tag,
			object_type_label = entry.object_type_label,
			object_type       = entry.object_type,
			name              = entry.name,
			uri               = entry.uri,
			vit_uri           = entry.vit_uri,
			expandable        = entry.expandable,
		}
	}
	emit_json(Children_JSON {
		command      = "children",
		kind         = adt.child_kind_string(command.child_kind),
		name         = command.name,
		categories   = categories,
		object_types = object_types,
		children     = child_items,
	}, allocator)
}

object_ref_json :: proc(entry: ^adt.Object_Ref) -> Object_Ref_JSON {
	return Object_Ref_JSON {
		uri          = entry.uri,
		object_type  = entry.object_type,
		name         = entry.name,
		package_name = entry.package_name,
		description  = entry.description,
	}
}

optional_json_string :: proc(value: string) -> json.Value {
	if value == "" {
		return json.Value(json.Null(nil))
	}
	return json.Value(json.String(value))
}

emit_json :: proc(value: any, allocator: mem.Allocator) {
	bytes, err := json.marshal(
		value,
		json.Marshal_Options{spec = .JSON, pretty = true, use_spaces = true, spaces = 2},
		allocator,
	)
	if err != nil {
		fmt.eprintf("error: failed to serialize JSON: %v\n", err)
		os.exit(1)
	}
	fmt.println(string(bytes))
}

fail_adt :: proc(action: string, err: adt.Error) {
	fmt.eprintf("error: %s failed: %s\n", action, adt_error_message(err))
	os.exit(1)
}

adt_error_message :: proc(err: adt.Error) -> string {
	switch err {
	case .None:
		return "none"
	case .Missing_Base_Url:
		return "missing SAP ADT base URL; set ABAP_ADT_URL, ABAP_ADT_BASE_URL, SAPBASE_URL, or --url"
	case .Missing_Username:
		return "missing SAP username; set ABAP_ADT_USER, ABAP_ADT_USERNAME, SAPUSER, or --user"
	case .Missing_Password:
		return "missing SAP password; set ABAP_ADT_PASSWORD, SAPPASS, or --password"
	case .Dotenv_Parse:
		return "failed to parse .env"
	case .File_Read:
		return "failed to read file"
	case .Invalid_Url:
		return "invalid URL or object path"
	case .Http:
		return "HTTP request failed"
	case .Http_Unsupported_Scheme:
		return "unsupported URL scheme; expected http:// or https://"
	case .Http_Network:
		return "network connection or socket I/O failed"
	case .Http_Response_Too_Large:
		return "HTTP response exceeded the configured size limit"
	case .Http_Bad_Response:
		return "HTTP response could not be parsed"
	case .Http_Invalid_Content_Length:
		return "HTTP response has an invalid Content-Length header"
	case .Http_Unsupported_Transfer_Encoding:
		return "HTTP response uses an unsupported Transfer-Encoding"
	case .Http_Invalid_Chunk:
		return "HTTP response has invalid chunked encoding"
	case .Bad_Status:
		return "SAP ADT returned an unsuccessful HTTP status"
	case .Missing_Csrf_Token:
		return "SAP ADT session bootstrap did not return a CSRF token"
	}
	return "unknown error"
}

print_usage :: proc() {
	fmt.println(`ABAP ADT query CLI.`)
	fmt.println("")
	fmt.println(`Usage:`)
	fmt.println(`  adt_cli [connection options] search <query> [--max-results N]`)
	fmt.println(`  adt_cli [connection options] get source <kind> <name> [--group <function-group>] [--raw]`)
	fmt.println(`  adt_cli [connection options] get ddic <kind> <name> [--raw]`)
	fmt.println(`  adt_cli [connection options] children <kind> <name>`)
	fmt.println("")
	fmt.println(`Connection options:`)
	fmt.println(`  --url <URL>              SAP host root or full ADT root`)
	fmt.println(`  --user <USER>            SAP username`)
	fmt.println(`  --password <PASSWORD>    SAP password`)
	fmt.println(`  --sap-client <CLIENT>    Optional SAP client, also read from ABAP_ADT_CLIENT / SAPCLIENT`)
	fmt.println(`  --env-dir <DIR>          Start .env discovery from this directory`)
	fmt.println("")
	fmt.println(`The same connection values can come from:`)
	fmt.println(`  ABAP_ADT_URL / ABAP_ADT_BASE_URL / SAPBASE_URL`)
	fmt.println(`  ABAP_ADT_USER / ABAP_ADT_USERNAME / SAPUSER`)
	fmt.println(`  ABAP_ADT_PASSWORD / SAPPASS`)
	fmt.println("")
	fmt.println(`Commands emit JSON by default. Use --raw on get source or get ddic`)
	fmt.println(`to print only the fetched source/XML.`)
}
