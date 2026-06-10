package abap_frontend_remote_dependencies

import "src:adt"
import "src:ast"
import dep_store "src:dependency_store"

import "core:hash"
import "core:mem"
import "core:strings"

Remote_Dependency_Kind :: enum {
	Include,
	Message_Class,
	Report,
	Function,
	Class,
	Interface,
	Type,
	Symbol,
}

Source_Order :: enum {
	ADT_First,
	Local_First,
}

Source_Kind :: enum {
	Unknown,
	Cache,
	Local_Export,
	ADT,
	Type_Pool,
}

ADT_Availability_Status :: enum {
	Unknown,
	Available,
	Unavailable,
}

ADT_Availability :: struct {
	status: ADT_Availability_Status,
	error:  adt.Error,
}

Remote_Dependency_Object_Role :: enum {
	Unknown,
	Report,
	Function_Module,
	Class,
	Interface,
	DDIC_Type,
	DDIC_Table,
	Type_Pool,
}

Request :: struct {
	name: string,
	kind: Remote_Dependency_Kind,
}

Remote_Dependency_Key :: struct {
	name: string,
	kind: Remote_Dependency_Kind,
}

Config :: struct {
	cache:              ^dep_store.Dependency_Store,
	profile:            ^dep_store.Dependency_Profile,
	cache_any_profile:  bool,
	local_export_roots: []string,
	adt_client:         ^adt.Client,
	adt_availability:   ^ADT_Availability,
	source_order:       Source_Order,
}

State :: struct {
	seen_cache_requests:      map[Remote_Dependency_Key]bool,
	seen_artifacts:           map[i64]bool,
	seen_local_requests:      map[Remote_Dependency_Key]bool,
	seen_adt_requests:        map[Remote_Dependency_Key]bool,
	seen_typepool_requests:   map[Remote_Dependency_Key]bool,
	seen_result_uris:         map[string]bool,
}

Artifact :: struct {
	request:        Request,
	source_kind:    Source_Kind,
	artifact_id:    i64,
	object_kind:    string,
	object_name:    string,
	object_uri:     string,
	object_type:    string,
	file_extension: string,
	source_text:    string,
	shared:         bool,
}

Interface_AST :: struct {
	key:         Remote_Dependency_Key,
	role:        Remote_Dependency_Object_Role,
	path:        string,
	root:        ^ast.File,
	source_hash: u64,
	generation:  u64,
}

Source_AST :: struct {
	key:            Remote_Dependency_Key,
	path:           string,
	root:           ^ast.File,
	provided_names: [dynamic]string,
	source_hash:    u64,
	generation:     u64,
}

Diagnostic :: struct {
	request: Request,
	source:  Source_Kind,
	message: string,
}

Open_Source :: struct {
	request:        Request,
	source_kind:    Source_Kind,
	object_kind:    string,
	object_name:    string,
	object_uri:     string,
	object_type:    string,
	file_extension: string,
	path:           string,
	source_text:    string,
	root:           ^ast.File,
	source_hash:    u64,
}

Result :: struct {
	allocator:        mem.Allocator,
	interfaces:       [dynamic]Interface_AST,
	sources:          [dynamic]Source_AST,
	misses:           [dynamic]Request,
	blocked_requests: [dynamic]Request,
	diagnostics:      [dynamic]Diagnostic,
}

state_make :: proc(allocator: mem.Allocator) -> State {
	return State {
		seen_cache_requests    = make(map[Remote_Dependency_Key]bool, 64, allocator),
		seen_artifacts         = make(map[i64]bool, 64, allocator),
		seen_local_requests    = make(map[Remote_Dependency_Key]bool, 64, allocator),
		seen_adt_requests      = make(map[Remote_Dependency_Key]bool, 64, allocator),
		seen_typepool_requests = make(map[Remote_Dependency_Key]bool, 64, allocator),
		seen_result_uris       = make(map[string]bool, 128, allocator),
	}
}

result_make :: proc(
	allocator: mem.Allocator = context.allocator,
) -> Result {
	return Result {
		allocator        = allocator,
		interfaces       = make([dynamic]Interface_AST, 0, 8, allocator),
		sources          = make([dynamic]Source_AST, 0, 8, allocator),
		misses           = make([dynamic]Request, 0, 8, allocator),
		blocked_requests = make([dynamic]Request, 0, 4, allocator),
		diagnostics      = make([dynamic]Diagnostic, 0, 8, allocator),
	}
}

remote_dependency_key :: proc(request: Request) -> Remote_Dependency_Key {
	return Remote_Dependency_Key {
		name = request.name,
		kind = request.kind,
	}
}

normalize_request :: proc(
	request: Request,
	allocator: mem.Allocator,
) -> (Request, bool) {
	name := strings.to_lower(strings.trim_space(request.name), allocator)
	if name == "" {
		return {}, false
	}
	return Request {
		name = name,
		kind = request.kind,
	}, true
}

normalize_requests :: proc(
	requests: []Request,
	allocator: mem.Allocator,
) -> [dynamic]Request {
	out := make([dynamic]Request, 0, len(requests), allocator)
	seen := make(map[Remote_Dependency_Key]bool, len(requests), context.temp_allocator)
	for request in requests {
		normalized, ok := normalize_request(request, allocator)
		if !ok {
			continue
		}
		key := remote_dependency_key(normalized)
		if key in seen {
			continue
		}
		seen[key] = true
		append(&out, normalized)
	}
	return out
}

unseen_requests :: proc(
	requests: []Request,
	seen: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) -> [dynamic]Request {
	out := make([dynamic]Request, 0, len(requests), allocator)
	for request in requests {
		if seen == nil {
			append(&out, request)
			continue
		}
		key := remote_dependency_key(request)
		if key in seen^ {
			continue
		}
		owned := key
		owned.name = strings.clone(key.name, seen.allocator)
		seen^[owned] = true
		append(&out, request)
	}
	return out
}

@(private)
result_add_miss :: proc(
	result: ^Result,
	request: Request,
) {
	for existing in result.misses {
		if remote_dependency_key(existing) == remote_dependency_key(request) {
			return
		}
	}
	append(&result.misses, clone_request(request, result.allocator))
}

@(private)
result_add_blocked :: proc(
	result: ^Result,
	request: Request,
) {
	for existing in result.blocked_requests {
		if remote_dependency_key(existing) == remote_dependency_key(request) {
			return
		}
	}
	append(&result.blocked_requests, clone_request(request, result.allocator))
}

@(private)
result_add_diagnostic :: proc(
	result: ^Result,
	request: Request,
	source: Source_Kind,
	message: string,
) {
	append(
		&result.diagnostics,
		Diagnostic {
			request = clone_request(request, result.allocator),
			source  = source,
			message = strings.clone(message, result.allocator),
		},
	)
}

@(private)
clone_request :: proc(
	request: Request,
	allocator: mem.Allocator,
) -> Request {
	return Request {
		name = strings.clone(request.name, allocator),
		kind = request.kind,
	}
}

source_hash :: proc(source: string) -> u64 {
	return hash.fnv64a(transmute([]byte)source)
}

standalone_dependency_profile :: proc() -> dep_store.Dependency_Profile {
	return dep_store.Dependency_Profile {
		product_version = "adt",
		default_package_version = "default",
	}
}
