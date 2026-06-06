package abap_frontend_semantic

import "src:ast"
import string_interner "src:string_interner"

import "core:mem"
import "core:strings"

Workspace_File_Kind :: enum {
	Unknown,
	Report,
	Include,
	Class,
	Interface,
	Function_Group,
	Type_Pool,
}

Workspace_Project_Kind :: enum {
	Root,
	Include_Fragment,
}

Workspace_File_Input :: struct {
	path:        string,
	root:        ^ast.File,
	kind:        Workspace_File_Kind,
	object_name: string,
}

Workspace_Include_Edge :: struct {
	name:           string_interner.String,
	name_range:     Range,
	if_found:       bool,
	target_index:   int,
	external_index: int,
	has_target:     bool,
	is_external:    bool,
}

Workspace_File_Facts :: struct {
	input_index:       int,
	path:              string,
	root:              ^ast.File,
	kind:              Workspace_File_Kind,
	explicit_root:     bool,
	provided_names:    [dynamic]string_interner.String,
	include_edges:     [dynamic]Workspace_Include_Edge,
	type_pool_imports: [dynamic]string_interner.String,
	included_by_any:   bool,
}

Workspace_Project_Plan :: struct {
	root_index: int,
	kind:       Workspace_Project_Kind,
}

Project_Discovery :: struct {
	allocator:   mem.Allocator,
	interner:    ^string_interner.Interner,
	external:    ^External_Semantics,
	facts:       [dynamic]Workspace_File_Facts,
	plans:       [dynamic]Workspace_Project_Plan,
	diagnostics: [dynamic]Checker_Diagnostic,
}

project_discovery_build :: proc(
	files: []Workspace_File_Input,
	interner: ^string_interner.Interner,
	external: ^External_Semantics,
	allocator: mem.Allocator,
) -> Project_Discovery {
	discovery := Project_Discovery {
		allocator   = allocator,
		interner    = interner,
		external    = external,
		facts       = make([dynamic]Workspace_File_Facts, 0, len(files), allocator),
		plans       = make([dynamic]Workspace_Project_Plan, 0, len(files), allocator),
		diagnostics = make([dynamic]Checker_Diagnostic, 0, 4, allocator),
	}

	for file, index in files {
		append(&discovery.facts, project_discovery_scan_file(&discovery, file, index))
	}
	project_discovery_resolve_includes(&discovery)
	project_discovery_detect_include_cycles(&discovery)
	project_discovery_build_plans(&discovery)
	return discovery
}

project_discovery_scan_file :: proc(
	discovery: ^Project_Discovery,
	file: Workspace_File_Input,
	index: int,
) -> Workspace_File_Facts {
	suffix_kind, suffix_name := workspace_suffix_metadata(file.path)
	kind := file.kind
	if kind == .Unknown {
		kind = suffix_kind
	}
	facts := Workspace_File_Facts {
		input_index       = index,
		path              = strings.clone(file.path, discovery.allocator) if file.path != "" else "",
		root              = file.root,
		kind              = kind,
		provided_names    = make([dynamic]string_interner.String, 0, 4, discovery.allocator),
		include_edges     = make([dynamic]Workspace_Include_Edge, 0, 4, discovery.allocator),
		type_pool_imports = make([dynamic]string_interner.String, 0, 1, discovery.allocator),
	}
	if file.object_name != "" {
		project_discovery_add_provided_name(discovery, &facts, file.object_name)
	}
	if suffix_name != "" {
		project_discovery_add_provided_name(discovery, &facts, suffix_name)
	}
	if stem := workspace_plain_abap_stem(file.path); stem != "" {
		project_discovery_add_provided_name(discovery, &facts, stem)
	}
	if facts.root == nil {
		return facts
	}

	for stmt in facts.root.stmts {
		#partial switch n in stmt.derived_stmt {
		case ^ast.Report_Stmt:
			if n.kind == .Report || n.kind == .Program {
				if name, _, ok := checker_expr_name(n.name); ok {
					project_discovery_add_provided_name(discovery, &facts, name)
					facts.kind = .Report
					facts.explicit_root = true
				}
			}
		case ^ast.Function_Pool_Decl:
			project_discovery_add_provided_name(discovery, &facts, n.name)
			facts.kind = .Function_Group
			facts.explicit_root = true
		case ^ast.Class_Decl:
			if facts.kind == .Unknown && workspace_global_object_name_matches_file(file.path, n.name, .Class) {
				facts.kind = .Class
			}
			if facts.kind == .Class {
				project_discovery_add_provided_name(discovery, &facts, n.name)
			}
		case ^ast.Interface_Decl:
			if facts.kind == .Unknown && workspace_global_object_name_matches_file(file.path, n.name, .Interface) {
				facts.kind = .Interface
			}
			if facts.kind == .Interface {
				project_discovery_add_provided_name(discovery, &facts, n.name)
			}
		case ^ast.Include_Stmt:
			for include in n.names {
				name := project_discovery_intern_name(discovery, include.name)
				if string_interner.is_valid(name) {
					append(
						&facts.include_edges,
						Workspace_Include_Edge {
							name           = name,
							name_range     = include.range,
							if_found       = n.if_found,
							target_index   = -1,
							external_index = -1,
						},
					)
				}
			}
		case ^ast.Type_Pools_Decl:
			for pool in n.pools {
				name := project_discovery_intern_name(discovery, pool)
				if string_interner.is_valid(name) {
					append(&facts.type_pool_imports, name)
				}
			}
		}
	}
	return facts
}

project_discovery_resolve_includes :: proc(discovery: ^Project_Discovery) {
	for &facts in discovery.facts {
		for &edge in facts.include_edges {
			if index, ok := project_discovery_find_local_provided_name(discovery, edge.name); ok {
				edge.target_index = index
				edge.has_target = true
				discovery.facts[index].included_by_any = true
				if discovery.facts[index].explicit_root {
					project_discovery_add_diagnostic(
						discovery,
						.Root_File_Included,
						edge.name_range,
						"include target is an explicit root",
					)
				}
				continue
			}
			if external_index, external_ok := project_discovery_find_external_source_name(discovery, edge.name); external_ok {
				edge.external_index = external_index
				edge.has_target = true
				edge.is_external = true
			}
		}
	}
}

project_discovery_detect_include_cycles :: proc(discovery: ^Project_Discovery) {
	visiting := make(map[int]bool, len(discovery.facts), discovery.allocator)
	visited := make(map[int]bool, len(discovery.facts), discovery.allocator)
	stack := make([dynamic]int, 0, len(discovery.facts), discovery.allocator)
	for _, index in discovery.facts {
		project_discovery_visit_include_cycles(discovery, index, &visiting, &visited, &stack)
	}
}

project_discovery_visit_include_cycles :: proc(
	discovery: ^Project_Discovery,
	index: int,
	visiting: ^map[int]bool,
	visited: ^map[int]bool,
	stack: ^[dynamic]int,
) {
	if visited^[index] {
		return
	}
	if visiting^[index] {
		return
	}
	visiting^[index] = true
	append(stack, index)
	for edge in discovery.facts[index].include_edges {
		if !edge.has_target || edge.is_external {
			continue
		}
		if visiting^[edge.target_index] {
			project_discovery_add_diagnostic(discovery, .Include_Cycle, edge.name_range, "include cycle")
			continue
		}
		project_discovery_visit_include_cycles(discovery, edge.target_index, visiting, visited, stack)
	}
	_ = pop(stack)
	delete_key(visiting, index)
	visited^[index] = true
}

project_discovery_build_plans :: proc(discovery: ^Project_Discovery) {
	for facts, index in discovery.facts {
		if workspace_file_kind_is_root(facts.kind) {
			append(&discovery.plans, Workspace_Project_Plan{root_index = index, kind = .Root})
			continue
		}
		if !facts.included_by_any {
			append(&discovery.plans, Workspace_Project_Plan{root_index = index, kind = .Include_Fragment})
		}
	}
}

project_discovery_find_local_provided_name :: proc(
	discovery: ^Project_Discovery,
	name: string_interner.String,
) -> (int, bool) {
	for facts, index in discovery.facts {
		for provided in facts.provided_names {
			if provided == name {
				return index, true
			}
		}
	}
	return -1, false
}

project_discovery_find_external_source_name :: proc(
	discovery: ^Project_Discovery,
	name: string_interner.String,
) -> (int, bool) {
	if discovery.external == nil {
		return -1, false
	}
	for source, index in discovery.external.source_files {
		for provided in source.provided_names {
			if provided == name {
				return index, true
			}
		}
	}
	return -1, false
}

project_discovery_edge_for_include :: proc(
	facts: ^Workspace_File_Facts,
	name: string_interner.String,
	range: Range,
) -> (^Workspace_Include_Edge, bool) {
	for &edge in facts.include_edges {
		if edge.name == name && edge.name_range == range {
			return &edge, true
		}
	}
	return nil, false
}

project_discovery_add_provided_name :: proc(
	discovery: ^Project_Discovery,
	facts: ^Workspace_File_Facts,
	name: string,
) {
	interned := project_discovery_intern_name(discovery, name)
	if !string_interner.is_valid(interned) {
		return
	}
	for existing in facts.provided_names {
		if existing == interned {
			return
		}
	}
	append(&facts.provided_names, interned)
}

project_discovery_add_diagnostic :: proc(
	discovery: ^Project_Discovery,
	kind: Checker_Diagnostic_Kind,
	range: Range,
	message: string,
	severity: Checker_Diagnostic_Severity = .Error,
) {
	append(
		&discovery.diagnostics,
		Checker_Diagnostic {
			kind     = kind,
			severity = severity,
			range    = range,
			message  = strings.clone(message, discovery.allocator) if message != "" else "",
		},
	)
}

project_discovery_intern_name :: proc(
	discovery: ^Project_Discovery,
	name: string,
) -> string_interner.String {
	canonical := strings.to_lower(name, context.temp_allocator)
	return string_interner.insert(discovery.interner, canonical)
}

workspace_file_kind_is_root :: proc(kind: Workspace_File_Kind) -> bool {
	#partial switch kind {
	case .Report, .Class, .Interface, .Function_Group, .Type_Pool:
		return true
	case:
	}
	return false
}

workspace_suffix_metadata :: proc(path: string) -> (Workspace_File_Kind, string) {
	base := workspace_path_base_lower(path)
	suffixes := [?]struct {suffix: string, kind: Workspace_File_Kind} {
		{".report.abap", .Report},
		{".program.abap", .Report},
		{".include.abap", .Include},
		{".class.abap", .Class},
		{".interface.abap", .Interface},
		{".function_group.abap", .Function_Group},
		{".type_pool.abap", .Type_Pool},
	}
	for item in suffixes {
		if strings.has_suffix(base, item.suffix) {
			return item.kind, base[:len(base) - len(item.suffix)]
		}
	}
	return .Unknown, ""
}

workspace_plain_abap_stem :: proc(path: string) -> string {
	base := workspace_path_base_lower(path)
	if strings.has_suffix(base, ".abap") {
		return base[:len(base) - len(".abap")]
	}
	return base
}

workspace_global_object_name_matches_file :: proc(
	path: string,
	name: string,
	kind: Workspace_File_Kind,
) -> bool {
	if name == "" {
		return false
	}
	lower := strings.to_lower(name, context.temp_allocator)
	if kind == .Class && (strings.has_prefix(lower, "lcl_") || strings.has_prefix(lower, "lif_")) {
		return false
	}
	if kind == .Interface && strings.has_prefix(lower, "lif_") {
		return false
	}
	return lower == workspace_plain_abap_stem(path)
}

workspace_path_base_lower :: proc(path: string) -> string {
	start := 0
	for i := len(path) - 1; i >= 0; i -= 1 {
		if path[i] == '/' || path[i] == '\\' {
			start = i + 1
			break
		}
	}
	return strings.to_lower(path[start:], context.temp_allocator)
}
