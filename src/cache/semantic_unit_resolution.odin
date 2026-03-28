package cache

import "../lang/ast"
import "../lang/lexer"
import "../lang/symbols"
import os "core:os/os2"
import "core:path/filepath"
import "core:strings"

workspace_uri_for_relative_path :: proc(
	workspace: ^Workspace,
	relative_path: string,
	allocator := context.allocator,
) -> string {
	if workspace == nil || len(workspace.root_path) == 0 {
		return strings.clone("", allocator)
	}

	normalized := normalize_manifest_path(relative_path, context.temp_allocator)
	full_path := filepath.join({workspace.root_path, normalized}, context.temp_allocator)
	return path_to_uri(full_path, allocator)
}

ensure_workspace_document_loaded :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	uri: string,
) -> ^Document {
	if workspace == nil || len(uri) == 0 {
		return nil
	}

	if document, ok := workspace.documents[uri]; ok {
		return document
	}

	path := uri_to_path(uri, context.temp_allocator)
	data, err := os.read_entire_file_from_path(path, context.temp_allocator)
	if err != nil {
		return nil
	}

	refresh_document(cache, uri, string(data), 0)
	if document, ok := workspace.documents[uri]; ok {
		return document
	}

	return nil
}

unit_root_relative_path :: proc(unit: ^Semantic_Unit, allocator := context.allocator) -> string {
	if unit == nil {
		return strings.clone("", allocator)
	}

	if len(unit.root_file) > 0 {
		return normalize_manifest_path(unit.root_file, allocator)
	}

	for member in unit.members {
		#partial switch member.role {
		case .Root, .Main:
			if len(member.file) > 0 {
				return normalize_manifest_path(member.file, allocator)
			}
		}
	}

	if len(unit.members) > 0 && len(unit.members[0].file) > 0 {
		return normalize_manifest_path(unit.members[0].file, allocator)
	}

	return strings.clone("", allocator)
}

append_project_diagnostic :: proc(project: ^Project, message: string) {
	if project == nil {
		return
	}
	append(
		&project.diagnostics,
		symbols.Diagnostic {
			range   = lexer.TextRange {0, 0},
			message = strings.clone(message, context.allocator),
		},
	)
}

build_unit_member_uri_map :: proc(
	workspace: ^Workspace,
	unit: ^Semantic_Unit,
	allocator := context.allocator,
) -> map[string]string {
	member_uris := make(map[string]string, allocator)
	if workspace == nil || unit == nil {
		return member_uris
	}

	root_relative := unit_root_relative_path(unit, context.temp_allocator)
	if len(root_relative) > 0 {
		root_uri := workspace_uri_for_relative_path(workspace, root_relative, allocator)
		root_name := filename_from_uri(root_uri, context.temp_allocator)
		if len(root_name) > 0 {
			member_uris[root_name] = root_uri
		}
	}

	for member in unit.members {
		if len(member.file) == 0 {
			continue
		}

		member_uri := workspace_uri_for_relative_path(workspace, member.file, allocator)
		if len(member.object_name) > 0 {
			member_uris[strings.to_lower(member.object_name, context.temp_allocator)] = member_uri
		}

		filename := filename_from_uri(member_uri, context.temp_allocator)
		if len(filename) > 0 && filename not_in member_uris {
			member_uris[filename] = member_uri
		}
	}

	return member_uris
}

build_folder_include_uri_map :: proc(uri: string, allocator := context.allocator) -> map[string]string {
	include_uris := make(map[string]string, allocator)
	folder_path := folder_from_uri(uri, context.temp_allocator)
	files := list_abap_files(folder_path, context.temp_allocator)
	for file_path in files {
		file_uri := path_to_uri(file_path, allocator)
		filename := filename_from_uri(file_uri, context.temp_allocator)
		if len(filename) > 0 {
			include_uris[filename] = file_uri
		}
	}
	return include_uris
}

resolve_manifest_unit_file :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	file_uri: string,
	include_uris: map[string]string,
	table: ^symbols.SymbolTable,
	result: ^symbols.ProjectResolutionResult,
	project: ^Project,
	active_stack: ^[dynamic]string,
	allocator := context.allocator,
) {
	if len(file_uri) == 0 || table == nil || result == nil {
		return
	}

	if stack_contains_uri(active_stack^[:], file_uri) {
		append_project_diagnostic(project, strings.concatenate({"Cyclic INCLUDE detected for ", file_uri}, allocator))
		return
	}
	append(active_stack, strings.clone(file_uri, allocator))
	defer pop(active_stack)

	document := ensure_workspace_document_loaded(cache, workspace, file_uri)
	if document == nil || document.ast == nil {
		append_project_diagnostic(project, strings.concatenate({"Unable to load local file ", file_uri}, allocator))
		return
	}

	for decl in document.ast.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Include_Decl:
			if d.name != nil {
				include_name := strings.to_lower(d.name.name, context.temp_allocator)
				if include_uri, ok := include_uris[include_name]; ok {
					include_doc := ensure_workspace_document_loaded(cache, workspace, include_uri)
					if include_doc != nil && include_doc.ast != nil {
						include_table := symbols.clone_symbol_table(table, allocator)
						resolve_manifest_unit_file(
							cache,
							workspace,
							include_uri,
							include_uris,
							include_table,
							result,
							project,
							active_stack,
							allocator,
						)
						result.file_tables[include_uri] = include_table
						symbols.validate_file_with_lookup(include_doc.ast, table, include_table)
						symbols.merge_symbols_into(table, include_table)
					} else {
						append_project_diagnostic(
							project,
							strings.concatenate({"Local INCLUDE file is missing: ", include_name}, allocator),
						)
					}
				} else {
					append_project_diagnostic(
						project,
						strings.concatenate({"INCLUDE target not available locally: ", include_name}, allocator),
					)
				}

				symbols.resolve_include_decl(table, d)
			}
		case:
			symbols.resolve_decl_into(table, decl)
		}
	}
}

resolve_local_project :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	root_uri: string,
	include_uris: map[string]string,
	unit_name: string,
	allocator := context.allocator,
) -> ^Project {
	if workspace == nil || len(root_uri) == 0 {
		return nil
	}

	project := new(Project, allocator)
	project.root_uri = strings.clone(root_uri, allocator)
	project.diagnostics = make([dynamic]symbols.Diagnostic, allocator)
	project.unit_name = strings.clone(unit_name, allocator)

	root_document := ensure_workspace_document_loaded(cache, workspace, project.root_uri)
	if root_document == nil || root_document.ast == nil {
		append_project_diagnostic(project, strings.concatenate({"Unable to load root file ", root_uri}, allocator))
		return project
	}

	project.resolution_result = new(symbols.ProjectResolutionResult, allocator)
	project.resolution_result.file_tables = make(map[string]^symbols.SymbolTable, allocator)

	current_table := symbols.create_empty_symbol_table(allocator)
	active_stack := make([dynamic]string, context.temp_allocator)

	resolve_manifest_unit_file(
		cache,
		workspace,
		project.root_uri,
		include_uris,
		current_table,
		project.resolution_result,
		project,
		&active_stack,
		allocator,
	)

	project.resolution_result.file_tables[project.root_uri] = current_table
	project.resolution_result.merged_table = current_table
	symbols.validate_file(root_document.ast, current_table)

	return project
}

resolve_manifest_unit_project :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	unit: ^Semantic_Unit,
	allocator := context.allocator,
) -> ^Project {
	if workspace == nil || unit == nil {
		return nil
	}

	root_relative := unit_root_relative_path(unit, context.temp_allocator)
	if len(root_relative) == 0 {
		return nil
	}

	root_uri := workspace_uri_for_relative_path(workspace, root_relative, allocator)
	include_uris := build_unit_member_uri_map(workspace, unit, context.temp_allocator)
	return resolve_local_project(cache, workspace, root_uri, include_uris, unit.name, allocator)
}

get_projects_for_uri :: proc(
	cache: ^Cache,
	uri: string,
	allocator := context.allocator,
) -> []^Project {
	result := make([dynamic]^Project, allocator)
	workspace := workspace_for_uri(cache, uri)
	if workspace == nil {
		return result[:]
	}

	units := workspace_units_for_uri(workspace, uri, context.temp_allocator)
	for unit in units {
		project := resolve_manifest_unit_project(cache, workspace, unit, allocator)
		if project != nil {
			append(&result, project)
		}
	}

	if len(result) == 0 {
		include_uris := build_folder_include_uri_map(uri, context.temp_allocator)
		project := resolve_local_project(cache, workspace, uri, include_uris, filename_from_uri(uri), allocator)
		if project != nil {
			append(&result, project)
		}
	}

	return result[:]
}

get_file_symbol_table :: proc(project: ^Project, uri: string) -> ^symbols.SymbolTable {
	if project == nil || project.resolution_result == nil {
		return nil
	}

	if table, ok := project.resolution_result.file_tables[uri]; ok {
		return table
	}

	return project.resolution_result.merged_table
}

merge_symbol_tables_for_lookup :: proc(target: ^symbols.SymbolTable, source: ^symbols.SymbolTable) {
	if target == nil || source == nil {
		return
	}

	symbols.merge_symbols_into(target, source)
	for diag in source.diagnostics {
		append(&target.diagnostics, diag)
	}
}

stack_contains_uri :: proc(stack: []string, uri: string) -> bool {
	for entry in stack {
		if entry == uri {
			return true
		}
	}
	return false
}
