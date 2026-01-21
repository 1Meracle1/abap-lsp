package cache

import "../lang/ast"
import "../lang/symbols"
import "core:log"
import "core:os"
import "core:path/filepath"
import "core:strings"
import "core:time"

Root_Decl_Kind :: enum {
	None,
	Report,
	Function_Pool,
	Class_Pool,
}

// discover_project :: proc(
// 	c: ^Cache,
// 	folder_path: string,
// 	opened_uri: string,
// 	allocator := context.allocator,
// ) -> ^Project {
// 	start := time.now()
// 	defer log.infof("discover_project took %.2fms", time.duration_milliseconds(time.since(start)))

// 	project := new(Project, allocator)
// 	project.folder_path = strings.clone(folder_path, allocator)
// 	project.diagnostics = make([dynamic]symbols.Diagnostic, allocator)

// 	{
// 		snap := get_snapshot(c, opened_uri)
// 		defer release_snapshot(snap)
// 		if snap == nil {
// 			log.errorf("opened file doesn't have snapshot: %s", opened_uri)
// 			return nil
// 		}
// 		project.root_kind = check_root_decl(snap.ast)
// 		if project.root_kind != .None {
// 			extract_includes(project, snap.ast)
// 		} else {
// 			files := list_abap_files(folder_path, context.temp_allocator)
// 			log.infof("Found %d .abap files in %s", len(files), folder_path)

// 			for file_path in files {
// 				uri := path_to_uri(file_path, context.temp_allocator)
// 				if uri == opened_uri {
// 					continue
// 				}

// 				snap := get_snapshot(c, uri)
// 				defer release_snapshot(snap)
// 				if snap == nil {
// 					data, ok := os.read_entire_file(file_path, context.temp_allocator)
// 					if !ok {
// 						log.errorf("Failed to read file: %s", uri)
// 						continue
// 					}
// 					refresh_document(c, uri, string(data), 0)
// 					snap = get_snapshot(c, uri)
// 				}

// 				if project.root_kind == .None {
// 					if snap != nil {
// 						root_kind := check_root_decl(snap.ast)
// 						if root_kind != .None {
// 							project.root_uri = strings.clone(uri)
// 							extract_includes(project, snap.ast)

// 							opened_uri_in_includes := false
// 							for include_uri in project.include_uris {
// 								if include_uri == opened_uri {
// 									opened_uri_in_includes = true
// 									break
// 								}
// 							}

// 							if opened_uri_in_includes {
// 								project.root_kind = root_kind
// 								break
// 							} else {
// 								delete(project.include_uris)
// 							}
// 						}
// 					}
// 				}
// 			}
// 		}
// 	}

// 	if project.root_kind == .None {
// 		project.root_uri = opened_uri
// 		return project
// 	}

// 	for uri in project.include_uris {
// 		snap := get_snapshot(c, uri)
// 		defer release_snapshot(snap)
// 		if snap == nil {
// 			file_path := uri_to_path(uri, context.temp_allocator)
// 			data, ok := os.read_entire_file(file_path, context.temp_allocator)
// 			if !ok {
// 				log.warnf("Failed to read file: %s", uri)
// 				continue
// 			}
// 			refresh_document(c, uri, string(data), 0)
// 			snap = get_snapshot(c, uri)
// 		}
// 	}

// 	return project
// }

// check_root_decl :: proc(file_ast: ^ast.File) -> Root_Decl_Kind {
// 	if file_ast == nil {
// 		return .None
// 	}

// 	for decl in file_ast.decls {
// 		#partial switch d in decl.derived_stmt {
// 		case ^ast.Report_Decl:
// 			return .Report
// 		}
// 		// TODO: Add FUNCTION-POOL and CLASS-POOL detection when those AST nodes exist
// 	}

// 	return .None
// }

// extract_includes :: proc(project: ^Project, file_ast: ^ast.File, allocator := context.allocator) {
// 	if file_ast == nil {
// 		return
// 	}
// 	project.include_uris = make([dynamic]string, allocator)
// 	project.include_name_map = make(map[string]string, allocator)
// 	for decl in file_ast.decls {
// 		#partial switch d in decl.derived_stmt {
// 		case ^ast.Include_Decl:
// 			if d.name != nil {
// 				include_name := d.name.name
// 				include_uri, found := find_include_file(
// 					project.folder_path,
// 					include_name,
// 					allocator,
// 				)
// 				if found {
// 					append(&project.include_uris, include_uri)
// 					// Store mapping from lowercase include name to URI
// 					include_name_lower := strings.to_lower(include_name, allocator)
// 					project.include_name_map[include_name_lower] = include_uri
// 					log.infof("  Include '%s' -> %s", include_name, include_uri)
// 				} else {
// 					log.warnf("  Include '%s' not found in folder", include_name)
// 					append(
// 						&project.diagnostics,
// 						symbols.Diagnostic {
// 							range = d.range,
// 							message = strings.concatenate(
// 								{"Include file not found: ", include_name, ".abap"},
// 								allocator,
// 							),
// 						},
// 					)
// 				}
// 			}
// 		}
// 	}
// }

// // Find include file in folder (case-insensitive match)
// // Returns the URI of the matching file, or empty string if not found
// find_include_file :: proc(
// 	folder_path: string,
// 	include_name: string,
// 	allocator := context.allocator,
// ) -> (
// 	string,
// 	bool,
// ) {
// 	include_lower := strings.to_lower(include_name, context.temp_allocator)

// 	dir_handle, open_err := os.open(folder_path)
// 	if open_err != os.ERROR_NONE {
// 		return "", false
// 	}
// 	defer os.close(dir_handle)

// 	entries, read_err := os.read_dir(dir_handle, -1, context.temp_allocator)
// 	if read_err != os.ERROR_NONE {
// 		return "", false
// 	}

// 	for entry in entries {
// 		// Skip directories
// 		if os.is_dir(entry.fullpath) {
// 			continue
// 		}
// 		name_lower := strings.to_lower(entry.name, context.temp_allocator)
// 		if !strings.has_suffix(name_lower, ".abap") {
// 			continue
// 		}
// 		// Remove .abap extension for comparison
// 		name_without_ext := name_lower[:len(name_lower) - 5]
// 		if name_without_ext == include_lower {
// 			full_path := filepath.join({folder_path, entry.name}, allocator)
// 			uri := path_to_uri(full_path, allocator)
// 			return uri, true
// 		}
// 	}

// 	return "", false
// }

// // =========================================================================
// // Project Symbol Resolution
// // =========================================================================

// // Resolve all files in a project and build merged symbol table
// // This implements the "State Cloning" pattern for INCLUDE handling:
// // - Symbols defined before an INCLUDE are visible inside the include
// // - Symbols defined in an INCLUDE are visible after the INCLUDE in the root
// resolve_project :: proc(c: ^Cache, project: ^Project) {
// 	if project == nil {
// 		return
// 	}
// 	start := time.now()
// 	defer log.infof("resolve_project took %.2fms", time.duration_milliseconds(time.since(start)))

// 	root_snap := get_snapshot(c, project.root_uri)
// 	if root_snap == nil {
// 		log.errorf("Cannot resolve project: root file snapshot not found: %s", project.root_uri)
// 		return
// 	}
// 	defer release_snapshot(root_snap)

// 	resolution_result := new(symbols.ProjectResolutionResult)
// 	resolution_result.file_tables = make(map[string]^symbols.SymbolTable)

// 	current_table := symbols.create_empty_symbol_table()
// 	if root_snap.ast != nil {
// 		process_file_with_includes(
// 			c,
// 			project,
// 			root_snap.ast,
// 			project.root_uri,
// 			current_table,
// 			resolution_result,
// 		)
// 	}
// 	resolution_result.file_tables[project.root_uri] = current_table
// 	resolution_result.merged_table = current_table

// 	symbols.validate_file(root_snap.ast, current_table)

// 	if project.resolution_result != nil {
// 		symbols.destroy_project_resolution_result(project.resolution_result)
// 	}
// 	project.resolution_result = resolution_result
// 	project.needs_resolution = false
// }

// process_file_with_includes :: proc(
// 	c: ^Cache,
// 	project: ^Project,
// 	file: ^ast.File,
// 	file_uri: string,
// 	table: ^symbols.SymbolTable,
// 	result: ^symbols.ProjectResolutionResult,
// ) {
// 	if file == nil || table == nil {
// 		return
// 	}

// 	for decl in file.decls {
// 		#partial switch d in decl.derived_stmt {
// 		case ^ast.Include_Decl:
// 			// Process include: clone current state, resolve include, continue with result
// 			if d.name != nil {
// 				include_name := strings.to_lower(d.name.name, context.temp_allocator)

// 				// Get include URI from our map
// 				include_uri, has_uri := project.include_name_map[include_name]
// 				if has_uri {
// 					// Get include file snapshot
// 					include_snap := get_snapshot(c, include_uri)
// 					if include_snap != nil {
// 						defer release_snapshot(include_snap)

// 						// Clone current symbol table state for include file
// 						include_table := symbols.clone_symbol_table(table)

// 						// Resolve include file into the cloned table
// 						process_file_with_includes(
// 							c,
// 							project,
// 							include_snap.ast,
// 							include_uri,
// 							include_table,
// 							result,
// 						)

// 						// Store the include's resulting symbol table
// 						result.file_tables[include_uri] = include_table

// 						// Run validation on include file
// 						// Use the include's own table (which has accumulated symbols) for lookups and diagnostics
// 						symbols.validate_file(include_snap.ast, include_table)

// 						// CRITICAL: Merge include's symbols back into current table
// 						// This is what makes symbols from include visible in the rest of root
// 						symbols.merge_symbols_into(table, include_table)
// 					}
// 				}

// 				// Also add the include itself as a symbol
// 				symbols.resolve_include_decl(table, d)
// 			}
// 		case:
// 			// Process normal declaration
// 			symbols.resolve_decl_into(table, decl)
// 		}
// 	}
// }

// get_project_for_uri :: proc(c: ^Cache, uri: string) -> ^Project {
// 	folder := folder_from_uri(uri, context.temp_allocator)
// 	if project, ok := c.projects[folder]; ok {
// 		return project
// 	}
// 	return nil
// }

// ensure_project_context :: proc(c: ^Cache, uri: string) -> ^Project {
// 	folder := folder_from_uri(uri, context.temp_allocator)
// 	if project := get_project_for_uri(c, uri); project != nil {
// 		if project.needs_resolution {
// 			resolve_project(c, project)
// 		}
// 		return project
// 	}

// 	project := discover_project(c, folder, uri)
// 	if project != nil {
// 		folder_key := strings.clone(folder)
// 		c.projects[folder_key] = project
// 		resolve_project(c, project)
// 	}

// 	return project
// }

// // Mark project as dirty (needs re-resolution)
// invalidate_project :: proc(project: ^Project) {
// 	if project == nil {
// 		return
// 	}
// 	project.needs_resolution = true
// }

// // Get the effective symbol table for a URI within a project
// // Returns the file-specific symbol table if the URI is in the project,
// // otherwise returns the merged table
// get_file_symbol_table :: proc(project: ^Project, uri: string) -> ^symbols.SymbolTable {
// 	if project == nil || project.resolution_result == nil {
// 		return nil
// 	}

// 	// Check if this URI has a specific symbol table
// 	if table, ok := project.resolution_result.file_tables[uri]; ok {
// 		return table
// 	}

// 	// Fall back to merged table
// 	return project.resolution_result.merged_table
// }

// // Check if a URI is part of a project (root or include)
// is_uri_in_project :: proc(project: ^Project, uri: string) -> bool {
// 	if project == nil {
// 		return false
// 	}

// 	if project.root_uri == uri {
// 		return true
// 	}

// 	for include_uri in project.include_uris {
// 		if include_uri == uri {
// 			return true
// 		}
// 	}

// 	return false
// }

// // Destroy a project and free its resources
// destroy_project :: proc(project: ^Project) {
// 	if project == nil {
// 		return
// 	}

// 	// Clean up resolution result
// 	if project.resolution_result != nil {
// 		symbols.destroy_project_resolution_result(project.resolution_result)
// 	}

// 	delete(project.folder_path)
// 	delete(project.root_uri)
// 	delete(project.include_uris)
// 	delete(project.include_name_map)
// 	delete(project.diagnostics)
// 	free(project)
// }
