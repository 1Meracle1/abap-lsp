#+private
package abap_frontend_remote_dependencies

import "src:ast"
import ddic_xml "src:ddic_xml"
import "src:parser"

import "core:mem"
import "core:strings"

result_add_artifact :: proc(
	result: ^Result,
	artifact: ^Artifact,
	state: ^State,
	allocator: mem.Allocator,
) -> bool {
	source, source_ok := interface_source(artifact, allocator)
	if !source_ok {
		result_add_diagnostic(
			result,
			artifact.request,
			artifact.source_kind,
			"dependency artifact cannot be converted to ABAP source",
		)
		return false
	}
	path := artifact_path(artifact, allocator)
	if !result_uri_add_if_missing(state, path) {
		return true
	}
	parse_policy := parser.Parse_Diagnostic_Policy.Include_Fragment if artifact_is_full_source(artifact) else .Strict
	parsed := parser.parse_with_diagnostic_policy(source, path, allocator, parse_policy)
	if parsed.root == nil {
		result_add_diagnostic(
			result,
			artifact.request,
			artifact.source_kind,
			"dependency artifact parse failed",
		)
		return false
	}
	for err in parsed.errors {
		result_add_diagnostic(
			result,
			artifact.request,
			artifact.source_kind,
			err.message,
		)
	}

	if artifact_is_full_source(artifact) {
		names := artifact_provided_names(artifact, allocator)
		append(
			&result.sources,
			Source_AST {
				key            = remote_dependency_key(artifact.request),
				path           = path,
				root           = parsed.root,
				provided_names = names,
				source_hash    = source_hash(source),
			},
		)
		return true
	}

	role := object_role(artifact.object_kind, artifact.object_type)
	interface_root := interface_root(parsed.root, role, allocator)
	key := interface_key(artifact, role, allocator)
	append(
		&result.interfaces,
		Interface_AST {
			key         = key,
			role        = role,
			path        = path,
			root        = interface_root,
			source_hash = source_hash(source),
		},
	)
	return true
}

interface_source :: proc(
	artifact: ^Artifact,
	allocator: mem.Allocator,
) -> (string, bool) {
	source_is_xml := source_is_xml(
	   artifact.object_kind,
	   artifact.file_extension,
	   artifact.source_text,
	   )
	if object_kind_is_ddic(artifact.object_kind) && !source_is_xml {
		return "", false
	}
	if source_is_xml {
		if object_kind_is_ddic(artifact.object_kind) || artifact.request.kind == .Type {
			source := ddic_xml.dependency_source(
				artifact.object_name if artifact.object_name != "" else artifact.request.name,
				artifact.object_kind,
				artifact.source_text,
				allocator,
			)
			return source, source != ""
		}
		return "", false
	}
	if strings.equal_fold(artifact.object_kind, TYPEPOOL_OBJECT_KIND) {
		return typepool_dependency_source(artifact.source_text, allocator), true
	}
	return strings.clone(artifact.source_text, allocator), artifact.source_text != ""
}

artifact_is_full_source :: proc(
	artifact: ^Artifact,
) -> bool {
	if artifact.request.kind == .Include {
		return true
	}
	if strings.equal_fold(artifact.object_kind, "include") {
		return true
	}
	return false
}

artifact_provided_names :: proc(
	artifact: ^Artifact,
	allocator: mem.Allocator,
) -> [dynamic]string {
	names := make([dynamic]string, 0, 2, allocator)
	append_provided_name(&names, artifact.request.name, allocator)
	append_provided_name(&names, artifact.object_name, allocator)
	return names
}

append_provided_name :: proc(
	names: ^[dynamic]string,
	name: string,
	allocator: mem.Allocator,
) {
	canonical := strings.to_lower(strings.trim_space(name), allocator)
	if canonical == "" {
		return
	}
	for existing in names^ {
		if existing == canonical {
			return
		}
	}
	append(names, canonical)
}

interface_key :: proc(
	artifact: ^Artifact,
	role: Remote_Dependency_Object_Role,
	allocator: mem.Allocator,
) -> Remote_Dependency_Key {
	name := artifact.object_name
	if name == "" {
		name = artifact.request.name
	}
	key := Remote_Dependency_Key {
		name = strings.to_lower(strings.trim_space(name), allocator),
		kind = role_request_kind(role),
	}
	return key
}

interface_root :: proc(
	root: ^ast.File,
	role: Remote_Dependency_Object_Role,
	allocator: mem.Allocator,
) -> ^ast.File {
	if root == nil {
		return nil
	}
	cloned := ast.clone_node(root, allocator).derived.(^ast.File)
	pruned := make([dynamic]^ast.Stmt, 0, len(cloned.stmts), allocator)
	for stmt in cloned.stmts {
		#partial switch n in stmt.derived_stmt {
		case ^ast.Class_Decl:
			if .Implementation in n.flags {
				continue
			}
			n.body = prune_oop_body(n.body, .Private, allocator)
			append(&pruned, stmt)
		case ^ast.Interface_Decl:
			n.body = prune_oop_body(n.body, .Public, allocator)
			append(&pruned, stmt)
		case ^ast.Function_Decl:
			n.body = make([dynamic]^ast.Stmt, 0, allocator)
			append(&pruned, stmt)
		case ^ast.Method_Decl,
		     ^ast.Form_Decl,
		     ^ast.Module_Decl,
		     ^ast.Event_Block_Stmt:
			continue
		case:
			if keep_interface_top_level_stmt(stmt, role) {
				append(&pruned, stmt)
			}
		}
	}
	cloned.stmts = pruned
	return cloned
}

prune_oop_body :: proc(
	body: [dynamic]^ast.Stmt,
	default_visibility: ast.Oop_Visibility,
	allocator: mem.Allocator,
) -> [dynamic]^ast.Stmt {
	out := make([dynamic]^ast.Stmt, 0, len(body), allocator)
	visibility := default_visibility
	for stmt in body {
		oop, ok := stmt.derived_stmt.(^ast.Oop_Simple_Stmt)
		if ok && oop.kind == .Class_Section {
			visibility = oop.visibility
			if visibility == .Public || visibility == .Protected {
				append(&out, stmt)
			}
			continue
		}
		if visibility == .Public || visibility == .Protected {
			append(&out, stmt)
		}
	}
	return out
}

keep_interface_top_level_stmt :: proc(
	stmt: ^ast.Stmt,
	role: Remote_Dependency_Object_Role,
) -> bool {
	if role == .DDIC_Type || role == .DDIC_Table || role == .Type_Pool {
		#partial switch _ in stmt.derived_stmt {
		case ^ast.Types_Decl,
		     ^ast.Constants_Decl,
		     ^ast.Type_Pools_Decl,
		     ^ast.Class_Decl,
		     ^ast.Interface_Decl:
			return true
		case:
			return false
		}
	}
	#partial switch _ in stmt.derived_stmt {
	case ^ast.Class_Decl,
	     ^ast.Interface_Decl,
	     ^ast.Function_Decl,
	     ^ast.Types_Decl,
	     ^ast.Constants_Decl:
		return true
	case:
		return false
	}
}

open_source_from_artifact :: proc(
	artifact: ^Artifact,
	allocator: mem.Allocator,
) -> Open_Source {
	source, ok := full_source(artifact, allocator)
	assert(ok)
	path := artifact_path(artifact, allocator)
	parsed := parser.parse_with_diagnostic_policy(
		source,
		path,
		allocator,
		.Include_Fragment if artifact_is_full_source(artifact) else .Strict,
	)
	return Open_Source {
		request        = clone_request(artifact.request, allocator),
		source_kind    = artifact.source_kind,
		object_kind    = strings.clone(artifact.object_kind, allocator),
		object_name    = strings.clone(artifact.object_name, allocator),
		object_uri     = strings.clone(artifact.object_uri, allocator),
		object_type    = strings.clone(artifact.object_type, allocator),
		file_extension = strings.clone(artifact.file_extension, allocator),
		path           = path,
		source_text    = source,
		root           = parsed.root,
		source_hash    = source_hash(source),
	}
}

full_source :: proc(
	artifact: ^Artifact,
	allocator: mem.Allocator,
) -> (string, bool) {
	source_is_xml := source_is_xml(
	   artifact.object_kind,
	   artifact.file_extension,
	   artifact.source_text,
	   )
	if object_kind_is_ddic(artifact.object_kind) && !source_is_xml {
		return "", false
	}
	if source_is_xml {
		if object_kind_is_ddic(artifact.object_kind) || artifact.request.kind == .Type {
			source := ddic_xml.dependency_source(
				artifact.object_name if artifact.object_name != "" else artifact.request.name,
				artifact.object_kind,
				artifact.source_text,
				allocator,
			)
			return source, source != ""
		}
		return "", false
	}
	if strings.equal_fold(artifact.object_kind, TYPEPOOL_OBJECT_KIND) {
		return typepool_dependency_source(artifact.source_text, allocator), true
	}
	return strings.clone(artifact.source_text, allocator), artifact.source_text != ""
}
