#+private
package abap_frontend_workspace

import dep_store "../dependency_store"
import toml "../encoding/toml"

import "core:mem"
import "core:os"
import "core:strings"

decode_dependency_profile :: proc(
	table: toml.Table,
	allocator: mem.Allocator,
) -> (dep_store.Dependency_Profile, bool) {
	product, product_ok := toml.table_get_string(table, "product_version")
	default_version, default_ok := toml.table_get_string(table, "default_package_version")
	product = strings.trim_space(product)
	default_version = strings.trim_space(default_version)
	if !product_ok || !default_ok || product == "" || default_version == "" {
		return {}, false
	}
	packages := make([dynamic]dep_store.Package_Version, 0, 4, allocator)
	if package_table, packages_ok := toml.table_get_table(table, "packages"); packages_ok {
		for package_name, value in package_table.entries {
			#partial switch version in value {
			case toml.String:
				name := strings.trim_space(package_name)
				trimmed_version := strings.trim_space(version)
				if name != "" && trimmed_version != "" {
					append(
						&packages,
						dep_store.Package_Version {
							package_name = strings.clone(name, allocator),
							version      = strings.clone(trimmed_version, allocator),
						},
					)
				}
			}
		}
	}
	return dep_store.Dependency_Profile {
		product_version         = strings.clone(product, allocator),
		default_package_version = strings.clone(default_version, allocator),
		packages                = packages[:],
	}, true
}

decode_manifest_member :: proc(value: toml.Value, allocator: mem.Allocator) -> (Manifest_Unit_Member, bool) {
	#partial switch v in value {
	case toml.String:
		return Manifest_Unit_Member{file = normalize_manifest_path(v, allocator)}, true
	case toml.Table:
		file, ok := toml.table_get_string(v, "file")
		if !ok {
			return {}, false
		}
		member := Manifest_Unit_Member{file = normalize_manifest_path(file, allocator)}
		if role, role_ok := toml.table_get_string(v, "role"); role_ok {
			member.role = strings.clone(role, allocator)
		}
		if object_name, object_ok := toml.table_get_string(v, "object_name"); object_ok {
			member.object_name = strings.clone(object_name, allocator)
		}
		return member, true
	}
	return {}, false
}

decode_manifest_dependency :: proc(
	value: toml.Value,
	allocator: mem.Allocator,
) -> (Manifest_Unit_Dependency, bool) {
	#partial switch v in value {
	case toml.String:
		return Manifest_Unit_Dependency{file = normalize_manifest_path(v, allocator)}, true
	case toml.Table:
		file, ok := toml.table_get_string(v, "file")
		if !ok {
			return {}, false
		}
		return Manifest_Unit_Dependency{file = normalize_manifest_path(file, allocator)}, true
	}
	return {}, false
}

manifest_unit_root_key :: proc(
	manifest: ^Workspace_Manifest,
	unit_index: int,
	allocator: mem.Allocator,
) -> string {
	path, ok := manifest_absolute_path(manifest.root_path, manifest.units[unit_index].root_file, allocator)
	return normalized_uri_path_key(path, allocator) if ok else ""
}

join_path2 :: proc(a, b: string, allocator: mem.Allocator) -> (string, bool) {
	path, err := os.join_path({a, b}, allocator)
	return path, err == nil
}

normalize_manifest_path :: proc(path: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_space(path)
	for strings.has_prefix(trimmed, "./") || strings.has_prefix(trimmed, ".\\") {
		trimmed = trimmed[2:]
	}
	out := strings.builder_make(allocator)
	for ch in trimmed {
		if ch == '\\' {
			strings.write_byte(&out, '/')
		} else {
			strings.write_rune(&out, ch)
		}
	}
	return strings.to_string(out)
}

is_abap_path :: proc(path: string, allocator: mem.Allocator) -> bool {
	lower := strings.to_lower(path, allocator)
	return strings.has_suffix(lower, ".abap")
}

should_skip_workspace_dir :: proc(name: string) -> bool {
	return name == "target" || name == ".git" || (len(name) > 0 && name[0] == '.')
}
