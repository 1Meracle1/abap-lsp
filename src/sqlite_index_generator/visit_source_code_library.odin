package main

import "persistence/sqlite3"

import "core:log"
import os "core:os/os2"
import "core:path/filepath"

visit_source_code_library :: proc(
	conn: sqlite3.Conn,
	path: string,
	package_name: string,
	parent_package_id: i64,
) {
	contents, err := os.read_all_directory_by_path(path, context.allocator)
	if err != nil {
		log.errorf(
			"failed to read source code library directory for package %s, error: %v\n",
			package_name,
			err,
		)
		return
	}
	for item in contents {
		path := filepath.join([]string{path, item.name})
		switch item.name {
		case "Includes":
			visit_includes_folder(conn, path, parent_package_id)
		case "Programs":
			visit_programs_folder(conn, path, package_name, parent_package_id)
		case "Function Groups":
			visit_function_groups_folder(conn, path, package_name, parent_package_id)
		case "Classes":
		case "Interfaces":
		}
	}
}

visit_includes_folder :: proc(conn: sqlite3.Conn, path: string, parent_obj_id: i64) {
	contents, err := os.read_all_directory_by_path(path, context.allocator)
	if err != nil {
		log.errorf(
			"failed to read includes directory for object id %d, error: %v\n",
			parent_obj_id,
			err,
		)
		return
	}
	for item in contents {
		if item.type == .Regular {
			file_id := persist_file(conn, path, item.name)
			persist_dev_object(conn, item.name, .Include, parent_obj_id, file_id)
		}
	}
}

visit_programs_folder :: proc(
	conn: sqlite3.Conn,
	path: string,
	package_name: string,
	parent_package_id: i64,
) {
	contents, err := os.read_all_directory_by_path(path, context.allocator)
	if err != nil {
		log.errorf(
			"failed to read programs directory for package %s, error: %v\n",
			package_name,
			err,
		)
		return
	}
	for item in contents {
		if item.type == .Directory {
			visit_report(conn, path, item.name, package_name, parent_package_id)
		}
	}
}

visit_report :: proc(
	conn: sqlite3.Conn,
	base_path: string,
	report_name: string,
	package_name: string,
	parent_package_id: i64,
) {
	path := filepath.join([]string{base_path, report_name})
	contents, err := os.read_all_directory_by_path(path, context.allocator)
	if err != nil {
		log.errorf(
			"failed to read source code library directory for package %s, error: %v\n",
			package_name,
			err,
		)
		return
	}

	root_file_id: i64 = -1
	for item in contents {
		if item.type == .Regular {
			if root_file_id != -1 {
				log.errorf("not expected multiple root files for the same report, path %s", path)
			}
			root_file_id = persist_file(conn, path, item.name)
		}
	}
	if root_file_id == -1 {
		return
	}
	report_id := persist_dev_object(conn, report_name, .Report, parent_package_id, root_file_id)

	for item in contents {
		if item.type == .Directory {
			if item.name == "Includes" {
				path := filepath.join([]string{path, item.name})
				visit_includes_folder(conn, path, report_id)
			}
			continue
		}
	}
}

visit_function_groups_folder :: proc(
	conn: sqlite3.Conn,
	path: string,
	package_name: string,
	parent_package_id: i64,
) {
	contents, err := os.read_all_directory_by_path(path, context.allocator)
	if err != nil {
		log.errorf(
			"failed to read function groups directory for package %s, error: %v\n",
			package_name,
			err,
		)
		return
	}
	for item in contents {
		if item.type == .Directory {
			visit_function_group(conn, path, item.name, package_name, parent_package_id)
		}
	}
}

visit_function_group :: proc(
	conn: sqlite3.Conn,
	base_path: string,
	func_grp_name: string,
	package_name: string,
	parent_package_id: i64,
) {
	path := filepath.join([]string{base_path, func_grp_name})
	contents, err := os.read_all_directory_by_path(path, context.allocator)
	if err != nil {
		log.errorf(
			"failed to read source code library directory for package %s, error: %v\n",
			package_name,
			err,
		)
		return
	}

	root_file_id: i64 = -1
	for item in contents {
		if item.type == .Regular {
			if root_file_id != -1 {
				log.errorf(
					"not expected multiple root files for the same function group, path %s",
					path,
				)
			}
			root_file_id = persist_file(conn, path, item.name)
		}
	}
	if root_file_id == -1 {
		return
	}
	func_grp_id := persist_dev_object(
		conn,
		func_grp_name,
		.Function_Group,
		parent_package_id,
		root_file_id,
	)

	for item in contents {
		if item.type == .Directory {
			path := filepath.join([]string{path, item.name})
			if item.name == "Includes" {
				visit_includes_folder(conn, path, func_grp_id)
			} else if item.name == "Function Modules" {
				visit_function_modules_folder(conn, path, func_grp_id)
			}
		}
	}
}

visit_function_modules_folder :: proc(conn: sqlite3.Conn, path: string, parent_obj_id: i64) {
	contents, err := os.read_all_directory_by_path(path, context.allocator)
	if err != nil {
		log.errorf(
			"failed to read function modules directory for object id %d, error: %v\n",
			parent_obj_id,
			err,
		)
		return
	}
	for item in contents {
		if item.type == .Regular {
			file_id := persist_file(conn, path, item.name)
			persist_dev_object(conn, item.name, .Function_Module, parent_obj_id, file_id)
		}
	}
}
