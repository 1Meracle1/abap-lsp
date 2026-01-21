package main

import "persistence"
import "persistence/sqlite3"

import "core:fmt"
import "core:log"
import os "core:os/os2"
import "core:path/filepath"
import "core:strings"

Dev_Object_Kind :: enum {
	Package,
	Report,
	Include,
	Function_Group,
	Function_Module,
	Class,
	Interface,
	Ddic_Data_Element,
	Ddic_Structure,
	Ddic_Table_Type,
	Ddic_Database_Table,
}

persist_dev_object :: proc(
	conn: sqlite3.Conn,
	url_encoded_name: string,
	kind: Dev_Object_Kind,
	parent_id: i64,
	file_id: i64,
) -> i64 {
	name, name_ok, name_err := path_unescape(url_encoded_name)
	if !name_ok {
		log.fatalf("failed to get name for '%s', error: %s", url_encoded_name, name_err)
	}

	kind_cstr: cstring
	switch kind {
	case .Package:
		kind_cstr = "package"
	case .Report:
		kind_cstr = "report"
	case .Include:
		kind_cstr = "include"
	case .Function_Group:
		kind_cstr = "function_group"
	case .Function_Module:
		kind_cstr = "function_module"
	case .Class:
		kind_cstr = "class"
	case .Interface:
		kind_cstr = "interface"
	case .Ddic_Data_Element:
		kind_cstr = "ddic_data_element"
	case .Ddic_Structure:
		kind_cstr = "ddic_structure"
	case .Ddic_Table_Type:
		kind_cstr = "ddic_table_type"
	case .Ddic_Database_Table:
		kind_cstr = "ddic_database_table"
	}

	params := []persistence.ParamValue {
		strings.clone_to_cstring(name),
		strings.clone_to_cstring(url_encoded_name),
		kind_cstr,
		fmt.ctprintf("%d", file_id),
	}
	query_result, ok, err_descr := persistence.exec_with_params(
		context.allocator,
		conn,
		`insert into dev_object (name, url_encoded_name, kind, file_id)
		values (?, ?, ?, ?) RETURNING id;`,
		params[:],
	)
	if !ok {
		log.fatalf("failed to persist dev object '%s', error: %v", name, err_descr)
	}
	defer persistence.query_result_deinit(&query_result)

	id: i64 = -1
	if len(query_result.rows) >= 1 {
		maybe_field := persistence.row_field_by_name(query_result.rows[0], "id")
		if field, ok := maybe_field.(persistence.Field); ok {
			value, value_ok := persistence.field_to_int(field)
			if value_ok {
				id = value
			}
		}
	}

	if id != -1 {
		persist_dev_object_children(conn, parent_id, []i64{id})
		log.infof("persisted dev object '%s'", name)
	} else {
		log.errorf("failed to persist dev object '%s', no id retrieved", name)
	}
	return id
}

persist_dev_object_children :: proc(conn: sqlite3.Conn, parent_id: i64, children: []i64) {
	if parent_id == -1 || len(children) == 0 {
		return
	}
    
    query_result, ok, err_descr := persistence.exec(context.allocator, conn, `BEGIN`)
	if !ok {
		log.fatalf(
			"failed to begin transaction to persist children for dev object '%d', error: %v",
			parent_id,
			err_descr,
		)
	}
	persistence.query_result_deinit(&query_result)

	for child in children {
		params := []persistence.ParamValue {
			fmt.ctprintf("%d", parent_id),
			fmt.ctprintf("%d", child),
		}
		query_result, ok, err_descr := persistence.exec_with_params(
			context.allocator,
			conn,
			`insert into dev_object_to_children (parent_id, child_id)
			values (?, ?);`,
			params[:],
		)
		if !ok {
			persistence.exec(context.allocator, conn, `ROLLBACK`)
			log.fatalf(
				"failed to persist child %d for dev object '%d', error: %v",
				parent_id,
				err_descr,
			)
		}
		persistence.query_result_deinit(&query_result)
	}
	query_result, ok, err_descr = persistence.exec(context.allocator, conn, `COMMIT`)
	if !ok {
		log.fatalf(
			"failed to commit persist children for dev object '%d', error: %v",
			parent_id,
			err_descr,
		)
	}
	persistence.query_result_deinit(&query_result)
}

persist_file :: proc(conn: sqlite3.Conn, base_path: string, name: string) -> i64 {
	path := filepath.join({base_path, name})
	data, err := os.read_entire_file_from_path(path, context.allocator)
	if err != nil {
		log.errorf("failed to read file %s to persist its content\n", path, err)
		return -1
	}

	params := []persistence.ParamValue {
		strings.clone_to_cstring(path, context.allocator),
		strings.clone_to_cstring(name, context.allocator),
		data,
	}
	query_result, ok, err_descr := persistence.exec_with_params(
		context.allocator,
		conn,
		`insert into src_file (path, name, content)
		values (?, ?, ?);`,
		params[:],
	)
	if !ok {
		log.fatalf("failed to persist file %s, error: %v", path, err_descr)
	}
	defer persistence.query_result_deinit(&query_result)

	if len(query_result.rows) >= 1 {
		maybe_field := persistence.row_field_by_name(query_result.rows[0], "id")
		if field, ok := maybe_field.(persistence.Field); ok {
			value, value_ok := persistence.field_to_int(field)
			if value_ok {
				return value
			}
		}
	}
	return -1
}
