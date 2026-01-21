package main

import "persistence/sqlite3"

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"
import os "core:os/os2"
import "core:path/filepath"
import "core:strings"

SAP_PACKAGES_PATH :: "D:\\dev\\abap\\sap_system_export"

arena: virtual.Arena

main :: proc() {
	err := virtual.arena_init_static(&arena, 16 * mem.Gigabyte)
	if err != nil {
		fmt.eprintf("failed to init arena: %v\n", err)
		return
	}
	context.allocator = virtual.arena_allocator(&arena)

	context.logger = log.create_console_logger()
	defer log.destroy_console_logger(context.logger)

	conn: sqlite3.Conn
	conn_str_c := strings.clone_to_cstring(SAP_PACKAGES_PATH + "\\index.sqlite")
	if rc := sqlite3.open(conn_str_c, &conn); rc != sqlite3.OK {
		log.fatal("failed to open sqlite3 db connection: %v, %s", rc, sqlite3.errmsg(conn))
	}
	defer sqlite3.close(conn)
	log.info("connection to sqlite3 db is established")

	visit_package(
		conn,
		SAP_PACKAGES_PATH +
		"\\%2FSTTP%2FMAIN\\Package\\%2FSTTP%2FRULES\\Package\\%2FSTTP%2FREPORTING\\Package",
		"%2FSTTP%2FREP_GOV_EU",
	)
	// free_all(context.temp_allocator)
}

visit_package :: proc(
	conn: sqlite3.Conn,
	base_path: string,
	package_name: string,
	parent_package_id: i64 = -1,
) {
	log.infof("visited package %s", package_name)
	arena_temp := virtual.arena_temp_begin(&arena)
	defer virtual.arena_temp_end(arena_temp)

	path := filepath.join([]string{base_path, package_name})
	contents, err := os.read_all_directory_by_path(path, context.allocator)
	if err != nil {
		log.errorf("failed to read directory for package %s, error: %v\n", package_name, err)
		return
	}
	package_id := persist_dev_object(
		conn,
		package_name,
		.Package,
		parent_id = parent_package_id,
		file_id = -1,
	)

	for item in contents {
		#partial switch (item.type) {
		case .Directory:
			switch item.name {
			case "Package":
				subpackage_path := filepath.join([]string{path, item.name})
				subpackage_contents, err := os.read_all_directory_by_path(
					subpackage_path,
					context.allocator,
				)
				if err != nil {
					log.errorf(
						"failed to read subpackage directory for package %s, error: %v\n",
						package_name,
						err,
					)
					return
				}
				for subpackage in subpackage_contents {
					visit_package(conn, subpackage_path, subpackage.name, package_id)
				}
			case "Source Code Library":
				scl_path := filepath.join([]string{path, item.name})
				visit_source_code_library(conn, scl_path, package_name, package_id)
			}
		}
	}
}

visit_dictionary :: proc(base_path: string, parent_package_id: i64) {

}

visit_table_type :: proc(path: string, parent_package_id: i64) {

}
