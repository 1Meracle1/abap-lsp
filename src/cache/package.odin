package cache

import "../lang/ast"
import "../lang/parser"
import "../lang/symbols"
import "core:log"
import "core:mem"
import "core:mem/virtual"
import "core:strings"
import "core:sync"
import "core:time"

Package :: struct {
	workspace:       ^Workspace,
	name:            string,
	path:            string,
	subpackages:     map[string]^Package,
	// Source Code Library
	includes:        [dynamic]^Document,
	reports:         [dynamic]^Report,
	func_groups:     [dynamic]^Function_Group,
	classes:         [dynamic]^Class,
	interfaces:      [dynamic]^Interface,
	// Dictionary
	database_tables: [dynamic]^Database_Table,
	data_elements:   [dynamic]^Data_Element,
	table_types:     [dynamic]^Table_Type,
	structures:      [dynamic]^Structure,
}

// package_new :: proc(name: string, path: string) -> ^Package {
// 	pkg := new(Package)
// 	pkg.name = name
// 	pkg.path = path

// 	return pkg
// }
