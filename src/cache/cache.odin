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

Workspace :: struct {
	uri:       string,
	name:      string,
	packages:  map[string]^Package,
	documents: map[string]^Document,
}

init :: proc(uri: string, name: string) -> ^Workspace {
	workspace := new(Workspace)
	workspace.uri = uri
	workspace.name = name
	workspace.packages = make(map[string]^Package)
	return workspace
}

deinit :: proc(workspace: ^Workspace) {
	delete(workspace.packages)
}

update_package_for_document :: proc(workspace: ^Workspace, document: ^Document) -> ^Package {
	return nil
}