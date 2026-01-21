package cache

import "../lang/ast"
import "../lang/parser"
import "../lang/symbols"
import "core:log"
import "core:mem"
import "core:mem/virtual"
import "core:strings"
import "core:time"

Document :: struct {
	uri:          string,
	path:         string,
	arena:        virtual.Arena,
	allocator:    mem.Allocator,
	text:         string,
	version:      int,
	ast:          ^ast.File,
	symbol_table: ^symbols.SymbolTable,
	workspace:    ^Workspace,
	pkg:          ^Package,
}

document_init :: proc(
	workspace: ^Workspace,
	uri: string,
	path: string,
	text: string,
	version: int,
) -> ^Document {
	document := new(Document)
	document.workspace = workspace
	document.uri = strings.clone(uri)
	document.path = strings.clone(path)

	_ = virtual.arena_init_growing(&document.arena)
	document.allocator = virtual.arena_allocator(&document.arena)

	document_refresh(document, text, version)

    if existing_document, exists := workspace.documents[uri]; exists {
        document_deinit(existing_document)
    }
	workspace.documents[uri] = document

	return document
}

document_deinit :: proc(document: ^Document) {
	virtual.arena_destroy(&document.arena)
	free(&document.uri)
	free(&document.path)
    free(document)
}

document_refresh :: proc(document: ^Document, text: string, version: int) {
	start := time.now()
	defer log.infof(
		"document_refresh took %.2fms for %s",
		time.duration_milliseconds(time.since(start)),
		document.path,
	)

	virtual.arena_free_all(&document.arena)
	context.allocator = document.allocator

	document.text = strings.clone(text)
	document.version = version

	document.ast = ast.new(ast.File, {})
	document.ast.src = text
	p: parser.Parser
	parser.parse_file(&p, document.ast)
	document.symbol_table = nil
}

document_resolve_package_context :: proc(document: ^Document) {

}
