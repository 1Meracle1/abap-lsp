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

Snapshot :: struct {
	arena:        virtual.Arena,
	allocator:    mem.Allocator,
	text:         string,
	version:      int,
	ast:          ^ast.File,
	symbol_table: ^symbols.SymbolTable,
	refs:         i32,
}

Document :: struct {
	uri:         string,
	snapshot_mu: sync.Mutex,
	snapshot:    ^Snapshot,
}

Cache :: struct {
	documents:    map[string]^Document,
	documents_mu: sync.Recursive_Mutex,
}

init :: proc() -> ^Cache {
	c := new(Cache)
	c.documents = make(map[string]^Document)
	return c
}

destroy :: proc(c: ^Cache) {
	for _, doc in c.documents {
		sync.mutex_lock(&doc.snapshot_mu)
		release_snapshot(doc.snapshot)
		sync.mutex_unlock(&doc.snapshot_mu)
		delete(doc.uri)
		free(doc)
	}
	delete(c.documents)
	free(c)
}

refresh_document :: proc(c: ^Cache, uri: string, text: string, version: int) {
	start := time.now()
	defer log.infof("refresh_document took %.2fms", time.duration_milliseconds(time.since(start)))

	sync.recursive_mutex_lock(&c.documents_mu)
	document, exists := c.documents[uri]
	if !exists {
		document = new(Document)
		document.uri = strings.clone(uri) // Persistent URI
		c.documents[document.uri] = document
	}
	sync.recursive_mutex_unlock(&c.documents_mu)

	new_snap := create_snapshot()
	new_snap.version = version
	new_snap.text = strings.clone(text, new_snap.allocator)
	parse_snapshot(new_snap, uri)

	sync.mutex_lock(&document.snapshot_mu)
	old_snap := document.snapshot
	document.snapshot = new_snap
	sync.mutex_unlock(&document.snapshot_mu)
	release_snapshot(old_snap)
}

get_snapshot :: proc(c: ^Cache, uri: string) -> ^Snapshot {
	sync.recursive_mutex_lock(&c.documents_mu)
	document, ok := c.documents[uri]
	sync.recursive_mutex_unlock(&c.documents_mu)
	if !ok {
		return nil
	}

	sync.mutex_lock(&document.snapshot_mu)
	snap := document.snapshot
	retain_snapshot(snap)
	sync.mutex_unlock(&document.snapshot_mu)
	return snap
}

create_snapshot :: proc() -> ^Snapshot {
	snapshot := new(Snapshot)
	snapshot.refs = 1
	_ = virtual.arena_init_growing(&snapshot.arena)
	snapshot.allocator = virtual.arena_allocator(&snapshot.arena)
	return snapshot
}

retain_snapshot :: proc(snapshot: ^Snapshot) {
	if snapshot != nil {
		sync.atomic_add(&snapshot.refs, 1)
	}
}

release_snapshot :: proc(snapshot: ^Snapshot) {
	if snapshot != nil {
		val := sync.atomic_sub(&snapshot.refs, 1)
		if val == 0 {
			virtual.arena_destroy(&snapshot.arena)
			free(snapshot)
		}
	}
}

parse_snapshot :: proc(snapshot: ^Snapshot, uri: string) {
	start := time.now()
	defer log.infof("parse_snapshot took %.2fms", time.duration_milliseconds(time.since(start)))

	context.allocator = snapshot.allocator

	file_ast := ast.new(ast.File, {})
	file_ast.fullpath = strings.clone(uri, snapshot.allocator)
	file_ast.src = snapshot.text

	p: parser.Parser
	parser.parse_file(&p, file_ast)
	snapshot.ast = file_ast
	snapshot.symbol_table = symbols.resolve_file(file_ast)
}
