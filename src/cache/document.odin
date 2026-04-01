package cache

import "../lang/ast"
import "../lang/parser"
import "../lang/symbols"
import "base:intrinsics"
import "core:log"
import "core:strings"
import "core:sync"
import "core:time"

document_entry_init :: proc(workspace: ^Workspace, uri: string, path: string) -> ^Document_Entry {
	entry := new(Document_Entry)
	entry.workspace = workspace
	entry.uri = strings.clone(uri)
	entry.path = strings.clone(path)
	return entry
}

document_entry_deinit :: proc(entry: ^Document_Entry) {
	if entry.current != nil {
		release_snapshot(entry.current)
	}
	delete(entry.uri)
	delete(entry.path)
	free(entry)
}

document_entry_publish :: proc(entry: ^Document_Entry, text: string, version: int) {
	start := time.now()
	defer log.infof(
		"document_refresh took %.2fms for %s",
		time.duration_milliseconds(time.since(start)),
		entry.path,
	)

	snapshot := create_snapshot(entry, text, version)

	old_snapshot: ^Snapshot
	sync.rw_mutex_lock(&entry.lock)
	old_snapshot = entry.current
	entry.current = snapshot
	sync.rw_mutex_unlock(&entry.lock)
	if old_snapshot != nil {
		release_snapshot(old_snapshot)
	}
}

retain_snapshot :: proc(snapshot: ^Snapshot) {
	_ = intrinsics.atomic_add(&snapshot.ref_count, 1)
}

release_snapshot :: proc(snapshot: ^Snapshot) {
	old_count := intrinsics.atomic_sub(&snapshot.ref_count, 1)
	if old_count == 1 {
		arena_slot_release(snapshot.arena_slot)
	}
}

create_snapshot :: proc(entry: ^Document_Entry, text: string, version: int) -> ^Snapshot {
	slot := arena_slot_acquire(entry.workspace.doc_pool)
	context.allocator = slot.allocator

	snapshot := new(Snapshot)
	snapshot.ref_count = 1
	snapshot.arena_slot = slot
	snapshot.uri = strings.clone(entry.uri)
	snapshot.path = strings.clone(entry.path)
	snapshot.text = strings.clone(text)
	snapshot.version = version

	snapshot.ast = ast.new(ast.File, {})
	snapshot.ast.src = snapshot.text

	p: parser.Parser
	parser.parse_file(&p, snapshot.ast)
	snapshot.symbol_table = resolve_snapshot_symbols(snapshot)
	return snapshot
}

resolve_snapshot_symbols :: proc(snapshot: ^Snapshot) -> ^symbols.SymbolTable {
	table := symbols.create_empty_symbol_table(context.allocator)
	symbols.resolve_file_into(snapshot.ast, table)
	symbols.validate_file(snapshot.ast, table)
	return table
}
