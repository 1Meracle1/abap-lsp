package tests_cache

import "../../src/cache"
import "core:fmt"
import "core:strings"
import "core:testing"

@(test)
test_document_lookup_survives_freed_caller_uri :: proc(t: ^testing.T) {
	store := cache.cache_init()
	defer cache.cache_deinit(store)

	_ = cache.cache_add_workspace(
		store,
		"file:///d%3A/dev/abap/lsp_development_examples",
		"lsp_development_examples",
	)

	uri := strings.clone("file:///d%3A/dev/abap/lsp_development_examples/basic.abap")
	cache.refresh_document(store, uri, "DATA lv_value TYPE i.", 1)
	delete(uri)

	lookup_uri := strings.clone("file:///d%3A/dev/abap/lsp_development_examples/basic.abap")
	defer delete(lookup_uri)

	snap := cache.get_snapshot(store, lookup_uri)
	if !testing.expect(t, snap != nil, "expected snapshot lookup to succeed after caller uri is freed") do return
	defer cache.release_snapshot(snap)

	testing.expect(
		t,
		snap.uri == lookup_uri,
		fmt.tprintf("expected snapshot uri %q, got %q", lookup_uri, snap.uri),
	)
	testing.expect(
		t,
		len(snap.ast.decls) == 1,
		fmt.tprintf("expected parsed document to have 1 declaration, got %d", len(snap.ast.decls)),
	)
}
