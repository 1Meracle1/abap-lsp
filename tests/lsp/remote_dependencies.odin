package tests_lsp

import "../../src/cache"
import "../../src/lsp"
import "../../src/lang/symbols"
import "core:strings"
import "core:testing"

@(test)
test_remote_dependency_request_is_suppressed_by_syntax_errors :: proc(t: ^testing.T) {
	store := cache.cache_init()
	defer cache.cache_deinit(store)

	workspace := cache.cache_add_workspace(
		store,
		"file:///d%3A/dev/abap/lsp_syntax_gate",
		"lsp_syntax_gate",
	)
	if !testing.expect(t, workspace != nil, "expected workspace") do return

	if workspace.manifest == nil {
		workspace.manifest = cache.manifest_init()
	}
	delete(workspace.manifest.connection)
	workspace.manifest.connection = strings.clone("DEV")

	uri := "file:///d%3A/dev/abap/lsp_syntax_gate/main.abap"
	cache.refresh_document(
		store,
		uri,
		`DATA lv_before TYPE i.
MOVE-CORRESPONDING1 is_cusset TO ls_cusset.
DATA lv_after TYPE zcl_remote_demo.`,
		1,
	)

	snap := cache.get_snapshot(store, uri)
	if !testing.expect(t, snap != nil, "expected snapshot") do return
	defer cache.release_snapshot(snap)
	if !testing.expect(t, cache.snapshot_has_syntax_errors(snap), "expected snapshot syntax errors") do return

	project := new(cache.Project)
	project.root_uri = uri
	project.documents = make([dynamic]^cache.Snapshot)
	append(&project.documents, snap)
	project.remote_candidates = make([dynamic]symbols.Remote_Candidate)
	append(
		&project.remote_candidates,
		symbols.Remote_Candidate{name = "zcl_remote_demo", kind = .Type_Name},
	)

	projects := make([dynamic]^cache.Project)
	append(&projects, project)

	srv := lsp.Server{storage = store}
	lsp.maybe_request_remote_dependency_resolution(&srv, uri, projects[:])

	testing.expect(
		t,
		len(workspace.remote_resolution_seen) == 0,
		"expected syntax errors to suppress remote dependency requests",
	)
}
