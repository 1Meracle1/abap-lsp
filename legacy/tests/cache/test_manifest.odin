package tests_cache

import "../../src/cache"
import "core:fmt"
import "core:testing"

@(test)
test_manifest_parse_without_trailing_newline :: proc(t: ^testing.T) {
	text := `version = 2
connection = "DEV"

[resolution]
dependency_mode = "remote-on-demand"
cache_dir = ".abapls/cache"

[[unit]]
name = "zcl_demo"
kind = "global-class"
root_file = "src/zcl_demo.clas.abap"`

	manifest := cache.manifest_parse(text, "manifest-no-newline")
	defer cache.manifest_deinit(manifest)

	if !testing.expect(t, manifest != nil, "expected manifest to parse") do return

	testing.expect(t, manifest.version == 2, fmt.tprintf("expected version 2, got %d", manifest.version))
	testing.expect(
		t,
		manifest.connection == "DEV",
		fmt.tprintf("expected connection DEV, got %q", manifest.connection),
	)
	testing.expect(t, len(manifest.units) == 1, fmt.tprintf("expected 1 unit, got %d", len(manifest.units)))

	if len(manifest.units) > 0 {
		unit := manifest.units[0]
		testing.expect(t, unit.name == "zcl_demo", fmt.tprintf("expected unit name, got %q", unit.name))
		testing.expect(t, unit.kind == .Global_Class, fmt.tprintf("expected global class, got %v", unit.kind))
		testing.expect(
			t,
			unit.root_file == "src/zcl_demo.clas.abap",
			fmt.tprintf("unexpected root file %q", unit.root_file),
		)
	}
}

@(test)
test_manifest_parse_members_and_comments :: proc(t: ^testing.T) {
	text := `version = 1 # comment

[[unit]]
name = "zfg_demo"
kind = "function-group"
root_file = "./src/saplzfg_demo.abap"

[[unit.member]]
role = "include"
file = "src/lzfg_demotop.abap"
object_name = "LZFG_DEMOTOP"

[[unit.member]]
role = "function-module"
file = "src/lzfg_demou01.abap"
object_name = "Z_FG_DEMO"` + "\n"

	manifest := cache.manifest_parse(text, "manifest-members")
	defer cache.manifest_deinit(manifest)

	if !testing.expect(t, manifest != nil, "expected manifest to parse") do return
	if !testing.expect(t, len(manifest.units) == 1, fmt.tprintf("expected 1 unit, got %d", len(manifest.units))) do return

	unit := manifest.units[0]
	testing.expect(t, len(unit.members) == 2, fmt.tprintf("expected 2 members, got %d", len(unit.members)))

	if len(unit.members) >= 2 {
		testing.expect(t, unit.members[0].role == .Include, fmt.tprintf("unexpected first role %v", unit.members[0].role))
		testing.expect(t, unit.members[0].object_name == "LZFG_DEMOTOP", "expected include object name")
		testing.expect(
			t,
			unit.members[1].role == .Function_Module,
			fmt.tprintf("unexpected second role %v", unit.members[1].role),
		)
		testing.expect(t, unit.members[1].object_name == "Z_FG_DEMO", "expected function module object name")
	}
}

@(test)
test_manifest_dependency_unit_detection :: proc(t: ^testing.T) {
	text := `version = 1
connection = "default"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "zcl_remote_demo"
kind = "global-class"
root_file = ".abapls/cache/dependencies/global-class/zcl_remote_demo.abap"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/global-class/zcl_remote_demo.abap"
object_name = "ZCL_REMOTE_DEMO"` + "\n"

	manifest := cache.manifest_parse(text, "manifest-dependency")
	defer cache.manifest_deinit(manifest)

	if !testing.expect(t, manifest != nil, "expected manifest to parse") do return
	if !testing.expect(t, len(manifest.units) == 1, fmt.tprintf("expected 1 unit, got %d", len(manifest.units))) do return

	unit := &manifest.units[0]
	testing.expect(t, cache.unit_is_dependency(unit), "expected dependency unit to be detected")
	testing.expect(
		t,
		unit.members[0].role == .Dependency,
		fmt.tprintf("expected dependency role, got %v", unit.members[0].role),
	)
}

@(test)
test_manifest_unknown_symbol_mode_defaults_and_parse :: proc(t: ^testing.T) {
	manifest := cache.manifest_init()
	defer cache.manifest_deinit(manifest)

	if !testing.expect(t, manifest != nil, "expected manifest") do return
	testing.expect(
		t,
		manifest.resolution.unknown_symbol_mode == cache.UNKNOWN_SYMBOL_MODE_REMOTE,
		fmt.tprintf(
			"expected default unknown symbol mode remote, got %q",
			manifest.resolution.unknown_symbol_mode,
		),
	)
	testing.expect(
		t,
		manifest.resolution.remote_request_parallelism == cache.DEFAULT_REMOTE_REQUEST_PARALLELISM,
		fmt.tprintf(
			"expected default remote request parallelism %d, got %d",
			cache.DEFAULT_REMOTE_REQUEST_PARALLELISM,
			manifest.resolution.remote_request_parallelism,
		),
	)
	testing.expect(
		t,
		manifest.resolution.remote_requests_per_second == cache.DEFAULT_REMOTE_REQUESTS_PER_SECOND,
		fmt.tprintf(
			"expected default remote requests per second %d, got %d",
			cache.DEFAULT_REMOTE_REQUESTS_PER_SECOND,
			manifest.resolution.remote_requests_per_second,
		),
	)

	parsed := cache.manifest_parse(
		`version = 1
connection = "default"

[resolution]
dependency_mode = "local-first"
unknown_symbol_mode = "log"
remote_request_parallelism = 6
remote_requests_per_second = 12` + "\n",
		"manifest-unknown-symbol-mode",
	)
	defer cache.manifest_deinit(parsed)

	if !testing.expect(t, parsed != nil, "expected parsed manifest") do return
	testing.expect(
		t,
		cache.normalize_dependency_mode(parsed.resolution.dependency_mode) == cache.DEPENDENCY_MODE_LOCAL_FIRST,
		fmt.tprintf(
			"expected parsed dependency mode local-first, got %q",
			parsed.resolution.dependency_mode,
		),
	)
	testing.expect(
		t,
		parsed.resolution.unknown_symbol_mode == "log",
		fmt.tprintf(
			"expected parsed unknown symbol mode log, got %q",
			parsed.resolution.unknown_symbol_mode,
		),
	)
	testing.expect(
		t,
		parsed.resolution.remote_request_parallelism == 6,
		fmt.tprintf(
			"expected parsed remote request parallelism 6, got %d",
			parsed.resolution.remote_request_parallelism,
		),
	)
	testing.expect(
		t,
		parsed.resolution.remote_requests_per_second == 12,
		fmt.tprintf(
			"expected parsed remote requests per second 12, got %d",
			parsed.resolution.remote_requests_per_second,
		),
	)
}
