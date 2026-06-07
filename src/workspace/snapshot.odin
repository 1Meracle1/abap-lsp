package abap_frontend_workspace

import execution "src:execution"
import "src:parser"
import remote_deps "src:remote_dependencies"
import "src:semantic"

import "core:mem"
import "core:strings"
import "core:sync"

Project_Id :: distinct u32

Dependency_Object_Kind :: enum {
	Unknown,
	Report,
	Include,
	Class,
	Interface,
	Function_Group,
	Function_Module,
	Type_Pool,
	Message_Class,
	DDIC_Data_Element,
	DDIC_Structure,
	DDIC_Table,
	DDIC_Table_Type,
	DDIC_View,
}

Dependency_Object_Key :: struct {
	product_version: string,
	package_name:    string,
	package_version: string,
	object_kind:     Dependency_Object_Kind,
	object_name:     string,
}

Workspace_Source_Input :: struct {
	uri:         string,
	text:        string,
	revision:    u64,
	open:        bool,
	kind:        semantic.Workspace_File_Kind,
	object_name: string,
}

Workspace_Opened_Dependency :: struct {
	key:   Dependency_Object_Key,
	input: Workspace_Source_Input,
}

Project_Snapshot :: struct {
	id:                  Project_Id,
	root_key:            Dependency_Object_Key,
	root_uri:            string,
	revision:            u64,
	source_inputs:       [dynamic]Workspace_Source_Input,
	opened_dependencies: [dynamic]Workspace_Opened_Dependency,
	session:             semantic.Semantic_Graph_Session,
	last_update:         semantic.Semantic_Graph_Update_Result,
}

Project_Slot :: struct {
	id:               Project_Id,
	root_key:         Dependency_Object_Key,
	root_uri:         string,
	current:          ^Project_Snapshot,
	pending_revision: u64,
	running_revision: u64,
	retired:          [dynamic]^Project_Snapshot,
}

Project_Builder :: struct {
	workspace: ^Workspace,
	slot:      ^Project_Slot,
	base:      ^Project_Snapshot,
	next:      ^Project_Snapshot,
	revision:  u64,
	inputs:    [dynamic]Workspace_Source_Input,
	opened:   [dynamic]Workspace_Opened_Dependency,
	options:   Options,
}

workspace_add_project :: proc(
	workspace: ^Workspace,
	root_key: Dependency_Object_Key,
	root_uri: string,
	allocator: mem.Allocator,
) -> (Project_Id, bool) {
	if workspace == nil {
		return Project_Id(0), false
	}
	workspace_snapshot_state_ensure(workspace, allocator)
	id := Project_Id(u32(len(workspace.projects)))
	slot := new(Project_Slot, allocator)
	slot^ = Project_Slot {
		id       = id,
		root_key = dependency_object_key_clone(root_key, allocator),
		root_uri = strings.clone(root_uri, allocator),
		retired  = make([dynamic]^Project_Snapshot, 0, 2, allocator),
	}
	append(&workspace.projects, slot)
	workspace_map_object_project(workspace, root_key, id, allocator)
	return id, true
}

workspace_map_object_project :: proc(
	workspace: ^Workspace,
	key: Dependency_Object_Key,
	project_id: Project_Id,
	allocator: mem.Allocator,
) {
	if workspace == nil || !workspace_project_id_valid(workspace, project_id) {
		return
	}
	workspace_snapshot_state_ensure(workspace, allocator)
	if projects, ok := workspace.projects_by_object[key]; ok {
		if !project_id_list_contains(projects[:], project_id) {
			append(&projects, project_id)
			workspace.projects_by_object[key] = projects
		}
		return
	}
	owned_key := dependency_object_key_clone(key, allocator)
	projects := make([dynamic]Project_Id, 0, 2, allocator)
	append(&projects, project_id)
	workspace.projects_by_object[owned_key] = projects
}

workspace_projects_for_object :: proc(
	workspace: ^Workspace,
	key: Dependency_Object_Key,
	allocator: mem.Allocator,
) -> [dynamic]Project_Id {
	out := make([dynamic]Project_Id, 0, 2, allocator)
	if workspace == nil || workspace.projects_by_object == nil {
		return out
	}
	if projects, ok := workspace.projects_by_object[key]; ok {
		for id in projects {
			append(&out, id)
		}
	}
	return out
}

workspace_current_project_snapshot :: proc(
	workspace: ^Workspace,
	project_id: Project_Id,
) -> (^Project_Snapshot, bool) {
	slot := workspace_project_slot(workspace, project_id)
	if slot == nil {
		return nil, false
	}
	current := sync.atomic_load_explicit(&slot.current, .Acquire)
	return current, current != nil
}

workspace_project_snapshot_builder :: proc(
	workspace: ^Workspace,
	project_id: Project_Id,
	inputs: []Workspace_Source_Input,
	options: Options,
	allocator: mem.Allocator,
) -> (Project_Builder, bool, string) {
	slot := workspace_project_slot(workspace, project_id)
	if slot == nil {
		return {}, false, "invalid project id"
	}
	owned_inputs := make([dynamic]Workspace_Source_Input, 0, len(inputs), allocator)
	for input in inputs {
		append(&owned_inputs, workspace_source_input_clone(input, allocator))
	}
	base := sync.atomic_load_explicit(&slot.current, .Acquire)
	opened := make([dynamic]Workspace_Opened_Dependency, 0, 2, allocator)
	if base != nil {
		for entry in base.opened_dependencies {
			append(&opened, workspace_opened_dependency_clone(entry, allocator))
		}
	}
	revision := slot.pending_revision + 1
	if base != nil && revision <= base.revision {
		revision = base.revision + 1
	}
	slot.pending_revision = revision
	return Project_Builder {
		workspace = workspace,
		slot      = slot,
		base      = base,
		revision  = revision,
		inputs    = owned_inputs,
		opened    = opened,
		options   = options,
	}, true, ""
}

project_builder_build :: proc(
	builder: ^Project_Builder,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> (^Project_Snapshot, bool, string) {
	if builder == nil || builder.slot == nil || pool == nil {
		return nil, false, "invalid project builder"
	}
	snapshot := new(Project_Snapshot, allocator)
	snapshot.id = builder.slot.id
	snapshot.root_key = dependency_object_key_clone(builder.slot.root_key, allocator)
	snapshot.root_uri = strings.clone(builder.slot.root_uri, allocator)
	snapshot.revision = builder.revision
	snapshot.source_inputs = make([dynamic]Workspace_Source_Input, 0, len(builder.inputs), allocator)
	for input in builder.inputs {
		append(&snapshot.source_inputs, workspace_source_input_clone(input, allocator))
	}
	snapshot.opened_dependencies = make(
		[dynamic]Workspace_Opened_Dependency,
		0,
		len(builder.opened),
		allocator,
	)
	for entry in builder.opened {
		append(&snapshot.opened_dependencies, workspace_opened_dependency_clone(entry, allocator))
	}
	files := make(
		[dynamic]semantic.Workspace_File_Input,
		0,
		len(builder.inputs) + len(builder.opened),
		context.temp_allocator,
	)
	for input in builder.inputs {
		append(&files, workspace_file_input_from_source(input, context.temp_allocator))
	}
	for opened in builder.opened {
		append(&files, workspace_file_input_from_source(opened.input, context.temp_allocator))
	}
	result := analyze_inputs(builder.workspace, files[:], pool, allocator)
	if !result.ok {
		project_snapshot_destroy(snapshot, allocator)
		free(snapshot, allocator)
		return nil, false, result.error
	}
	snapshot.session = result.session
	snapshot.last_update = result.last_update
	builder.next = snapshot
	return snapshot, true, ""
}

workspace_build_project_snapshot :: proc(
	workspace: ^Workspace,
	project_id: Project_Id,
	inputs: []Workspace_Source_Input,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> (^Project_Snapshot, bool, string) {
	builder, builder_ok, builder_error := workspace_project_snapshot_builder(
		workspace,
		project_id,
		inputs,
		options,
		allocator,
	)
	if !builder_ok {
		return nil, false, builder_error
	}
	return project_builder_build(&builder, pool, allocator)
}

workspace_open_remote_dependency_object :: proc(
	workspace: ^Workspace,
	project_id: Project_Id,
	key: Dependency_Object_Key,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> (^Project_Snapshot, bool, string) {
	current, current_ok := workspace_current_project_snapshot(workspace, project_id)
	if !current_ok || current == nil {
		return nil, false, "project has no current snapshot"
	}
	if workspace_opened_dependency_index(current.opened_dependencies[:], key) >= 0 {
		return current, true, ""
	}
	object_kind := workspace_dependency_object_kind_text(key.object_kind)
	if object_kind == "" {
		return nil, false, "unsupported dependency object kind"
	}
	config := remote_dependency_config_from_workspace(workspace)
	source_input, source_ok, source_error := remote_deps.open_source(
		&config,
		object_kind,
		key.object_name,
		allocator,
	)
	if !source_ok {
		return nil, false, source_error
	}
	opened := Workspace_Opened_Dependency {
		key = dependency_object_key_clone(key, allocator),
		input = Workspace_Source_Input {
			uri         = strings.clone(source_input.path, allocator),
			text        = strings.clone(source_input.source_text, allocator),
			revision    = current.revision + 1,
			open        = true,
			kind        = workspace_file_kind_for_dependency(key),
			object_name = strings.clone(key.object_name, allocator),
		},
	}
	return workspace_rebuild_project_snapshot_with_opened_dependencies(
		workspace,
		project_id,
		current,
		{opened},
		{},
		pool,
		options,
		allocator,
	)
}

workspace_close_remote_dependency_object :: proc(
	workspace: ^Workspace,
	project_id: Project_Id,
	key: Dependency_Object_Key,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> (^Project_Snapshot, bool, string) {
	current, current_ok := workspace_current_project_snapshot(workspace, project_id)
	if !current_ok || current == nil {
		return nil, false, "project has no current snapshot"
	}
	if workspace_opened_dependency_index(current.opened_dependencies[:], key) < 0 {
		return current, true, ""
	}
	return workspace_rebuild_project_snapshot_with_opened_dependencies(
		workspace,
		project_id,
		current,
		{},
		{key},
		pool,
		options,
		allocator,
	)
}

workspace_publish_project_snapshot :: proc(
	workspace: ^Workspace,
	project_id: Project_Id,
	next: ^Project_Snapshot,
) -> ^Project_Snapshot {
	slot := workspace_project_slot(workspace, project_id)
	if slot == nil || next == nil {
		return nil
	}
	next.id = slot.id
	current := sync.atomic_load_explicit(&slot.current, .Acquire)
	if current == next {
		return current
	}
	old := sync.atomic_exchange_explicit(&slot.current, next, .Acq_Rel)
	slot.running_revision = next.revision
	if old != nil {
		append(&slot.retired, old)
	}
	return old
}

@(private)
workspace_snapshot_state_ensure :: proc(workspace: ^Workspace, allocator: mem.Allocator) {
	if workspace.projects.allocator.procedure == nil {
		workspace.projects = make([dynamic]^Project_Slot, 0, 4, allocator)
	}
	if workspace.projects_by_object == nil {
		workspace.projects_by_object = make(
			map[Dependency_Object_Key][dynamic]Project_Id,
			16,
			allocator,
		)
	}
}

@(private)
workspace_snapshot_state_destroy :: proc(workspace: ^Workspace, allocator: mem.Allocator) {
	if workspace == nil {
		return
	}
	for slot in workspace.projects {
		if slot == nil {
			continue
		}
		current := sync.atomic_exchange_explicit(&slot.current, nil, .Acq_Rel)
		if current != nil {
			project_snapshot_destroy(current, allocator)
			free(current, allocator)
		}
		for snapshot in slot.retired {
			if snapshot != nil {
				project_snapshot_destroy(snapshot, allocator)
				free(snapshot, allocator)
			}
		}
		delete(slot.retired)
		free(slot, allocator)
	}
	if workspace.projects.allocator.procedure != nil {
		delete(workspace.projects)
	}
	if workspace.projects_by_object != nil {
		for _, projects in workspace.projects_by_object {
			delete(projects)
		}
		delete(workspace.projects_by_object)
	}
}

@(private)
project_snapshot_destroy :: proc(snapshot: ^Project_Snapshot, allocator: mem.Allocator) {
	if snapshot == nil {
		return
	}
	_ = allocator
	semantic.semantic_graph_update_result_destroy(&snapshot.last_update)
	semantic.semantic_graph_session_destroy(&snapshot.session)
	snapshot^ = {}
}

@(private)
workspace_rebuild_project_snapshot_with_opened_dependencies :: proc(
	workspace: ^Workspace,
	project_id: Project_Id,
	current: ^Project_Snapshot,
	to_open: []Workspace_Opened_Dependency,
	to_close: []Dependency_Object_Key,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> (^Project_Snapshot, bool, string) {
	builder, builder_ok, builder_error := workspace_project_snapshot_builder(
		workspace,
		project_id,
		current.source_inputs[:],
		options,
		allocator,
	)
	if !builder_ok {
		return nil, false, builder_error
	}
	next_opened := make(
		[dynamic]Workspace_Opened_Dependency,
		0,
		len(builder.opened) + len(to_open),
		allocator,
	)
	for entry in builder.opened {
		if workspace_dependency_key_list_contains(to_close, entry.key) {
			continue
		}
		append(&next_opened, workspace_opened_dependency_clone(entry, allocator))
	}
	for entry in to_open {
		if workspace_opened_dependency_index(next_opened[:], entry.key) >= 0 {
			continue
		}
		append(&next_opened, workspace_opened_dependency_clone(entry, allocator))
		workspace_map_object_project(workspace, entry.key, project_id, allocator)
	}
	builder.opened = next_opened
	next, build_ok, build_error := project_builder_build(&builder, pool, allocator)
	if !build_ok {
		return nil, false, build_error
	}
	_ = workspace_publish_project_snapshot(workspace, project_id, next)
	return next, true, ""
}

@(private)
workspace_project_slot :: proc(
	workspace: ^Workspace,
	project_id: Project_Id,
) -> ^Project_Slot {
	if workspace == nil || !workspace_project_id_valid(workspace, project_id) {
		return nil
	}
	return workspace.projects[int(project_id)]
}

@(private)
workspace_project_id_valid :: proc(workspace: ^Workspace, project_id: Project_Id) -> bool {
	index := int(project_id)
	return workspace != nil &&
	       workspace.projects.allocator.procedure != nil &&
	       0 <= index &&
	       index < len(workspace.projects) &&
	       workspace.projects[index] != nil
}

@(private)
project_id_list_contains :: proc(projects: []Project_Id, id: Project_Id) -> bool {
	for existing in projects {
		if existing == id {
			return true
		}
	}
	return false
}

@(private)
dependency_object_key_clone :: proc(
	key: Dependency_Object_Key,
	allocator: mem.Allocator,
) -> Dependency_Object_Key {
	return Dependency_Object_Key {
		product_version = strings.clone(key.product_version, allocator),
		package_name    = strings.clone(key.package_name, allocator),
		package_version = strings.clone(key.package_version, allocator),
		object_kind     = key.object_kind,
		object_name     = strings.clone(key.object_name, allocator),
	}
}

@(private)
workspace_opened_dependency_clone :: proc(
	entry: Workspace_Opened_Dependency,
	allocator: mem.Allocator,
) -> Workspace_Opened_Dependency {
	return Workspace_Opened_Dependency {
		key = dependency_object_key_clone(entry.key, allocator),
		input = workspace_source_input_clone(entry.input, allocator),
	}
}

@(private)
workspace_source_input_clone :: proc(
	input: Workspace_Source_Input,
	allocator: mem.Allocator,
) -> Workspace_Source_Input {
	return Workspace_Source_Input {
		uri         = strings.clone(input.uri, allocator),
		text        = strings.clone(input.text, allocator),
		revision    = input.revision,
		open        = input.open,
		kind        = input.kind,
		object_name = strings.clone(input.object_name, allocator) if input.object_name != "" else "",
	}
}

@(private)
workspace_opened_dependency_index :: proc(
	opened: []Workspace_Opened_Dependency,
	key: Dependency_Object_Key,
) -> int {
	for entry, i in opened {
		if workspace_dependency_object_key_equal(entry.key, key) {
			return i
		}
	}
	return -1
}

@(private)
workspace_dependency_key_list_contains :: proc(
	keys: []Dependency_Object_Key,
	key: Dependency_Object_Key,
) -> bool {
	for existing in keys {
		if workspace_dependency_object_key_equal(existing, key) {
			return true
		}
	}
	return false
}

@(private)
workspace_dependency_object_key_equal :: proc(left, right: Dependency_Object_Key) -> bool {
	return strings.equal_fold(left.product_version, right.product_version) &&
	       strings.equal_fold(left.package_name, right.package_name) &&
	       strings.equal_fold(left.package_version, right.package_version) &&
	       left.object_kind == right.object_kind &&
	       strings.equal_fold(left.object_name, right.object_name)
}

@(private)
workspace_dependency_object_kind_text :: proc(kind: Dependency_Object_Kind) -> string {
	switch kind {
	case .Report:
		return "report"
	case .Include:
		return "include"
	case .Class:
		return "global-class"
	case .Interface:
		return "global-interface"
	case .Function_Group:
		return "function-group"
	case .Function_Module:
		return "function-module"
	case .Type_Pool:
		return remote_deps.TYPEPOOL_OBJECT_KIND
	case .Message_Class:
		return "message-class"
	case .DDIC_Data_Element:
		return "ddic-data-element"
	case .DDIC_Structure:
		return "ddic-structure"
	case .DDIC_Table:
		return "ddic-table"
	case .DDIC_Table_Type:
		return "ddic-table-type"
	case .DDIC_View:
		return "ddic-view"
	case .Unknown:
	}
	return ""
}

@(private)
workspace_file_input_from_source :: proc(
	input: Workspace_Source_Input,
	allocator: mem.Allocator,
) -> semantic.Workspace_File_Input {
	parsed := parser.parse(input.text, input.uri, allocator)
	return semantic.Workspace_File_Input {
		path        = strings.clone(input.uri, allocator),
		root        = parsed.root,
		kind        = input.kind,
		object_name = strings.clone(input.object_name, allocator) if input.object_name != "" else "",
	}
}

@(private)
workspace_file_kind_for_dependency :: proc(key: Dependency_Object_Key) -> semantic.Workspace_File_Kind {
	#partial switch key.object_kind {
	case .Report:
		return .Report
	case .Include:
		return .Include
	case .Class:
		return .Class
	case .Interface:
		return .Interface
	case .Function_Group, .Function_Module:
		return .Function_Group
	case .Type_Pool:
		return .Type_Pool
	}
	return .Unknown
}
