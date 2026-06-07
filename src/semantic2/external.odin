package abap_frontend_semantic2

import "src:ast"
import string_interner "src:string_interner"
import trace "src:trace"

import "core:mem"
import "core:strings"

External_Candidate_Kind :: enum {
	Include_Source,
	Report,
	Function_Module,
	Class,
	Interface,
	DDIC_Type,
	DDIC_Table,
	Message_Class,
	Type_Pool,
	Global_Symbol,
}

External_Candidate_Hint :: enum {
	None,
	Include_Statement,
	Type_Reference,
	Identifier,
	Call_Function,
	Perform_In_Program,
	Submit,
	Open_SQL_Source,
	Type_Pool_Statement,
}

External_Candidate_Reason :: enum {
	Unresolved_Include,
	Unresolved_Reference,
	Unresolved_Type,
	Unresolved_Routine,
	Unresolved_SQL_Source,
	Type_Pool_Import,
}

Checker_Unresolved_Candidate :: struct {
	name:      string_interner.String,
	namespace: Namespace,
	kind:      External_Candidate_Kind,
	hint:      External_Candidate_Hint,
	reason:    External_Candidate_Reason,
	range:     Range,
	file:      ^Project_File,
	scope:     ^Scope,
	node:      ^ast.Node,
	if_found:  bool,
}

Semantic_Project_Id :: distinct u32

Semantic_Object_Key :: struct {
	kind: External_Candidate_Kind,
	name: string_interner.String,
}

Semantic_Project_Role :: enum {
	Editable_Root,
	External_Interface,
	Include_Fragment,
}

External_Interface_Object_Role :: enum {
	Unknown,
	Report,
	Function_Module,
	Class,
	Interface,
	DDIC_Type,
	DDIC_Table,
	Type_Pool,
}

External_Binding_Quality :: enum {
	Complete,
	Provisional,
	Blocked,
}

External_Binding :: struct {
	project_id: Semantic_Project_Id,
	entity:     ^Entity,
	quality:    External_Binding_Quality,
	generation: u64,
}

Semantic_Provider_Binding :: struct {
	key:     Semantic_Object_Key,
	binding: External_Binding,
}

Semantic_Dependency_Edge :: struct {
	key:      Semantic_Object_Key,
	binding:  External_Binding,
	resolved: bool,
	range:    Range,
	file:     ^Project_File,
	node:     ^ast.Node,
}

Semantic_Project_Record :: struct {
	id:                      Semantic_Project_Id,
	role:                    Semantic_Project_Role,
	root_key:                Semantic_Object_Key,
	project:                 ^Project,
	checker:                 ^Checker,
	provides:                [dynamic]Semantic_Object_Key,
	provider_bindings:       [dynamic]Semantic_Provider_Binding,
	resolved_dependencies:   [dynamic]Semantic_Dependency_Edge,
	unresolved_dependencies: [dynamic]Semantic_Dependency_Edge,
	unresolved:              [dynamic]Checker_Unresolved_Candidate,
	generation:              u64,
}

External_Source_File :: struct {
	path:           string,
	root:           ^ast.File,
	provided_names: [dynamic]string_interner.String,
}

External_Source_Input :: struct {
	path:           string,
	root:           ^ast.File,
	provided_names: []string,
	source_hash:    u64,
	generation:     u64,
}

External_Interface_Input :: struct {
	key:         Semantic_Object_Key,
	path:        string,
	root:        ^ast.File,
	source_hash: u64,
	generation:  u64,
	role:        External_Interface_Object_Role,
}

External_Lookup_Key :: struct {
	namespace: Namespace,
	name:      string_interner.String,
}

External_Semantic_Index :: struct {
	allocator:                    mem.Allocator,
	interner:                     ^string_interner.Interner,
	next_project_id:              u32,
	projects:                     [dynamic]Semantic_Project_Record,
	providers:                    map[Semantic_Object_Key]External_Binding,
	project_by_root_key:          map[Semantic_Object_Key]Semantic_Project_Id,
	dependents_by_object:         map[Semantic_Object_Key][dynamic]Semantic_Project_Id,
	unresolved_waiters_by_object: map[Semantic_Object_Key][dynamic]Semantic_Project_Id,
	lookup:                       map[External_Lookup_Key]Semantic_Object_Key,
}

External_Field_Summary :: struct {
	name:      string,
	type_name: string,
}

External_Semantics :: struct {
	allocator:          mem.Allocator,
	interner:           ^string_interner.Interner,
	index:              External_Semantic_Index,
	source_files:       [dynamic]External_Source_File,
	interface_projects: [dynamic]^Project,
	interface_checkers: [dynamic]^Checker,
	compat_project:     ^Project,
	compat_checker:     ^Checker,
	compat_root_file:   ^Project_File,
	compat_record_id:   Semantic_Project_Id,
}

external_semantics_make :: proc(
	interner: ^string_interner.Interner,
	allocator: mem.Allocator = context.allocator,
) -> External_Semantics {
	assert(interner != nil)
	return External_Semantics {
		allocator = allocator,
		interner = interner,
		index = external_semantic_index_make(interner, allocator),
		source_files = make([dynamic]External_Source_File, 0, 4, allocator),
		interface_projects = make([dynamic]^Project, 0, 4, allocator),
		interface_checkers = make([dynamic]^Checker, 0, 4, allocator),
	}
}

external_semantics_destroy :: proc(external: ^External_Semantics) {
	if external == nil {
		return
	}
	for project in external.interface_projects {
		if project != nil {
			project_destroy(project)
			free(project, external.allocator)
		}
	}
	for checker in external.interface_checkers {
		if checker != nil {
			free(checker, external.allocator)
		}
	}
	if external.compat_project != nil {
		project_destroy(external.compat_project)
		free(external.compat_project, external.allocator)
	}
	if external.compat_checker != nil {
		free(external.compat_checker, external.allocator)
	}
	external^ = {}
}

external_semantic_index_make :: proc(
	interner: ^string_interner.Interner,
	allocator: mem.Allocator = context.allocator,
) -> External_Semantic_Index {
	assert(interner != nil)
	return External_Semantic_Index {
		allocator = allocator,
		interner = interner,
		next_project_id = 1,
		projects = make([dynamic]Semantic_Project_Record, 0, 4, allocator),
		providers = make(map[Semantic_Object_Key]External_Binding, 0, allocator),
		project_by_root_key = make(map[Semantic_Object_Key]Semantic_Project_Id, 0, allocator),
		dependents_by_object = make(
			map[Semantic_Object_Key][dynamic]Semantic_Project_Id,
			0,
			allocator,
		),
		unresolved_waiters_by_object = make(
			map[Semantic_Object_Key][dynamic]Semantic_Project_Id,
			0,
			allocator,
		),
		lookup = make(map[External_Lookup_Key]Semantic_Object_Key, 0, allocator),
	}
}

external_semantic_index_import_providers :: proc(
	index: ^External_Semantic_Index,
	source: ^External_Semantic_Index,
) {
	if index == nil || source == nil {
		return
	}
	for key, binding in source.providers {
		index.providers[key] = binding
		if raw := u32(binding.project_id); raw >= index.next_project_id {
			index.next_project_id = raw + 1
		}
	}
	for lookup_key, object_key in source.lookup {
		index.lookup[lookup_key] = object_key
	}
	for root_key, project_id in source.project_by_root_key {
		index.project_by_root_key[root_key] = project_id
		if raw := u32(project_id); raw >= index.next_project_id {
			index.next_project_id = raw + 1
		}
	}
}

external_semantic_index_import_external_project_records :: proc(
	index: ^External_Semantic_Index,
	source: ^External_Semantic_Index,
) {
	if index == nil || source == nil {
		return
	}
	for record in source.projects {
		if record.role != .External_Interface || !semantic_object_key_is_valid(record.root_key) {
			continue
		}
		if _, exists := external_semantic_index_project_record(index, record.id); exists {
			continue
		}
		stored := external_semantic_index_add_project_record(index, record)
		for edge in stored.resolved_dependencies {
			external_semantic_index_add_dependency(index, stored.id, edge)
		}
		for edge in stored.unresolved_dependencies {
			external_semantic_index_add_dependency(index, stored.id, edge)
		}
	}
}

semantic_project_record_make :: proc(
	index: ^External_Semantic_Index,
	role: Semantic_Project_Role,
	project: ^Project,
	checker: ^Checker,
	root_key: Semantic_Object_Key = {},
	generation: u64 = 0,
) -> Semantic_Project_Record {
	assert(index != nil)
	id := Semantic_Project_Id(index.next_project_id)
	index.next_project_id += 1
	return Semantic_Project_Record {
		id = id,
		role = role,
		root_key = root_key,
		project = project,
		checker = checker,
		provides = make([dynamic]Semantic_Object_Key, 0, 4, index.allocator),
		provider_bindings = make([dynamic]Semantic_Provider_Binding, 0, 4, index.allocator),
		resolved_dependencies = make([dynamic]Semantic_Dependency_Edge, 0, 8, index.allocator),
		unresolved_dependencies = make([dynamic]Semantic_Dependency_Edge, 0, 8, index.allocator),
		unresolved = make([dynamic]Checker_Unresolved_Candidate, 0, 8, index.allocator),
		generation = generation,
	}
}

external_semantic_index_add_project_record :: proc(
	index: ^External_Semantic_Index,
	record: Semantic_Project_Record,
) -> ^Semantic_Project_Record {
	assert(index != nil)
	append(&index.projects, record)
	stored := &index.projects[len(index.projects) - 1]
	if semantic_object_key_is_valid(stored.root_key) {
		index.project_by_root_key[stored.root_key] = stored.id
	}
	if raw := u32(stored.id); raw >= index.next_project_id {
		index.next_project_id = raw + 1
	}
	return stored
}

external_semantic_index_project_record :: proc(
	index: ^External_Semantic_Index,
	id: Semantic_Project_Id,
) -> (
	^Semantic_Project_Record,
	bool,
) {
	for &record in index.projects {
		if record.id == id {
			return &record, true
		}
	}
	return nil, false
}

external_semantic_index_publish_provider :: proc(
	index: ^External_Semantic_Index,
	record: ^Semantic_Project_Record,
	key: Semantic_Object_Key,
	entity: ^Entity,
	quality: External_Binding_Quality = .Complete,
	generation: u64 = 0,
) -> External_Binding {
	assert(index != nil && record != nil && entity != nil)
	assert(semantic_object_key_is_valid(key))
	binding := External_Binding {
		project_id = record.id,
		entity     = entity,
		quality    = quality,
		generation = generation,
	}
	index.providers[key] = binding
	semantic_project_record_add_provide(record, key)
	semantic_project_record_add_provider_binding(record, key, binding)
	external_semantic_index_add_entity_lookup(index, key, entity)
	return binding
}

external_semantic_index_lookup :: proc(
	index: ^External_Semantic_Index,
	namespace: Namespace,
	name: string_interner.String,
	preferred_kind: External_Candidate_Kind = .Global_Symbol,
) -> (
	Semantic_Object_Key,
	External_Binding,
	bool,
) {
	if index == nil || !string_interner.is_valid(name) {
		return {}, {}, false
	}
	preferred_key := Semantic_Object_Key {
		kind = preferred_kind,
		name = name,
	}
	if binding, ok := index.providers[preferred_key];
	   ok && external_binding_occupies_namespace(binding, namespace) {
		return preferred_key, binding, true
	}
	if key, key_ok := index.lookup[External_Lookup_Key{namespace = namespace, name = name}];
	   key_ok {
		if binding, binding_ok := index.providers[key];
		   binding_ok && external_binding_occupies_namespace(binding, namespace) {
			return key, binding, true
		}
	}
	return {}, {}, false
}

external_semantic_index_add_dependency :: proc(
	index: ^External_Semantic_Index,
	project_id: Semantic_Project_Id,
	edge: Semantic_Dependency_Edge,
) {
	if index == nil ||
	   !semantic_project_id_is_valid(project_id) ||
	   !semantic_object_key_is_valid(edge.key) {
		return
	}
	if edge.resolved {
		external_semantic_index_add_project_id(
			&index.dependents_by_object,
			edge.key,
			project_id,
			index.allocator,
		)
	} else {
		external_semantic_index_add_project_id(
			&index.unresolved_waiters_by_object,
			edge.key,
			project_id,
			index.allocator,
		)
	}
}

semantic_project_record_add_provide :: proc(
	record: ^Semantic_Project_Record,
	key: Semantic_Object_Key,
) {
	if record == nil || !semantic_object_key_is_valid(key) {
		return
	}
	for existing in record.provides {
		if existing == key {
			return
		}
	}
	append(&record.provides, key)
}

semantic_project_record_add_provider_binding :: proc(
	record: ^Semantic_Project_Record,
	key: Semantic_Object_Key,
	binding: External_Binding,
) {
	if record == nil || !semantic_object_key_is_valid(key) || binding.entity == nil {
		return
	}
	for &existing in record.provider_bindings {
		if existing.key == key {
			existing.binding = binding
			return
		}
	}
	append(&record.provider_bindings, Semantic_Provider_Binding{key = key, binding = binding})
}

semantic_project_record_add_dependency :: proc(
	record: ^Semantic_Project_Record,
	edge: Semantic_Dependency_Edge,
) {
	if record == nil || !semantic_object_key_is_valid(edge.key) {
		return
	}
	if edge.resolved {
		checker_add_external_dependency_edge_to_list(&record.resolved_dependencies, edge)
	} else {
		checker_add_external_dependency_edge_to_list(&record.unresolved_dependencies, edge)
	}
}

external_semantic_index_add_entity_lookup :: proc(
	index: ^External_Semantic_Index,
	key: Semantic_Object_Key,
	entity: ^Entity,
) {
	if index == nil || entity == nil {
		return
	}
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if entity_kind_occupies(entity.kind, namespace) {
			lookup_key := External_Lookup_Key {
				namespace = namespace,
				name      = entity.name,
			}
			if _, exists := index.lookup[lookup_key]; !exists {
				index.lookup[lookup_key] = key
			}
		}
	}
}

external_semantic_index_add_project_id :: proc(
	waiters: ^map[Semantic_Object_Key][dynamic]Semantic_Project_Id,
	key: Semantic_Object_Key,
	project_id: Semantic_Project_Id,
	allocator: mem.Allocator,
) {
	if projects, ok := waiters^[key]; ok {
		for existing in projects {
			if existing == project_id {
				return
			}
		}
		append(&projects, project_id)
		waiters^[key] = projects
		return
	}
	projects := make([dynamic]Semantic_Project_Id, 0, 2, allocator)
	append(&projects, project_id)
	waiters^[key] = projects
}

external_semantic_index_remove_project_id :: proc(
	waiters: ^map[Semantic_Object_Key][dynamic]Semantic_Project_Id,
	key: Semantic_Object_Key,
	project_id: Semantic_Project_Id,
) {
	if waiters == nil ||
	   !semantic_object_key_is_valid(key) ||
	   !semantic_project_id_is_valid(project_id) {
		return
	}
	projects, ok := waiters^[key]
	if !ok {
		return
	}
	write := 0
	for existing in projects {
		if existing == project_id {
			continue
		}
		projects[write] = existing
		write += 1
	}
	if write == len(projects) {
		return
	}
	if write == 0 {
		delete(projects)
		delete_key(waiters, key)
		return
	}
	resize(&projects, write)
	waiters^[key] = projects
}

external_semantic_index_replace_project_record :: proc(
	index: ^External_Semantic_Index,
	record: Semantic_Project_Record,
) -> ^Semantic_Project_Record {
	assert(index != nil)
	if semantic_object_key_is_valid(record.root_key) {
		_ = external_semantic_index_remove_project_record_by_root_key(index, record.root_key)
	} else if semantic_project_id_is_valid(record.id) {
		_ = external_semantic_index_remove_project_record(index, record.id)
	}
	stored := external_semantic_index_add_project_record(index, record)
	external_semantic_index_add_project_record_contributions(index, stored)
	return stored
}

external_semantic_index_remove_project_record_by_root_key :: proc(
	index: ^External_Semantic_Index,
	root_key: Semantic_Object_Key,
) -> bool {
	if index == nil || !semantic_object_key_is_valid(root_key) {
		return false
	}
	for record in index.projects {
		if record.root_key == root_key {
			return external_semantic_index_remove_project_record(index, record.id)
		}
	}
	return false
}

external_semantic_index_remove_project_record :: proc(
	index: ^External_Semantic_Index,
	id: Semantic_Project_Id,
) -> bool {
	if index == nil || !semantic_project_id_is_valid(id) {
		return false
	}
	removed := false
	removed_record: Semantic_Project_Record
	write := 0
	for record in index.projects {
		if record.id == id {
			removed = true
			removed_record = record
			continue
		}
		index.projects[write] = record
		write += 1
	}
	if removed {
		resize(&index.projects, write)
		external_semantic_index_remove_project_record_contributions(index, &removed_record)
	}
	return removed
}

external_semantic_index_rebuild_maps :: proc(index: ^External_Semantic_Index) {
	if index == nil {
		return
	}
	when trace.ENABLED {
		trace_start := trace.now()
		project_count := len(index.projects)
		resolved_count := 0
		unresolved_count := 0
	}
	next_project_id := index.next_project_id
	external_semantic_index_reset_maps(index)
	for &record in index.projects {
		when trace.ENABLED {
			resolved_count += len(record.resolved_dependencies)
			unresolved_count += len(record.unresolved_dependencies)
		}
		if semantic_object_key_is_valid(record.root_key) {
			index.project_by_root_key[record.root_key] = record.id
		}
		if raw := u32(record.id); raw >= next_project_id {
			next_project_id = raw + 1
		}
		for provided in record.provider_bindings {
			index.providers[provided.key] = provided.binding
			external_semantic_index_add_entity_lookup(index, provided.key, provided.binding.entity)
		}
		for edge in record.resolved_dependencies {
			external_semantic_index_add_dependency(index, record.id, edge)
		}
		for edge in record.unresolved_dependencies {
			external_semantic_index_add_dependency(index, record.id, edge)
		}
	}
	index.next_project_id = next_project_id
	when trace.ENABLED {
		trace.eprintf(
			"[trace - semantic2] external index rebuild maps projects=%d resolved_edges=%d unresolved_edges=%d elapsed_ms=%.3f\n",
			project_count,
			resolved_count,
			unresolved_count,
			trace.duration_ms_since(trace_start),
		)
	}
}

external_semantic_index_add_project_record_contributions :: proc(
	index: ^External_Semantic_Index,
	record: ^Semantic_Project_Record,
) {
	assert(index != nil && record != nil)
	for provided in record.provider_bindings {
		index.providers[provided.key] = provided.binding
		external_semantic_index_add_entity_lookup(index, provided.key, provided.binding.entity)
	}
	for edge in record.resolved_dependencies {
		external_semantic_index_add_dependency(index, record.id, edge)
	}
	for edge in record.unresolved_dependencies {
		external_semantic_index_add_dependency(index, record.id, edge)
	}
}

external_semantic_index_remove_project_record_contributions :: proc(
	index: ^External_Semantic_Index,
	record: ^Semantic_Project_Record,
) {
	assert(index != nil && record != nil)
	if semantic_object_key_is_valid(record.root_key) {
		external_semantic_index_rebuild_root_key(index, record.root_key)
	}
	for edge in record.resolved_dependencies {
		external_semantic_index_remove_project_id(
			&index.dependents_by_object,
			edge.key,
			record.id,
		)
	}
	for edge in record.unresolved_dependencies {
		external_semantic_index_remove_project_id(
			&index.unresolved_waiters_by_object,
			edge.key,
			record.id,
		)
	}
	for provided in record.provider_bindings {
		external_semantic_index_rebuild_provider_key(index, provided.key)
		external_semantic_index_rebuild_entity_lookups(index, provided.binding.entity)
	}
}

external_semantic_index_rebuild_root_key :: proc(
	index: ^External_Semantic_Index,
	root_key: Semantic_Object_Key,
) {
	assert(index != nil)
	if !semantic_object_key_is_valid(root_key) {
		return
	}
	project_id: Semantic_Project_Id
	found := false
	for record in index.projects {
		if record.root_key == root_key {
			project_id = record.id
			found = true
		}
	}
	if found {
		index.project_by_root_key[root_key] = project_id
	} else {
		delete_key(&index.project_by_root_key, root_key)
	}
}

external_semantic_index_rebuild_provider_key :: proc(
	index: ^External_Semantic_Index,
	key: Semantic_Object_Key,
) {
	assert(index != nil)
	if !semantic_object_key_is_valid(key) {
		return
	}
	binding: External_Binding
	found := false
	for record in index.projects {
		for provided in record.provider_bindings {
			if provided.key == key {
				binding = provided.binding
				found = true
			}
		}
	}
	if found {
		index.providers[key] = binding
	} else {
		delete_key(&index.providers, key)
	}
}

external_semantic_index_rebuild_entity_lookups :: proc(
	index: ^External_Semantic_Index,
	entity: ^Entity,
) {
	assert(index != nil)
	if entity == nil {
		return
	}
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if entity_kind_occupies(entity.kind, namespace) {
			external_semantic_index_rebuild_lookup_key(
				index,
				External_Lookup_Key{namespace = namespace, name = entity.name},
			)
		}
	}
}

external_semantic_index_rebuild_lookup_key :: proc(
	index: ^External_Semantic_Index,
	lookup_key: External_Lookup_Key,
) {
	assert(index != nil)
	delete_key(&index.lookup, lookup_key)
	for record in index.projects {
		for provided in record.provider_bindings {
			entity := provided.binding.entity
			if entity != nil &&
			   entity.name == lookup_key.name &&
			   entity_kind_occupies(entity.kind, lookup_key.namespace) {
				index.lookup[lookup_key] = provided.key
				return
			}
		}
	}
}

external_semantic_index_reset_maps :: proc(index: ^External_Semantic_Index) {
	assert(index != nil)
	clear(&index.providers)
	clear(&index.project_by_root_key)
	for _, projects in index.dependents_by_object {
		delete(projects)
	}
	clear(&index.dependents_by_object)
	for _, projects in index.unresolved_waiters_by_object {
		delete(projects)
	}
	clear(&index.unresolved_waiters_by_object)
	clear(&index.lookup)
}

external_binding_occupies_namespace :: proc(
	binding: External_Binding,
	namespace: Namespace,
) -> bool {
	return binding.entity != nil && entity_kind_occupies(binding.entity.kind, namespace)
}

semantic_project_id_is_valid :: #force_inline proc(id: Semantic_Project_Id) -> bool {
	return u32(id) != 0
}

semantic_object_key_is_valid :: #force_inline proc(key: Semantic_Object_Key) -> bool {
	return string_interner.is_valid(key.name)
}

external_semantics_analyze_interface_input :: proc(
	external: ^External_Semantics,
	input: External_Interface_Input,
) -> ^Semantic_Project_Record {
	assert(external != nil && input.root != nil)
	root_key := external_interface_input_key(input)
	assert(semantic_object_key_is_valid(root_key))

	project := new(Project, external.allocator)
	assert(project != nil)
	project^ = project_make_with_interner(external.interner)
	append(&external.interface_projects, project)

	checker := new(Checker, external.allocator)
	assert(checker != nil)
	checker_init(checker, project, external)
	append(&external.interface_checkers, checker)

	file := checker_add_file(checker, input.path, input.root)
	checker_check_file(checker, file)

	record := semantic_project_record_make(
		&external.index,
		.External_Interface,
		project,
		checker,
		root_key,
		input.generation,
	)
	external_semantics_publish_interface_project_providers(
		external,
		&record,
		file,
		root_key,
		input.generation,
	)
	for edge in checker.info.resolved_external_dependencies {
		semantic_project_record_add_dependency(&record, edge)
	}
	for edge in checker.info.unresolved_external_dependencies {
		semantic_project_record_add_dependency(&record, edge)
	}
	for candidate in checker.info.unresolved {
		checker_add_unresolved_candidate_to_list(&record.unresolved, candidate)
	}

	stored := external_semantic_index_add_project_record(&external.index, record)
	for edge in stored.resolved_dependencies {
		external_semantic_index_add_dependency(&external.index, stored.id, edge)
	}
	for edge in stored.unresolved_dependencies {
		external_semantic_index_add_dependency(&external.index, stored.id, edge)
	}
	return stored
}

external_semantics_reanalyze_interface_input :: proc(
	external: ^External_Semantics,
	input: External_Interface_Input,
) -> ^Semantic_Project_Record {
	assert(external != nil)
	root_key := external_interface_input_key(input)
	assert(semantic_object_key_is_valid(root_key))
	_ = external_semantic_index_remove_project_record_by_root_key(&external.index, root_key)
	return external_semantics_analyze_interface_input(external, input)
}

external_semantics_publish_interface_project_providers :: proc(
	external: ^External_Semantics,
	record: ^Semantic_Project_Record,
	file: ^Project_File,
	root_key: Semantic_Object_Key,
	generation: u64,
) {
	assert(external != nil && record != nil && file != nil && file.root_scope != nil)
	if entity, ok := external_interface_entity_for_key(file.root_scope, root_key); ok {
		_ = external_semantic_index_publish_provider(
			&external.index,
			record,
			root_key,
			entity,
			generation = generation,
		)
	}
	for entity in file.root_scope.declarations {
		if key, ok := external_object_key_for_provider_entity(entity); ok {
			_ = external_semantic_index_publish_provider(
				&external.index,
				record,
				key,
				entity,
				generation = generation,
			)
		}
	}
}

external_semantics_add_source_file :: proc(
	external: ^External_Semantics,
	path: string,
	root: ^ast.File,
	provided_names: []string,
) -> ^External_Source_File {
	assert(external != nil)
	names := make([dynamic]string_interner.String, 0, len(provided_names), external.allocator)
	for name in provided_names {
		interned := external_intern_name(external, name)
		if string_interner.is_valid(interned) {
			append(&names, interned)
		}
	}
	append(
		&external.source_files,
		External_Source_File {
			path = strings.clone(path, external.allocator),
			root = root,
			provided_names = names,
		},
	)
	return &external.source_files[len(external.source_files) - 1]
}

external_semantics_upsert_source_input :: proc(
	external: ^External_Semantics,
	input: External_Source_Input,
) -> ^External_Source_File {
	assert(external != nil)
	for &source in external.source_files {
		if source.path != input.path {
			continue
		}
		source.root = input.root
		source.provided_names = external_source_input_intern_names(external, input.provided_names)
		return &source
	}
	return external_semantics_add_source_file(
		external,
		input.path,
		input.root,
		input.provided_names,
	)
}

external_source_input_intern_names :: proc(
	external: ^External_Semantics,
	provided_names: []string,
) -> [dynamic]string_interner.String {
	names := make([dynamic]string_interner.String, 0, len(provided_names), external.allocator)
	for name in provided_names {
		interned := external_intern_name(external, name)
		if string_interner.is_valid(interned) {
			append(&names, interned)
		}
	}
	return names
}

external_semantics_lookup :: proc(
	external: ^External_Semantics,
	namespace: Namespace,
	name: string_interner.String,
	preferred_kind: External_Candidate_Kind = .Global_Symbol,
) -> (
	^Entity,
	bool,
) {
	if _, binding, ok := external_semantic_index_lookup(
		&external.index,
		namespace,
		name,
		preferred_kind,
	); ok {
		return binding.entity, true
	}
	return nil, false
}

external_semantics_add_entity :: proc(
	external: ^External_Semantics,
	namespace: Namespace,
	name: string,
	kind: Entity_Kind,
) -> ^Entity {
	assert(external != nil)
	_, _, _, root_scope, record := external_semantics_ensure_compat_project(external)
	entity := external_new_entity(external, kind)
	entity.name = external_intern_name(external, name)
	entity.state = .Resolved
	entity.scope = root_scope
	entity.source_file = external.compat_root_file
	entity.type = external_default_type_for_entity(external, entity)
	_ = scope_insert_declaration(root_scope, entity)
	key := external_object_key_for_entity(entity, namespace)
	_ = external_semantic_index_publish_provider(&external.index, record, key, entity)
	return entity
}

external_semantics_add_class_summary :: proc(
	external: ^External_Semantics,
	name: string,
) -> ^Entity {
	entity := external_semantics_add_entity(external, .Type, name, .Class)
	payload, ok := entity.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	payload.definition_scope = external_new_scope(external, entity.scope, .Class, entity)
	entity.type = external_type_class_or_interface(external, entity, .Class)
	return entity
}

external_semantics_add_interface_summary :: proc(
	external: ^External_Semantics,
	name: string,
) -> ^Entity {
	entity := external_semantics_add_entity(external, .Type, name, .Interface)
	payload, ok := entity.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	payload.definition_scope = external_new_scope(external, entity.scope, .Interface, entity)
	entity.type = external_type_class_or_interface(external, entity, .Interface)
	return entity
}

external_semantics_add_structure_summary :: proc(
	external: ^External_Semantics,
	name: string,
	fields: []External_Field_Summary,
) -> ^Entity {
	entity := external_semantics_add_entity(external, .Type, name, .Type_Def)
	structure_scope := external_new_scope(external, entity.scope, .Structure, entity)
	structure := external_new_structure(external, entity.name, structure_scope)
	structure_type := external_new_type(external, .Structure)
	structure_type.name = entity.name
	structure_type.structure = structure
	entity.type = external_new_type(external, .Named)
	entity.type.name = entity.name
	entity.type.entity = entity
	entity.type.base = structure_type
	if payload, ok := entity.payload.(^Entity_Type_Name_Payload); ok && payload != nil {
		payload.is_alias = true
		payload.underlying = structure_type
		payload.structure = structure
		payload.original_type = entity.type
	}
	for field, index in fields {
		field_entity := external_new_entity(external, .Field)
		field_entity.name = external_intern_name(external, field.name)
		field_entity.state = .Resolved
		field_entity.scope = structure_scope
		field_entity.owner = entity
		field_entity.type = external_builtin_type(external, field.type_name)
		field_payload, field_ok := field_entity.payload.(^Entity_Field_Payload)
		assert(field_ok && field_payload != nil)
		field_payload.owner_structure = structure
		field_payload.field_index = i32(index)
		field_payload.type_clause_form = .Type
		field_payload.has_type_clause_form = true
		append(&structure.fields, field_entity)
		_ = scope_insert_declaration(structure_scope, field_entity)
	}
	if record, ok := external_semantic_index_project_record(
		&external.index,
		external.compat_record_id,
	); ok {
		_ = external_semantic_index_publish_provider(
			&external.index,
			record,
			Semantic_Object_Key{kind = .DDIC_Table, name = entity.name},
			entity,
		)
	}
	return entity
}

external_semantics_add_routine_summary :: proc(
	external: ^External_Semantics,
	name: string,
	kind: Entity_Kind = .Module,
) -> ^Entity {
	assert(kind == .Form || kind == .Method || kind == .Module || kind == .Event)
	entity := external_semantics_add_entity(external, .Routine, name, kind)
	signature_scope := external_new_scope(
		external,
		entity.scope,
		checker_scope_kind_for_routine(kind),
		entity,
	)
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	payload.signature_scope = signature_scope
	payload.body_scope = signature_scope
	entity.type = external_new_type(external, .Routine)
	entity.type.routine.signature_scope = signature_scope
	return entity
}

external_semantics_ensure_compat_project :: proc(
	external: ^External_Semantics,
) -> (
	^Project,
	^Checker,
	^Project_File,
	^Scope,
	^Semantic_Project_Record,
) {
	assert(external != nil)
	if external.compat_project == nil {
		external.compat_project = new(Project, external.allocator)
		assert(external.compat_project != nil)
		external.compat_project^ = project_make_with_interner(external.interner)

		external.compat_checker = new(Checker, external.allocator)
		assert(external.compat_checker != nil)
		checker_init(external.compat_checker, external.compat_project, nil)
		external.compat_root_file = checker_add_file(
			external.compat_checker,
			"external://semantic-summary",
			nil,
		)

		record := semantic_project_record_make(
			&external.index,
			.External_Interface,
			external.compat_project,
			external.compat_checker,
		)
		stored := external_semantic_index_add_project_record(&external.index, record)
		external.compat_record_id = stored.id
	}
	record, ok := external_semantic_index_project_record(
		&external.index,
		external.compat_record_id,
	)
	assert(
		ok &&
		record != nil &&
		external.compat_root_file != nil &&
		external.compat_root_file.root_scope != nil,
	)
	return external.compat_project,
		external.compat_checker,
		external.compat_root_file,
		external.compat_root_file.root_scope,
		record
}

external_interface_input_key :: proc(input: External_Interface_Input) -> Semantic_Object_Key {
	key := input.key
	if input.role != .Unknown {
		key.kind = external_interface_object_role_kind(input.role)
	}
	return key
}

external_interface_object_role_kind :: proc(
	role: External_Interface_Object_Role,
) -> External_Candidate_Kind {
	switch role {
	case .Report:
		return .Report
	case .Function_Module:
		return .Function_Module
	case .Class:
		return .Class
	case .Interface:
		return .Interface
	case .DDIC_Type:
		return .DDIC_Type
	case .DDIC_Table:
		return .DDIC_Table
	case .Type_Pool:
		return .Type_Pool
	case .Unknown:
	}
	return .Global_Symbol
}

external_interface_entity_for_key :: proc(
	scope: ^Scope,
	key: Semantic_Object_Key,
) -> (
	^Entity,
	bool,
) {
	if scope == nil || !semantic_object_key_is_valid(key) {
		return nil, false
	}
	namespace := external_object_key_namespace(key)
	if entity, ok := scope_lookup_declaration(scope, namespace, key.name); ok {
		return entity, true
	}
	return nil, false
}

external_object_key_namespace :: proc(key: Semantic_Object_Key) -> Namespace {
	#partial switch key.kind {
	case .Function_Module:
		return .Routine
	case .Report, .Include_Source, .Message_Class:
		return .Value
	case:
	}
	return .Type
}

external_object_key_for_provider_entity :: proc(entity: ^Entity) -> (Semantic_Object_Key, bool) {
	if entity == nil || !string_interner.is_valid(entity.name) {
		return {}, false
	}
	#partial switch entity.kind {
	case .Class:
		return Semantic_Object_Key{kind = .Class, name = entity.name}, true
	case .Interface:
		return Semantic_Object_Key{kind = .Interface, name = entity.name}, true
	case .Module:
		return Semantic_Object_Key{kind = .Function_Module, name = entity.name}, true
	case .Report:
		return Semantic_Object_Key{kind = .Report, name = entity.name}, true
	case .Type_Def:
		return Semantic_Object_Key{kind = .DDIC_Type, name = entity.name}, true
	case:
	}
	return {}, false
}

external_object_key_for_entity :: proc(
	entity: ^Entity,
	namespace: Namespace,
) -> Semantic_Object_Key {
	assert(entity != nil)
	kind := External_Candidate_Kind.Global_Symbol
	#partial switch entity.kind {
	case .Class:
		kind = .Class
	case .Interface:
		kind = .Interface
	case .Module:
		kind = .Function_Module
	case .Report:
		kind = .Report
	case .Type_Def:
		kind = .DDIC_Type
	case:
		if namespace == .Routine {
			kind = .Function_Module
		}
	}
	return Semantic_Object_Key{kind = kind, name = entity.name}
}

checker_add_unresolved_candidate :: proc(
	ctx: ^Checker_Context,
	name: string_interner.String,
	namespace: Namespace,
	kind: External_Candidate_Kind,
	hint: External_Candidate_Hint,
	reason: External_Candidate_Reason,
	range: Range,
	node: ^ast.Node = nil,
	if_found := false,
) {
	if ctx == nil || !string_interner.is_valid(name) {
		return
	}
	candidate := Checker_Unresolved_Candidate {
		name      = name,
		namespace = namespace,
		kind      = kind,
		hint      = hint,
		reason    = reason,
		range     = range,
		file      = ctx.file,
		scope     = ctx.scope,
		node      = node,
		if_found  = if_found,
	}
	if !checker_unresolved_candidate_list_contains(ctx.info.unresolved[:], candidate) {
		append(&ctx.info.unresolved, candidate)
		checker_add_unresolved_dependency_edge(ctx, candidate)
	}
}

checker_add_unresolved_candidate_to_list :: proc(
	list: ^[dynamic]Checker_Unresolved_Candidate,
	candidate: Checker_Unresolved_Candidate,
) {
	for existing in list^ {
		if existing.name == candidate.name &&
		   existing.kind == candidate.kind &&
		   existing.namespace == candidate.namespace &&
		   existing.file == candidate.file &&
		   existing.range == candidate.range {
			return
		}
	}
	append(list, candidate)
}

checker_unresolved_candidate_list_contains :: proc(
	list: []Checker_Unresolved_Candidate,
	candidate: Checker_Unresolved_Candidate,
) -> bool {
	for existing in list {
		if existing.name == candidate.name &&
		   existing.kind == candidate.kind &&
		   existing.namespace == candidate.namespace &&
		   existing.file == candidate.file &&
		   existing.range == candidate.range {
			return true
		}
	}
	return false
}

checker_add_resolved_external_dependency :: proc(
	ctx: ^Checker_Context,
	key: Semantic_Object_Key,
	binding: External_Binding,
	range: Range = {},
	node: ^ast.Node = nil,
) {
	if ctx == nil || !semantic_object_key_is_valid(key) || binding.entity == nil {
		return
	}
	edge := Semantic_Dependency_Edge {
		key      = key,
		binding  = binding,
		resolved = true,
		range    = range,
		file     = ctx.file,
		node     = node,
	}
	checker_add_external_dependency_edge_to_list(&ctx.info.resolved_external_dependencies, edge)
}

checker_add_unresolved_dependency_edge :: proc(
	ctx: ^Checker_Context,
	candidate: Checker_Unresolved_Candidate,
) {
	if ctx == nil || !string_interner.is_valid(candidate.name) {
		return
	}
	edge := Semantic_Dependency_Edge {
		key = Semantic_Object_Key{kind = candidate.kind, name = candidate.name},
		resolved = false,
		range = candidate.range,
		file = candidate.file,
		node = candidate.node,
	}
	checker_add_external_dependency_edge_to_list(&ctx.info.unresolved_external_dependencies, edge)
}

checker_add_external_dependency_edge_to_list :: proc(
	list: ^[dynamic]Semantic_Dependency_Edge,
	edge: Semantic_Dependency_Edge,
) {
	for existing in list^ {
		if existing.key == edge.key &&
		   existing.resolved == edge.resolved &&
		   existing.file == edge.file &&
		   existing.range == edge.range &&
		   existing.node == edge.node {
			return
		}
	}
	append(list, edge)
}

external_intern_name :: proc(
	external: ^External_Semantics,
	name: string,
) -> string_interner.String {
	canonical := strings.to_lower(name, context.temp_allocator)
	return string_interner.insert(external.interner, canonical)
}

external_new_entity :: proc(external: ^External_Semantics, kind: Entity_Kind) -> ^Entity {
	project, _, _, _, _ := external_semantics_ensure_compat_project(external)
	return project_new_entity(project, kind)
}

external_new_scope :: proc(
	external: ^External_Semantics,
	parent: ^Scope,
	kind: Scope_Kind,
	owner: ^Entity = nil,
) -> ^Scope {
	project, _, _, root_scope, _ := external_semantics_ensure_compat_project(external)
	parent_scope := parent
	if parent == nil {
		if kind == .File {
			return root_scope
		}
		parent_scope = root_scope
	}
	scope := project_new_scope(project)
	scope.kind = kind
	scope.parent = parent_scope
	scope.owner = owner
	if parent_scope != nil {
		scope.next = parent_scope.head_child
		parent_scope.head_child = scope
		append(&parent_scope.children, scope)
	}
	return scope
}

external_new_type :: proc(external: ^External_Semantics, kind: Type_Kind) -> ^Type {
	project, _, _, _, _ := external_semantics_ensure_compat_project(external)
	return project_new_type(project, kind)
}

external_new_structure :: proc(
	external: ^External_Semantics,
	name: string_interner.String,
	scope: ^Scope,
) -> ^Structure {
	project, _, file, _, _ := external_semantics_ensure_compat_project(external)
	return project_new_structure(project, name, file, scope)
}

external_default_type_for_entity :: proc(external: ^External_Semantics, entity: ^Entity) -> ^Type {
	#partial switch entity.kind {
	case .Class, .Interface:
		return external_type_class_or_interface(external, entity, entity.kind)
	case .Type_Def:
		typ := external_new_type(external, .Named)
		typ.name = entity.name
		typ.entity = entity
		return typ
	case .Form, .Method, .Module, .Event:
		return external_new_type(external, .Routine)
	case:
	}
	return nil
}

external_type_class_or_interface :: proc(
	external: ^External_Semantics,
	entity: ^Entity,
	kind: Entity_Kind,
) -> ^Type {
	typ := external_new_type(external, .Class if kind == .Class else .Interface)
	typ.name = entity.name
	typ.entity = entity
	return typ
}

external_builtin_type :: proc(external: ^External_Semantics, name: string) -> ^Type {
	typ := external_new_type(external, .Builtin)
	typ.name = external_intern_name(external, name)
	return typ
}
