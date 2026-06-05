package abap_frontend_semantic_analyze

import "src:tokenizer"

import topological_sort "core:container/topological_sort"
import "core:mem"

Project_Provider_Dependency_Kind :: enum {
	Include,
	Reference,
	Sql_Source,
	Call,
}

Project_Entity_Use :: struct {
	entity:    Entity_Handle,
	provider:  Provider_Handle,
	reference: Reference_Id,
	range:     tokenizer.Range,
}

Project_Provider_Dependency_Edge :: struct {
	from:      Provider_Handle,
	to:        Provider_Handle,
	kind:      Project_Provider_Dependency_Kind,
	reference: Reference_Id,
}

Project_Graph :: struct {
	entity_uses:           map[Entity_Handle][dynamic]Project_Entity_Use,
	uses_by_provider:      map[Provider_Handle][dynamic]Project_Entity_Use,
	provider_dependencies: map[Provider_Handle][dynamic]Project_Provider_Dependency_Edge,
	provider_dependents:   map[Provider_Handle][dynamic]Project_Provider_Dependency_Edge,
	providers:             map[Provider_Handle]bool,
	allocator:             mem.Allocator,
}

Project_Graph_Topological_Result :: struct {
	ordered: [dynamic]Provider_Handle,
	cycled:  [dynamic]Provider_Handle,
}

project_graph_make :: proc(allocator: mem.Allocator) -> Project_Graph {
	return Project_Graph {
		entity_uses = make(map[Entity_Handle][dynamic]Project_Entity_Use, 64, allocator),
		uses_by_provider = make(map[Provider_Handle][dynamic]Project_Entity_Use, 16, allocator),
		provider_dependencies = make(
			map[Provider_Handle][dynamic]Project_Provider_Dependency_Edge,
			16,
			allocator,
		),
		provider_dependents = make(
			map[Provider_Handle][dynamic]Project_Provider_Dependency_Edge,
			16,
			allocator,
		),
		providers = make(map[Provider_Handle]bool, 16, allocator),
		allocator = allocator,
	}
}

project_graph_init :: proc(graph: ^Project_Graph, allocator: mem.Allocator) {
	graph^ = project_graph_make(allocator)
}

project_graph_destroy :: proc(graph: ^Project_Graph) {
	if graph.entity_uses != nil {
		for _, uses in graph.entity_uses {
			delete(uses)
		}
		delete(graph.entity_uses)
	}
	if graph.uses_by_provider != nil {
		for _, uses in graph.uses_by_provider {
			delete(uses)
		}
		delete(graph.uses_by_provider)
	}
	if graph.provider_dependencies != nil {
		for _, edges in graph.provider_dependencies {
			delete(edges)
		}
		delete(graph.provider_dependencies)
	}
	if graph.provider_dependents != nil {
		for _, edges in graph.provider_dependents {
			delete(edges)
		}
		delete(graph.provider_dependents)
	}
	if graph.providers != nil {
		delete(graph.providers)
	}
	graph^ = Project_Graph{}
}

project_graph_rebuild_from_project :: proc(
	graph: ^Project_Graph,
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	allocator: mem.Allocator,
) {
	project_graph_destroy(graph)
	project_graph_init(graph, allocator)
	if project == nil {
		return
	}
	for _, source_file_index in project.providers.source_files {
		project_graph_update_unit_from_project(graph, project, lookup, source_file_index)
	}
}

project_graph_update_unit_from_project :: proc(
	graph: ^Project_Graph,
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_index: int,
) {
	if project == nil ||
	   source_file_index < 0 ||
	   source_file_index >= len(project.providers.source_files) {
		return
	}
	project_graph_ensure_initialized(graph)

	unit := &project.providers.source_files[source_file_index]
	source_provider := source_file_provider_handle(unit)
	project_graph_remove_entity_uses_from_provider(graph, source_provider)
	project_graph_remove_provider_dependencies_from(graph, source_provider)
	project_graph_add_provider(graph, source_provider)

	for edge in unit.include_edges {
		if edge.has_target {
			project_graph_add_provider_dependency(
				graph,
				source_provider,
				source_file_provider_handle(
					&project.providers.source_files[source_file_id_index(edge.target)],
				),
				.Include,
			)
		}
	}
	for ref in unit.references {
		project_graph_record_reference_use(graph, project, source_file_index, ref)
	}
	if lookup == nil {
		return
	}
	for sql_source in unit.sql_sources {
		if handle, ok := resolve_type_name_in_project_lookup(
			project,
			lookup,
			source_file_index,
			sql_source.name,
		); ok {
			project_graph_add_provider_dependency(
				graph,
				source_provider,
				source_file_provider_handle(
					&project.providers.source_files[source_file_id_index(handle.unit)],
				),
				.Sql_Source,
			)
		}
	}
	for call_site in unit.call_sites {
		#partial switch call_site.target.kind {
		case .Function:
			if handle, ok := root_symbol_in_source_file_lookup(
				project,
				unit.source_file_id,
				.Routine,
				call_site.target.function_name,
			); ok && handle.unit != INVALID_SOURCE_FILE_ID {
				project_graph_add_provider_dependency(
					graph,
					source_provider,
					source_file_provider_handle(
						&project.providers.source_files[source_file_id_index(handle.unit)],
					),
					.Call,
				)
			}
		case .Report:
			if handle, ok := root_symbol_in_source_file_lookup(
				project,
				unit.source_file_id,
				.Value,
				call_site.target.report_name,
			); ok && handle.unit != INVALID_SOURCE_FILE_ID {
				project_graph_add_provider_dependency(
					graph,
					source_provider,
					source_file_provider_handle(
						&project.providers.source_files[source_file_id_index(handle.unit)],
					),
					.Call,
				)
			}
		case:
		}
	}
}

project_graph_add_provider :: proc(graph: ^Project_Graph, provider: Provider_Handle) {
	if !provider_handle_is_valid(provider) {
		return
	}
	project_graph_ensure_initialized(graph)
	graph.providers[provider] = true
}

project_graph_add_entity_use :: proc(graph: ^Project_Graph, use: Project_Entity_Use) {
	if !provider_handle_is_valid(use.entity.provider) ||
	   !provider_handle_is_valid(use.provider) ||
	   use.reference == INVALID_REFERENCE_ID {
		return
	}
	project_graph_add_provider(graph, use.provider)
	project_graph_add_provider(graph, use.entity.provider)
	project_graph_append_entity_use(graph, use)
	project_graph_append_provider_use(graph, use)
}

project_graph_add_provider_dependency :: proc(
	graph: ^Project_Graph,
	from, to: Provider_Handle,
	kind: Project_Provider_Dependency_Kind,
	reference := INVALID_REFERENCE_ID,
) {
	if !provider_handle_is_valid(from) || !provider_handle_is_valid(to) {
		return
	}
	project_graph_add_provider(graph, from)
	project_graph_add_provider(graph, to)
	if from == to {
		return
	}
	edge := Project_Provider_Dependency_Edge {
		from      = from,
		to        = to,
		kind      = kind,
		reference = reference,
	}
	project_graph_append_dependency_edge(graph, edge)
	project_graph_append_dependent_edge(graph, edge)
}

project_graph_uses_of_entity :: proc(
	graph: ^Project_Graph,
	entity: Entity_Handle,
) -> []Project_Entity_Use {
	if graph == nil || graph.entity_uses == nil {
		return nil
	}
	if uses, ok := graph.entity_uses[entity]; ok {
		return uses[:]
	}
	return nil
}

project_graph_provider_dependencies :: proc(
	graph: ^Project_Graph,
	provider: Provider_Handle,
) -> []Project_Provider_Dependency_Edge {
	if graph == nil || graph.provider_dependencies == nil {
		return nil
	}
	if edges, ok := graph.provider_dependencies[provider]; ok {
		return edges[:]
	}
	return nil
}

project_graph_provider_dependents :: proc(
	graph: ^Project_Graph,
	provider: Provider_Handle,
) -> []Project_Provider_Dependency_Edge {
	if graph == nil || graph.provider_dependents == nil {
		return nil
	}
	if edges, ok := graph.provider_dependents[provider]; ok {
		return edges[:]
	}
	return nil
}

project_graph_providers_affected_by_changed_provider :: proc(
	graph: ^Project_Graph,
	changed: Provider_Handle,
	allocator: mem.Allocator,
) -> [dynamic]Provider_Handle {
	out := make([dynamic]Provider_Handle, 0, 4, allocator)
	if graph == nil || graph.provider_dependents == nil || !provider_handle_is_valid(changed) {
		return out
	}

	seen := make(map[Provider_Handle]bool, 16, context.temp_allocator)
	queue := make([dynamic]Provider_Handle, 0, 4, context.temp_allocator)
	seen[changed] = true
	append(&queue, changed)
	for cursor := 0; cursor < len(queue); cursor += 1 {
		current := queue[cursor]
		dependents, ok := graph.provider_dependents[current]
		if !ok {
			continue
		}
		for edge in dependents {
			dependent := edge.from
			if dependent in seen {
				continue
			}
			seen[dependent] = true
			append(&out, dependent)
			append(&queue, dependent)
		}
	}
	return out
}

project_graph_provider_dependency_order :: proc(
	graph: ^Project_Graph,
	allocator: mem.Allocator,
) -> Project_Graph_Topological_Result {
	sorter: topological_sort.Sorter(Provider_Handle)
	topological_sort.init(&sorter)
	defer topological_sort.destroy(&sorter)

	if graph != nil && graph.providers != nil {
		for provider, _ in graph.providers {
			topological_sort.add_key(&sorter, provider)
		}
	}
	if graph != nil && graph.provider_dependencies != nil {
		for from, edges in graph.provider_dependencies {
			topological_sort.add_key(&sorter, from)
			for edge in edges {
				topological_sort.add_key(&sorter, edge.to)
				topological_sort.add_dependency(&sorter, edge.from, edge.to)
			}
		}
	}

	sorted, cycled := topological_sort.sort(&sorter)
	result := Project_Graph_Topological_Result {
		ordered = make([dynamic]Provider_Handle, 0, len(sorted), allocator),
		cycled  = make([dynamic]Provider_Handle, 0, len(cycled), allocator),
	}
	for provider in sorted {
		append(&result.ordered, provider)
	}
	for provider in cycled {
		append(&result.cycled, provider)
	}
	delete(sorted)
	delete(cycled)
	return result
}

project_providers_affected_by_changed_provider :: proc(
	project: ^Project_Analysis,
	changed: Provider_Handle,
	allocator: mem.Allocator,
) -> [dynamic]Provider_Handle {
	if project == nil {
		return make([dynamic]Provider_Handle, 0, 0, allocator)
	}
	return project_graph_providers_affected_by_changed_provider(&project.graph, changed, allocator)
}

include_visible_source_files_for_project_graph :: proc(
	project: ^Project_Analysis,
	allocator: mem.Allocator,
) -> [][dynamic]Source_File_Id {
	count := 0
	if project != nil {
		count = len(project.providers.source_files)
	}
	out := make([][dynamic]Source_File_Id, count, allocator)
	for i in 0 ..< count {
		out[i] = make([dynamic]Source_File_Id, allocator)
	}
	if project == nil {
		return out
	}
	for unit in project.providers.source_files {
		expansion := make([dynamic]Source_File_Id, allocator)
		stack := make([dynamic]Source_File_Id, allocator)
		project_graph_collect_include_expansion(project, unit.source_file_id, &stack, &expansion)
		for participant in expansion {
			idx := source_file_id_index(participant)
			if idx < 0 || idx >= len(out) {
				continue
			}
			for candidate in expansion {
				if candidate != participant {
					push_unique_unit(&out[idx], candidate)
				}
			}
		}
	}
	return out
}

@(private)
project_graph_record_reference_use :: proc(
	graph: ^Project_Graph,
	project: ^Project_Analysis,
	source_file_index: int,
	ref: Reference_Data,
) {
	if !ref.has_resolution {
		return
	}
	unit := &project.providers.source_files[source_file_index]
	source_provider := source_file_provider_handle(unit)
	entity, entity_ok := project_graph_entity_for_reference_resolution(project, ref)
	if !entity_ok {
		return
	}
	project_graph_add_entity_use(
		graph,
		Project_Entity_Use {
			entity = entity,
			provider = source_provider,
			reference = ref.id,
			range = ref.range,
		},
	)
	project_graph_add_provider_dependency(
		graph,
		source_provider,
		entity.provider,
		.Reference,
		ref.id,
	)
}

@(private)
project_graph_entity_for_reference_resolution :: proc(
	project: ^Project_Analysis,
	ref: Reference_Data,
) -> (
	Entity_Handle,
	bool,
) {
	#partial switch ref.resolution.kind {
	case .Symbol:
		return entity_handle_from_symbol_handle(project, ref.resolution.symbol)
	case .Provider_Entity:
		return ref.resolution.entity, provider_handle_is_valid(ref.resolution.entity.provider)
	case .Builtin_Type:
		return builtin_entity_handle(.Type, ref.name)
	case .Builtin_Routine:
		return builtin_entity_handle(.Routine, ref.name)
	}
	return {}, false
}

@(private)
project_graph_collect_include_expansion :: proc(
	project: ^Project_Analysis,
	source_file_id: Source_File_Id,
	stack, out: ^[dynamic]Source_File_Id,
) {
	idx := source_file_id_index(source_file_id)
	if project == nil ||
	   idx < 0 ||
	   idx >= len(project.providers.source_files) ||
	   unit_list_contains(stack^[:], source_file_id) {
		return
	}
	append(stack, source_file_id)
	push_unique_unit(out, source_file_id)
	provider := source_file_provider_handle(&project.providers.source_files[idx])
	for edge in project_graph_provider_dependencies(&project.graph, provider) {
		if edge.kind != .Include || edge.to.kind != .File {
			continue
		}
		target := Source_File_Id(u32(edge.to.id))
		project_graph_collect_include_expansion(project, target, stack, out)
	}
	resize(stack, len(stack^) - 1)
}

@(private)
project_graph_ensure_initialized :: proc(graph: ^Project_Graph) {
	if graph.providers != nil {
		return
	}
	allocator := graph.allocator
	if allocator.procedure == nil {
		allocator = context.allocator
	}
	project_graph_init(graph, allocator)
}

@(private)
project_graph_append_entity_use :: proc(graph: ^Project_Graph, use: Project_Entity_Use) {
	if uses, ok := graph.entity_uses[use.entity]; ok {
		for existing in uses {
			if project_entity_use_equal(existing, use) {
				return
			}
		}
		append(&uses, use)
		graph.entity_uses[use.entity] = uses
	} else {
		next := make([dynamic]Project_Entity_Use, 0, 2, graph.allocator)
		append(&next, use)
		graph.entity_uses[use.entity] = next
	}
}

@(private)
project_graph_append_provider_use :: proc(graph: ^Project_Graph, use: Project_Entity_Use) {
	if uses, ok := graph.uses_by_provider[use.provider]; ok {
		for existing in uses {
			if project_entity_use_equal(existing, use) {
				return
			}
		}
		append(&uses, use)
		graph.uses_by_provider[use.provider] = uses
	} else {
		next := make([dynamic]Project_Entity_Use, 0, 2, graph.allocator)
		append(&next, use)
		graph.uses_by_provider[use.provider] = next
	}
}

@(private)
project_graph_append_dependency_edge :: proc(
	graph: ^Project_Graph,
	edge: Project_Provider_Dependency_Edge,
) {
	if edges, ok := graph.provider_dependencies[edge.from]; ok {
		for existing in edges {
			if project_provider_dependency_edge_equal(existing, edge) {
				return
			}
		}
		append(&edges, edge)
		graph.provider_dependencies[edge.from] = edges
	} else {
		next := make([dynamic]Project_Provider_Dependency_Edge, 0, 2, graph.allocator)
		append(&next, edge)
		graph.provider_dependencies[edge.from] = next
	}
}

@(private)
project_graph_append_dependent_edge :: proc(
	graph: ^Project_Graph,
	edge: Project_Provider_Dependency_Edge,
) {
	if edges, ok := graph.provider_dependents[edge.to]; ok {
		for existing in edges {
			if project_provider_dependency_edge_equal(existing, edge) {
				return
			}
		}
		append(&edges, edge)
		graph.provider_dependents[edge.to] = edges
	} else {
		next := make([dynamic]Project_Provider_Dependency_Edge, 0, 2, graph.allocator)
		append(&next, edge)
		graph.provider_dependents[edge.to] = next
	}
}

@(private)
project_graph_remove_entity_uses_from_provider :: proc(
	graph: ^Project_Graph,
	provider: Provider_Handle,
) {
	if graph.uses_by_provider == nil {
		return
	}
	uses, ok := graph.uses_by_provider[provider]
	if !ok {
		return
	}
	for use in uses {
		project_graph_remove_entity_use(graph, use)
	}
	delete(uses)
	delete_key(&graph.uses_by_provider, provider)
}

@(private)
project_graph_remove_provider_dependencies_from :: proc(
	graph: ^Project_Graph,
	provider: Provider_Handle,
) {
	if graph.provider_dependencies == nil {
		return
	}
	edges, ok := graph.provider_dependencies[provider]
	if !ok {
		return
	}
	for edge in edges {
		project_graph_remove_dependent_edge(graph, edge)
	}
	delete(edges)
	delete_key(&graph.provider_dependencies, provider)
}

@(private)
project_graph_remove_entity_use :: proc(graph: ^Project_Graph, use: Project_Entity_Use) {
	uses, ok := graph.entity_uses[use.entity]
	if !ok {
		return
	}
	write := 0
	for existing in uses {
		if project_entity_use_equal(existing, use) {
			continue
		}
		uses[write] = existing
		write += 1
	}
	if write == 0 {
		delete(uses)
		delete_key(&graph.entity_uses, use.entity)
	} else {
		resize(&uses, write)
		graph.entity_uses[use.entity] = uses
	}
}

@(private)
project_graph_remove_dependent_edge :: proc(
	graph: ^Project_Graph,
	edge: Project_Provider_Dependency_Edge,
) {
	if graph.provider_dependents == nil {
		return
	}
	edges, ok := graph.provider_dependents[edge.to]
	if !ok {
		return
	}
	write := 0
	for existing in edges {
		if project_provider_dependency_edge_equal(existing, edge) {
			continue
		}
		edges[write] = existing
		write += 1
	}
	if write == 0 {
		delete(edges)
		delete_key(&graph.provider_dependents, edge.to)
	} else {
		resize(&edges, write)
		graph.provider_dependents[edge.to] = edges
	}
}

@(private)
project_entity_use_equal :: proc(a, b: Project_Entity_Use) -> bool {
	return a.entity == b.entity && a.provider == b.provider && a.reference == b.reference
}

@(private)
project_provider_dependency_edge_equal :: proc(a, b: Project_Provider_Dependency_Edge) -> bool {
	return a.from == b.from && a.to == b.to && a.kind == b.kind && a.reference == b.reference
}
