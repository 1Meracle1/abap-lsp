package abap_frontend_semantic2

import "core:testing"

@(test)
checker_add_external_dependency_edge_to_list_replaces_duplicate_site :: proc(t: ^testing.T) {
	edges := make([dynamic]Semantic_Dependency_Edge, 0, 2, context.allocator)
	defer delete(edges)
	indexes := make(map[Semantic_Dependency_Edge_Key]int, 2, context.allocator)
	defer delete(indexes)

	key := Semantic_Object_Key{kind = .Class, name = "zcl_dep"}
	first := Semantic_Dependency_Edge {
		key      = key,
		binding  = External_Binding{project_id = Semantic_Project_Id(1), generation = 1},
		resolved = true,
	}
	next := first
	next.binding = External_Binding{project_id = Semantic_Project_Id(2), generation = 2}

	add_external_dependency_edge_to_list(&edges, &indexes, first)
	add_external_dependency_edge_to_list(&edges, &indexes, next)

	testing.expect_value(t, len(edges), 1)
	testing.expect_value(t, len(indexes), 1)
	if len(edges) == 1 {
		testing.expect_value(t, edges[0].binding.project_id, Semantic_Project_Id(2))
		testing.expect_value(t, edges[0].binding.generation, u64(2))
	}
}
