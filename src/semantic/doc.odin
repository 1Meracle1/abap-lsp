package abap_frontend_semantic2

/*
semantic is the project-snapshot semantic checker for parsed ABAP source.

The package consumes parser ASTs and produces semantic projects, diagnostics,
entity uses, expression/type information, include-expansion results, and
external dependency candidates. It deliberately does not load files, parse text,
fetch ADT objects, read dependency caches, or convert DDIC XML. Callers perform
that I/O first and pass parsed local or external inputs into this package.

Top-level usage:

1. Workspace analysis.

   Use `semantic_workspace_analyze` when analyzing a folder-shaped set of parsed
   files:

   - Build `Workspace_File_Input` values from parsed source files.
   - Optionally build `External_Semantics` or `External_Interface_Input` values
     for already-fetched external objects.
   - Call `semantic_workspace_analyze`.
   - Read `Workspace_Analysis.project_results` for per-root checker snapshots.
   - Read `Workspace_Analysis.file_projects` or
     `semantic_workspace_projects_for_file` to invalidate only projects affected
     by a physical file change.
   - Read `Workspace_Analysis.unresolved` or `external_requests` to drive the
     caller's dependency-fetch loop.
   - Call `semantic_workspace_analysis_destroy` when the snapshot is no longer
     needed.

   A workspace analysis builds one `Project` per inferred root/root expansion,
   not one global semantic universe. Includes are resolved and expanded in
   lexical root order. Include fragments with no known root can become synthetic
   low-confidence project snapshots.

2. Single-project checking.

   Use the checker API directly for focused tests or tools that already have the
   parsed root AST:

   - Create a `Project` with `project_make`.
   - Create a `Checker` with `checker_make` or `checker_init`.
   - Add parsed files with `checker_add_file` or `checker_add_file`-equivalent
     workspace helpers.
   - Call `checker_check_file` for the root file.
   - Query `checker.info.diagnostics`, `uses`, `expr_infos`, and dependency
     edge arrays.
   - Destroy the `Project` with `project_destroy`.

   `checker_init` registers project-owned builtin types, constants, variables,
   structures, fields, and builtin procedures in the builtin scope. Builtins are
   therefore resolved through normal scope/entity lookup rather than through
   separate string checks.
   External semantic contexts use `checker_init_with_builtins` to import one
   shared builtin scope into each parsed external project, avoiding repeated
   builtin graph construction while keeping file scopes project-owned.

3. Query API.

   Use `semantic_query` over one project/checker snapshot. Query results are
   pointer-based and are valid only while that snapshot is alive:

   - `semantic_query_decls` finds declarations and class/structure members.
   - `semantic_query_refs` finds entity uses and all uses resolving to an
     entity pointer.
   - `semantic_query_facts` returns expression/operand/type information.
   - `semantic_query_diagnostics` returns diagnostic copies.
   - `semantic_query_completion` reads the lexical scope chain and, when
     supplied, the workspace provider index for completions.

   The package intentionally does not expose stable semantic IDs or persistent
   provider handles. LSP/workspace consumers should replace whole snapshots after
   edits and use pointers within the active snapshot.

4. External dependencies.

   External dependencies enter semantic only after the caller has fetched and
   parsed or summarized them:

   - `External_Semantics` stores caller-supplied external summaries and parsed
     external-interface projects.
   - It owns a lazily built builtin project/checker used by external interface
     and summary checkers.
   - External lookup returns project-owned provider bindings through
     `External_Semantic_Index`.
   - Resolved and unresolved relationships are recorded as
     `Semantic_Dependency_Edge` values on project records.
   - Missing external objects are emitted as `Checker_Unresolved_Candidate`
     values. The caller decides how to fetch, cache, block, or retry them.

   External includes are source inputs that participate in lexical include
   expansion. Other external objects can be represented as parsed external
   interface inputs or as compatibility summaries.

5. Incremental external scheduling.

   `Semantic_Graph_Session` is the stateful API for feeding newly fetched
   external objects back into analysis:

   - `semantic_graph_session_make` creates a session around editable files and
     external inputs.
   - `semantic_graph_session_apply_update` accepts changed editable files,
     fetched external objects, blocked dependencies, and an
     `external_frontier_stable` flag.
   - The session updates provider bindings, reverse dependency indexes, and
     unresolved waiter maps.
   - Editable project rebuilds can be deferred until the external frontier is
     stable, so a root is not rebuilt once per transitive dependency layer.

Architecture:

- `project.odin` owns snapshot storage. `Project` allocates files, entities,
  scopes, types, structures, declaration info, and records from project-owned
  storage. Pointer identity is the semantic identity inside one snapshot.

- `entity.odin` and `scope.odin` define the semantic graph: declarations,
  payloads, namespaces, lookup surfaces, and scope nesting.

- `checker.odin` orchestrates checking. It owns checker state, diagnostics,
  use/type-info records, unresolved candidates, dependency edges, builtin scope
  setup, file scopes, and shared lookup helpers.

- `check_builtin.odin` and `checker_builtin_procs.odin` define builtin entity
  registration and builtin-procedure metadata/dispatch.

- `check_decl.odin` collects declarations and creates entities. Declaration
  checking materializes types, routine signatures, body scopes, OOP members, and
  metadata entities.

- `check_type.odin` is the centralized AST type-reference to semantic `Type`
  path. Declaration, OOP, SQL, and expression code should use it instead of
  reinterpreting type syntax locally.

- `check_expr.odin` converts expressions to operands and records expression
  type/value/use information. Statement and SQL checkers consume these operands
  instead of resolving identifiers again.

- `check_stmt.odin` owns statement-list dispatch, local declarations,
  assignments, calls, control flow, table operations, messages, object/data
  creation, and statement-level compatibility checks.

- `check_oop.odin` owns class/interface member validation, inherited and
  qualified method signatures, `me`/`super` receivers, event-handler parameters,
  visibility, friends, aliases, and contextual member access.

- `check_sql.odin` owns Open SQL source scopes, aliases, projections, host
  variables, inline result shapes, joins, aggregate aliases, and SQL target
  compatibility. SQL lookup is intentionally separate from normal ABAP lexical
  lookup.

- `project_discovery.odin` scans parsed workspace files for root/include
  classification, provided names, local include edges, external include source
  edges, include cycles, and project plans.

- `workspace.odin` turns discovery plans into per-root project/checker
  snapshots, records file-to-project usage, imports external providers, and
  aggregates unresolved candidates.

- `external.odin` defines object keys, external/project-backed provider
  bindings, dependency edges, reverse indexes, external summaries, and parsed
  external-interface analysis.

- `semantic_graph.odin` manages incremental graph/session state around editable
  files, fetched external objects, provider replacement, reverse dependency
  invalidation, and deferred editable rebuilds.

- `query.odin` exposes snapshot-local pointer queries for LSP/editor consumers.

Important invariants:

- Pointer identity is valid only for the owning project/checker snapshot.
- Semantic names are canonical lowercase strings. Snapshot-owned names live in
  the owning project arena; persistent graph/index references clone names into
  their owning allocator before crossing snapshot boundaries.
- The checker is the owner of semantic facts. Avoid call-site workarounds that
  duplicate declaration, type, lookup, expression, OOP, SQL, or dependency
  policies.
- Impossible internal states should be asserted, not hidden behind defensive
  branches.
- Old `src/semantic/analyze` remains parallel until consumers are explicitly
  migrated to semantic.
*/
