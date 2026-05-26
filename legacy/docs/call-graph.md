# `abap_cli call-graph` project call graph

`abap_cli call-graph` emits a project-scale JSON call graph for ABAP code so downstream tools can
answer caller/callee questions across files and objects.

## Command shapes

Workspace-aware graph export:

```bat
cargo run -p abap_cli -- call-graph --json path\to\zcl_demo.abap
```

Focused query for one callable:

```bat
cargo run -p abap_cli -- call-graph --json --symbol zcl_demo~run path\to\zcl_demo.abap
```

Pretty-printed output:

```bat
cargo run -p abap_cli -- call-graph --json --pretty --symbol zcl_demo~run path\to\zcl_demo.abap
```

Notes:

- `--json` is required.
- Passing a file path loads the surrounding workspace through `abap_cache`, then builds one graph
  for the whole loaded project.
- `--symbol` matches against node id, short name, or qualified name.
- If stdin or no file path is used, the graph is limited to the single published input.

## Current coverage

The graph currently models:

- methods,
- forms and `PERFORM`,
- function modules and `CALL FUNCTION`,
- event blocks as callable entry nodes.

Edges are emitted with explicit resolution state:

- `resolved`: the target was linked to a concrete graph node,
- `unresolved`: the call shape was seen, but the target could not be resolved with current facts.

Unresolved edges are preserved instead of being dropped.

## Top-level output

Without `--symbol`, the output includes:

- `phase`
- `target_uri`
- `project_node_count`
- `project_edge_count`
- `nodes`
- `edges`

With `--symbol`, the output also includes:

- `symbol_query`
- `matched_nodes`
- `outbound`
- `inbound`
- `unresolved`

## Node shape

Each node includes:

- `id`
- `kind`
- `name`
- `qualified_name`
- `unit_uri`
- `decl_range`

## Edge shape

Each edge includes:

- `source`
- `target`
- `edge_kind`
- `resolution_status`
- `target_name`
- `source_range`

## Intended use

The call graph is meant to support questions such as:

- who calls this method, form, or function module?
- what can this routine reach next?
- which call sites remain dynamic or unresolved?
- how does control move across files in a procedural or OO ABAP flow?

## Current limits

- Dynamic dispatch precision is intentionally conservative. When resolution is uncertain, the edge
  stays unresolved.
- Event blocks are represented as source nodes, but no synthetic scheduler or runtime dispatcher
  edges are added.
- The graph is deterministic and incremental-friendly at the project snapshot level, but there is
  not yet a separate persisted graph cache format.
