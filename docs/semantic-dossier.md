# `abap_cli analyze` semantic dossier

`abap_cli analyze` emits one compact JSON dossier for an ABAP file or workspace object so downstream
tools do not need to re-parse source heuristically to answer common semantic questions.

## Command shapes

Single-file analysis:

```bat
cargo run -p abap_cli -- analyze --json path\to\zcl_demo.abap
```

Workspace-aware analysis:

```bat
cargo run -p abap_cli -- analyze --json --with-project path\to\zcl_demo.abap
```

Pretty-printed output:

```bat
cargo run -p abap_cli -- analyze --json --with-project --pretty path\to\zcl_demo.abap
```

Notes:

- `--json` is required.
- `--with-project` loads workspace peers through `abap_cache` so cross-unit references and includes
  can resolve against the surrounding project.
- The export is intentionally semantic and compact. It does not dump the raw AST.

## Top-level schema

The current schema id is `abap.semantic_dossier` with `schema_version = 1`.

Top-level fields:

- `schema`
- `schema_version`
- `target`
- `project`
- `summary`
- `parse_diagnostics`
- `semantic_diagnostics`
- `structures`
- `symbols`
- `references`
- `scopes`
- `classes`
- `function_modules`
- `call_sites`
- `assignment_sites`
- `perform_calls`
- `sql`
- `includes`
- `unresolved_names`

## Key sections

`target`

- Identifies the analyzed unit.
- Includes `uri`, local `path` when available, `object_name`, dependency status, and provided names.

`project`

- Present when project context is loaded.
- Includes workspace root, manifest presence, total unit count, and dependency unit count.

`summary`

- Stable counts for diagnostics, symbols, references, scopes, class facts, calls, SQL, and include
  edges.

`references`

- Includes the name, namespace, kind, range, scope id, and resolution.
- Resolution is explicit: unresolved, builtin, external, or a concrete symbol handle with unit and
  symbol ids.

`classes`

- Bundles class members, inheritance facts, implemented interfaces, and aliases.

`sql`

- Includes `touched_objects`.
- Each query includes clause ranges, sources, projections, predicates, targets, and SQL name refs.

`unresolved_names`

- Buckets unresolved lexical references, unresolved SQL name refs, and unresolved includes.

## Intended use

The dossier is meant to be sufficient for questions such as:

- what symbols exist here?
- what does this object call?
- which SQL objects and columns are touched?
- which names failed to resolve?
- what class members, inheritance facts, and scopes matter for this file?

## Stability guidance

- Prefer consuming field names and explicit enums instead of positional assumptions.
- Treat absent optional sections as meaning "not available in this analysis mode".
- Do not infer raw syntax structure from the dossier; use `abap_cli parse --json --ast` for syntax
  tree inspection when needed.
