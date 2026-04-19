# `abap_cli call-dataflow`

`abap_cli call-dataflow` traces how one concrete ABAP call site gets its argument values.

It is designed for function module calls and method calls, and combines:

- lifecycle context from report events, screen flow, and perform/call chains
- technical parameter traces for the selected occurrence
- field-to-field mappings for structures
- row-to-field provenance for internal-table payloads

## Command

```text
abap_cli call-dataflow [--json] --target NAME [--caller NAME] [--occurrence N] [--diagram ascii|svg|mermaid|rich-mermaid] [--pretty] FILE
```

Examples:

```text
abap_cli call-dataflow --target BAPI_PO_CREATE1 path\to\report.abap
abap_cli call-dataflow --target BAPI_PO_CREATE1 --caller f_bapi_po_create_fm path\to\report.abap
abap_cli call-dataflow --target BAPI_PO_CREATE1 --diagram svg path\to\report.abap
abap_cli call-dataflow --target BAPI_PO_CREATE1 --diagram mermaid path\to\report.abap
abap_cli call-dataflow --target BAPI_PO_CREATE1 --diagram rich-mermaid path\to\report.abap
abap_cli --json call-dataflow --target BAPI_PO_CREATE1 --occurrence 2 path\to\report.abap
```

## Selection Rules

- Matches are collected from project call sites around `FILE`.
- Filtering is deterministic:
  - target name match
  - optional caller name match
  - sorted by `unit_uri`, then source start/end
- If more than one call site remains and `--occurrence` is not supplied, the command returns an ambiguous result with ordered `matches` and no partial graph.

## Human Output

Default output is Markdown.

It includes:

- selected call summary
- lifecycle diagram rendered as ASCII by default
- detailed Mermaid provenance graph per parameter
- one technical mapping table per parameter

With `--diagram rich-mermaid`, the human output switches to one merged Mermaid graph that combines:

- lifecycle path to the selected call
- parameter root nodes connected directly to the selected call
- merged provenance nodes across parameters
- SQL source tables, SQL predicates, and predicate host-variable inputs
- field-level target nodes where the trace can resolve them

Available human diagram renderers:

- `ascii` (default)
- `svg`
- `mermaid`
- `rich-mermaid`

Synthetic runtime edges are marked explicitly. Current synthetic edges cover screen dispatch such as:

- `CALL SCREEN 9000 -> MODULE user_command_9000 INPUT`
- `CALL SCREEN 9000 -> MODULE status_9000 OUTPUT`

## JSON Schema

`--json` emits the canonical machine-readable schema:

```json
{
  "schema": "abap.call_dataflow_trace",
  "schema_version": 1
}
```

Top-level fields:

- `query`
- `selected_call`
- `matches`
- `lifecycle`
- `parameter_traces`
- `summary`

Key shapes:

- `selected_call`: the chosen occurrence, caller metadata, target metadata, and argument count
- `matches`: ordered candidates when the query is ambiguous
- `lifecycle.nodes`: callable/event/module nodes
- `lifecycle.edges`: perform/function/method/synthetic screen-dispatch edges
- `parameter_traces[*].field_mappings`: detailed provenance edges such as `assignment`, `perform_binding`, `perform_write`, `append_row`, `call_output`, `global_state`, `constant`
- `parameter_traces[*].provenance`: graph nodes/edges for target fields, assignments, appends, perform handoffs, SQL queries, SQL predicates, predicate host inputs, and SQL source tables

## Precision Rules

- Structure parameters emit exact field paths when the trace sees selector writes such as `cs_po_header-doc_type = ...`.
- Internal tables emit `table[*].field` paths when the trace can follow a row work area into `APPEND`.
- SQL-backed values now surface query/source provenance such as `FROM mara -> WHERE matnr = p_matnr -> SELECT INTO lv_matnr -> assignment -> poheader.matnr`.
- Output parameters stop at the selected call boundary for external SAP code. V1 does not trace into external callee internals.
- Dynamic dispatch, macro-heavy flow, and alias-heavy field-symbol flow stay conservative and surface as broader mappings or unresolved terminals instead of invented precision.

## Example

Shortened human output for the `po_creation` sample with target `BAPI_PO_CREATE1`:

````markdown
# Call Dataflow

- Target: `BAPI_PO_CREATE1`
- Matches: `1`
- Diagram: `ascii`

## Lifecycle

```text
event_block end-of-selection
`-- screen_dispatch CALL SCREEN 9000 (input) -> function_module user_command_9000 [synthetic]
    `-- perform -> form create_sto
        |-- perform -> form f_bapi_header_data
        |-- perform -> form f_bapi_item_data
        `-- perform -> form f_bapi_po_create_fm
            `-- selected_call -> function_module bapi_po_create1 [selected]
```

## Parameters

### `poitem`

| Target Path | Source | Kind | Location |
| --- | --- | --- | --- |
| poitem | t_poitem | perform_binding | file:///...F02.abap:53226-53234 |
| poitem | f_bapi_item_data:ct_poitem | perform_write | file:///...F02.abap:52830-53287 |
| poitem[*] | ls_poitem | append_row | file:///...F01.abap:69168-69177 |
| poitem[*].material | <lfs_final_data>-matnr | assignment | file:///...F01.abap:67362-67384 |
| poitem[*].quantity | lw_verme | assignment | file:///...F01.abap:67174-67182 |
```
````

## Notes

- ASCII is the default terminal-oriented renderer in V1.
- SVG is the graphical renderer for Markdown/HTML-capable viewers and for downstream tooling.
- Mermaid remains available as a source-format alternative with `--diagram mermaid`.
- `rich-mermaid` is the preferred single-diagram renderer when you want one field-level graph instead of separate per-parameter graphs.
- The renderer boundary is intentionally narrow so additional diagram formats can be added without changing the trace schema.
- JSON is the canonical intermediate format for downstream LLM or SVG generation.

## Prompt Recipe

When you want a narrative review or a custom re-rendering on top of the canonical trace, capture both the JSON and the merged graph:

```text
abap_cli --json call-dataflow --target BAPI_PO_CREATE1 --caller f_bapi_po_create_fm path\to\report.abap > call-dataflow.json
abap_cli call-dataflow --target BAPI_PO_CREATE1 --caller f_bapi_po_create_fm --diagram rich-mermaid path\to\report.abap
```

Then use a prompt in this shape:

```text
Analyze the attached `call-dataflow.json` and the emitted `rich-mermaid` graph.

Produce one single rich dataflow diagram for the selected call with these rules:
- show one merged graph, not separate parameter diagrams
- trace exact field-to-field mappings into the target call parameters
- for each SQL query, show:
  - source tables actually used
  - projected fields actually consumed later
  - predicate fields and host-variable inputs
- make shared mutable state explicit when multiple scopes write/read the same structure or internal table
- show side effects after the call, such as database updates, external methods, function modules, and commits
- call out missing or suspicious lineage explicitly instead of guessing
- prefer sparse graphs: include only fields and tables that actually contribute to the selected call or its side effects

After the diagram, list:
1. unresolved lineage gaps
2. likely false positives / conservative edges
3. the exact ABAP locations that would improve precision if the analyzer learned them
```
