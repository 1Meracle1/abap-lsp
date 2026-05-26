# Static Analysis Artifacts

## Overview

The repository now has two layers for routine-oriented static analysis:

1. `routine_analysis` builds the heavyweight internal artifact:
   - routine descriptors
   - instruction IR
   - CFG blocks/edges
   - forward/backward dataflow state
2. `static_analysis` builds a compact summary for user-facing consumers:
   - per-routine metadata
   - executable ranges
   - instruction/block counts
   - dataflow convergence status
   - grouped routine findings

The compact summary is derived from the heavyweight analysis. It does not replace it.

## Storage Rules

- Do not store routine IR, CFG, or dataflow blobs on `UnitAnalysis`.
- Keep heavyweight analysis artifacts attached near `AnalysisSnapshot` behind `Arc`.
- Treat the compact summary as a separate snapshot-scoped artifact, also behind `Arc`.
- `UnitAnalysis` should remain focused on collect/resolve/facts/validate outputs that are already
  part of the incremental project pipeline.

## Invariants

- The compact summary is read-only and derived from published routine-analysis results.
- Summary findings are limited to routine-analysis diagnostics:
  - `unreachable_code`
  - `use_before_definite_assignment`
  - `possibly_unbound_field_symbol`
  - `dead_store`
- Dossier export may expose compact routine summaries, but it must not dump the internal IR / CFG /
  dataflow structures directly.
- Incremental project updates may rebuild the shared snapshot artifacts, but they must not increase
  `UnitAnalysis` clone size just to support user-facing static-analysis queries.
- Missing compact summary data must degrade by omission rather than by serving stale results.

## Publish And Preview Behavior

Full publish / committed analysis:

- Build `ProjectRoutineAnalysis`.
- Build the compact static-analysis summary from that shared analysis.
- Attach both artifacts to the published snapshot.
- Record per-stage timings for the compact summary build.

Committed-context preview:

- Favor editor latency over rebuilding full project static analysis.
- Reuse committed project context when possible.
- Omit the compact static-analysis summary from the preview snapshot when it would be stale for the
  edited unit.

Single-document fallback preview:

- When there is no committed project context, build a single-unit routine analysis and compact
  summary.

## User-Facing Fallbacks

- Snapshot query APIs must tolerate the summary being unavailable.
- Semantic dossier export may omit the `static_analysis` section when the summary is unavailable.
- Consumers should interpret absence as "not available in this mode", not as "no findings exist".
