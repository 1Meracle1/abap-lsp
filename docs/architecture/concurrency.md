# Concurrency Model

## Goals

- Keep the protocol loop simple and blocking.
- Push CPU-heavy work onto bounded worker threads.
- Publish immutable analysis snapshots so readers do not race with writers.
- Preserve deterministic behavior under repeated parallel test runs.

## Proposed Execution Model

```mermaid
flowchart LR
  client[ClientIO] --> mainLoop[MainProtocolLoop]
  mainLoop --> jobQueue[BoundedWorkQueue]
  jobQueue --> workerPool[WorkerPool]
  workerPool --> completions[TypedCompletionQueue]
  completions --> commit[CacheCommitBoundary]
  commit --> snapshots[ArcSnapshots]
  snapshots --> handlers[LSPHandlers]
```

## Rules

- One foreground protocol thread owns inbound request ordering and outbound notifications.
- Worker threads run parse, resolve, validate, and workspace refresh jobs.
- Worker results are committed through a single cache publication boundary so visible state changes stay coherent.
- Published snapshots are immutable and reference-counted with `Arc`.
- Locks protect maps and publication points, not every node in the syntax tree.

## Preferred Primitives

- `rayon` or a dedicated bounded executor for CPU-heavy work.
- `crossbeam-channel` for typed completion messages and shutdown coordination.
- `parking_lot::{Mutex, RwLock}` for low-overhead synchronization around cache maps and scheduling state.

## Anti-Goals

- No async runtime for core server execution.
- No unbounded job fan-out.
- No pervasive interior mutability inside published semantic objects.

## Test Focus

- Snapshot publication is atomic from the reader perspective.
- Older snapshots remain readable while newer versions are published.
- Workspace invalidation and remote dependency refreshes do not deadlock.
- Repeated parallel analysis runs produce the same diagnostics and symbol results.
