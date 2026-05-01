# LSP Replay

`lsp_replay.py` is a dependency-free Python client for replaying editor-like LSP
sessions against `abap_lsp_server`. It is intended for bug reproduction: define
the workspace, wait for ABAP analysis to finish, then run hover, definition,
completion, or raw LSP requests.

Run from the repository root:

```bat
python tools\lsp_replay\lsp_replay.py repro.json
```

The script uses `target\debug\abap_lsp_server.exe` when present, otherwise it
runs `cargo run -q -p abap_lsp_server --`. Override that with `--server`:

```bat
python tools\lsp_replay\lsp_replay.py repro.json --server target\debug\abap_lsp_server.exe
```

Example scenario:

```json
{
  "workspace": "examples/perf_corpus",
  "timeoutMs": 30000,
  "steps": [
    { "initialize": {} },
    { "initialized": {} },
    { "waitAnalysis": {} },
    {
      "open": {
        "path": "examples/perf_corpus/src/reports/ZPERF_DRIVER/ZPERF_DRIVER.abap"
      }
    },
    { "waitAnalysis": {} },
    {
      "hover": {
        "path": "examples/perf_corpus/src/reports/ZPERF_DRIVER/ZPERF_DRIVER.abap",
        "line": 10,
        "character": 4,
        "saveAs": "hover_p_vendor",
        "expect": { "resultNotNull": true }
      }
    },
    {
      "definition": {
        "path": "examples/perf_corpus/src/reports/ZPERF_DRIVER/ZPERF_DRIVER.abap",
        "line": 14,
        "character": 11,
        "saveAs": "definition_create_sto",
        "expect": { "resultNotNull": true }
      }
    }
  ]
}
```

Supported steps:

- `initialize`, `initialized`
- `open`, `change`
- `waitAnalysis`
- `hover`, `definition` / `gotoDefinition`, `completion`, `references`,
  `semanticTokens`, `inlayHint`
- `readDependencyDocument` for `abapls/readDependencyDocument`
- `diagnostics` to snapshot the latest `publishDiagnostics` for a URI, or add
  `"history": true` to include every diagnostic publication seen for that URI
- `request` and `notify` for raw LSP/custom methods
- `sleep`

Each run prints a JSON transcript with per-step responses, saved results,
analysis statuses, and notification counts. Add `"trace": true` or pass
`--trace` to include all received notifications. Long strings are truncated in
the printed transcript by default; set `"maxTranscriptStringChars": 0` to print
full payloads.

`waitAnalysis` waits for the next `finished` analysis status after the most
recent `initialized`, `open`, `change`, or known workspace-analysis
notification. It also accepts a complete `progress` status by default because
eager `initialized` analysis can finish that way today; set
`"acceptCompleteProgress": false` on the step to require an explicit `finished`
phase. Add `"trigger": "open"` or another trigger name to require a specific
analysis trigger.

Steps with a response can use `"saveAs": "name"`. Later steps can refer to
nested saved values with `"uriFromSaved": "name.uri"` or
`"textFromSaved": "name.sourceText"`, which is useful for jumping to and opening
`abapls-cache` dependency documents returned by go-to-definition.

If a workspace requests remote dependencies and there is no client-side ADT
fetcher in the replay, set `"autoFailRemoteDependencies": true` to answer
`abapls/resolveRemoteDependencies` with failed candidates so analysis can settle
without SAP access.
