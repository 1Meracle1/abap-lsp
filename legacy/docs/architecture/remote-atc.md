# Remote SAP ATC Provider

## Decision

Use a shared ATC result protocol, with the VS Code client as the first transport owner.

The Rust language server owns:

- `abapls.toml` lint configuration
- imported ATC finding cache keys
- mapping imported findings into LSP diagnostics
- deduplication boundaries and stale-result filtering

The VS Code client should own:

- SAP credentials and secret storage
- ADT HTTP(S) session setup, CSRF token handling, cookies, and trust store behavior
- SAP ATC run creation, polling, cancellation, and result parsing

`abap_adt_cli` can later add an ATC subcommand that emits the same result payload. That gives both
interactive editor runs and script/CI runs one shared wire shape without moving credentials into the
language server.

The exact SAP ATC ADT endpoints are not validated in this workspace. Do not implement live ATC HTTP
calls until they are tested against a configured SAP system and check variant.

## Configuration

`abapls.toml` supports:

```toml
[lints.sap_atc]
mode = "off"          # "off" | "manual" | "on-save"
check_variant = "DEFAULT"
configuration = "OPTIONAL_CONFIGURATION"
```

`mode = "off"` hides imported ATC diagnostics. `manual` and `on-save` allow imported ATC results to
surface as diagnostics. `check_variant` defaults to `DEFAULT`; blank variants normalize back to
`DEFAULT`. `configuration` is optional and never stores credentials.

## Implemented Ingestion Notification

Client or CLI bridge to server:

```json
{
  "jsonrpc": "2.0",
  "method": "abapls/sapAtcResultsUpdated",
  "params": {
    "workspaceUri": "file:///D:/repo",
    "sourceUri": "file:///D:/repo/src/reports/ZMAIN/ZMAIN.abap",
    "documentVersion": 12,
    "objectName": "ZMAIN",
    "checkVariant": "DEFAULT",
    "configuration": "OPTIONAL_CONFIGURATION",
    "fetchedAt": "2026-04-27T00:00:00Z",
    "findings": [
      {
        "sapCheckId": "CI_ALL_FIELDS_NEEDED",
        "sapMessageId": "MSG001",
        "message": "List the required columns explicitly",
        "severity": "warning",
        "mappedLocalLintId": "abap-lsp.select-star",
        "exemptionState": "none",
        "suppressionState": null,
        "location": {
          "uri": "file:///D:/repo/src/reports/ZMAIN/ZMAIN.abap",
          "objectName": "ZMAIN",
          "includeName": "ZMAIN",
          "startLine": 10,
          "startColumn": 3,
          "endLine": 10,
          "endColumn": 24
        }
      }
    ]
  }
}
```

Location line and column values are 1-based, matching typical SAP result displays. The language
server converts them to LSP's 0-based UTF-16 positions and drops invalid locations.

## Diagnostic Mapping

Imported findings are emitted as LSP diagnostics only when all of these match the current snapshot:

- source URI
- document version
- object name, when both sides know it
- check variant
- optional configuration
- `[lints.sap_atc].mode` is `manual` or `on-save`

Diagnostics use:

- `source = "sap-atc"`
- `code = mappedLocalLintId` when provided or when a SAP alias maps to a native lint
- otherwise `code = "sap-atc:<check-id>/<message-id>"`
- `data.kind = "sap_atc_lint"`
- `data.sapCheckId`, `data.sapMessageId`, `data.objectName`, `data.includeName`
- `data.checkVariant`, `data.configuration`, `data.fetchedAt`
- `data.exemptionState`, `data.suppressionState`

Severity accepts SAP-like numeric and textual values:

- `1`, `error`, `fatal` -> error
- `2`, `warning`, unknown -> warning
- `3`, `info`, `note` -> information
- `4`, `hint` -> hint

Findings with exemption or suppression state text containing `exempt` or `suppress` receive the LSP
`unnecessary` tag, but are still shown so the user can inspect imported server state.

## Cache Key

The server caches imported results by:

```text
(source_uri, document_version, object_name, check_variant, configuration)
```

This prevents stale or repeated remote results from replacing diagnostics for a newer local editor
snapshot. A later live provider should check this cache before launching another ATC run for the
same object/version/check variant.

## Planned Run Request

A future server-to-client notification can request a run without exposing credentials:

```json
{
  "jsonrpc": "2.0",
  "method": "abapls/requestSapAtc",
  "params": {
    "workspaceUri": "file:///D:/repo",
    "sourceUri": "file:///D:/repo/src/reports/ZMAIN/ZMAIN.abap",
    "documentVersion": 12,
    "objectName": "ZMAIN",
    "objectUri": "/sap/bc/adt/programs/programs/zmain",
    "checkVariant": "DEFAULT",
    "configuration": null,
    "reason": "manual"
  }
}
```

The client should answer by sending `abapls/sapAtcResultsUpdated`, even when no findings are
returned. A failed run should not include credentials or Authorization headers in logs.

## SAP API TODOs

Validate these against a real SAP system before implementing live calls:

- ADT endpoint path and media type for ATC run creation.
- Request body shape for object sets covering programs, includes, global classes, interfaces,
  function modules, and DDIC objects.
- How `check_variant` and `configuration` are named in the ATC API on supported SAP releases.
- Polling endpoint, terminal states, cancellation behavior, and timeout semantics.
- Result media type and exact fields for check ID, message ID, priority, object/include location,
  exemption, and suppression state.
- Whether inactive editor buffers can be checked directly or must be activated/saved first.
- Authorization failure shape and how to distinguish missing ATC authorization from missing object
  authorization.
- How SAP encodes namespaces and include names in ATC object references.

Until these are validated, keep live ATC execution out of `abap_lsp_server` and `abap_lsp`.
