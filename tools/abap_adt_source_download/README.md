# ABAP ADT Source Download

Stdlib-only Go exporter for downloading ABAP package content over ADT.

The tool is intended for environments like Citrix where you want to upload a single executable and run it without pulling external Go modules on the target machine.

## Build

```bash
go build .
```

## Configuration

CLI flags are preferred, but `.env` in the current working directory is also supported.

Supported environment keys:

```env
ABAP_ADT_URL=https://your-sap-host:port/sap/bc/adt
ABAP_ADT_USER=your_username
ABAP_ADT_PASSWORD=your_password
ABAP_ADT_CLIENT=100
ABAP_ADT_OUTPUT=D:\dev\abap\sap_system_export
ABAP_ADT_OBJECTS_FILE=D:\dev\abap\unknown_objects.txt
ABAP_ADT_DEPENDENCY_CANDIDATES_FILE=D:\dev\rust\abap-lsp\remote_candidates.json
ABAP_ADT_PACKAGES=/STTP/MAIN,/AIF/MAIN
RATE_LIMIT_RPM=60
MAX_CONCURRENT_REQUESTS=4
```

`ABAP_ADT_URL` may also be given as the SAP host root. The exporter appends `/sap/bc/adt` automatically when needed.

## Usage

```bash
.\abap_adt_source_download.exe ^
  -url https://your-sap-host:port ^
  -user your_username ^
  -pass your_password ^
  -client 100 ^
  -output D:\dev\abap\sap_system_export ^
  -objects-file D:\dev\abap\unknown_objects.txt ^
  -package /STTP/MAIN ^
  -package /AIF/MAIN
```

Options:

- `-package` is repeatable.
- `-objects-file` is optional. When present, the exporter still walks the configured packages but only downloads objects whose names appear in the file. The file is plain text, one object name per line. Empty lines plus lines starting with `#` or `;` are ignored.
- `-dependency-candidates-file` switches the tool into package-free dependency mode. In that mode, `-package` must be omitted and `-output` must point at the workspace root. The tool writes into `.abapls/cache/...` just like the VS Code extension.
- `-clean` removes the existing export root contents before download.
- `-rpm` and `-parallel` control request pacing.
- At the end of a filtered run, the exporter logs any requested object names that were not found under the configured packages.

## Building An Objects File From The Workspace

`abap-cli` can emit remote dependency candidates from editable project files only.

PowerShell example:

```powershell
cargo run -p abap_cli -- remote-candidates . | Set-Content .\unknown_objects.txt
```

If you want the structured output instead:

```powershell
cargo run -p abap_cli -- remote-candidates --json --pretty .
```

That JSON output now includes both the deduped candidate list and `source_candidates` keyed by source URI, so it can be consumed directly by the Go tool’s dependency mode.

## Extension-Like Dependency Fetch Mode

This mode mirrors the VS Code extension’s remote dependency flow:

1. Read remote candidates from `abap-cli`.
2. Search ADT globally with `quickSearch`.
3. Pick the best supported object by candidate kind.
4. Fetch the artifact by ADT URI.
5. Write package cache files, object metadata, negative markers, and dependency-manifest entries under `.abapls/cache`.

Generate the batch file:

```powershell
cargo run -p abap_cli -- remote-candidates --json --pretty . | Set-Content .\remote_candidates.json
```

Resolve dependencies into the current workspace cache:

```powershell
go run . `
  -url https://your-sap-host:port `
  -user your_username `
  -pass your_password `
  -client 100 `
  -output D:\dev\rust\abap-lsp `
  -dependency-candidates-file D:\dev\rust\abap-lsp\remote_candidates.json
```

Plain text input is also accepted in dependency mode. Each line may be either `object_name` or `object_name|kind`. When no kind is given, the tool uses `symbol`.

## Export Layout Compatibility

The VS Code extension's local export fallback does a recursive scan under each configured export root and looks for files named:

- `encodeURIComponent(OBJECT_NAME).abap`
- `encodeURIComponent(OBJECT_NAME).xml`

The exporter in this folder keeps encoded filenames and package directories, so it is compatible with that lookup strategy.

Example sidecar:

```toml
[local_export]
roots = ["D:/dev/abap/sap_system_export"]

[dependencies]
source = "local-first"
```

Important: this local export lookup is currently implemented in the VS Code client extension. The Rust workspace loader in this repo does not appear to consume `[local_export]` on its own yet.
